## visualization-motorcycle-race
## Author: Parveen Sharma
## date: May-16-2015

library(reshape2)
library(sqldf)
library(lubridate)
library(dplyr)
library(tidyr)

require(reshape2)
require(sqldf)
require(lubridate)
require(dplyr)
require(tidyr)


# Please change this to folder where Data files have been downloaded and Kept
setwd("C:/Users/Parveen/Desktop/r-prog/VISUALIZATION MOTORCYCLE MATH/")

j_motor <- read.csv("CAX_EMC_Journalist_Data/CAX_EMC_Journalist_Motorcycle_Data.csv", stringsAsFactors=F)
j_camera <- read.csv("CAX_EMC_Journalist_Data/CAX_EMC_Journalist_Garmin_Camera.csv", stringsAsFactors=F)
j_watch <- read.csv("CAX_EMC_Journalist_Data/CAX_EMC_Journalist_Garmin_Watch_Data.csv", stringsAsFactors=F)

r_motor <- read.csv("CAX_EMC_Racer_Data/CAX_EMC_Racer_Motorcycle_Data.csv", stringsAsFactors=F)
r_camera <- read.csv("CAX_EMC_Racer_Data/CAX_EMC_Racer_Garmin_Camera.csv", stringsAsFactors=F)
r_watch <- read.csv("CAX_EMC_Racer_Data/CAX_EMC_Racer_Garmin_Watch_Data.csv", stringsAsFactors=F)

laptime <- read.csv("laptime.csv")
r_lap <- laptime[c(1,2,3)]
j_lap <- laptime[c(1,4,5)]

## str(j_motor)

# library(reshape2) #function::colsplit 
# Use TimeStamp variable
j_motor <- data.frame(j_motor, colsplit(as.character(j_motor$TimeStamp), pattern = ":", names = c("pHH","pMM", "pSS")))
r_motor <- data.frame(r_motor, colsplit(as.character(r_motor$TimeStamp), pattern = ":", names = c("pHH","pMM", "pSS")))

j_lap <- data.frame(j_lap, colsplit(as.character(j_lap$Journalist.Start), pattern = ":", names = c("pHH","pMM", "pSS")))
r_lap <- data.frame(r_lap, colsplit(as.character(r_lap$Racer.Start), pattern = ":", names = c("pHH","pMM", "pSS")))

#####################
# Connecting Lap info

# Created New Function w/FuzzyLogic
#FuzzyLogic for matching data within a Range

fuzzy_join <- function(fulldata, fulldatacolumn, attachdata, attachdatacolumn, attachdatapaste){
  # fulldata - j_motor - this is the main data
  # fulldatacolumn - j_motor$TimeStamp - this is the column from main data for join
  # attachdata - j_lap - this is the another data to join with fuzzy logic to the 
  # attachdatacolumn - j_lap$Journalist.Start
  # attachdatapaste - j_lap$Lap
  
  dt <- "2015-05-02" #Choose any date here
  nTime <- as.matrix(unique(fulldatacolumn))
  nTime.wDt <- paste(dt, nTime, sep=" ") 
  nTime.dt <- as.POSIXct(nTime.wDt)
  nTime.num <- as.double(nTime.dt)
  
  j_lap.t <- as.matrix(unique(attachdatacolumn))
  j_lap.wDt <- paste(dt, j_lap.t, sep=" ") 
  j_lap.dt <- as.POSIXct(j_lap.wDt)
  j_lap.num <- as.double(j_lap.dt)
  
  mylap <- matrix(ncol=1, nrow=length(nTime.num))
  mygap <- matrix(ncol=1, nrow=length(nTime.num))
  for(i in 1:length(nTime.num)){
    for(j in length(j_lap.num):1){
      mygap[i] <- nTime.num[i]-nTime.num[1]
      if(j_lap.num[j]>=nTime.num[i]) {
        ## move to the next identifier
        next
      } else {
        mylap[i] <- paste(attachdatapaste[j])
        break
      }
    }
  }
  
  j_info <- data.frame(cbind(nTime, mylap, mygap))
  colnames(j_info) <- c("TimeStamp","LapName", "TimeLag")
  # Return dataset with LapNumber information
  merge(fulldata, j_info, by="TimeStamp")
}

## Motorcycle
j_motor.n <- fuzzy_join(j_motor, j_motor$TimeStamp, laptime, laptime$Journalist.Start, laptime$Lap)
r_motor.n <- fuzzy_join(r_motor, r_motor$TimeStamp, laptime, laptime$Racer.Start, laptime$Lap)
j_motor.n$who <- "Jounalist"
r_motor.n$who <- "Racer"

j_motor.n$TimeNum <- as.numeric(rownames(j_motor.n))
r_motor.n$TimeNum <- as.numeric(rownames(r_motor.n))

fin.motor <- rbind(r_motor.n, j_motor.n)

## Convert multple rows into Single TimeStamp Rows
fin.motor.use <- fin.motor
fin.motor.use <- data.frame(fin.motor.use, colsplit(as.character(fin.motor.use$TimeStamp), pattern = "\\.", names = c("pT","pMS")))
fin.motor.use$TimeStamp <- NULL
fin.motor.use$TimeStamp <- fin.motor.use$pT
fin.motor.use$pMS <- NULL
fin.motor.use$pT <- NULL

## CUSTOM FUNCTION
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# fin.motor.use$ID <- paste(trim(fin.motor.use$who),trim(fin.motor.use$LapName),trim(fin.motor.use$TimeStamp),sep="")

## library(sqldf)
fin.motor.unique <- sqldf("
                    Select DISTINCT
                    who
                    , LapName
                    , TimeStamp
                    , MIN(TimeNum) as Timer
                    , MIN(TimeLag) as TimeLag
                    , AVG(V_GPS) as avg_Speed
                    , MEDIAN(Throttle) as med_Throttle
                    , MEDIAN(Course) as med_Course
                    , MAX(GearChange) as max_GearChange
                    , MAX(RPM_C) as max_EngineRev
                    , MEDIAN(GPS_Yaw) as med_GPSyaw
                    , AVG(Banking_GPS) as avg_BikeBanking
                    , AVG(Latitude) as avg_Latitude
                    , AVG(Longitude) as avg_Longitude
                    , MAX(Gear_Nr) as max_Gear_Nr
                    , AVG(GearChange) as avg_GearChange
                    from
                    'fin.motor.use'
                    group by
                    who
                    , LapName
                    , TimeStamp
                    
                    ")

## Camera
j_camera.n <- fuzzy_join(j_camera, j_camera$TimeStamp, laptime, laptime$Journalist.Start, laptime$Lap)
r_camera.n <- fuzzy_join(r_camera, r_camera$TimeStamp, laptime, laptime$Racer.Start, laptime$Lap)
j_camera.n$who <- "Jounalist"
r_camera.n$who <- "Racer"
fin.camera <- rbind(r_camera.n, j_camera.n)

## Watch
j_watch.n <- fuzzy_join(j_watch, j_watch$TimeStamp, laptime, laptime$Journalist.Start, laptime$Lap)
r_watch.n <- fuzzy_join(r_watch, r_watch$TimeStamp, laptime, laptime$Racer.Start, laptime$Lap)
j_watch.n$who <- "Jounalist"
r_watch.n$who <- "Racer"
fin.watch <- rbind(r_watch.n, j_watch.n)

# Collate Camera and Watch data into One
camwat <- merge(fin.watch, fin.camera, by=c("who","LapName", "TimeStamp"), all=TRUE)
camwat.use <- camwat[c("who", "LapName", "TimeStamp", "HeartRate", "CaloriesBurned_lap", "DistanceMeters8", "LatitudeDegrees.x", "LatitudeDegrees.y", "LongitudeDegrees.x", "LongitudeDegrees.y")]
camwat.unique <- unique(camwat.use)

## MERGE MOTORCYCLE-with-CAMERA-WATCH
fin.match.data <- merge(fin.motor.unique, camwat.unique, by=c("who","LapName", "TimeStamp"), all.x=TRUE)
# Get Matching Sequence
fin.match.data$TimeLag <- ave(fin.match.data$Timer, fin.match.data$who, FUN = seq_along)

write.csv(fin.match.data, "fin.match.data_2015-05-15.csv")
