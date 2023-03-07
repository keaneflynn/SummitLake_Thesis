library(dplyr)
library(tidyr)
library(lubridate)

setwd("~/code/SummitLake_Thesis/") #working directory for linux machine
#setwd("~/Downloads/SummitLake_Thesis")


###Fish Tracking###
fishTracking_2021 <- read.csv2("data/fish/fishTracking_2021.csv", sep=",")
fishTracking_2022 <- read.csv2("data/fish/fishTracking_2022.csv", sep=",")

fishTracking <- rbind(fishTracking_2021, fishTracking_2022) 
fishTracking$date <- as.Date(fishTracking$date, "%m/%d/%Y")
fishTracking$trip_number <- as.numeric(fishTracking$trip_number)
options(digits=8)
fishTracking$latitude <- as.numeric(fishTracking$latitude)
fishTracking$longitude <- as.numeric(fishTracking$longitude)
fishTracking$julian_days <- yday(fishTracking$date)
fishTracking$year <- year(fishTracking$date)

###Fish Capture###
fishCapture_2021 <- read.csv2('data/fish/fishCapture_2021.csv', sep=",")[,c(1:6)]
fishCapture_2022 <- read.csv2("data/fish/fishCapture_2022.csv", sep=",")[,c(1:6)]

fishCapture <- rbind(fishCapture_2021, fishCapture_2022)[c(1:56),]
fishCapture$date_tagged <- as.Date(fishCapture$date_tagged, "%m/%d/%Y")
fishCapture$year <- year(fishCapture$date_tagged)

###Grouping Datasets together###
fishDataframe <- fishTracking %>% full_join(fishCapture, by=c("tag_id", "year"))
fishDataframe <- fishDataframe %>% 
  select(date, julian_days, year, trip_number, tag_id, fork_length_mm, 
         weight_g, latitude.x, longitude.x) %>% 
  rename(latitude = latitude.x, longitude = longitude.x)
fishDataframe$fork_length_mm <- as.numeric(fishDataframe$fork_length_mm)
fishDataframe$weight_g <- as.numeric(fishDataframe$weight_g) #Final fish dataset

write.csv2(fishDataframe, "data/fish/finalFish_dataset.csv") #This data will now be imported into qgis to add habitat and other attributes to each fishes point

