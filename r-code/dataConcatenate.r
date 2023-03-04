library(dplyr)
library(tidyr)
library(lubridate)

setwd("~/code/SummitLake_Thesis/") #working directory for linux machine
#setwd("~/Downloads/SummitLake_Thesis")


###Import datasets###
fishTracking_2021 <- read.csv2("data/fish/fishTracking_2021.csv", sep=",", )
fishTracking_2022 <- read.csv2("data/fish/fishTracking_2022.csv", sep=",")

fishTracking <- rbind(fishTracking_2021, fishTracking_2022) 
fishTracking$date <- as.Date(fishTracking$date, "%m/%d/%Y")
fishTracking$trip_number <- as.numeric(fishTracking$trip_number)
options(digits=8)
fishTracking$latitude <- as.numeric(fishTracking$latitude)
fishTracking$longitude <- as.numeric(fishTracking$longitude)


 