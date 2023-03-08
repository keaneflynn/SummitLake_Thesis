library(dplyr)
library(tidyr)
library(lubridate)
library(rgdal)
library(terra)

setwd("~/code/SummitLake_Thesis/") #working directory for linux machine
#setwd("~/Downloads/SummitLake_Thesis")


###Fish Tracking###
fishTracking_2021 <- read.csv2("data/fish/fishTracking_2021.csv", sep=",")
fishTracking_2022 <- read.csv2("data/fish/fishTracking_2022.csv", sep=",")

fishTracking <- rbind(fishTracking_2021,fishTracking_2022) 
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

fishCapture <- rbind(fishCapture_2021,fishCapture_2022)[c(1:56),]
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

write.csv2(fishDataframe, "data/fish/fish_final.csv") #This data will now be imported into qgis to add habitat and other attributes to each fishes point


###Invertebrate data concatenation & manipulation###
field_inverts_2021 <- read.csv2("data/inverts/invertSampling_2021.csv", sep=",")
field_inverts_2022 <- read.csv2("data/inverts/invertSampling_2022.csv", sep=",")
field_inverts <- rbind(field_inverts_2021,field_inverts_2022)[,c(1,3:8,12)]
field_inverts$site_id <- tolower(field_inverts$site_id)
field_inverts$date <- as.Date(field_inverts$date, "%m/%d/%Y")
field_inverts[c(1,2,5,6),1] <- c("2021-07-20","2021-07-20","2021-07-19","2021-07-19")


lab_inverts <- read.csv2("data/inverts/processedInverts.csv", sep=",")[,c(2:5,8)] %>% 
  rename(date=sample_date, site_id=sample_location)
lab_inverts$date <- as.Date(lab_inverts$date, "%m/%d/%Y")
lab_inverts$invert_mass_mg <- as.numeric(lab_inverts$invert_mass_mg)
lab_inverts[c(25,26),1] <- c("2022-08-17", "2022-08-17")


inverts <- field_inverts %>% full_join(lab_inverts, by=c("site_id", "date")) %>% 
  na.omit() %>% 
  select(date, site_id, terrestrial_aquatic, latitude, longitude, net_depth_cm, 
         net_width_cm, velocity_fps, total_time, invert_mass_mg) %>% 
  mutate_at(c('latitude','longitude', 'net_depth_cm', 'net_width_cm', 'velocity_fps', 'total_time'), as.numeric) %>% 
  mutate(invertMass_m3 = invert_mass_mg * 3.281 * (1/3600) * (1000/(net_depth_cm * net_width_cm)) * (1/total_time) * (1/velocity_fps)) #converts dry packet weight to drift mass per volume of water

write.csv2(inverts, "data/inverts/inverts_final.csv")


###Habitat data cleansing###
hab_import <- read.csv2("data/habitat/Summit_Habitat.csv", sep=",")[,c(2,5:7,10,12:15,17,19:25)] %>% na.omit()

coords_utm <- cbind(hab_import$northing, hab_import$easting) #convert UTM zone 11 coords to lat long WGS84 projection
coords_temp <- project(vect(coords_utm, crs="+proj=utm +zone=11 +datum=WGS84  +units=m"),
             "+proj=longlat +datum=WGS84")
longlat <- geom(coords_temp)[,c("x", "y")]
hab_import$latitude <- longlat[,2]
hab_import$longitude <- longlat[,1]

hab_import <- hab_import[,-c(3,4)] %>% 
  rowwise() %>% 
  mutate(lwd_total = sum(LWD_small, LWD_med, LWD_large, root_debris)) %>% 
  mutate_at(c("undercut_RB_m","undercut_LB_m","max_depth_m"), as.numeric) %>% 
  mutate(undercut_bank_m = sum(undercut_LB_m,undercut_RB_m))
  
#hab_import$channel_type <- replace(hab_import$channel_type, hab_import$channel_type=="FNRN", "run") #transform data


