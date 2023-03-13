library(dplyr)
library(tidyr)
library(lubridate)
library(rgdal)
library(terra)

setwd("~/code/SummitLake_Thesis/") #working directory for linux machine
#setwd("~/Downloads/SummitLake_Thesis") #working directory for Macbook


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

write.csv(fishDataframe, "data/fish/fish_final.csv") #This data will now be imported into qgis to add habitat and other attributes to each fishes point


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

write.csv(inverts, "data/inverts/inverts_final.csv")


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

hab_import <- hab_import %>% #converting percentages of substrate to "dominant substrate type" column
  mutate(dominant_substrate = ifelse(sand>gravel & sand>cobble & sand>boulder & sand>bedrock, "sand", 
                              ifelse(gravel>=sand & gravel>cobble & gravel>boulder & gravel>bedrock, "gravel",
                              ifelse(cobble>sand & cobble>gravel & cobble>boulder & cobble>bedrock, "cobble",
                              ifelse(boulder>sand & boulder>gravel & boulder>cobble & boulder>bedrock, "boulder",
                              ifelse(bedrock>sand & bedrock>gravel & bedrock>cobble & bedrock>boulder, "bedrock", "na"))))))


###Habitat type aliases###
hab_import$channel_type[grepl("SSLS",hab_import$channel_type)]<-"scour_pool" #changing from USFS habitat codes to something less cryptic
hab_import$channel_type[grepl("FNRN",hab_import$channel_type)]<-"run"
hab_import$channel_type[grepl("FTRF",hab_import$channel_type)]<-"riffle"
hab_import$channel_type[grepl("SSMC",hab_import$channel_type)]<-"mid-channel"
hab_import$channel_type[grepl("SSPL",hab_import$channel_type)]<-"plunge_pool"
hab_import$channel_type[grepl("SDDD",hab_import$channel_type)]<-"debris_pool"
hab_import$channel_type[grepl("FTRP",hab_import$channel_type)]<-"rapid"
hab_import$channel_type[grepl("FTCC",hab_import$channel_type)]<-"cascade"
hab_import$channel_type[grepl("FNSH",hab_import$channel_type)]<-"sheet"
hab_import$channel_type[grepl("SDOT",hab_import$channel_type)]<-"debris_pool"


hab_import <- hab_import %>%
  unique() %>%
  select(stream, channel_type, latitude, longitude, max_depth_m, lwd_total, 
         total_shade, undercut_bank_m, dominant_substrate)

write.csv(hab_import, "data/habitat/habitat_final.csv")


###Clean up logger data###
logger_data_formatting <- function(dataset, sitename, start_date="2021-06-09", end_date="2022-11-01"){ #Function to import hobo logger data and make it look all clean and shit
  data <- read.delim2(dataset, skip=7, header=T, sep=",")[-1,c(3,5,6)] %>% 
    mutate_at(c("Temperature","Dissolved.Oxygen"), as.numeric) %>% 
    mutate(date = as.Date(Pacific.Standard.Time, "%Y-%m-%d")) %>% 
    mutate(time_pst = format(as.POSIXct(Pacific.Standard.Time), "%H:%M:%S")) %>% 
    mutate(site = sitename) %>% 
    rename(temperature_c=Temperature, dissolved_oxygen_mgL=Dissolved.Oxygen) %>% 
    group_by(date) %>% 
    mutate(daily_mean_temp_c = mean(temperature_c),
           daily_max_temp_c = quantile(temperature_c, 0.75),
           daily_min_temp_c = quantile(temperature_c, 0.25)) %>% 
    mutate(daily_mean_do_mgL = mean(dissolved_oxygen_mgL),
           daily_max_do_mgL = quantile(dissolved_oxygen_mgL, 0.75),
           daily_min_do_mgL = quantile(dissolved_oxygen_mgL, 0.25)) %>%  
    select(site, date, daily_mean_temp_c, daily_mean_do_mgL) %>% #can add other variables in here
    unique() %>% 
    filter(date >= start_date, date <= end_date) 
  return(data)
}

mg1 <- logger_data_formatting("data/hydrology/minidot_mg1.txt", "mg1", start_date="2021-06-09", end_date="2022-11-01")
mg2 <- logger_data_formatting("data/hydrology/minidot_mg2.txt", "mg2", start_date="2021-06-09", end_date="2022-11-01")
mg3 <- logger_data_formatting("data/hydrology/minidot_mg3.txt", "mg3", start_date="2021-06-09", end_date="2022-11-01")
mg4 <- logger_data_formatting("data/hydrology/minidot_mg4.txt", "mg4", start_date="2021-06-09", end_date="2022-11-01")
mg5 <- logger_data_formatting("data/hydrology/minidot_mg5.txt", "mg5", start_date="2021-06-09", end_date="2022-11-01")
sc1 <- logger_data_formatting("data/hydrology/minidot_sc1.txt", "sc1", start_date="2021-06-09", end_date="2022-11-01")
sc2 <- logger_data_formatting("data/hydrology/minidot_sc2.txt", "sc2", start_date="2021-06-09", end_date="2022-11-01")

logger_final <- rbind(mg2, mg3, mg4, mg5, sc1, sc2)
write.csv(logger_final, "data/hydrology/logger_final.csv")


###Clean Hobo logger data###
test <- read.csv2("data/hydrology/hoboLogger_sc2_corrected.csv")[-1,c(3,7)]
test$Date.Time..PST.PDT. <- parse_date_time(test$Date.Time..PST.PDT., '%m/%d/%Y %H/%M/%S')
test <- test %>% 
  mutate(date = as.Date(Date.Time..PST.PDT., "%m/%d/%Y")) %>% 
  mutate(time_pst = format(as.POSIXct(Date.Time..PST.PDT.), " %H:%M:%S")) %>% 
  mutate(water_level_cm = Water.Level....ft. * 30.48) %>% 
  group_by(date) %>% 
  mutate(daily_mean_level_cm = mean(water_level_cm)) %>% 
  ungroup() %>% 
  distinct(date, .keep_all=T) %>% 
  select(date, daily_mean_level_cm) %>% 
  mutate(change_since_deploy = daily_mean_level_cm - daily_mean_level_cm[1])
  
  
  

hobo_logger <- function(dataset, sitename, start_date, end_date){
  data <- read.csv2(dataset)[-1,c(3,7)] %>%
    mutate(date = as.Date(Date.Time..PSTe.PDT., "%m/%d/%Y"))
}


###Import and join GIS data###
hydroSite <- read.csv2("data/finalData/SummitSites.csv", sep=",")[,c(5:7,12)]
hydroSite$stream <- c("Mahogany Creek", "Mahogany Creek", "Mahogany Creek", "Mahogany Creek", "Mahogany Creek",
                      "Summer Camp Creek", "Summer Camp Creek")

fishData <- read.csv2("data/finalData/SummitFish.csv", sep=",")[,c(4:13)] 
fishData$date <- as.Date(fishData$date, "%Y/%m/%d")

habitatData <- read.csv2("data/finalData/SummitHabitat.csv", sep=",")[,c(4:13)]
habitatData <- habitatData[-231,] #filtering out erroneous data point




