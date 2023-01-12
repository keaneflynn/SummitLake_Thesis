setwd('/home/keane/R')
getwd()
library(dplyr)

channel_form_raw <- read.csv('summitHabitat/MCHabitatData2015_JS - Channel Unit Form.csv')
channel_form <- channel_form_raw[-c(1,2,3),-c(1,2,3,8,9:15,18:20,22:28,31:38)]
names(channel_form) <- c('stream','Reach..','SO','Channel.Unit.and.Type', 
                         'unit_length_m','wetted_width_m','max_depth_m',
                         'pool_crest_depth_bf_m','formed_by')
#View(channel_form)

coordinates <- read.csv('summitHabitat/MCHabitatData2015_JS - Comments.csv', skip = 1)
coordinates <- coordinates[-c(421, 442),]
coordinates$SO <- as.numeric(coordinates$SO)
#View(coordinates)

vegetation <- read.csv('summitHabitat/MCHabitatData2015_JS - Vegetation .csv')
vegetation <- vegetation[,-c(11,12,16,17,22,23,28,29)]
names(vegetation)[4] <- 'SO'
#View(vegetation)

wolmann <- read.csv('summitHabitat/MCHabitatData2015_JS - Wolman.csv', skip = 1) 
wolmann <- wolmann[,-c(26:28)] %>% na.omit()
#View(wolmann)

flow <- read.csv('summitHabitat/MCHabitatData2015_JS - Flow.csv', skip = 1) 
flow <- flow[,-c(12,13)] %>% na.omit()
#View(flow)

df <- left_join(channel_form, coordinates, by = c("Reach..", "SO"))
df <- left_join(df, vegetation, by = c("Reach..", "SO"))
df <- left_join(df, wolmann, by = c("Reach..", "SO"))
df <- left_join(df, flow, by = c("Reach..", "SO"))
df_output <- df %>% select(stream,reach=Reach..,seq_order=SO,channel_type=Channel.Unit.and.Type.x,northing=Latitude.Northing,easting=Longitude.Easting,
                           unit_length_m,wetted_width_m,max_depth_m,pool_crest_depth_bf_m,LWD_small,LWD_med,LWD_large,root_debris,riparian_shade,total_shade,
                           fish_present,undercut_LB_m,undercut_RB_m,sand=Sand,gravel=Gravel,cobble=Cobble,boulder=Boulder,bedrock=Bedrock,
                           dominantSizeClass_LB,dominantSizeClass_RB,understoryVeg_NV_LB=understoryVeg_.NV_LB,understoryVeg_GF_LB=understoryVeg_.GF_LB,
                           understoryVeg_SR_LB=understoryVeg_.SR_LB,understoryVeg_NV_RB=understoryVeg_.NV_RB,understoryVeg_GF_RB=understoryVeg_.GF_RB,
                           understoryVeg_SR_RB=understoryVeg_.SR_RB,overstoryVeg_SB_LB=overstoryVeg_.SB_LB,overstoryVeg_HW_LB=overstoryVeg_.HW_LB,
                           overstoryVeg_HQ_LB=overstoryVeg_.HQ_LB,overstoryVeg_WS_LB=overstoryVeg_.WS_LB,overstoryVeg_SB_RB=overstoryVeg_.SB_RB,
                           overstoryVeg_HW_RB=overstoryVeg_.HW_RB,overstoryVeg_HQ_RB=overstoryVeg_.HQ_RB,overstoryVeg_WS_RB=overstoryVeg_.WS_RB)
View(df_output)

write.csv(df_output,"Summit_Habitat.csv")




