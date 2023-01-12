library(dplyr)
library(lubridate)

setwd('/home/keane/R/SummitLake/')
movement_df <- read.csv('data/SummitFishData.csv')
tagging_df <- read.csv('data/FishCaptureData.csv')
head(movement_df)
movement_df <- movement_df %>% 
  select(date,trip_number=trip_numbe,
         tag_id,lake_distance_m=LakeDist,
         signal_str,gain,notes) %>% 
  arrange(tag_id,trip_number)
  
head(movement_df)
vectors = c(12,13,15,18,21,24,29,31,38)
test <- movement_df$tag_id
which(test == 12)
which(test == 13)
which(test == 15)
which(test == 18)
which(test == 21)
which(test == 24)
#which(test == 29) #not on summer camp creek
which(test == 31)
which(test == 38)
loopList <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
              19,20,21,22,23,24,35,36,37,38,39,40,41,
              50,51,52,53,54,55,56,57,58,68,69,70,71,
              72,97,98,133,134)

for (i in loopList) {
  movement_df$lake_distance_m[i] <- movement_df$lake_distance_m[i] + 8383 #adding distance of first creek for fork in creek
}

df <- movement_df %>% 
  group_by(tag_id) %>% 
  mutate(lake_distance_m_dif = lake_distance_m - lag(lake_distance_m,default=first(lake_distance_m))) %>% 
  mutate(direction_travelled = ifelse(lake_distance_m_dif>0,'US','DS')) %>% 
  mutate(distance_travelled = abs(lake_distance_m_dif)) %>% 
  mutate(avgDistance_travelled = sum(distance_travelled)/(length(distance_travelled)-1)) %>% 
  #filter(!trip_number==0) %>% 
  mutate(max_detections=max(trip_number)) %>% 
  mutate(ds_movement = ifelse(direction_travelled=='DS',1,0)) %>% 
  mutate(us_movement = ifelse(direction_travelled=='US',1,0)) #%>% 
  #mutate(weekly_distance_travelled = lake_distance_m_dif / ) #distance traveled per week / trip number

df <- left_join(df, tagging_df, by="tag_id") %>% 
  select(date=date.x,species,trip_number,
         tag_id,fork_length,weight_g,lake_distance_m,
         direction_travelled,distance_travelled,avgDistance_travelled,
         max_detections,ds_movement,us_movement)

write.csv(df, "Fish_Movements.csv")
model1 <- lm(df$avgDistance_travelled~df$max_detections)
summary(model1)

testdf <- df %>% filter(max_detections>=3)

#Average weekly distance (break into US vs DS)
plot(df$fork_length,df$avgDistance_travelled,xlim=c(120,240),
     xlab='Fork Length (mm)',ylab='Average Weekly Distance Travelled',
     main='Average Weekly Distance vs Fork length')

#plot1_us <- testdf %>% filter(direction_travelled == 'US')
#plot(plot1_us$fork_length,plot1_us$avgDistance_travelled,xlim=c(120,240),
#     xlab='Fork Length (mm)',ylab='Average Weekly Distance Travelled',
#     main='Average Weekly Distance vs Fork length')

#plot1_ds <- testdf %>% filter(direction_travelled == 'DS')
#plot(plot1_ds$fork_length,plot1_ds$avgDistance_travelled,xlim=c(120,240),
#     xlab='Fork Length (mm)',ylab='Average Weekly Distance Travelled',
#     main='Average Weekly Distance vs Fork length')


boxplot(distance_travelled ~ direction_travelled, data=df)

#Distance from lake vs weekly movement (break into US vs DS)

plot(df$lake_distance_m,df$avgDistance_travelled,xlim=c(5500,13000),
     xlab='Distance from Lake (m)',ylab='Average Weekly Distance Travelled',
     main='Distance from Lake vs Weekly Movement')
#Distance travelled between tracking events (boxplots dude)
hist(unique(df$avgDistance_travelled),breaks=25,xlim=c(0,200),
     xlab='Average Weekly Distance Travelled (m)', 
     main='Histogram of Distance Travelled \n between Tracking Events')
#Fish size distribution (boxplots again)
hist(unique(df$fork_length),breaks=15,
     xlab='Fork Length (mm)',main='Fish Size Distribution')

barplot(c(sum(df$ds_movement),sum(df$us_movement)),width=c(1,1),
        names.arg=c('DS','US'),xlab='Directional Movements',
        ylab='Number of Movements',col=c('red','blue'),
        main='Total Directional Movements')



