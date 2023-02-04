library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)
setwd("~/code/SummitLake_Thesis/")
 
###Global Variables###
date_range <- c(as.Date("2022-05-12"), 
                as.Date("2022-11-16"))

month_dates <- data.frame(dates=c(as.Date("2022-06-01"), as.Date("2022-07-01"), 
                                  as.Date("2022-08-01"), as.Date("2022-09-01"),
                                  as.Date("2022-10-01"), as.Date("2022-11-01")))

sample_dates <- data.frame(
  x=c(as.Date("2022-08-09"), as.Date("2022-08-16"), as.Date("2022-08-24"),
      as.Date("2022-9-07"), as.Date("2022-10-04"), as.Date("2022-11-01")),
  y=28,
  xend=c(as.Date("2022-08-09"), as.Date("2022-08-16"), as.Date("2022-08-24"),
         as.Date("2022-9-07"), as.Date("2022-10-04"), as.Date("2022-11-01")),
  yend=20)


###Functions###  
minidot_temp <- function(dataset, sitename, start_date, end_date) {
  data <- read.delim2(dataset, skip=7, header=T, 
                      sep=",")[-1,c(3,5,6)] %>% 
    rename(pacific_standard_time=Pacific.Standard.Time, 
           water_temp_c=Temperature,
           dissolved_oxygen_mgL=Dissolved.Oxygen)
  data$water_temp_c <- as.double(data$water_temp_c)
  data$dissolved_oxygen_mgL <-as.double(data$dissolved_oxygen_mgL)
  data$date <- as.Date(data$pacific_standard_time)
  data$time_pst <- format(as.POSIXct(data$pacific_standard_time), format = "%H:%M:%S")
  data <- select(data, date, time_pst, water_temp_c, dissolved_oxygen_mgL) %>% 
    filter(date >= start_date, date <= end_date) %>% 
    group_by(date) %>% 
    mutate(daily_mean_temp = mean(water_temp_c),
           daily_mean_DO = mean(dissolved_oxygen_mgL),
           sd_temp = sd(water_temp_c),
           sd_DO = sd(dissolved_oxygen_mgL),
           low_range_temp = daily_mean_temp - sd_temp,
           high_range_temp = daily_mean_temp + sd_temp,
           low_range_DO = daily_mean_DO - sd_DO,
           high_range_DO = daily_mean_DO + sd_DO)
  data <- subset(data, water_temp_c < high_range_temp & water_temp_c > low_range_temp)
  data <- subset(data, dissolved_oxygen_mgL < high_range_DO & dissolved_oxygen_mgL > low_range_DO)
  data <- select(data, date, time_pst, water_temp_c, dissolved_oxygen_mgL)
  g <- ggplot(data, aes(date, water_temp_c)) + 
       geom_boxplot(aes(group=date)) +
       ggtitle(sitename) +
       theme(plot.title = element_text(hjust = 0.5)) +
       scale_x_date(limits=date_range) +
       geom_segment(data=sample_dates, aes(x=x, y=y, xend=xend, yend=yend),
                    arrow=arrow(length=unit(0.3,"cm")),
                    color="Blue") +
       geom_vline(data=month_dates, aes(xintercept=dates),
                  linetype="dotted")
  return(g)
}

minidot_do <- function(dataset, sitename, start_date, end_date) {
  data <- read.delim2(dataset, skip=7, header=T, 
                      sep=",")[-1,c(3,5,6)] %>% 
    rename(pacific_standard_time=Pacific.Standard.Time, 
           water_temp_c=Temperature,
           dissolved_oxygen_mgL=Dissolved.Oxygen)
  data$water_temp_c <- as.double(data$water_temp_c)
  data$dissolved_oxygen_mgL <-as.double(data$dissolved_oxygen_mgL)
  data$date <- as.Date(data$pacific_standard_time)
  data$time_pst <- format(as.POSIXct(data$pacific_standard_time), format = "%H:%M:%S")
  data <- select(data, date, time_pst, water_temp_c, dissolved_oxygen_mgL) %>% 
  filter(date >= start_date, date <= end_date) %>% 
    group_by(date) %>% 
    mutate(daily_mean_temp = mean(water_temp_c),
           daily_mean_DO = mean(dissolved_oxygen_mgL),
           sd_temp = sd(water_temp_c),
           sd_DO = sd(dissolved_oxygen_mgL),
           low_range_temp = daily_mean_temp - sd_temp,
           high_range_temp = daily_mean_temp + sd_temp,
           low_range_DO = daily_mean_DO - sd_DO,
           high_range_DO = daily_mean_DO + sd_DO)
  data <- subset(data, water_temp_c < high_range_temp & water_temp_c > low_range_temp)
  data <- subset(data, dissolved_oxygen_mgL < high_range_DO & dissolved_oxygen_mgL > low_range_DO)
  data <- select(data, date, time_pst, water_temp_c, dissolved_oxygen_mgL)
  g <- ggplot(data, aes(date, dissolved_oxygen_mgL)) + 
       geom_boxplot(aes(group=date)) +
       ggtitle(sitename) +
       theme(plot.title = element_text(hjust = 0.5)) +
       scale_x_date(limits=date_range) +
       geom_segment(data=sample_dates, aes(x=x, y=y, xend=xend, yend=yend),
                    arrow=arrow(length=unit(0.3,"cm")),
                    color="Blue") +
       geom_vline(data=month_dates, aes(xintercept=dates),
                  linetype="dotted")
  return(g)
}

iButton_temp <- function(dataset, sitename, start_date, end_date){
  data <- read.csv2(dataset, 
                    skip = 20, header = F, sep = ",")[,c(1,2,4)]
  colnames(data) <- c("date", "time_pst", "air_temp_c")
  data$date <- mdy(data$date)
  data$time_pst <- parse_date_time(data$time_pst, '%I:%M:%S %p')
  data$time_pst <- format(as.POSIXct(data$time_pst), format = "%H:%M:%S")
  data$air_temp_c <- as.double(data$air_temp_c)
  data <- select(data, date, time_pst, air_temp_c) %>% 
  filter(date >= start_date, date <= end_date)
  g <- ggplot(data, aes(date, air_temp_c)) + 
       geom_boxplot(aes(group=date)) +
       ggtitle(sitename) +
       theme(plot.title = element_text(hjust = 0.5)) +
       scale_x_date(limits=date_range) +
       geom_vline(data=month_dates, aes(xintercept=dates),
                  linetype="dotted")
  return(g)
}

hoboLogger <- function(dataset, sitename, start_date, end_date){
  data <- read.csv2(dataset, sep=",")[-1,c(2,5,6)] 
  data$Date.Time..PST.PDT. <- parse_date_time(data$Date.Time..PST.PDT., '%m/%d/%Y %H/%M/%S')
  data$date <- as.Date(data$Date.Time..PST.PDT.)
  data$time_pst <- format(as.POSIXct(data$Date.Time..PST.PDT.), format = "%H:%M:%S") 
  data$Ch..3...Temperature.....F.. <- as.double(data$Ch..3...Temperature.....F..) 
  data$Ch..3...Temperature.....F.. <- ((data$Ch..3...Temperature.....F.. - 32)*(5/9))
  data$Water.Level....ft. <- as.double(data$Water.Level....ft.)
  data$Water.Level....ft. <- data$Water.Level....ft. * 30.48
  data <- select(data, date, time_pst, Ch..3...Temperature.....F.., Water.Level....ft.) 
  data <- rename(data, date=date, time_pst=time_pst, water_temp_c=Ch..3...Temperature.....F.., 
                 water_level_cm=Water.Level....ft.) %>% 
  filter(date >= start_date, date <= end_date) %>% 
    group_by(date) %>% 
    mutate(daily_mean_water_level = mean(water_level_cm),
           sd_water_level = sd(water_level_cm),
           high_range_level = daily_mean_water_level + sd_water_level,
           low_range_level = daily_mean_water_level - sd_water_level)
  data <- subset(data, water_level_cm < high_range_level & water_level_cm > low_range_level)
  data <- select(data, date, time_pst, water_level_cm)
  g <- ggplot(data, aes(date, water_level_cm)) +
       geom_boxplot(aes(group=date)) +
       #geom_hline(aes(yintercept=data[1,3]), colour="blue") +
       ggtitle(sitename) +
       theme(plot.title = element_text(hjust = 0.5)) +
       scale_x_date(limits=date_range) +
       geom_vline(data=month_dates, aes(xintercept=dates),
                  linetype="dotted")
  return(g)
}
###End Functions###

hoboLogger(dataset="data/hydrology/hoboLogger_mg4.csv", sitename="MG4",
           start_date="2022-05-12", end_date="2022-08-30")#5/12/2022-11/2/2022
hoboLogger(dataset="data/hydrology/hoboLogger_mg5.csv", sitename="MG5",
           start_date="2022-07-05", end_date="2022-11-15")#7/5/2022-11/15/2022
hoboLogger(dataset="data/hydrology/hoboLogger_sc1.csv", sitename="SC1",
           start_date="2022-05-12", end_date="2022-11-02")#5/12/2022-11/2/2022
hoboLogger(dataset="data/hydrology/hoboLogger_sc2.csv", sitename="SC2",
           start_date="2022-07-05", end_date="2022-11-16")#7/5/2022-11/16/2022

iButton_temp(dataset="data/airTemp/iButton_mg2_2022.csv", sitename="MG2",
             start_date="2022-05-12", end_date="2022-11-01")#5/12/2022-11/1/2022
iButton_temp(dataset="data/airTemp/iButton_mg3_2022.csv", sitename="MG3",
             start_date="2022-05-12", end_date="2022-11-01")#5/12/2022-11/1/2022
iButton_temp(dataset="data/airTemp/iButton_mg4_2022.csv", sitename="MG4",
             start_date="2022-05-12", end_date="2022-09-07")#5/12/2022-9/7/2022
iButton_temp(dataset="data/airTemp/iButton_mg5_2022.csv", sitename="MG5",
             start_date="2022-07-05", end_date="2022-11-15")#7/5/2022-11/15/2022
iButton_temp(dataset="data/airTemp/iButton_sc2_2022.csv", sitename="SC2",
             start_date="2022-07-05", end_date="2022-11-16")#7/5/2022-11/16/2022

#minidot_temp(dataset = "data/hydrology/minidot_mg1.txt", sitename = "Mahogany Creek Site 1",
#             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_temp(dataset = "data/hydrology/minidot_mg2.txt", sitename = "Mahogany Creek Site 2",
             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_temp(dataset = "data/hydrology/minidot_mg3.txt", sitename = "Mahogany Creek Site 3",
             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_temp(dataset = "data/hydrology/minidot_mg4.txt", sitename = "Mahogany Creek Site 4",
             start_date="2022-05-12", end_date="2022-09-07")#5/12/2022-9/7/2022
minidot_temp(dataset = "data/hydrology/minidot_mg5.txt", sitename = "Mahogany Creek Site 5",
             start_date="2022-07-05", end_date="2022-11-14")#7/5/2022-11/15/2022
minidot_temp(dataset = "data/hydrology/minidot_sc1.txt", sitename = "Summer Camp Creek Site 1",
             start_date="2022-05-12", end_date="2022-11-01")#5/12/2022-11/2/2022
minidot_temp(dataset = "data/hydrology/minidot_sc2.txt", sitename = "Summer Camp Creek Site 2",
             start_date="2022-07-05", end_date="2022-11-15")#7/5/2022-11/16/2022

#minidot_do(dataset = "data/hydrology/minidot_mg1.txt", sitename = "MG1",
#             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_do(dataset = "data/hydrology/minidot_mg2.txt", sitename = "MG2",
             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_do(dataset = "data/hydrology/minidot_mg3.txt", sitename = "MG3",
             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_do(dataset = "data/hydrology/minidot_mg4.txt", sitename = "MG4",
             start_date="2022-05-12", end_date="2022-09-07")#5/12/2022-9/7/2022
minidot_do(dataset = "data/hydrology/minidot_mg5.txt", sitename = "MG5",
             start_date="2022-07-05", end_date="2022-11-14")#7/5/2022-11/15/2022
minidot_do(dataset = "data/hydrology/minidot_sc1.txt", sitename = "SC1",
             start_date="2022-05-12", end_date="2022-11-01")#5/12/2022-11/2/2022
minidot_do(dataset = "data/hydrology/minidot_sc2.txt", sitename = "SC2",
             start_date="2022-07-05", end_date="2022-11-15")#7/5/2022-11/16/2022
