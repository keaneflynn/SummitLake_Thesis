library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)
setwd("~/code/SummitLake_Thesis/")

###Hobo logger Corrections###

mg4 <- read.csv2("data/hydrology/hoboLogger_mg4.csv", sep=",") 
  mg4$Water.Level....ft. <- as.numeric(mg4$Water.Level....ft.) 
  mg4$Water.Level....ft. <- mg4$Water.Level....ft. + 0.4327 #0.5855
write.csv2(mg4, file="data/hydrology/hoboLogger_mg4_corrected.csv")

mg5 <- read.csv2("data/hydrology/hoboLogger_mg5.csv", sep=",") #no corrections needed
  mg5$Water.Level....ft. <- as.numeric(mg5$Water.Level....ft.) 
write.csv2(mg5, file="data/hydrology/hoboLogger_mg5_corrected.csv")

sc1 <- read.csv2("data/hydrology/hoboLogger_sc1.csv", sep=",") #no corrections needed
  sc1$Water.Level....ft. <- as.numeric(sc1$Water.Level....ft.)
write.csv2(sc1, "data/hydrology/hoboLogger_sc1_corrected.csv")

sc2 <- read.csv2("data/hydrology/hoboLogger_sc2.csv", sep=",")
  sc2$Water.Level....ft. <- as.numeric(sc2$Water.Level....ft.)
  sc2$Water.Level....ft. <- sc2$Water.Level....ft. + 0.0819
write.csv2(sc2, "data/hydrology/hoboLogger_sc2_corrected.csv")
 
##Global Variables###
date_range <- c(as.Date("2022-05-12"), 
                as.Date("2022-11-16"))

month_dates <- data.frame(dates=c(as.Date("2022-06-01"), as.Date("2022-07-01"), 
                                  as.Date("2022-08-01"), as.Date("2022-09-01"),
                                  as.Date("2022-10-01"), as.Date("2022-11-01")))

sample_dates <- data.frame(x=c(as.Date("2022-08-09"), as.Date("2022-08-16"), as.Date("2022-08-24"),
                               as.Date("2022-9-07"), as.Date("2022-10-04"), as.Date("2022-11-01")),
                           y=28,
                           xend=c(as.Date("2022-08-09"), as.Date("2022-08-16"), as.Date("2022-08-24"),
                                  as.Date("2022-9-07"), as.Date("2022-10-04"), as.Date("2022-11-01")),
                           yend=18)

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
  data <- select(data, date, time_pst, water_temp_c, dissolved_oxygen_mgL, daily_mean_temp)
  g <- ggplot(data, aes(date, daily_mean_temp)) + 
       geom_line() +
       ggtitle(sitename) +
       theme(plot.title = element_text(hjust = 0.5)) +
       scale_x_date(limits=date_range) +
       geom_segment(data=sample_dates, aes(x=x, y=y, xend=xend, yend=yend),
                    arrow=arrow(length=unit(0.3,"cm")),
                    col="#36454f") +
       geom_vline(data=month_dates, aes(xintercept=dates),
                  linetype="dotted") +
       xlab("Date") +
       ylab("Water Temperature (C)") +
       ylim(0,30) +
       geom_hline(aes(yintercept=20, linetype="Bioenergetic Threshold \n(Dickerson & Vinyard 1999)"), 
                  colour="red") +
       scale_linetype_manual(name="", values=2, 
                             guide=guide_legend(override.aes=list(color="red")))
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
  data <- select(data, date, time_pst, water_temp_c, dissolved_oxygen_mgL, daily_mean_DO)
  g <- ggplot(data, aes(date, daily_mean_DO)) + 
       geom_line() +
       ggtitle(sitename) +
       theme(plot.title = element_text(hjust = 0.5)) +
       scale_x_date(limits=date_range) +
       geom_segment(data=sample_dates, aes(x=x, y=12.5, xend=xend, yend=11),
                    arrow=arrow(length=unit(0.3,"cm")),
                    col="#36454f") +
       geom_vline(data=month_dates, aes(xintercept=dates),
                  linetype="dotted") +
       xlab("Date") +
       ylab("Dissolved Oxygen (mg/L)") +
       ylim(3,13) +
       geom_hline(aes(yintercept=5, linetype="DO Threshold \n(Doudoroff & Shumway 1970)"), 
                  colour="red") +
       scale_linetype_manual(name="", values=2, 
                             guide=guide_legend(override.aes=list(color="red")))
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
  data <- read.csv2(dataset)[-1,c(3,6,7)] 
  data$Date.Time..PST.PDT. <- parse_date_time(data$Date.Time..PST.PDT., '%m/%d/%Y %H/%M/%S')
  data$date <- as.Date(data$Date.Time..PST.PDT.)
  data$time_pst <- format(as.POSIXct(data$Date.Time..PST.PDT.), format = "%H:%M:%S") 
  data$Ch..3...Temperature.....F.. <- as.double(data$Ch..3...Temperature.....F..) 
  data$Ch..3...Temperature.....F.. <- ((data$Ch..3...Temperature.....F.. - 32)*(5/9))
  #data$Water.Level....ft. <- as.double(data$Water.Level....ft.)
  data$Water.Level....ft. <- data$Water.Level....ft. * 30.48
  data <- select(data, date, time_pst, Ch..3...Temperature.....F.., Water.Level....ft.) 
  data <- rename(data, date=date, time_pst=time_pst, water_temp_c=Ch..3...Temperature.....F.., 
                 water_level_cm=Water.Level....ft.) %>% 
  filter(date >= start_date, date <= end_date) %>% 
    group_by(date) %>% 
    mutate(daily_mean_water_level = mean(water_level_cm),
           sd_water_level = sd(water_level_cm),
           high_range_level = daily_mean_water_level + sd_water_level,
           low_range_level = daily_mean_water_level - sd_water_level) %>% 
    ungroup()
  data <- subset(data, water_level_cm < high_range_level & water_level_cm > low_range_level)
  data <- select(data, date, time_pst, water_level_cm, daily_mean_water_level)
  g <- ggplot(data, aes(date, daily_mean_water_level)) +
       geom_line() + #aes(group=date)
       geom_hline(aes(yintercept=as.numeric(data[1,3])), colour="blue") +
       ggtitle(sitename) +
       xlab("Date") +
       ylab("Stage measurement (cm)") +
       theme(plot.title = element_text(hjust = 0.5)) +
       scale_x_date(limits=date_range) +
       geom_vline(data=month_dates, aes(xintercept=dates),
                  linetype="dotted")
  return(g)
}
###End Functions###

hoboLogger(dataset="data/hydrology/hoboLogger_mg4_corrected.csv", sitename="Mahogany Creek Site 4: Water Level (cm)",
           start_date="2022-05-12", end_date="2022-08-30")#5/12/2022-11/2/2022
hoboLogger(dataset="data/hydrology/hoboLogger_mg5_corrected.csv", sitename="Mahogany Creek Site 5: Water Level (cm)",
           start_date="2022-07-05", end_date="2022-11-15")#7/5/2022-11/15/2022
hoboLogger(dataset="data/hydrology/hoboLogger_sc1_corrected.csv", sitename="Summer Camp Creek Site 1: Water Level (cm)",
           start_date="2022-05-12", end_date="2022-11-02")#5/12/2022-11/2/2022
hoboLogger(dataset="data/hydrology/hoboLogger_sc2_corrected.csv", sitename="Summer Camp Creek Site 2: Water Level (cm)",
           start_date="2022-07-05", end_date="2022-11-16")#7/5/2022-11/16/202

iButton_temp(dataset="data/airTemp/iButton_mg2_2022.csv", sitename="Mahogany Creek Site 2: Air Temperature (C)",
             start_date="2022-05-12", end_date="2022-11-01")#5/12/2022-11/1/2022
iButton_temp(dataset="data/airTemp/iButton_mg3_2022.csv", sitename="Mahogany Creek Site 3: Air Temperature (C)",
             start_date="2022-05-12", end_date="2022-11-01")#5/12/2022-11/1/2022
iButton_temp(dataset="data/airTemp/iButton_mg4_2022.csv", sitename="Mahogany Creek Site 4: Air Temperature (C)",
             start_date="2022-05-12", end_date="2022-09-07")#5/12/2022-9/7/2022
iButton_temp(dataset="data/airTemp/iButton_mg5_2022.csv", sitename="Mahogany Creek Site 5: Air Temperature (C)",
             start_date="2022-07-05", end_date="2022-11-15")#7/5/2022-11/15/2022
iButton_temp(dataset="data/airTemp/iButton_sc2_2022.csv", sitename="Summer Camp Creek Site 2: Air Temperature (C)",
             start_date="2022-07-05", end_date="2022-11-16")#7/5/2022-11/16/2022

#minidot_temp(dataset = "data/hydrology/minidot_mg1.txt", sitename = "Mahogany Creek Site 1",
#             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_temp(dataset = "data/hydrology/minidot_mg2.txt", sitename = "Mahogany Creek Site 2: Water Temperature (C)",
             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_temp(dataset = "data/hydrology/minidot_mg3.txt", sitename = "Mahogany Creek Site 3: Water Temperature (C)",
             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_temp(dataset = "data/hydrology/minidot_mg4.txt", sitename = "Mahogany Creek Site 4: Water Temperature (C)",
             start_date="2022-05-12", end_date="2022-09-07")#5/12/2022-9/7/2022
minidot_temp(dataset = "data/hydrology/minidot_mg5.txt", sitename = "Mahogany Creek Site 5: Water Temperature (C)",
             start_date="2022-07-05", end_date="2022-11-14")#7/5/2022-11/15/2022
minidot_temp(dataset = "data/hydrology/minidot_sc1.txt", sitename = "Summer Camp Creek Site 1: Water Temperature (C)",
             start_date="2022-05-12", end_date="2022-11-01")#5/12/2022-11/2/2022
minidot_temp(dataset = "data/hydrology/minidot_sc2.txt", sitename = "Summer Camp Creek Site 2: Water Temperature (C)",
             start_date="2022-07-05", end_date="2022-11-15")#7/5/2022-11/16/2022

#minidot_do(dataset = "data/hydrology/minidot_mg1.txt", sitename = "Mahogany Creek Site 1",
#             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_do(dataset = "data/hydrology/minidot_mg2.txt", sitename = "Mahogany Creek Site 2: Dissolved Oxygen (mg/L)",
             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_do(dataset = "data/hydrology/minidot_mg3.txt", sitename = "Mahogany Creek Site 3: Dissolved Oxygen (mg/L)",
             start_date="2022-05-12", end_date="2022-10-31")#5/12/2022-11/1/2022
minidot_do(dataset = "data/hydrology/minidot_mg4.txt", sitename = "Mahogany Creek Site 4: Dissolved Oxygen (mg/L)",
             start_date="2022-05-12", end_date="2022-09-07")#5/12/2022-9/7/2022
minidot_do(dataset = "data/hydrology/minidot_mg5.txt", sitename = "Mahogany Creek Site 5: Dissolved Oxygen (mg/L)",
             start_date="2022-07-05", end_date="2022-11-14")#7/5/2022-11/15/2022
minidot_do(dataset = "data/hydrology/minidot_sc1.txt", sitename = "Summer Camp Creek Site 1: Dissolved Oxygen (mg/L)",
             start_date="2022-05-12", end_date="2022-11-01")#5/12/2022-11/2/2022
minidot_do(dataset = "data/hydrology/minidot_sc2.txt", sitename = "Summer Camp Creek Site 2: Dissolved Oxygen (mg/L)",
             start_date="2022-07-05", end_date="2022-11-15")#7/5/2022-11/16/2022
