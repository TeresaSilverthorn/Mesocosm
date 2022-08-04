# Mesocosm GHG Trial @@

# Effects of temperature and organic matter quantity on decomposition and C gas fluxes #


# Load necessary packages 
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(plyr)
library(gasfluxes)
library(tidyverse)
library(data.table)
library(stringr)
library(car)
library(dpseg)
library(scales)

#Load Picarro data

#########################################
### load raw data and metadata
### put the date-time in the same format for all files
##########################################

# In terms of converting from GMT time (Picarro) to France time. GMT +1 is from January to March 28, 2021 and GMT +2 from March 28 to October 31, then again GMT +1 from October 31 to December 31, 2021

#load raw data of each campaign
# Now with the 2nd campaign, ID isn't unique, thus we need to add date to ID

## CAMPAIGN 1 - May 30 to June 3 2022 ##

Picarro<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Data/Picarro_dat", pattern='dat', full.names=T, recursive=TRUE), fread ,header=T))

str(Picarro) # 24863 obs. of  22 variables

# create a column merging date and time
time<-as.POSIXct(paste(Picarro$DATE, Picarro$TIME), format="%Y-%m-%d %H:%M:%S")
Picarro<-cbind(Picarro,time)
str(Picarro) #24863 obs. of  23 variables

#Since the Picarro data is in GMT time, we need to add an offset of +2hrs 
Picarro$time<- as.POSIXlt(Picarro$time) +7200

#scatter plot
June10 <- ggplot(data=Picarro[which(Picarro$time<"2022-06-10 23:00" & Picarro$time>"2022-06-10 12:00"),],aes(time, CO2_dry))+ geom_line() + scale_x_datetime(breaks=date_breaks("1 hour"))
June10 





dryCO2 <- ggplot(data=Picarro[which(Picarro$time<"2022-06-10 17:30" & Picarro$time>"2022-06-10 14:00"),],aes(time, CO2_dry))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min")) + ylim(600, 1100) +ggtitle("dry") 
dryCO2
ggsave("mesocosm_dry_CO2_june10.pdf", units="in", width=12, height=3)


dryCH4 <- ggplot(data=Picarro[which(Picarro$time<"2022-06-10 17:30" & Picarro$time>"2022-06-10 14:00"),],aes(time, CH4_dry))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min")) + ylim(1.93, 2.025) +ggtitle("dry") 
dryCH4
ggsave("mesocosm_dry_CH4_june10.pdf", units="in", width=12, height=3)


CO2_t0 <- ggplot(data=Picarro[which(Picarro$time<"2022-06-10 21:30" & Picarro$time>"2022-06-10 17:40"),],aes(time, CO2_dry))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"))  + ylim(400, 2000) +ggtitle("t0")
CO2_t0
ggsave("mesocosm_t0_CO2__june10.pdf", units="in", width=12, height=2.5)

CH4_t0 <- ggplot(data=Picarro[which(Picarro$time<"2022-06-10 21:30" & Picarro$time>"2022-06-10 17:40"),],aes(time, CH4_dry))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min")) + ylim(1.8, 3.5) + ggtitle("t0")
CH4_t0
ggsave("mesocosm_t0_CH4_june10.pdf", units="in", width=12, height=2.5)


CO2_t24 <- ggplot(data=Picarro[which(Picarro$time<"2022-06-11 18:00" & Picarro$time>"2022-06-11 13:00"),],aes(time, CO2_dry))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"))  + ylim(400, 3500) +ggtitle("t24")
CO2_t24
ggsave("mesocosm_t24_CO2_june11.pdf", units="in", width=12, height=3)

CH4_t24 <- ggplot(data=Picarro[which(Picarro$time<"2022-06-11 18:00" & Picarro$time>"2022-06-11 13:00"),],aes(time, CH4_dry))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min")) + ylim(1.8, 3)  +ggtitle("t24")
CH4_t24
ggsave("mesocosm_t24_CH4_june11.pdf", units="in", width=12, height=3)



####################################



#load in necessary packages
library(ggplot2)
library(data.table)
library(lubridate)
library(scales)


#Note #Brevon water temperature is stable 

#read ibutton data from the trial 
ibutton72<- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/25C x3 column trial/72_B200000031DB6D21_042522.csv")
#rename columns
colnames(ibutton72) <- c("date_time", "unit", "value")
#change time format
ibutton72$date_time <- as.POSIXct(ibutton72$date_time, format = "%d/%m/%y %I:%M:%S %p")


ibutton71<- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/25C x3 column trial/71_0B00000031CB3821_042522.csv")
#rename columns
colnames(ibutton71) <- c("date_time", "unit", "value")
#change time format
ibutton71$date_time <- as.POSIXct(ibutton71$date_time, format = "%d/%m/%y %I:%M:%S %p")


ibutton99<- fread("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/25C x3 column trial/99_FF00000031E5CC21_042522.csv")
#rename columns
colnames(ibutton99) <- c("date_time", "unit", "value")
#change time format
ibutton99$date_time <- as.POSIXct(ibutton99$date_time, format = "%d/%m/%y %I:%M:%S %p")



#plot


ib71 <- ggplot(data=ibutton71_clip,aes(date_time, value))+ geom_line() +  scale_x_datetime(breaks=date_breaks("1 day"), date_labels = "%d")
ib71 #pretty solidly at 25 degrees, with some very small jumps (when the anode heated up)


ib72 <- ggplot(data=ibutton72_clip,aes(date_time, value))+ geom_line() +  scale_x_datetime(breaks=date_breaks("1 day"), date_labels = "%d") + scale_y_continuous(breaks = seq(17, 26, by = 1))
ib72 #a lot more jumpy than 71, but still around 25C


ib99 <- ggplot(data=ibutton99,aes(date_time, value))+ geom_line() +  scale_x_datetime(breaks=date_breaks("1 day"), date_labels = "%d") + scale_y_continuous(breaks = seq(19, 27.5, by = 1))
ib99  #average more to 24.5 than 25....


#calculate the average from April 21-25

ibutton71_clip <- ibutton71[which(ibutton71$date_time<"2022-04-25 12:00" & ibutton71$date_time>"2022-04-21 23:00"),]
mean(ibutton71_clip$value) #25.0098

ibutton72_clip <- ibutton72[which(ibutton72$date_time<"2022-04-25 12:00" & ibutton72$date_time>"2022-04-21 23:00"),]
mean(ibutton72_clip$value) #25.11275

ibutton99_clip <- ibutton99[which(ibutton99$date_time<"2022-04-25 12:00" & ibutton99$date_time>"2022-04-21 23:00"),]
mean(ibutton99_clip$value) #24.61471



