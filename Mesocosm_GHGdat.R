## Mesocosm ##

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

# Load LGR data

LGRdat<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/LGR_dat/Archive", pattern='txt', full.names=T, recursive=TRUE), fread ,header=T))
str(LGRdat) #will get some errors due to the file formatting but it's ok

#Subset just the useful columns (Time, [N2O]d_ppm, AmbT_C)
LGRdat<- subset(LGRdat, select = c( "Time", "[CH4]d_ppm", "[CO2]d_ppm"))
str(LGRdat) 

#Rename the CH4 and CO2 (dry) column to get rid of the square brackets because they can cause some issues later
names(LGRdat)[names(LGRdat) == "[CH4]d_ppm"] <- "CH4_ppm"
names(LGRdat)[names(LGRdat) == "[CO2]d_ppm"] <- "CO2_ppm"

# change the time format to "as.POSIXct" time - R's time format
options(digits.secs=3) # this keeps the 3 decimal second fractions
LGRdat$Time <- as.POSIXct(LGRdat$Time, format = "%m/%d/%Y %H:%M:%OS", tz="Europe/Paris")

#Save

write.csv(LGRdat, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/LGR_dat/LGR_dat_mesocosm_2022.csv")


#Plot to check

tdry <- ggplot(data=LGRdat[which(LGRdat$Time<"2022-07-26 14:00" & LGRdat$Time>"2022-07-26 7:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") + ylim(450, 600)
tdry 

t0 <- ggplot(data=LGRdat[which(LGRdat$Time<"2022-07-26 19:00" & LGRdat$Time>"2022-07-26 14:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") + ylim(400, 800)
t0 

t24 <- ggplot(data=LGRdat[which(LGRdat$Time<"2022-07-27 19:00" & LGRdat$Time>"2022-07-27 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") 
t24 

t48 <- ggplot(data=LGRdat[which(LGRdat$Time<"2022-07-28 19:00" & LGRdat$Time>"2022-07-28 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") 
t48

t4 <- ggplot(data=LGRdat[which(LGRdat$Time<"2022-07-30 19:00" & LGRdat$Time>"2022-07-30 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") 
t4
