# MESOCOSM EXPERIMENT 2022 #
# Effects of temperature and organic matter quantity on deocmposition and C gas fluxes #



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


