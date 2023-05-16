## Script for calculating Co2 and CH4 fluxes from LGR for mesocosm experiment ##

# Effects of temperature and organic matter quantity on decomposition and C gas fluxes #

# Load necessary packages 
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(plyr)
library(dplyr)
library(gasfluxes)
library(tidyverse)
library(data.table)
library(stringr)
library(car)
library(dpseg)
library(scales)

# Load LGR data

LGRdat_raw<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/LGR_dat/Archive", pattern='txt', full.names=T, recursive=TRUE), fread ,header=T))
str(LGRdat_raw) #will get some errors due to the file formatting but it's ok
#73632 obs of 28 vars

#Subset just the useful columns (Time, [CO2]d_ppm, [CH4]d_ppm, AmbT_C), used H2O corrected gas values denoted by "d"
LGRdat_raw<- subset(LGRdat_raw, select = c( "Time", "[CO2]d_ppm", "[CH4]d_ppm"))
str(LGRdat_raw) 

#Rename the CH4 and CO2 (dry) column to get rid of the square brackets because they can cause some issues later
names(LGRdat_raw)[names(LGRdat_raw) == "[CH4]d_ppm"] <- "CH4_ppm"
names(LGRdat_raw)[names(LGRdat_raw) == "[CO2]d_ppm"] <- "CO2_ppm"

# change the time format to "as.POSIXct" time 
options(digits.secs=3) # this keeps the 3 decimal second fractions
LGRdat_raw$Time <- as.POSIXct(LGRdat_raw$Time, format = "%m/%d/%Y %H:%M:%OS", tz="Europe/Paris")

#Save
write.csv(LGRdat_raw, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/LGR_dat/LGR_dat_mesocosm_2022.csv")


############################################################
#Plot to check

tdryCO2 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-26 14:00" & LGRdat_raw$Time>"2022-07-26 7:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") + ylim(450, 600)
tdryCO2 

tdryCH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-26 14:00" & LGRdat_raw$Time>"2022-07-26 7:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M")  + ylim(1.975, 2.05)
tdryCH4

t0CO2 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-26 19:00" & LGRdat_raw$Time>"2022-07-26 14:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") + ylim(400, 800)
t0CO2 

t0CH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-26 19:00" & LGRdat_raw$Time>"2022-07-26 14:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") 
t0CH4


t24CO2 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-27 19:00" & LGRdat_raw$Time>"2022-07-27 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") 
t24CO2 

t24CH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-27 19:00" & LGRdat_raw$Time>"2022-07-27 9:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M")   + ylim(1.98, 2.2)
t24CH4 

t48 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-28 19:00" & LGRdat_raw$Time>"2022-07-28 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") 
t48

t4CO2 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-30 19:00" & LGRdat_raw$Time>"2022-07-30 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") + ylim(400, 950)
t4CO2

t4CH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-30 19:00" & LGRdat_raw$Time>"2022-07-30 9:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M")  + ylim(2.05, 2.27)
t4CH4

t8CO2 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-08-03 19:00" & LGRdat_raw$Time>"2022-08-03 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") 
t8CO2

t8CH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-08-03 19:00" & LGRdat_raw$Time>"2022-08-03 9:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M")  #+ ylim(2.05, 2.27)
t8CH4

t8CH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-08-03 14:00" & LGRdat_raw$Time>"2022-08-03 13:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("1 min"), date_labels = "%I:%M")  #+ ylim(2.05, 2.27)
t8CH4

t16CO2 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-08-11 19:00" & LGRdat_raw$Time>"2022-08-11 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") + ylim(450, 1050)
t16CO2

t16CH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-08-11 19:00" & LGRdat_raw$Time>"2022-08-11 9:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M")  #+ ylim(2.05, 2.27)
t16CH4

####################################################################
#Load times

ancil_dat <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/Mesocosm_GHG_data_sheet_2022.csv")
str(ancil_dat) #288 obs of 11 vars

#re-format the time

ancil_dat$start_time<- as.POSIXct(paste(ancil_dat$Date, ancil_dat$start_time), format="%Y-%m-%d %H:%M")
ancil_dat$end_time<- as.POSIXct(paste(ancil_dat$Date, ancil_dat$end_time), format="%Y-%m-%d %H:%M")

#calculate volume of cylinder

ancil_dat$volume_L <- ( (pi) * (4.75^2) * ancil_dat$Headspace_height_cm )* 0.001
#need to check and adjust this equation. Need the correct radius of the cylinder and heights for the columns without water. Mesocosm diameter is 9.5 cm (r= 4.75). And the height of the empty mesocosm is 30cm #also need to account for the volume of the tubes (is in the google Sheet2) and sometimes we used short tubes, and sometimes long tubes (2022-07-26_dry_12 switched to long tube). 
# Add 0.0005701990666 to volume for days before 2022-07-26
ancil_dat <- ancil_dat %>% 
  mutate(volume_L = if_else(Date < as.Date("2022-07-26"), volume_L + 0.0002850995333 , volume_L))

# Add 0.0002850995333  to volume for days after 2022-07-26
ancil_dat <- ancil_dat %>% 
  mutate(volume_L = if_else(Date > as.Date("2022-07-26"), volume_L +0.0005701990666, volume_L))


ancil_dat$area_m2 <- 0.007088218 #R of mescosm is 4.75cm. Area is 70.88 cm2  

#Munoz et al 2018measured CO2 flux from sediment mesocosm on a per area basis, could also consider a per volume of sediment basis? 

########################################################################

########## Adjust start and end times based on LGR time drift #########

#On average LGR is 8 minutes ahead, but will need to double check this manually by looking at some flux plots. Checked 2022-08-03 +8 mins

ancil_dat$start_time <- as.POSIXct(ancil_dat$start_time) + 510 #add 30s to start

ancil_dat$end_time <-as.POSIXct(ancil_dat$end_time) + 480


#Based on visual inspection the gasflux figures, clip some start and end times

Newtimes <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/New_flux_times.csv")

#select relevant columns
CO2_newtimes<- subset(Newtimes, select = c( "ID_unique",  "CO2_s_add_to_start",  "CO2_s_sub_from_end"))

CH4_newtimes<- subset(Newtimes, select = c( "ID_unique",  "CH4_s_add_to_start",  "CH4_s_sub_from_end"))

#Make NAs 0
#CO2_newtimes$CO2_s_add_to_start[is.na(CO2_newtimes$CO2_s_add_to_start)] <- 0
#CO2_newtimes$CO2_s_sub_from_end[is.na(CO2_newtimes$CO2_s_sub_from_end)] <- 0

#merge data frames
ancil_dat_CO2 <- merge(ancil_dat, CO2_newtimes, by = "ID_unique")
ancil_dat_CH4 <- merge(ancil_dat, CH4_newtimes, by = "ID_unique")

# Add seconds to start_time column in ancil_dat based on CO2_s_add_to_start column
ancil_dat_CO2 <- ancil_dat_CO2 %>% 
  mutate(start_time = if_else(!is.na(CO2_s_add_to_start), 
                              start_time + seconds(CO2_s_add_to_start), 
                              start_time))

ancil_dat_CH4 <- ancil_dat_CH4 %>% 
  mutate(start_time = if_else(!is.na(CH4_s_add_to_start), 
                              start_time + seconds(CH4_s_add_to_start), 
                              start_time))

# Subtract seconds from end_time column in ancil_dat based on CO2_s_sub_from_end column
ancil_dat_CO2 <- ancil_dat_CO2 %>% 
  mutate(end_time = if_else(!is.na(CO2_s_sub_from_end), 
                            end_time - seconds(CO2_s_sub_from_end), 
                            end_time))

ancil_dat_CH4 <- ancil_dat_CH4 %>% 
  mutate(end_time = if_else(!is.na(CH4_s_sub_from_end), 
                            end_time - seconds(CH4_s_sub_from_end), 
                            end_time))


#######################################################

#Clip the LGR data to the start and end times for CO2

ID <- ancil_dat_CO2$ID_unique
startT<-ancil_dat_CO2$start_time #start times
endT<-ancil_dat_CO2$end_time  # end times 

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID[i]
  data<-LGRdat_raw[LGRdat_raw$Time >= st & LGRdat_raw$Time <= se,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

LGR_dat_CO2<-get(paste("data",length(startT),sep="_"))


str(LGR_dat_CO2) #52532 obs. of  4 variables

rm(list = ls()[grep("^data_", ls())]) #clear all of the clipped datasets

#######################################################

#Clip the LGR data to the start and end times for CH4

ID <- ancil_dat_CH4$ID_unique
startT<-ancil_dat_CH4$start_time #start times
endT<-ancil_dat_CH4$end_time  # end times 

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID[i]
  data<-LGRdat_raw[LGRdat_raw$Time >= st & LGRdat_raw$Time <= se,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

LGR_dat_CH4<-get(paste("data",length(startT),sep="_"))


str(LGR_dat_CH4) 

rm(list = ls()[grep("^data_", ls())]) #clear all of the clipped datasets


###############################################################################

#In order to set the initial time of each measurement to 0: 
#start by adding a column for epoch time (expressed as seconds since Jan 1, 1970)
LGR_dat_CO2$epoch_time <- as.integer(as.POSIXct(LGR_dat_CO2$Time), tz="Europe/Paris")
LGR_dat_CH4$epoch_time <- as.integer(as.POSIXct(LGR_dat_CH4$Time), tz="Europe/Paris")

str(LGR_dat_CO2) 
str(LGR_dat_CH4)

#there are duplicated time rows in the data, delete them (can cause problems later)
dupsCO2 <- LGR_dat_CO2[duplicated(epoch_time)]
dupsCH4 <- LGR_dat_CH4[duplicated(epoch_time)]

str(dupsCO2)  # 206 obs. of  5 variables
str(dupsCH4)  #220

LGR_dat_CO2 <- LGR_dat_CO2 %>% 
  # Base the removal on the "epoch_time" column
  distinct(epoch_time, .keep_all = TRUE)

LGR_dat_CH4 <- LGR_dat_CH4 %>% 
  # Base the removal on the "epoch_time" column
  distinct(epoch_time, .keep_all = TRUE)


str(LGR_dat_CO2)  #38533 obs. of  5 variables
str(LGR_dat_CH4)  #40011 obs. of  5 variables

#then set  the initial time of each measure to 0h  (use Naiara's function to rest the min time to each time)

rescale <- function(x) (x-min(x))

#apply this function to all epoch_time (seconds) of each measure, 
#and divide by 36000 (hours)
LGR_dat_CO2 <- setDT(LGR_dat_CO2)[,c("flux_time"):=.(rescale(epoch_time/3600)),by=.(ID)]
LGR_dat_CH4 <- setDT(LGR_dat_CH4)[,c("flux_time"):=.(rescale(epoch_time/3600)),by=.(ID)]

#######################

#Merge times with LGR dat

#Merge ancil_dat with N2O #Need to use a unique ID e.g. "2021-12-03_BU02_R4_LGR"
#Need to rename tthe ID column in LGR_dat_CO2 to "ID_unique"
names(LGR_dat_CO2)[names(LGR_dat_CO2) == "ID"] <- "ID_unique"
names(LGR_dat_CH4)[names(LGR_dat_CH4) == "ID"] <- "ID_unique"

str(LGR_dat_CO2) #38533  obs. of  6 variables
str(LGR_dat_CH4) #40011 obs. of  6 variables

CO2_dat <- merge(ancil_dat, LGR_dat_CO2, by="ID_unique")
CH4_dat <- merge(ancil_dat, LGR_dat_CH4, by="ID_unique")

str(CO2_dat) #38533  obs. of  18 variables
str(CH4_dat) #40011 obs. of  18 variables

######  Convert LGR_dat concentration from ppm to mg-CO2-C/L and ug CH4-C/L using the ideal gas law (PV=nRT) for input to gasfluxes package.  #############  Note that ppm = uL/L

#Check if we have some accurate room temp data (e.g. from iButton on Picarro?)


#put CO2 and CH4 concentration (now in ppm) in mg/L
# mg/L = ((ppm  * molecular mass *1 atm )/1000) / (0.082 * 293K )
# ug/L = (ppm  * molecular mass *1 atm ) / (0.082 * 293K )

#Note here that we assume 1 atm for the pressure and 20C for the temperature

CO2_dat$CO2_mg_L <- ((CO2_dat$CO2_ppm  * 12.011 * 1 )/1000) / (0.08206 *(20 + 273.15))

CH4_dat$CH4_ug_L <- (CH4_dat$CH4_ppm  * 12.011 *1 ) / (0.082*(20 + 273.15))



#Check the units
#V = L
#A = m2
# flux time = h
# concentration of CO2 / CH4 = mg/L

#[f0] = mg/m^2/h

mean(CO2_dat$volume_L) # 0.8352545... L
mean(CO2_dat$area_m2) # 0.007854... m2
mean(CH4_dat$volume_L) # 0.8400581... L
mean(CH4_dat$area_m2) # 0.007854... m2
median(CO2_dat$CO2_mg_L) # ~0.27 mg/L          
median(CH4_dat$CH4_ug_L) # ~1.0 ug/L      
mean(CO2_dat$flux_time) # 0.076 h = 4.56 minutes
mean(CH4_dat$flux_time) # 0.076 h = 4.56 minutes



#If gasflux() is getting an error "Error: flux_time not sorted in flux ID 2021-03-24_AL01_A1." flux time needs to be ordered within each ID (use ID unique for all of the data)
CO2_dat <- data.table(CO2_dat, key = c("ID_unique", "flux_time")) 
CH4_dat <- data.table(CH4_dat, key = c("ID_unique", "flux_time")) 

#### Run gasfluxes to calculate CO2 flux rate based on volume and area  ####
CO2.results<- gasfluxes(CO2_dat, .id = "ID_unique", 
                        .V = "volume_L", .A = "area_m2", 
                        .times = "flux_time",.C = "CO2_mg_L", 
                        methods = c("linear"), plot=F)

#turn plot to F if you don't want plots 

str(CO2.results) #252 obs, all obs present

missing_values <- setdiff( ancil_dat$ID_unique, CO2.results$ID_unique) #Normal to be missing the 2022-07-29 because I didn't measure GHGs then, but the others--the times are input correctly, so it must be a LGR data issue?


CH4.results<- gasfluxes(CH4_dat, .id = "ID_unique", 
                        .V = "volume_L", .A = "area_m2", 
                        .times = "flux_time",.C = "CH4_ug_L", 
                        methods = c("linear"), plot=F)

str(CH4.results) #252 obs. of  10 variables

#Subset the useful columns 
CO2.results<- subset(CO2.results, select = c( "ID_unique", "linear.f0"))
names(CO2.results)[names(CO2.results) == "linear.f0"] <- "CO2_C_mg_m2_h" #rename

write.csv (CO2.results, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/mesocosm_CO2_flux.results.csv")

CH4.results<- subset(CH4.results, select = c( "ID_unique", "linear.f0"))
names(CH4.results)[names(CH4.results) == "linear.f0"] <- "CH4_C_ug_m2_h" #rename

write.csv (CH4.results, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/mesocosm_CH4_flux.results.csv")

#Merge the CO2 and CH4 results by ID
CO2.CH4.results <- merge (CO2.results, CH4.results, by= "ID_unique")
str(CO2.CH4.results) #252 obs. of  3 variables

#Load the treatment codes
treatment_codes <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/Treatment_codes.csv")

#Merge with the ancillary data
CO2.CH4.fluxes <- merge (CO2.CH4.results, ancil_dat, by= "ID_unique")
str(CO2.CH4.fluxes) #252 obs. of  15 variables

CO2.CH4.fluxes <- merge (CO2.CH4.fluxes, treatment_codes, by= "Column_ID")
str(CO2_fluxes) #252 obs. of  25 variables

write.csv (CO2.CH4.fluxes, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/CO2.CH4.fluxes.csv")

