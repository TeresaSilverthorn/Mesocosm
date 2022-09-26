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

LGRdat_raw<-do.call(rbind, lapply(list.files("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/LGR_dat/Archive", pattern='txt', full.names=T, recursive=TRUE), fread ,header=T))
str(LGRdat_raw) #will get some errors due to the file formatting but it's ok

#Subset just the useful columns (Time, [N2O]d_ppm, AmbT_C)
LGRdat_raw<- subset(LGRdat_raw, select = c( "Time", "[CH4]d_ppm", "[CO2]d_ppm"))
str(LGRdat_raw) 

#Rename the CH4 and CO2 (dry) column to get rid of the square brackets because they can cause some issues later
names(LGRdat_raw)[names(LGRdat_raw) == "[CH4]d_ppm"] <- "CH4_ppm"
names(LGRdat_raw)[names(LGRdat_raw) == "[CO2]d_ppm"] <- "CO2_ppm"

# change the time format to "as.POSIXct" time - R's time format
options(digits.secs=3) # this keeps the 3 decimal second fractions
LGRdat_raw$Time <- as.POSIXct(LGRdat_raw$Time, format = "%m/%d/%Y %H:%M:%OS", tz="Europe/Paris")

#Save

write.csv(LGRdat_raw, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/LGR_dat/LGR_dat_mesocosm_2022.csv")


############################################################
#Plot to check

tdry <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-26 14:00" & LGRdat_raw$Time>"2022-07-26 7:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") + ylim(450, 600)
tdry 

tdryCH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-26 14:00" & LGRdat_raw$Time>"2022-07-26 7:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M")  + ylim(1.975, 2.05)
tdryCH4

t0 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-26 19:00" & LGRdat_raw$Time>"2022-07-26 14:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") + ylim(400, 800)
t0 

t0CH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-26 19:00" & LGRdat_raw$Time>"2022-07-26 14:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") 
t0CH4


t24 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-27 19:00" & LGRdat_raw$Time>"2022-07-27 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") 
t24 

t24CH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-27 19:00" & LGRdat_raw$Time>"2022-07-27 9:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M")   + ylim(1.98, 2.2)
t24CH4 

t48 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-28 19:00" & LGRdat_raw$Time>"2022-07-28 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") 
t48

t4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-30 19:00" & LGRdat_raw$Time>"2022-07-30 9:05"),],aes(Time, CO2_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M") + ylim(400, 950)
t4

t4CH4 <- ggplot(data=LGRdat_raw[which(LGRdat_raw$Time<"2022-07-30 19:00" & LGRdat_raw$Time>"2022-07-30 9:05"),],aes(Time, CH4_ppm))+ geom_line() + scale_x_datetime(breaks=date_breaks("30 min"), date_labels = "%I:%M")  + ylim(2.05, 2.27)
t4CH4

####################################################################
#Load times

times <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Mesocosm_GHG_data_sheet_2022.csv")
str(times)

#re format the time

times$start_time<- as.POSIXct(paste(times$Date, times$start_time), format="%Y-%m-%d %H:%M")
times$end_time<- as.POSIXct(paste(times$Date, times$end_time), format="%Y-%m-%d %H:%M")

#calculate volume of cylinder

times$volume_L <- ( (pi) * (4.75^2) * times$Headspace_height_cm )* 0.001
#need to check and adjust this equation. Need the correct radius of the cylinder and heights for the columns without water. Mesocosm diameter is 9.5 cm (r= 4.75). And the height of the empty mesocosm is 30cm
#also need to account for the volume of the tubes (is in the google Sheet2)



times$area_m2 <- 0.007088218 #R of mescosm is 4.75cm 70.88 cm2 or 


########################################################

########## Adjust start and end times based on LGR time drift #########

#On average LGR is 8 minutes ahead, but will need to double check this manually by looking at some flux plots

times$start_time <- as.POSIXct(times$start_time) + 500

times$end_time <-as.POSIXct(times$end_time) + 480



#######################################################

#Clip the LGR data to the start and end times
#May need to acocunt for the fact that the LGR is 8 minutes ahead (check)


ID <- times$ID_unique
startT<-times$start_time #start times
endT<-times$end_time  # end times 

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

LGR_dat<-get(paste("data",length(startT),sep="_"))


str(LGR_dat) #20995 obs. of  4 variables

rm(list = ls()[grep("^data_", ls())]) #clear all of the clipped datasets


###############################################################################

#In order to set the initial time of each measurement to 0: 
#start by adding a column for epoch time (expressed as seconds since Jan 1, 1970)
LGR_dat$epoch_time <- as.integer(as.POSIXct(LGR_dat$Time), tz="Europe/Paris")

str(LGR_dat) 

#there are duplicated time rows in the data, delete them
dups <- LGR_dat[duplicated(epoch_time)]
str(dups)  

LGR_dat <- LGR_dat %>% 
  # Base the removal on the "epoch_time" column
  distinct(epoch_time, .keep_all = TRUE)

str(LGR_dat) 

#then set  the initial time of each measure to 0h 
#use Naiara's function to rest the min time to each time

rescale <- function(x) (x-min(x))

#apply this function to all epoch_time (seconds) of each measure, 
#and divide by 36000 (hours)
LGR_dat <- setDT(LGR_dat)[,c("flux_time"):=.(rescale(epoch_time/3600)),by=.(ID)]

#######################

#Merge times with LGR dat


#Merge ancil_dat with N2O #Need to use a unique ID e.g. "2021-12-03_BU02_R4_LGR"
#Need to rename tthe ID column in LGR_dat to "ID_unique"
names(LGR_dat)[names(LGR_dat) == "ID"] <- "ID_unique"

str(LGR_dat)

CO2_CH4_dat <- merge(times, LGR_dat, by="ID_unique")

str(CO2_CH4_dat) 

CO2_CH4_dat <- na.omit(CO2_CH4_dat)  #delete incomplete cases


######  Convert LGR_dat concentration from ppm to mg-CO2-C/L using the ideal gas law (PV=nRT) for input to gasfluxes package.  #############  Note that ppm = uL/L


#put CO2 and CH4 concentration (now in ppm) in mg/L
# mg/L = ((ppm  * molecular mass *1 atm )/1000) / (0.082 * 293K )
# ug/L = (ppm  * molecular mass *1 atm ) / (0.082 * 293K )


CO2_CH4_dat$CO2_mg_L <- ((CO2_CH4_dat$CO2_ppm  * 12.011 * 1 )/1000) / (0.08206 *(20 + 273.15))
#Note here that we need to use the specific pressures and temperatures

CO2_CH4_dat$CH4_ug_L <- (CO2_CH4_dat$CH4_ppm  * 12.011 *1 ) / (0.082*(20 + 273.15))

str(CO2_CH4_dat) # 291629 obs. of  44 variables


#Check the units
#V = L
#A = m2
# flux time = h
# concentration of CO2 / CH4 = mg/L

#[f0] = mg/m^2/h

mean(CO2_CH4_dat$volume_L) # 4.5... L
mean(CO2_CH4_dat$area_m2) # 0.007854... m2
median(CO2_CH4_dat$CO2_mg_L) # ~0.27 mg/L          
median(CO2_CH4_dat$CH4_ug_L) # ~1.0 ug/L      
mean(CO2_CH4_dat$flux_time) # 0.03h = 1.8 minutes


#If getting an error "Error: flux_time not sorted in flux ID 2021-03-24_AL01_A1." need to investigate
#flux time needs to be ordered within each ID (use ID unique for all of the data)
CO2_CH4_dat <- data.table(CO2_CH4_dat, key = c("ID_unique", "flux_time")) 


CO2.results<- gasfluxes(CO2_CH4_dat, .id = "ID_unique", 
                        .V = "volume_L", .A = "area_m2", 
                        .times = "flux_time",.C = "CO2_mg_L", 
                        methods = c("linear"), plot=T)

#turn plot to F if you don't want plots 

str(CO2.results)

CH4.results<- gasfluxes(CO2_CH4_dat, .id = "ID_unique", 
                        .V = "volume_L", .A = "area_m2", 
                        .times = "flux_time",.C = "CH4_ug_L", 
                        methods = c("linear"), plot=T)

str(CH4.results)

#Subset the useful columns 
CO2.results<- subset(CO2.results, select = c( "ID_unique", "linear.f0"))
names(CO2.results)[names(CO2.results) == "linear.f0"] <- "CO2_Cmg_m2_h" #rename

CH4.results<- subset(CH4.results, select = c( "ID_unique", "linear.f0"))
names(CH4.results)[names(CH4.results) == "linear.f0"] <- "CH4_Cug_m2_h" #rename


#Load the treatment codes
treatment_codes <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Treatment_codes.csv")

#Merge with the ancillary data
CO2_fluxes <- merge (CO2.results, times, by= "ID_unique")
str(CO2_fluxes) #106 obs. of  19 variables

CO2_fluxes <- merge (CO2_fluxes, treatment_codes, by= "Column_ID")
str(CO2_fluxes)

write.csv (CO2_fluxes, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/mesocosm_CO2_flux.results.csv")



#Merge with the ancillary data
CH4_fluxes <- merge (CH4.results, times, by= "ID_unique")
str(CH4_fluxes) #106 obs. of  19 variables

CH4_fluxes <- merge (CH4_fluxes, treatment_codes, by= "Column_ID")
str(CH4_fluxes)

write.csv (CH4_fluxes, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/mesocosm_CH4_flux.results.csv")


#plot

boxplot(CO2_Cmg_m2_h ~ start_time, data= (CO2_fluxes))


boxplot(linear.f0 ~ start_time, data= (CH4_fluxes))


#Show interactive effect of temperature and OM content with grouped boxplot per gas per time
#For CO2

#Make temp_C and leaf_treatment a factor with 3 levels
CO2_fluxes$temp_C <- factor(CO2_fluxes$temp_C)
CO2_fluxes$leaf_treatment <- factor(CO2_fluxes$leaf_treatment)
#reorder factor levels
CO2_fluxes$leaf_treatment <- factor(CO2_fluxes$leaf_treatment, levels=c("control", "low", "medium", "high"))

#Make temp_C and leaf_treatment a factor with 3 levels for CH4 as well
CH4_fluxes$temp_C <- factor(CH4_fluxes$temp_C)
CH4_fluxes$leaf_treatment <- factor(CH4_fluxes$leaf_treatment)
#reorder factor levels
CH4_fluxes$leaf_treatment <- factor(CH4_fluxes$leaf_treatment, levels=c("control", "low", "medium", "high"))



####

CO2_fluxes_dry <- CO2_fluxes[which(CO2_fluxes$t=='dry'),]

CO2_dry <- ggplot(CO2_fluxes_dry, aes(x=temp_C, y=CO2_Cmg_m2_h, fill=leaf_treatment)) + 
  geom_boxplot() + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) + xlab(expression("Temperature " ( degree*C))) +  theme_pubr()  + scale_fill_manual(values = c("#F9E4C4", "#EE8A81", "#C8586C", "#6F2849")) + labs(fill = "OM quantity") 
CO2_dry

####

CH4_fluxes_dry <- CH4_fluxes[which(CH4_fluxes$t=='dry'),]

CH4_dry <- ggplot(CH4_fluxes_dry, aes(x=temp_C, y=CH4_Cug_m2_h, fill=leaf_treatment)) + 
  geom_boxplot() +ylab(expression(mu*g~CH[4]*`-C`~m^-2~h^-1)) + xlab(expression("Temperature " ( degree*C))) +  theme_pubr()  + scale_fill_manual(values = c("#F9E4C4", "#EE8A81", "#C8586C", "#6F2849")) + labs(fill = "OM quantity")
CH4_dry


#####

CO2_fluxes_t0 <- CO2_fluxes[which(CO2_fluxes$t=='t0'),]

CO2_t0 <- ggplot(CO2_fluxes_t0, aes(x=temp_C, y=CO2_Cmg_m2_h, fill=leaf_treatment)) + 
  geom_boxplot() + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) + xlab(expression("Temperature " ( degree*C))) +  theme_pubr()  + scale_fill_manual(values = c("#F9E4C4", "#EE8A81", "#C8586C", "#6F2849")) + labs(fill = "OM quantity")
CO2_t0


####

CH4_fluxes_t0 <- CH4_fluxes[which(CH4_fluxes$t=='t0'),]

CH4_t0 <- ggplot(CH4_fluxes_t0, aes(x=temp_C, y=CH4_Cug_m2_h, fill=leaf_treatment)) + 
  geom_boxplot() +ylab(expression(mu*g~CH[4]*`-C`~m^-2~h^-1)) + xlab(expression("Temperature " ( degree*C))) +  theme_pubr()  + scale_fill_manual(values = c("#F9E4C4", "#EE8A81", "#C8586C", "#6F2849")) + labs(fill = "OM quantity")
CH4_t0


####

CO2_fluxes_t24 <- CO2_fluxes[which(CO2_fluxes$t=='t24'),]

CO2_t24 <- ggplot(CO2_fluxes_t24, aes(x=temp_C, y=CO2_Cmg_m2_h, fill=leaf_treatment)) + 
  geom_boxplot() + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) + xlab(expression("Temperature " ( degree*C))) +  theme_pubr()  + scale_fill_manual(values = c("#F9E4C4", "#EE8A81", "#C8586C", "#6F2849")) + labs(fill = "OM quantity")
CO2_t24

####

CH4_fluxes_t24 <- CH4_fluxes[which(CH4_fluxes$t=='t24'),]

CH4_t24 <- ggplot(CH4_fluxes_t24, aes(x=temp_C, y=CH4_Cug_m2_h, fill=leaf_treatment)) + 
  geom_boxplot() +ylab(expression(mu*g~CH[4]*`-C`~m^-2~h^-1)) + xlab(expression("Temperature " ( degree*C))) +  theme_pubr()  + scale_fill_manual(values = c("#F9E4C4", "#EE8A81", "#C8586C", "#6F2849")) + labs(fill = "OM quantity")
CH4_t24


