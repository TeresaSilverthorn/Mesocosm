###########################################
## Script for analyzing CO2 and CH4 flux data from mesocosm experiment examining temperature and ## OM quantity effects on GHG fluxes from simulated isolated pools 
##################################################################
##  Load necessary packages  ##
library(data.table)
library(ggplot2)
library(tidyr)
library(scales)
library(dplyr)
library(ggpmisc)
library(rstatix)
library(ggpubr)
library(emmeans)
library(lme4)
library(nlme)
library(lmerTest)
library(ggallin) #for pseudolog10 transformation dealing with negative numbers

#Set wd for figures
setwd("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Figures")

#Load in the GHG flux results from Mesocosm_GHGR.R, combined with the ancillary data and treatment codes.

dat <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/CO2.CH4.fluxes.csv")

str(dat) #252 obs of 27 vars #since we measured DO on one day that we did not measure GHGs, that measure is missing here

#Make necessary factors
dat$leaf_treatment <- as.factor(dat$leaf_treatment) #make leaf treatment a factor
dat$leaf_treatment <- factor(dat$leaf_treatment, levels = c("control", "low", "medium", "high")) #logically reorder
dat$temp_C <- as.factor(dat$temp_C) #make the temperature treatment a factor
dat$t <- as.factor(dat$t)           #make time a factor

#format Date
dat$Date <-as.POSIXct(dat$Date, format="%Y-%m-%d", tz = "Europe/Berlin")

#rename
names(dat)[names(dat) == "DO_."] <- "DO_percent"

#Missing the 29/07/2023 data, import and add

July29 <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/Mesocosm_GHG_data_sheet_2022.csv")

#format Date
July29$Date <-as.POSIXct(July29$Date, format="%Y-%m-%d", tz = "Europe/Berlin")

July29 <- subset(July29, Date=="2022-07-29")

#rename
names(July29)[names(July29) == "DO_."] <- "DO_percent"

#merge with treatment codes
treatment_codes <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/Treatment_codes.csv")

July29 <- merge (July29, treatment_codes, by= "Column_ID")

#Make temp_C a factor
July29$temp_C <- as.factor(July29$temp_C)
July29$leaf_treatment <- as.factor(July29$leaf_treatment)

#add to dat
dat <- bind_rows(dat, July29)

#Import decomposition data
leaf_mass_loss <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/leaf_mass_loss.csv")
str(leaf_mass_loss) #36 obs. of  3 variables

#merge
dat <- merge(dat, leaf_mass_loss, by="Column_ID")

#plot
palette2 <- c("#FFE085", "#F27E29", "#E53C22")

tiff("AFDM_remain", units="in", width=7, height=5, res=300)

AFDM_remain <-  ggplot(subset(dat, leaf_treatment!= "control" & t=="dry"), aes(x=leaf_treatment, y=percent_AFDM_remaining, fill=temp_C)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) + scale_fill_manual(values = palette2) +
  ylab("AFDM remaining (%)") +
  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line(), axis.title.x = element_blank())
AFDM_remain 

dev.off()

# Summary data
summary <- dat %>%
  dplyr::group_by(leaf_treatment) %>%
  dplyr::summarise(
    #mean_sediment_g = mean(sediment_g),
   # sd_mean_sediment_g = sd(sediment_g), 
    mean_dry_input_leaves_g = mean(dry_input_leaves_g), 
    sd_dry_input_leaves_g = sd(dry_input_leaves_g))



#Import and merge sediment data
OM <- fread ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/Sediment Data/Sediment_OM_mesocosm.csv")
str(OM)

#Subset relevant columns
OM<- subset(OM, select = c("Sample ID", "sed_moisture_percent", "sed_OM_percent"))

#rename
names(OM)[names(OM) == "Sample ID"] <- "Column_ID"

#merge
dat <- merge(dat, OM, by="Column_ID")


###############################################################################
#Plot the DO data

tiff("DO_leaftreatment", units="in", width=7, height=5, res=300)

DOplot <- ggplot(dat, aes(as.factor(Date), DO_mg_L )) + 
  theme_bw() + geom_boxplot() 
DOplot 

dev.off()


#DO means by treatment
DO_means <- dat_means <- dat %>% 
  dplyr::group_by(Date, temp_C, leaf_treatment) %>% # we group by the two values
  dplyr::summarise(
    DO_mg_L = mean(DO_mg_L),
    DO_percent = mean(DO_percent) )

str(DO_means) 
  
#format Date
DO_means$Date <-as.POSIXct(DO_means$Date, format="%Y-%m-%d", tz = "Europe/Berlin")
  

tiff("DO_timeseries", units="in", width=7, height=5, res=300)
#plot change in DO over time
palette1 <- c("#e1e590", "#BCD980", "#5D966D", "#3D6160")

time_series_DO <- ggplot(subset(DO_means, Date>="2022-07-29"), aes(x=Date, y=DO_percent)) +
  geom_line(aes(linetype=temp_C, colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(shape=temp_C,  colour=leaf_treatment), size=2.5, alpha=0.7) +
  scale_fill_manual(values = palette1) + scale_colour_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() )  +  scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%d/%m", tz="Europe/Berlin"))
time_series_DO

dev.off()

#plot OM content
#Note the initial sediment OM content: need to burn the dry sediment sample along with the one missing valueM22 


tiff("OM", units="in", width=7, height=5, res=300)

OMplot <- ggplot(subset(dat, t=="dry"), aes(leaf_treatment, sed_OM_percent)) + ylab("Sediment OM (%)")+
  theme_bw() + geom_boxplot(aes(fill = temp_C), colour="black")   + scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505")) 
OMplot 

dev.off()

levels(dat$t)

# Plot all of the GHG data
boxplot(CO2_C_mg_m2_h ~ start_time, data= (dat))

boxplot(CH4_C_ug_m2_h ~ start_time, data= (dat)) #two very high CH4 fluxes: 2022-08-11_t16_32, and 2022-08-11_t16_28...  

###########  Plot by sampling time  ########################

#dry
CO2_dry <- ggplot(subset(dat, t=="dry"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
   theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
     aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "Dry")
CO2_dry 
#weird that the high temp treatments are so consistently variable... effect of mesocosm location? 

CH4_dry <- ggplot(subset(dat, t=="dry"), aes(x=leaf_treatment, y=CH4_C_ug_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "Dry")
CH4_dry 

#T0
CO2_t0 <- ggplot(subset(dat, t=="t0"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t0")
CO2_t0 

CH4_t0 <- ggplot(subset(dat, t=="t0"), aes(x=leaf_treatment, y=CH4_C_ug_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t0")
CH4_t0 

#T24
CO2_t24 <- ggplot(subset(dat, t=="t24"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t24")
CO2_t24 

CH4_t24 <- ggplot(subset(dat, t=="t24"), aes(x=leaf_treatment, y=CH4_C_ug_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t24")
CH4_t24 

#T48
CO2_t48 <- ggplot(subset(dat, t=="t48"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t48")
CO2_t48 

CH4_t48 <- ggplot(subset(dat, t=="t48"), aes(x=leaf_treatment, y=CH4_C_ug_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t48")
CH4_t48

#T4
CO2_t4 <- ggplot(subset(dat, t=="t4"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t4")
CO2_t4 

CH4_t4 <- ggplot(subset(dat, t=="t4"), aes(x=leaf_treatment, y=CH4_C_ug_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t4")
CH4_t4

#T8
CO2_t8 <- ggplot(subset(dat, t=="t8"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.8, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t8")
CO2_t8 

CH4_t8 <- ggplot(subset(dat, t=="t8"), aes(x=leaf_treatment, y=CH4_C_ug_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t8")
CH4_t8

#T16
CO2_t16 <- ggplot(subset(dat, t=="t16"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.8, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t16")
CO2_t16 

CH4_t16 <- ggplot(subset(dat, t=="t16"), aes(x=leaf_treatment, y=CH4_C_ug_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t16")
CH4_t16


#########################
#plot by treatments

tiff("CO2_full", units="in", width=7, height=5, res=300)

CO2_full <- ggplot(dat, aes(x=leaf_treatment, y=CO2_C_mg_m2_h) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C), outlier.shape = NA ) + geom_point(
    aes(fill = temp_C, color =temp_C),  size=3, position = position_dodge(0.8))   +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  
CO2_full

dev.off()

tiff("CH4_full", units="in", width=7, height=5, res=300)

lab_y <- c("-1", "0", "1", "10", "100", "1000","10000", "100000") 
brk_y <- c(-1, 0, 1, 10, 100, 1000,10000, 100000)

CH4_full <- ggplot(subset(dat, leaf_treatment !="NA"), aes(x=leaf_treatment, y=CH4_C_ug_m2_h)) + 
  theme_bw() + geom_boxplot(aes(color = temp_C), outlier.shape = NA ) + geom_point(
    aes(fill = temp_C, color =temp_C),  size=3, position = position_dodge(0.8))   +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505")) + coord_trans(y=pseudolog10_trans) + scale_y_continuous( breaks = brk_y,labels = lab_y, limits = c(-4, 160000)) 
CH4_full

dev.off()  #log2 transformation of the axis results in negative numbers to be removed. Can use scale_y_continuous(trans=pseudolog10_trans)  or manually set the scale


### Treatment means ####

dat_means <- dat %>% 
  dplyr::group_by(Date, t, temp_C, leaf_treatment) %>% # we group by the two values
  dplyr::summarise(
    mean_CO2_C_mg_m2_h = mean(CO2_C_mg_m2_h),
    sd_CO2_C_mg_m2_h = sd(CO2_C_mg_m2_h),
    mean_CH4_C_ug_m2_h = mean(CH4_C_ug_m2_h),
    sd_CH4_C_ug_m2_h = sd(CH4_C_ug_m2_h),
    DO_mg_L = mean(DO_mg_L),
    DO_percent = mean(DO_percent),
    Water_temp_C = mean(Water_temp_C),
    sediment_g = mean(sediment_g),
    dry_input_leaves_g = mean(dry_input_leaves_g),
    dry_output_leaves_g = mean(dry_output_leaves_g),
    incubation_water_mL = mean(incubation_water_mL),
    groundwater_ml = mean(groundwater_ml),
    sed_moisture_percent = mean(sed_moisture_percent),
    sed_OM_percent = mean(sed_OM_percent) )

str(dat_means) #84 obs of 19 vars

    
#Convert CH4 ug to mg
dat_means$mean_CH4_C_mg_m2_h <- dat_means$mean_CH4_C_ug_m2_h/1000

#Change date format
dat_means$Date <-as.POSIXct(dat_means$Date, format="%Y-%m-%d", tz = "Europe/Berlin")

#Make t into a numerical variable based on the hours since rewetting
dat_means$t <- as.character(dat_means$t)

dat_means <- dat_means %>%
  mutate(time_num= case_when(
    startsWith(t, "dry") ~ "-20",
    startsWith(t, "t0") ~ "0",
    startsWith(t, "t24") ~ "24",
    startsWith(t, "t48") ~ "48", 
    startsWith(t, "t4") ~ "96",
    startsWith(t, "t8") ~ "192", 
    startsWith(t, "t16") ~ "384", 
  ))


dat_means$time_num <- as.numeric(dat_means$time_num)
  
# Plot time series 
palette1 <- c("#FAFFA1", "#BCD980", "#5D966D", "#3D6160")

tiff("time_series_CO2", units="in", width=7, height=5, res=300)

time_series_CO2 <- ggplot(dat_means, aes(x=as.numeric(time_num), y=mean_CO2_C_mg_m2_h)) +
  geom_line(aes(linetype=temp_C,  colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(shape=temp_C, colour=leaf_treatment), size=2.5, alpha=0.7) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-20,0,24,48,72, 96, 192, 384), labels = c("dry", "0", "24", "48", "72", "96", "192", "384"))+ xlab("Time since wetting (hours)") + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) 
time_series_CO2

dev.off()

#Because there is such  large jump in CH4 at the last time poit, the scale conceals the trends before hand, so consider an inset map of time_num<=190

short_time_series_CH4 <- ggplotGrob(ggplot(subset(dat_means, time_num<=190), aes(x=time_num, y=mean_CH4_C_mg_m2_h)) +
  geom_line(aes(linetype=temp_C,  colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(shape=temp_C, colour=leaf_treatment), size=1.5, alpha=0.8) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), legend.title=element_blank() , axis.line = element_line() , legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_continuous(breaks = c(-20,0,24,48,72, 96, 192, 384), labels = c("dry", "0", "24", "48", "72", "96", "192", "384"))+ xlab("Time since wetting (hours)")  + ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1)) )
short_time_series_CH4

tiff("time_series_CH4", units="in", width=7, height=5, res=300)

full_time_series_CH4 <- ggplot(dat_means, aes(x=time_num, y=mean_CH4_C_mg_m2_h)) +
  annotation_custom(grob = short_time_series_CH4, xmin = -40, xmax = 280, ymin = 40, ymax = 89) +
  geom_line(aes(linetype=temp_C,  colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(shape=temp_C, colour=leaf_treatment), size=2.5, alpha=0.8) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-20,0,24,48,72, 96, 192, 384), labels = c("dry", "0", "24", "48", "72", "96", "192", "384"))+ xlab("Time since wetting (hours)")  + ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1))
full_time_series_CH4

dev.off()


############# Statistical analysis ######################################
#One-way repeated measures ANOVA of the effect of temperature and OM content on GHGs, also check for the interactive effect. 


#for data analysis, exclude any NAs in the GHG column
dat <- dat %>% drop_na(CO2_C_mg_m2_h)


#Check for outliers

identify_outliers(data.frame(dat$CO2_C_mg_m2_h)) #no extreme outliers
identify_outliers(data.frame(dat$CH4_C_ug_m2_h)) #some extreme outliers

#Check for normality
ggqqplot(dat$CO2_C_mg_m2_h) #relatively normal, but a bit skewed at the extremes
ggqqplot(log(dat$CO2_C_mg_m2_h) ) #Improve after log transformation
ggqqplot(sqrt(dat$CO2_C_mg_m2_h) ) #Improve after square root transformation, but not as good as log
ggqqplot((dat$CO2_C_mg_m2_h)^(1/3) )  #Improved after cube root, better than square root, but not as good as log
ggqqplot(dat, "CO2_C_mg_m2_h", facet.by = "Date")

ggqqplot(dat$CH4_C_ug_m2_h) #not good, 2 "outliers"
min(dat$CH4_C_ug_m2_h, na.rm=T) #-3.407595
ggqqplot(log(dat$CH4_C_ug_m2_h + 4.407595)) #better, but still skewed, but this is the best option
ggqqplot(sqrt(dat$CH4_C_ug_m2_h + 4.407595)) #not good
ggqqplot((dat$CH4_C_ug_m2_h)^(1/3) )  #not good
ggqqplot(subset(dat,CH4_C_ug_m2_h<50), "CH4_C_ug_m2_h", facet.by = "Date") # outliers on 2022-08-11

AFDM <- subset(dat, t=="dry") #remove NAs for the control
AFDM <- AFDM %>% drop_na(percent_AFDM_remaining)
ggqqplot(AFDM$percent_AFDM_remaining) #Normal


#Summary stats
dat %>%
  group_by(leaf_treatment, temp_C) %>%
  get_summary_stats(CO2_C_mg_m2_h, type = "mean_sd") #replace with CH4 

#Summary stats
dat %>%
  group_by(t) %>%
  get_summary_stats(CH4_C_ug_m2_h, type = "mean_sd") #replace with CH4 

# Run the repeated measures ANOVA
CO2_temp <- aov(CO2_C_mg_m2_h ~ temp_C + Error(Column_ID/Date), data = dat)
summary(CO2_temp) #sig

CO2_OM <- aov(CO2_C_mg_m2_h ~ leaf_treatment + Error(Column_ID/Date), data = dat)
summary(CO2_OM) #sig

CH4_temp <- aov(CH4_C_ug_m2_h ~ temp_C + Error(Column_ID/Date), data = dat)
summary(CH4_temp) #p = 0.4

CH4_OM <- aov(CH4_C_ug_m2_h ~ leaf_treatment + Error(Column_ID/Date), data = dat)
summary(CO2_OM) #sig

# Run two-way repeated measures ANOVA to test for individual and interactive effects of temperature and OM content
aov_CO2_temp_OM <- aov(log(CO2_C_mg_m2_h) ~ temp_C * leaf_treatment + Error(Column_ID/(Date)), data = dat)
summary(aov_CO2_temp_OM) #individually significant effects, but non-significant interaction effect

aov_CH4_temp_OM <- aov(log(CH4_C_ug_m2_h+ 4.407595) ~ temp_C * leaf_treatment + Error(Column_ID/(Date)), data = dat)
summary(aov_CH4_temp_OM) # temp 0.001, leaf treatment <0.0001, interaction: 0.83

#for AFDM use two way ANOVA with interaction, no need for repeated measures structure
aov_AFDM_temp_OM <- aov(percent_AFDM_remaining ~ temp_C * leaf_treatment, data = AFDM)
summary(aov_AFDM_temp_OM)

#Run post hoc test following repeated measures ANOVA #TukeyHSD does not work on repeated measures so can use emmeans

# Run post-hoc testing with emmeans
emmCO2 <- emmeans(aov_CO2_temp_OM, ~ temp_C * leaf_treatment)
pairs(emmCO2) #pairwise comparisons #default p adjustment is "Tukey"

emmCH4 <- emmeans(aov_CH4_temp_OM, ~ temp_C * leaf_treatment)
pairs(emmCH4) #pairwise comparisons #default p adjustment is "Tukey"

TukeyHSD(aov_AFDM_temp_OM) #no significant pairwise comparisons

#### Run linear mixed effect model ####
lme_CO2 <- lme(CO2_C_mg_m2_h ~ temp_C * leaf_treatment, data=dat, random = ~1|Column_ID/Date)
summary(lme_CO2)

#Set reference level
dat$leaf_treatment <- relevel(dat$leaf_treatment, ref = "control")
dat$leaf_treatment <- factor(dat$leaf_treatment, levels=c("control","low", "medium", "high" ))
levels(dat$leaf_treatment)
dat$temp_C <- relevel(dat$temp_C, ref = "20")
levels(dat$temp_C)

lmer_CO2 <- lmer(CO2_C_mg_m2_h ~ temp_C * leaf_treatment + (1 | Column_ID/Date), data = dat)
summary(lmer_CO2)

#try using lsmeans for a post hoc test 


