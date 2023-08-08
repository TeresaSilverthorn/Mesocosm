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
library(cowplot) #inset plots
library(MuMIn)
library(MASS)
library(car)

#### Set wd for figures ####
setwd("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Figures")

#### Load data ####

#Load in the GHG flux results from Mesocosm_GHGR.R, combined with the ancillary data and treatment codes.

dat <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/CO2.CH4.fluxes.csv")

str(dat) #252 obs of 28 vars #since we measured DO on one day that we did not measure GHGs, that measure is missing here

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

##### Import decomposition data ####
leaf_mass_loss <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/leaf_mass_loss.csv")
str(leaf_mass_loss) #36 obs. of  3 variables

#merge
dat <- merge(dat, leaf_mass_loss, by="Column_ID")


#### Import and merge sediment data ####
OM <- fread ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/Sediment Data/Sediment_OM_mesocosm.csv")
str(OM)

#Subset relevant columns
OM<- subset(OM, select = c("Sample ID", "sed_moisture_percent", "sed_OM_percent"))

#rename
names(OM)[names(OM) == "Sample ID"] <- "Column_ID"

#merge
dat <- merge(dat, OM, by="Column_ID")

#### Import the ibutton daily average data ####
Tave <- read.csv("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/Daily_ave_ibutton_temps_mesocosm.csv")

#merge with dat
Tave$Date <-as.POSIXct(Tave$Date, format="%Y-%m-%d", tz = "Europe/Berlin")

#Merge based on dat (left merge)

dat <- merge(x=dat, y=Tave, by=c("Date", "Column_ID"), all.x=TRUE)


# Get temperature averages excluding dry and t0

Tave_summary <- dat %>%
  dplyr::group_by(temp_C) %>%
  dplyr::filter(t != "dry" & t != "t0") %>%
  dplyr::summarise(mean_Avg_Temp = mean(Avg_Temp), sd_Avg_Temp = sd(Avg_Temp))

Water_temp_summary <- dat %>%
  dplyr::group_by(temp_C) %>%
  dplyr::filter(t != "dry" & t != "t0") %>%
  dplyr::summarise(mean_Avg_Temp = mean(Water_temp_C), sd_Avg_Temp = sd(Water_temp_C)) 

#can add Column ID in group by, to see the variability by column


#when comparing the ibutton temperatures to the point measures I took, they do not always correspond... Check this!
palette1 <- c("#e1e590", "#BCD980", "#5D966D", "#3D6160")


temp1 <- ggplot(dat, aes(x=Avg_Temp, y=Water_temp_C)) + 
  geom_point(aes(shape=temp_C,  colour=as.factor(Date)), size=2.5, alpha=0.7) + geom_abline() +
 # scale_fill_manual(values = palette1) + scale_colour_manual(values = palette1) + 
  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + xlim(20,33) + ylim(20,33)
temp1

temp <- ggplot(subset(dat, Date!="2022-08-11"), aes(x=Water_temp_C, y=Avg_Temp,)) + 
    geom_point(aes(shape=temp_C,  colour=leaf_treatment), size=2.5, alpha=0.7) + geom_abline() +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)  + stat_regline_equation(label.y = 30) + stat_cor(label.y = 29) +
  geom_text(aes(label = Column_ID), size = 3, nudge_x = 0.2, nudge_y = 0.2) +
  scale_fill_manual(values = palette1) + scale_colour_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + xlim(20,33) + ylim(20,33)
temp #relationship y = 5.9+0.71x

#Overall they have a good relationship, but there are a few that are off, eg/ some 30C that are reported too low by the ibutton, perhaps the ibuttom was out of the water and we need to correct this!
#no point measures for 2022-07-26
#2022-07-27 and 28 looks good
#2022-07-29 perhaps one of the 25C ibuttons was out of the water
#2022-07-30 perhaps one of the 25C ibuttons was out of the water
#2022-08-03 perhaps one or two of the 30 was out of water
#2022-08-11 maybe all of them were out of the water???? In my notes I have 26,31,33,32,28 temp regulator out of water

#So I think it's better to use the manual temperatures than the ibuttons, especially for 2022-08-11, because by then there was so much evaporation (esp at high temps) that the temperature controller as well as the ibutton itself may have been out of the water

tiff("manual_temp", units="in", width=7, height=6, res=300)

manual_temp <- ggplot(subset(dat, Date !="2022-07-29"), aes(x=as.factor(Date), y=Water_temp_C)) + 
  geom_point(aes(shape=temp_C), size=2, alpha=0.4) + 
  geom_line(aes(group = Column_ID, color = as.factor(Column_ID)), size = 1) + 
  theme_bw() + theme(axis.title = element_text(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylim(19,33.3)
manual_temp

dev.off()

tiff("ibutton_temp", units="in", width=7, height=6, res=300)

ibutton_temp <- ggplot(subset(dat, Date !="2022-07-26"), aes(x=as.factor(Date), y=Avg_Temp)) + 
  geom_point(aes(shape=temp_C), size=2, alpha=0.4) + 
  geom_line(aes(group = Column_ID, color = as.factor(temp_C)), size = 1) + 
  theme_bw() + theme(axis.title = element_text(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylim(19,33.3) 
ibutton_temp

dev.off()

#Try replacing the 2022-08-11 Avg_Temp data with the Water_temp_C data, and corrected by the equation of the relationship between the two measurement types

dat <- dat %>%
  mutate(Avg_Temp = case_when(
    Date == "2022-08-11" ~ 5.9 + 0.71 * Water_temp_C,
    TRUE ~ Avg_Temp
  ))


temp <- ggplot(subset(dat, Date !="2022-07-26"), aes(x=as.factor(Date), y=Avg_Temp)) + 
  geom_point(aes(shape=temp_C), size=2, alpha=0.4) + 
  geom_line(aes(group = Column_ID, color = as.factor(temp_C)), size = 1) + 
  theme_bw() + theme(axis.title = element_text(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylim(19,33.3) 
temp

#make another column for time as days, also numerical
dat <- dat %>%
  mutate(t_days= case_when(
    startsWith(t, "dry") ~ "-1",
    startsWith(t, "t0") ~ "0",
    startsWith(t, "t24") ~ "1",
    startsWith(t, "t48") ~ "2", 
    startsWith(t, "t72") ~ "3", 
    startsWith(t, "t4") ~ "4",
    startsWith(t, "t8") ~ "8", 
    startsWith(t, "t16") ~ "16", 
  ))

dat$t_days <- as.numeric(dat$t_days)

tiff("temp_final", units="in", width=7, height=5, res=300)

palette3 <- c( "#BCD980", "#5D966D", "#3D6160")

ibutton_temp_final <- ggplot(subset(dat, Date !="2022-07-26"), aes(x=as.factor(t_days), y=Avg_Temp)) + 
  geom_point(aes(shape=temp_C, colour=temp_C), size=2, alpha=0.9) + 
  scale_color_manual(values = palette3) +
  geom_line(aes(group = Column_ID, color = as.factor(temp_C)), size = 0.8, alpha=0.5) + 
  theme_bw() + theme(axis.title = element_text(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylim(19,33.3) +  ylab("Water temperature (\u00B0C)") + xlab("Days since water addition") 
ibutton_temp_final

dev.off()


Tave_summary2 <- dat %>%
  dplyr::group_by(temp_C) %>%
  dplyr::filter(t != "dry" & t != "t0") %>%
  dplyr::summarise(mean_Avg_Temp = mean(Avg_Temp), sd_Avg_Temp = sd(Avg_Temp))


###______________________
#make a new category for control vs leaves

dat <- dat %>%
  mutate(leaf_presence= case_when(
    startsWith(as.character(leaf_treatment), "control") ~ "sediment",
    startsWith(as.character(leaf_treatment), "low") ~ "sediment + OM",
    startsWith(as.character(leaf_treatment), "medium") ~ "sediment + OM",
    startsWith(as.character(leaf_treatment), "high") ~ "sediment + OM"  ))


# summary by date

summary2 <- dat %>%
  dplyr::group_by(Date, leaf_presence) %>%
  dplyr::summarise(
       mean_CO2_C_mg_m2_h = mean(CO2_C_mg_m2_h), 
    sd_CO2_C_mg_m2_h = sd(CO2_C_mg_m2_h), 
    mean_CH4_C_mg_m2_h = mean(CH4_C_mg_m2_h),
    sd_CH4_C_mg_m2_h = sd(CH4_C_mg_m2_h))

write.csv(summary2, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/date_flux_means.csv")

leaf_presence_CO2 <- ggplot(subset(dat, t!="t72"), aes(as.factor(t_days), CO2_C_mg_m2_h, fill=leaf_presence)  ) + geom_boxplot() +  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) + xlab("Days since water addition") + scale_fill_manual(values = c("#e1e590", "#5D966D")) +  scale_x_discrete(labels = c("dry", "0", "1", "2", "4", "8", "16"))
leaf_presence_CO2

min(dat$CH4_C_mg_m2_h, na.rm=T)

leaf_presence_CH4 <- ggplot(subset(dat, t!="t72"), aes(as.factor(t_days), log(CH4_C_mg_m2_h +0.003350449) , fill=leaf_presence)  ) + geom_boxplot() +  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +   ylab(expression(log~CH[4]~flux~(mg~CH[4]*`-C`~m^-2~h^-1))) + xlab("Days since water addition") + scale_fill_manual(values = c("#e1e590", "#5D966D")) +  scale_x_discrete(labels = c("dry", "0", "1", "2", "4", "8", "16"))
leaf_presence_CH4 #this excludes 1 outlier

#Combine 

tiff("leaf_presence_CO2CH4", units="in", width=6, height=7, res=300)

leaf_presence_CO2CH4 <- ggarrange(leaf_presence_CO2 +theme(axis.title.x = element_blank()),  leaf_presence_CH4 ,
                         ncol = 1, nrow = 2, align="hv",common.legend = T,legend="top",  labels = c("(a)", "(b)"))
leaf_presence_CO2CH4

dev.off()

#Calculate the percent increase 
#Increase ÷ Original Number × 100
CO2_sed <- mean(summary2$mean_CO2_C_mg_m2_h[summary2$leaf_presence == "sediment"],) #17.9422
CO2_OM <-mean(summary2$mean_CO2_C_mg_m2_h[summary2$leaf_presence == "leaf"],)     #88.89439
((CO2_OM-CO2_sed)/CO2_sed) *100  #395.4487 percent increase
#or alternatively 
CO2_OM/CO2_sed #4.954487 times higher


CH4_sed <-mean(summary2$mean_CH4_C_mg_m2_h[summary2$leaf_presence == "sediment"],) #0.01209158
CH4_OM <-mean(summary2$mean_CH4_C_mg_m2_h[summary2$leaf_presence == "leaf"],)     #2.196209
((CH4_OM-CH4_sed)/CH4_sed) *100  #18063.12 percent increase
#or alternatively 
CH4_OM/CH4_sed # 181.6312times higher

# summary by treatment
summary3 <- dat %>%
  dplyr::group_by(leaf_treatment, temp_C) %>%
  dplyr::filter(t != "dry" & t != "t0") %>%
  dplyr::summarise(
    mean_CO2_C_mg_m2_h = mean(CO2_C_mg_m2_h, na.rm=T), 
    sd_CO2_C_mg_m2_h = sd(CO2_C_mg_m2_h, na.rm=T), 
    mean_CH4_C_mg_m2_h = mean(CH4_C_mg_m2_h, na.rm=T),
    sd_CH4_C_mg_m2_h = sd(CH4_C_mg_m2_h, na.rm=T))

# save as csv

write.csv(summary3, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/treatment_flux_means.csv")



###############################################################################

# Save dat as a csv

str(dat) #288 obs 36 vars

write.csv(dat, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/dat.csv")


###############################################################################
### Treatment means ####

dat_means <- dat %>% 
  dplyr::group_by(Date, t, temp_C, leaf_treatment) %>% # we group by the two values
  dplyr::summarise(
    mean_CO2_C_mg_m2_h = mean(CO2_C_mg_m2_h),
    sd_CO2_C_mg_m2_h = sd(CO2_C_mg_m2_h),
    mean_CH4_C_mg_m2_h = mean(CH4_C_mg_m2_h),
    sd_CH4_C_mg_m2_h = sd(CH4_C_mg_m2_h),
    DO_mg_L = mean(DO_mg_L),
    DO_percent = mean(DO_percent),
    Water_temp_C = mean(Water_temp_C),
    Avg_Temp = mean(Avg_Temp),  
    sediment_g = mean(sediment_g),
    dry_input_leaves_g = mean(dry_input_leaves_g),
    dry_output_leaves_g = mean(dry_output_leaves_g),
    incubation_water_mL = mean(incubation_water_mL),
    groundwater_ml = mean(groundwater_ml),
    sed_moisture_percent = mean(sed_moisture_percent),
    sed_OM_percent = mean(sed_OM_percent) )

str(dat_means) #84 obs of 19 vars


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

#make another column for time as days, also numerical
dat_means <- dat_means %>%
  mutate(t_days= case_when(
    startsWith(t, "dry") ~ "-1",
    startsWith(t, "t0") ~ "0",
    startsWith(t, "t24") ~ "1",
    startsWith(t, "t48") ~ "2", 
    startsWith(t, "t4") ~ "4",
    startsWith(t, "t8") ~ "8", 
    startsWith(t, "t16") ~ "16", 
  ))

dat_means$t_days <- as.numeric(dat_means$t_days)

# Save as a csv

str(dat_means) #288 obs 36 vars

write.csv(dat_means, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/dat_means.csv")


###############################################################################
#### Plot the AFDM data ####

#plot as boxplot
palette2 <- c("#cba736", "#ba5027", "#741016")

tiff("AFDM_remain", units="in", width=6, height=4, res=300)

AFDM_remain <-  ggplot(subset(dat, leaf_treatment!= "control" & t=="dry"), aes(x=leaf_treatment, y=percent_AFDM_remaining, fill=temp_C)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=8,
               outlier.size=4) + scale_fill_manual(values = palette2) +
  ylab("AFDM remaining (%)") +
  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line(), axis.title.x = element_blank())
AFDM_remain 

dev.off()


#plot as dots
palette2 <- c("#e2b74b", "#B7692A", "#A01313")

tiff("AFDM_remain_points", units="in", width=6, height=4, res=300)

AFDM_remain <-  ggplot(subset(dat, leaf_treatment!= "control" & t=="dry"), aes(x=leaf_treatment, y=percent_AFDM_remaining, colour=temp_C, group=interaction(leaf_treatment, temp_C))) + 
  geom_jitter(position = position_dodge(0.8), shape = 16, size = 3, alpha=0.6, stroke = 0.5)+ scale_colour_manual(values = palette2) +
  stat_summary(
    fun.data = function(x) {
      ymin <- min(x)
      ymax <- max(x)
      return(data.frame(ymin = ymin, ymax = ymax))
    },
    geom = "linerange", end_cap=F,width = 0.2,position = position_dodge(0.8)) +
  scale_colour_manual(values = palette2) +
  ylab("AFDM remaining (%)") +
  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line(), axis.title.x = element_blank())
AFDM_remain 

dev.off()

#### Plots AFDM vs CO2 and CH4 at the last day #### Can also try against the sum of emissions?
tiff("CO2_AFDM", units="in", width=5, height=5, res=300)

CO2_AFDM <- ggplot(subset(dat, leaf_treatment!= "control" & t=="t16"), aes(percent_AFDM_remaining, CO2_C_mg_m2_h)) + geom_point(aes(colour=temp_C, shape=leaf_treatment), size=3.5, alpha=0.6) +  theme_bw() +  theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank(), axis.ticks.x=element_blank(),  legend.position=c(.9,.75), axis.line = element_line(colour = "black"), text = element_text(size = 20), axis.text = element_text(size = 20, colour="black"))   + scale_colour_manual(values = palette2) +  geom_smooth(aes(colour=temp_C), method = "lm", linetype="dashed", se=FALSE,  formula = y ~ x) 
CO2_AFDM

dev.off()

tiff("CH4_AFDM", units="in", width=5, height=5, res=300)

CH4_AFDM <- ggplot(subset(dat, leaf_treatment!= "control" & t=="t16"), aes(percent_AFDM_remaining, CH4_C_mg_m2_h)) + geom_point(aes(colour=temp_C, shape=leaf_treatment), size=3.5, alpha=0.6) +  theme_bw() +  theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank(), axis.ticks.x=element_blank(),  legend.position=c(.9,.75), axis.line = element_line(colour = "black"), text = element_text(size = 20), axis.text = element_text(size = 20, colour="black"))   + scale_colour_manual(values = palette2) +  geom_smooth(aes(colour=temp_C), method = "lm", linetype="dashed", se=FALSE,  formula = y ~ x) + coord_trans(y=pseudolog10_trans)
CH4_AFDM

dev.off()

#Check correlation matrix
sub_dat1 <- subset(dat, leaf_treatment!= "control" & t=="t16")
head(sub_dat1)

result_CO2 <- sub_dat1 %>%
  group_by(temp_C) %>%
  summarise(correlation = cor(CO2_C_mg_m2_h, percent_AFDM_remaining),
            p_value = cor.test(CO2_C_mg_m2_h, percent_AFDM_remaining)$p.value) %>%
  ungroup()

result_CH4 <- sub_dat1 %>%
  group_by(temp_C) %>%
  summarise(correlation = cor(CH4_C_mg_m2_h, percent_AFDM_remaining),
            p_value = cor.test(CH4_C_mg_m2_h, percent_AFDM_remaining)$p.value) %>%
  ungroup()



#plot CO2 vs CH4, excluding 2 very high CH4 values

tiff("CO2vCH4", units="in", width=5, height=5, res=300) 

ghg <- ggplot(subset(dat, leaf_treatment!= "control" & t=="t16"), aes(CO2_C_mg_m2_h, CH4_C_mg_m2_h)) + geom_point(aes(colour=temp_C, shape=leaf_treatment), size=3.5, alpha=0.6) +  theme_bw() +  theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank(), axis.ticks.x=element_blank(),  legend.position=c(.9,.75), axis.line = element_line(colour = "black"), text = element_text(size = 20), axis.text = element_text(size = 20, colour="black"))   + scale_colour_manual(values = palette2) + ylim(-5, 12000) 
ghg

dev.off()


sub <- subset(dat, t=="dry")
sd(sub$CH4_C_mg_m2_h, na.rm=T)


# Summary data
summary <- dat %>%
  dplyr::group_by(leaf_treatment) %>%
  dplyr::summarise(
    #mean_sediment_g = mean(sediment_g),
    # sd_mean_sediment_g = sd(sediment_g), 
    mean_dry_input_leaves_g = mean(dry_input_leaves_g), 
    sd_dry_input_leaves_g = sd(dry_input_leaves_g))



#### Plot the DO data #### 

dat_withDO <- dat #should have 288 obs

tiff("DO_leaftreatment", units="in", width=7, height=5, res=300)

DOplot <- ggplot(dat, aes(as.factor(Date), DO_mg_L )) + 
  theme_bw() + geom_boxplot() 
DOplot 

dev.off()


#DO means by treatment
DO_means <- dat_withDO  %>% 
  dplyr::group_by(Date, temp_C, leaf_presence) %>% # we group by the two values
  dplyr::summarise(
    DO_mg_L = mean(DO_mg_L),
    sd=mean(DO_mg_L),
    DO_percent = mean(DO_percent) )

str(DO_means) 

x <- subset(DO_means, leaf_presence!="sediment")
max(x$DO_mg_L, na.rm=T) #0.2666667
  
#format Date
DO_means$Date <-as.POSIXct(DO_means$Date, format="%Y-%m-%d", tz = "Europe/Berlin")


tiff("DO_timeseries", units="in", width=7, height=5, res=300)
#plot change in DO over time
palette1 <- c("#e1e590", "#BCD980", "#5D966D", "#3D6160")

time_series_DO <- ggplot(subset(dat, Date>="2022-07-29"), aes(x=as.numeric(t_days), y=DO_mg_L)) +
  #geom_line(aes(linetype=temp_C, colour=leaf_treatment, group = interaction(temp_C, leaf_treatment, drop=T) ) )+
  geom_line(aes(group = interaction(leaf_treatment, temp_C), linetype=temp_C, colour = interaction(leaf_treatment)), stat = "summary", fun = "mean", size = 0.7) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  geom_point(aes(shape=temp_C,  colour=leaf_treatment), size=2.5, alpha=0.7, position=position_dodge(0.05)) +
  #geom_errorbar(aes(ymin=DO_mg_L-sd, ymax=DO_mg_L+sd, colour=leaf_treatment), width=.2 )+
  scale_fill_manual(values = palette1) + scale_colour_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() )   + ylab("Dissolved oxygen (mg/L)") + xlab("Days since water addition") +scale_x_continuous(breaks = c(3, 4, 8, 16))
time_series_DO

dev.off()

#What is the relationship between DO and CH4/CO2 fluxes ?

palette3 <- c( "#BCD980", "#5D966D", "#3D6160")

inset_DO <- ggplot(subset(dat_means, leaf_treatment!="control"), aes(x=DO_mg_L, y=mean_CO2_C_mg_m2_h)) +
  geom_point(aes(colour=leaf_treatment), size=2.5, alpha=0.7) +
  scale_color_manual(values = palette3) + theme_bw() + theme(legend.position = "none", axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1))  + stat_smooth(aes(colour=leaf_treatment), geom="line", size=1, alpha=0.7, method = "lm", se=FALSE,  formula = y ~ x)
inset_DO


CO2_DO <- ggplot(dat_means, aes(x=DO_mg_L, y=mean_CO2_C_mg_m2_h)) +
  geom_point(aes(colour=leaf_treatment), size=2.5, alpha=0.7) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1))  + stat_smooth(aes(colour=leaf_treatment), geom="line", size=1, alpha=0.7, method = "lm", se=FALSE,  formula = y ~ x)
CO2_DO


#Add inset figure using ggdraw
tiff("CO2_DO_full", units="in", width=7, height=5, res=300)

CO2_DO_full <- ggdraw(CO2_DO) +
  draw_plot(inset_DO, height = 0.45, width=0.45, x = 0.35, y = 0.5) 
CO2_DO_full

dev.off()


# Ch4 vs DO
CH4_DO_inset <- ggplot(subset(dat_means, leaf_treatment!="control"), aes(x=DO_mg_L, y=mean_CH4_C_mg_m2_h)) +
  geom_point(aes(colour=leaf_treatment), size=2.5, alpha=0.7) +
  scale_color_manual(values = palette3) + theme_bw() + theme(legend.position="none", axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() )  + stat_smooth(aes(colour=leaf_treatment), geom="line", size=1, alpha=0.7, method = "lm", se=FALSE,  formula = y ~ x) 
CH4_DO_inset

CH4_DO <- ggplot(dat_means, aes(x=DO_mg_L, y=mean_CH4_C_mg_m2_h)) +
  geom_point(aes(colour=leaf_treatment), size=2.5, alpha=0.7) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() )  + stat_smooth(aes(colour=leaf_treatment), geom="line", size=1, alpha=0.7, method = "lm", se=FALSE,  formula = y ~ x) 
CH4_DO

#Add inset figure using ggdraw
tiff("CH4_DO_full", units="in", width=7, height=5, res=300)

CH4_DO_full <- ggdraw(CH4_DO) +
  draw_plot(CH4_DO_inset, height = 0.45, width=0.45, x = 0.35, y = 0.5) 
CH4_DO_full

dev.off()



#### Plot OM content ####
#Note the initial sediment OM content: need to burn the dry sediment sample along with the one missing valueM22 


tiff("OM", units="in", width=7, height=5, res=300)

OMplot <- ggplot(subset(dat, t=="dry"), aes(leaf_treatment, sed_OM_percent)) + ylab("Sediment OM (%)")+
  theme_bw() + geom_boxplot(aes(fill = temp_C), colour="black")   + scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505")) 
OMplot 

dev.off()

levels(dat$t)

# Plot all of the GHG data
boxplot(CO2_C_mg_m2_h ~ start_time, data= (dat))

boxplot(CH4_C_mg_m2_h ~ start_time, data= (dat)) #two very high CH4 fluxes: 2022-08-11_t16_32, and 2022-08-11_t16_28...  


##### Plot headspace height / volume ######


palette1 <- c("#e1e590", "#BCD980", "#5D966D", "#3D6160")

time_series_volume <- ggplot(subset(dat, t!="dry" & t!="t72"), aes(x=as.numeric(t_days), y=volume_L)) +
 geom_line(aes(group = interaction(leaf_treatment, temp_C), linetype=temp_C, colour = interaction(leaf_treatment)), stat = "summary", fun = "mean", size = 0.7) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  geom_point(aes(shape=temp_C,  colour=leaf_treatment), size=2.5, alpha=0.7, position=position_dodge(0.05)) +
  scale_fill_manual(values = palette1) + scale_colour_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab("Headspace volume (L)") + xlab("Days since water addition") + scale_x_continuous(breaks = c(0, 1, 2, 4, 8, 16))
time_series_volume



#Instead of headspace volume, plot the water volume, which is 1.5 (water) plus whatever volume the sediments occupied ~0.43 to 0.4 - -headspace volume

tiff("time_series_volume", units="in", width=7.5, height=4, res=300)

time_series_volume2 <- ggplot(subset(dat, t!="dry" & t!="t72"), aes(x=as.numeric(t_days), y=1.85-volume_L)) +
  geom_line(aes(group = interaction(leaf_treatment, temp_C), linetype=temp_C, colour = interaction(leaf_treatment)), stat = "summary", fun = "mean", size = 0.7) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  geom_point(aes(shape=temp_C,  colour=leaf_treatment), size=2.5, alpha=0.7, position=position_dodge(0.05)) +
  scale_fill_manual(values = palette1) + scale_colour_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab("Water volume (L)") + xlab("Days since water addition") + scale_x_continuous(breaks = c(0, 1, 2, 4, 8, 16))
time_series_volume2

dev.off()


###########  Plot GHGs by sampling time  ########################

#dry
CO2_dry <- ggplot(subset(dat, t=="dry"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
   theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
     aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "Dry")
CO2_dry 
#weird that the high temp treatments are so consistently variable... effect of mesocosm location? 

CH4_dry <- ggplot(subset(dat, t=="dry"), aes(x=leaf_treatment, y=CH4_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "Dry")
CH4_dry 

#T0
CO2_t0 <- ggplot(subset(dat, t=="t0"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t0")
CO2_t0 

CH4_t0 <- ggplot(subset(dat, t=="t0"), aes(x=leaf_treatment, y=CH4_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t0")
CH4_t0 

#T24
CO2_t24 <- ggplot(subset(dat, t=="t24"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t24")
CO2_t24 

CH4_t24 <- ggplot(subset(dat, t=="t24"), aes(x=leaf_treatment, y=CH4_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t24")
CH4_t24 

#T48
CO2_t48 <- ggplot(subset(dat, t=="t48"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t48")
CO2_t48 

CH4_t48 <- ggplot(subset(dat, t=="t48"), aes(x=leaf_treatment, y=CH4_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t48")
CH4_t48

#T4
CO2_t4 <- ggplot(subset(dat, t=="t4"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t4")
CO2_t4 

CH4_t4 <- ggplot(subset(dat, t=="t4"), aes(x=leaf_treatment, y=CH4_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t4")
CH4_t4

#T8
CO2_t8 <- ggplot(subset(dat, t=="t8"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.8, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t8")
CO2_t8 

CH4_t8 <- ggplot(subset(dat, t=="t8"), aes(x=leaf_treatment, y=CH4_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t8")
CH4_t8

#T16
CO2_t16 <- ggplot(subset(dat, t=="t16"), aes(x=leaf_treatment, y=CO2_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.8, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t16")
CO2_t16 

CH4_t16 <- ggplot(subset(dat, t=="t16"), aes(x=leaf_treatment, y=CH4_C_mg_m2_h, label=Column_ID) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C) ) + geom_dotplot(
    aes(fill = temp_C, color =temp_C) , binaxis='y', stackdir='center', dotsize = .5, position = position_dodge(0.8))   + geom_text(aes(label=Column_ID, color=temp_C), hjust=1.4, vjust=0, position = position_dodge(0.8), size=2.5) +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  + labs(title = "t16")
CH4_t16


#########################
#### Plot GHGs by treatments ####

tiff("CO2_full", units="in", width=7, height=5, res=300)

CO2_full <- ggplot(dat, aes(x=leaf_treatment, y=CO2_C_mg_m2_h) ) + 
  theme_bw() + geom_boxplot(aes(color = temp_C), outlier.shape = NA ) + geom_point(
    aes(fill = temp_C, color =temp_C),  size=3, position = position_dodge(0.8))   +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505"))  
CO2_full

dev.off()

tiff("CH4_full", units="in", width=7, height=5, res=300)

lab_y <- c("-1", "0", "1", "10", "100") 
brk_y <- c(-1, 0, 1, 10, 100)

CH4_full <- ggplot(subset(dat, leaf_treatment !="NA"), aes(x=leaf_treatment, y=CH4_C_mg_m2_h)) + 
  theme_bw() + geom_boxplot(aes(color = temp_C), outlier.shape = NA ) + geom_point(
    aes(fill = temp_C, color =temp_C),  size=3, position = position_dodge(0.8))   +  scale_fill_manual(values = c("#F3E109", "#ED9D12", "#F04505"))+   scale_color_manual(values = c("#F3E109", "#ED9D12", "#F04505")) + coord_trans(y=pseudolog10_trans) + scale_y_continuous( breaks = brk_y,labels = lab_y, limits = c(-0.004, 160)) 
CH4_full


dev.off()  #log2 transformation of the axis results in negative numbers to be removed. Can use scale_y_continuous(trans=pseudolog10_trans)  or manually set the scale



  
#### Plot GHG time series ### 
palette1 <- c("#FAFFA1", "#BCD980", "#5D966D", "#3D6160")

tiff("time_series_CO2", units="in", width=7, height=5, res=300)

time_series_CO2 <- ggplot(dat_means, aes(x=as.numeric(t_days), y=mean_CO2_C_mg_m2_h)) +
  geom_line(aes(linetype=temp_C,  colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(shape=temp_C, colour=leaf_treatment), size=2.5, alpha=0.7) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-1,0,1,2,3, 4, 8, 16), labels = c("dry", "0", "1", "2", "3", "4", "8", "16"))+ xlab("Time since water addition (days)") + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) 
time_series_CO2

dev.off()

#Because there is such  large jump in CH4 at the last time point, the scale conceals the trends before hand, so consider an inset map of time_num<=190

short_time_series_CH4 <- ggplotGrob(ggplot(subset(dat_means, t_days<=7), aes(x=t_days, y=mean_CH4_C_mg_m2_h)) +
  geom_line(aes(linetype=temp_C,  colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(shape=temp_C, colour=leaf_treatment), size=1.5, alpha=0.8) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), legend.title=element_blank() , axis.line = element_line() , legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_continuous(breaks = c(-1,0,1,2,3, 4, 8, 16), labels = c("dry", "0", "1", "2", "3", "4", "8", "16"))+ xlab("Time since water addition (days)")  + ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1)) )
short_time_series_CH4

#tiff("time_series_CH4", units="in", width=7, height=5, res=300)

full_time_series_CH4 <- ggplot(dat_means, aes(x=t_days, y=mean_CH4_C_mg_m2_h)) +
  annotation_custom(grob = short_time_series_CH4, xmin = -1, xmax = 10, ymin = 25, ymax = 89) +
  geom_line(aes(linetype=temp_C,  colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(shape=temp_C, colour=leaf_treatment), size=2.5, alpha=0.8) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-1,0,1,2,3, 4, 8, 16), labels = c("dry", "0", "1", "2", "3", "4", "8", "16"))+ xlab("Time since water addition (days)")  + ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1))
full_time_series_CH4

#dev.off()


## Plot a simplified time series for CO2 by subsetting each temperature treatment in a A,B,C subset plot

time_series_CO2_20 <- ggplot(subset(dat_means, temp_C=="20" & t_days!="t72"), aes(x=as.numeric(t_days), y=mean_CO2_C_mg_m2_h)) +
  geom_line(aes(colour=leaf_treatment ) )+
  geom_point(aes(colour=leaf_treatment), size=3, alpha=0.7) +
  scale_color_manual(values = palette1) + theme_bw() + theme(plot.margin = margin(t = 0.0,  r = 0.01,  b = 0.0,  l = 0.01), axis.title = element_text(), panel.grid.major = element_blank(), legend.position="none",  panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-1,0,1,2,4, 8, 16), labels = c("dry", "0", "1", "2", "4", "8", "16"))+ xlab("Time since ater addition (days)") + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) + ylim(0, 235) + ggtitle("(a) 20 \u00B0C ")
time_series_CO2_20

time_series_CO2_25 <- ggplot(subset(dat_means, temp_C=="25" & t_days!="t72"), aes(x=as.numeric(t_days), y=mean_CO2_C_mg_m2_h)) +
  geom_line(aes(colour=leaf_treatment ) )+
  geom_point(aes(colour=leaf_treatment), size=3, alpha=0.7, shape=17) +
  scale_color_manual(values = palette1) + theme_bw() + theme(plot.margin = margin(t = 0.0,  r = 0.01,  b = 0.0,  l = 0.01), axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , legend.position="none",  axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-1,0,1,2, 4, 8, 16), labels = c("dry", "0", "1", "2", "4", "8", "16"))+ xlab("Time since water addition (days)") + ylab(expression(CO[2]~flux~(mg~CO[2]*`-C`~m^-2*~h^-1))) +ylim(0, 235) + ggtitle("(b) 25 \u00B0C")
time_series_CO2_25

time_series_CO2_30 <- ggplot(subset(dat_means, temp_C=="30" & t_days!="t72"), aes(x=as.numeric(t_days), y=mean_CO2_C_mg_m2_h)) +
  geom_line(aes(colour=leaf_treatment ) )+
  geom_point(aes(colour=leaf_treatment), size=3, alpha=0.7, shape=15) +
  scale_color_manual(values = palette1) + theme_bw() + theme(plot.margin = margin(t = 0.0,  r = 0.01,  b = 0.0,  l = 0.01), axis.title = element_text(), panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-1,0,1,2, 4, 8, 16), labels = c("dry", "0", "1", "2", "4", "8", "16"))+ xlab("Time since water addition (days)") + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) + ylim(0, 235) + ggtitle("(c) 30 \u00B0C")
time_series_CO2_30



## Plot a simplified time series for CH4 by subsetting each temperature treatment in a A,B,C subset plot
## add inset figures for up to 96h for CH4

short_time_series_CH4_20 <- ggplot(subset(dat_means, t_days<=7 &  temp_C=="20" & t_days!="t72"), aes(x=t_days, y=mean_CH4_C_mg_m2_h)) +
                                      geom_line(aes(colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
                                      geom_point(aes(colour=leaf_treatment),  size=2.5, alpha=0.8) +
                                      scale_color_manual(values = palette1) + theme_bw() + theme(text = element_text(size = 7), axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), legend.title=element_blank() , axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_line() , legend.position="none") +
                                      scale_x_continuous(breaks = c(-1,0,1,2, 4, 8, 16), labels = c("dry", "0", "1", "2", "4", "8", "16"))
short_time_series_CH4_20

full_time_series_CH4_20 <- ggplot(subset(dat_means, temp_C=="20" & t_days!="t72"), aes(x=t_days, y=mean_CH4_C_mg_m2_h)) +
  geom_line(aes(colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(colour=leaf_treatment), size=2.5, alpha=0.8) +
  scale_color_manual(values = palette1) + theme_bw() + theme(legend.position = "none", axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x = element_blank(), panel.border = element_blank(), axis.title.y = element_text(colour = "white"), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-1,0,1,2, 4, 8, 16), labels = c("dry", "0", "1", "2", "4", "8", "16"))+ #xlab("Time since wetting (hours)")  + 
  ylab(expression(CH[4]~flux~(mg~CH[4]*`-C`~m^-2~h^-1))) + 
  ylim(0, 100) + coord_trans(y=pseudolog10_trans) + ggtitle("(d) 20 \u00B0C ")
full_time_series_CH4_20

#Add inset figure using ggdraw
time_series_CH4_20 <- ggdraw(full_time_series_CH4_20 ) +
  draw_plot(short_time_series_CH4_20, height = 0.45, width=0.45, x = 0.2, y = 0.45) 
time_series_CH4_20
  

##### Inset map for CH4 25 degrees 
short_time_series_CH4_25 <- ggplot(subset(dat_means, t_days<=7 &  temp_C=="25" & t_days!="t72"), aes(x=t_days, y=mean_CH4_C_mg_m2_h)) +
  geom_line(aes(colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(colour=leaf_treatment),  size=2.5, alpha=0.8, shape=17) +
  scale_color_manual(values = palette1) + theme_bw() + theme(text = element_text(size = 7), axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), legend.title=element_blank() , axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_line() , legend.position="none") +
  scale_x_continuous(breaks = c(-1,0,1,2, 4, 8, 16), labels = c("dry", "0", "1", "2",  "4", "8", "16"))
short_time_series_CH4_25

full_time_series_CH4_25 <- ggplot(subset(dat_means, temp_C=="25" & t_days!="t72"), aes(x=t_days, y=mean_CH4_C_mg_m2_h)) +
  geom_line(aes(colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(colour=leaf_treatment), size=2.5, alpha=0.8, shape=17) +
  scale_color_manual(values = palette1) + theme_bw() + theme(legend.position = "none", axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-1,0,1,2,4, 8, 16), labels = c("dry", "0", "1", "2", "4", "8", "16"))+ #xlab("Time since wetting (hours)")  
  ylab(expression(CH[4]~flux~(mg~CH[4]*`-C`~m^-2~h^-1))) +  
  ylim(0, 100) + coord_trans(y=pseudolog10_trans) + ggtitle("(e) 25 \u00B0C ")
full_time_series_CH4_25

#Add inset figure using ggdraw
time_series_CH4_25 <- ggdraw(full_time_series_CH4_25 ) +
  draw_plot(short_time_series_CH4_25, height = 0.45, width=0.45, x = 0.2, y = 0.45) 
time_series_CH4_25


##### Inset map for CH4 30 degrees 
short_time_series_CH4_30 <- ggplot(subset(dat_means, t_days<=7 &  temp_C=="30" & t_days!="t72"), aes(x=t_days, y=mean_CH4_C_mg_m2_h)) +
  geom_line(aes(colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(colour=leaf_treatment),  size=2.5, alpha=0.8, shape=15) +
  scale_color_manual(values = palette1) + theme_bw() + theme(text = element_text(size = 7), axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), legend.title=element_blank() , axis.title.x = element_blank(), axis.title.y = element_blank(), axis.line = element_line() , legend.position="none") +
  scale_x_continuous(breaks = c(-1,0,1,2, 4, 8, 16), labels = c("dry", "0", "1", "2", "4", "8", "16"))
short_time_series_CH4_30

full_time_series_CH4_30 <- ggplot(subset(dat_means, temp_C=="30" & t_days!="t72"), aes(x=t_days, y=mean_CH4_C_mg_m2_h)) +
  geom_line(aes(colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(colour=leaf_treatment), size=2.5, alpha=0.8, shape=15) +
  scale_color_manual(values = palette1) + theme_bw() + theme(legend.position = "none", axis.title = element_text(), panel.grid.major = element_blank(), axis.title.y = element_text(colour = "white"), panel.grid.minor = element_blank(),panel.background = element_blank(),  panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-1,0,1,2, 4, 8, 16), labels = c("dry", "0", "1", "2", "4", "8", "16"))+ xlab("Time since water addition (days)")  + 
  ylab(expression(CH[4]~flux~(mg~CH[4]*`-C`~m^-2~h^-1))) + 
  ylim(0, 100) + coord_trans(y=pseudolog10_trans) + ggtitle("(f) 30 \u00B0C ")
full_time_series_CH4_30

#Add inset figure using ggdraw
time_series_CH4_30 <- ggdraw(full_time_series_CH4_30 ) +
  draw_plot(short_time_series_CH4_30, height = 0.44, width=0.42, x = 0.18, y = 0.48) 
time_series_CH4_30




##### Combine CO2 temperature plots and label ####

tiff("CO2_by_temp", units="in", width=3.45, height=8.45, res=1500)

#Figure 4 in manuscript 
CO2_by_temp <- ggarrange(time_series_CO2_20 + theme(axis.title.y = element_blank(), axis.title.x = element_blank() ),                                    time_series_CO2_25  + theme(axis.title.x = element_blank() ), 
                         time_series_CO2_30 + theme(axis.title.y = element_blank() ),
                         ncol = 1, nrow = 3, align="hv",common.legend = F)
CO2_by_temp

dev.off()

#

##### Combine CH4 temperature plots and label ####


tiff("CH4_by_temp", units="in", width=3.5, height=8.5, res=1500)
#Figure 4 in manuscript 

CH4_by_temp <- ggarrange(time_series_CH4_20 + theme(axis.title.y = element_blank(), axis.title.x = element_blank() ), 
                time_series_CH4_25  + theme(axis.title.x = element_blank() ),
                time_series_CH4_30 + theme(axis.title.y = element_blank() ), 
                ncol = 1, nrow = 3, align="hv",common.legend = F,legend="top")
CH4_by_temp

dev.off()



#### Plot GHGs vs temperature ####

#Plot against the actual iButton data vs the categorical data vs point measures

tiff("CO2_temp", units="in", width=7, height=5, res=300)

CO2_temp <- ggplot(subset(dat_means, t!="dry" & t!="t0"), aes(x=Avg_Temp, y=mean_CO2_C_mg_m2_h)) +
   geom_point(aes(colour=leaf_treatment, shape=temp_C), size=2.5, alpha=0.7) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) +   xlab("Water temperature (\u00B0C)") + stat_smooth(aes(colour=leaf_treatment), geom="line", size=1, alpha=0.7, method = "lm", se=FALSE,  formula = y ~ x)
CO2_temp

dev.off()

min(dat_means$mean_CH4_C_mg_m2_h, na.rm=T)


tiff("CH4_temp", units="in", width=7, height=5, res=300)

min(dat_means$mean_CH4_C_mg_m2_h, na.rm=T)

CH4_temp <- ggplot(subset(dat_means, t!="dry" & t!="t0"), aes(x=Avg_Temp, y=log(mean_CH4_C_mg_m2_h+ 1.0002647657) )) +
  geom_point(aes(colour=leaf_treatment, shape=temp_C), size=2.5, alpha=0.7) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab(expression(log~mg~CH[4]*`-C`~m^-2~h^-1)) +   xlab("Water temperature (\u00B0C)") + stat_smooth(aes(colour=leaf_treatment), geom="line", size=1, alpha=0.7, method = "lm", se=FALSE,  formula = y ~ x)
CH4_temp

dev.off()

#Alternatively: 

CH4_temp <- ggplot(subset(dat_means, t!="dry" & t!="t0" & t!="t72"), aes(x=Avg_Temp, y=mean_CH4_C_mg_m2_h)) +
  geom_point(aes(colour=leaf_treatment, shape=temp_C), size=2.5, alpha=0.7) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1)) +   xlab("Daily average water temperature (\u00B0C)") + stat_smooth(aes(colour=leaf_treatment), geom="line", size=1, alpha=0.7, method = "lm", se=FALSE,  formula = y ~ x)  + coord_trans(y=pseudolog10_trans)
CH4_temp

#### Combine ####

tiff("CO2CH4_vs_temp", units="in", width=5, height=7, res=300)

#Figure 3 in the manuscript

CO2CH4_vs_temp <- ggarrange(CO2_temp + theme(axis.title.x = element_blank() ),                                                                              CH4_temp  ,
                           ncol = 1, nrow = 2, align="hv",common.legend = T,legend="top" )
CO2CH4_vs_temp

dev.off()




############# Statistical analysis ######################################
# Consider removing the dry and t0 values, since they might hide the effect of temperature

#One-way repeated measures ANOVA of the effect of temperature and OM content on GHGs, also check for the interactive effect. 

#for data analysis, exclude any NAs in the GHG column
#dat <- dat %>% drop_na(CO2_C_mg_m2_h)


#Check for outliers

identify_outliers(data.frame(dat$CO2_C_mg_m2_h)) #no extreme outliers
identify_outliers(data.frame(dat$CH4_C_mg_m2_h)) #some extreme outliers

#Check for normality
ggqqplot(dat$CO2_C_mg_m2_h) #relatively normal, but a bit skewed at the extremes
ggqqplot(log(dat$CO2_C_mg_m2_h) ) #Improve after log transformation
ggqqplot(sqrt(dat$CO2_C_mg_m2_h) ) #Improve after square root transformation, but not as good as log
ggqqplot((dat$CO2_C_mg_m2_h)^(1/3) )  #Improved after cube root, better than square root, but not as good as log
ggqqplot(dat, "CO2_C_mg_m2_h", facet.by = "Date")

#For CH4: 
ggplot(dat, aes(x=log(CH4_C_mg_m2_h + 0.9966496) ) ) +
  geom_histogram(binwidth=.5, colour="black", fill="white") #here you can see the extreme left skew, even when log transformed
ggqqplot(dat$CH4_C_mg_m2_h) #not good, 2 "outliers"
min(dat$CH4_C_mg_m2_h, na.rm=T) #-0.003350449
ggqqplot(log(dat$CH4_C_mg_m2_h + 0.9966496)) #better, but still skewed, but this is the best option
ggqqplot(sqrt(dat$CH4_C_mg_m2_h + 0.9966496)) #not good
ggqqplot((dat$CH4_C_mg_m2_h)^(1/3) )  #not good
ggqqplot(subset(dat,CH4_C_mg_m2_h<50), "CH4_C_mg_m2_h", facet.by = "Date") # outliers on 2022-08-10
ggqqplot(dat,"CH4_C_mg_m2_h", facet.by = "Date") 
#try removing the two outliers (they are from the same date and treatment...)
dat_outlier.rm <- subset(dat, CH4_C_mg_m2_h <= 9.97)
ggqqplot(dat_outlier.rm$CH4_C_mg_m2_h) #better than before, but not great
ggqqplot(log(dat_outlier.rm$CH4_C_mg_m2_h + 0.9966496)) #not great
ggqqplot((dat_outlier.rm$CH4_C_mg_m2_h)^(1/3) ) #maybe the best, but still pretty bad
ggqqplot(dat_outlier.rm,"CH4_C_mg_m2_h", facet.by = "Date") 

#But note that it's the residuals of the models that need to be normally distributed, not the data itself


#Summary stats
dat %>%
  group_by(leaf_treatment, temp_C) %>%
  get_summary_stats(CO2_C_mg_m2_h, type = "mean_sd") #replace with CH4 

#Summary stats
sum_dat <- dat %>%
  group_by(t_days, leaf_treatment, temp_C) %>%
  filter(t_days != "3")   %>%
summarize(
mean_CO2_C_mg_m2_h = mean(CO2_C_mg_m2_h), 
mean_CH4_C_mg_m2_h = sd(CH4_C_mg_m2_h) )


##### Calculate the total C loss over the entire experiment #####
#total area of the 36 mesocosms is 0.25524 m2, for the 3 replicates the area is 0.02127
 

sum_dat <- sum_dat %>%
  mutate(t_days = as.character(t_days), 
         hours= case_when(
    startsWith(t_days, "-1") ~ "12",
    startsWith(t_days, "0") ~ "12",
    startsWith(t_days, "1") ~ "24",
    startsWith(t_days, "2") ~ "24", 
    startsWith(t_days, "4") ~ "48",
    startsWith(t_days, "8") ~ "96", 
    startsWith(t_days, "16") ~ "194")) %>%
  mutate(area_m2 = 0.02127) %>%
  mutate(CO2_total_output_mg = mean_CO2_C_mg_m2_h* as.numeric(hours)*area_m2) %>%
  mutate(CH4_total_output_mg = mean_CH4_C_mg_m2_h*as.numeric(hours)*area_m2)

sum(sum_dat$CO2_total_output_mg) #8005.293 mg C loss using averages, 4639.364 using treatment means 
sum(sum_dat$CH4_total_output_mg) #1467.424 mg C loss using averages, 52.4495 using treatment means


#total:  
#1467.424 + 8005.293 = 9472.717 mg C using means
#4639.364 + 52.4495 = 4691.813 mg C using treatment means


write.csv(sum_dat, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/sum_dat.csv")

#Can assume a 50% carbon content of leaf AFDM Gulis, V., & Suberkropp, K. (2003). Freshwater biology

sum_AFDM <- AFDM %>%
  group_by(leaf_treatment, temp_C) %>%
  summarize(
    mean_percent_AFDM_remain = mean(percent_AFDM_remaining), 
    mean_dry_input = mean(dry_input_leaves_g) )  %>%
  mutate(loss = (mean_dry_input*( (100-mean_percent_AFDM_remain) /100) ) ) %>%
  mutate(C_loss_g = loss /2  ) #assume 50% is carbon
         
sum(sum_AFDM$C_loss_g) #9.038162 g or 9038.162 mg

# Therefore 
4691.813/9038.162 #0.5191114 
#just over 50% of the C loss from decomposition was out-gassed. 

#the CO2 fraction

4639.364/9038.162  #0.5133083
52.4495/9038.162   #0.005803116

#for the CO2 equivalent of CH4 is 52.4495 * 28 GWP

52.4495*28 #1468.586


#Co2eq +CO2

1468.586+ 4639.364 #6107.95 is the total loss in CO2 eq

6107.95/9038.162 #0.6757956 the percent loss in CO2 eq

1468.586/9038.162 # 0.1624872 #CH4 loss in CO2eq


# Run the repeated measures ANOVA, make sure to exclude the dry and t0 measurements
CO2_temp <- aov(CO2_C_mg_m2_h ~ temp_C + Error(Column_ID/Date), data = subset(dat, t!="dry" & t!="t0" & t!="t72"))
summary(CO2_temp) #sig

CO2_OM <- aov(CO2_C_mg_m2_h ~ leaf_treatment + Error(Column_ID/Date), data = dat)
summary(CO2_OM) #sig

CH4_temp <- aov(CH4_C_mg_m2_h ~ temp_C + Error(Column_ID/Date), data = subset(dat, t!="dry" & t!="t0" & t!="t72"))
summary(CH4_temp) #p = 0.4

CH4_OM <- aov(CH4_C_mg_m2_h ~ leaf_treatment + Error(Column_ID/Date), data = dat)
summary(CO2_OM) #sig



# Run two-way repeated measures ANOVA to test for individual and interactive effects of temperature and OM content
aov_CO2_temp_OM <- aov(log(CO2_C_mg_m2_h) ~ temp_C * leaf_treatment + Error(Column_ID/(Date)), data = subset(dat, t!="dry" & t!="t0" & t!="t72") )
summary(aov_CO2_temp_OM) #individually significant effects, but non-significant interaction effect
#when we remove dry and t0, the interaction term becomes significant
#When you use the actual ibutton temperatures, the interaction is no longer significant

aov_CH4_temp_OM <- aov(log(CH4_C_mg_m2_h+  1.0005103858) ~ temp_C * leaf_treatment + Error(Column_ID/(Date)), data = subset(dat, t!="dry" & t!="t0" & t!="t72"))
summary(aov_CH4_temp_OM) # temp 0.001, leaf treatment <0.0001, interaction: 0.83
#When we remove dry and t0, the interaction term is still not significant
#When we use the actual iButton temperature, the interaction term is almost significant 0.0569

#Run post hoc test following repeated measures ANOVA #TukeyHSD does not work on repeated measures so can use emmeans

# Run post-hoc testing with emmeans
emmCO2 <- emmeans(aov_CO2_temp_OM, ~ temp_C * leaf_treatment)
pairs(emmCO2) #pairwise comparisons #default p adjustment is "Tukey", 

emmCH4 <- emmeans(aov_CH4_temp_OM, ~ temp_C * leaf_treatment)
pairs(emmCH4) #pairwise comparisons #default p adjustment is "Tukey" 

#### Run linear mixed effect model ####
# 1. I want to test if the actual temperature * OM quantity had an effect 
# 2. I want to test if time had a significant effect on GHGs 
# 3. I want to test the effect of DO on GHGs

#use data excluding dry and t0 (as well as t72 when we only measured DO)
dat_sub <- subset(dat, t!="dry" & t!="t0" & t!="t72")

str(dat_sub) #180 obs of 35 vars

#Make sure Column ID is a factor
dat_sub$Column_ID <- as.factor(dat_sub$Column_ID)

# Identify outliers using Cook's Distance 
cooksD <- cooks.distance(lmer_CO2) #run the model with dat_sub first
influential <- cooksD[(cooksD > (15* mean(cooksD, na.rm = TRUE)))]
influential #288

n <- nrow(dat_sub)
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 28/n, lty = 2, col = "steelblue") # add cutoff line

names_of_influential <- names(influential)
outliers <- dat_sub[names_of_influential,]
CO2_dat_sub_without_outliers <- dat_sub %>% anti_join(outliers)

#Make sure Column ID is a factor
CO2_dat_sub_without_outliers$Column_ID <- as.factor(CO2_dat_sub_without_outliers$Column_ID)

# Re run model excluding 2 outliers
lmer_CO2 <- lmer(log(CO2_C_mg_m2_h) ~ temp_C * leaf_treatment * as.factor(t_days)  + DO_mg_L + (1 | Column_ID), data = CO2_dat_sub_without_outliers)

summary(lmer_CO2) #Temp is significant, so are the OM treatments, date nor any interactions are significant

plot(lmer_CO2) #Looks great after removing two outliers identified by cooks
qqnorm(resid(lmer_CO2))
vif(lmer_CO2)
r.squaredGLMM(lmer_CO2)

#try without interaction, 
lmer_CO2_2 <- lmer(log(CO2_C_mg_m2_h) ~ Avg_Temp + leaf_treatment + Date  + (1 | Column_ID), data = CO2_dat_sub_without_outliers)
summary(lmer_CO2_2) #similar results

#and without random effects
lm_CO2 <- lm(log(CO2_C_mg_m2_h) ~ Avg_Temp * leaf_treatment + Date , data = CO2_dat_sub_without_outliers)
summary(lm_CO2)  #similar results

#Add DO
lmer_CO2_DO <- lmer(log(CO2_C_mg_m2_h) ~ Avg_Temp * leaf_treatment + Date  + DO_mg_L + (1 | Column_ID), data = dat_sub)
summary(lmer_CO2_DO)  #using the data with the 2 outliers removed, makes the random effect variance 0, and messes with the conditional and marginal r2, so stick with all the data

r.squaredGLMM(lmer_CO2_DO)

AIC(lmer_CO2) #92.71339
AIC(lmer_CO2_DO)  #23.94926 So better to include DO 

#Check assumptions:
plot(lmer_CO2_DO) #log transforming improves residuals, but there is potential an outlier
qqnorm(resid(lmer_CO2_DO))



#Run dredge
options(na.action = "na.fail") # Required for dredge to run

lmer_CO2_dredge<- dredge(lmer_CO2, trace = 2, extra = list(
  "R^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  }))

options(na.action = "na.omit") # set back to default

nrow(lmer_CO2_dredge)  #how many models were run 19
head(lmer_CO2_dredge)


top.lmer_CO2_dredge  <- get.models(lmer_CO2_dredge , subset=delta <= 2) #The top models, with delta AIC <2

# only one top model: log(CO2_C_mg_m2_h) ~ leaf_treatment + temp_C + (1 | Column_ID)

lmer_CO2_dredge <- lmer(log(CO2_C_mg_m2_h) ~ temp_C  * leaf_treatment  + (1 | Column_ID), data = CO2_dat_sub_without_outliers)

summary(lmer_CO2_dredge) # 30C is significant, all the leaf treatments are significant
r.squaredGLMM(lmer_CO2_dredge) # m 0.91



##################################################################################

#Now for CH4 
# Identify outliers using Cook's Distance 
cooksD <- cooks.distance(lmer_CH4) #run the model first just with dat_sun
influential <- cooksD[(cooksD > (15* mean(cooksD, na.rm = TRUE)))]
influential #278 282

n <- nrow(dat_sub)
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 50/n, lty = 2, col = "steelblue") # add cutoff line

names_of_influential <- names(influential)
outliers <- dat_sub[names_of_influential,]
CH4_dat_sub_without_outliers <- dat_sub %>% anti_join(outliers)


min(dat_sub$CH4_C_mg_m2_h) #0.0005103858
min(CH4_dat_sub_without_outliers$CH4_C_mg_m2_h) #0.0005103858

lmer_CH4 <- lmer(log(CH4_C_mg_m2_h+ 1.0005103858) ~ temp_C * leaf_treatment * as.factor(t_days) + DO_mg_L + (1 | Column_ID), data = CH4_dat_sub_without_outliers)
summary(lmer_CH4)  #OM medium and high are sig, Date is sig, OM med/high and temp interactions are significant
#why suddenly is only date significant.... try with + 0.9966496 or +4.407595
#so bizarre that OM high has a negative estimate.... Using temperature as a factor seems to solve this....! and adding date (t_days) as a factor

plot(lmer_CH4) #Looks better when the 3 outliers removed, but has a weird pattern to it...
qqnorm(resid(lmer_CH4)) #better, a bit of an S curve
vif(lmer_CH4)
r.squaredGLMM(lmer_CH4)


#try without interaction
lmer_CH4_2 <- lmer(log(CH4_C_mg_m2_h+  1.0005103858) ~ Avg_Temp * leaf_treatment  + (1 | Column_ID), data = CH4_dat_sub_without_outliers)
summary(lmer_CH4_2)

#try without random effect structure
lm_CH4 <- lm(log(CH4_C_mg_m2_h+  1.0005103858) ~ Avg_Temp * leaf_treatment, data = CH4_dat_sub_without_outliers)
summary(lm_CH4)


lmer_DO_CH4 <- lmer(log(CH4_C_mg_m2_h+ 0.9966496) ~ Avg_Temp * leaf_treatment + Date + DO_mg_L + (1 | Column_ID), data = dat_sub)
summary(lmer_DO_CH4)  #compare model performance when including DO. 

AIC(lmer_CH4) #330.5843
AIC(lmer_DO_CH4)  #330.5843  

#The AIC is the same therefore,  there is no strong evidence to support the inclusion of DO_mg_L as a fixed effect in the model.
 
plot(lmer_CH4) #log transforming improves residuals, but a bit fan shaped... 
qqnorm(resid(lmer_CH4))




#Run dredge
options(na.action = "na.fail") # Required for dredge to run

lmer_CH4_dredge<- dredge(lmer_CH4, trace = 2, extra = list(
  "R^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  }))

options(na.action = "na.omit") # set back to default

nrow(lmer_CH4_dredge)  #how many models were run 19
head(lmer_CH4_dredge)


top.lmer_CH4_dredge <- get.models(lmer_CH4_dredge, subset=delta <= 2) #The top models, with delta AIC <2

# only one top model:  log(CH4_C_mg_m2_h + 1.0005103858) ~ as.factor(t_days) + leaf_treatment +  temp_C + as.factor(t_days):leaf_treatment +  as.factor(t_days):temp_C + leaf_treatment:temp_C + as.factor(t_days):leaf_treatment:temp_C + (1 | Column_ID) 

#can simplify to just the 3 way interaction

lmer_CH4_dredge <- lmer(log(CH4_C_mg_m2_h + 1.0005103858) ~ as.factor(t_days)*leaf_treatment*temp_C + (1 | Column_ID) , data = CH4_dat_sub_without_outliers)

summary(lmer_CH4_dredge) # T16 med and high sig, T16 low*30 med*30 high*30, t8 med*30 high 30 significant
r.squaredGLMM(lmer_CH4_dredge) # m 0.94

#### ANOVA / LMM for AFDM ####
# Test for the individual and interactive effects of temperature and OM on AFDM remaining (%)

AFDM <- subset(dat, t=="dry") #remove NAs for the control
AFDM <- AFDM %>% drop_na(percent_AFDM_remaining)
ggqqplot(AFDM$percent_AFDM_remaining) #Normal

#for AFDM use two way ANOVA with interaction, need to account for the replicates with the error term
aov_AFDM_temp_OM <- aov(percent_AFDM_remaining ~ temp_C * leaf_treatment + Error(Column_ID), data = AFDM)
summary(aov_AFDM_temp_OM) # no sig.



#try using group means
AFDM_means <- AFDM %>% group_by(leaf_treatment, temp_C) %>%
  dplyr::summarise(percent_AFDM_remaining = mean(percent_AFDM_remaining, na.rm=TRUE) )

aov_AFDM_temp_OM_means <- aov(percent_AFDM_remaining ~ temp_C * leaf_treatment, data = AFDM_means)
summary(aov_AFDM_temp_OM_means)  

#Run post hoc test following repeated measures ANOVA #TukeyHSD does not work on repeated measures so can use emmeans

TukeyHSD(aov_AFDM_temp_OM) #no significant pairwise comparisons #doesn't work if you include error term

emm_AFDM <- emmeans(aov_AFDM_temp_OM, ~ temp_C * leaf_treatment)
pairs(emm_AFDM) #pairwise comparisons #default p adjustment is "Tukey", 

#try using an LMM

lmer_AFDM <- lmer(percent_AFDM_remaining ~ Avg_Temp * leaf_treatment + (1 | leaf_treatment), data = AFDM)
summary(lmer_AFDM)
plot(lmer_AFDM) 
qqnorm(resid(lmer_AFDM))
r.squaredGLMM(lmer_AFDM)


###### Run model for DO ######
#treatment and time effect on DO #note: need to use the data set with DO on day 3!

lmer_DO <- lmer(log(DO_mg_L) ~ temp_C * leaf_treatment + as.factor(t_days) + (1 | Column_ID), data = dat)
summary(lmer_DO)

r.squaredGLMM(lmer_DO)

plot(lmer_DO) 
qqnorm(resid(lmer_DO)) #improved after log transformation
