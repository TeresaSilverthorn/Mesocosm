##### version 2 incorporating comments from reviewers of JGR Biogeosciences ################

## load in necessary packages
library(ggplot2)
library(ggpubr)
library(dplyr)
library(purrr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(car)
#Note that the anova for decomposition is in Mesocosm_leaf_litter_mass_loss.R

#### Set wd for figures ####
setwd("C:/Users/teres/Documents/Mesocosm experiment/Mesocosm/Figures")

## Read  in the dat file from the Data_analysis_mesocosm.R file which includes data cleaning and data interpolation for temperature probles that were found out of the water 

dat <- read.csv("C:/Users/teres/Documents/Mesocosm experiment/Mesocosm/dat.csv")


str(dat) # 288 obs of 38 vars

#Make necessary factors
dat$leaf_treatment <- as.factor(dat$leaf_treatment) #make leaf treatment a factor
dat$leaf_treatment <- factor(dat$leaf_treatment, levels = c("control", "low", "medium", "high")) #logically reorder
dat$temp_C <- as.factor(dat$temp_C) #make the temperature treatment a factor
dat$t <- as.factor(dat$t)           #make time a factor

#drop the old leaf_presence column
dat <- dat %>%
  dplyr::select(-leaf_presence)

#format Date
dat$Date <-as.POSIXct(dat$Date, format="%Y-%m-%d", tz = "Europe/Berlin")

# Add a column for the CO2 to CH4 ratio

dat$CO2CH4ratio <- dat$CO2_C_mg_m2_h/dat$CH4_C_mg_m2_h

#load in treatment means
dat_means <- read.csv("C:/Users/teres/Documents/Mesocosm experiment/Mesocosm/dat_means.csv")


##### Plot CO2 : CH4 

ratio <- ggplot(subset(dat, t!="dry" & t!="t0"  & t!="t24" & t!="t72"), aes(x=t_days, y=CO2CH4ratio)) +
  geom_point(aes(shape=as.factor(leaf_treatment)), size=2.5, alpha=0.7) +  scale_shape_manual(values = c("control" = 19, "low" = 5, "medium" = 15, "high" = 4)) +
  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + scale_y_log10()
ratio

##### Figure 3 : GHG vs temperature ####


tiff("CO2_temp", units="in", width=7, height=5, res=300)

CO2_temp <- ggplot(subset(dat, t!="dry" & t!="t0"  & t!="t24"), aes(x=Avg_Temp, y=CO2_C_mg_m2_h)) +
  geom_point(aes(shape=as.factor(leaf_treatment)), size=2.5, alpha=0.7) +  scale_shape_manual(values = c("control" = 19, "low" = 5, "medium" = 15, "high" = 4)) +
  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) +   xlab("Water temperature (\u00B0C)") + stat_smooth(aes(linetype=leaf_treatment), geom="line", size=1, alpha=0.7, method = "lm", se=FALSE,  formula = y ~ x)
CO2_temp # note the reviewers wanted to see all of the data here, no sequential colours, and describe what the lines are in the caption

dev.off()

CO2control <- lm(CO2_C_mg_m2_h~Avg_Temp, dat=subset(dat, t!="dry" & t!="t0"  & t!="t24" & leaf_treatment=="control"))
summary(CO2control) #p = 0.0005

CO2low <- lm(CO2_C_mg_m2_h~Avg_Temp, dat=subset(dat, t!="dry" & t!="t0"  & t!="t24" & leaf_treatment=="low"))
summary(CO2low) #p < 0.0001

CO2medium <- lm(CO2_C_mg_m2_h~Avg_Temp, dat=subset(dat, t!="dry" & t!="t0"  & t!="t24" & leaf_treatment=="medium"))
summary(CO2medium) # p < 0.0001

CO2high <- lm(CO2_C_mg_m2_h~Avg_Temp, dat=subset(dat, t!="dry" & t!="t0"  & t!="t24" & leaf_treatment=="high"))
summary(CO2high) # p = 0.0002


tiff("CH4_temp", units="in", width=7, height=5, res=300)

CH4_temp <- ggplot(subset(dat, t!="dry" & t!="t0"  & t!="t24"), aes(x=Avg_Temp, y=CH4_C_mg_m2_h )) +
  geom_point(aes(shape=as.factor(leaf_treatment)), size=2.5, alpha=0.7) +  scale_shape_manual(values = c("control" = 19, "low" = 5, "medium" = 15, "high" = 4)) +
  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1))  +   xlab("Water temperature (\u00B0C)") + scale_y_log10() + stat_smooth(data=subset(dat, leaf_treatment =="control"), aes(linetype=leaf_treatment), geom="line", size=1, alpha=0.7, method = "lm", se=FALSE,  formula = y ~ x) 
CH4_temp

CH4control <- lm(CH4_C_mg_m2_h~Avg_Temp, dat=subset(dat, t!="dry" & t!="t0"  & t!="t24" & leaf_treatment=="control"))
summary(CH4control) #p =0.04

CH4low <- lm(CH4_C_mg_m2_h~Avg_Temp, dat=subset(dat, t!="dry" & t!="t0"  & t!="t24" & leaf_treatment=="low"))
summary(CH4low) #NS

CH4medium <- lm(CH4_C_mg_m2_h~Avg_Temp, dat=subset(dat, t!="dry" & t!="t0"  & t!="t24" & leaf_treatment=="medium"))
summary(CH4medium) #NS

CH4high <- lm(CH4_C_mg_m2_h~Avg_Temp, dat=subset(dat, t!="dry" & t!="t0"  & t!="t24" & leaf_treatment=="high"))
summary(CH4high) #NS


#there are different ways of log transforming in ggplot: you can transform the data, you can transform the scale(before statistics), and you can transform the coordinate system (after statistics, changes shape of geoms)


dev.off()

##### Figure 3: combine CO2 and CH4 v temperature 


tiff("CO2CH4_vs_temp", units="in", width=6, height=7.5, res=300)

CO2CH4_vs_temp <- ggarrange(CO2_temp + theme(axis.title.x = element_blank() ), CH4_temp  ,
                 ncol = 1, nrow = 2, align="hv",common.legend = T,legend="right", labels = c("(a)", "(b)") )
CO2CH4_vs_temp

dev.off()


################################################

#### Figure 4. time series of CO2 and CH4 by treatments ####


time_series_CO2_20 <- ggplot(subset(dat, temp_C=="20" & t!="dry" & t!="t0"  & t!="t24"), aes(x=as.numeric(t_days), y=CO2_C_mg_m2_h)) +
   stat_summary(fun.y = mean, geom = "line", aes(group = leaf_treatment), size = .7, alpha= 0.3) +    stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group= leaf_treatment), colour="black" , width = 0.2) +   stat_summary(fun.data = "mean_se", geom = "point", aes(shape = leaf_treatment), size = 3, alpha = 0.5) +
  scale_shape_manual(values = c("control" = 19, "low" = 5, "medium" = 15, "high" = 4)) +
   theme_bw() + theme(plot.margin = margin(t = 0.0,  r = 0.01,  b = 0.0,  l = 0.01), axis.title = element_text(), panel.grid.major = element_blank(), legend.position="none",  panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + 
  scale_x_continuous(breaks = c(2,4, 8, 16), labels = c( "2", "4", "8", "16"))+ xlab("Incubation time (days)") + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) +  ggtitle("(a) 20 \u00B0C ") + ylim(0, 280) 
time_series_CO2_20

time_series_CO2_25 <- ggplot(subset(dat, temp_C=="25" & t!="dry" & t!="t0"  & t!="t24"), aes(x=as.numeric(t_days), y=CO2_C_mg_m2_h)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = leaf_treatment), size = .7, alpha= 0.3) +    stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group= leaf_treatment), colour="black" , width = 0.2) +   stat_summary(fun.data = "mean_se", geom = "point", aes(shape = leaf_treatment), size = 3, alpha = 0.5) +
  scale_shape_manual(values = c("control" = 19, "low" = 5, "medium" = 15, "high" = 4)) +
  theme_bw() + theme(plot.margin = margin(t = 0.0,  r = 0.01,  b = 0.0,  l = 0.01), axis.title = element_text(), panel.grid.major = element_blank(), legend.position="none",  panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(2,4, 8, 16), labels = c( "2", "4", "8", "16"))+ xlab("Incubation time (days)") + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) +  ggtitle("(c) 25 \u00B0C ") + ylim(0, 280) 
time_series_CO2_25

time_series_CO2_30 <- ggplot(subset(dat, temp_C=="30" & t!="dry" & t!="t0"  & t!="t24"), aes(x=as.numeric(t_days), y=CO2_C_mg_m2_h)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = leaf_treatment), size = .7, alpha= 0.3) +    stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group= leaf_treatment), colour="black" , width = 0.2) +   stat_summary(fun.data = "mean_se", geom = "point", aes(shape = leaf_treatment), size = 3, alpha = 0.5) +
  scale_shape_manual(values = c("control" = 19, "low" = 5, "medium" = 15, "high" = 4)) +
  theme_bw() + theme(plot.margin = margin(t = 0.0,  r = 0.01,  b = 0.0,  l = 0.01), axis.title = element_text(), panel.grid.major = element_blank(), legend.position="none",  panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(2,4, 8, 16), labels = c( "2", "4", "8", "16"))+ xlab("Incubation time (days)") + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) +  ggtitle("(e) 30 \u00B0C ") + ylim(0, 280) 
time_series_CO2_30

full_time_series_CH4_20 <- ggplot(subset(dat, temp_C=="20" & t!="dry" & t!="t0"  & t!="t24"), aes(x=t_days, y=CH4_C_mg_m2_h)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = leaf_treatment), size = .7, alpha= 0.3) +    stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group= leaf_treatment), colour="black" , width = 0.2) +   stat_summary(fun.data = "mean_se", geom = "point", aes(shape = leaf_treatment), size = 3, alpha = 0.5) +
  scale_shape_manual(values = c("control" = 19, "low" = 5, "medium" = 15, "high" = 4)) +
  theme_bw() + theme(legend.position = "none", axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + 
  scale_y_continuous(trans = "log10", breaks = c(0.01, 0.1, 1.0, 10, 100), labels = c("0.01","0.1", "1.0", "10", "100"), limits = c(-0.01, 100)) +
  xlab("Incubation time (days)") +
  scale_x_continuous(breaks = c(2, 4, 8, 16), labels = c("2", "4", "8", "16"))+
  ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1)) + 
 ggtitle("(b) 20 \u00B0C ") 
full_time_series_CH4_20

full_time_series_CH4_25 <- ggplot(subset(dat, temp_C=="25" & t!="dry" & t!="t0"  & t!="t24"), aes(x=t_days, y=CH4_C_mg_m2_h)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = leaf_treatment), size = .7, alpha= 0.3) +    stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group= leaf_treatment), colour="black" , width = 0.2) +   stat_summary(fun.data = "mean_se", geom = "point", aes(shape = leaf_treatment), size = 3, alpha = 0.5) +
  scale_shape_manual(values = c("control" = 19, "low" = 5, "medium" = 15, "high" = 4)) +
  theme_bw() + theme(legend.position = "none", axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),  panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +  scale_y_continuous(trans = "log10", breaks = c(0.01, 0.1, 1.0, 10, 100), labels = c("0.01","0.1", "1.0", "10", "100"), limits = c(-0.01, 100)) +  xlab("Incubation time (days)") +
  scale_x_continuous(breaks = c(2, 4, 8, 16), labels = c("2", "4", "8", "16"))+
  ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1)) + 
  ggtitle("(d) 25 \u00B0C ") 
full_time_series_CH4_25

full_time_series_CH4_30 <- ggplot(subset(dat, temp_C=="30" & t!="dry" & t!="t0"  & t!="t24"), aes(x=t_days, y=CH4_C_mg_m2_h)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = leaf_treatment), size = .7, alpha= 0.3) +    stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group= leaf_treatment), colour="black" , width = 0.2) +   stat_summary(fun.data = "mean_se", geom = "point", aes(shape = leaf_treatment), size = 3, alpha = 0.5) +
  scale_shape_manual(values = c("control" = 19, "low" = 5, "medium" = 15, "high" = 4)) +
  theme_bw() + theme(legend.position = "none", axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(2, 4, 8, 16), labels = c("2", "4", "8", "16")) +  
  scale_y_continuous(trans = "log10", breaks = c(0.01, 0.1, 1.0, 10, 100), labels = c("0.01","0.1", "1.0", "10", "100")) + 
  xlab("Incubation time (days)") + 
  ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1)) + 
  ggtitle("(f) 30 \u00B0C ") 
full_time_series_CH4_30

#Need to harmonize the y axes here, unfortunately ylim doesnt work with scale_y_log10()


# Combine

tiff("CO2CH4_timeseries", units="in", width=8, height=9.5, res=300)

CO2CH4_timeseries <- ggarrange(
 time_series_CO2_20 + theme(axis.title.x = element_blank() ),
  full_time_series_CH4_20 + theme(axis.title.x = element_blank() ), 
   time_series_CO2_25 + theme(axis.title.x = element_blank() ),
    full_time_series_CH4_25 + theme(axis.title.x = element_blank()),  
     time_series_CO2_30,
      full_time_series_CH4_30, 
                         ncol = 2, nrow = 3, align="hv",common.legend = T,legend="top")
CO2CH4_timeseries

dev.off()

#################################################
#### Figure 5: decomposition rate ####

#showing points with SE:

decomp_dday <-  ggplot(subset(dat, leaf_treatment!= "control"), aes(x=leaf_treatment, y=k_dday, group=temp_C)) +  stat_summary(fun.data = "mean_se", geom = "errorbar",position = position_dodge(width = 0.3), aes(group= temp_C), colour="black" , width = 0.2) +  stat_summary(fun.data = "mean_se", geom = "point",position = position_dodge(width = 0.3), aes(shape = temp_C), size = 3, alpha = 0.5) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab(expression(italic(k) ~ dd^-1))
decomp_dday # make the k italic # use different point shapes than the previous plots as not to confuse temperature and leaf litter treatments

decomp_day <-  ggplot(subset(dat, leaf_treatment!= "control"), aes(x=leaf_treatment, y=k_day, group=temp_C)) +  stat_summary(fun.data = "mean_se", geom = "errorbar",position = position_dodge(width = 0.3), aes(group= temp_C), colour="black" , width = 0.2) +  stat_summary(fun.data = "mean_se", geom = "point",position = position_dodge(width = 0.3), aes(shape = temp_C), size = 3, alpha = 0.5) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +ylab(expression(italic(k) ~ d^-1))

decomp_day


#Combine 
tiff("decomposition", units="in", width=5, height=5, res=300)

decomp <- ggarrange(
  decomp_day ,
  decomp_dday , 
  ncol = 1, nrow = 2, align="hv",common.legend = T,legend="top")
decomp

dev.off()


#########################################################
#### Run linear mixed models ####


#use data excluding dry, t0, t24 (as well as t72 when we only measured DO)
dat_sub <- subset(dat, t!="dry" & t!="t0" & t!="t24" & t!="t72")


str(dat_sub) #180 obs of 35 vars

#Make sure Column ID is a factor
dat_sub$Column_ID <- as.factor(dat_sub$Column_ID)

#### Run lmer for CO2 ####

lmer_CO2 <- lmer(log(CO2_C_mg_m2_h) ~ temp_C * leaf_treatment + (1 | Column_ID), data = dat_sub) 

summary(lmer_CO2) #Temp is significant, so are the OM treatments, date nor any interactions are significant

AIC(lmer_CO2) #without t_days: 87.75228  #AIC with t_days: 162.209, therefore can remove t_days

plot(lmer_CO2) #improved after log transformation
qqnorm(resid(lmer_CO2))
vif(lmer_CO2) # nothing >10, no issues with collinearity
r.squaredGLMM(lmer_CO2)


#### Run lmer for CH4 ####


lmer_CH4 <- lmer(log(CH4_C_mg_m2_h) ~ temp_C * leaf_treatment * as.factor(t_days) + (1 | Column_ID), data = dat_sub)  

summary(lmer_CH4) #

AIC(lmer_CH4) #without t_days: 634.2258 #AIC with t_days: 324.8658, therefore should include t_days

plot(lmer_CH4) #improved after log transformation
qqnorm(resid(lmer_CH4))
vif(lmer_CH4) # nothing >10, no issues with collinearity
r.squaredGLMM(lmer_CH4) # marginal : 0.93

###################################################
#### GHG by treatment summary table S2 ####

mean(dat_sub$CH4_C_mg_m2_h) # total average 
sd(dat_sub$CH4_C_mg_m2_h) # total sd

# summary by treatment
ghg_summary <- dat %>%
  dplyr::group_by(leaf_treatment, temp_C) %>%
  dplyr::filter(t != "dry" & t != "t0" & t != "t24") %>%
  dplyr::summarise(
    mean_CO2_C_mg_m2_h = mean(CO2_C_mg_m2_h, na.rm=T), 
    sd_CO2_C_mg_m2_h = sd(CO2_C_mg_m2_h, na.rm=T), 
    mean_CH4_C_mg_m2_h = mean(CH4_C_mg_m2_h, na.rm=T),
    sd_CH4_C_mg_m2_h = sd(CH4_C_mg_m2_h, na.rm=T))

##############

#### Figure S3 final water temperatures ####

tiff("temp_final", units="in", width=7, height=5, res=300)

palette3 <- c( "#BCD980", "#5D966D", "#3D6160")

ibutton_temp_final <- ggplot(subset(dat, Date !="2022-07-26" & t != "dry" & t != "t0" & t != "t24"), aes(x=as.factor(t_days), y=Avg_Temp)) + 
  geom_point(aes(shape=temp_C, colour=temp_C), size=2, alpha=0.9) + 
  scale_color_manual(values = palette3) +
  geom_line(aes(group = Column_ID, color = as.factor(temp_C)), size = 0.8, alpha=0.5) + 
  theme_bw() + theme(axis.title = element_text(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() , text = element_text(size = 12), axis.text = element_text(size = 12, colour="black")) + ylim(19,33.3) +  ylab("Water temperature (\u00B0C)") + xlab("Incubation time (days)") 
ibutton_temp_final

dev.off()

#this data is corrected for day 16 using the manual measurements, as the ibuttons were out of the water then (see Data_analysis_mesocosm.R for details of the correction)

#### plot DO by treatment ####

DO_bytreatment <- decomp_day <-  ggplot(dat, aes(x=leaf_treatment, y=DO_mg_L, group=temp_C)) +  stat_summary(fun.data = "mean_se", geom = "errorbar",position = position_dodge(width = 0.3), aes(group= temp_C), colour="black" , width = 0.2) +  stat_summary(fun.data = "mean_se", geom = "point",position = position_dodge(width = 0.3), aes(shape = temp_C), size = 3, alpha = 0.7) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab("DO (mg/L)")
DO_bytreatment



#### Run lmer for DO ####

#treatment and time effect on DO #note: need to use the data set with DO on day 3!

lmer_DO <- lmer(log(DO_mg_L) ~ temp_C * leaf_treatment + as.factor(t_days) + (1 | Column_ID), data = dat)
summary(lmer_DO)

r.squaredGLMM(lmer_DO)

plot(lmer_DO) 
qqnorm(resid(lmer_DO)) #improved after log transformation

#### Plot DO times series Figure S4 ####

tiff("DO_timeseries", units="in", width=7, height=5, res=300)

time_series_DO <- ggplot(dat, aes(x=as.numeric(t_days), y=DO_mg_L)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = leaf_treatment), size = .7, alpha= 0.3) +    stat_summary(fun.data = "mean_se", geom = "errorbar", aes(group= leaf_treatment), colour="black" , width = 0.2) +   stat_summary(fun.data = "mean_se", geom = "point", aes(shape = leaf_treatment), size = 3, alpha = 0.5) +
  scale_shape_manual(values = c("control" = 19, "low" = 5, "medium" = 15, "high" = 4)) +
  theme_bw() + theme(plot.margin = margin(t = 0.0,  r = 0.01,  b = 0.0,  l = 0.01), axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() , text = element_text(size = 12), axis.text = element_text(size = 12, colour="black"))  + ylab(expression("Dissolved oxygen mg L"^-1)) + xlab("Incubation time (days)") + scale_y_log10() 
time_series_DO

dev.off()

##### relationship between GHG and k #####

CO2_k <- ggplot(subset(dat, t!="dry" & t!="t0"  & t!="t24" & leaf_treatment!="control"), aes(x=k_day, y=CO2_C_mg_m2_h)) +    geom_point(aes(shape=as.factor(leaf_treatment)), size=2.5, alpha=0.7) +  scale_shape_manual(values = c("low" = 5, "medium" = 15, "high" = 4))  + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1))   + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) # stat_poly_line(aes(group=leaf_treatment, linetype=leaf_treatment), colour="grey", se=F) +
CO2_k


CH4_k <- ggplot(subset(dat, t!="dry" & t!="t0"  & t!="t24" & leaf_treatment!="control"), aes(x=k_day, y=CH4_C_mg_m2_h)) +    geom_point(aes(shape=as.factor(leaf_treatment)), size=2.5, alpha=0.7) +  scale_shape_manual(values = c("low" = 5, "medium" = 15, "high" = 4))    + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +  scale_y_log10() +   ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1)) 
CH4_k


# Use pipes to calculate correlation coefficients and p-values
correlation_data <- data.frame(
  leaf_treatment = c("low", "medium", "high"))


correlation_data <- correlation_data %>%
  mutate(
    correlation_coefficient = map_dbl(leaf_treatment, ~ {
      subset_data <- filter(dat, leaf_treatment == .x)
      cor.test(subset_data$CO2_C_mg_m2_h, subset_data$k_day)$estimate
    }),
    p_value = map_dbl(leaf_treatment, ~ {
      subset_data <- filter(dat, leaf_treatment == .x)
      cor.test(subset_data$CO2_C_mg_m2_h, subset_data$k_day)$p.value
    })
  )

# Print the correlation data
print(correlation_data) # low is significant

#now for CH4

correlation_data2 <- data.frame(
  leaf_treatment = c("low", "medium", "high"))


correlation_data2 <- correlation_data2 %>%
  mutate(
    correlation_coefficient = map_dbl(leaf_treatment, ~ {
      subset_data <- filter(dat, leaf_treatment == .x)
      cor.test(subset_data$CH4_C_mg_m2_h, subset_data$k_day)$estimate
    }),
    p_value = map_dbl(leaf_treatment, ~ {
      subset_data <- filter(dat, leaf_treatment == .x)
      cor.test(subset_data$CH4_C_mg_m2_h, subset_data$k_day)$p.value
    })
  )

# Print the correlation data
print(correlation_data2) # no relationships are significant




# linear models give the same p value:

lmCO2_k_low<- lm(CO2_C_mg_m2_h ~ k_day, data=subset(dat, leaf_treatment=="low") )
summary(lmCO2_k_low) # p = 0.007

lmCO2_k_med<- lm(CO2_C_mg_m2_h ~ k_day, data=subset(dat, leaf_treatment=="medium") )
summary(lmCO2_k_med) # p = 0.96


lmCO2_k_high <- lm(CO2_C_mg_m2_h ~ k_day, data=subset(dat, leaf_treatment=="high") )
summary(lmCO2_k_high) # p = 0.08


lmCO2_k <- lm(CO2_C_mg_m2_h ~ k_day, data=dat)
summary(lmCO2_k) # p = 0.0522

cor(dat$CO2_C_mg_m2_h, dat$k_day, use = "complete.obs") #0.14
cor.test(dat$CO2_C_mg_m2_h, dat$k_day, use = "complete.obs") #0.0522

########

#### Figure S2 : presence vs absence of leaf litter

#make a new category for control vs leaves

dat <- dat %>%
  mutate(leaf_presence= case_when(
    startsWith(as.character(leaf_treatment), "control") ~ "sediment",
    startsWith(as.character(leaf_treatment), "low") ~ "sediment + LL",
    startsWith(as.character(leaf_treatment), "medium") ~ "sediment + LL",
    startsWith(as.character(leaf_treatment), "high") ~ "sediment + LL"  ))


# summary by date and leaf presence vs absence

dat %>%
  dplyr::group_by(leaf_presence) %>%
  dplyr::filter(t != "dry" & t != "t0" & t != "t24") %>%
  dplyr::summarise(
    mean_CO2_C_mg_m2_h = mean(CO2_C_mg_m2_h, na.rm=T), 
    sd_CO2_C_mg_m2_h = sd(CO2_C_mg_m2_h, na.rm=T), 
    mean_CH4_C_mg_m2_h = mean(CH4_C_mg_m2_h, na.rm=T),
    sd_CH4_C_mg_m2_h = sd(CH4_C_mg_m2_h, na.rm=T))



leaf_presence_CO2 <- ggplot(subset(dat, t != "dry" & t != "t0" & t != "t24" & t!="t72"), aes(as.factor(t_days), CO2_C_mg_m2_h, fill=leaf_presence)  ) + geom_boxplot() +  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() , text = element_text(size = 12), axis.text = element_text(size = 12, colour="black"))+  ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) + xlab("Incubation time (days)") + scale_fill_manual(values = c("#e1e590", "#5D966D")) 
leaf_presence_CO2

leaf_presence_CH4 <- ggplot(subset(dat, t != "dry" & t != "t0" & t != "t24" & t!="t72"), aes(as.factor(t_days), CH4_C_mg_m2_h, fill=leaf_presence)  ) + geom_boxplot() +  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ,  text = element_text(size = 12), axis.text = element_text(size = 12, colour="black")) +   ylab(expression(mg~CH[4]*`-C`~m^-2~h^-1))  + xlab("Incubation time (days)") + scale_fill_manual(values = c("#e1e590", "#5D966D")) + 
scale_y_continuous(trans = "log10", breaks = c(0.01, 1, 100), labels = c("0.01", "1.0", "100"))
leaf_presence_CH4 


#Combine 

tiff("leaf_presence_CO2CH4", units="in", width=6, height=7, res=300)

leaf_presence_CO2CH4 <- ggarrange(leaf_presence_CO2 +theme(axis.title.x = element_blank()),  leaf_presence_CH4 ,
                                  ncol = 1, nrow = 2, align="hv",common.legend = T,legend="top",  labels = c("(a)", "(b)"))
leaf_presence_CO2CH4

dev.off()


##### Figure S1 Plot headspace height / volume ######

tiff("time_series_volume", units="in", width=7, height=5, res=300)

palette1 <- c("#e1e590", "#BCD980", "#5D966D", "#3D6160")

time_series_volume <- ggplot(subset(dat,  t!="dry" &  t != "t0" & t != "t24" & t!="t72"), aes(x=as.numeric(t_days), y=1.8-volume_L)) +
  geom_line(aes(group = interaction(leaf_treatment, temp_C), linetype=temp_C, colour = interaction(leaf_treatment)), stat = "summary", fun = "mean", size = 0.7) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  geom_point(aes(shape=temp_C,  colour=leaf_treatment), size=2.5, alpha=0.7, position=position_dodge(0.05)) +
  scale_fill_manual(values = palette1) + scale_colour_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ,  text = element_text(size = 12), axis.text = element_text(size = 12, colour="black"))  + ylab("Water volume (L)") + xlab("Incubation time (days)") + scale_x_continuous(breaks = c(0, 1, 2, 4, 8, 16))
time_series_volume

dev.off()






##### Calculate the total C loss over the entire experiment #####
#total area of the 36 mesocosms is 0.25524 m2, for the 3 replicates the area is 0.02127

sum_dat <- dat %>%
  dplyr::filter(t != "dry" & t != "t0" & t != "t24" & t != "t72") %>%
  mutate(t_days = as.character(t_days), 
         hours= case_when(
           t_days == "-1" ~ "12",
           t_days == "0" ~ "12",
           t_days == "1" ~ "24",
           t_days == "2" ~ "24", 
           t_days == "4" ~ "48",
           t_days == "8" ~ "96", 
           t_days == "16" ~ "194")) %>%
  mutate(area_m2 = 0.02127) %>%
  mutate(CO2_total_output_mg = CO2_C_mg_m2_h* as.numeric(hours)*area_m2) %>%
  mutate(CH4_total_output_mg = CH4_C_mg_m2_h*as.numeric(hours)*area_m2)   %>%
  group_by(t_days, leaf_treatment, temp_C)  %>%
  summarise(CO2_total_output_mg = mean(CO2_total_output_mg),
          CH4_total_output_mg = mean(CH4_total_output_mg)) %>%
  mutate(CH4_CO2e_total_output_mg = CH4_total_output_mg*27) 

#6th IPCC assessment report, the 100-year GWP of CH4 is 27 (Table 7.15, Forster et al., 2021).   

# What proportion of emissions are from the sediments, and what proportion from the leaves?
#select just the data from the control (no leaves)
control_data <- sum_dat %>%
  filter(leaf_treatment == "control") %>%
  dplyr::select(t_days, temp_C, CO2_total_output_mg, CH4_CO2e_total_output_mg) %>%
  rename(controlCO2 = CO2_total_output_mg, controlCH4_CO2e = CH4_CO2e_total_output_mg)  %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-leaf_treatment)


#join back to the other data, and make a column for the percent contribution of the leaves
sum_dat <- sum_dat %>%
  dplyr::left_join(control_data, by = c("t_days", "temp_C"))

sum_dat <- sum_dat %>%
  mutate(percent_CO2_leaves = (1-controlCO2/CO2_total_output_mg) *100)%>%
  mutate(percent_CO2_sediments = controlCO2/CO2_total_output_mg*100) %>%
  mutate(percent_CH4_CO2e_leaves = (1-controlCH4_CO2e/CH4_CO2e_total_output_mg) *100)%>%
  mutate(percent_CH4_CO2e_sediments = controlCH4_CO2e/CH4_CO2e_total_output_mg*100)

