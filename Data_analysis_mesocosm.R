###########################################
## Script for analyzing CO2 and CH4 flux data from mesocosm experiment examining temperature and ## OM quantity effects on GHG fluxes from simulated isolated pools 
##################################################################
##  Load necessary packages  ##
library(data.table)
library(ggplot2)
library(tidyr)
library(scales)
library(dplyr)


#Load in the GHG flux results from Mesocosm_GHGR.R, combined with the ancillary data and treatment codes. 

dat <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/CO2.CH4.fluxes.csv")

#Make necessary factors
dat$leaf_treatment <- as.factor(dat$leaf_treatment)
dat$leaf_treatment <- factor(dat$leaf_treatment, levels = c("control", "low", "medium", "high"))
dat$temp_C <- as.factor(dat$temp_C)
dat$t <- as.factor(dat$t)
dat$Water_temp_C <- as.numeric(dat$Water_temp_C)

#format Date
dat$Date <-as.POSIXct(dat$Date, format="%Y-%m-%d", tz = "Europe/Berlin")

#rename
names(dat)[names(dat) == "DO_."] <- "DO_percent"


#Import ibutton data

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

# Plot all of the data
boxplot(CO2_C_mg_m2_h ~ start_time, data= (dat))

boxplot(CH4_C_ug_m2_h ~ start_time, data= (dat)) #two very high CH4 fluxes: 2022-08-11_t16_32, and 2022-08-11_t16_28...  

#Plot by sampling time

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


dat_means$t
  
# Plot time series 
palette1 <- c("#FAFFA1", "#BCD980", "#5D966D", "#3D6160")


time_series_CO2 <- ggplot(dat_means, aes(x=as.numeric(time_num), y=mean_CO2_C_mg_m2_h)) +
  geom_line(aes(linetype=temp_C,  colour=leaf_treatment, group = interaction(temp_C, leaf_treatment) ) )+
  geom_point(aes(shape=temp_C, colour=leaf_treatment), size=2.5, alpha=0.7) +
  scale_color_manual(values = palette1) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) +
  scale_x_continuous(breaks = c(-20,0,24,48,72, 96, 192, 384), labels = c("dry", "0", "24", "48", "72", "96", "192", "384"))+ xlab("Time since wetting (hours)") + ylab(expression(mg~CO[2]*`-C`~m^-2*~h^-1)) 

time_series_CO2




time_series_CO2 <- ggplot(subset(dat_means, leaf_treatment!="control"), aes(x=Date, y=mean_CH4_C_ug_m2_h)) +
  geom_line(aes(linetype=leaf_treatment) )+
  geom_point(aes(shape=temp_C, colour=temp_C)) 
time_series_CO2

