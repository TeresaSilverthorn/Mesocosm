####### Script to calculate the leaf litter mass loss and decomposition rate in simulated isolated pools ######################
##################################################################
##  Load necessary packages  ##
library(ggplot2)
library(tidyr)
library(dplyr)

#Set wd for figures
setwd("C:/Users/teres/Documents/Mesocosm experiment/Mesocosm/Figures")

#Load in the leaf litter weights, combine with the ancillary data and treatment codes. 
ancil_dat <- read.csv ("C:/Users/teres/Documents/Mesocosm experiment/Data/Treatment_codes.csv")

# Calculate ash free dry mass remaining AFDM
#First calculate the % of organic matter in the subsamples: 
# %OM = (sample dry mass - sample ash dry mass ) / sample dry mass * 100
ancil_dat$OM_percent_leaves <-( ((ancil_dat$ceramic_pot.dry_leaves_g - ancil_dat$ceramic_pot_g_leaves) - (ancil_dat$ceramic_pot.burned_leaves_g - ancil_dat$ceramic_pot_g_leaves)) /  (ancil_dat$ceramic_pot.dry_leaves_g - ancil_dat$ceramic_pot_g_leaves) ) *100


ancil_dat$AFDM <- ancil_dat$dry_output_leaves_g * (ancil_dat$OM_percent_leaves/100)

#ancil_dat$percent_AFDM_remaining <- (ancil_dat$AFDM/ancil_dat$dry_input_leaves_g) *100


#### Calculate the decomposition rate (k) ####
#Decomposition rate (k) is the proportion of litter mass loss (LML) per degree day (dd), to account for differences in temperature across sites 

#LML = [initial AFDM (g) – final AFDM (g)]/initial AFDM (g), where initial AFDM was previously corrected by leaching, drying and ash content, which were estimated in the laboratory per Boyero et al 2021
#this is really a percent of the LML

ancil_dat$LML <- ( (ancil_dat$dry_input_leaves_g * 0.9453203) - ancil_dat$AFDM) / ancil_dat$dry_input_leaves_g

#The average OM content of the initial leaves is 94.53203 % (from Tarentaine experiment)

#Divide by degree days to correct for the effect of temperature (actual mesocosm temperature)

Tave <- read.csv("C:/Users/teres/Documents/Mesocosm experiment/Data/Daily_ave_ibutton_temps_mesocosm.csv")

Tave$Date <-as.POSIXct(Tave$Date, format="%Y-%m-%d", tz = "Europe/Berlin")

#SUbset Tave by date in: 2022-07-26 and Date out: 2022-08-11

Tave <- subset(Tave, Date >= as.POSIXct("2022-07-26", tz = "Europe/Berlin") )
Tave <- subset(Tave, Date <= as.POSIXct("2022-08-11", tz = "Europe/Berlin") )
  head(Tave)
# For loop to calculate degree days for each mesocosm

# Create an empty vector to store degree days for each mesocosm
ancil_dat$degreedays <- NA

# Loop through each mesocosm
for (mesocosm_id in 1:36) {
  # Subset Tave dataframe for the specific mesocosm
  mesocosm_temp <- Tave[Tave$Column_ID == mesocosm_id, "Avg_Temp"]
  
  # Calculate degree days for the mesocosm
  degree_days <- sum(ifelse(mesocosm_temp > 0, mesocosm_temp, 0))
  
  # Assign degree days to the corresponding rows in ancil_dat
  ancil_dat$degreedays[ancil_dat$Column_ID == mesocosm_id] <- degree_days
}


#Check
sum(Tave$Avg_Temp[Tave$Column_ID == "36"]) #445.5417 OK

#Calculate decomposition rate corrected by degree days 
#k = -ln (M final / M initial ) / degree days

ancil_dat$k_dday <- -log( ancil_dat$AFDM / ancil_dat$dry_input_leaves_g * 0.9453203) / ancil_dat$degreedays


ancil_dat$k_day <- -log( ancil_dat$AFDM / ancil_dat$dry_input_leaves_g * 0.9453203) / 16  #Initial weight multiplied by OM content, final AFDM is already multiplied by OM content # 16 days is incubation duration, not accounting for degree days



#plot

ancil_dat$temp_C <- as.factor(ancil_dat$temp_C)
ancil_dat$leaf_treatment <- factor(ancil_dat$leaf_treatment, levels = c("low", "medium", "high"))

decomp_dday_box <-  ggplot(subset(ancil_dat, leaf_treatment!= "control"), aes(x=leaf_treatment, y=k_dday, fill=temp_C)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +   ylab("k / degree day") +
  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line(), axis.title.x = element_blank())
decomp_dday_box 

decomp_day <-  ggplot(subset(ancil_dat, leaf_treatment!= "control"), aes(x=leaf_treatment, y=k_day, fill=temp_C)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +   ylab("k / day") +
  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line(), axis.title.x = element_blank())
decomp_day 

#showing points with SE:

decomp_dday <-  ggplot(subset(ancil_dat, leaf_treatment!= "control"), aes(x=leaf_treatment, y=k_dday, group=temp_C)) +  stat_summary(fun.data = "mean_se", geom = "errorbar",position = position_dodge(width = 0.3), aes(group= temp_C), colour="black" , width = 0.2) +  stat_summary(fun.data = "mean_se", geom = "point",position = position_dodge(width = 0.3), aes(shape = temp_C), size = 3, alpha = 0.7) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab("k / degree day")
decomp_dday # make the k italic # use different point shapes than the previous plots as not to confuse temperature and leaf litter treatments

decomp_day <-  ggplot(subset(ancil_dat, leaf_treatment!= "control"), aes(x=leaf_treatment, y=k_day, group=temp_C)) +  stat_summary(fun.data = "mean_se", geom = "errorbar",position = position_dodge(width = 0.3), aes(group= temp_C), colour="black" , width = 0.2) +  stat_summary(fun.data = "mean_se", geom = "point",position = position_dodge(width = 0.3), aes(shape = temp_C), size = 3, alpha = 0.7) + theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.title.x = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line() ) + ylab("k / day")
decomp_day


#Combine 
tiff("decomposition", units="in", width=5, height=5, res=300)

decomp <- ggarrange(
  decomp_day ,
  decomp_dday , 
  ncol = 1, nrow = 2, align="hv",common.legend = T,legend="top")
decomp

dev.off()


#subset just the k column and column_ID columns 
leaf_mass_loss <- ancil_dat[, c("k_dday", "k_day", "Column_ID", "AFDM", "LML")]

str(leaf_mass_loss) #36 obs. of  3 variables

#save

write.csv(leaf_mass_loss, "C:/Users/teres/Documents/Mesocosm experiment/Data/leaf_mass_loss.csv")

################
#### Statistical analysis ######


#### Lmer or anova for k ? 

dat_k <- subset(dat, t=="t16") # choose just one time point (arbitrary)
#remove NAs
dat_k <- subset(dat_k, complete.cases(k_day))

ggqqplot(dat_k$k_dday) #Normal

#for AFDM use two way ANOVA with interaction
aov_k_dday <- aov(k_dday ~ temp_C * leaf_treatment, data = dat_k)
summary(aov_k_dday) # No sig
plot(aov_k_dday, 2) #residuals are normal
plot(aov_k_dday, 3) #homogeneity of variances is a bit iffy

aov_k_day <- aov(k_day ~ temp_C * leaf_treatment, data = dat_k)
summary(aov_k_day) # temp 0.0527
plot(aov_k_day, 2) #residuals are normal
plot(aov_k_day, 3) #homogeneity of variances is alright

k_summary <- dat_k %>%
  dplyr::group_by(temp_C)  %>%
   dplyr::summarise(
     k_day = mean(k_day, na.rm=T), 
     k_dday = mean(k_dday, na.rm=T))
