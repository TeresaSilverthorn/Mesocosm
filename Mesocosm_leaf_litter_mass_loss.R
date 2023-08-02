####### Script to calculate the leaf litter mass loss in simulated isolated pools ######################
##################################################################
##  Load necessary packages  ##
library(ggplot2)
library(tidyr)
library(dplyr)

#Set wd for figures
setwd("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Figures")

#Load in the leaf litter weights, combine with the ancillary data and treatment codes. 
ancil_dat <- read.csv ("C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/Data/Treatment_codes.csv")

# Calculate ash free dry mass remaining AFDM
#First calculate the % of organic matter in the subsamples: 
# %OM = (sample dry mass - sample ash dry mass ) / sample dry mass * 100
ancil_dat$OM_percent_leaves <-( ((ancil_dat$ceramic_pot.dry_leaves_g - ancil_dat$ceramic_pot_g_leaves) - (ancil_dat$ceramic_pot.burned_leaves_g - ancil_dat$ceramic_pot_g_leaves)) /  (ancil_dat$ceramic_pot.dry_leaves_g - ancil_dat$ceramic_pot_g_leaves) ) *100


ancil_dat$AFDM <- ancil_dat$dry_output_leaves_g * (ancil_dat$OM_percent_leaves/100)

ancil_dat$percent_AFDM_remaining <- (ancil_dat$AFDM/ancil_dat$dry_input_leaves_g) *100

#plot
palette2 <- c("#FFE085", "#F27E29", "#E53C22")
ancil_dat$temp_C <- as.factor(ancil_dat$temp_C)
ancil_dat$leaf_treatment <- factor(ancil_dat$leaf_treatment, levels = c("low", "medium", "high"))

AFDM_remain <-  ggplot(subset(ancil_dat, leaf_treatment!= "control"), aes(x=leaf_treatment, y=percent_AFDM_remaining, fill=temp_C)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) + scale_fill_manual(values = palette2) +
  ylab("AFDM remaining (%)") +
  theme_bw() + theme(axis.title = element_text(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_blank(), legend.title=element_blank() , axis.line = element_line(), axis.title.x = element_blank())
AFDM_remain 


#ancil_dat$k <- ((ancil_dat$dry_input_leaves_g - ancil_dat$dry_output_leaves_g) / ancil_dat$dry_input_leaves_g ) *100

#subset just the k column and column_ID columns 
leaf_mass_loss <- ancil_dat[, c("percent_AFDM_remaining", "Column_ID")]

str(leaf_mass_loss) #252 obs. of  30 variables

#save

write.csv(leaf_mass_loss, "C:/Users/teresa.silverthorn/Dropbox/My PC (lyp5183)/Documents/Mesocosm experiment/Mesocosm/leaf_mass_loss.csv")

