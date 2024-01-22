###########################################################################
# Project: CND Synthesis Working Group
# Author: Mack White - unless it doesn't work
# Goal: Generate figures for CND synthesis in-person meeting
# Date(s): January 22 - 25, 2024
# Additional Authors:
###########################################################################

# housekeeping ------------------------------------------------------------

# install.packages(librarian)
librarian::shelf(readr, tidyverse, googledrive, readxl, 
                 taxize, stringr, lubridate, scales, ggeffects,
                 ggplot2)

cons <- read_csv("data/tier1/harmonized_consumer_20240117.csv")
glimpse(cons)

###########################################################################
# FIGURE ONE: BIOMASS -----------------------------------------------------
###########################################################################

# SBC - Aquatic Dataset ---------------------------------------------------

# section leads: place name here
sbc_aqua <- cons |> 
  filter(raw_filename == "Annual_All_Species_Biomass_at_transect_20230814.csv", #sort out your dataset using the raw filename
         measurement_type == "wetmass") #if you have multiple measurements for biomass, use wet mass for Figure One

### setting up data for biomass figure generation

sbc_summ <- sbc_aqua |> #setting up new dataframe with summarized dat
  group_by(year,site) |> #global biomass est for each of the diet_cat
  summarize(mean_bm = sum(measurement_value)/n()) #calculate mean biomass for each diet_cat for "project"

### generation of global biomass estimate per project per diet category

ggplot(sbc_summ, aes(fill = diet_cat, x=year, y = as.numeric(mean_bm))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", 
       y = "Mean Annual Biomass (g/m2)") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top")

### save your plot below, but make sure to rename 
### highlight text below, then (command +shift + c) to run code

# ggsave(filename='plots/figure_one/sbc_aqua_mean_bm',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 8,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

###########################################################################
# FIGURE TWO: EXCRETION ---------------------------------------------------
###########################################################################