#author: Mack White
#date: 09-14-2023
#goal(s): CCE data structure and story w/ DC, AS, & GC.
#https://ccelter.ucsd.edu/

# load libraries ----------------------------------------------------------

library(lubridate)
library(scales)
library(imputeTS)
library(pracma)
library(car)
library(viridis)
library(reshape2)
library(vegan)
library(tidyquant)
library(glmmTMB)
library(DHARMa)
library(stringr)
library(MuMIn)
library(ggeffects)
library(tidyverse)

# read in excretion/egestion data -----------------------------------------

# read in env data --------------------------------------------------------

###daily sea surface temps (1916-2019)
sst <- read.csv("site data stories/CCE/data/daily seas surface temp measurements_1916thru2019.csv")
glimpse(sst) #date read in as date format
summary(sst) #min temp = 10.1 C, w max = 26.4 
# CCE there is a lot of side sampling programs associated, shore station data as well - COS continuous data, but coastal
# this lter site more focused on open ocean side of things - lower resolution datasets and interannual pertubations
# mack needs to double check this data set
# cce hits lagrangian cycle - psudeo kinda - drifter and follow a water mass - saw how it propogated at coast and followed into pelagic
# how specific water masses influence inshore and offshore
# fluid mechanics - going with flow

# read in consumer data ---------------------------------------------------

###dry weight biomass mesozooplankton (2006 - 2017)
# 2021 will be available pretty soon and PYYMM
# bc CCE is lower res, maybe best option would be image-based biomass ts 
# carbon bm ests from imaging - more temporal rs and at some location - not sat imaging
# dante sent link in chat to these data - zooscan database + the CCE team made figure
# rather than looking at ts, looking at c fluxes ~> n fluxes (sent paper in chat - estimates within could be converted to nitrogen, but not over time just from 06 and 07 cruises)
dw_bm_mz <- read_csv("site data stories/CCE/data/dryweightbiomass_mesozooplankton_2006thrupresent.csv")
glimpse(dw_bm_mz) #no date (or year, month, etc.), just cruise name
unique(dw_bm_mz$cruise) #8 unique cruises - 8 years? Biannual - every other year
summary(dw_bm_mz) #min mean biomass (mg/m3) = 0.01, max = 104.17
dw_bm_mz <- dw_bm_mz |> 
      rename("mean_bm_mgm3" = 'biomass_mean (mg/m³)',
             'size_mm' = 'size_fraction  (mm)',
             'mean_bm_mgm2'= 'int_biomass_mean (mg/m²)') |> 
      mutate_if(is.character,as.factor) |> 
      mutate(log_mean_bm_mgm2 = log(mean_bm_mgm2))

###spring annual average mesozooplankton as carbon (no dates)
saa_mz_carbon <- read_csv("site data stories/CCE/data/spring_annual_avg_totalmesozooplankton_as_carbon_1951thru2008.csv")
glimpse(saa_mz_carbon) #spring annual averages as carbon w year as metric of time
summary(saa_mz_carbon) #min log10 mg/m2 = 2.65, max = 3.36
saa_mz_carbon <- saa_mz_carbon |> 
      rename("log_c_bm_mgm2" = "Mesozooplankton C biomass (log10) (mg/m²)") |> 
      mutate(c_bm_mgm2 = 10^log_c_bm_mgm2)
saa_mz_carbon <- na_interpolation(saa_mz_carbon)
      
summary(saa_mz_carbon) #min mg/m2 = 441.2 max = 2307.8

###gut fluoresence mesozooplankton (no dates)
gf_mz <- read_csv("site data stories/CCE/data/gut_fluoresence_mesozooplankton_2006thrupresent.csv")
unique(gf_mz$cruise) #8 unique cruises - same as dw_bm_mz data above? 8 years?
glimpse(gf_mz) #no date (or year, month, etc.), just cruise name
summary(gf_mz) #biomass mean mg/m2 min = 0.807, max = 22569.8
gf_mz <- gf_mz |> 
      rename("mean_bm_mgm3" = 'biomass_mean (mg/m³)',
             'mean_bm_mgm2'= 'int_biomass_mean (mg/m²)') |> 
      mutate_if(is.character,as.factor) |> 
      mutate(log_mean_bm_mgm2 = log(mean_bm_mgm2))
# plotting env data -------------------------------------------------------
glimpse(sst)
sst <- sst |> 
      rename('date' = 'Date..PST.',
             'sst_c' = 'Sea.Surface.Temperature..C.') #|> 
      # group_by(Year, Month) |> 
      # summarise(ym_mean = mean(sst_c)) |> 
      # drop_na()

sst_plot <- ggplot(sst, aes(x=date, y=sst_c, group = 1)) +
      # facet_wrap(~Site_Name) +
      geom_line(color = "black", linewidth = 1.5) +
      geom_point(size = 2.5) +
      geom_smooth() +
      labs(title = "CCE-LTER: Sea Surface Temperatures (1916-2019)", x = "Date", 
           y = "Sea Surface Temp (C)") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            axis.text = element_text(size=12,face="bold", color = "black"),
            axis.title = element_text(size=12,face="bold", color = "black"))
sst_plot

# ggsave(filename='CCE/plots/sst_1916thru2019.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 15, #wide for formatting
#        height = 5,
#        units = c("cm"),
#        dpi = 300)


# plotting consumer data --------------------------------------------------
glimpse(dw_bm_mz)
dw_bm_plot <- ggplot(dw_bm_mz, aes(x = cruise, y = log_mean_bm_mgm2, fill = cruise)) +
      geom_boxplot() +
      # scale_y_continuous(name = "mean_bm_mgm2") +
      labs(title = "CCE-LTER: Mean Mesozooplankton Dry Weight Biomass", x = "Cruise Name", 
           y = "Mean Mesozooplankton Dry Weight Biomass log(mg/m2)") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            axis.text = element_text(size=12,face="bold", color = "black"),
            axis.title = element_text(size=12,face="bold", color = "black"),
            legend.position = 'none')
dw_bm_plot

# ggsave(filename='CCE/plots/dw_bm_boxplot_cruises.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 10, #wide for formatting
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# glimpse(gf_mz)
# gf_plot <- ggplot(gf_mz, aes(x = cruise, y = log_mean_bm_mgm2, fill = cruise)) +
#       geom_boxplot() +
#       # scale_y_continuous(name = "mean_bm_mgm2") +
#       labs(title = "CCE-LTER: Mean Mesozooplankton Dry Weight Biomass - GF Dataset", x = "Cruise Name", 
#            y = "Mean Mesozooplankton Biomass (mg/m2)") + #is this dryweight or wet weight in this data set?
#       theme(panel.grid.major = element_blank(), 
#             panel.grid.minor = element_blank(),
#             panel.background = element_blank(), 
#             axis.line = element_line(colour = "black"),
#             plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
#             axis.text = element_text(size=12,face="bold", color = "black"),
#             axis.title = element_text(size=12,face="bold", color = "black"),
#             legend.position = 'none')
# gf_plot #same data as above, this dataset just includes the gut fluoresence data as well I am guessing? What metric of interest in this dataset?

# ggsave(filename='CCE/plots/dw_bm_boxplot_cruises.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 10, #wide for formatting
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

glimpse(saa_mz_carbon)
mz_saa_carbon_plot <- ggplot(saa_mz_carbon, aes(x=Year, y=log_c_bm_mgm2, group = 1)) +
      # facet_wrap(~Site_Name) +
      geom_line(color = "black", linewidth = 1.5) +
      geom_point(size = 2.5) +
      geom_smooth() +
      labs(title = "CCE-LTER: Spring Annual Average Mesozooplankton Biomass (1951-2009)", x = "Year", 
           y = "Mesozooplankton Biomass (as Carbon; mg/m2)") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            axis.text = element_text(size=12,face="bold", color = "black"),
            axis.title = element_text(size=12,face="bold", color = "black"))
mz_saa_carbon_plot

# ggsave(filename='CCE/plots/saa_mz_biomass_as_carbon.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 10, #wide for formatting
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

#looking at SST - need to determine where this data came from once EDI is back up
# mz figure - link in chat may better represent - each of these cruises has onshore and offshore tracks
# mean bar - 
# challenge in these ecosystem dont occur at the same time of year, CCE cruises funded in these cycles - different cruises focused on different things
# even if you took the mean from all the samples and looked year to year, wouldn't necessarily be comparable
# grid data will be a bit better bc it is consistent in space and time
# that data is in chat 
# saa figure - could still seperate by onshore and offshore - more variability onshore in spring bc pulses of upwelling nutrients
# cruises could have happened during more productive times - potentially look at disturbance too, maybe
# missing the blob and son of the blob, maybe daughter
# intensity of upwelling increased with changes in SST - been cool and warm times, can see that in drops in zooplankton
# upwelling has intensified - nutrients and food availability, seen in size and abundance
# not necessarily look at long term trend - aside from extreme heat wave - largest disturbance can point to
# el nino cliamte variability - periods of up and down oscillations - particulat species of zooplankton track these env indices
# somewhat of a linear trend - for thes data, multiple el nino events - lot about this in the literature
# harmonization note - idea - pertubrations captured at a site related to sampling frequency - for CCE interannual disturbance - grouping by sampling frequency or timescale of disturbances
# el nino a perutbation? differences in disturbance vs perturbation - el nino an env pertubation
# el nino a perubation bc periodicity - not not gonna come back where disturbance is something like the blob and blob2
# the perfect storm is when blob and el nino came together, would consider those interactive effects of EN there
# el nino alters the dominant water mass (really everything)
# could bring up different sp of plankton - greater stratification, etc.- lot of papers on these things though

#important links from chat:
#zooscan database - https://oceaninformatics.ucsd.edu/zooscandb/
#CALCOFI - https://calcofi.org/data/marine-ecosystem-data/zooplankton/
# c/n flux paper - https://www.frontiersin.org/articles/10.3389/fmars.2019.00508/full


# BIG TAKEAWAYS -----------------------------------------------------------

#CALCOFI imaging data may be best dataset for working group efforts
#nutrient/energy availability ~ upwelling, el-nino, and sst - oscillating and interactive
#heat blob is prominent disturbance event, but ^ effects when interacting with el nino
