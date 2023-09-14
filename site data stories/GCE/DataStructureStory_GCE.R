#author: Mack White
#date: 09-11-2023
#goal(s): GCE ta structure and goals w Amanda Spivak @ UGA
#https://amandaspivak.wordpress.com/

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

#most of the longterm consumer data comes from the fall monitoring efforts
#how does grasshopper differ from some of the planthoppers, have crabhole data bc we have something like 10 different crab speces
#hard to do abundance bc they scatter, crabholes as proxy for these sorts of things
#porewater nutrients with associated crabhole densities

# project doesnt do porewater monitoring, but they do surface water (org + inorg nutrients, temp, salinity, etc.)
# nutrient data collected quarterly on incoming or high tides
# project-specific porewater dataset and methods differ alot across sites potentially
# for eg, GCE has dataset but may be rather variable - everything up to 2021 or 2022 has been posted

# molluscs certainly have a CND effect - localized - these animals also change the elevation of the marsh
# crab hole data, not seeing a ton of effect so far
# littoraria - in part bc of how it effects the grazes, sort of like a lawnmower - in terms of nutrients, could really effect demand
# these are like small snails
# grasshoppers havent been thought of as huge good source
# Amanda suggests molluscs, littoraria, and crab hole
      # good relationships between crab hole size, and the size of the actual crab themselves
      # periwinkle snail - juvenile vs adult distinction, seem to be in bigger quadrats than fall monitoring density

# some crabs surface bioturbators, some rip up roots, and some burrow deeper - so effects on CND very contextual
# most of these crabs detritivores, and opportunistic omnivores
# zone - creek bank vs. mid marsh...represents how much tidal flooding, creek bank more snails - can climb up the grasses and mid-marsh more compacted,
# we see differences in dist. of snails due to geomorphology of marsh - see that for crabs, eg sand fiddler vs mud fiddler vs mud crabs - main reason while
# sampling across these zones to really get at hydrologic influeces in the salt marsh
# system is so structured by the hydrology - why CND little studied in these habitats, so dynamic

#Di lorio et al - salinity variation through time - would group by salinity instead of by sound and then grouping it by midmarsh vs creek bank

#plants structuring 3D habitat, 

# read in excretion data --------------------------------------------------

# read in env data --------------------------------------------------------

# read in consumer data ---------------------------------------------------

#Mollusc Abundace -> 2000 through 2020, sampled annually in October it looks liek
mollusc_abund_GCE <- read_csv("GCE/data/mollusc_abund_GCE.CSV")
summary(mollusc_abund_GCE)#mollusc density ranging from 0-1092 ind/m2

# if for some of the species, were they targeted? eg mussels in mussel mound - sampling artifact, they are looking for mussels - maybe why such high max density
# if this applies, then likely just for the ribbed mussels
#melampus everywhere, while ribbed mussels are sort of patchy - for this mollusc data, may be worth pulling out sessile vs mobiles

# collecting all molluscs, and bringing them back to lab to process 

unique(mollusc_abund_GCE$Site_Name) #10 sites - GCE homepage, sites are sort of set up along salinity gradients which differ across the different sounds (which there are 3 sounds) 
#link to https://gce-lter.marsci.uga.edu/public/app/dataset_details.asp?accession=INV-GCES-1610
glimpse(mollusc_abund_GCE)

#https://gce-lter.marsci.uga.edu/public/app/species_details.asp?id=Melampus%20bidentatus
#https://gce-lter.marsci.uga.edu/public/app/species_details.asp?id=Geukensia%20demissa

mollusc_abund <- mollusc_abund_GCE |> 
      mutate(date = as.Date(Date, "%m/%d/%y"))
glimpse(mollusc_abund)

### Year-site means
mollusc_abund_site <- mollusc_abund |> 
      group_by(date, Year, Site_Name) |> 
      summarise(mean_dens = mean(Mollusc_Density)) |> 
      drop_na() # had some issues w missing data in 2005/2006 - not best solution but for quick look, probably okay

summary(mollusc_abund_site)
glimpse(mollusc_abund_site)
###grouping salinity makes a lot of sense, instead of just sites or the entire LTER site
###no one has really looked at these data - detailed monitoring, but no champion on those data
###what they are looking at is a little different than just simple LT looks
mollusc_abund_plot <- ggplot(mollusc_abund_site, aes(x=date, y=mean_dens, group = 1)) +
      facet_wrap(~Site_Name) +
      geom_line(color = "black", linewidth = 1.5) +
      geom_point(size = 2.5) +
      geom_smooth() +
      labs(title = "GCE-LTER: Fall Monitoring Mollusc Densities", x = "Date", 
           y = "Mean Mollusc Densities (ind/m^2)") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            axis.text = element_text(size=12,face="bold", color = "black"),
            axis.title = element_text(size=12,face="bold", color = "black"))
mollusc_abund_plot

# ggsave(filename='GCE/plots/mollusc_den_bysite.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 18, #wide for formatting
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### Annual mean across all sites
mollusc_abund_all <- mollusc_abund |> 
      group_by(Year) |> 
      summarise(mean_dens = mean(Mollusc_Density)) |> 
      drop_na() # had some issues w missing data in 2005/2006 - not best solution but for quick look, probably okay

summary(mollusc_abund_all)
glimpse(mollusc_abund_all)

mollusc_abund_plot_ALL <- ggplot(mollusc_abund_all, aes(x=Year, y=mean_dens, group = 1)) +
      # facet_wrap(~Site_Name) +
      geom_line(color = "black", linewidth = 1.5) +
      geom_point(size = 2.5) +
      geom_smooth() +
      labs(title = "GCE-LTER: Fall Monitoring Mollusc Densities", x = "Date", 
           y = "Mean Mollusc Densities (ind/m^2)") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            axis.text = element_text(size=12,face="bold", color = "black"),
            axis.title = element_text(size=12,face="bold", color = "black"))
mollusc_abund_plot_ALL

# ggsave(filename='GCE/plots/mollusc_den_ALL.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7, #wide for formatting
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### means by zone through time

mollusc_abund_zone <- mollusc_abund |> 
      group_by(Year, Zone) |> 
      summarise(mean_dens = mean(Mollusc_Density)) |> 
      drop_na() # had some issues w missing data in 2005/2006 - not best solution but for quick look, probably okay

summary(mollusc_abund_zone)
glimpse(mollusc_abund_zone)

mollusc_abund_plot_ZONE <- ggplot(mollusc_abund_zone, aes(x=Year, y=mean_dens, group = 1)) +
      facet_wrap(~Zone) +
      geom_line(color = "black", linewidth = 1.5) +
      geom_point(size = 2.5) +
      geom_smooth() +
      labs(title = "GCE-LTER: Fall Monitoring Mollusc Densities", x = "Date", 
           y = "Mean Mollusc Densities (ind/m^2)") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            axis.text = element_text(size=12,face="bold", color = "black"),
            axis.title = element_text(size=12,face="bold", color = "black"))
mollusc_abund_plot_ZONE

# ggsave(filename='GCE/plots/mollusc_den_ZONE.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 12, #wide for formatting
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

river <- read_csv("GCE/data/altamaha river_2008thru2020_gaugedata.csv") #altamaha river data
glimpse(river)
summary(river)

river <- river|> 
      mutate(Date = as.Date(Date, "%m/%d/%y")) |> 
      select(Date, Year, Month, Day, Daily_Mean_GageHeight, Daily_Mean_Temp_Water)
      # mutate(gageheight_imp = ts(river$Daily_Mean_GageHeight,start(2008,2020),frequency = 365)) |> 
      # mutate(temp_imp = ts(river$Daily_Mean_Temp_Water,start=c(2008,2020),frequency = 365))

altmaha_river_temp <- ggplot(river, aes(x=as.Date(Date), y = as.numeric(Daily_Mean_Temp_Water), group = 1)) + 
      geom_line(color = "black", linewidth = 0.5) +
      geom_point(size = 1.0) +
      # geom_smooth() +
      # facet_wrap(~common_name) +
      labs(x = "Sample Date", 
           y = "Altmaha USGSGage Mean Daily Temperature (°C)") +
      theme(panel.grid.major = element_blank(), 
            # panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            # axis.text = element_text(size=12,face="bold", color = "black"),
            # axis.title = element_text(size=12,face="bold", color = "black"), 
            axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
            panel.grid.minor = element_blank(),legend.position = "none") + 
      scale_x_date(limits = as.Date(c("2014-01-01", "2018-01-01")), date_labels = "%Y",breaks ='1 year')
altmaha_river_temp # looks like data only for 2014-2018 - so set limits

altmaha_river_stage <- ggplot(river, aes(x=as.Date(Date), y = as.numeric(Daily_Mean_GageHeight), group = 1)) + 
      geom_line(color = "black", linewidth = 0.5) +
      geom_point(size = 1.0) +
      geom_smooth() +
      # facet_wrap(~common_name) +
      labs(x = "Sample Date", 
           y = "Altmaha USGSGage Mean Daily Gage Height (m)") +
      theme(panel.grid.major = element_blank(), 
            # panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            # axis.text = element_text(size=12,face="bold", color = "black"),
            # axis.title = element_text(size=12,face="bold", color = "black"), 
            axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
            panel.grid.minor = element_blank(),legend.position = "none") + 
      scale_x_date(date_labels = "%Y",breaks ='1 year')
altmaha_river_stage

# ggsave(filename='GCE/plots/altmaha_river_stage.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 15, #wide for formatting
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

