#author: Mack White
#date: 09-04-2023
#goal(s): SBC data structure and goals w Kyle Emery (Post-doc @ UCLA)


# load libraries ----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(writexl)

# read in excretion data --------------------------------------------------

# invertebrate_NH4_excretions <- read_csv("SBC/data/exc/invertebrate_NH4_excretions.csv")
# glimpse(invertebrate_NH4_excretions)
#metadata: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.137.1

################################################################################
################################################################################
################################################################################
# read in env data --------------------------------------------------------

### BEACH WRACK: NUTRIENT DEPOSITION - Sept 2008 through Dec 2020. SEVEN sites? 
beach_wrack_cover <- read_csv("SBC/data/env/Wrack_Cover_All_Years_20210929.csv")
glimpse(beach_wrack_cover)
#metadata: https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.40
summary(beach_wrack_cover)

#Q: How long are sample transects? Does 1,186 kg of wrack appear correct for BIOMASS?
# noted data was a bit large and messy, would be willing to go through if we think it can be used
# five sites that are consistent, three at each site and surveyed monthly for kelp, seagrass, etc
# not a consistent length bc beach is not consistent naturally, density gets it close given changing dimensions
# shore perpendicular, from cliff to ocean - about a meter wind band, area within wrack (m^2.m = m of wrack)
# goal is to collapse width dimension w/ m of wrack regardless of beach variability in time/space
# cover is est for wrack subsidies, quantifying nutrients need to convert to biomass - tho biomass not for all types of wrack
# random stuff, do not think biomass relationship for those - could estimate. believes vol is available for all though
# biomass - avg values, but C:N data for most of the ind species - large values legitimate

beach_wrack_cover$day=1

beach_wrack_cover$s_date=as.Date(paste(beach_wrack_cover$YEAR, beach_wrack_cover$MONTH, beach_wrack_cover$day,sep="-"))

beach_wrack_grouped <- beach_wrack_cover |>
      group_by(s_date, SITE) |>
      summarise(BM_tot = sum(BIOMASS)) |> 
      mutate(log_BM_tot = log(BM_tot))

beach_wrack_ts_plot <- ggplot(beach_wrack_grouped, aes(x=s_date, y=log_BM_tot, group = 1)) +
      facet_wrap(~SITE) +
      geom_line(color = "black", linewidth = 1.5) +
      geom_point(size = 2.5) +
      geom_smooth() +
      labs(title = "SBC-LTER: Monthly Beach Wrack Cover", x = "Date", 
           y = "Log Total Biomass (wet weight; kg)") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            axis.text = element_text(size=12,face="bold", color = "black"),
            axis.title = element_text(size=12,face="bold", color = "black"))

beach_wrack_ts_plot

ggsave(
      'SBC/plots/SBC_wrack.png',
      plot = last_plot(),
      scale = 1.75,
      width = 6,
      height = 4,
      units = "in",
      dpi = 300,
)
# dataset includes carion, coded a "a" oyster -> dolphin or sea lion, no conversions there
# get it relatively frequently, but doesn't happen where those transects usually are
# separate data set, with dead animals on bird transect data
# winter storms and wave energy can generate big wrack values - but also in summertime as well, under story algae goes crazy bc of kelp loss and then shaded out once more
# other types of wrack coming from inland - all of this recorded in data file

#Q: CSB a typo or site only sampled a few times? - very short term, and CCB was short term (wouldn't want to use - GROOMED BEACH)
#Q: see sort of sinusoidal pattern across space and time? 2019 looks like down year for wrack? 
#Q: Any other thoughts here? - valleys in graphs resulting from el nino and warm blob, statewide/coastal-wide decline in kelp
# big storms + warming = uptick, then low offshore availability
################################################################################
################################################################################
################################################################################
# read in consumer data ---------------------------------------------------

# water_benthics <- read_csv("SBC/data/consumer/Annual_All_Species_Biomass_at_transect_20230201.csv")
# glimpse(water_benthics)
# #metadata: https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.50

### BEACH BIRDS - October 2008 through December 2019. SIX sites
beach_birds <- read_csv("SBC/data/consumer/Shorebird_count_20211012.csv")
glimpse(beach_birds)
#metadata: https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.51
summary(beach_birds)
# seasonality in which they overwinter in SBC, april through aug relatively low then peak
# same sites, CSB-CCB could be dropped - SAME sites as wrack but parallel km survey (no within survey replication, but count)
# also surveyed at same frequency as wrack - species + counts, data is available to convert biomass
# no excretion data, but there is very likely some estimates in the literature
# this should also have the large carion (e.g., birds and marine mammals - maybe fish) data in it... not the molluscs, and tiny things

beach_birds$s_date<-as.POSIXct(beach_birds$DATE,"%m/%d/%Y",tz = "UTC") #format datetime 

beach_birds_grouped <- beach_birds |>
      group_by(s_date, SITE) |>
      summarise(count_tot = sum(TOTAL)) |> 
      mutate(log_count_tot = log(count_tot))

beach_birds_plot <- ggplot(beach_birds_grouped, aes(x=s_date, y=count_tot, group = 1)) +
      facet_wrap(~SITE) +
      geom_line(color = "black", linewidth = 1.5) +
      geom_point(size = 2.5) +
      geom_smooth() +
      labs(title = "SBC-LTER: Shorebird Counts", x = "Date", 
           y = "Counts (#)") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            axis.text = element_text(size=12,face="bold", color = "black"),
            axis.title = element_text(size=12,face="bold", color = "black"))

beach_birds_plot

ggsave(
      'SBC/plots/SBC_shorebird_counts.png',
      plot = last_plot(),
      scale = 1.75,
      width = 6,
      height = 4,
      units = "in",
      dpi = 300,
)
#Q: Generating excretion estimates? Do we need understanding of size/mass? If so, is that data available?
#Q: Any other thoughts here?

#BEACH MACROINVERTEBRATE DATA - Aug 2011 through Nov 2020
beach_macroinverts <- read_csv("SBC/data/consumer/Beach_Consumer_Zerofilled_20211011 (1).csv")
glimpse(beach_macroinverts)
#metadata: https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.91
summary(beach_macroinverts)
#Q: transect lengths?
# this is like an annual survey, when we would expect them to be at their highest numbers
# good timeseries, but pretty limiting
# useful for site-level differences
# includes detritivores and predators - a lot of endemic inverts on the beach
# same sampling methods as the wrack data, core samples - nocturnal is why
beach_macroinverts$s_date=as.Date(paste(beach_macroinverts$Year, beach_macroinverts$Month, beach_macroinverts$Day,sep="-"))

beach_inverts_grouped <- beach_macroinverts |>
      group_by(s_date, Site) |>
      summarise(tot_wtwet = sum(Wet_Weight)) |> 
      mutate(log_tot_wtwet = log(tot_wtwet))

beach_inverts_plot <- ggplot(beach_inverts_grouped, aes(x=s_date, y=tot_wtwet, group = 1)) +
      facet_wrap(~Site) +
      geom_line(color = "black", linewidth = 1.5) +
      geom_point(size = 2.5) +
      geom_smooth() +
      labs(title = "SBC-LTER: Invert Biomass", x = "Date", 
           y = "Total Biomass (wet weight; g)") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            axis.text = element_text(size=12,face="bold", color = "black"),
            axis.title = element_text(size=12,face="bold", color = "black"))

beach_inverts_plot

ggsave(
      'SBC/plots/SBC_inver_annualsampling_biomass.png',
      plot = last_plot(),
      scale = 1.75,
      width = 6,
      height = 4,
      units = "in",
      dpi = 300,
)
#Q: any thoughts here? 
#BEACH TALITRIDS - Jan 2013 through May 2019. FOUR sites - two of which only sampled in recent yrs?
beach_talitrids <- read_csv("SBC/data/consumer/IV_EC_talitrid_population.csv")
glimpse(beach_talitrids)
#metadata: uncertain
summary(beach_talitrids)
#Q: 2083 g per m seem legitimate? - totally, and birds will be there all the time pecking them out of the sand
# really cool, intricate web of how all the nutrients are moving through animals
#Q: any other thoughts?
# exact same survey methods as previous one, but survey every 6-8 weeks - a little more haphazard
# can pull seasonal trends out of it.
# people have looked at talitrid CND - paper which is a really good review below:
# https://onlinelibrary.wiley.com/doi/full/10.1111/brv.12886
# excretion rate estimates from kyle
# https://www.sciencedirect.com/science/article/pii/S0272771418308126?casa_token=Me9lWIJY2kcAAAAA:9pIE-UMBVZTEmr1MTjBDDmW-ObW5W_GR7uRyYUXZY2l43rgagho37B2SNlypS3xvmxLMvsz9reo - nitrate and ammonium
# the DIN contribution per talitrid am phipod ranges from 0.69 to 1.67 μmoles day−1
# . This can also be ex pressed as 0.40 to 0.97 μg-N individual−1 h−1
# diet study out of that lab showed fish eat a lot of beach hoppers - more wrack =  more hoppers in fish diet, plus during high tide series
# tons of seasonality - boom and bust across seasons on the beach, heat blob and el nino may be causing similar trends here
# beach had those storm effects as well, narrow beaches - combination of lack of food and habitat
beach_talitrids$s_date<-as.POSIXct(beach_talitrids$date,"%m/%d/%y",tz = "UTC") #format datetime 

beach_talitrids_plot <- ggplot(beach_talitrids, aes(x=s_date, y=biomass_g_per_m, group = 1)) +
      facet_wrap(~site) +
      geom_line(color = "black", linewidth = 1.5) +
      geom_point(size = 2.5) +
      geom_smooth() +
      labs(title = "SBC-LTER: Talitrid Biomass", x = "Date", 
           y = "Biomass (wet weight; g/m)") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            axis.text = element_text(size=12,face="bold", color = "black"),
            axis.title = element_text(size=12,face="bold", color = "black"))

beach_talitrids_plot

ggsave(
      'SBC/plots/SBC_talitrid_biomass.png',
      plot = last_plot(),
      scale = 1.75,
      width = 6,
      height = 4,
      units = "in",
      dpi = 300,
)

#Q: seems pretty variable, but any thoughts? see similar patterns across sites where things sort of drop 2015-2017
################################################################################
################################################################################
################################################################################

#github username: Kyle-Emery