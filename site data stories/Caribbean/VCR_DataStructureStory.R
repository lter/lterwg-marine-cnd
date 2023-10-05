#author: Mack White
#date: 09-28-2023
#goal(s): VCR data structure and goals w Max C
#Max Lab Page -> 

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

# Important Online Data Links (if applicable): ----------------------------

# http://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.247
# https://drive.google.com/drive/folders/1hj1czpbv6-xNb5lDBB415UiCEP_zvTOQ
# http://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.329
# http://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.373
# http://www.vcrlter.virginia.edu/cgi-bin/showDataset.cgi?docid=knb-lter-vcr.187

# VCR fish data set is getting ready to go up 
### inverts counts and biomass for certain groups - 50 sites
# like, worm biomass - several other groups - not bivalves, or some of the other crustaceans
# same with gastropods
# could totally try and generate some all relationships for biomass
# biomass ests not quite as accurate bc they are frozen
# throw taps and cores for inverts

### fish
# fishes are C&R, ID and size - TL @ ~28 sites
# small forage fishes, pipefishes, larval fishes
# VIMS data set does not sample as many sites and could be complimentary in some ways
# sample one meadow, monthly, and in the summer - maybe not in same swath
# don't know that much about the VIMS data - if anyone is gonna have allo, gonna be them for converting TL to mass

# temperature loggers at all sites, hydrodynamic model data, depth and elevation at these different sites, and seagrass and sediment
# both the fish and invert data, annual and in the summer
# one seine pull for site, and triplicate throw traps and benthic cores - samples are kept separate
# 2019 - 2022, for the inverts
# 2019 - 2023, minus 2020 for fish - Max C will email when its available
# blue crabs for a single year

# we get big things coming into the meadows, don't sample them, and major rec fisheries of some of the adults of the juveniles
# not very good ests of their density

# distance to inlets data set - Max C sent

# read in excretion data --------------------------------------------------

# read in env data --------------------------------------------------------

# read in consumer data ---------------------------------------------------

# other important links ---------------------------------------------------

# Notes from Meeting ------------------------------------------------------

##### Q1a: What data sets do you envision as being part of synthesis efforts from Caribbean (consumer type? length of time series? units?)

##### Q1b:Considerations/Important Notes? Env data sets? Exc?

##############################################################################

##### Q2a: Have you or others looked at changes in biomass of those communities/species through time? Any trends? 

##### Q2b: If so, relevant figures or references in literature? Would you be willing to share consumer ts plot at Oct 17 meeting?

# really concerned about heat waves, tide-dominated
# in meadow, on summer day, can get bathwater hot...
# areas closer to ocean inlets gets a lot of water flushing over
# different parts likely experiencing heat differently, spatially

# also have storms, like thunder storms, biggest storms in fall through winter but don't really sample around them
# pretty sure abundance is quite low during winter 

##############################################################################

##### Q3a: What sort of disturbances/perturbations which may have effects on consumers in Caribbean?

##### Q3b: Are there specific disturbance events that come to mind (e.g., FCE -> cold snap in 2010, droughts in 2011 and 2015, hurricane in 2017)

##### Q3c: Do you feel the consumer/env data sets from the VCR capture these? Do you feel your sampling periodicity captures?

##############################################################################

##### Q4a: Goals of October Meeting - what do you want to get out of it? What do you think would be an important goal(s) of the meeting?

# synthesis working groups are hard - making good use of the time, clear tasks during that time period towards whatever product you are building to
# can spend a lot of time brainstorming
# good to do one on one meetings before hand, keep people engaged and involved

# the site is rather oligotrophic, small and rural - nutrient conc data which goes back to the 90s - 1992

##############################################################################
##### Q5a: Break-out prompts you think would be useful to advancing synthesis efforts?

##############################################################################