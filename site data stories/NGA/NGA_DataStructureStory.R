#author: Mack White
#date: 09-22-2023
#goal(s): NGA data structure and goals w Russell Hopcroft
#locate and aggregate data for synthesis efforts
#review and summarize data
#visualize data through simple figure generation - could be used during Oct 17 meeting
#chat about site-level data story 
#Russ Lab Page -> https://www.uaf.edu/cfos/people/faculty/detail/russell-hopcroft.php

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

# read in excretion data --------------------------------------------------

# read in env data --------------------------------------------------------

# read in consumer data ---------------------------------------------------

NGA_good <- NGA_SKQ201810S_QUAD_Microscopy_L2_V1
# this is only one cruise from 2018, would need to merge multiple years of csv files to get time series
# good metadata online w units generally in the column name
# RH has pre-merged dataset, especially for historical data which was iconsistent w formatting
# clean dataset, wont make me go through and clean - still finding some mistakes
# may need some additional qc but this is a good set to move forward w according to RH - zooplankton best organized across all data


# looking at the NGA data, year with either month or ship name code
#TGC vs SKW vs WSD ship name
NOAA_zp_1997thru2006 <- read.csv("site data stories/NGA/data/US_GLOBEC_NEP_SECM_zoop.csv")
summary(NOAA_zp_1997thru2006)
glimpse(NOAA_zp_1997thru2006)
# coastal monitoring of inland water ways between islands - not specifically an NGA data set
# NOAA datasets do not generally have biomass, but instead counts

# other important links ---------------------------------------------------

# nga data repo - https://drive.google.com/drive/folders/1j8QGQR6_vD1SQnFwVnaAy0W-1c_owCRv
# site level data stories - https://docs.google.com/presentation/d/1-P0Fm4N4-J8zjMK_ygQVKqNftX_At1ij/edit#slide=id.g27a2f8f4ba7_1_153
# NGA DataOne - https://search.dataone.org/portals/NGALTER
# Kimmel & Duffy-Anderson (2020) -https://academic.oup.com/plankt/article/42/3/334/5837810
# Bordeur and Ware (1992) - https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1365-2419.1992.tb00023.x?saml_referrer

# Notes from Meeting ------------------------------------------------------

##### Q1a: What datasets do you envision as being part of synthesis efforts from NGA Site / Northern Gulf of Alaska?

#2018-2020, but some historical data someplace... gave to NCEAS in a hurtt ~10 yrs ago in related project and merged up w/o looking at anything closely

#some ridiculous stuff shows up bc of that -> RH will send merged dataset
# other dataset which we may want to look at -> Microzooplankton abundance and biomass from research cruises for the Northern Gulf of Alaska (NGA) LTER site, 2018-2021
# handled by same data group, but not necessarily great QC/QA
# mz data sets w quad net and multinet as well, quad targets smaller and multinet targets larger and more mobile - > selective of certain taxa for each individual net groupings (some overlap between)
# diel consideration as well
# jellyfish data set that is ~4-5 years old, those big jellyfish you would catch in fisheries in trawl
# third piece of gear used here

# macronutrient and chl-a data sets as well, for those c ests an assumed conversion (usually a constant)
# suzanne makes efforts to look at C:Chl-a ratios, differs seasonally and across size fractions
# LTER period on website, macronutrients hannah and suzanne chl-a - making efforts to build timeseries but not sure what state they are in

# plankton nets are depth integrated, chl and macronutrients all bottlebased so need to interpolate
# bottle is representative of midpoint to midpoint, pretty food assumption unless weird stuff going on
# multi-net data is depth-stratified as well, RH can provide pre-integrated

##### Q1b:Considerations/Important Notes (e.g., length of time series, structure, limitations)

##############################################################################

##### Q2a: Have you or others looked at changes in biomass of those communities/species through time?

##### Q2b: If so, relevant figures or references in literature?

##############################################################################

##### Q3a: What sort of disturbances/perturbations which may have effects on consumers in NGA?

##### Q3b: Are there specific disturbance events that come to mind (e.g., FCE -> cold snap in 2010, droughts in 2011 and 2015, hurricane in 2017)

##### Q3c: Do data sets from NGA capture these? Do you feel your sampling periodicity captures?

##############################################################################

##### Q4a: Site-level Data Stories - which data set would you envision?

##### Q4b: Do relevant figures already exist?

##############################################################################

##### Q5a: Goals of October Meeting - what do you want to get out of it?

##### Q5a: Break-out prompts you think would be useful to advancing synthesis efforts?

# snap shot values or take advantage of modeling which makes own assumptions between snapshots

##### Q5c: What are some of the big takeaways from the meeting?

##############################################################################