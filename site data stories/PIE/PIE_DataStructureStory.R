#author: Mack White
#date: 09-18-2023
#goal(s): PIE data structure and goals w Jenn Caselle
#locate and aggregate data for synthesis efforts
#review and summarize data
#visualize data through simple figure generation - could be used during Oct 17 meeting
#chat about site-level data story 
#Jenn Lab Page -> https://www.nelsonecolab.net/

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
# nekton biomass - easies thing to do is calculate amount o fnutrients fluxed in the animals
# other aspect is connectivity consideration during high marsh flooding
# more days of flooding = increase in availability, can calculate the amount of material coming off high marsh into creek system
# biomass of fish in creek is more than creeks wo high marsh could support on their own
# when fish are down there excreting -> ests in mud flats.. when fish in creeks excreting N that wouldn't be available at low tide
# drivers of change for amount of nutrients moved by fish in given year
# years w more flooding, changes trophic position - flux should increase until marsh goes away
# US Coastal Marshes - spartina grasses that like rarely inundated habitats (~2-3x month)
# other grasses that can stand constant inundation - as SLR rises, converting steadily the habitat and grasses
# marsh equilibrium model when start converting - influences fish foraging - productivity declines when constantly inundated - when dry eating terrestrial inverts
# theoretical tipping point is MEM - we are seeing that now

dat <- read_csv("site data stories/PIE/data/LTE-TIDE-NektonFlumeIndividual_v6_2.csv")
# SW and CL are nutrient addition sites -> not a direct effect of enrichment on #s of fish
# 2.5x initially/at peak but complications with geomorphology
# the loss of structure was more powerful control on #s of fish in any creek, relative to nut enrichment at all
# biggest driver of biomass through time is nutrients from winter snow melt coming over the dam - sweet spot
# residence in 9-14 day window, blooms in bay drive productivity
# comes down to FW in right timeframe - eg rainy summer, stuff everywhere... 
# comes back to SLR question - @ first shift that window up - FW-SW head changing - passive dam
# timing/mag of FW inputs controlling productivity 
# biomass is variable, no obvious trends - 95% mummichogs - seeing less shrimp than we used to... no idea why
# mummichogs, monoculture of dominant biomass - omnivirous fish w trophic level ~3 eating whatever and prefers to eat proteiny things

# could group by fert vs unfert

# mummichog density plots for data story???
# temps and hydro maybe main drivers - seems to be the case
# spatial and temporal drivers of estuarine food webs 2015 paper has some good driver information
# parker river discharge - usgs gage -> https://waterdata.usgs.gov/monitoring-location/01101000/#parameterCode=00065&period=P7D&showMedian=true

# disturbances -> winter storm effects in plants, but not really in fish data... amt of sediment on marsh...
# droughts -> last few years which wacks the biomass really bad
# SLR - press 
# PIE issues... getting more intense rainfall, low as well - variability in rainfall patterns starting to show
# things getting hotter... 
# Fiddler Crabs have showed up (range expansion) - other predators like blue crabs as well... summer drought is big one
# Drought effects all the other animals that they go and get off the high marsh - not a lot of inverts, too dry - need to retreat into forest... not as much food growing, etc. 
# Size structure is hard to do bc of the way they grow... gonna be vary variable - nets catch fish 12mm or larger.... gonna get summer months in these data
# If you don't split up appropriately, could be an issue... mummichog excretion rates in the literature
# on thing to get out of this would be to see the magnitudes of nutrients across the different sites
# food web people never ever plug their data into way p productivity do...
# all these transformations... how efficient are these foods webs relative to amount of nutrients 
# no matter who sort of wins the lower productivity race... efficient way to get nutrients to high order consumer
# do you lose a lot of the nutrients bc of all the different species... bc in diffuse boxes. 
# FCE maybe more resilient bc of diversity in moving nutrients up... bc chain
# need to scale where they are at in the food web, how many steps to get there and excrete that... can try to apple-apple comparison
# mobility considerations, two ways - excretion or flux - crossing boundaries by moving nutrients
# moving from high to low nutrient availability? 
# what is more important in certain systems - the biomass of it - the tissue itself or its excretion from the animal
# mummichog get trapped in tidal pool example - algal biomass driven by them and then take all those nutrients to the bay
# mollusc eg is benthic-pelagic, 

# Other Notes -------------------------------------------------------------