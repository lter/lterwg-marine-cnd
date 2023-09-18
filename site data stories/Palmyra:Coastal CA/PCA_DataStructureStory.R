#author: Mack White
#date: 09-15-2023
#goal(s): Palmyra/Coastal CA data structure and goals w Jenn Caselle
#locate and aggregate data for synthesis efforts
#review and summarize data
#visualize data through simple figure generation - could be used during Oct 17 meeting
#chat about site-level data story 
#Jenn Lab Page -> https://labs.eemb.ucsb.edu/caselle/jennifer/
  
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

dat <- read_csv("site data stories/Palmyra:Coastal CA/data/MLPA_swath_site_means.csv")

# Other Notes -------------------------------------------------------------
#Datasets in DataOne do not have biomass, though Jenn does 
#Lets start with the "finals" sort of like the masters here at FCE
#MLPA Merged Datasets appear to be the cleanest

#in these kelp forest surveys - multiple techniques - seperated by techniques in data files
#swath...large and mobile - upc for benthic/sessile inverts and aspects of habitat - fish = fish counts
#also have size frequency data - roving diver for key fisheries inverts and 4-5 species of other species
#also raw and site mean datasets... high # of transects - raw is REALLY raw and giant datasets and other levels of stratification
#users of dataset... all you site means, could maybe want transect but usually focus on site mean data
#they work primarily with count data, except for upc which is % cover, and generally analyze dens from counts except for fish
#they are sizing all fish, so generate biomass as main metric... dont size all other organsims, however
#inverts and algae, using density - except for select species of inverts w relationships to generate biomass estimates
#looking at kelp forest swatch - multiple campuses which contribute and processing all on jenns groups
#all of these data have some metadata than jenn will pass along
#survey year is the year we would want to use - annual surveys - sizing giant kelp is stipe count

#MLPA swath site means - timeseries scoring = 1(best), 2(decent), Null (not good)


# UPC DATA ----------------------------------------------------------------

# every time on point, cover, substrate, and relief
# substrate - what kind of reef is it 
# cover - what is on it, the organism -> taxa categories (branching vs brown vs bryo vs corcal, etc.)
# benthic sessile organsisms
# relief - measure of highest to lowest point (but categorical)
# no zero-populating w these data, or at least it is much more rare
# when we 
#jenn gonna put all protocols in drive

# fish --------------------------------------------------------------------
#similar to other datasets - but for fishes vertical partitioning
#this has temperature
#really clean datasets - rather than having me do it, reach out to Jenn first
#jenn will add a lot of additional materials
#Li has made uber/master dataset to tie SBC & PISCO data (merging these dont give huge heartburn)


# PALMYRA -----------------------------------------------------------------

#Jenn has a clean dataset on fish densities and biomass @ irregular time points (maybe annual???)
#Never received access to benthic data - scripps colleagues have not shared that yet.

#Local kelp forest group going to be money -> 

#october 17 meeting - intentions of being there, but will be on layover but flight leaves near end of meeting
#move Jenn to beginning of case study slides.