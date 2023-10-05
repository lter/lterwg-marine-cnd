#author: Mack White
#date: 09-26-2023
#goal(s): Caribbean data structure and goals w Jake Allgeier
#Jake Lab Page -> https://www.jacoballgeier.com/

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

# other important links ---------------------------------------------------

# Notes from Meeting ------------------------------------------------------

##### Q1a: What data sets do you envision as being part of synthesis efforts from Caribbean (consumer type? length of time series? units?)

##### Q1b:Considerations/Important Notes? Env data sets? Exc?

# bunch of data sets >100 artificial reefs - specific to proejcts, some just to see what is going on 
# for example, one of the oldest projects factorial design (fishing pressure and nutrient pollution)
# inconsistent time series since 2010 - no data for a couple years, multiple surveys within a few months
# currently compiling all of that, postdoc working on that dataset - mainly interested in production
# biodiversity and ecosystem function, including feedbacks - all built in seagrass beds
# some seagrass community and production data alongside
# newer style of reefs, which are bigger and more fish more diversity -> katrina has made best time series with these for six reefs
# clusters of reefs for experiment ->
# spotty, here and there -> ts analysis, only thing comfortable with using would be those six six reefs
# some ts analysis, not that many dates - dozens of surveys across ~3 years... those first reefs since 2010 
# all of them will have some signal of assembly or changing community
# these are the main things -> just compiled, could give more specific #'s - two concurrent studies, biodiversity
# surveys mostly by Jake or Katrina - some of older by layman
# goal is to count every fish on the reef
# not many cryptics, but dont look at those bc tucked in those cinderblocks
# pretty good sampling shown w clove oil studies
# dont really look at invertebrates, but do count the big and obvious - tons of smaller inverts we dont mess
# some extraordinary stuff, do count however - things such as lobster and crabs
# make up small proportion of biomass generally -> some seasonality to this however
# one of the cooler findings, fish in haiti - reefs are loaded and fishermen have hard time fishing these reefs despite heavy fishing pressure
# white grunt squirrel fish, grey snapper at times, schoolmasters decent - sort of big biomass contributors
# 

##############################################################################

##### Q2a: Have you or others looked at changes in biomass of those communities/species through time? Any trends? 

##### Q2b: If so, relevant figures or references in literature? Would you be willing to share consumer ts plot at Oct 17 meeting?

# have never really looked at it until now - get all that data together and see what it says - relating productivity and biomass

##############################################################################

##### Q3a: What sort of disturbances/perturbations which may have effects on consumers in Caribbean?

##### Q3b: Are there specific disturbance events that come to mind (e.g., FCE -> cold snap in 2010, droughts in 2011 and 2015, hurricane in 2017)

##### Q3c: Do you feel the consumer/env data sets from the Caribbean capture these? Do you feel your sampling periodicity captures?

# hurricanes, heat - epic heat here recently, last few years have good record of temperature and also wave action - didn't have these buoys out when these big storms come through
# Dorian was the big one in 2019 - surveys spring before and maybe after? covid hit, so maybe not...
# this last summer for heat, also some pretty strong cold snaps - w heat, know certain threshold for bleaching but also get some incredible cold snaps - crazy thermocline a few years ago

##############################################################################

##### Q4a: Goals of October Meeting - what do you want to get out of it? What do you think would be an important goal(s) of the meeting?

# not going to be able to make the October meeting

### what questions could we address or are we missing something?
# main comment with this line of work is that on some level, if it is just talking about amount of nutrients.. some sort of response variable is necessary at some point - CND as proxy for thing that fish are doing
# we know translocating stuff into the system, but dont truly know to extent that CND matters at this point
# recycling, does it matter? rate at which recycling - potentially speeding up production
# when and why 
# interplay between recycling and exogenous stuff coming into system if dont have true response variable
# turnover - if have a lot of biomass vs low biomass, size structure should matter a lot
# think about through time and relative role through 

# yes and no with background nutrient conc - snapshot concentrations, doesnt tell you anything about uptake
# could be highly productive and taking everything up - doesnt necessarily say about the fish
# better than nothing, with time strong signals can make signals
# missing that measurement of process - snapshot of water column isnt a process but better than nothing

# stress moving beyond how much they pee here or there, good first step, but thinking about why do we care about - persistently challenge that we look at the context - does it make a difference? 

# stipulation of publishing is important to note in the meeting - newer paper indicative of how they have been thinking about this - 2023 Andskog

##### Q5a: Break-out prompts you think would be useful to advancing synthesis efforts?

##############################################################################