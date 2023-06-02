### Author: Mack White
### Project: Consumer Nutrient Dynamics LTER Working Group
# set soft wrap long lines and rainbow parantheses
### Description: from the abstract below: 

# load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(viridis)
library(plotly)
library(ggmap)
library(writexl)
library(dplyr)
library(data.table)
library(hrbrthemes)

# data inquiry ------------------------------------------------------------

BioTimeData <- read.csv("data/other/BioTIMEQuery_24_06_2021.csv")
BioTimeMetadata <- read.csv("data/other/BioTIMEMetadata_24_06_2021.csv")

BioTimeFishplusInverts <- BioTimeMetadata %>%
  filter(TAXA == "Fish" | TAXA == "Marine invertebrates") #isolating fish and marine invertevrate datasets

BioTimeFishplusInverts_MARINE <- BioTimeFishplusInverts %>%
  filter(REALM == "Marine") #isolating marine studies

write_xlsx(BioTimeFishplusInverts_MARINE, "BioTimeFishplusInverts_MARINE.xlsx") #generation of excel file to easily share with other PIs
