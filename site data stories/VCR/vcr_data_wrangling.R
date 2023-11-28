library(tidyverse)
library(readr)

vcr_dat <- read_csv("~/Downloads/vcr_2012thru2018_fish.csv")
fish_species <- as.data.frame(unique(vcr_dat$speciesName)) |> 
  mutate(a = NA, 
         b = NA)
write_csv(fish_species, "vcr_fish_species_lwr.csv")

