# author: mack white
# project: cnd synthesis working group
# goal: zero-filling data example
# date(s): january 2024

mcr <- read_csv("data/harmonized_consumer_20240122.csv") |> 
  filter(project == "MCR") 

### know you needed some extra steps based on the fork length issue somewhere in here
### ie count and wet mass per "size class" for each into column

### pretty much here, spreading each possible combination out - i.e., every species-year-site-subsite combination
all_combinations <- expand.grid(common_name = unique(mcr$common_name),
                                year = unique(mcr$year),
                                site = unique(mcr$site),
                                subsite_level1 = unique(mcr$subsite_level1))

### merge the above with the original dataset... gonna have a TON of NAs - going to be a giant dataset most likely
result <- merge(all_combinations, mcr, 
                by = c("common_name", "year", "site", "subsite_level1"), 
                all.x = TRUE)

glimpse(result)

na_count_result <- which(is.na(result$count)) #going to show the # of "new" NAs from zero-filling
result$count[is.na(result$count)] <- 0 #turns all those new NAs into zeros (i.e., caught zero of those species at the year-site-subsite_level1 grouping)
na_count_result_zero <- which(is.na(result$number_of_fish)) #NAs for catch changed to zeros - should be an empty list now

na_wetmass_result <- which(is.na(result$wetmass)) #same as above, but for wetmass
result$wetmass[is.na(result$wetmass)] <- 0 #same as above, but for wetmass
na_weight_result_zero <- which(is.na(result$weight)) #same as above, but for wetmass

glimpse(result)

result_clean <- result |>
  group_by(site) |> #group by whatever variable determines the transect area (think for you it is by site, correct?)
  mutate(area = mean(transect_area, na.rm = TRUE)) #lazy way of generating transect area for those sites which will now be listed as "NA" because of the zero filling

### go on to calculate biomass :)
