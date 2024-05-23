# author: mack white
# project: cnd synthesis wg
# goal: calculating excretion
# date(s): January 2024

# install.packages(librarian)
librarian::shelf(readr, tidyverse, googledrive, readxl, 
                 taxize, stringr, lubridate, scales, ggeffects,
                 ggplot2)

map_data <- read_csv("data/MAP_years1thru19.csv")
glimpse(map_data)
diet_tax <- read_csv("data/diet_tax.csv") |> 
  filter(project == "FCE")
glimpse(diet_tax)

map_all <- left_join(map_data, diet_tax, by = "common_name") |> 
  select(s.yr, s.mo, s.day, HYDROYEAR, DRAINAGE, SITE, BOUT, Distance,
         TempC, CATCHNUMBER, WEIGHT, common_name, scientific_name,
         diet_cat) |> 
  janitor::clean_names()

map_all$s_yr <- as.factor(map_all$s_yr)
map_all$hydroyear <- as.factor(map_all$hydroyear)
map_all$s_mo <- as.factor(map_all$s_mo)
map_all$s_day <- as.factor(map_all$s_day)
map_all$drainage <- as.factor(map_all$drainage)
map_all$site <- as.factor(map_all$site)
map_all$bout <- as.factor(map_all$bout)

# Group by 'Grouping_Column' and calculate the average value for each group
avg_weight <- map_all %>%
  group_by(common_name) %>%
  mutate(avg_weight = mean(weight, na.rm = TRUE))
glimpse(avg_weight)

#missing a lot if we group by common_name and year (9316 out of 23927 observations)

# Replace NAs with the corresponding group's average value
map_all_weight <- avg_weight %>%
  mutate(weight = ifelse(is.na(weight), 
                         avg_weight, 
                         weight))
glimpse(map_all_weight)

na_weight <- which(is.na(map_all_weight$weight)) #missing 166 observations - not bad

map_all_weight_clean <- map_all_weight %>%
  filter(complete.cases(weight))

na_weight_clean <- which(is.na(map_all_weight_clean$weight)) #missing zero

### REVIEW BELOW - MAY JUST BE ABLE TO DO BY YEAR AND TROPHIC GROUP
all_combinations <- expand.grid(common_name = unique(map_all_weight_clean$common_name),
                                hydroyear = unique(map_all_weight_clean$hydroyear),
                                s_mo = unique(map_all_weight_clean$s_mo),
                                drainage = unique(map_all_weight_clean$drainage),
                                site = unique(map_all_weight_clean$site),
                                bout = unique(map_all_weight_clean$bout))

result <- merge(all_combinations, map_all_weight_clean, 
                by = c("common_name", "hydroyear", "s_mo", "drainage",
                       "site", "bout"), all.x = TRUE)

glimpse(result)

na_catch_result <- which(is.na(result$catchnumber))
result$catchnumber[is.na(result$catchnumber)] <- 0
na_catch_result_zero <- which(is.na(result$catchnumber)) #NAs for catch changed to zeros

na_weight_result <- which(is.na(result$weight))
result$weight[is.na(result$weight)] <- 0
na_weight_result_zero <- which(is.na(result$weight)) #Nas replaced with zeros

na_distance_result <- which(is.na(result$distance))
result$distance[is.na(result$distance)] <- 100
na_distance_result <- which(is.na(result$distance))

glimpse(result)

result_clean <- result %>%
  filter(complete.cases(bout))

map_temp <- result_clean |> 
  mutate(avgtemp = mean(temp_c, na.rm = TRUE),
         weight_g = weight*1000)

### convert to dry mass
dm_conv <- read_csv("data/dm_conversions_cndwg.csv") |> 
  select(-level) |> 
  filter(kingdom == "Animalia",
         dm_wm_mean < 1)

result_full <- left_join(map_temp, diet_tax, by = "common_name")

result_full <- result_full |> 
  rename(diet_cat = diet_cat.y) |> #renaming column rename from join
  select(common_name, hydroyear, s_mo, drainage, site, bout, distance,
         catchnumber, weight_g, kingdom, phylum, class, order, family, genus,
         species, diet_cat, avgtemp)

dm_coeff <- dm_conv |> 
  group_by(class) |> 
  summarise(dm_coeff= mean(dm_wm_mean))

result_full_dm <- left_join(result_full, dm_coeff, by = "class")
glimpse(result_full_dm)

fce_all_dm <- result_full_dm |> 
  mutate(dm_g_ind = weight_g*dm_coeff,
         density = catchnumber/distance) |> 
  rename(count = catchnumber)

fce_all_dm$hydroyear <- as.character(fce_all_dm$hydroyear)

glimpse(fce_all_dm)

fce_all_dm$hydroyear2 <- substring(fce_all_dm$hydroyear, first=1,last=4)

# write_csv(fce_all_dm, "data/map_revised01242024.csv")

check <- read_csv("data/map_revised01242024.csv")
glimpse(check)

diet_updated <- check |> 
  select(common_name, diet_cat) |> 
  distinct()