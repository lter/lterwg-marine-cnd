###########################################################################
# Project: CND Synthesis Working Group
# Author: Mack White - unless it doesn't work
# Goal: Generate figures for CND synthesis in-person meeting
# Date(s): January 22 - 25, 2024
# Additional Authors:
###########################################################################

# housekeeping ------------------------------------------------------------

# install.packages(librarian)
librarian::shelf(readr, tidyverse, googledrive, readxl, 
                 taxize, stringr, lubridate, scales, ggeffects,
                 ggplot2)

# cons <- read_csv("data/harmonized_consumer_20240122.csv")
glimpse(cons)

###########################################################################
# FIGURE ONE: BIOMASS -----------------------------------------------------
###########################################################################

# SBC - Aquatic Dataset ---------------------------------------------------

# section leads: place name here
sbc_aqua <- cons |> 
  filter(raw_filename == "Annual_All_Species_Biomass_at_transect_20230814.csv", #sort out your dataset using the raw filename
         measurement_type == "wetmass") #if you have multiple measurements for biomass, use wet mass for Figure One

### setting up data for biomass figure generation

sbc_summ <- sbc_aqua |> #setting up new dataframe with summarized dat
  group_by(year,site) |> #global biomass est for each of the diet_cat
  summarize(mean_bm = sum(measurement_value)/n()) #calculate mean biomass for each diet_cat for "project"

### generation of global biomass estimate per project per diet category

ggplot(sbc_summ, aes(fill = diet_cat, x=year, y = as.numeric(mean_bm))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", 
       y = "Mean Annual Biomass (g/m2)") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top")

### save your plot below, but make sure to rename 
### highlight text below, then (command +shift + c) to run code

# ggsave(filename='plots/figure_one/sbc_aqua_mean_bm',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 8,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### generation of global biomass percentage per project per diet category

ggplot(sbc_summ, aes(fill = diet_cat, x=year, y = as.numeric(mean_bm))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Year", 
       y = "Mean Annual Biomass (g/m2)") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top")

# ggsave(filename='plots/figure_one/sbc_aqua_percent_bm',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 8,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# FCE data ----------------------------------------------------------------

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

map_cpue <- result |> 
  group_by(hydroyear, common_name) |>
  mutate(weight_g = weight*1000) |> 
  summarise(sumwt = sum(weight_g),
            sumdist = sum(distance),
            bm_cpue_100 = (sumwt/sumdist)*100,
            bm_cpue_m = (bm_cpue_100/100))

map_join <- left_join(map_cpue, diet_tax, by = "common_name")
glimpse(map_join)  

map <- map_join |> 
  group_by(hydroyear, diet_cat) |> 
  summarise(bm = sum(bm_cpue_m)) |> 
  filter(complete.cases(diet_cat,
                        hydroyear)) |> 
  filter(hydroyear != "2021-2019" & hydroyear != "2021-2021" & hydroyear != "2021-2020")

glimpse(map)

ggplot(map, aes(fill = diet_cat, x=hydroyear, y = as.numeric(bm))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Biomass (g/m)") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top")

# ggsave(filename='plots/fce_all_meanannual_biomass_01222024.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

ggplot(map, aes(fill = diet_cat, x=hydroyear, 
                           y = as.numeric(bm))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Biomass (% of total)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") 

# ggsave(filename='plots/fce_all_percent_biomass_01222024.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)


# PIE calculations and figure one -----------------------------------------

cons <- read_csv("data/LTE-TIDE-NektonFlumeIndividual_v6_3.csv") |> 
  rename(sp_code = Species)
spcode <- read_csv("data/PIE_speciescode_table.csv")

pie <- left_join(cons, spcode, by = "sp_code")

diet <- read_csv("data/diet_tax.csv") |> 
  filter(project == "PIE")

pie <- left_join(pie, diet, by = "common_name") |> 
  janitor::clean_names() |> 
  filter(year >=1901) # for some reason, odd year of 1900... removed

na_weight <- which(is.na(pie$weight)) #missing 20870 out of 66093 obs

# Group by 'Grouping_Column' and calculate the average value for each group
avg_weight <- pie %>%
  group_by(common_name) %>%
  mutate(avg_weight = mean(weight, na.rm = TRUE))
glimpse(avg_weight)

# Replace NAs with the corresponding group's average value
pie_weight <- avg_weight %>%
  mutate(weight = ifelse(is.na(weight), 
                         avg_weight, 
                         weight))
glimpse(pie_weight)

na_weight_clean <- which(is.na(pie_weight$weight)) #no more NAs

all_combinations <- expand.grid(common_name = unique(pie_weight$common_name),
                                year = unique(pie_weight$year),
                                creek = unique(pie_weight$creek),
                                branch = unique(pie_weight$branch),
                                replicate = unique(pie_weight$replicate))

result <- merge(all_combinations, pie_weight, 
                by = c("common_name", "year", "creek", "branch",
                       "replicate"), all.x = TRUE)

glimpse(result)

na_number_of_fish_result <- which(is.na(result$number_of_fish))
result$number_of_fish[is.na(result$number_of_fish)] <- 0
na_number_of_fish_result_zero <- which(is.na(result$number_of_fish)) #NAs for catch changed to zeros

na_weight_result <- which(is.na(result$weight))
result$weight[is.na(result$weight)] <- 0
na_weight_result_zero <- which(is.na(result$weight)) #Nas replaced with zeros

glimpse(result)

result_clean <- result |> 
  select(date, common_name, year, branch, replicate, number_of_fish, length,
         weight, mean_area, scientific_name, kingdom, phylum, class, order, 
         family, genus, diet_cat_x) |> 
  rename(diet_cat = diet_cat_x) |> 
  group_by(year) |> 
  mutate(transect_area = mean(mean_area, na.rm = TRUE)) |> 
  select(-mean_area)
  
glimpse(result_clean)

# na_area_result <- which(is.na(result_clean$transect_area))

pie_cpue <- result_clean |> 
  group_by(year, common_name) |>
  # mutate(weight_g = weight*1000) |> 
  summarise(sumwt = sum(weight),
            sumdist = sum(transect_area),
            bm_cpue_m = (sumwt/sumdist))

pie_join <- left_join(pie_cpue, diet, by = "common_name")
glimpse(pie_join)  
unique(pie_join$year)

pie <- pie_join |> 
  group_by(year, diet_cat) |> 
  summarise(bm = sum(bm_cpue_m))

glimpse(pie)

ggplot(pie, aes(fill = diet_cat, x=year, y = as.numeric(bm))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", 
       y = "Mean Annual Biomass (g/m)") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top")

# ggsave(filename='plots/pie_all_meanannual_biomass_01222024.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

ggplot(pie, aes(fill = diet_cat, x=year, 
                y = as.numeric(bm))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Biomass (% of total)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") 

# ggsave(filename='plots/pie_all_percent_biomass_01222024.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# VCR calculations and figure ---------------------------------------------

cons <- read_csv("data/VCR14232_2.csv") |> 
  rename(common_name = speciesName)

diet <- read_csv("data/diet_tax.csv") |> 
  filter(project == "VCR")

vcr <- left_join(cons, diet, by = "common_name") |> 
  janitor::clean_names()
glimpse(vcr)
  
na_weight <- which(is.na(vcr$weight))#missing 84 out of 2417 obs

nofish <- vcr |> 
  filter(site_comment == "No fish collected") #51 events

# Group by 'Grouping_Column' and calculate the average value for each group
avg_weight <- vcr %>%
  group_by(common_name) %>%
  mutate(avg_weight = mean(weight, na.rm = TRUE))
glimpse(avg_weight)

# Replace NAs with the corresponding group's average value
vcr_weight <- avg_weight %>%
  mutate(weight = ifelse(is.na(weight), 
                         avg_weight, 
                         weight))
glimpse(vcr_weight)

na_weight_clean <- which(is.na(vcr_weight$weight))
vcr_weight$weight[is.na(vcr_weight$weight)] <- 0
na_weight_clean <- which(is.na(vcr_weight$weight))

vcr_weight$date <- mdy(vcr_weight$sample_date)
glimpse(vcr_weight)

vcr_weight$year <- year(vcr_weight$date)
vcr_weight$month <- month(vcr_weight$date)
vcr_weight$day <- day(vcr_weight$date)
glimpse(vcr_weight)

vcr_weight_clean <- vcr_weight |> 
  select(year, month, site, common_name, length_cm, weight, count, 
         temperature, salinity, transect_area, scientific_name, kingdom, 
         phylum, class, order, family, genus, species, diet_cat)


all_combinations <- expand.grid(common_name = unique(vcr_weight_clean$common_name),
                                year = unique(vcr_weight_clean$year),
                                site = unique(vcr_weight_clean$site))

result <- merge(all_combinations, vcr_weight_clean, 
                by = c("common_name", "year", "site"), all.x = TRUE)

glimpse(result)

na_count_result <- which(is.na(result$count))
result$count[is.na(result$count)] <- 0
na_count_result_zero <- which(is.na(result$count)) #NAs for catch changed to zeros

na_weight_result <- which(is.na(result$weight))
result$weight[is.na(result$weight)] <- 0
na_weight_result_zero <- which(is.na(result$weight)) #Nas replaced with zeros

na_transectarea_result <- which(is.na(result$transect_area))
result$transect_area[is.na(result$transect_area)] <- 25
na_transectarea_result_zero <- which(is.na(result$transect_area)) #Nas replaced with zeros

glimpse(result)

vcr_cpue <- result |> 
  group_by(year, common_name) |>
  # mutate(weight_g = weight*1000) |> 
  summarise(sumwt = sum(weight),
            sumdist = sum(transect_area),
            bm_cpue_m2 = (sumwt/sumdist))

vcr_join <- left_join(vcr_cpue, diet, by = "common_name")
# glimpse(vcr_join)  
# unique(vcr_join$year)

vcr <- vcr_join |> 
  group_by(year, diet_cat) |> 
  summarise(bm = sum(bm_cpue_m2)) |> 
  filter(complete.cases(diet_cat))

glimpse(vcr)

ggplot(vcr, aes(fill = diet_cat, x=year, y = as.numeric(bm))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", 
       y = "Mean Annual Biomass (g/m2)") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top")

# ggsave(filename='plots/vcr_all_meanannual_biomass_01222024.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

ggplot(vcr, aes(fill = diet_cat, x=year, 
                y = as.numeric(bm))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Biomass (% of total)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") 

ggsave(filename='plots/vcr_all_percent_biomass_01222024.png',
       plot = last_plot(),
       scale = 2.5,
       width = 7,
       height = 5,
       units = c("cm"),
       dpi = 300)


###########################################################################
# FIGURE TWO: EXCRETION ---------------------------------------------------
###########################################################################