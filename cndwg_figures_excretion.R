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

glimpse(fce_all_dm)

### calculate n supply

cons <- fce_all_dm

cons_n <- cons %>% 
  mutate(N_vert_coef = if_else(phylum == "Chordata", 0.7804, 0),
         N_diet_coef = if_else(diet_cat == "algae_detritus", -0.0389,
                                              if_else(diet_cat == "invert", -0.2013,
                                                      if_else(diet_cat == "fish", -0.0537,
                                                              if_else(diet_cat == "fish_invert", -0.1732, 
                                                                      if_else(diet_cat == "algae_invert", 0,
                                                                              NA))))),
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = if_else(Nexc_log10 > 0, Nexc_log10, 0))

glimpse(cons_n)
  
n_cpue <- cons_n |> 
  group_by(common_name, hydroyear) |> 
  mutate(sum_n = sum(Nexc_log10),
            sum_dist = sum(distance),
            n_cpue_100 = (sum_n/sum_dist)*100,
            n_cpue_m = (n_cpue_100/100))

n_cpue_trophic <- n_cpue |> 
  group_by(hydroyear, diet_cat) |> 
  summarise(n_supp_m = sum(n_cpue_m)) |> 
  filter(complete.cases(diet_cat,
                        hydroyear)) |> 
  filter(hydroyear != "2021-2019" & hydroyear != "2021-2021" & hydroyear != "2021-2020")
  
glimpse(n_cpue_trophic)

n_cpue_total <- n_cpue_trophic |> 
  group_by(hydroyear) |> 
  summarise(n_supp_m = sum(n_supp_m))

### produce nitrogen figures
ggplot(n_cpue_trophic, aes(fill = diet_cat, x=hydroyear, y = as.numeric(n_supp_m))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Nitrogen Supply Rate (log10(μg/m/h))") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") +
  ggtitle("Calculate w/ Global Average Temperature (23.6 C)")

# ggsave(filename='plots/fce_mean_annual_nsupply_01242024_globalavgtemp.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

ggplot(n_cpue_trophic, aes(fill = diet_cat, x=hydroyear, y = as.numeric(n_supp_m))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Percent Annual Nitrogen Supply Rate (log10(μg/m/h))") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") +
  ggtitle("Calculate w/ Global Average Temperature (23.6 C)")

# ggsave(filename='plots/fce_percent_annual_nsupply_01242024_globalavgtemp.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### bring in temp for mean annual
temp <- map_all_weight_clean |>
  group_by(hydroyear) |> 
  summarise(avgtemp = mean(temp_c)) |> 
  filter(complete.cases(hydroyear)) |> 
  filter(hydroyear != "2021-2019" & hydroyear != "2021-2021" & hydroyear != "2021-2020")

temp$avgtemp[is.na(temp$avgtemp)] <- 23.6
glimpse(temp)

cons1 <- fce_all_dm

cons1 <- left_join(cons1, temp, by = "hydroyear")
glimpse(cons1)
unique(cons1$avgtemp.y)

cons1 <- cons1 |> 
  rename(avgtemp = avgtemp.y)
cons1$avgtemp[is.na(cons1$avgtemp)] <- 23.6
unique(cons1$avgtemp)

cons1 <- cons1 %>% 
  mutate(N_vert_coef = if_else(phylum == "Chordata", 0.7804, 0),
         N_diet_coef = if_else(diet_cat == "algae_detritus", -0.0389,
                               if_else(diet_cat == "invert", -0.2013,
                                       if_else(diet_cat == "fish", -0.0537,
                                               if_else(diet_cat == "fish_invert", -0.1732, 
                                                       if_else(diet_cat == "algae_invert", 0,
                                                               NA))))),
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = if_else(Nexc_log10 > 0, Nexc_log10, 0))

glimpse(cons1)

n_cpue1 <- cons1 |> 
  group_by(common_name, hydroyear) |> 
  mutate(sum_n = sum(Nexc_log10),
         sum_dist = sum(distance),
         n_cpue_100 = (sum_n/sum_dist)*100,
         n_cpue_m = (n_cpue_100/100))

n_cpue_trophic1 <- n_cpue1 |> 
  group_by(hydroyear, diet_cat) |> 
  summarise(n_supp_m = sum(n_cpue_m)) |> 
  filter(complete.cases(diet_cat,
                        hydroyear)) |> 
  filter(hydroyear != "2021-2019" & hydroyear != "2021-2021" & hydroyear != "2021-2020")

glimpse(n_cpue_trophic1)

n_cpue_total1 <- n_cpue_trophic1 |> 
  group_by(hydroyear) |> 
  summarise(n_supp_m = sum(n_supp_m))

### produce nitrogen figures
ggplot(n_cpue_trophic, aes(fill = diet_cat, x=hydroyear, y = as.numeric(n_supp_m))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Nitrogen Supply Rate (log10(μg/m/h))") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") +
  ggtitle("Calculate w/ Annual Average Temperature (23.6 C)")

# ggsave(filename='plots/fce_mean_annual_nsupply_01242024_annualavgtemp.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

ggplot(n_cpue_trophic, aes(fill = diet_cat, x=hydroyear, y = as.numeric(n_supp_m))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Percent Annual Nitrogen Supply Rate (log10(μg/m/h))") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") +
  ggtitle("Calculate w/ Annual Average Temperature (23.6 C)")

# ggsave(filename='plots/fce_percent_annual_nsupply_01242024_annualavgtemp.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# try with huge temp  -----------------------------------------------------

cons2 <- fce_all_dm |> 
  select(-avgtemp) |> 
  mutate(avgtemp = 53.6)

cons2 <- cons2 %>% 
  mutate(N_vert_coef = if_else(phylum == "Chordata", 0.7804, 0),
         N_diet_coef = if_else(diet_cat == "algae_detritus", -0.0389,
                               if_else(diet_cat == "invert", -0.2013,
                                       if_else(diet_cat == "fish", -0.0537,
                                               if_else(diet_cat == "fish_invert", -0.1732, 
                                                       if_else(diet_cat == "algae_invert", 0,
                                                               NA))))),
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = if_else(Nexc_log10 > 0, Nexc_log10, 0))

glimpse(cons2)

n_cpue2 <- cons2 |> 
  group_by(common_name, hydroyear) |> 
  mutate(sum_n = sum(Nexc_log10),
         sum_dist = sum(distance),
         n_cpue_100 = (sum_n/sum_dist)*100,
         n_cpue_m = (n_cpue_100/100))

n_cpue_trophic2 <- n_cpue2 |> 
  group_by(hydroyear, diet_cat) |> 
  summarise(n_supp_m = sum(n_cpue_m)) |> 
  filter(complete.cases(diet_cat,
                        hydroyear)) |> 
  filter(hydroyear != "2021-2019" & hydroyear != "2021-2021" & hydroyear != "2021-2020")

glimpse(n_cpue_trophic2)

n_cpue_total2 <- n_cpue_trophic2 |> 
  group_by(hydroyear) |> 
  summarise(n_supp_m = sum(n_supp_m))

### produce nitrogen figures
ggplot(n_cpue_trophic2, aes(fill = diet_cat, x=hydroyear, y = as.numeric(n_supp_m))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Nitrogen Supply Rate (log10(μg/m/h))") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") +
  ggtitle("Calculate w/ Annual Average Temperature (53.6 C)")

# ggsave(filename='plots/fce_mean_annual_nsupply_01242024_annualavgtemp_tempinflated.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

ggplot(n_cpue_trophic2, aes(fill = diet_cat, x=hydroyear, y = as.numeric(n_supp_m))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Percent Annual Nitrogen Supply Rate (log10(μg/m/h))") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") +
  ggtitle("Calculate w/ Annual Average Temperature (53.6 C)")

# ggsave(filename='plots/fce_percent_annual_nsupply_01242024_annualavgtemp_tempinflated.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# inflate biomass, not temperature
### calculate n supply

cons3 <- fce_all_dm |> 
  mutate(drymass_g = drymass_g*0.44+drymass_g,
         avgtemp = 23.6)

cons3 <- cons3 %>% 
  mutate(N_vert_coef = if_else(phylum == "Chordata", 0.7804, 0),
         N_diet_coef = if_else(diet_cat == "algae_detritus", -0.0389,
                               if_else(diet_cat == "invert", -0.2013,
                                       if_else(diet_cat == "fish", -0.0537,
                                               if_else(diet_cat == "fish_invert", -0.1732, 
                                                       if_else(diet_cat == "algae_invert", 0,
                                                               NA))))),
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = if_else(Nexc_log10 > 0, Nexc_log10, 0))

glimpse(cons3)

n_cpue3 <- cons3 |> 
  group_by(common_name, hydroyear) |> 
  mutate(sum_n = sum(Nexc_log10),
         sum_dist = sum(distance),
         n_cpue_100 = (sum_n/sum_dist)*100,
         n_cpue_m = (n_cpue_100/100))

n_cpue_trophic3 <- n_cpue3 |> 
  group_by(hydroyear, diet_cat) |> 
  summarise(n_supp_m = sum(n_cpue_m)) |> 
  filter(complete.cases(diet_cat,
                        hydroyear)) |> 
  filter(hydroyear != "2021-2019" & hydroyear != "2021-2021" & hydroyear != "2021-2020")

glimpse(n_cpue_trophic3)

n_cpue_total3 <- n_cpue_trophic3 |> 
  group_by(hydroyear) |> 
  summarise(n_supp_m = sum(n_supp_m))

### produce nitrogen figures
ggplot(n_cpue_trophic3, aes(fill = diet_cat, x=hydroyear, y = as.numeric(n_supp_m))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Nitrogen Supply Rate (log10(μg/m/h))") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") +
  ggtitle("Calculate w/ Global Average Temperature (23.6 C)")

# ggsave(filename='plots/fce_mean_annual_nsupply_01242024_globalavgtemp_biomassinflated.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

ggplot(n_cpue_trophic3, aes(fill = diet_cat, x=hydroyear, y = as.numeric(n_supp_m))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Percent Annual Nitrogen Supply Rate (log10(μg/m/h))") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") +
  ggtitle("Calculate w/ Global Average Temperature (23.6 C)")

# ggsave(filename='plots/fce_percent_annual_nsupply_01242024_globalavgtemp.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)


# deflate biomass, not temperature
### calculate n supply

cons4 <- fce_all_dm |> 
  mutate(drymass_g = drymass_g-(drymass_g*0.44),
         avgtemp = 23.6)

cons4 <- cons4 %>% 
  mutate(N_vert_coef = if_else(phylum == "Chordata", 0.7804, 0),
         N_diet_coef = if_else(diet_cat == "algae_detritus", -0.0389,
                               if_else(diet_cat == "invert", -0.2013,
                                       if_else(diet_cat == "fish", -0.0537,
                                               if_else(diet_cat == "fish_invert", -0.1732, 
                                                       if_else(diet_cat == "algae_invert", 0,
                                                               NA))))),
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = if_else(Nexc_log10 > 0, Nexc_log10, 0))

glimpse(cons4)

n_cpue4 <- cons4 |> 
  group_by(common_name, hydroyear) |> 
  mutate(sum_n = sum(Nexc_log10),
         sum_dist = sum(distance),
         n_cpue_100 = (sum_n/sum_dist)*100,
         n_cpue_m = (n_cpue_100/100))

n_cpue_trophic4 <- n_cpue4 |> 
  group_by(hydroyear, diet_cat) |> 
  summarise(n_supp_m = sum(n_cpue_m)) |> 
  filter(complete.cases(diet_cat,
                        hydroyear)) |> 
  filter(hydroyear != "2021-2019" & hydroyear != "2021-2021" & hydroyear != "2021-2020")

glimpse(n_cpue_trophic4)

n_cpue_total4 <- n_cpue_trophic4 |> 
  group_by(hydroyear) |> 
  summarise(n_supp_m = sum(n_supp_m))

### produce nitrogen figures
ggplot(n_cpue_trophic4, aes(fill = diet_cat, x=hydroyear, y = as.numeric(n_supp_m))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Nitrogen Supply Rate (log10(μg/m/h))") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") +
  ggtitle("Calculate w/ Global Average Temperature (23.6 C)")

# ggsave(filename='plots/fce_mean_annual_nsupply_01242024_globalavgtemp_biomassdeflated.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# deflate temp, not biomass
### calculate n supply

cons5 <- fce_all_dm |> 
  mutate(avgtemp= 3.6)

cons5 <- cons5 %>% 
  mutate(N_vert_coef = if_else(phylum == "Chordata", 0.7804, 0),
         N_diet_coef = if_else(diet_cat == "algae_detritus", -0.0389,
                               if_else(diet_cat == "invert", -0.2013,
                                       if_else(diet_cat == "fish", -0.0537,
                                               if_else(diet_cat == "fish_invert", -0.1732, 
                                                       if_else(diet_cat == "algae_invert", 0,
                                                               NA))))),
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = 1.461 + 0.6840*(log10(drymass_g)) + 0.0246*avgtemp + N_diet_coef + N_vert_coef,
         Nexc_log10 = if_else(Nexc_log10 > 0, Nexc_log10, 0))

glimpse(cons5)

n_cpue5 <- cons5 |> 
  group_by(common_name, hydroyear) |> 
  mutate(sum_n = sum(Nexc_log10),
         sum_dist = sum(distance),
         n_cpue_100 = (sum_n/sum_dist)*100,
         n_cpue_m = (n_cpue_100/100))

n_cpue_trophic5 <- n_cpue5 |> 
  group_by(hydroyear, diet_cat) |> 
  summarise(n_supp_m = sum(n_cpue_m)) |> 
  filter(complete.cases(diet_cat,
                        hydroyear)) |> 
  filter(hydroyear != "2021-2019" & hydroyear != "2021-2021" & hydroyear != "2021-2020")

glimpse(n_cpue_trophic5)

n_cpue_total5 <- n_cpue_trophic5 |> 
  group_by(hydroyear) |> 
  summarise(n_supp_m = sum(n_supp_m))

### produce nitrogen figures
ggplot(n_cpue_trophic5, aes(fill = diet_cat, x=hydroyear, y = as.numeric(n_supp_m))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Nitrogen Supply Rate (log10(μg/m/h))") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") +
  ggtitle("Calculate w/ Global Average Temperature (3.6 C)")

ggsave(filename='plots/fce_mean_annual_nsupply_01242024_globalavgtemp_tempdeflated.png',
       plot = last_plot(),
       scale = 2.5,
       width = 7,
       height = 5,
       units = c("cm"),
       dpi = 300)
