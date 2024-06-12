librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, ggpmisc, corrplot, performance, ggeffects, ggpubr, parameters)

###########################################################################
# summary stats from  harmonized dataset ----------------------------------
###########################################################################

all <- read_csv("local_data/harmonized_consumer_ready_for_excretion.csv")
colnames(all)
unique(all$year) #years 1999-2023
unique(all$scientific_name)#894 species

inds <- all |>
  filter(measurement_type == "count") |> 
  mutate(measurement_value = as.numeric(measurement_value)) |> 
  summarize(sum = sum(measurement_value, na.rm = TRUE)) #2,864,931 individuals

###########################################################################
# summary stats from  excretion dataset -----------------------------------
###########################################################################

all_exc <- read_csv("local_data/harmonized_consumer_excretion_CLEAN.csv")
colnames(all_exc)
unique(all_exc$family)#242 families

all_exc_filtered <- all_exc |> 
  group_by(project, habitat) |> 
  ### filtering out sharks and rays that are considered "biomass busters"
  mutate(mean_dmperind = mean(`dmperind_g/ind`, na.rm = TRUE),  
         sd_dmperind = sd(`dmperind_g/ind`, na.rm = TRUE),  
         lower_bound = mean_dmperind - 5 * sd_dmperind,  
         upper_bound = mean_dmperind + 5 * sd_dmperind,
         ### +/- 5 SD cutoff... rest of sharks and rays included
         outlier = `dmperind_g/ind` < lower_bound | `dmperind_g/ind` > upper_bound,
         sharkray = grepl("\\bshark\\b|\\bray\\b", common_name, ignore.case = TRUE),
         elasmo = class %in% c("Chondrichthyes", "Elasmobranchii")) |> 
  ungroup() |> 
  filter(!(outlier & sharkray & elasmo))

size_info <- all_exc_filtered|>
  # filter(measurement_type == "dmperind") |>
  # mutate(measurement_value = as.numeric(measurement_value)) |>
  filter(`mean_dmperind` > 0) |>
  summarize(min = min(mean_dmperind, na.rm = TRUE),
            max = max(mean_dmperind, na.rm = TRUE),
            percentile_25 = quantile(mean_dmperind, 0.25, na.rm = TRUE),
            percentile_75 = quantile(mean_dmperind, 0.75, na.rm = TRUE))

#min - 3.833611e-20 (Doliolidae @ NGA)
#max - 126518.4 (Muraenidae @ )
#25% - 1.954415
#75% - 40.97594

###########################################################################
# summary stats from summarized excretion dataset -------------------------
###########################################################################

all_exc_sum <- read_csv("local_data/harmonized_consumer_excretion_CLEAN_summarized.csv")
colnames(all_exc_sum)

exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")
add <- read_csv("local_data/timeseries_axis_titles.csv")

dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, climate, latitude, site, year, month, vert, everything()) |> 
  select(-n_spp)

dt1 <- left_join(dt, add)

na_count_per_column <- sapply(dt1, function(x) sum(is.na(x)))
print(na_count_per_column)

site_summary_stats <- dt1 |> 
  group_by(axis_name_5) |> 
  summarize(min = min(year),
            max = max(year),
            years = max-min,
            mean_n_ug_hr_area = mean(total_nitrogen),
            sd_n_ug_hr_area = sd(total_nitrogen),
            mean_p_ug_hr_area = mean(total_phosphorus),
            sd_p_ug_hr_area = sd(total_phosphorus),
            mean_bm_g_area = mean(total_biomass),
            sd_bm_g_area = sd(total_biomass),
            mean_n_ug_day_area = mean(total_nitrogen*24),
            sd_n_ug_day_area = sd(total_nitrogen*24),
            mean_p_ug_day_area = mean(total_phosphorus*24),
            sd_p_ug_day_area = sd(total_phosphorus*24),
            mean_n_g_day_area = mean_n_ug_day_area*1e-6,
            sd_n_g_day_area = sd_n_ug_day_area*1e-6,
            mean_p_g_day_area = mean_p_ug_day_area*1e-6,
            sd_p_g_day_area = sd_p_ug_day_area*1e-6)
write_csv(site_summary_stats, "output/ms first round/tables/some_site_summary_stats.csv")

test <- all |> 
  filter(project == "FCE")

