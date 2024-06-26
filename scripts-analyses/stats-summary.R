library(librarian)
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, ggpmisc, corrplot, performance, ggeffects, ggpubr, parameters)

###########################################################################
# summary stats from  harmonized dataset ----------------------------------
###########################################################################

all <- read_csv("local_data/harmonized_consumer_ready_for_excretion.csv")
species_list <- readxl::read_excel(path = file.path("tier1", "CNDWG_harmonized_consumer_species.xlsx"),na=".")

#### read data end 
  
#### calculate excretion rate

# take out the rows that are needed 

df1 <- all %>%
  filter(measurement_type == "count") 

### pivot_wider indicates duplicates and generating lists instead of columns in dbl
### check for replicates in data
df1_replicates <- df1 |> 
  group_by(across(everything())) |> 
  filter(n() > 1) |> 
  ungroup()

unique(df1_replicates$project) #[1] "CoastalCA" "SBC"  
unique(df1_replicates$habitat) #[1] "ocean"

df1_replicates_distinct <- df1_replicates |> 
  distinct()
### appears that everything for for sbc-beach and CoastalCA are duplicated
### df1_replicates = 4380336 obs.
### df1_replicates_distinct = 2190168 obs.
### > 2190168*2 = [1] 4380336

### get rid of duplicates - but check out data first to make sure we aren't 
### kicking anything we shouldn't

rep <- mean(df1_replicates$measurement_value)
rep_distinct <- mean(df1_replicates_distinct$measurement_value)

df2 <- df1 |> 
  distinct()

df3 <- df2 |> 
  filter(!project %in% c("SBC", "NGA", "CCE")) |> 
  mutate(value = as.numeric(measurement_value)) |> 
  filter(value > 0) |> 
  filter(sp_code != "BAITBALL")

test <- df3 |> 
  filter(project == "PIE")

df4 <- df3 |> 
  filter(!scientific_name %in% c("Carcinus maenas",
                                 "Carcinus septemspinosa",
                                 "Palaemon pugio",
                                 "Uca pugnax",
                                 "Xanthidae"))

write_csv(df4, "../../../../../../Downloads/ind_counts_minus_sbc.csv")
unique(df4$scientific_name)
# fix nas that dont make sense --------------------------------------------

# check to see anything that don't have diet cat column
# peace3<-df4 %>%
#   filter(is.na(diet_cat)) %>%
#   distinct(project,habitat,sp_code,scientific_name,species,diet_cat)
na_count_per_column <- sapply(df4, function(x) sum(is.na(x)))
print(na_count_per_column)

### examine small # of data w missing scientific name column
value_nas <- df4 |> 
  filter(is.na(scientific_name)) #all from PISCO and are anchovies/sardines

### fill NAs in scientific_name with order 'Clupeiformes' since most resolved portion of shared taxonomy

df5 <- df4 |> 
  mutate(scientific_name = ifelse(is.na(scientific_name), "Clupeiformes", scientific_name))

na_count_per_column <- sapply(df5, function(x) sum(is.na(x)))
print(na_count_per_column)

### examine data without family classification

## FCE 
value_nas <- df5 |> 
  filter(is.na(family)) |> 
  filter(project == "FCE") 
unique(value_nas$scientific_name)

## VCR
value_nas <- df5 |> 
  filter(is.na(family)) |> 
  filter(project == "VCR") 
unique(value_nas$scientific_name)

## CoastalCA
value_nas <- df5 |> 
  filter(is.na(family)) |> 
  filter(project == "CoastalCA") 
unique(value_nas$scientific_name)

### tidy up environment
rm(df, df1, df2, df3, df5, df1_replicates, 
   df1_replicates_distinct, value_nas, na_count_per_column,
   rep, rep_distinct)


inds <- all |>
  filter(measurement_type == "count") |> 
  mutate(measurement_value = as.numeric(measurement_value)) |> 
  summarize(sum = sum(measurement_value, na.rm = TRUE)) #2,864,931 individuals
###########################################################################
# summary stats from  excretion dataset -----------------------------------
###########################################################################

all_exc <- read_csv("local_data/harmonized_consumer_excretion_CLEAN.csv")
colnames(all_exc) 
unique(all_exc$year)#years 1999-2023
unique(all_exc$family)#242 families
unique(all_exc$scientific_name)#894 species

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
test <- all_exc_filtered |> filter(project == "CoastalCA") |> 
  group_by(site) |> 
  summarize(n = n_distinct(scientific_name),
            min = min(year),
            max = max(year),
            years = max-min)
test1 <- all_exc_filtered |> filter(project == "NGA") |> 
  summarize(n = n_distinct(sp_code))

site_species <- all_exc_filtered |> 
  group_by(project, habitat) |> 
  summarize(species = n_distinct(scientific_name))



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

