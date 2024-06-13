###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): simple linear model time series analysis
###date(s): June 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, forecast, tseries, remotePARTS)

exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")
add <- read_csv("local_data/timeseries_axis_titles.csv")

dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, climate, latitude, site, year, month, vert, everything()) |> 
  select(-n_spp)

na_count_per_column <- sapply(dt, function(x) sum(is.na(x)))
print(na_count_per_column) 
dt1 <- na.omit(dt)

### summarize all sites measured within the dataset annualy
model_dt <- dt1 |> 
  mutate(habitat = fct_recode(habitat, 
                              "Prince William Sound" = "Knight Island Passage",
                              "MPA" = "Marine Protected Area")) |> 
  filter(!habitat %in% c("Kodiak Island", "Middleton Island")) |> 
  group_by(project, ecosystem, vert, habitat, site, year) |> 
  summarize(mean_n = mean(total_nitrogen),
            cv_n = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            sd_n = sd(total_nitrogen, na.rm = TRUE),
            mean_p = mean(total_phosphorus),
            cv_p = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            sd_p = sd(total_phosphorus, na.rm = TRUE),
            mean_bm = mean(total_biomass),
            cv_bm = (sd(total_biomass, na.rm = TRUE) / mean(total_biomass, na.rm = TRUE)),
            sd_bm = sd(total_biomass, na.rm = TRUE)) |> 
  na.omit() |> 
  ungroup()

glimpse(model_dt)

dat <- left_join(model_dt, add, relationship = "many-to-many")

### filter out sites with less than 10 years of data
dat_ts <- dat |> 
  group_by(axis_name_5, ecosystem_2, vert, site) |> 
  mutate(count = n()) |> 
  ungroup() |> 
  filter(count > 10) |> 
  group_by(vert) |> 
  mutate(z_mean_n = scale(mean_n, center = TRUE, scale = TRUE),
         z_mean_p = scale(mean_p, center = TRUE, scale = TRUE),
         z_mean_bm = scale(mean_bm, center = TRUE, scale = TRUE),
         log_z_mean_n = scale(log1p(mean_n), center = TRUE, scale = TRUE),
         log_z_mean_p = scale(log1p(mean_p), center = TRUE, scale = TRUE),
         log_z_mean_bm = scale(log1p(mean_bm), center = TRUE, scale = TRUE)) |> 
  ungroup()

# write_csv(dat_ts, "local_data/annual_time_series_investigation.csv")
glimpse(dat_ts)

#############################################################################
# simple linear models with slope, p-value, r.squared -----------------------
#############################################################################

results <- dat_ts |> 
  group_by(axis_name_5, habitat, site, vert) |> 
  do({
    model <- lm(mean_n ~ year, data = .)
    data.frame(
      slope = coef(model)[2],                      
      p_value = summary(model)$coefficients[2,4],  
      r_squared = summary(model)$r.squared         
    )
  })

glimpse(results)

results_summary <- results |> 
  group_by(axis_name_5, habitat, vert) |> 
  summarize(mean_slope = mean(slope),
            cv_slope = (sd(slope) / mean(slope)),
            mean_pvalue = mean(p_value), 
            cv_pvalue = (sd(p_value) / mean(p_value)),
            mean_rsquared = mean(r_squared),
            cv_rsquared = (sd(r_squared) / mean(r_squared)))