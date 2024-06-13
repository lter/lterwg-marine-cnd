###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): nutrient supply stability correlation figure
###date(s): May 2024
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
  group_by(axis_name_5, ecosystem_2, vert, site) |> 
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
# ARMA/ARIMA models -------------------------------------------------------
#############################################################################

glimpse(dat_ts)

# https://par.nsf.gov/servlets/purl/10301082#page=1.47
# For each taxon time series, we estimated a temporal trend using an
# autoregressive model fit using restricted maximum likelihood52
# . Before fitting
# models, we scaled time such that the distance between consecutive years was
# equal and spanned 0 to 1, and we Z-transformed the natural log of species counts.
# Resulting trends can be interpreted as the change in species abundance in standard
# deviations per unit scaled time. Our autoregressive models also estimated the
# temporal autocorrelation coefficient, b, which was used to remove time series
# whose trends could not be well estimated due to high temporal autocorrelation.
# We filtered time series based on three levels of stringency in quality criteria, and
# examined whether our degree of filtering stringency altered median trends among
# LTER sites. Our relaxed criteria required at least four years of counts, one of which
# had to be non-zero (n = 5,328 out of 6,501 trends remained). Moderate criteria
# required at least 8 years of counts, of which 4 had to be non-zero (n = 2,266 trends
#                                                                     remained). Strict criteria required at least 15 years of counts, of which 10 had to
# be non-zero, and that temporal autocorrelation be <1 (n = 308 trends remained).
# Because LTER site median time trends were insensitive to filtering stringency by
# these criteria (Extended Data Fig. 3), we present results from the relaxed criteria
# that retained abundance time trends for the most taxa and that were most inclusive
# of large trends. We present overall patterns in time trends among LTERs and
# sample points within LTERs in terms of percentiles. Because Midwest farmland
# spanned several ecoregions, we further separate aphid abundance trends from this
# dataset by ecoregion (Extended Data Fig. 8). We use a one-sample T test (using
#                                                                          the t.test R function) to test whether mean trends among LTERs are different
# from zero at α = 5%. For this analysis, we grouped datasets by LTER (d.f. = 12)
# or site–taxa group (d.f. = 22) (Table 1). We note that no means were significantly
# different from zero at α = 5% (Supplementary Table 1). To represent trends in
# terms of net percent change per year, we regressed log10-transformed abundance
# on year (scaled between 0 and the length of the time series); because the slope of
# this regression represents proportional change, we calculated percent change as the
# slope multiplied by 100.

dat_ts$axis_name_5 <- as.factor(dat_ts$axis_name_5)
dat_ts$ecosystem_2 <- as.factor(dat_ts$ecosystem_2)
dat_ts$vert <- as.factor(dat_ts$vert)
dat_ts$habitat <- as.factor(dat_ts$habitat)
dat_ts$site <- as.factor(dat_ts$site)

# Group data and fit AR models
ar_results <- dat_ts |> 
  group_by(axis_name_5, ecosystem_2, vert, habitat, site) |> 
  do(model = fitAR(.data$log_z_mean_n~.data$year))

ar_trends <- ar_results |> 
  rowwise() |> 
  mutate(trend = model$coefficients[".data$year"],  
         trend_se = model$SE[".data$year"],
         trend_pval = model$pval[".data$year"]) |> 
  ungroup() |> 
  select(-model) 

test <- ar_trends |> 
  mutate(group = paste(axis_name_5,habitat),
         group = as.factor(group))

test |> 
  rename(Ecosystem = ecosystem_2) |> 
  filter(vert == "vertebrate") |> 
  ggplot(aes(x = group, y = trend, fill = Ecosystem)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  geom_violin(trim = FALSE, alpha = 1.0) +  # Draw violin plots with some transparency
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +  # Diamond shape for medians
  scale_fill_brewer(palette = "Set3") +  # Color palette
  labs(y = "Nitrogen Supply Trend") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "top",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

test |> 
  rename(Ecosystem = ecosystem_2) |> 
  filter(vert == "invertebrate") |> 
  ggplot(aes(x = group, y = trend, fill = Ecosystem)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  geom_violin(trim = FALSE, alpha = 1.0) +  # Draw violin plots with some transparency
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "darkgrey") +  # Diamond shape for medians
  scale_fill_brewer(palette = "Set3") +  # Color palette
  labs(y = "Nitrogen Supply Trend") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "top",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

# plots by research program -----------------------------------------------

test |> 
  rename(Ecosystem = ecosystem_2) |> 
  filter(vert == "vertebrate") |> 
  ggplot(aes(x = axis_name_5, y = trend, fill = Ecosystem)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  geom_violin(trim = FALSE, alpha = 1.0) +  # Draw violin plots with some transparency
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +  # Diamond shape for medians
  scale_fill_brewer(palette = "Set3") +  # Color palette
  labs(y = "Nitrogen Supply Trend") +
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "top",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

test |> 
  rename(Ecosystem = ecosystem_2) |> 
  filter(vert == "invertebrate") |> 
  ggplot(aes(x = axis_name_5, y = trend, fill = Ecosystem)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  geom_violin(trim = FALSE, alpha = 1.0) +  # Draw violin plots with some transparency
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "darkgrey") +  # Diamond shape for medians
  scale_fill_brewer(palette = "Set3") +  # Color palette
  labs(y = "Nitrogen Supply Trend") +
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "top",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

