###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): CV trend: AR time series models using restricted maximum likelihood
###date(s): May 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, forecast, tseries, remotePARTS, ggpubr)

### read in csv/xlsx files
exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")
add <- read_csv("local_data/timeseries_axis_titles.csv")

###########################################################################
# Data Wrangling/Manipulation ---------------------------------------------
###########################################################################

dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, climate, latitude, site, year, month, vert, everything()) |> 
  select(-n_spp)

na_count_per_column <- sapply(dt, function(x) sum(is.na(x)))
print(na_count_per_column) 
dt1 <- na.omit(dt)

### summarize all sites measured within the dataset annualy
model_dt <- dt1 |> 
  #recode NGA habitats per online resource mentioned in 'site-time series' script
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
###########################################################################
# Pull out time series with >10 years of obs ------------------------------
###########################################################################
dat_ts <- dat |> 
  group_by(axis_name_5, ecosystem_2, vert, site) |> 
  mutate(count = n()) |> 
  ungroup() |> 
  ### literature recommends 10 years for statistical power
  ### White, 2019; BioScience https://quantmarineecolab.github.io/pdfs/2019_White_Bioscience.pdf
  filter(count > 10) |> 
  # group_by(vert) |> 
  group_by(axis_name_5, ecosystem_2, vert, site) |>
  mutate(z_mean_n = scale(mean_n, center = TRUE, scale = TRUE),
         z_mean_p = scale(mean_p, center = TRUE, scale = TRUE),
         z_mean_bm = scale(mean_bm, center = TRUE, scale = TRUE),
         log_z_mean_n = scale(log1p(mean_n), center = TRUE, scale = TRUE),
         log_z_mean_p = scale(log1p(mean_p), center = TRUE, scale = TRUE),
         log_z_mean_bm = scale(log1p(mean_bm), center = TRUE, scale = TRUE),
         log_cv_mean_n = log1p(cv_n),
         log_cv_mean_p = log1p(cv_p),
         log_cv_mean_bm = log1p(cv_bm)) |> 
  ungroup()

glimpse(dat_ts)
#############################################################################
# AR Modeling in remotePARTS ------------------------------------------------
#############################################################################
### see Crossley et al 2020 for methodology
### https://par.nsf.gov/servlets/purl/10301082#page=1.47

### reclassify key columns as.factor()
dat_ts$axis_name_5 <- as.factor(dat_ts$axis_name_5)
dat_ts$ecosystem_2 <- as.factor(dat_ts$ecosystem_2)
dat_ts$vert <- as.factor(dat_ts$vert)
dat_ts$habitat <- as.factor(dat_ts$habitat)
dat_ts$site <- as.factor(dat_ts$site)

### fit AR model across all individual sites
ar_results <- dat_ts |> 
  group_by(axis_name_5, ecosystem_2, vert, habitat, site) |> 
  do(model = fitAR(.data$log_cv_mean_n~.data$year))

### pull coefficients of interest out of model
ar_trends <- ar_results |> 
  rowwise() |> 
  mutate(trend = model$coefficients[".data$year"],  
         trend_se = model$SE[".data$year"],
         trend_pval = model$pval[".data$year"]) |> 
  ungroup() |> 
  select(-model) 
###########################################################################
# Plots trends by research program ----------------------------------------
###########################################################################

### set up new factor for plotting by Program-Habitat
pdat <- ar_trends |> 
  mutate(group = paste(axis_name_5,habitat),
         group = as.factor(group))

ecosystem_colors <- c("Coastal" = "#7fcdff", "Pelagic" = "#064273", "Estuarine" = "#76b6c4")

a <- pdat |> 
  rename(Ecosystem = ecosystem_2) |> 
  filter(vert == "vertebrate") |> 
  ggplot(aes(x = axis_name_5, y = trend, fill = Ecosystem)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  # geom_violin(trim = FALSE, alpha = 1.0) +  # Draw violin plots with some transparency
  geom_boxplot() +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +  # Diamond shape for medians
  scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
  labs(y = "Aggregate Nitrogen Supply Rate Trend") +
  scale_y_continuous(limits = c(-0.04,0.04), breaks = c(-0.03,-0.02,-0.01,0,0.01,0.02,0.03)) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "none",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

b <- pdat |> 
  rename(Ecosystem = ecosystem_2) |> 
  filter(vert == "invertebrate") |> 
  ggplot(aes(x = axis_name_5, y = trend, fill = Ecosystem)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  # geom_violin(trim = FALSE, alpha = 1.0) +  # Draw violin plots with some transparency
  geom_boxplot() +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +  # Diamond shape for medians
  scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
  labs(y = "Aggregate Nitrogen Supply Rate Trend") +
  scale_y_continuous(limits = c(-0.04,0.04), breaks = c(-0.03,-0.02,-0.01,0,0.01,0.02,0.03)) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "none",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

### plot of supply trends by community
ggarrange(a, b,
          labels = c('a)','b)'),
          ncol = 2, vjust = 1, align = "h")

# saving for publication
# ggsave("output/ms first round/plots/combined_nitrogen_supply_CV_trend_boxplot.tiff", units = "in", width = 10,
#        height = 6, dpi =  600, compression = "lzw")
