###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): Community Metric Trends: AR time series models using restricted maximum likelihood
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
comm <- read_csv("local_data/community_data_filtered.csv") |> 
  select(-date)
add <- read_csv("local_data/timeseries_axis_titles.csv")

### join data for modeling with site characteristic information
dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, climate, latitude, site, year, month, vert, everything()) |> 
  filter(vert == "vertebrate") |> 
  ### if species richness is important in models, remove CCE & NGA as they are not taxonomically resolved species, but instead "groups"
  filter(!project %in% c("CCE", "NGA")) |>
  ### recalculated species richness in community dataset prep and wanted to ensure it matched up
  ### however, can remove now
  select(-n_spp, -date)

dt1 <- left_join(dt, comm, relationship = "many-to-many")

na_count_per_column <- sapply(dt1, function(x) sum(is.na(x)))
print(na_count_per_column) 

# testna <- dt1 |> 
#   filter(is.na(Species_Richness))
### these are instances where programs-sites observed zero fishes
### do we keep these or get rid of them? I initially got rid of them since we are interested
### in aspects of community but maybe should keep and make zeros?
### omit NAs - ran both ways and doesn't change the story at all, just the coefficients slightly
### the NAs make up ~1% of all data going into the models

# dt1[is.na(dt1)] <- 0
# dt2 <- dt1

dt2 <- na.omit(dt1)

### summarize all sites measured within the dataset annualy
model_dt <- dt2 |> 
  group_by(project, ecosystem, vert, habitat, site, year) |> 
  summarize(max_size_mean = mean(mean_max_size),
            biomass_mean = mean(total_biomass),
            species_richness_mean = mean(Species_Richness),
            family_richness_mean = mean(Family_Richness),
            species_diversity_mean = mean(Species_Inverse_Simpson_Diversity_Index),
            trophic_diversity_mean = mean(Trophic_Inverse_Simpson_Diversity_Index)) |> 
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
  mutate(log_z_mean_max_size = scale(log1p(max_size_mean), center = TRUE, scale = TRUE),
         log_z_mean_biomass = scale(log1p(biomass_mean), center = TRUE, scale = TRUE),
         log_z_mean_spp_rich = scale(log1p(species_richness_mean), center = TRUE, scale = TRUE),
         log_z_mean_fam_rich = scale(log1p(family_richness_mean), center = TRUE, scale = TRUE),
         log_z_mean_spp_div = scale(log1p(species_diversity_mean), center = TRUE, scale = TRUE),
         log_z_mean_troph_div = scale(log1p(trophic_diversity_mean), center = TRUE, scale = TRUE)) |> 
  ungroup()

glimpse(dat_ts)
unique(dat_ts$axis_name_5)

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
  do(model = fitAR(.data$log_z_mean_troph_div~.data$year))

### pull coefficients of interest out of model
ar_trends <- ar_results |> 
  rowwise() |> 
  mutate(trend = model$coefficients[".data$year"],  
         trend_se = model$SE[".data$year"],
         trend_pval = model$pval[".data$year"]) |> 
  ungroup() |> 
  select(-model) 
###########################################################################
# Plots magnitude trends by research program ----------------------------------------
###########################################################################

### set up new factor for plotting by Program-Habitat
pdat <- ar_trends |> 
  mutate(group = paste(axis_name_5,habitat),
         group = as.factor(group))

ecosystem_colors <- c("Coastal" = "#7fcdff", "Pelagic" = "#064273", "Estuarine" = "#76b6c4")

a <- pdat |> 
  rename(Ecosystem = ecosystem_2) |> 
  # filter(vert == "vertebrate") |> 
  ggplot(aes(x = axis_name_5, y = trend, fill = Ecosystem)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  # geom_violin(trim = FALSE, alpha = 1.0) +  # Draw violin plots with some transparency
  geom_boxplot() +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +  # Diamond shape for medians
  scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
  labs(y = "Trophic Diversity Trend") +
  scale_y_continuous(limits = c(-0.23,0.23), breaks = c(-0.2, -0.15, -0.1,-0.05,0,0.05,0.1,0.15,0.2))+
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "none",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

### fit AR model across all individual sites
ar_results <- dat_ts |> 
  group_by(axis_name_5, ecosystem_2, vert, habitat, site) |> 
  do(model = fitAR(.data$log_z_mean_spp_div~.data$year))

### pull coefficients of interest out of model
ar_trends <- ar_results |> 
  rowwise() |> 
  mutate(trend = model$coefficients[".data$year"],  
         trend_se = model$SE[".data$year"],
         trend_pval = model$pval[".data$year"]) |> 
  ungroup() |> 
  select(-model) 
###########################################################################
# Plots magnitude trends by research program ----------------------------------------
###########################################################################

### set up new factor for plotting by Program-Habitat
pdat <- ar_trends |> 
  mutate(group = paste(axis_name_5,habitat),
         group = as.factor(group))

b <- pdat |> 
  rename(Ecosystem = ecosystem_2) |> 
  # filter(vert == "invertebrate") |> 
  ggplot(aes(x = axis_name_5, y = trend, fill = Ecosystem)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  # geom_violin(trim = FALSE, alpha = 1.0) +  # Draw violin plots with some transparency
  geom_boxplot() +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +  # Diamond shape for medians
  scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
  labs(y = "Species Diversity Trend") +
  scale_y_continuous(limits = c(-0.23,0.23), breaks = c(-0.2, -0.15, -0.1,-0.05,0,0.05,0.1,0.15,0.2))+
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "none",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

### fit AR model across all individual sites
ar_results <- dat_ts |> 
  group_by(axis_name_5, ecosystem_2, vert, habitat, site) |> 
  do(model = fitAR(.data$log_z_mean_spp_rich~.data$year))

### pull coefficients of interest out of model
ar_trends <- ar_results |> 
  rowwise() |> 
  mutate(trend = model$coefficients[".data$year"],  
         trend_se = model$SE[".data$year"],
         trend_pval = model$pval[".data$year"]) |> 
  ungroup() |> 
  select(-model) 
###########################################################################
# Plots magnitude trends by research program ----------------------------------------
###########################################################################

### set up new factor for plotting by Program-Habitat
pdat <- ar_trends |> 
  mutate(group = paste(axis_name_5,habitat),
         group = as.factor(group))

c <- pdat |> 
  rename(Ecosystem = ecosystem_2) |> 
  # filter(vert == "invertebrate") |> 
  ggplot(aes(x = axis_name_5, y = trend, fill = Ecosystem)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  # geom_violin(trim = FALSE, alpha = 1.0) +  # Draw violin plots with some transparency
  geom_boxplot() +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +  # Diamond shape for medians
  scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
  labs(y = "Species Richness Trend") +
  scale_y_continuous(limits = c(-0.23,0.23), breaks = c(-0.2, -0.15, -0.1,-0.05,0,0.05,0.1,0.15,0.2))+
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "none",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

### fit AR model across all individual sites
ar_results <- dat_ts |> 
  group_by(axis_name_5, ecosystem_2, vert, habitat, site) |> 
  do(model = fitAR(.data$log_z_mean_fam_rich~.data$year))

### pull coefficients of interest out of model
ar_trends <- ar_results |> 
  rowwise() |> 
  mutate(trend = model$coefficients[".data$year"],  
         trend_se = model$SE[".data$year"],
         trend_pval = model$pval[".data$year"]) |> 
  ungroup() |> 
  select(-model) 
###########################################################################
# Plots magnitude trends by research program ----------------------------------------
###########################################################################

### set up new factor for plotting by Program-Habitat
pdat <- ar_trends |> 
  mutate(group = paste(axis_name_5,habitat),
         group = as.factor(group))

d <- pdat |> 
  rename(Ecosystem = ecosystem_2) |> 
  # filter(vert == "invertebrate") |> 
  ggplot(aes(x = axis_name_5, y = trend, fill = Ecosystem)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  # geom_violin(trim = FALSE, alpha = 1.0) +  # Draw violin plots with some transparency
  geom_boxplot() +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "black") +  # Diamond shape for medians
  scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
  labs(y = "Family Richness Trend") +
  scale_y_continuous(limits = c(-0.23,0.23), breaks = c(-0.2, -0.15, -0.1,-0.05,0,0.05,0.1,0.15,0.2))+
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "none",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

### plot of supply trends by community
ggarrange(a, b, c, d,
          labels = c('a)','b)','c)','d)'),
          ncol = 2, nrow = 2, align = "hv")

# saving for publication
# ggsave("output/ms first round/plots/supplemental_community_metrics.tiff", units = "in", width = 10,
#        height = 10, dpi =  600, compression = "lzw")
