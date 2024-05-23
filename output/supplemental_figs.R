###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): nutrient supply stability models
###date(s): March-April 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, corrplot, performance, ggeffects, sjlabelled)

exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")

### join data for modeling with site characteristic information
dt <- left_join(exc, sc) |> 
  dplyr::select(project, ecosystem, ocean, biome, site, year, month, vert, everything()) |> 
  # filter(vert == "vertebrate") |> 
  ### if species richness is important in models, remove CCE & NGA as they are not taxonomically resolved species, but instead "groups"
  # filter(!project %in% c("CCE", "NGA")) |> 
  ### recalculated species richness in community dataset prep and wanted to ensure it matched up
  ### however, can remove now
  dplyr::select(-n_spp)

# ### read in community metrics for analysis
# comm <- read_csv("local_data/community_data_filtered.csv")
# 
# dt1 <- left_join(dt, comm, relationship = "many-to-many")
# 
# na_count_per_column <- sapply(dt, function(x) sum(is.na(x)))
# print(na_count_per_column)
### 195 NAs within community dataset - because I removed sites instances where nothing was caught
### set community metrics to zero here, since it represents periods of time where nothing was collected

# dt2 <- dt1 %>%
#   mutate(across(everything(), ~replace(., is.na(.), 0)))
# 
# na_count_per_column <- sapply(dt2, function(x) sum(is.na(x)))
# print(na_count_per_column) 

### summarize all sites measured within the dataset annualy
model_dt <- dt |> 
  group_by(project, vert, ecosystem, ocean, biome, strata, site) |> 
  summarize(mean_n = mean(total_nitrogen),
            ### calculating coefficient of variation for nutrient supply across time for each site
            cv_n = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            ### calculating stability of nutrient supply (i.e., 1/cv)
            n_stability = 1/cv_n,
            mean_p = mean(total_phosphorus),
            ### calculating coefficient of variation for nutrient supply across time for each site
            cv_p = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            ### calculating stability of nutrient supply (i.e., 1/cv)
            p_stability = 1/cv_p,
            mean_bm = mean(total_biomass),
            ### calculating coefficient of variation for biomass across time for each site
            cv_bm = (sd(total_biomass, na.rm = TRUE) / mean(total_biomass, na.rm = TRUE)),
            ### calculating stability of biomass (i.e., 1/cv)
            bm_stability = 1/cv_bm,
            min_ss = mean(mean_min_size),
            mean_ss = mean(mean_mean_size),
            max_ss = mean(mean_max_size)) |> 
            ### binned data from dives makes this metric hard to get after
            # size_skew = mean(mean_skew_size),
            # spp_rich = mean(Species_Richness),
            # fam_richness = mean(Family_Richness),
            # SppShDivInd = mean(Species_Shannon_Diversity_Index),
            # SppInvSimpDivInd = mean(Species_Inverse_Simpson_Diversity_Index),
            # TrophShDivInd = mean(Trophic_Shannon_Diversity_Index),
            # TrophInvSimpDivInd = mean(Trophic_Inverse_Simpson_Diversity_Index))|> 
  ### omit three sites with NA here - it appears because there was no replication of the sites (i.e., one-offs in datasets)
  na.omit() |> 
  ungroup()

glimpse(model_dt)

data <- model_dt|>
  ### mutates character class columns as factors
  mutate_at(c("project", "vert", "ecosystem", "ocean", "biome", "strata", "site"), as.factor)
glimpse(data)

###########################################################################
# select covariates -------------------------------------------------------
###########################################################################

model_data <- data |> 
  ### selection of variables we want for regression models
  dplyr::select(n_stability, p_stability, bm_stability, project, vert, site, strata, ecosystem, ocean, biome, 
                mean_bm, min_ss, mean_ss, max_ss)
                # spp_rich, fam_richness, SppShDivInd, SppInvSimpDivInd,
                # TrophShDivInd, TrophInvSimpDivInd)
glimpse(model_data)

unique(model_data$project)
cols = c("CCE" = '#00008B',
         "FCE" = '#3CB371',
         "MCR" = '#FF7F50',
         "NGA" = '#1E90FF',
         "PIE" = '#556B2F',
         "PISCO-Central" = '#708090',
         "PISCO-South" = '#B0C4DE', 
         "SBC-Beach" = '#F4A460',
         "SBC-Ocean" = '#004953',
         "VCR" = '#7CFC00')

model_data |> 
  filter(n_stability <= 2.5) |>
  # filter(vert == "invertebrate") |>
  # filter(project %in% c("NGA", "SBC-Beach", "SBC-Ocean", "CCE")) |> 
  ggplot(aes(x = bm_stability, y = n_stability, color = project)) + 
  geom_point() +
  scale_color_manual(values = cols) +
  geom_smooth(method = "lm", data = subset(model_data, vert == "vertebrate"), 
              se = FALSE, color = "black", linetype = "solid") +
  geom_smooth(method = "lm", data = subset(model_data, vert == "invertebrate"), 
              se = FALSE, color = "black", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid") +  # Adds the 1:1 line
  labs(x = "Biomass Stability", y = "Nitrogen Stability") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18),
        legend.position = "bottom",
        legend.text = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 18))

ggsave(
  filename = "cns_stability_functionof_bm_stability.tiff",
  path = "plots",
  width = 15, height = 8
)

dt |> 
  # filter(!project == "SBC-Beach") |>
  # filter(vert == "vertebrate") |> 
  group_by(project, site, year) |> 
  summarize(mean_n = mean(total_nitrogen)) |> 
  # filter(mean_n <= 20000) |>
  # ggplot(aes(y = project, x = scale(mean_n, center = FALSE), fill = project)) +
  ggplot(aes(y = project, x = log(mean_n), fill = project)) +
  # filter(mean_n <= 20000) |> 
  # ggplot(aes(y = project, x = mean_n, fill = project)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA) +
  scale_fill_manual(values = cols) +
  geom_jitter(aes(color = project), position = position_jitter(width = 0.2), size = 1.0, alpha = 0.3) +
  # coord_flip() +  # Flip coordinates to make the boxplot horizontal
  labs(x = "Total Nitrogen Supply", y = "Project") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18),
        legend.position = "none")

# ggsave(
#   filename = "total_n_jitter_boxplot_log_short.tiff",
#   path = "plots",
#   width = 15, height = 8
# )

dt_n <- dt |> group_by(project, site, year) |> summarize(mean_n = mean(total_nitrogen))
n_anova <- aov(mean_n ~ project, data = dt_n)
summary(n_anova)
TukeyHSD(n_anova)
# phosphorus jitterbox ----------------------------------------------------

dt |> 
  # filter(!project == "SBC-Beach") |>
  # filter(vert == "vertebrate") |> 
  group_by(project, site, year) |> 
  summarize(mean_p = mean(total_phosphorus)) |> 
  # filter(mean_n <= 20000) |>
  # ggplot(aes(y = project, x = scale(mean_p, center = FALSE), fill = project)) +
  # filter(mean_n <= 20000) |> 
  # ggplot(aes(y = project, x = mean_p, fill = project)) +
  ggplot(aes(y = project, x = log(mean_p), fill = project)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA) +
  scale_fill_manual(values = cols) +
  geom_jitter(aes(color = project), position = position_jitter(width = 0.2), size = 1.0, alpha = 0.3) +
  # coord_flip() +  # Flip coordinates to make the boxplot horizontal
  labs(x = "Total Phosphorus Supply", y = "Project") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18),
        legend.position = "none")

ggsave(
  filename = "total_p_jitter_boxplot_log_short.tiff",
  path = "plots",
  width = 15, height = 8
)

# size structure jitterbox -------------------------------------------------------

dt |> 
  # filter(!project == "SBC-Beach") |>
  # filter(vert == "vertebrate") |> 
  group_by(project, site, year) |> 
  summarize(mean_ss = mean(mean_max_size)) |> 
  # filter(mean_ss <= 200) |>
  # ggplot(aes(y = project, x = scale(mean_ss, center = FALSE), fill = project)) +
  ggplot(aes(y = project, x = log1p(mean_ss), fill = project)) +
  # filter(mean_n <= 20000) |> 
  # ggplot(aes(y = project, x = mean_ss, fill = project)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA) +
  scale_fill_manual(values = cols) +
  geom_jitter(aes(color = project), position = position_jitter(width = 0.2), size = 1.0, alpha = 0.3) +
  # coord_flip() +  # Flip coordinates to make the boxplot horizontal
  labs(x = "Mean Max Size Structure", y = "Project") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18),
        legend.position = "none")

ggsave(
  filename = "size_structure_jitter_boxplot_log_short.tiff",
  path = "plots",
  width = 15, height = 8
)

# mean biomass jitterbox ------------------------------------------------

dt |> 
  # filter(!project == "SBC-Beach") |>
  # filter(vert == "vertebrate") |> 
  group_by(project, site, year) |> 
  summarize(mean_bm = mean(total_biomass)) |> 
  # filter(mean_bm <= 300) |>
  # ggplot(aes(y = project, x = scale(mean_bm, center = FALSE), fill = project)) +
  ggplot(aes(y = project, x = log1p(mean_bm), fill = project)) +
  # filter(mean_n <= 20000) |> 
  # ggplot(aes(y = project, x = mean_bm, fill = project)) +
  # ggplot(aes(y = project, x = log1p(mean_bm), fill = project)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA) +
  scale_fill_manual(values = cols) +
  geom_jitter(aes(color = project), position = position_jitter(width = 0.2), size = 1.0, alpha = 0.3) +
  # coord_flip() +  # Flip coordinates to make the boxplot horizontal
  labs(x = "Total Community Dry Biomass", y = "Project") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18),
        legend.position = "none")

ggsave(
  filename = "total_bm_jitter_boxplot_log_short.tiff",
  path = "plots",
  width = 15, height = 8
)
