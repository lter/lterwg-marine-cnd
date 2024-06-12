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
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, corrplot, performance, ggeffects, ggpubr, parameters)

exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")

### join data for modeling with site characteristic information
dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, climate, latitude, site, year, month, vert, everything()) |> 
  filter(vert == "vertebrate") |> 
  ### if species richness is important in models, remove CCE & NGA as they are not taxonomically resolved species, but instead "groups"
  filter(!project %in% c("CCE", "NGA")) |> 
  ### recalculated species richness in community dataset prep and wanted to ensure it matched up
  ### however, can remove now
  select(-n_spp)

### read in community metrics for analysis
comm <- read_csv("local_data/community_data_filtered.csv")

dt1 <- left_join(dt, comm, relationship = "many-to-many")

na_count_per_column <- sapply(dt1, function(x) sum(is.na(x)))
print(na_count_per_column) 
### 195 NAs within community dataset - because I removed sites instances where nothing was caught
### set community metrics to zero here, since it represents periods of time where nothing was collected

dt2 <- na.omit(dt1)

na_count_per_column <- sapply(dt2, function(x) sum(is.na(x)))
print(na_count_per_column) 

### summarize all sites measured within the dataset annualy
model_dt <- dt2 |> 
  group_by(project, ecosystem, climate, latitude, strata, site) |> 
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
            max_ss = mean(mean_max_size),
            ### binned data from dives makes this metric hard to get after
            # size_skew = mean(mean_skew_size),
            spp_rich = mean(Species_Richness),
            fam_richness = mean(Family_Richness),
            SppShDivInd = mean(Species_Shannon_Diversity_Index),
            SppInvSimpDivInd = mean(Species_Inverse_Simpson_Diversity_Index),
            TrophShDivInd = mean(Trophic_Shannon_Diversity_Index),
            TrophInvSimpDivInd = mean(Trophic_Inverse_Simpson_Diversity_Index))|> 
  ### omit three sites with NA here - it appears because there was no replication of the sites (i.e., one-offs in datasets)
  na.omit() |> 
  ungroup()

glimpse(model_dt)
###########################################################################
# recode factors as such --------------------------------------------------
###########################################################################

data <- model_dt|>
  ### mutates character class columns as factors
  mutate_at(c("project", "ecosystem", "climate", "strata", "site"), as.factor)
glimpse(data)

###########################################################################
# select covariates -------------------------------------------------------
###########################################################################

model_data <- data |> 
  ### selection of variables we want for regression models
  select(n_stability, p_stability, bm_stability, project, site, strata, ecosystem, climate, 
         latitude, mean_bm, min_ss, mean_ss, max_ss,
         spp_rich, fam_richness, SppShDivInd, SppInvSimpDivInd,
         TrophShDivInd, TrophInvSimpDivInd)
glimpse(model_data)

###########################################################################
# explore collinearity ----------------------------------------------------
###########################################################################

numeric_data <- model_data |> 
  ### filter for only numeric data
  select(where(is.numeric))

### generate correlation matrix and visually assess collinearity
cor_matrix <- cor(numeric_data)
### visually assess
corrplot(cor_matrix, method = "number")

### checking for normality in dependent variable - first pull out dependent variable
normality_check <- model_data$bm_stability
### run shapiro-wilks test
result <- shapiro.test(normality_check)
### visually assess
hist(normality_check)
### data right-skewed, so check shapiro-wilks with log transformation
result <- shapiro.test(log(normality_check)) #use log link function

###########################################################################
# create global model -----------------------------------------------------
###########################################################################

model_data_scaled <- model_data |> 
  ### this is a function syntax
  mutate(across(latitude:TrophInvSimpDivInd,\(x) scale(x, center = FALSE)))

glimpse(model_data_scaled)

global_model_bm <- glmmTMB(
  bm_stability ~ 
    # ecosystem +
    # latitude +
    mean_bm + max_ss +
    fam_richness + spp_rich +
    SppInvSimpDivInd + TrophInvSimpDivInd + (1|project),
  data = model_data_scaled,
  na.action = "na.fail",
  family = gaussian(link = "log"),
  REML = FALSE
)
# 
diagnose(global_model_bm)
performance::check_model(global_model_bm)

model_set_bm <- dredge(global_model_bm,
                      subset = !(`cond(SppInvSimpDivInd)`&&`cond(spp_rich)`) & !(`cond(mean_bm)` && `cond(max_ss)`) & !(`cond(spp_rich)` && `cond(fam_richness)`)) |>
  filter(delta < 4)
