
###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, corrplot, performance)
# exc <- read_csv("local_data/model_data.csv") #sites with at least 10 years of data
exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")

### join data for modeling with site characteristic information
dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, biome, site, year, month, vert, everything())

### summarize all sites measured within the dataset annualy
model_dt_site <- dt |> 
  group_by(project, ecosystem, ocean, biome, strata, site, vert) |> 
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
            mean_spp = mean(n_spp),
            min_ss = mean(mean_min_size),
            mean_ss = mean(mean_mean_size),
            max_ss = mean(mean_max_size))|> 
  ### omit seven sites with NA here - it appears because there was no replication of the sites - must have been one-offs in datasets
  na.omit() |> 
  ungroup()

###########################################################################
# filter out vertebrate data and sites with taxonomic resolution ----------
###########################################################################

vm.df <- model_dt_site |> 
  filter(vert == "vertebrate") |> 
  ## if species richness is important in models, remove CCE & NGA as they are not taxonomically resolved species, but instead "groups"
  filter(!project %in% c("CCE", "NGA"))
glimpse(vm.df)

###########################################################################
# recode factors as such --------------------------------------------------
###########################################################################

data <- vm.df |>
  ### mutates character class columns as factors
  mutate_at(c("project", "ecosystem", "ocean", "biome", "strata", "site"), as.factor)
glimpse(data)

###########################################################################
# select covariates -------------------------------------------------------
###########################################################################

model_data <- data |> 
  ### selection of variables we want for regression models
  select(n_stability, project, site, strata, ecosystem, ocean, biome, mean_bm, mean_spp, min_ss, mean_ss, max_ss)
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

# remove collinear variables and re-examine -------------------------------

numeric_data2 <- model_data |> 
  ### remove correlated numeric data
  select(-mean_bm, -mean_ss) |> 
  ### filter for only numeric data
  select(where(is.numeric))

### generate correlation matrix and visually assess collinearity
cor_matrix2 <- cor(numeric_data2)
### visually assess
corrplot(cor_matrix2, method = "number")

### checking for normality in dependent variable - first pull out dependent variable
normality_check <- model_data$n_stability
### run shapiro-wilks test
result <- shapiro.test(normality_check)
### visually assess
hist(normality_check)
### data right-skewed, so check shapiro-wilks with log transformation
result <- shapiro.test(log(normality_check)) #use log link function

###########################################################################
# create global model -----------------------------------------------------
###########################################################################

global_model <- glmmTMB(n_stability ~ ecosystem + biome + ocean +
                        mean_spp + min_ss + max_ss + (1|project), data = model_data,
                        na.action = "na.fail",
                        family = Gamma(link = "log"))

###########################################################################
# create model set --------------------------------------------------------
###########################################################################

model_set <- dredge(global_model)
### seven models with Delta AICc <4

###########################################################################
# run models with delta AICc <4 -------------------------------------------
###########################################################################
m1 <- glmmTMB(n_stability ~ biome + ecosystem + max_ss + mean_spp + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m2 <- glmmTMB(n_stability ~ biome + max_ss + mean_spp + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m3 <- glmmTMB(n_stability ~ biome + max_ss + mean_spp + ocean + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m4 <- glmmTMB(n_stability ~ biome + max_ss + mean_spp + min_ss + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m5 <- glmmTMB(n_stability ~ biome + ecosystem + max_ss + mean_spp + min_ss + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m6 <- glmmTMB(n_stability ~ biome + ecosystem + max_ss + mean_spp + ocean + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m7 <- glmmTMB(n_stability ~ biome + max_ss + mean_spp + min_ss + ocean + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

###########################################################################
# compare model performance -----------------------------------------------
###########################################################################

model_performance <- compare_performance(m1, m2, m3, m4, m5, m6, m7)

### read out model performance table
# write_csv(model_performance, "tables/nstability_model_performance.csv")


