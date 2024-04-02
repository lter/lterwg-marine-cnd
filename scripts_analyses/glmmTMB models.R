# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, corrplot, performance)
# look into dredge() within MuMIn package
# exc <- read_csv("local_data/model_data.csv") #sites with at least 10 years of data
exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")
dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, biome, site, year, month, vert, everything())

model_dt_site <- dt |> 
  group_by(project, ecosystem, ocean, biome, strata, site, vert) |> 
  summarize(mean_n = mean(total_nitrogen),
            cv_n = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            n_stability = 1/cv_n,
            mean_p = mean(total_phosphorus),
            cv_p = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            p_stability = 1/cv_p,
            mean_bm = mean(total_biomass),
            cv_bm = (sd(total_biomass, na.rm = TRUE) / mean(total_biomass, na.rm = TRUE)),
            bm_stability = 1/cv_bm,
            mean_spp = mean(n_spp),
            min_ss = mean(mean_min_size),
            mean_ss = mean(mean_mean_size),
            max_ss = mean(mean_max_size))|> 
  ### omit seven sites with NA here - it appears because there was no replication of the sites
  na.omit() |> 
  ungroup()

vm.df <- model_dt_site |> 
  filter(vert == "vertebrate") |> 
  ## if species richness is important in models, remove CCE & NGA as they are not taxonomically resolved species, but instead "groups"
  filter(!project %in% c("CCE", "NGA"))
glimpse(vm.df)

# recode factors as such --------------------------------------------------

data <- vm.df |>
  mutate_at(c("project", "ecosystem", "ocean", "biome", "strata", "site"), as.factor)
glimpse(data)

# select covariates -------------------------------------------------------

model_data <- data |> 
  select(n_stability, project, site, strata, ecosystem, ocean, biome, mean_bm, mean_spp, min_ss, mean_ss, max_ss)
glimpse(model_data)
# explore collinearity ----------------------------------------------------

numeric_data <- model_data |> 
  ### filter for only numeric data
  select(where(is.numeric))

### generate correlation matrix and visually assess collinearity
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method = "number")

# remove collinear variables and re-examine -------------------------------

numeric_data2 <- model_data |> 
  select(-mean_bm, -mean_ss) |> 
  select(where(is.numeric))

cor_matrix2 <- cor(numeric_data2)
corrplot(cor_matrix2, method = "number")

# create global model -----------------------------------------------------
global_model <- glmmTMB(n_stability ~ ecosystem + ocean + biome +
                        mean_spp + min_ss + max_ss + (1|project), data = model_data,
                        family = Gamma(link = "log"))

# create model set --------------------------------------------------------

model_set <- dredge(global_model)



