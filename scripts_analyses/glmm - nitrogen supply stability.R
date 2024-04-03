
###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, corrplot, performance, ggeffects, sjlabelled)
# exc <- read_csv("local_data/model_data.csv") #sites with at least 10 years of data
exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")

### join data for modeling with site characteristic information
dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, biome, site, year, month, vert, everything()) |> 
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

dt2 <- dt1 %>%
  mutate(across(everything(), ~replace(., is.na(.), 0)))

na_count_per_column <- sapply(dt2, function(x) sum(is.na(x)))
print(na_count_per_column) 

### summarize all sites measured within the dataset annualy
model_dt <- dt2 |> 
  group_by(project, ecosystem, ocean, biome, strata, site) |> 
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
  mutate_at(c("project", "ecosystem", "ocean", "biome", "strata", "site"), as.factor)
glimpse(data)

###########################################################################
# select covariates -------------------------------------------------------
###########################################################################

model_data <- data |> 
  ### selection of variables we want for regression models
  select(n_stability, project, site, strata, ecosystem, ocean, biome, 
         mean_bm, min_ss, mean_ss, max_ss,
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

### exploratory plots with log n-stability ~ project (lm)
model_data|>ggplot(aes(mean_bm, n_stability))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="lm")
model_data|>ggplot(aes(max_ss, n_stability))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="lm")
model_data|>ggplot(aes(fam_richness, n_stability))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="lm")
model_data|>ggplot(aes(spp_rich, n_stability))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="lm")
model_data|>ggplot(aes(SppShDivInd, n_stability))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="lm")
model_data|>ggplot(aes(TrophShDivInd, n_stability))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="lm")

### exploratory plots with log n-stability (lm)
model_data|>ggplot(aes(mean_bm, log(n_stability)))+geom_point()+geom_smooth(method="lm")
model_data|>ggplot(aes(max_ss, log(n_stability)))+geom_point()+geom_smooth(method="lm")
model_data|>ggplot(aes(fam_richness, log(n_stability)))+geom_point()+geom_smooth(method="lm")
model_data|>ggplot(aes(spp_rich, log(n_stability)))+geom_point()+geom_smooth(method="lm")
model_data|>ggplot(aes(SppShDivInd, log(n_stability)))+geom_point()+geom_smooth(method="lm")
model_data|>ggplot(aes(TrophShDivInd, log(n_stability)))+geom_point()+geom_smooth(method="lm")

### exploratory plots with log n-stability ~ project (gam)
model_data|>ggplot(aes(mean_bm, log(n_stability)))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="gam")
model_data|>ggplot(aes(max_ss, log(n_stability)))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="gam")
model_data|>ggplot(aes(fam_richness, log(n_stability)))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="gam")
model_data|>ggplot(aes(spp_rich, log(n_stability)))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="gam")
model_data|>ggplot(aes(SppShDivInd, log(n_stability)))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="gam")
model_data|>ggplot(aes(TrophShDivInd, log(n_stability)))+geom_point()+facet_wrap(~project,scales="free")+geom_smooth(method="gam")

### exploratory plots with log n-stability (gam)
model_data|>ggplot(aes(mean_bm, log(n_stability)))+geom_point()+geom_smooth(method="gam")
model_data|>ggplot(aes(max_ss, log(n_stability)))+geom_point()+geom_smooth(method="gam")
model_data|>ggplot(aes(fam_richness, log(n_stability)))+geom_point()+geom_smooth(method="gam")
model_data|>ggplot(aes(spp_rich, log(n_stability)))+geom_point()+geom_smooth(method="gam")
model_data|>ggplot(aes(SppShDivInd, log(n_stability)))+geom_point()+geom_smooth(method="gam")
model_data|>ggplot(aes(TrophShDivInd, log(n_stability)))+geom_point()+geom_smooth(method="gam")


###########################################################################
# create global model -----------------------------------------------------
###########################################################################

global_model <- glmmTMB(n_stability ~ biome + ecosystem +
                        mean_bm + max_ss + 
                        fam_richness + spp_rich +
                        SppShDivInd + TrophShDivInd + (1|project), data = model_data,
                        na.action = "na.fail",
                        family = Gamma(link = "log"))

### tried running nested random intercept models but failed to converge
### i.e., (biome/project; project/strata; project/site)
### also, don't believe a random slope makes sense here

#rerun with sig terms from best-fit model
global_model2 <- glmmTMB(n_stability ~ biome + ecosystem +
                        mean_bm + max_ss + 
                        fam_richness +
                        TrophShDivInd + (1|project), data = model_data,
                        na.action = "na.fail",
                        family = Gamma(link = "log"))
###########################################################################
# create model set --------------------------------------------------------
###########################################################################

model_set <- dredge(global_model)
### 15 models with Delta AICc <4

model_set_output <- model_set |> 
  filter(delta <= 4)
model_set_output$weight <- as.numeric(model_set_output$weight)

glimpse(model_set_output)
write_csv(model_set_output, "tables/model_set_output.csv")

# model_set2 <- dredge(global_model2)
# ### 5 models with Delta AICc <4

###########################################################################
# run models with delta AICc <4 -------------------------------------------
###########################################################################
m1 <- glmmTMB(n_stability ~ biome + ecosystem + fam_richness + max_ss + mean_bm + TrophShDivInd + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m2 <- glmmTMB(n_stability ~ biome + fam_richness + max_ss + mean_bm + TrophShDivInd + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m3 <- glmmTMB(n_stability ~ biome + ecosystem + fam_richness + max_ss + mean_bm + spp_rich + TrophShDivInd + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m4 <- glmmTMB(n_stability ~ biome + ecosystem + max_ss + mean_bm + spp_rich + TrophShDivInd + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m5 <- glmmTMB(n_stability ~ biome + fam_richness + max_ss + mean_bm + spp_rich + TrophShDivInd + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m6 <- glmmTMB(n_stability ~ biome + ecosystem + fam_richness + max_ss + mean_bm + SppShDivInd + TrophShDivInd + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m7 <- glmmTMB(n_stability ~ biome + fam_richness + max_ss + TrophShDivInd + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m8 <- glmmTMB(n_stability ~ biome + fam_richness + max_ss + mean_bm + SppShDivInd + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m9 <- glmmTMB(n_stability ~ biome + fam_richness + max_ss + mean_bm + SppShDivInd + TrophShDivInd + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m10 <- glmmTMB(n_stability ~ biome + ecosystem + max_ss + mean_bm + TrophShDivInd + (1|project), data = model_data,
              na.action = "na.fail",
              family = Gamma(link = "log"))

m11 <- glmmTMB(n_stability ~ biome + ecosystem + fam_richness + max_ss + TrophShDivInd + (1|project), data = model_data,
               na.action = "na.fail",
               family = Gamma(link = "log"))

m12_global <- glmmTMB(n_stability ~ biome + ecosystem + fam_richness + max_ss + mean_bm + spp_rich + SppShDivInd + TrophShDivInd + (1|project), data = model_data,
               na.action = "na.fail",
               family = Gamma(link = "log"))

m13 <- glmmTMB(n_stability ~ biome + fam_richness + max_ss + mean_bm + spp_rich + (1|project), data = model_data,
                      na.action = "na.fail",
                      family = Gamma(link = "log"))

m14 <- glmmTMB(n_stability ~ biome + ecosystem + max_ss + mean_bm + spp_rich + SppShDivInd + TrophShDivInd + (1|project), data = model_data,
               na.action = "na.fail",
               family = Gamma(link = "log"))

m15 <- glmmTMB(n_stability ~ biome + fam_richness + max_ss + spp_rich + TrophShDivInd + (1|project), data = model_data,
                  na.action = "na.fail",
                  family = Gamma(link = "log"))

m16_null <- glmmTMB(n_stability ~ 1 + (1|project), data = model_data,
               na.action = "na.fail",
               family = Gamma(link = "log"))

###########################################################################
# compare model performance -----------------------------------------------
###########################################################################

model_performance <- compare_performance(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, 
                                         m11, m12_global, m13, m14, m15, m16_null)

model_performance_deltaaicc <- model_performance |> 
  mutate(DeltaAICc = AICc - 1.239188) |> 
  select(Name, AICc, DeltaAICc, AICc_wt, R2_marginal, RMSE, Sigma)

### read out model performance table
# write_csv(model_performance_deltaaicc, "tables/nstability_model_performance.csv")

avg_model <- model.avg(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, 
                       m11, m12_global, m13, m14, m15)

summary(avg_model)
### type = "re" incorporates variance from random effects
# pred_fam_rich <- ggpredict(avg_model, "fam_richness", type = "re")
# pred_max_ss <- ggpredict(avg_model, "max_ss", type = "re")
# pred_TrophShDivInd <- ggpredict(avg_model, "TrophShDivInd", type = "re")

### can't use average model because then no variance incorporated?
pred_fam_rich <- ggpredict(m1, "fam_richness", type = "sim")
pred_max_ss <- ggpredict(m1, "max_ss", type = "sim")
pred_TrophShDivInd <- ggpredict(m1, "TrophShDivInd", type = "sim")

plot(pred_fam_rich)
plot(pred_max_ss)
plot(pred_TrophShDivInd)

