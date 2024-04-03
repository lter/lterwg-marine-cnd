
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
  select(n_stability, p_stability, bm_stability, project, site, strata, ecosystem, ocean, biome, 
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

### looking @ exploratory plots - not super useful... because relationships masked by other information

###########################################################################
# create global model -----------------------------------------------------
###########################################################################

# global_model <- glmmTMB(n_stability ~ biome + ecosystem +
#                         mean_bm + max_ss + 
#                         fam_richness + spp_rich +
#                         SppShDivInd + TrophShDivInd + 
#                         SppInvSimpDivInd + TrophInvSimpDivInd + (1|project), data = model_data,
#                         na.action = "na.fail",
#                         family = Gamma(link = "log"))

# global_model_1 <- glmmTMB(n_stability ~ biome + ecosystem +
#                           mean_bm + max_ss + 
#                           fam_richness + spp_rich +
#                           SppShDivInd + TrophShDivInd + 
#                           SppInvSimpDivInd + TrophInvSimpDivInd + (1|project), data = model_data,
#                         na.action = "na.fail",
#                         family = gaussian(link = "log"))

### drop shannon diversity metrics and keep inv simp bc allgeier et al., 2014

model_data_scaled <- model_data |> 
  ### this is a function syntax
  mutate(across(mean_bm:TrophInvSimpDivInd,\(x) scale(x, center = FALSE)))

glimpse(model_data_scaled)

global_model_2_N <- glmmTMB(
  n_stability ~ biome + ecosystem +
    mean_bm + max_ss +
    fam_richness + spp_rich +
    SppInvSimpDivInd + TrophInvSimpDivInd + (1|project),
    data = model_data_scaled,
    na.action = "na.fail",
    family = gaussian(link = "log"),
    REML = FALSE
)

diagnose(global_model_2_N)
performance::check_model(global_model_2_N)

global_model_2_P <- glmmTMB(
  p_stability ~ biome + ecosystem +
    mean_bm + max_ss +
    fam_richness + spp_rich +
    SppInvSimpDivInd + TrophInvSimpDivInd + (1|project),
  data = model_data,
  na.action = "na.fail",
  family = gaussian(link = "log"),
  REML = FALSE
)

performance::check_model(global_model_2_P)

global_model_2_bm <- glmmTMB(
    bm_stability ~ biome + ecosystem +
    mean_bm + max_ss +
    fam_richness + spp_rich +
    SppInvSimpDivInd + TrophInvSimpDivInd + (1|project),
  data = model_data,
  na.action = "na.fail",
  family = gaussian(link = "log"),
  REML = FALSE
)

performance::check_model(global_model_2_bm)

### added correlated terms for possible subsetting in dredge w WRJ - April 3 2024
### terms added are InvSimpDivInd for species and trophic group

### tried running nested random intercept models but failed to converge
### i.e., (biome/project; project/strata; project/site)
### also, don't believe a random slope makes sense here

### you cannot compare models fitted with different fixed effects using REML - Zuur + Ben Bolker

### 
###########################################################################
# create model set --------------------------------------------------------
###########################################################################

model_set_N <- dredge(global_model_2_N,
                    subset = !(`cond(fam_richness)`&&`cond(spp_rich)`)) |> 
  filter(delta < 4)

model_set_P <- dredge(global_model_2_P,
                      subset = !(`cond(fam_richness)`&&`cond(spp_rich)`)) |> 
  filter(delta < 4)

model_set_bm <- dredge(global_model_2_bm,
                      subset = !(`cond(fam_richness)`&&`cond(spp_rich)`)) |> 
  filter(delta < 4)

### 5 models with Delta AICc <4
### look for sign (+/-) switching in variables... dependent on other things and maybe not the best variable for explaining

model_set$weight <- as.numeric(model_set$weight)
glimpse(model_set)

# write_csv(model_set, "tables/model_set_output.csv")

plot(model_data$bm_stability, model_data$p_stability)

model_data |> ggplot(aes(n_stability, p_stability, color = project)) +
  geom_point() +
  geom_abline() + 
  facet_wrap(~project, scales = "free")

plot(model_data$bm_stability, model_data$n_stability)

###########################################################################
# run models with delta AICc <4 -------------------------------------------
###########################################################################
m1 <- glmmTMB(n_stability ~ biome + fam_richness + max_ss + mean_bm + TrophInvSimpDivInd + (1|project), data = model_data_scaled,
              family = gaussian(link = "log"),
              REML = FALSE)

### switched to REML = TRUE, given presumed issues with model convergence resulting in Na/NaN in Std. Error & Warnings after model fitting
### REML chosen over ML as it maximizes variance

test <- compare_performance(m1)
summary(m1)
plot(m1)
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

### adding a term and making it non-significant, within AICc of 4
### doesn't make a ton of sense in our case, but if interested in predicting... then it would be
### look into it being used in ecology???
### AC sees no red flags so far - but ask folks in wg for opinion and see if they have stronger thoughts!

### type = "re" incorporates variance from random effects

### can't use average model because then no variance incorporated?
pred_fam_rich <- ggeffect(m1, "fam_richness[0:1.6 by=0.1]", type = "re")
pred_max_ss <- ggeffect(m1, "max_ss[0:3 by=0.1]", type = "re")
pred_TrophShDivInd <- ggeffect(m1, "TrophInvSimpDivInd[0:1.4 by=0.1]", type = "re")
pred_biome <- ggeffect(m1, "biome", type = "re")

plot(pred_fam_rich)
plot(pred_max_ss)
plot(pred_TrophShDivInd)
plot(pred_biome)

### AC will get back on better ways to investigate marginal effects
### VSCode - user interface for python!

### EDI Uploading
# Have seen groups upload by themselves - if find we don't have bandwidth
# I'll reach out to Li and cc AC

### worth re-scaling (i.e., reverting back to raw values for plotting)
### how does it know how to scale???
