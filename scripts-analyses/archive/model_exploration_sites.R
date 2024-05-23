# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, mgcv, gamm4, nlme, lme4, glmmTMB, MuMIn, performance)
# look into dredge() within MuMIn package
# exc <- read_csv("local_data/model_data.csv") #sites with at least 10 years of data
exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")
dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, biome, site, year, month, vert, everything())

model_dt_site <- dt |> 
  group_by(project, ecosystem, ocean, biome, strata, site, vert) |> 
  summarize(mean_n = mean(total_nitrogen),
            cv_n = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)) * 100,
            log_cv_n = log(cv_n),
            mean_p = mean(total_phosphorus),
            cv_p = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)) * 100,
            mean_bm = mean(total_biomass),
            cv_bm = (sd(total_biomass, na.rm = TRUE) / mean(total_biomass, na.rm = TRUE)) * 100,
            mean_spp = mean(n_spp),
            cv_spp = (sd(total_biomass, na.rm = TRUE) / mean(total_biomass, na.rm = TRUE)) * 100,
            min_ss = mean(mean_min_size),
            cv_min_ss = (sd(mean_min_size, na.rm = TRUE) / mean(mean_min_size, na.rm = TRUE)) * 100,
            max_ss = mean(mean_max_size),
            cv_max_ss = (sd(mean_max_size, na.rm = TRUE) / mean(mean_max_size, na.rm = TRUE)) * 100) |> 
  ### omit seven sites with NA here - it appears because there was no replication of the sites
  na.omit()

vm_site.df <- model_dt_site |> 
  filter(vert == "vertebrate") |> 
  ## if species richness is important in models, remove CCE & NGA as they are not taxonomically resolved species, but instead "groups"
  filter(!project %in% c("CCE", "NGA"))

im_site.df <- model_dt_site |> 
  filter(vert == "invertebrate")

#exploratory plots
boxplot(vm_site.df$log_cv_n~vm_site.df$project)      #differences across projects - CCE sig different from SBC, PISCO-South, PISCO-Central sig different from MCR, FCE, and PIE
boxplot(vm_site.df$log_cv_n~vm_site.df$ecosystem)    #differences across ecosystems - sig difference between estuarine, nearshore, and pelagic
boxplot(vm_site.df$mean_spp~vm_site.df$biome)        #differences across biomes - sig difference between [descending] tropical, subtropical, and temperate
boxplot(vm_site.df$log_cv_n~vm_site.df$ocean)        #difference between atlantic and pacific - sig difference in atlantic
boxplot(vm_site.df$log_cv_n~vm_site.df$year)         #kinda looks like increasing across time...
vm_site.df |> ggplot(aes(mean_bm, mean_spp)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
vm_site.df |> ggplot(aes(site, log_cv_n)) + geom_boxplot() + facet_wrap(~project, scales = "free")
#differences across sites within projects           
plot(vm_site.df$mean_bm, vm_site.df$log_cv_n)
vm_site.df |> ggplot(aes(mean_bm, log_cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm_site.df$cv_bm, vm_site.df$log_cv_n)
vm_site.df |> ggplot(aes(cv_bm, log_cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm_site.df$mean_spp, vm_site.df$log_cv_n)
vm_site.df |> ggplot(aes(mean_spp, log_cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm_site.df$cv_spp, vm_site.df$log_cv_n)
vm_site.df |> ggplot(aes(cv_spp, log_cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm_site.df$min_ss, vm_site.df$log_cv_n)
vm_site.df |> ggplot(aes(min_ss, log_cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm_site.df$cv_min_ss, vm_site.df$log_cv_n)
vm_site.df |> ggplot(aes(cv_min_ss, log_cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm_site.df$max_ss, vm_site.df$log_cv_n)
vm_site.df |> ggplot(aes(max_ss, log_cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm_site.df$cv_max_ss, vm_site.df$log_cv_n)
vm_site.df |> ggplot(aes(cv_max_ss, log_cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm_site.df$year, vm_site.df$log_cv_n)
vm_site.df |> ggplot(aes(year, log_cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
vm_site.df |> ggplot(aes(year, log_cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "gam", formula = y~s(x))

# lmer models - vertebrate data -------------------------------------------

#'null' models
mVERT_null <- lmer(log_cv_n~1 +(1|project),data=vm_site.df,REML=F)
# mVERT_year <- lmer(log_cv_n~year +(1|project/site),data=vm_site.df,REML=F)
# categorical variables - single term -------------------------------------
mVERT_ecosystem <- lmer(log_cv_n~ecosystem +(1|project),data=vm_site.df,REML=F)
mVERT_ocean <- lmer(log_cv_n~ocean +(1|project),data=vm_site.df,REML=F)
mVERT_biome <- lmer(log_cv_n~biome +(1|project),data=vm_site.df,REML=F)
# categorical variables - interactions with year --------------------------
# mVERT_ecoXyear <- lmer(log_cv_n~ecosystem*year +(1|project/site),data=vm_site.df,REML=F)
# mVERT_oceXyear <- lmer(log_cv_n~ocean*year +(1|project/site),data=vm_site.df,REML=F)
# mVERT_bioXyear <- lmer(log_cv_n~biome*year +(1|project/site),data=vm_site.df,REML=F)
# mVERT_ecoXbiomeXyear <- lmer(log_cv_n~ecosystem*biome*year +(1|project/site),data=vm_site.df,REML=F)
# mVERT_ecoXoceanXyear <- lmer(log_cv_n~ocean*ocean*year +(1|project/site),data=vm_site.df,REML=F)
# mVERT_ecoXbiome <- lmer(log_cv_n~ecosystem*biome +(1|project/site),data=vm_site.df,REML=F)
# mVERT_ecoXocean <- lmer(log_cv_n~ocean*ocean +(1|project/site),data=vm_site.df,REML=F)
# categorical variables - global ------------------------------------------
mVERT_global_cat <- lmer(log_cv_n~ecosystem + ocean + biome + (1|project),data=vm_site.df,REML=F)
# mVERT_global_cat_with_yr <- (lmer(log_cv_n~ year + ecosystem + ocean + biome + ecosystem*year + ocean*year + biome*year + (1|project/site),data=vm_site.df,REML=F))
# continuous variables - single term --------------------------------------
mVERT_biomass <- lmer(log_cv_n~mean_bm +(1|project),data=vm_site.df,REML=F)
mVERT_spp_rich <- lmer(log_cv_n~mean_spp +(1|project),data=vm_site.df,REML=F)
mVERT_min_ss <- lmer(log_cv_n~min_ss +(1|project),data=vm_site.df,REML=F)
mVERT_max_ss <- lmer(log_cv_n~max_ss +(1|project),data=vm_site.df,REML=F)
# mVERT_biomassXyear <- lmer(log_cv_n~mean_bm +(1|project/site),data=vm_site.df,REML=F)
# mVERT_spp_richXyear <- lmer(log_cv_n~mean_spp +(1|project/site),data=vm_site.df,REML=F)
# mVERT_min_ssXyear <- lmer(log_cv_n~min_ss +(1|project/site),data=vm_site.df,REML=F)
# mVERT_max_ssXyear <- lmer(log_cv_n~mean_bm +(1|project/site),data=vm_site.df,REML=F)
# continuous variables - interactions -------------------------------------
mVERT_biomassXspp_rich <- lmer(log_cv_n~mean_bm*mean_spp +(1|project),data=vm_site.df,REML=F)
mVERT_biomassXmax_ss <- lmer(log_cv_n~mean_bm*max_ss +(1|project),data=vm_site.df,REML=F)
mVERT_max_ssXspp_rich <- lmer(log_cv_n~max_ss*mean_spp +(1|project),data=vm_site.df,REML=F)
mVERT_max_ss.spp_rich <- lmer(log_cv_n~max_ss+mean_spp +(1|project),data=vm_site.df,REML=F)
mVERT_max_ss.spp_rich.ecosystem.biome <- lmer(log_cv_n~max_ss+mean_spp+ecosystem+biome +(1|project),data=vm_site.df,REML=F)
mVERTnested_max_ss.spp_rich.ecosystem.biome <- lmer(log_cv_n~max_ss+mean_spp+ecosystem+biome +(1|project/strata),data=vm_site.df,REML=F)
mVERT_max_ss.spp_rich.ecosystemXbiome <- lmer(log_cv_n~max_ss+mean_spp+ecosystem*biome +(1|project),data=vm_site.df,REML=F)
mVERT_max_ss.spp_rich.ecosystem <- lmer(log_cv_n~max_ss+mean_spp+ecosystem +(1|project),data=vm_site.df,REML=F)
# continuous variables - global -------------------------------------------
mVERT_global_cont <- lmer(log_cv_n~mean_bm + mean_spp + min_ss + max_ss +(1|project),data=vm_site.df,REML=F)
# global - all ------------------------------------------------------------
mVERT_global <- lmer(log_cv_n~ecosystem + ocean + biome + mean_bm + mean_spp + min_ss + max_ss +(1|project),data=vm_site.df,REML=F)
# AICc Comparison ---------------------------------------------------------
#AICc comparison of competing models
n = dim(vm_site.df)[1]  #sample size
VERT.Table = AIC (
  mVERT_null,
  mVERT_ecosystem, mVERT_ocean, mVERT_biome,
  mVERT_global_cat,
  mVERT_biomass, mVERT_spp_rich, mVERT_min_ss, mVERT_max_ss,
  mVERT_biomassXspp_rich, mVERT_biomassXmax_ss, mVERT_max_ssXspp_rich,
  mVERT_max_ss.spp_rich, mVERT_max_ss.spp_rich.ecosystem.biome, mVERT_max_ss.spp_rich.ecosystemXbiome,
  mVERT_max_ss.spp_rich.ecosystem, mVERTnested_max_ss.spp_rich.ecosystem.biome,
  mVERT_global_cont,
  mVERT_global
) 

AICc = VERT.Table$AIC + (2*VERT.Table$df*(VERT.Table$df+1))/(n-VERT.Table$df-1)    #calculate AICc from AIC (Table$AIC), n, and K(Table$df)
VERT.Table = cbind(VERT.Table,AICc)                                      #insert AICc values into original table
deltaAICc = VERT.Table$AICc - min(VERT.Table$AICc)                       #calculates the delta AICc values
VERT.Table = cbind(VERT.Table,deltaAICc)                                 #insert deltaAICc values into original table
w_i = (exp(-VERT.Table$deltaAICc/2))/(sum(exp(-VERT.Table$deltaAICc/2))) #calculates the Akaike model weights, corrected for sample size
VERT.Table = cbind(VERT.Table,w_i)                                       #insert w_i values into original table
VERT.Table
#Ordered by delta AICc
VERT.TableOrd = VERT.Table[order(VERT.Table$AICc,VERT.Table$deltaAICc,VERT.Table$w_i),]    #reorganize table using typical journal format
VERT.TableOrd

# summary(mVERT_max_ssXspp_rich)
# summary(mVERT_max_ss.spp_rich.ecosystem.biome)
# summary(mVERT_global)

performance::check_collinearity(mVERT_max_ss.spp_rich.ecosystem.biome)
performance::performance_accuracy(mVERT_max_ss.spp_rich.ecosystem)
performance::check_model(mVERT_max_ss.spp_rich.ecosystem)
performance::check_convergence(mVERT_max_ss.spp_rich.ecosystem)
performance::check_normality(mVERT_max_ss.spp_rich.ecosystem) #residuals are non-normal
performance::check_heteroscedasticity(mVERT_max_ss.spp_rich.ecosystem.biome)
log_cv_n_check <- vm_site.df$log_cv_n
result <- shapiro.test(log_cv_n_check)
hist(log_cv_n_check)

# with glmmtmb can use log link function (i.e., error structure)with raw data
# gamma with log-link or identity link
# definitely gaussian with log link and identity link

# could just do global models with additive effects - maybe interactions with biome bc relationship with temperature
# ensure that no models are fit with no more than 10 independent variables
# all model combinations with up to 10 variables
# AICc accounts for that, however
## dredge will give you a table with normal diagnostics, but also column with variable names
## if blank, not included in that model
## number is b for that variable and '+' means included and a categorical variable
## intercept and nothing else... that is null model
## can filter by delta(?) less than four 
## will also give weight... 
## after this, run models, and then use ggeffects to look at marginal effects plot based on model fit
## talk about it this way - not p values, using information criterion instead
## atlas paper in 2020 - were trying to predict lake production based on # of fish
## can use compare performance with performance package to compare models
## can generate a hypothesis table for each of the factors 
## maybe inverse simpson (ie # of species given same evenness)
## can use dredge to say, "dont fit these two variables in same model bc collinear"
## 1/cv
## get rid of 100 on cv