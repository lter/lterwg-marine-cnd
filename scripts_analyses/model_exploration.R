# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, mgcv, gamm4, nlme, lme4, performance)

# exc <- read_csv("local_data/model_data.csv") #sites with at least 10 years of data
exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")
dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, biome, site, year, month, vert, everything())

model_dt <- dt |> 
  group_by(project, ecosystem, ocean, biome, strata, site, year, vert) |> 
  summarize(mean_n = mean(total_nitrogen),
            cv_n = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)) * 100,
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

vm.df <- model_dt |> 
  filter(vert == "vertebrate") |> 
  filter(!project %in% c("CCE", "NGA"))

im.df <- model_dt |> 
  filter(vert == "invertebrate")

# vert models -----------------------------------------------------------

#exploratory plots
boxplot(vm.df$cv_n~vm.df$project)      #differences across projects - CCE sig different from SBC, PISCO-South, PISCO-Central sig different from MCR, FCE, and PIE
boxplot(vm.df$cv_n~vm.df$ecosystem)    #differences across ecosystems - sig difference between estuarine, nearshore, and pelagic
boxplot(vm.df$cv_n~vm.df$biome)        #differences across biomes - sig difference between [descending] tropical, subtropical, and temperate
boxplot(vm.df$cv_n~vm.df$ocean)        #difference between atlantic and pacific - sig difference in atlantic
boxplot(vm.df$cv_n~vm.df$year)         #kinda looks like increasing across time...
vm.df |> ggplot(aes(mean_bm, mean_spp)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
vm.df |> ggplot(aes(site, cv_n)) + geom_boxplot() + facet_wrap(~project, scales = "free")
#differences across sites within projects           
plot(vm.df$mean_bm, vm.df$cv_n)
vm.df |> ggplot(aes(mean_bm, cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm.df$cv_bm, vm.df$cv_n)
vm.df |> ggplot(aes(cv_bm, cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm.df$mean_spp, vm.df$cv_n)
vm.df |> ggplot(aes(mean_spp, cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm.df$cv_spp, vm.df$cv_n)
vm.df |> ggplot(aes(cv_spp, cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm.df$min_ss, vm.df$cv_n)
vm.df |> ggplot(aes(min_ss, cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm.df$cv_min_ss, vm.df$cv_n)
vm.df |> ggplot(aes(cv_min_ss, cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm.df$max_ss, vm.df$cv_n)
vm.df |> ggplot(aes(max_ss, cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm.df$cv_max_ss, vm.df$cv_n)
vm.df |> ggplot(aes(cv_max_ss, cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
plot(vm.df$year, vm.df$cv_n)
vm.df |> ggplot(aes(year, cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "lm")
vm.df |> ggplot(aes(year, cv_n)) + geom_point() + facet_wrap(~strata, scales = "free") + geom_smooth(method = "gam", formula = y~s(x))



# lmer models - vertebrate data -------------------------------------------

#'null' models
mVERT_null <- lmer(cv_n~1 +(1|project/strata),data=vm.df,REML=F)
mVERT_year <- lmer(cv_n~year +(1|project/strata),data=vm.df,REML=F)
# categorical variables - single term -------------------------------------
mVERT_ecosystem <- lmer(cv_n~ecosystem +(1|project/strata),data=vm.df,REML=F)
mVERT_ocean <- lmer(cv_n~ocean +(1|project/strata),data=vm.df,REML=F)
mVERT_biome <- lmer(cv_n~biome +(1|project/strata),data=vm.df,REML=F)
# categorical variables - interactions with year --------------------------
mVERT_ecoXyear <- lmer(cv_n~ecosystem*year +(1|project/strata),data=vm.df,REML=F)
mVERT_oceXyear <- lmer(cv_n~ocean*year +(1|project/strata),data=vm.df,REML=F)
mVERT_bioXyear <- lmer(cv_n~biome*year +(1|project/strata),data=vm.df,REML=F)
mVERT_ecoXbiomeXyear <- lmer(cv_n~ecosystem*biome*year +(1|project/strata),data=vm.df,REML=F)
mVERT_ecoXoceanXyear <- lmer(cv_n~ocean*ocean*year +(1|project/strata),data=vm.df,REML=F)
mVERT_ecoXbiome <- lmer(cv_n~ecosystem*biome +(1|project/strata),data=vm.df,REML=F)
mVERT_ecoXocean <- lmer(cv_n~ocean*ocean +(1|project/strata),data=vm.df,REML=F)
# categorical variables - global ------------------------------------------
mVERT_global_cat <- (lmer(cv_n~ecosystem + ocean + biome + ecosystem*year + ocean*year + biome*year + (1|project/strata),data=vm.df,REML=F))
mVERT_global_cat_with_yr <- (lmer(cv_n~ year + ecosystem + ocean + biome + ecosystem*year + ocean*year + biome*year + (1|project/strata),data=vm.df,REML=F))
# continuous variables - single term --------------------------------------
mVERT_biomass <- lmer(cv_n~mean_bm +(1|project/strata),data=vm.df,REML=F)
mVERT_spp_rich <- lmer(cv_n~mean_spp +(1|project/strata),data=vm.df,REML=F)
mVERT_min_ss <- lmer(cv_n~min_ss +(1|project/strata),data=vm.df,REML=F)
mVERT_max_ss <- lmer(cv_n~max_ss +(1|project/strata),data=vm.df,REML=F)
mVERT_biomassXyear <- lmer(cv_n~mean_bm*year +(1|project/strata),data=vm.df,REML=F)
mVERT_spp_richXyear <- lmer(cv_n~mean_spp*year +(1|project/strata),data=vm.df,REML=F)
mVERT_min_ssXyear <- lmer(cv_n~min_ss*year +(1|project/strata),data=vm.df,REML=F)
mVERT_max_ssXyear <- lmer(cv_n~max_ss*year +(1|project/strata),data=vm.df,REML=F)


# AICc Comparison ---------------------------------------------------------
#AICc comparison of competing models
n = dim(vm.df)[1]  #sample size
VERT.Table = AIC (
  mVERT_null, mVERT_year,
  mVERT_ecosystem, mVERT_ocean, mVERT_biome,
  mVERT_ecoXyear, mVERT_oceXyear, mVERT_bioXyear,
  mVERT_ecoXbiomeXyear, mVERT_ecoXoceanXyear,
  mVERT_ecoXbiome, mVERT_ecoXocean,
  mVERT_global_cat, mVERT_global_cat_with_yr,
  mVERT_biomass, mVERT_spp_rich, mVERT_min_ss, mVERT_max_ss,
  mVERT_biomassXyear, mVERT_spp_richXyear, mVERT_min_ssXyear, mVERT_max_ssXyear
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



# lmer models - invertebrate data -------------------------------------------

#'null' models
mINV_null <- lmer(cv_n~1 +(1|project/strata),data=im.df,REML=F)
mINV_year <- lmer(cv_n~year +(1|project/strata),data=im.df,REML=F)
# categorical variables - single term -------------------------------------
mINV_ecosystem <- lmer(cv_n~ecosystem +(1|project/strata),data=im.df,REML=F)
mINV_ocean <- lmer(cv_n~ocean +(1|project/strata),data=im.df,REML=F)
mINV_biome <- lmer(cv_n~biome +(1|project/strata),data=im.df,REML=F)
# categorical variables - interactions with year --------------------------
mINV_ecoXyear <- lmer(cv_n~ecosystem*year +(1|project/strata),data=im.df,REML=F)
mINV_oceXyear <- lmer(cv_n~ocean*year +(1|project/strata),data=im.df,REML=F)
mINV_bioXyear <- lmer(cv_n~biome*year +(1|project/strata),data=im.df,REML=F)
mINV_ecoXbiomeXyear <- lmer(cv_n~ecosystem*biome*year +(1|project/strata),data=im.df,REML=F)
mINV_ecoXoceanXyear <- lmer(cv_n~ocean*ocean*year +(1|project/strata),data=im.df,REML=F)
mINV_ecoXbiome <- lmer(cv_n~ecosystem*biome +(1|project/strata),data=im.df,REML=F)
mINV_ecoXocean <- lmer(cv_n~ocean*ocean +(1|project/strata),data=im.df,REML=F)
# categorical variables - global ------------------------------------------
mINV_global_cat <- (lmer(cv_n~ecosystem + ocean + biome + ecosystem*year + ocean*year + biome*year + (1|project/strata),data=im.df,REML=F))
mINV_global_cat_with_yr <- (lmer(cv_n~ year + ecosystem + ocean + biome + ecosystem*year + ocean*year + biome*year + (1|project/strata),data=im.df,REML=F))
# continuous variables - single term --------------------------------------
mINV_biomass <- lmer(cv_n~mean_bm +(1|project/strata),data=im.df,REML=F)
mINV_spp_rich <- lmer(cv_n~mean_spp +(1|project/strata),data=im.df,REML=F)
mINV_min_ss <- lmer(cv_n~min_ss +(1|project/strata),data=im.df,REML=F)
mINV_max_ss <- lmer(cv_n~mean_bm +(1|project/strata),data=im.df,REML=F)
mINV_biomassXyear <- lmer(cv_n~mean_bm +(1|project/strata),data=im.df,REML=F)
mINV_spp_richXyear <- lmer(cv_n~mean_spp +(1|project/strata),data=im.df,REML=F)
mINV_min_ssXyear <- lmer(cv_n~min_ss +(1|project/strata),data=im.df,REML=F)
mINV_max_ssXyear <- lmer(cv_n~mean_bm +(1|project/strata),data=im.df,REML=F)


# AICc Comparison ---------------------------------------------------------
#AICc comparison of competing models
n = dim(im.df)[1]  #sample size
INV.Table = AIC (
  mINV_null, mINV_year,
  mINV_ecosystem, mINV_ocean, mINV_biome,
  mINV_ecoXyear, mINV_oceXyear, mINV_bioXyear,
  mINV_ecoXbiomeXyear, mINV_ecoXoceanXyear,
  mINV_ecoXbiome, mINV_ecoXocean,
  mINV_global_cat, mINV_global_cat_with_yr,
  mINV_biomass, mINV_spp_rich, mINV_min_ss, mINV_max_ss,
  mINV_biomassXyear, mINV_spp_richXyear, mINV_min_ssXyear, mINV_max_ssXyear
)  
AICc = INV.Table$AIC + (2*INV.Table$df*(INV.Table$df+1))/(n-INV.Table$df-1)    #calculate AICc from AIC (Table$AIC), n, and K(Table$df)
INV.Table = cbind(INV.Table,AICc)                                      #insert AICc values into original table
deltaAICc = INV.Table$AICc - min(INV.Table$AICc)                       #calculates the delta AICc values
INV.Table = cbind(INV.Table,deltaAICc)                                 #insert deltaAICc values into original table
w_i = (exp(-INV.Table$deltaAICc/2))/(sum(exp(-INV.Table$deltaAICc/2))) #calculates the Akaike model weights, corrected for sample size
INV.Table = cbind(INV.Table,w_i)                                       #insert w_i values into original table
INV.Table
#Ordered by delta AICc
INV.TableOrd = INV.Table[order(INV.Table$AICc,INV.Table$deltaAICc,INV.Table$w_i),]    #reorganize table using typical journal format
INV.TableOrd
