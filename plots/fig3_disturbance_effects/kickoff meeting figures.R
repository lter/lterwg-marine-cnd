##################
###LTER CND Synthesis WG
#
#
###author: mwhite
#######################################
###goal: generate figures for LTER CND WG Kickoff Meeting

library(lubridate)
library(scales)
library(imputeTS)
library(pracma)
library(car)
library(viridis)
library(reshape2)
library(vegan)
library(tidyquant)
library(glmmTMB)
library(DHARMa)
library(stringr)
library(MuMIn)
library(ggeffects)
library(tidyverse)

dat <- read_csv("lter cnd wg projects/MAPmaster_yrs1thru19_speciesnames_CLEAN.csv")
#View(dat)
glimpse(dat)
unique(dat$common_name)

dat$common_name<-str_replace(dat$common_name, fixed(" "), "_") #first get rid of spaces in species names, it causes issues
#unique(dat$common_name)

dat$s_date=as.Date(paste(dat$s.yr,dat$s.mo,dat$s.day,sep="-")) #fix sample date 
dat$s_date

dat$BOUT<-as.factor(paste(dat$SITE,dat$BOUT,sep="_")) #rename to unique bout name
dat$SITE<-as.factor(dat$SITE) #make site a factor
glimpse(dat)

# filter data for sites/consumers of interest - keep NAs for empty --------

rookery <- dat |> 
  filter(CATCHCODE== 'S'|CATCHCODE==  'C'| CATCHCODE == 'NA', #remove irrelavent catchcodes (keep NA for empty bouts)
         SITE == 8 |SITE == 9|SITE==10|SITE==11|SITE==13) |> 
  mutate(catchnumber_mc =                                          
           ifelse(common_name!='Bowfin' & common_name!='Snook'& common_name!='Florida_gar' &
                    common_name!='Largemouth_bass', CATCHNUMBER * 0, CATCHNUMBER))
  
# calculate length-weight relationships for all species -------------------

###length-weight relationship for Common Snook
ind_metrics_snook <- rookery  |>  filter(common_name =='Snook' &!is.na(SL) & !is.na(WEIGHT)) |>  mutate(wwt_g = WEIGHT*1000)
snookallometry<-lm(log10(wwt_g)~log10(as.numeric(SL)),data=ind_metrics_snook) #find LW relationship
ind_metrics_snook$resid<-resid(snookallometry) #identify residuals
SD2_snook<-2*sd(resid(snookallometry))  # identify 2 sd of resuduals
ind_metrics_snook$Outs<-ifelse(abs(ind_metrics_snook$resid) >SD2_snook, T, F) #remove resids with sd over 2 (outliers)
ind_metrics_snook_out_rm<-ind_metrics_snook|>  filter(Outs == FALSE)
snookallometry_out_rm<-lm(log10(wwt_g)~log10(as.numeric(SL)),data=ind_metrics_snook_out_rm) # rerun with trimmed data
summary(snookallometry_out_rm)
confint(snookallometry_out_rm, level = 0.95)
length(ind_metrics_snook_out_rm$SL)

###Coefs
###a = 10^-2.022075 = 0.009504406, b = 3.078241 , r2= 0.987 , n= 1560

###length-weight relationship for Largemouth Bass
ind_metrics_bass <- rookery  |>  filter(common_name =='Largemouth_bass' &!is.na(SL) & !is.na(WEIGHT)) |>  mutate(wwt_g = WEIGHT*1000)
bassallometry<-lm(log10(wwt_g)~log10(as.numeric(SL)),data=ind_metrics_bass) #find LW relationship
ind_metrics_bass$resid<-resid(bassallometry) #identify residuals
SD2_bass<-2*sd(resid(bassallometry))  # identify 2 sd of resuduals
ind_metrics_bass$Outs<-ifelse(abs(ind_metrics_bass$resid) >SD2_bass, T, F) #remove resids with sd over 2 (outliers)
ind_metrics_bass_out_rm<-ind_metrics_bass|>  filter(Outs == FALSE)
bassallometry_out_rm<-lm(log10(wwt_g)~log10(as.numeric(SL)),data=ind_metrics_bass_out_rm) # rerun with trimmed data
summary(bassallometry_out_rm)
confint(bassallometry_out_rm, level = 0.95)
length(ind_metrics_bass_out_rm$SL)

###Coefs
###a = 10^-1.914264 = 0.01218249, b = 3.200356 , r2= 0.976 , n= 3720

###length-weight relationship for Florida Gar - remember to use TL, not SL
ind_metrics_gar <- rookery  |>  filter(common_name =='Florida_gar' &!is.na(TL) & !is.na(WEIGHT)) |>  mutate(wwt_g = WEIGHT*1000)
garallometry<-lm(log10(wwt_g)~log10(as.numeric(TL)),data=ind_metrics_gar) #find LW relationship
ind_metrics_gar$resid<-resid(garallometry) #identify residuals
SD2_gar<-2*sd(resid(garallometry))  # identify 2 sd of resuduals
ind_metrics_gar$Outs<-ifelse(abs(ind_metrics_gar$resid) >SD2_gar, T, F) #remove resids with sd over 2 (outliers)
ind_metrics_gar_out_rm<-ind_metrics_gar|>  filter(Outs == FALSE)
garallometry_out_rm<-lm(log10(wwt_g)~log10(as.numeric(TL)),data=ind_metrics_gar_out_rm) # rerun with trimmed data
summary(garallometry_out_rm)
confint(garallometry_out_rm, level = 0.95)
length(ind_metrics_gar_out_rm$TL)

###Coefs
###a = 10^-3.08617 = 0.000820030, b = 3.43859 , r2= 0.933 , n= 2665

###length-weight relationship for Bowfin - remember to use TL, not SL
ind_metrics_bowfin <- rookery  |>  filter(common_name =='Bowfin' &!is.na(TL) & !is.na(WEIGHT)) |>  mutate(wwt_g = WEIGHT*1000)
bowfinallometry<-lm(log10(wwt_g)~log10(as.numeric(TL)),data=ind_metrics_bowfin) #find LW relationship
ind_metrics_bowfin$resid<-resid(bowfinallometry) #identify residuals
SD2_bowfin<-2*sd(resid(bowfinallometry))  # identify 2 sd of resuduals
ind_metrics_bowfin$Outs<-ifelse(abs(ind_metrics_bowfin$resid) >SD2_bowfin, T, F) #remove resids with sd over 2 (outliers)
ind_metrics_bowfin_out_rm<-ind_metrics_bowfin|>  filter(Outs == FALSE)
bowfinallometry_out_rm<-lm(log10(wwt_g)~log10(as.numeric(TL)),data=ind_metrics_bowfin_out_rm) # rerun with trimmed data
summary(bowfinallometry_out_rm)
confint(bowfinallometry_out_rm, level = 0.95)
length(ind_metrics_bowfin_out_rm$TL)

###Coefs
###a = 10^-2.13312 = 0.007360037, b = 3.07471 , r2= 0.969 , n= 1412

# Apply WL relationships to spp with only SL/TL values, convert kg to g while at it --------

rookery1_snook<-rookery|> filter(common_name=='Snook') |>  mutate(wetbiomass_g = ifelse(!is.na(WEIGHT), WEIGHT*1000, 0.009504406*SL^3.078241))
rookery2_bass<-rookery|> filter(common_name=='Largemouth_bass') |>  mutate(wetbiomass_g = ifelse(!is.na(WEIGHT), WEIGHT*1000,0.01218249*SL^3.200356))
rookery3_gar<-rookery|> filter(common_name=='Florida_gar') |>  mutate(wetbiomass_g = ifelse(!is.na(WEIGHT), WEIGHT*1000,0.000820030*TL^3.43859))
rookery4_bowfin<-rookery|> filter(common_name=='Bowfin') |>  mutate(wetbiomass_g = ifelse(!is.na(WEIGHT), WEIGHT*1000,0.007360037*TL^3.07471))

###bindrows of all species
Allobind<-bind_rows(rookery1_snook,rookery2_bass,rookery3_gar,rookery4_bowfin)

###select dataset without sf species
Allorm<-rookery|>  
  filter(common_name!='Snook'&common_name!='Largemouth_bass'&common_name!='Florida_gar'&common_name!='Bowfin') 

###mean SL of each species
summary(Allobind) # issue with crazy weights, even after removing outliers earlier in script
Allobind |>  
  filter(!is.na(SL))|> group_by(common_name)|>  summarize(n=n(),SL_m=mean(SL,na.rm = T), SL_se=sd(SL,na.rm = T)/n()^0.5)
Allobind |>  
  filter(!is.na(TL))|> group_by(common_name)|>  summarize(n=n(),TL_m=mean(TL,na.rm = T), TL_se=sd(TL,na.rm = T)/n()^0.5)

###bind sf biomass data with map data to have right number of 0s
rookery_cleanish <- bind_rows(Allobind,Allorm)
# glimpse(rookery_cleanish)
# summary(rookery_cleanish)

# boxplot(SL~s_date,data=rookery1_snook)
# boxplot(SL~s_date,data=rookery2_bass)
# boxplot(TL~s_date,data=rookery3_gar)
# boxplot(TL~s_date,data=rookery4_bowfin)
# boxplot(wetbiomass_g~s_date,data=rookery1_snook) 
# boxplot(wetbiomass_g~s_date,data=rookery2_bass)
# boxplot(wetbiomass_g~s_date,data=rookery3_gar)
# boxplot(wetbiomass_g~s_date,data=rookery4_bowfin)
# 
# rookery_badTL <- rookery_cleanish |> 
#   filter(TL >= 100)
# rookery_badSL <- rookery_cleanish |> 
#   filter(SL >= 100)
# rookery_badwwt_g <- rookery_cleanish |> 
#   filter(wetbiomass_g >= 8000)

# impute missing data -----------------------------------------------------

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))# function for mean imputation

#impute based on grouping factors, for this we will impute based on the mean of the same species in the same hydroyear and season
#we could do it by site or bout if we have individuals of same to get a mean for each level of grouping factor.

rookery_mean_imp_spec <- rookery_cleanish |>  
  group_by(HYDROYEAR, common_name) |> 
  mutate(wetbiomass_g = na_if(wetbiomass_g,0)*catchnumber_mc,WEIGHT=na_if(WEIGHT,0)*catchnumber_mc, #multiply by catchnumber because some rows include several individuals shocked
         wetbiomass_g_imp = impute.mean(wetbiomass_g)*catchnumber_mc) |> 
  filter(wetbiomass_g_imp <= 11000)
glimpse(rookery_mean_imp_spec)

rookery_mean_imp_forviewing <-data.frame(rookery_mean_imp_spec) |> 
  select(HYDROYEAR,s_date,catchnumber_mc,SL,TL,SITE,BOUT,SEASON,species,latin_name, common_name,
         wetbiomass_g,wetbiomass_g_imp) |> 
  filter(catchnumber_mc!=0)

summary(rookery_mean_imp_forviewing) #make sure imputations didn't generate crazy-ass weights

# bring in temp data ------------------------------------------------------

#install.packages("dataRetrieval")
library(dataRetrieval) # https://doi-usgs.github.io/dataRetrieval/

#Bottle Creek Temp near Rookery Branch, Everglades NPS 
siteNumber <- "022908295" # found here https://waterdata.usgs.gov/monitoring-location/022908295/#parameterCode=00065&period=P7D
#ChoptankInfo <- readNWISdata(022908295) # don't mind warning, still works
parameterCd <- "00010" # parameter codes listed here -> http://water.nv.gov/hearings/past/Spring%20Valley%202006/exhibits/SNWA/5__Hydrochemistry_and_Geochemistry/Data/USGS/USGS_NWIS/ParameterCodes.htm

BCdata <- readNWISdv(
  siteNumber, parameterCd,
  "2005-01-01", "2023-08-01"
)

glimpse(BCdata)

botcreektemp <- BCdata |> 
  mutate(temp_c = X_00010_00003) |> 
  mutate(s_date = Date) |> 
  select(s_date, temp_c) 

rookery_imp_temp <- inner_join(rookery_mean_imp_spec, botcreektemp)
glimpse(rookery_imp_temp)

# bring in nutrient data --------------------------------------------------
### we will do this another time - https://fce-lter.fiu.edu/data/core/metadata/?packageid=knb-lter-fce.1072.17
### downloaded and in folder as "sr_waterquality_08122023.csv"

# generate excretion estimates --------------------------------------------

### for snook
rookery_exc_snook <- rookery_imp_temp |> 
  filter(common_name == 'Snook') |> 
  mutate(drywt_g = wetbiomass_g_imp*0.0271,
         logN_exc_ug_hr = 1.4610 + 0.6840 * log10(drywt_g) + 0.0246*temp_c - 0.1732 + 0.7804,
         logP_exc_ug_hr = 0.6757 + 0.6840 * log10(drywt_g) + 0.0194*temp_c - 0.4525 + 0.7504,
         N_exc_ug_hr = 10^logN_exc_ug_hr,
         P_exc_ug_hr = 10^logP_exc_ug_hr,
         N_exc_g_hr = N_exc_ug_hr*0.000001,
         P_exc_g_hr = P_exc_ug_hr*0.000001,
         N_exc_ug_day = N_exc_ug_hr*24,
         P_exc_ug_day = P_exc_ug_hr*24,
         N_exc_g_day = N_exc_g_hr*24,
         P_exc_g_day = P_exc_g_hr*24,
         N_exc_mol_hr = N_exc_g_hr*0.055437104941331,
         N_exc_umol_hr = N_exc_mol_hr*1000000,
         P_exc_mol_hr = P_exc_g_hr*0.03228539149637,
         P_exc_umol_hr = P_exc_mol_hr*1000000)

summary(rookery_exc_snook)

### for bass
rookery_exc_bass <- rookery_imp_temp |> 
  filter(common_name == "Largemouth_bass") |> 
  mutate(drywt_g = wetbiomass_g_imp*0.0271,
         logN_exc_ug_hr = 1.4610 + 0.6840 * log10(drywt_g) + 0.0246*temp_c - 0.1732 + 0.7804,
         logP_exc_ug_hr = 0.6757 + 0.6840 * log10(drywt_g) + 0.0194*temp_c - 0.4525 + 0.7504,
         N_exc_ug_hr = 10^logN_exc_ug_hr,
         P_exc_ug_hr = 10^logP_exc_ug_hr,
         N_exc_g_hr = N_exc_ug_hr*0.000001,
         P_exc_g_hr = P_exc_ug_hr*0.000001,
         N_exc_ug_day = N_exc_ug_hr*24,
         P_exc_ug_day = P_exc_ug_hr*24,
         N_exc_g_day = N_exc_g_hr*24,
         P_exc_g_day = P_exc_g_hr*24,
         N_exc_mol_hr = N_exc_g_hr*0.055437104941331,
         N_exc_umol_hr = N_exc_mol_hr*1000000,
         P_exc_mol_hr = P_exc_g_hr*0.03228539149637,
         P_exc_umol_hr = P_exc_mol_hr*1000000)

summary(rookery_exc_bass)

### for gar
rookery_exc_gar <- rookery_imp_temp |> 
  filter(common_name == "Florida_gar") |> 
  mutate(drywt_g = wetbiomass_g_imp*0.0271,
         logN_exc_ug_hr = 1.4610 + 0.6840 * log10(drywt_g) + 0.0246*temp_c - 0.1732 + 0.7804,
         logP_exc_ug_hr = 0.6757 + 0.6840 * log10(drywt_g) + 0.0194*temp_c - 0.4525 + 0.7504,
         N_exc_ug_hr = 10^logN_exc_ug_hr,
         P_exc_ug_hr = 10^logP_exc_ug_hr,
         N_exc_g_hr = N_exc_ug_hr*0.000001,
         P_exc_g_hr = P_exc_ug_hr*0.000001,
         N_exc_ug_day = N_exc_ug_hr*24,
         P_exc_ug_day = P_exc_ug_hr*24,
         N_exc_g_day = N_exc_g_hr*24,
         P_exc_g_day = P_exc_g_hr*24,
         N_exc_mol_hr = N_exc_g_hr*0.055437104941331,
         N_exc_umol_hr = N_exc_mol_hr*1000000,
         P_exc_mol_hr = P_exc_g_hr*0.03228539149637,
         P_exc_umol_hr = P_exc_mol_hr*1000000)

summary(rookery_exc_gar)

### for bowfin

rookery_exc_bowfin <- rookery_imp_temp |> 
  filter(common_name == "Bowfin") |> 
  mutate(drywt_g = wetbiomass_g_imp*0.0271,
         logN_exc_ug_hr = 1.4610 + 0.6840 * log10(drywt_g) + 0.0246*temp_c - 0.1732 + 0.7804,
         logP_exc_ug_hr = 0.6757 + 0.6840 * log10(drywt_g) + 0.0194*temp_c - 0.4525 + 0.7504,
         N_exc_ug_hr = 10^logN_exc_ug_hr,
         P_exc_ug_hr = 10^logP_exc_ug_hr,
         N_exc_g_hr = N_exc_ug_hr*0.000001,
         P_exc_g_hr = P_exc_ug_hr*0.000001,
         N_exc_ug_day = N_exc_ug_hr*24,
         P_exc_ug_day = P_exc_ug_hr*24,
         N_exc_g_day = N_exc_g_hr*24,
         P_exc_g_day = P_exc_g_hr*24,
         N_exc_mol_hr = N_exc_g_hr*0.055437104941331,
         N_exc_umol_hr = N_exc_mol_hr*1000000,
         P_exc_mol_hr = P_exc_g_hr*0.03228539149637,
         P_exc_umol_hr = P_exc_mol_hr*1000000)

summary(rookery_exc_bowfin)

#bindrows of all species
Allobindexc <- bind_rows(rookery_exc_snook, rookery_exc_bass, rookery_exc_gar, rookery_exc_bowfin)

# rookery_exc_viewing <- Allobindexc |> 
#   select(s_date, temp_c, TL, SL, HYDROYEAR, common_name, wetbiomass_g_imp, drywt_g, N_exc_ug_hr, P_exc_ug_hr, N_exc_g_hr, P_exc_g_hr, catchnumber_mc) |> 
#   filter(catchnumber_mc!=0)

#select dataset without mesoconsumer species
Allormexc <- rookery_imp_temp |>  
  filter(common_name!='Snook'&common_name!='Florida_gar'&common_name!='Largemouth_bass'&common_name!='Bowfin') 

#bind mc biomass data with map data to have right number of 0s
rookery_exc <- bind_rows(Allobindexc, Allormexc)

# generate summaries at different levels_similar to "sunfish_comm. --------
# glimpse(rookery_exc)

### summary statistics by each bout and species
mc_bout_sum_sp <- rookery_exc |>  
  group_by(s_date,HYDROYEAR,SITE,BOUT,common_name)|>  
  summarise(distance1=mean(Distance),catch = sum(as.numeric(catchnumber_mc), na.rm = T),
            wt_biomass_g=sum(wetbiomass_g_imp),dry_biomass_g=sum((drywt_g),na.rm=T),N_all_day= sum(N_exc_g_day,na.rm=T), P_all_day= sum(P_exc_g_day,na.rm=T),
            cpue_biomass_100m=sum(wetbiomass_g_imp/distance1*100),cpue_100m=sum((catchnumber_mc/distance1)*100),
            N_100m = sum((N_all_day/distance1)*100,na.rm=T), P_100m = sum((P_all_day/distance1)*100,na.rm=T),
            N_umol_hr = sum(N_exc_umol_hr, na.rm = T), P_umol_hr = sum(P_exc_umol_hr),
            N_100m_umol_hr = sum((N_umol_hr/distance1)*100,na.rm = T), 
            P_100m_umol_hr = sum((P_umol_hr/distance1)*100,na.rm=T),
            cpue_dry_biomass_g = sum(((dry_biomass_g/distance1)*100), na.rm=T),
            temp_mean=mean(as.numeric(temp_c),na.rm=T),
            sal_mean=mean(as.numeric(SALINITY),na.rm=T))
summary(mc_bout_sum_sp)

### Bowfin thing came back to bite me in the ass :( - need to fix tomorrow, but otherwise looking good and the code below should be good to go!
### fixed on 08/13/2023 - just went into excel, corrected, and ran through code again so not future issue

### summary statistics by each bout (summarizing all mc species)
mc_bout_sum <- rookery_exc |>  
  group_by(s_date,HYDROYEAR,SITE,BOUT)|>  
  summarise(distance1=mean(Distance),catch = sum(as.numeric(catchnumber_mc), na.rm = T),
            wt_biomass_g=sum(wetbiomass_g_imp),dry_biomass_g=sum((drywt_g),na.rm=T),N_all_day= sum(N_exc_g_day,na.rm=T), P_all_day= sum(P_exc_g_day,na.rm=T),
            cpue_biomass_100m=sum(wetbiomass_g_imp/distance1*100),cpue_100m=sum((catchnumber_mc/distance1)*100),
            N_100m = sum((N_all_day/distance1)*100,na.rm=T), P_100m = sum((P_all_day/distance1)*100,na.rm=T),
            N_umol_hr = sum(N_exc_umol_hr, na.rm = T), P_umol_hr = sum(P_exc_umol_hr),
            N_100m_umol_hr = sum((N_umol_hr/distance1)*100,na.rm = T), 
            P_100m_umol_hr = sum((P_umol_hr/distance1)*100,na.rm=T),
            cpue_dry_biomass_g = sum(((dry_biomass_g/distance1)*100), na.rm=T),
            temp_mean=mean(as.numeric(temp_c),na.rm=T),
            sal_mean=mean(as.numeric(SALINITY),na.rm=T))

### summary statistics by each site (summarizing all mc species)
glimpse(mc_bout_sum)

mc_sample_site_mean <- mc_bout_sum |>  
  group_by(s_date,HYDROYEAR,SITE) |>  
  summarise(n=n(), m_cpue_100m = mean(cpue_100m), cpue_100m_se= sd(cpue_100m)/n^0.5, 
            m_cpue_biomass_100m = mean(cpue_biomass_100m), cpue_biomass_100m_se = sd(cpue_biomass_100m)/n^0.5,
            m_cpue_dry_biomass_g = mean(cpue_dry_biomass_g), cpue_dry_biomass_g_se = sd(cpue_dry_biomass_g)/n^0.5,
            m_N_100m_g = mean(N_100m), N_100m_g_se = sd(N_100m)/n^0.5,
            m_P_100m_g = mean(P_100m), P_100m_g_se = sd(P_100m)/n^0.5,
            m_N_100m_umol = mean(N_100m_umol_hr), N_100m_umol_se = sd(N_100m_umol_hr)/n^0.5,
            m_P_100m_umol = mean(P_100m_umol_hr), P_100m_umol_se = sd(P_100m_umol_hr)/n^0.5,
            m_temp = mean(temp_mean), temp_se = sd(temp_mean)/n^0.5)

summary(mc_sample_site_mean)
#####

EXAMPLE_WG <- mc_bout_sum |> 
  

### summary statistics by sampling date (site mean)

mc_sample_date_mean <- mc_sample_site_mean |>  
  group_by(s_date,HYDROYEAR) |>  
  summarise(n=n(), dm_cpue_100m = mean(m_cpue_100m), d_cpue_100m_se= sd(m_cpue_100m)/n^0.5, 
            dm_cpue_biomass_100m = mean(m_cpue_biomass_100m), d_cpue_biomass_100m_se = sd(m_cpue_biomass_100m)/n^0.5,
            dm_cpue_dry_biomass_g = mean(m_cpue_dry_biomass_g), d_cpue_dry_biomass_g_se = sd(m_cpue_dry_biomass_g)/n^0.5,
            dm_N_100m_g = mean(m_N_100m_g), d_N_100m_g_se = sd(m_N_100m_g)/n^0.5,
            dm_P_100m_g = mean(m_P_100m_g), d_P_100m_g_se = sd(m_P_100m_g)/n^0.5,
            dm_N_100m_umol = mean(m_N_100m_umol), d_N_100m_umol_se = sd(m_N_100m_umol)/n^0.5,
            dm_P_100m_umol = mean(m_P_100m_umol), d_P_100m_umol_se = sd(m_P_100m_umol)/n^0.5,
            dm_temp = mean(m_temp), d_temp_se = sd(m_temp)/n^0.5)

summary(mc_sample_date_mean)

mc_sample_date_mean <- mc_sample_date_mean |> 
  mutate(dm_N_100m_ug = dm_N_100m_g*1000000,
         d_N_100m_ug_se = d_N_100m_g_se*1000000,
         dm_P_100m_ug = dm_P_100m_g*1000000,
         d_P_100m_ug_se = d_P_100m_g_se*1000000)

summary(mc_sample_date_mean)

#write.csv(mc_sample_date_mean,"lter cnd wg projects/rookery_exc_sample_date_means_08132023.csv" )

# plotting n/100m for all mesoconsumers  ----------------------------------

mc_cpue_plot_yr <- ggplot(mc_sample_date_mean, aes(x=as.Date(s_date) ,y = as.numeric(dm_cpue_100m))) +   geom_line()+
  geom_point()+theme_bw()+scale_color_manual(values=c("gray60","black"))+ 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')+
  labs(x = "Sample date", y = 'Mesoconsumer CPUE (n/100m)')
mc_cpue_plot_yr

mc_cpue_plot_yr1 <- ggplot(mc_sample_date_mean, aes(x=as.Date(s_date), y = as.numeric(dm_cpue_100m), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Sample Date", 
       y = "Mesoconusmer CPUE (n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_cpue_plot_yr1
mc_sample_date_mean_interp <- na_interpolation(mc_sample_date_mean) # weird missing data, maybe supposed to be zeros... double check later, but going with this for now!

mc_cpue_plot_yr2 <- ggplot(mc_sample_date_mean_interp, aes(x=as.Date(s_date), y = as.numeric(dm_cpue_100m), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Sample Date", 
       y = "Mesoconusmer CPUE (n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_cpue_plot_yr2 

# ggsave(filename='lter cnd wg projects/plots/mc_cpue_plot_yr_n.png', plot = mc_cpue_plot_yr2,
#        scale = 2.5,
#        width = 6,
#        height = 3,
#        units = c("cm"),
#        dpi = 300)

# plotting cpue - biomass/100m for all mesoconsumers ----------------------

mc_sample_date_mean_interp_biomass <- mc_sample_date_mean_interp |> 
  mutate(biomass_kg_100m = dm_cpue_biomass_100m*0.001)

mc_cpue_bm_plot_yr <- ggplot(mc_sample_date_mean_interp_biomass, aes(x=as.Date(s_date) ,y = as.numeric(biomass_kg_100m))) + geom_line()+
  geom_point()+theme_bw()+scale_color_manual(values=c("gray60","black"))+ 
  #geom_errorbar(data = sf_sampledate_mean_plot,aes(ymin =  dm_kcal_100m-d_kcal_100m_se ,ymax =  dm_kcal_100m+ d_kcal_100m_se ),width=0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),panel.grid.minor = element_blank(),legend.position = "none") + 
  #facet_wrap(. ~ hydroyrwetdry,scales = "free_x",ncol=3)+ 
  scale_x_date(date_labels = "%Y",breaks ='1 year')+
  labs(x = "Sample Date", y = 'Mesoconsumer Biomass (kg/100m)')
mc_cpue_bm_plot_yr

# plotting by species? ----------------------------------------------------

### summary statistics by each bout (summarizing all mc species)
mc_bout_sum_sp <- rookery_exc |>  
  group_by(s_date,HYDROYEAR,SITE,BOUT, common_name)|>  
  summarise(distance1=mean(Distance),catch = sum(as.numeric(catchnumber_mc), na.rm = T),
            wt_biomass_g=sum(wetbiomass_g_imp),dry_biomass_g=sum((drywt_g),na.rm=T),N_all_day= sum(N_exc_g_day,na.rm=T), P_all_day= sum(P_exc_g_day,na.rm=T),
            cpue_biomass_100m=sum(wetbiomass_g_imp/distance1*100),cpue_100m=sum((catchnumber_mc/distance1)*100),
            N_100m = sum((N_all_day/distance1)*100,na.rm=T), P_100m = sum((P_all_day/distance1)*100,na.rm=T),
            N_umol_hr = sum(N_exc_umol_hr, na.rm = T), P_umol_hr = sum(P_exc_umol_hr),
            N_100m_umol_hr = sum((N_umol_hr/distance1)*100,na.rm = T), 
            P_100m_umol_hr = sum((P_umol_hr/distance1)*100,na.rm=T),
            cpue_dry_biomass_g = sum(((dry_biomass_g/distance1)*100), na.rm=T),
            temp_mean=mean(as.numeric(temp_c),na.rm=T),
            sal_mean=mean(as.numeric(SALINITY),na.rm=T))

### summary statistics by each site AND EACH SPECIES
#glimpse(mc_bout_sum_sp)

mc_sample_site_mean_sp <- mc_bout_sum_sp |>  
  group_by(s_date,HYDROYEAR,SITE, common_name) |>  
  summarise(n=n(), m_cpue_100m = mean(cpue_100m), cpue_100m_se= sd(cpue_100m)/n^0.5, 
            m_cpue_biomass_100m = mean(cpue_biomass_100m), cpue_biomass_100m_se = sd(cpue_biomass_100m)/n^0.5,
            m_cpue_dry_biomass_g = mean(cpue_dry_biomass_g), cpue_dry_biomass_g_se = sd(cpue_dry_biomass_g)/n^0.5,
            m_N_100m_g = mean(N_100m), N_100m_g_se = sd(N_100m)/n^0.5,
            m_P_100m_g = mean(P_100m), P_100m_g_se = sd(P_100m)/n^0.5,
            m_N_100m_umol = mean(N_100m_umol_hr), N_100m_umol_se = sd(N_100m_umol_hr)/n^0.5,
            m_P_100m_umol = mean(P_100m_umol_hr), P_100m_umol_se = sd(P_100m_umol_hr)/n^0.5,
            m_temp = mean(temp_mean), temp_se = sd(temp_mean)/n^0.5)

summary(mc_sample_site_mean_sp)

### summary statistics by sampling date (site mean) AND SPECIES

mc_sample_date_mean_sp <- mc_sample_site_mean_sp |>  
  group_by(s_date,HYDROYEAR, common_name) |>  
  summarise(n=n(), dm_cpue_100m = mean(m_cpue_100m), d_cpue_100m_se= sd(m_cpue_100m)/n^0.5, 
            dm_cpue_biomass_100m = mean(m_cpue_biomass_100m), d_cpue_biomass_100m_se = sd(m_cpue_biomass_100m)/n^0.5,
            dm_cpue_dry_biomass_g = mean(m_cpue_dry_biomass_g), d_cpue_dry_biomass_g_se = sd(m_cpue_dry_biomass_g)/n^0.5,
            dm_N_100m_g = mean(m_N_100m_g), d_N_100m_g_se = sd(m_N_100m_g)/n^0.5,
            dm_P_100m_g = mean(m_P_100m_g), d_P_100m_g_se = sd(m_P_100m_g)/n^0.5,
            dm_N_100m_umol = mean(m_N_100m_umol), d_N_100m_umol_se = sd(m_N_100m_umol)/n^0.5,
            dm_P_100m_umol = mean(m_P_100m_umol), d_P_100m_umol_se = sd(m_P_100m_umol)/n^0.5,
            dm_temp = mean(m_temp), d_temp_se = sd(m_temp)/n^0.5)

summary(mc_sample_date_mean_sp)

mc_sample_date_mean_sp <- mc_sample_date_mean_sp |> 
  mutate(dm_N_100m_ug = dm_N_100m_g*1000000,
         d_N_100m_ug_se = d_N_100m_g_se*1000000,
         dm_P_100m_ug = dm_P_100m_g*1000000,
         d_P_100m_ug_se = d_P_100m_g_se*1000000)

summary(mc_sample_date_mean)
glimpse(mc_sample_date_mean)

mc_sample_date_mean_sp <- mc_sample_date_mean_sp |> 
  mutate(dm_N_m_umol = dm_N_100m_umol*0.01,
         d_N_m_umol_se = d_N_100m_umol_se*0.01,
         dm_P_m_umol = dm_P_100m_umol*0.01,
         d_P_m_umol_se = d_P_100m_umol_se*0.01)

summary(mc_sample_date_mean)
glimpse(mc_sample_date_mean)

# write.csv(mc_sample_date_mean,"lter cnd wg projects/rookery_exc_sample_date_SPECIES_means_08132023.csv" )
# write.csv(mc_sample_date_mean,"lter cnd wg projects/rookery_exc_sample_date_SPECIES_means_09122023.csv" )

mc_sample_date_mean_sp_interp <- na_interpolation(mc_sample_date_mean_sp) # weird missing data, maybe supposed to be zeros... double check later, but going with this for now!

mc_sample_date_mean_sp_interp_biomass <- mc_sample_date_mean_sp_interp |> 
  mutate(biomass_kg_100m = dm_cpue_biomass_100m*0.001)

# plotting cpue n/100 m by SPECIES ----------------------------------------

mc_cpue_plot_yr2_SP <- ggplot(mc_sample_date_mean_sp_interp, aes(x=as.Date(s_date), y = as.numeric(dm_cpue_100m), group = 1)) + 
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  facet_wrap(~common_name) +
  labs(x = "Sample Date", 
       y = "Mesoconusmer CPUE (n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_cpue_plot_yr2_SP

# ggsave(filename='lter cnd wg projects/plots/mc_cpue_plot_yr_n_BYSPECIES.png', plot = mc_cpue_plot_yr2_SP,
#        scale = 2.5,
#        width = 9,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# plotting cpue kg/100 m by SPECIES ---------------------------------------

mc_cpue_plot_yr2_biomass_SP <- ggplot(mc_sample_date_mean_sp_interp_biomass, aes(x=as.Date(s_date), y = as.numeric(biomass_kg_100m), group = 1)) + 
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  facet_wrap(~common_name) +
  labs(x = "Sample Date", 
       y = "Mesoconusmer Biomass CPUE (kg/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_cpue_plot_yr2_biomass_SP

# ggsave(filename='lter cnd wg projects/plots/mc_cpue_plot_yr_biomass_BYSPECIES.png', plot = mc_cpue_plot_yr2_biomass_SP,
#        scale = 2.5,
#        width = 9,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# plotting N exc/100m by SPECIES ------------------------------------------
#glimpse(mc_sample_date_mean_interp_biomass)

### grams of N/100m/day/species
mc_N_exc_SP <- ggplot(mc_sample_date_mean_sp_interp_biomass, aes(x=as.Date(s_date), y = as.numeric(dm_N_100m_g), group = 1)) + 
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  facet_wrap(~common_name) +
  labs(x = "Sample Date", 
       y = "Nitrogen Excretion (g/100m/day)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_N_exc_SP

# ggsave(filename='lter cnd wg projects/plots/mc_N_exc_BYSPECIES.png', plot = mc_N_exc_SP,
#        scale = 2.5,
#        width = 9,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### just trying to do it not facet-wrapped, looks really busy
mc_N_exc_SP1 <- ggplot(mc_sample_date_mean_sp_interp_biomass, aes(x=as.Date(s_date), y = as.numeric(dm_N_100m_g),
                                                                  group=common_name, color=common_name)) + 
  geom_line(linewidth = 1.0) +
  scale_color_viridis(discrete = TRUE) +
  #geom_point(size = 1.0) +
  #geom_smooth() +
  #facet_wrap(~common_name) +
  labs(x = "Sample Date", 
       y = "Nitrogen Excretion (g/100m/day)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.justification = "right") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_N_exc_SP1

# ggsave(filename='lter cnd wg projects/plots/mc_N_exc_BYSPECIES_nofacet.png', plot = mc_N_exc_SP1,
#        scale = 2.5,
#        width = 9,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# plotting P exc/100m by SPECIES ------------------------------------------
#glimpse(mc_sample_date_mean_interp_biomass)

### grams of P/100m/day/species
mc_P_exc_SP <- ggplot(mc_sample_date_mean_sp_interp_biomass, aes(x=as.Date(s_date), y = as.numeric(dm_P_100m_g), group = 1)) + 
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  facet_wrap(~common_name) +
  labs(x = "Sample Date", 
       y = "Phosphorus Excretion (g/100m/day)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_P_exc_SP

# ggsave(filename='lter cnd wg projects/plots/mc_P_exc_BYSPECIES.png', plot = mc_P_exc_SP,
#        scale = 2.5,
#        width = 9,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### just trying to do it not facet-wrapped, looks really busy
mc_P_exc_SP1 <- ggplot(mc_sample_date_mean_sp_interp_biomass, aes(x=as.Date(s_date), y = as.numeric(dm_P_100m_g),
                                                                  group=common_name, color=common_name)) + 
  geom_line(linewidth = 1.0) +
  scale_color_viridis(discrete = TRUE) +
  #geom_point(size = 1.0) +
  #geom_smooth() +
  #facet_wrap(~common_name) +
  labs(x = "Sample Date", 
       y = "Phosphorus Excretion (g/100m/day)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.justification = "right") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_P_exc_SP1

# ggsave(filename='lter cnd wg projects/plots/mc_P_exc_BYSPECIES_nofacet.png', plot = mc_P_exc_SP1,
#        scale = 2.5,
#        width = 9,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)


# Plotting everything above in umol/100m/hr -------------------------------

### umols of N/100m/hr/species
mc_N_exc_SP_umol <- ggplot(mc_sample_date_mean_sp_interp_biomass, aes(x=as.Date(s_date), y = as.numeric(dm_N_100m_umol), group = 1)) + 
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  facet_wrap(~common_name) +
  labs(x = "Sample Date", 
       y = "Nitrogen Excretion (umol/100m/hr)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_N_exc_SP_umol

# ggsave(filename='lter cnd wg projects/plots/umol_N_hr_100m.png', 
#        plot = last_plot(),
#        scale = 2.5,
#        width = 9,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# plotting P exc/100m by SPECIES ------------------------------------------
#glimpse(mc_sample_date_mean_interp_biomass)

### umols of P/100m/hr/species
mc_P_exc_SP_umol <- ggplot(mc_sample_date_mean_sp_interp_biomass, aes(x=as.Date(s_date), y = as.numeric(dm_P_100m_umol), group = 1)) + 
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  facet_wrap(~common_name) +
  labs(x = "Sample Date", 
       y = "Phosphorus Excretion (umol/100m/hr)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_P_exc_SP_umol

# ggsave(filename='lter cnd wg projects/plots/umol_P_hr_100m.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 9,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# plotting everything above as umol/m/hr ----------------------------------

### umols of N/m/hr/species
mc_N_exc_SP_umol_meter <- ggplot(mc_sample_date_mean_sp_interp_biomass, aes(x=as.Date(s_date), y = as.numeric(dm_N_m_umol), group = 1)) + 
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  facet_wrap(~common_name) +
  labs(x = "Sample Date", 
       y = "Nitrogen Excretion (umol/m/hr)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_N_exc_SP_umol_meter

# ggsave(filename='lter cnd wg projects/plots/umol_N_hr_m.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 9,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# plotting P exc/100m by SPECIES ------------------------------------------
#glimpse(mc_sample_date_mean_interp_biomass)

### umols of P/100m/hr/species
mc_P_exc_SP_umol_meter <- ggplot(mc_sample_date_mean_sp_interp_biomass, aes(x=as.Date(s_date), y = as.numeric(dm_P_m_umol), group = 1)) + 
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  facet_wrap(~common_name) +
  labs(x = "Sample Date", 
       y = "Phosphorus Excretion (umol/m/hr)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
mc_P_exc_SP_umol_meter

# ggsave(filename='lter cnd wg projects/plots/umol_P_hr_m.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 9,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# replicating peters et al 2019 figure ------------------------------------

# ACTUAL CONTRIBUTION -----------------------------------------------------

stackedbar_dat <- mc_sample_date_mean_sp_interp_biomass |> 
  group_by(HYDROYEAR,common_name) |> 
  summarize(mean_n = mean(dm_N_m_umol),
            mean_p = mean(dm_P_m_umol))

#Prepare names for plotting

stackedbar_dat$common_name <- as.factor(stackedbar_dat$common_name)
stackedbar_dat$common_name <- recode_factor(stackedbar_dat$common_name, 
                                            Florida_gar = "Florida Gar",
                                            Largemouth_bass = "Largemouth Bass",
                                            Snook = "Common Snook")
stackedbar_dat <- stackedbar_dat |> 
  rename("Species" = "common_name")

glimpse(stackedbar_dat)

N_exc_umol_bar <- ggplot(stackedbar_dat, aes(fill = Species, x=HYDROYEAR, 
                                             y = as.numeric(mean_n))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Areal N Excretion (μmol/m/hr)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") 
N_exc_umol_bar

# ggsave(filename='lter cnd wg projects/plots/umol_N_hr_m_STACKEDbar.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

P_exc_umol_bar <- ggplot(stackedbar_dat, aes(fill = Species, x=HYDROYEAR, 
                                             y = as.numeric(mean_p))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Areal P Excretion (μmol/m/hr)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") 
P_exc_umol_bar

# ggsave(filename='lter cnd wg projects/plots/umol_P_hr_m_STACKEDbar.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)


# PERCENT CONTRIBUTION STACKED BAR ----------------------------------------

N_exc_umol_bar1 <- ggplot(stackedbar_dat, aes(fill = Species, x=HYDROYEAR, 
                                             y = as.numeric(mean_n))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Areal N Excretion (% Contribution)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") 
N_exc_umol_bar1

# ggsave(filename='lter cnd wg projects/plots/umol_N_hr_m_STACKEDbar_perCONT.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)


P_exc_umol_bar1 <- ggplot(stackedbar_dat, aes(fill = Species, x=HYDROYEAR, 
                                             y = as.numeric(mean_p))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Areal P Excretion (% Contribution)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top")
P_exc_umol_bar1

# ggsave(filename='lter cnd wg projects/plots/umol_P_hr_m_STACKEDbar_perCONT.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# TALL BOIS FOR OCT 17 2023 MEETING ---------------------------------------

N_exc_umol_bar <- ggplot(stackedbar_dat, aes(fill = Species, x=HYDROYEAR, 
                                             y = as.numeric(mean_n))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Areal N Excretion (μmol/m/hr)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") 
N_exc_umol_bar

# ggsave(filename='lter cnd wg projects/plots/umol_N_hr_m_STACKEDbar_TALL.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 7,
#        units = c("cm"),
#        dpi = 300)

N_exc_umol_bar1 <- ggplot(stackedbar_dat, aes(fill = Species, x=HYDROYEAR, 
                                              y = as.numeric(mean_n))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Areal N Excretion (% Contribution)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") 
N_exc_umol_bar1

# ggsave(filename='lter cnd wg projects/plots/umol_N_hr_m_STACKEDbar_perCONT_TALL.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 7,
#        units = c("cm"),
#        dpi = 300)

########################## PHOSPHORUS

P_exc_umol_bar <- ggplot(stackedbar_dat, aes(fill = Species, x=HYDROYEAR, 
                                             y = as.numeric(mean_p))) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Areal P Excretion (μmol/m/hr)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") 
P_exc_umol_bar

# ggsave(filename='lter cnd wg projects/plots/umol_P_hr_m_STACKEDbar_TALL.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 7,
#        units = c("cm"),
#        dpi = 300)

P_exc_umol_bar1 <- ggplot(stackedbar_dat, aes(fill = Species, x=HYDROYEAR, 
                                              y = as.numeric(mean_p))) + 
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Hydrologic Year", 
       y = "Mean Annual Areal P Excretion (% Contribution)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "top") 
P_exc_umol_bar1

# ggsave(filename='lter cnd wg projects/plots/umol_P_hr_m_STACKEDbar_perCONT_TALL.png',
#        plot = last_plot(),
#        scale = 2.5,
#        width = 7,
#        height = 7,
#        units = c("cm"),
#        dpi = 300)
