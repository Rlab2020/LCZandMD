#######################################################################
###--------------------------------------------------------------------
### LOADING PACKAGES ###
setwd("/Users/guuuuu/Desktop")

packages=c('reshape2','dplyr','openair','zoo','car','foreign','stats','data.table',
           'stringr','scales','readxl','writexl','compareGroups','survival','survminer',
           'lme4','lmerTest','car','glmnet','broom','caret','rms','xgboost','impute','mediation')
# lapply(packages, install.packages, character.only=T)
lapply(packages, require, character.only=T)
# Appoint Func
select <- dplyr::select
rename <- dplyr::rename


#######################################################################
###--------------------------------------------------------------------
### LOADING DATA ### 501856
load("/Users/guuuuu/Desktop/LCZ_mh/data/data_lcz_mh.rda")
data0 <- data_lcz_mh


#######################################################################
###--------------------------------------------------------------------
### DATA PREPARATION ### FULL DATASET 502369
data0 <- data0 %>%  filter(!is.na(lcz0)) ### REMOVE PARTICIAPNTS WITHOUT EXPOSURE DATA ### 442083
data0 <- data0 %>% filter(md_time>0) %>% filter(mdp!=1) ### REMOVE PREVALENT CASES WITH MD ### 359106
data0 <- data0 %>% filter(city=="urban") ### REMOVE PARTICIPANTS RESIDING IN RURAL AREAS ### 347662
data0 <- data0 %>% filter(unfollow==0) ### REMOVE PARTICIAPNTS WITHOUT FOLLOW-UP DATA ### 346860


#######################################################################
###--------------------------------------------------------------------
### VARIABLE TRANSFORMATION ###
### AGE: <=59 and >=60
data0 <- data0 %>% mutate(age0 = case_when(age<=59~"level1",T~"level2"))
data0$age0 <- factor(data0$age0, levels=c("level1","level2"))
### SES
data0$ses <- factor(data0$ses, levels = c("1","2","3"), labels = c("high", "moderate", "low"))
### WHR: poor - male+0.9 and female+0.85
data0 <- data0 %>% mutate(whr0 = case_when(is.na(whr)~NA_character_,whr>=0.9&sex=="male"~"poor",whr>=0.85&sex=="female"~"poor",T~"ideal")) 
data0$whr0 <- factor(data0$whr0, levels=c("ideal","poor"))
### BMI: <18.5, 18.5-24.9, 25.0-29.9, >=30.0
data0 <- data0 %>% mutate(bmi0 = case_when(is.na(bmi)~NA_character_,bmi<18.5~"level1", bmi>=18.5&bmi<=24.9~"level2", bmi>=25.0&bmi<=29.9~"level3", T~"level4")) 
data0$bmi0 <- factor(data0$bmi0, levels=c("level1","level2","level3","level4"))

