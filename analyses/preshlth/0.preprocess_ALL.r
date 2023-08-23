#install.packages('tidyverse')
library(tidyverse)

getwd()
# Ye: un-comment this next line to change path to your own
setwd('C:/Users/55484/OneDrive - ICF/Documents/ADIA_S13') 

#(using 08.20, flat data file with updated race variable)
dat <- read.csv('data/finalvar_08202023.csv')
summary(dat)
summary(dat$w)
summary(dat$bneedin)
summary(dat$mloveaf)


### 1.- Cleaning, recoding
dat$id  <- dat$childid
dat$yob <- dat$biryear_xrnd
dat$sex <- dat$ygender_xrnd

dat$sex <- dat$ygender_xrnd

#replacing income with adjusted income
dat$mhhinco <-  dat$mhhinco_adj

#sensitivity/discrimination indicator
#dat$DISC <- dat$sensitivityRES
#changing DISC to be the raw reason for discrimination variable 02/18/2023 as sensativity analysis was null
dat$DISC <- dat$discrim_reason
table(dat$sensitivityNOTRES, useNA = "ifany")
table(dat$sensitivityRES, useNA = "ifany")

#discrimination indicator tables
table(dat$discrim_reason, useNA = "ifany")
table(dat$sensitivityRES, useNA = "ifany")
table(dat$sensitivityNOTRES, useNA = "ifany")
table(dat$discrim_reason, dat$sensitivityRES)
table(dat$discrim_reason, dat$sensitivityNOTRES)

mean(is.na(dat$sensitivityRES))
mean(is.na(dat$sensitivityNOTRES))

dat$female <- dat$sex - 1

dat$anyrace <- as.integer(apply(
  dat[, c('white','black','asian_nhpi','othrace')], 1,
  sum, na.rm = TRUE) >= 2)

dat$race[dat$white ==1] <- 2
dat$race[dat$black ==1] <- 3
dat$race[dat$asian_nhpi ==1] <- 4
dat$race[dat$othrace == 1] <- 5
dat$race[dat$hisp ==1] <- 1
dat$race[dat$anyrace == 1] <- 6
#dat$race[is.na(dat$race)]<-9

table(dat$race)

##factors
dat$race <- factor(dat$race)


head(dat)
#view(dat[,c('white','black','asian_nhpi','othrace', 'hisp', 'race')])

###Cohort
dat$ageo <- 2018 - dat$yob
#Ye: this is cohort with anotehr name

#age grp-0-17 18-24; 25-29; 30-34; 35-39; 40-50
dat$agegrp[dat$ageo <= 17 & dat$ageo >= 0]  <- 1
dat$agegrp[dat$ageo <= 24 & dat$ageo >= 18] <- 2
dat$agegrp[dat$ageo <= 29 & dat$ageo >= 25] <- 3
dat$agegrp[dat$ageo <= 34 & dat$ageo >= 30] <- 4
dat$agegrp[dat$ageo <= 39 & dat$ageo >= 35] <- 5
dat$agegrp[dat$ageo <= 50 & dat$ageo >= 40] <- 6
table(dat$agegrp)
#view(dat[, c('ageo', 'agegrp')])

#urban rural
table(dat$urbnrur)
dat$rural <- as.numeric(dat$urbnrur == 0)
dat$mixur <- as.numeric(dat$urbnrur == 2)


#===============================================================================

#----------------------------------------------------------------------------
### any Exposure
#----------------------------------------------------------------------------
anyACE_list  <- c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
                  'msubstu',  'mmental',  'DISC',  'loveaff',  'incarce',
                  'divorce',  'physabu',  'subsuse',  'mentill')
#any_ACE[!any_ACE %in% colnames(dat)]
apply(is.na(dat[, anyACE_list]), 2, mean)

dat$anyACE <- as.integer(apply(
  dat[, anyACE_list], 1, sum, na.rm = TRUE) >= 1)
table(dat$anyACE, useNA = 'ifany')

# was your intention to set anyACE to NA (instead of 0) when any individual variable is missing?
# in that case we could use this
dat$anyACE[dat$anyACE == 0 & !complete.cases(dat[, anyACE_list])] <- NA
table(dat$anyACE, useNA = 'ifany')

#view(dat[,c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
#            'msubstu',  'mmental',  'DISC',  'loveaff',  'incarce',
#            'divorce',  'physabu',  'subsuse',  'mentill', 'anyACE')])
#view(dat[,is.na(c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
#                  'msubstu',  'mmental',  'DISC',  'loveaff',  'incarce',
#                  'divorce',  'physabu',  'subsuse',  'mentill', 'anyACE'))])

#----------------------------------------------------------------------------
### any traditional ACE
#----------------------------------------------------------------------------
anyACE_T_list  <- c('incarce', 'divorce',  'physabu',  'subsuse',  'mentill')
apply(is.na(dat[, anyACE_T_list]), 2, mean)

dat$anyACE_T <- as.integer(apply(
  dat[, anyACE_T_list], 1, sum, na.rm = TRUE) >= 1)
table(dat$anyACE_T, useNA = 'ifany')

dat$anyACE_T[dat$anyACE_T == 0 & !complete.cases(dat[, anyACE_T_list])] <- NA
table(dat$anyACE_T, useNA = 'ifany')


#----------------------------------------------------------------------------
#any expanded ACE
#----------------------------------------------------------------------------
anyACE_E_list  <- c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
          'msubstu',  'mmental',  'DISC',  'loveaff')
apply(is.na(dat[, anyACE_E_list]), 2, mean)

dat$anyACE_E <- as.integer(apply(
  dat[, anyACE_E_list], 1, sum, na.rm = TRUE) >= 1)
dat$anyACE_E[dat$anyACE_E == 0 & !complete.cases(dat[, anyACE_E_list])] <- NA
table(dat$anyACE_E, useNA = "ifany")

#view(dat[,c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
#            'msubstu',  'mmental',  'DISC',  'loveaff', 'anyACE_E', 
#            'incarce', 'divorce',  'physabu',  'subsuse',  'mentill', 'anyACE_T','anyACE')])



summary(dat)


#01/17/23 Ye: 
# Should we drop cases with unknown ACE_T expousure status?
# The causal tree needs complete info on ACE_T

#dat <-
#  dat %>%
#  filter(!is.na(anyACE_T))
# summary(dat)

#----------------------------------------------------------------------------
### 2.- define a training sample---------------------------------------------
#----------------------------------------------------------------------------

#simple random sample
#dat$ts  <- dat$id%in%sample(unique(dat$id), length(unique(dat$id))/2)

###stratfied random sample
#how to define strata? 
#stratifed random sample from sex, race, gender, ruralm, any traditional ace
set.seed(0203)
dat <-
  dat %>%
  mutate(strata = paste0(female,
    race, cut(yob, 3), rural, anyACE_T)) %>%
  group_by(strata) %>%
  mutate(training.sample = 
    if_else(id %in% sample(unique(id), round(n_distinct(id) / 2)), 1, 0)
    ) %>%
  ungroup



n_distinct(dat$strata)
table(dat$strata, dat$training.sample)
table(dat$training.sample)
#ideally training sample is equal to valdation sample

###01/17/23 
### Ye: by stratifying by traditional ACE
# we can run this process only once
# since it does not change with the definition of DISC


saveRDS(dat,'data/finalvar.Rds')
#saveRDS(dat,'data/finalvar_NOTRES.Rds')
#saveRDS(dat,'C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_RES.Rds')
