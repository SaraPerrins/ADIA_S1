library(tidyverse)

#i <- 'RES'
i <- 'NOTRES'



dat <- read.csv('C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_01.09.2023.csv')
summary(dat)
### 1.- Cleaning, recoding
dat$id <-dat$childid
dat$yob<-dat$biryear_xrnd
dat$sex<-dat$ygender_xrnd

#sensitivity/discrimination indicator

#dat$DISC <- dat$sensitivityRES
dat$DISC <- dat$sensitivityNOTRES


#discrimination indicator
table(dat$discrim_reason, useNA='ifany')
table(dat$sensitivityRES, useNA='ifany')
table(dat$sensitivityNOTRES, useNA='ifany')
table(dat$discrim_reason, dat$sensitivityRES)
table(dat$discrim_reason, dat$sensitivityNOTRES)

dat$female <-dat$sex-1

dat$anyrace <- as.integer(apply(
  dat[, c('white','black','asian_nhpi','asian','othrace')], 1, sum, na.rm = TRUE) >= 2)

dat$race[dat$white==1]<-2
dat$race[dat$black==1]<-3
dat$race[dat$asian_nhpi==1]<-4
dat$race[dat$asian==1]<-5
dat$race[dat$othrace==1]<-6
dat$race[dat$hisp==1]<-1
dat$race[dat$anyrace==1]<-7
#dat$race[is.na(dat$race)]<-9

##factors
dat$race <- factor(dat$race)

head(dat)
#view(dat[,c('white','black','asian_nhpi','asian','othrace', 'hisp', 'race')])


#any Exposure
dat$anyACE <- as.integer(apply(
  dat[, c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
          'msubstu',  'mmental',  'DISC',  'loveaff',  'incarce',
          'divorce',  'physabu',  'subsuse',  'mentill')], 1, sum, na.rm = TRUE) >= 1)


dat$anyACE[dat$commstr==0  & dat$ecstand==0  & dat$bneedin==0  & dat$mloveaf==0  & dat$mphysab==0  &
             dat$msubstu==0  & dat$mmental==0  & dat$DISC==0  & dat$loveaff==0  & dat$incarce==0  &
             dat$divorce==0  & dat$physabu==0  & dat$subsuse==0  & dat$mentill==0] <- 0
dat$anyACE[is.na(c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
                   'msubstu',  'mmental',  'DISC',  'loveaff',  'incarce',
                   'divorce',  'physabu',  'subsuse',  'mentill'))]<-NA
table(dat$anyACE)
table(dat$anyACE,useNA='ifany')
#view(dat[,c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
#            'msubstu',  'mmental',  'DISC',  'loveaff',  'incarce',
#            'divorce',  'physabu',  'subsuse',  'mentill', 'anyACE')])
#view(dat[,is.na(c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
#                  'msubstu',  'mmental',  'DISC',  'loveaff',  'incarce',
#                  'divorce',  'physabu',  'subsuse',  'mentill', 'anyACE'))])


#any traditional ACE
dat$anyACE_T <- as.integer(apply(
  dat[, c('incarce', 'divorce',  'physabu',  'subsuse',  'mentill')], 1, sum, na.rm = TRUE) >= 1)
dat$anyACE_T[dat$incarce==0  & dat$divorce==0  & dat$physabu==0  & dat$subsuse==0  & dat$mentill==0] <- 0
dat$anyACE_T[is.na(c('incarce', 'divorce',  'physabu',  'subsuse',  'mentill'))]<-NA
table(dat$anyACE_T,useNA='ifany')


#any expanded ACE
dat$anyACE_E <- as.integer(apply(
  dat[, c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
          'msubstu',  'mmental',  'DISC',  'loveaff')], 1, sum, na.rm = TRUE) >= 1)
dat$anyACE_E[dat$commstr==0  & dat$ecstand==0  & dat$bneedin==0  & dat$mloveaf==0  & dat$mphysab==0  &
             dat$msubstu==0  & dat$mmental==0  & dat$DISC==0  & dat$loveaff==0] <- 0
dat$anyACE_E[is.na(c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
                   'msubstu',  'mmental',  'DISC',  'loveaff'))]<-NA
table(dat$DISC,useNA='ifany')
table(dat$anyACE_E,useNA='ifany')

#view(dat[,c('commstr',  'ecstand',  'bneedin',  'mloveaf',  'mphysab',
#            'msubstu',  'mmental',  'DISC',  'loveaff', 'anyACE_E', 
#            'incarce', 'divorce',  'physabu',  'subsuse',  'mentill', 'anyACE_T','anyACE')])



#Age in 2018
dat$ageo <- 2018-dat$yob
#age grp-0-17 18-24; 25-29; 30-34; 35-39; 40-50
dat$agegrp[dat$ageo<=17 & dat$ageo>=0] <-1
dat$agegrp[dat$ageo<=24 & dat$ageo>=18]<-2
dat$agegrp[dat$ageo<=29 & dat$ageo>=25]<-3
dat$agegrp[dat$ageo<=34 & dat$ageo>=30]<-4
dat$agegrp[dat$ageo<=39 & dat$ageo>=35]<-5
dat$agegrp[dat$ageo<=50 & dat$ageo>=40]<-6
table(dat$agegrp)
#view(dat[, c('ageo', 'agegrp')])






summary(dat)


#drop cases with no exposure info-no case with missing 
dat <-
  dat %>%
  filter(!is.na(anyACE))
summary(dat)

### 2.- define a training sample------------------------------------------------

#simple random sample
#dat$ts  <- dat$id%in%sample(unique(dat$id), length(unique(dat$id))/2)

#stratfied random sample
#how to define strata? stratifed random sample from sex, race, gender, anyace
dat <-
  dat %>%
  mutate(strata=paste0(female,
    race,cut(yob,3),anyACE)) %>%
  group_by(strata) %>%
  mutate(training.sample=if_else(id%in%sample(unique(id), round(n_distinct(id)/2)),1,0)) %>%
  ungroup



n_distinct(dat$strata)
table(dat$strata, dat$training.sample)
#ideally training sample is equal to valdation sample

saveRDS(dat,'C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_NOTRES.Rds')
#saveRDS(dat,'C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_RES.Rds')