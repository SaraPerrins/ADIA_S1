library(tidyverse)

dat <- read.csv('data/RegressionTree_Test1.csv')
#dat <- haven::read_sas('data/rtree_test1.sas7bdat')
colnames(dat)[colnames(dat)%in%c('C0000100','C0000200','C0005300','C0005400','C0005700')] <-
  c('id','mid','race','sex','yob')
head(dat)

### 1.- Cleaning, recoding
#any (traditional) ACEs
dat <-
  dat %>%
  mutate(
    anyACE=case_when(
      ACEmentalill!=0 | ACEalcohol!=0 | ACEphysharm!=0 | ACEaffection==0 ~ 1,
      ACEmentalill==0 & ACEalcohol==0 & ACEphysharm==0 & ACEaffection!=0 ~ 0
    ))

table(dat$anyACE)

##factors
dat$race <- factor(dat$race)
#Ellen: for ordinal variables it is better to treat them as numeric


summary(dat)


#Age in 2018
dat$ageo <- 2018-dat$yob

summary(dat)

table(dat$sex)
dat$sex[dat$sex=='I'] <- NA
dat$female <- as.numeric(dat$sex)-1

#drop cases with no traditional ACE info
dat <-
  dat %>%
  filter(!is.na(anyACE))
summary(dat)

### 2.- define a training sample------------------------------------------------

#simple random sample
#dat$ts  <- dat$id%in%sample(unique(dat$id), length(unique(dat$id))/2)

#stratfied random sample
dat <-
  dat %>%
  mutate(strata=paste0(sex,race,cut(yob,3),anyACE)) %>%
  group_by(strata) %>%
  mutate(training.sample=if_else(id%in%sample(unique(id), round(n_distinct(id)/2)),1,0)) %>%
  ungroup



n_distinct(dat$strata)
table(dat$strata, dat$training.sample)

dat$w <- 1 #temporary!!!!!@@!!@@!!!@@@
saveRDS(dat,'data/test.Rds')
