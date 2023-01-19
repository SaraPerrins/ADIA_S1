library(tidyverse)
library(rpart)
library(partykit)
library(rpart.plot)
library(survey)
library(causalTree)
set.seed(0203)
options(digits = 3)
options(scipen = 10^3)


getwd()
# Ye: un-comment this next line to change path to your own
#setwd('C:/Users/21983/OneDrive - ICF/ADIA/study 1') 

#i <- 'RES'
i   <- 'NOTRES'
out <- 'preshlth'
#out <- 'drinkdy'




#===============================================================================
### 1.- 'Classical' regression tree (expousure & covariates are all lump together as predictors)
#===============================================================================
dat <- readRDS("data/finalvar.Rds")
# 01/17/2023
# Ye: I am thinking we cna make the change here, rahter than in 0.preprocess
dat$DISC <- unlist(dat[, paste0("sensitivity", i)])
table(dat$DISC, useNA = "ifany")


summary(dat)
#dat<-dat[dat$drinkdy<=20,]
### 0.- variables & roles
# exposures
x <- c('commstr', 'ecstand', 'bneedin', 'mloveaf', 'mphysab', 'msubstu', 'mmental',
       'DISC', 'loveaff', 'incarce', 'divorce', 'physabu', 'subsuse', 'mentill')
# traditional ACE
#t <- c('incarce', 'divorce', 'physabu', 'subsuse', 'mentill')
# expanded ACE
#e <- c('commstr', 'ecstand', 'bneedin', 'mloveaf', 'mphysab', 'msubstu', 'mmental', 'discrim', 'loveaff')
# covariates/confounding
z <- c('female', 'agegrp', 'black', 'white', 'hisp', 'asian', 'asian_nhpi', 'othrace', 'mhighgd_bin',
       'urbnrur', 'mhhinco')
dat$Z <- dat[, z]
dat$X <- dat[, x]
#dat$T <- dat[,t]
#dat$E <- dat[,e]
dat$y <- dat$preshlth
#dat$y <- dat$drinkdy


df0 <-
  dat %>%
  filter(training.sample == 1)



# the default action deletes all observations for which y is missing, but
# keeps those in which one or more predictors are missing.


#a.- grow large tree
tree <- with(df0, rpart(y ~ ., 
  data = cbind(y, Z, X), cp = -1, xval = 10, weights = w))
plotcp(tree, col='red' )


#b.- prune based on complexity
opcp <- tree$cptable[, "CP"][which.min(tree$cptable[, "xerror"])]
ptree <- prune(tree, cp = opcp)
rpart.plot(ptree, roundint = FALSE)


png(
  paste0("output/tree.plot.unconditional.", out, ".w.", i, ".png"),
  width = 480 * 4, heigh = 480 * 4, res = 300)
rpart.plot(ptree, roundint = FALSE)
dev.off()

dat$node.cls  <- factor(predict(as.party(ptree), type = "node", newdata = dat))
table(dat$node.cls)

saveRDS(dat, file = "data/NLS.tree.unconditional.w.notres.Rds")

#===============================================================================
### 2.- Regression tree 'conditional' on covariates
#===============================================================================
###Ye: why loading the data again?
#dat <- readRDS('data/finalvar.Rds')
#dat$DISC <- unlist(dat[, paste0("sensitivity", i)])
#table(dat$DISC, useNA = "ifany")

# expousures and covaraites are treated differently
# we explore expousures after adjusting for covariates
#Reference:  Stanfill et al. (2019) https://doi.org/10.1177/1179597219858954

x <- c('commstr', 'ecstand', 'bneedin', 'mloveaf', 'mphysab', 'msubstu', 'mmental',
       'DISC', 'loveaff', 'incarce', 'divorce', 'physabu', 'subsuse', 'mentill')

# covariates/confounding
z <- c('female', 'agegrp', 'black', 'white', 'hisp', 'asian', 'asian_nhpi', 'othrace', 'mhighgd_bin',
       'urbnrur', 'mhhinco')
       
dat$Z <- dat[, z]
dat$X <- dat[, x]
dat$y <- dat$preshlth
#dat$y <- dat$drinkdy

dat$W <-
  dat %>%
  mutate(strata = paste0(female, race, cut(yob, 3))) %>%
  group_by(strata) %>%
  summarize(across(c(all_of(x)), ~ .x - weighted.mean(.x, w, na.rm = TRUE))) %>%
  ungroup %>%
  select(-1)

df0 <-
  dat %>%
  filter(training.sample == 1)


#summary(df0$cw)
#df0 <-  df0 %>%  filter(!is.na(cw))

#a.- grow large tree
#tree <- with(df0, rpart(y ~ . , data=cbind(y,W),cp=-1, weights=w))
tree <- with(df0, rpart(y ~ ., data = cbind(y, W), cp = -1, weights = w))
plotcp(tree, col = "red")
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.

#b.- prune based on complexity
opcp <- tree$cptable[, "CP"][which.min(tree$cptable[, "xerror"])]
ptree.cond <- prune(tree, cp = opcp)
rpart.plot(ptree.cond, roundint = FALSE)
#plot(as.party(ptree.cond))


png(
  paste0("output/tree.plot.conditional.", out, ".nw.", i, ".png"),
  width = 480 * 4, heigh = 480 * 4, res = 300)
rpart.plot(ptree.cond, roundint = FALSE)
dev.off()

dat$node.cnd  <- factor(
  predict(as.party(ptree.cond),
  type = "node", newdata = dat))
table(dat$node.cnd)

#saveRDS(dat, file = "C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.conditional.nw.Rds")

#===============================================================================
### 3.- A casual tree
#===============================================================================

### a. Find balancing weights among those with and without any ACEs
#Entropy Balancing
#Reference: #https://web.stanford.edu/~jhain/Paper/eb.pdf
source("R/ebw.r")

# In orther to balance the missing pattern we need to:
# for categorical variables, create an NA category (addNA)
# this should inlcude binary varaibles not declared as s
# for continuous, add indicator is.na and impute mean

dat$C <-
  dat$Z %>%
  mutate(across(where(is.factor), addNA, ifany = TRUE))  %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x)) && n_distinct(.x)==2), ~ addNA(factor(.x)))) %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x))), is.na,.names = 'NA_{.col}')) %>%
  mutate(across(where(is.numeric),~ replace(.,is.na(.),mean(.,na.rm=T))))  %>%
  model.matrix(~.,.) %>%
  .[, -1]

colMeans(dat$C)
# will not balance very rare attributes
dat$C <- dat$C[, colMeans(dat$C) > .05]
dat$C <- dat$C[, !colnames(dat$C) %in%
  c("NA_whiteTRUE", "NA_hispTRUE", "NA_asianTRUE",
  "NA_asian_nhpiTRUE", "NA_othraceTRUE")]

colMeans(dat$C)



### 01/17/23 Causal tree needs complete info on ACE_T
#one solution is to collpase certainly unexposed with expousure unknown
dat$anyACE_T[is.na(dat$anyACE_T)] <- 0
table(dat$anyACE_T)
#an alternative solution would be imputation


tgt  <- colMeans(dat$C); tgt
sapply(split(dat, dat$anyACE_T), \(D) with(D, colMeans(D$C)))

ebw1 <- with(dat, ebw(id = id[anyACE_T == 1],
  covariates = C[anyACE_T == 1, ],
  target.margins = tgt,
  base.weight = w[anyACE_T == 1]))
ebw0 <- with(dat, ebw(id = id[anyACE_T == 0],
  covariates = C[anyACE_T == 0, ],
  target.margins = tgt,
  base.weight = w[anyACE_T == 0]))
dat  <- left_join(dat, rbind(ebw0, ebw1))
summary(dat)

with(dat, data.frame(C, anyACE_T, w, wb)) %>%
  group_by(anyACE_T) %>%
  summarize(across(everything(), list(
  ~ weighted.mean(., w),
  ~ weighted.mean(., wb))
  ))
df0 <-
  dat %>%
  filter(training.sample == 1)





#Approximate Balancing Weights (these are an alternative to entropy balancing)
#Reference:  https://doi.org/10.48550/arXiv.2007.09056
source("R/find.weights.r")

# In orther to balance the missing pattern we need to
# for categorical variables, create an NA category (addNA)
# this should inlcude binary variables (even if not declared as factors)
# for continuous, add indicator is.na and impute mean
x <- c('female', 'agegrp', 'black', 'white', 'hisp', 'asian', 'asian_nhpi', 'othrace', 'mhighgd_bin',
       'urbnrur', 'mhhinco')
dat$y <- dat$preshlth
#dat$y  <- dat$drinkdy

dat$X <-
  dat[, x] %>%
  mutate(across(
    where(is.factor),
    addNA, ifany = T))  %>%
  mutate(across(
    where(~ is.numeric(.x) && any(is.na(.x)) && n_distinct(.x) == 2),
    ~ addNA(factor(.x)))) %>%
  mutate(across(
    where(~ is.numeric(.x) && any(is.na(.x))),
    is.na, .names = "NA_{.col}")) %>%
  mutate(across(
    where(is.numeric),
    ~ replace(., is.na(.), mean(., na.rm = TRUE))))  %>%
  model.matrix(~., .) %>%
  .[, -1]

summary(dat$X)

#weights add up to 1 in each group
tgt  <- colMeans(dat$X)
sd2  <- apply(dat$X, 2, \(v) if (mean(v) < .05) sqrt(.05 * .95) else sd(v))
phiX <- scale(dat$X, center = tgt, scale = sd2)

dat$w[!is.na(dat$anyACE_T) & dat$anyACE_T == 1] <-
  find.weights(phiX[!is.na(dat$anyACE_T) & dat$anyACE_T == 1, ], lambda = 0.01)
dat$w[!is.na(dat$anyACE_T) & dat$anyACE_T == 0] <-
  find.weights(phiX[!is.na(dat$anyACE_T) & dat$anyACE_T == 0, ], lambda = 0.01)

# Ye: lambda=0 give maximumn similarity
# at the cost of more varaibility of the weights (i.e.,less effective sample size)
# you coudl increase a bit, e.g. 0.05, to reduce weight  variability,
#if you still get
tapply(dat$preshlth, dat$anyACE_T, mean, na.rm = TRUE)
tapply(dat$preshlth * dat$w, dat$anyACE_T, sum, na.rm = TRUE)


with(dat, data.frame(X, anyACE_T, w)) %>%
  group_by(anyACE_T) %>%
  summarize(across(everything() & !w, list(
    mean,
    ~ weighted.mean(., w))
    )) %>%
  t

### 01/17/23 
#Ye: no need to sample again
#stratfied random sample
#dat <-
#  dat %>%
#  mutate(strata = paste0(x, anyACE_T)) %>%
#  group_by(strata) %>%
#  mutate(training.sample = if_else(id %in%
#                                     sample(unique(id), round(n_distinct(id)/2)), 1, 0)) %>%
#  ungroup
#table(dat$strata, dat$training.sample)


#expanded ACE
ace.e <-  c('commstr', 'ecstand', 'bneedin', 'mloveaf', 'mphysab', 'msubstu', 'mmental',
         'DISC', 'loveaff')

summary(dat[, ace.e])
dat$ACE.E <- dat[, ace.e]


### . find effect modifiers
#Reference https://doi.org/10.48550/arXiv.1504.01132

###case weights
dat$cw <- dat$w * sum(!is.na(dat$anyACE_T))
# replace normalized weights (that add up to 1 in each arm)
# with weigths that add up to sample size
# this seems to be inconsequent as long  as n is not small (n>=300)
# rtree shuld be insensitve to this scaling changes,
# the fact that causalTree is not completly insensitive 
# may be due to some rounding issue

df0 <-
  dat %>%
  filter(training.sample == 1 & !is.na(anyACE_T))
dim(df0)
dim(dat)

table(dat$anyACE_T)
help(causalTree)

###Causal Trees (CT)
set.seed(0203)
tree_causal <- with(df0, causalTree(y ~ .,
                             data = cbind(y, ACE.E),
                             treatment = anyACE_T,
                             weights = cw,
                             split.Rule = "CT", cv.option = "CT", xval = 5,
                             split.Honest = TRUE, cv.Honest = TRUE, cp = 0)
)
tree_causal$cptable[1:5, ]

opcp <- tree_causal$cptable[, 1][which.min(tree_causal$cptable[, 4])]
ptree_causal <- prune(ctree, cp = opcp)

rpart.plot(ptree_causal, roundint = FALSE)


png(
  paste0("output/tree.plot.causal.", out, ".w.", i, ".png"),
  width = 480 * 4, heigh = 480 * 4, res = 300)
rpart.plot(ptree_causal, roundint = FALSE)
dev.off()

dat$node.cau  <- factor(
  predict(partykit:::as.party(ptree_causal),
          type = "node", newdata = dat))
table(dat$node.cau)

#saveRDS(dat, file = "C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.causal.w.Rds")
saveRDS(dat, file = "C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.causal.w.notres.Rds")