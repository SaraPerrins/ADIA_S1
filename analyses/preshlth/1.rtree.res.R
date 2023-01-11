library(tidyverse)
library(rpart)
library(partykit)
library(rpart.plot)
library(survey)
library(causalTree)
set.seed(0203)

#i <- 'RES'
i <- 'NOTRES'
out <- 'preshlth'
#out <- 'drinkdy'


#===============================================================================
### 1.- 'Classical' regression tree (expousure & covariates are all lump together as predictors)
#===============================================================================
#dat <- readRDS('C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_RES.Rds')
dat <- readRDS('C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_NOTRES.Rds')
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
dat$Z <- dat[,z]
dat$X <- dat[,x]
#dat$T <- dat[,t]
#dat$E <- dat[,e]
dat$y <- dat$preshlth        
#dat$y <- dat$drinkdy


df0 <-
  dat %>%
  filter(training.sample==1)



# the default action deletes all observations for which y is missing, but
# keeps those in which one or more predictors are missing.


#a.- grow large tree
tree <- with(df0, rpart(y ~ . , data=cbind(y,Z,X),cp=-1,xval = 10, weights=w))
plotcp(tree,col='red' )
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.

#b.- prune based on complexity
opcp <- tree$cptable[,'CP'][which.min(tree$cptable[,'xerror'])]
ptree <- prune(tree, cp = opcp)
rpart.plot(ptree,roundint = F)


png(paste0('C:/Users/21983/OneDrive - ICF/ADIA/study 1/output/tree.plot.unconditional.', out, '.w.', i,'.png'),width = 480*4,heigh=480*4,res=300)
rpart.plot(ptree,roundint = F)
dev.off()

dat$node.cls  <- factor(predict(as.party(ptree),type='node',newdata=dat))
table(dat$node.cls)

#saveRDS(dat, file = "C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.unconditional.w.Rds")
saveRDS(dat, file = "C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.unconditional.w.notres.Rds")

#===============================================================================
### 2.- Regression tree 'conditional' on covariates
#===============================================================================
#dat <- readRDS('C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_RES.Rds')
dat <- readRDS('C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_NOTRES.Rds')
summary(dat)

# expousures and covaraites are treated differently
# we explore expousures after adjusting for covariates
#Reference:  Stanfill et al. (2019) https://doi.org/10.1177/1179597219858954

x <- c('commstr', 'ecstand', 'bneedin', 'mloveaf', 'mphysab', 'msubstu', 'mmental',
       'DISC', 'loveaff', 'incarce', 'divorce', 'physabu', 'subsuse', 'mentill')
# covariates/confounding
z <- c('female', 'agegrp', 'black', 'white', 'hisp', 'asian', 'asian_nhpi', 'othrace', 'mhighgd_bin',
       'urbnrur', 'mhhinco')
dat$Z <- dat[,z]
dat$X <- dat[,x]
dat$y <- dat$preshlth        
#dat$y <- dat$drinkdy

dat$W <-
  dat %>%
  mutate(strata=paste0(female,race,cut(yob,3))) %>%
  group_by(strata) %>%
  summarize(across(c(all_of(x)),~.x-weighted.mean(.x,w,na.rm=T))) %>%
  ungroup %>%
  select(-1)

df0 <-
  dat %>%
  filter(training.sample==1)


#summary(df0$cw)
#df0 <-  df0 %>%  filter(!is.na(cw))

#a.- grow large tree
#tree <- with(df0, rpart(y ~ . , data=cbind(y,W),cp=-1, weights=w))
tree <- with(df0, rpart(y ~ . , data=cbind(y,W),cp = -1,weights=w))
plotcp(tree,col='red' )
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.

#b.- prune based on complexity
opcp <- tree$cptable[,'CP'][which.min(tree$cptable[,'xerror'])]
ptree.cond <- prune(tree, cp = opcp)
rpart.plot(ptree.cond,roundint=F)
#plot(as.party(ptree.cond))


png(paste0('C:/Users/21983/OneDrive - ICF/ADIA/study 1/output/tree.plot.conditional.', out, '.nw.', i,'.png'),width = 480*4,heigh=480*4,res=300)
rpart.plot(ptree.cond,roundint=F)
dev.off()

dat$node.cnd  <- factor(predict(as.party(ptree.cond),type='node',newdata=dat))
table(dat$node.cnd)

#saveRDS(dat, file = "C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.conditional.nw.Rds")


#===============================================================================
### 3.- A casual tree
#===============================================================================
#dat <- readRDS('C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_RES.Rds')
dat <- readRDS('C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar_NOTRES.Rds')
summary(dat)
#dat<-dat[dat$drinkdy<=20,]
dim(dat)
summary(dat)


### . Find balancing weights among those with and without any ACEs

#Approximate Balancing Weights (these are an alternative to entropy balancing)
#Reference:  https://doi.org/10.48550/arXiv.2007.09056
source('C:/Users/21983/OneDrive - ICF/ADIA/study 2/Program/find.weights.r')

# In orther to balance the missing pattern we need to
# for categorical variables, create an NA category (addNA)
# this should inlcude binary variables (even if not declared as factors)
# for continuous, add indicator is.na and impute mean
x <- c('female', 'agegrp', 'black', 'white', 'hisp', 'asian', 'asian_nhpi', 'othrace', 'mhighgd_bin',
       'urbnrur', 'mhhinco')
dat$y <- dat$preshlth  
#dat$y  <- dat$drinkdy

dat$X <-
  dat[,x] %>%
  mutate(across(where(is.factor),addNA,ifany=T))  %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x)) && n_distinct(.x)==2), ~addNA(factor(.x)))) %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x))), is.na,.names = 'NA_{.col}')) %>%
  mutate(across(where(is.numeric),~ replace(., is.na(.),mean(., na.rm=T))))  %>%
  model.matrix(~., .) %>%
  .[, -1]



#weights add up to 1 in each group
tgt  <- colMeans(dat$X)
sd2  <- apply(dat$X, 2, function(v) if(mean(v) < .05) {sqrt(.05 * .95)} else {sd(v)})
phiX <- scale(dat$X, center = tgt, scale = sd2)
dat$w[dat$anyACE_T == 1] <- find.weights(phiX[dat$anyACE_T == 1, ], lambda = 0.01)
dat$w[dat$anyACE_T == 0] <- find.weights(phiX[dat$anyACE_T == 0, ], lambda = 0.01)
# Ye: lambda=0 give maximumn similarity
# at the cost of more varaibility of the weights (i.e.,less effective sample size)
# you coudl increase a bit, e.g. 0.05, to reduce weight  variability,
#if you still get
tapply(dat$preshlth , dat$anyACE_T, mean)
tapply(dat$preshlth * dat$w, dat$anyACE_T, sum)


with(dat, data.frame(X,anyACE_T,w)) %>%
  group_by(anyACE_T) %>%
  summarize(across(everything()&!w, list(
    mean,
    ~ weighted.mean(.,w))
  )) %>% t

#stratfied random sample
dat <-
  dat %>%
  mutate(strata = paste0(x, anyACE_T)) %>%
  group_by(strata) %>%
  mutate(training.sample = if_else(id %in% 
                                     sample(unique(id), round(n_distinct(id)/2)), 1, 0)) %>%
  ungroup
table(dat$strata, dat$training.sample)


#expanded ACE
ace.e <-  c('commstr', 'ecstand', 'bneedin', 'mloveaf', 'mphysab', 'msubstu', 'mmental',
         'DISC', 'loveaff')

summary(dat[, ace.e])
dat$ACE.E <- dat[, ace.e]


### . find effect modifiers


#Reference https://doi.org/10.48550/arXiv.1504.01132

dat$cw <- dat$w * nrow(dat)
df0 <-
  dat %>%
  filter(training.sample == 1)
dim(df0)
dim(dat)



###Causal Trees (CT)
tree <- with(df0, causalTree(y ~ .,
                             data = cbind(y,ACE.E), treatment = anyACE_T, weights = cw,
                             split.Rule = "CT", cv.option = "CT", xval = 5,
                             split.Honest = TRUE, cv.Honest = TRUE, cp = 0)
)
tree$cptable

opcp <- tree$cptable[, 1][which.min(tree$cptable[, 4])]
ptree_causal <- prune(tree, cp = opcp)

rpart.plot(ptree_causal,roundint=F) #Ye : chose the plot you like to save below

png(paste0('C:/Users/21983/OneDrive - ICF/ADIA/study 1/output/tree.plot.causal.', out, '.w.', i,'.png'),width = 480*4,heigh=480*4,res=300)
rpart.plot(ptree_causal,roundint=F)
dev.off()

dat$node.cau  <- factor(
  predict(partykit:::as.party(ptree_causal),
          type = "node", newdata = dat))
table(dat$node.cau)

#saveRDS(dat, file = "C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.causal.w.Rds")
saveRDS(dat, file = "C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.causal.w.notres.Rds")




