library(tidyverse)
library(rpart)
library(partykit)
library(rpart.plot)
library(survey)
set.seed(0203)

dat <- readRDS('C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/finalvar.Rds')
dim(dat)
summary(dat)
#dat <-dat[dat$drinkdy <=20,]
dat$sex <-dat$sex-1

### 0.- variables & roles
# exposures
x <- c('commstr', 'ecstand', 'bneedin', 'mloveaf', 'mphysab', 'msubstu', 'mmental',
       'discrim', 'loveaff', 'incarce', 'divorce', 'physabu', 'subsuse', 'mentill')
# covariates/confounding
z <- c('sex', 'ageo', 'black', 'white', 'hisp', 'asian', 'asian_nhpi', 'othrace', 'mhighgd_bin',
       'urbnrur', 'mhhinco')
dat$Z <- dat[,z]
dat$X <- dat[,x]
dat$y <- dat$preshlth        
#dat$y <- dat$drinkdy
df0 <-
  dat %>%
  filter(training.sample==1)

#===============================================================================
### 1.- 'Classical' regression tree (expousure & covariates are all lump together as predictors)
#===============================================================================
# the default action deletes all observations for which y is missing, but
# keeps those in which one or more predictors are missing.


#a.- grow large tree
tree <- with(df0, rpart(y ~ . , data=cbind(y,X,Z),cp=-1,xval = 10, weights=w))
plotcp(tree,col='red' )
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.

#b.- prune based on complexity
opcp <- tree$cptable[,'CP'][which.min(tree$cptable[,'xerror'])]
ptree <- prune(tree, cp = opcp)
rpart.plot(ptree,roundint = F)
#plot(as.party(ptree))

png('C:/Users/21983/OneDrive - ICF/ADIA/study 1/output/tree.plot.preshlth.unconditional.png',width = 480*4,heigh=480*4,res=300)
rpart.plot(ptree,roundint = F)
dev.off()



#===============================================================================
### 2.- Regression tree 'conditional' on covariates
#===============================================================================
# expousures and covaraites are treated differently
# we explore expousures after adjusting for covariates
#Reference:  Stanfill et al. (2019) https://doi.org/10.1177/1179597219858954

dat$W <-
  dat %>%
  mutate(strata=paste0(sex,race,cut(yob,3))) %>%
  group_by(strata) %>%
  summarize(across(c(all_of(x), anyACE),~.x-weighted.mean(.x,w,na.rm=T))) %>%
  ungroup %>%
  select(-1)


df0 <-
  dat %>%
  filter(training.sample==1)


summary(df0$cw)
df0 <-
  df0 %>%
  filter(!is.na(cw))

#a.- grow large tree
tree <- with(df0, rpart(y ~ . , data=cbind(y,W),cp=-1, weights=w))
plotcp(tree,col='red' )
#A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.

#b.- prune based on complexity
opcp <- tree$cptable[,'CP'][which.min(tree$cptable[,'xerror'])]
ptree.cond <- prune(tree, cp = opcp)
rpart.plot(ptree.cond,roundint=F)
#plot(as.party(ptree.cond))


png('C:/Users/21983/OneDrive - ICF/ADIA/study 1/output/tree.plot.preshlth.conditional.png',width = 480*4,heigh=480*4,res=300)
rpart.plot(ptree.cond,roundint=F)
dev.off()

#===============================================================================
### 3.- A casual tree
#===============================================================================

### . Find balancing weights among those with and without any ACEs

#Approximate Balancing Weights (these are an alternative to entropy balancing)
#Reference:  https://doi.org/10.48550/arXiv.2007.09056
source('C:/Users/21983/OneDrive - ICF/ADIA/study 2/Program/find.weights.r')

# In orther to balance the missing pattern we need to
# for categorical variables, create an NA category (addNA)
# this should inlcude binary variables (even if not declared as factors)
# for continuous, add indicator is.na and impute mean

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
dat$w[dat$anyACE == 1] <- find.weights(phiX[dat$anyACE == 1, ], lambda = 0.01)
dat$w[dat$anyACE == 0] <- find.weights(phiX[dat$anyACE == 0, ], lambda = 0.01)
# Ye: lambda=0 give maximumn similarity
# at the cost of more varaibility of the weights (i.e.,less effective sample size)
# you coudl increase a bit, e.g. 0.05, to reduce weight  variability,
#if you still get
tapply(dat$preshlth , dat$anyACE, mean)
tapply(dat$preshlth * dat$w, dat$anyACE, sum)


with(dat, data.frame(X,anyACE,w)) %>%
  group_by(anyACE) %>%
  summarize(across(everything()&!w, list(
    mean,
    ~ weighted.mean(.,w))
  )) %>% t


### . find effect modifiers

library(causalTree)
#Reference https://doi.org/10.48550/arXiv.1504.01132

dat$cw <- dat$w * nrow(dat)
df0 <-
  dat %>%
  filter(training.sample == TRUE)
dim(df0)
dim(dat)



###Causal Trees (CT)
tree <- with(df0, causalTree(y ~ .,
                             data = cbind(y,Z), treatment = anyACE, weights = cw,
                             split.Rule = "CT", cv.option = "CT", xval = 5,
                             split.Honest = TRUE, cv.Honest = TRUE, cp = 0)
)
tree$cptable

opcp <- tree$cptable[, 1][which.min(tree$cptable[, 4])]
ptree_causal <- prune(tree, cp = opcp)

rpart.plot(ptree_causal) #Ye : chose the plot you like to save below
#plot(as.party(ptree_causal))


png("C:/Users/21983/OneDrive - ICF/ADIA/study 1/output/tree.plot.preshlth.causal.png",width = 480*4, heigh = 480*4, res = 300)
rpart.plot(ptree_causal)
dev.off()


#===============================================================================
### 4. Save class membership for all cases
#===============================================================================

dat$node.cls  <- factor(predict(as.party(ptree),type='node',newdata=dat))
dat$node.cnd  <- factor(predict(as.party(ptree.cond),type='node',newdata=dat))
dat$node.cau  <- factor(predict(partykit:::as.party(ptree_causal),type = "node", newdata = dat))
table(dat$node.cls)
table(dat$node.cnd)
table(dat$node.cau)


#saveRDS(dat,'data/test.tree.Rds')