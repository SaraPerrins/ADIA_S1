# nlscya
# https://nlsinfo.org/weights/nlscya


#Second, and of most fundamental importance, any researcher using this data set must be continually conscious
#of the fact the children are not representative of a full cross-section of American children. When appropriate
#weights are applied to the sample, the children are approximately typical of children who have been born to a
#nationally representative sample of American women (who were 14-22 as of January 1, 1979)

#https://www.nlsinfo.org/sites/default/files/attachments/121214/NLSYChildren1992Evaluation.pdf

#library(devtools)
#install_github("susanathey/causalTree")
#https://doi.org/10.48550/arXiv.1504.01132

library(tidyverse)
library(survey)
library(survival)
library(survival)
library(gt)

options(digits = 3)
options(scipen = 10^3)



#i <- 'RES'
i   <- 'NOTRES'
out <- 'preshlth'
#out <- 'drinkdy'

#dat <- readRDS("C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.unconditional.w.Rds")
#dat <- readRDS("C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.unconditional.w.notres.Rds")
file_name <- paste0("data/NLS.tree", out, i, ".Rds")
dat <- readRDS(file_name)
#======================================================================================
#1.- unconditional
#======================================================================================
table(dat$node.cls)
tapply(dat$y, dat$node.cls,function(x){round(c("mean" = mean(x, na.rm = TRUE),
                                     "median" = median(x, na.rm = TRUE)), digits = 1)})

df1 <-
  dat %>%
  filter(training.sample == 0)
summary(df1$w)

sd.w     <- svydesign(id = ~id, weights = ~w, data = df1)
fit.cls  <- svyglm(y ~ node.cls, design = sd.w)

summary(fit.cls)
anova(fit.cls, update(fit.cls, . ~ 1))

pred.cls <- predict(fit.cls,
  newdata = data.frame(node.cls = unique(dat$node.cls)))


tb <- data.frame(unique(dat$node.cls), pred.cls, confint(pred.cls))[order(unique(dat$node.cls)),]
tb
#saving the output in nice format
tb %>%
  gt %>%
  tab_header(title = "Predicted values") %>%
  gtsave(paste0("output/predicted_cls", out, i, ".html"))

#with(df1,ftable(ACEmentalill,female,ACEphysharm,node.cls))

#======================================================================================
#2.- conditional
#======================================================================================

#dat <- readRDS("C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.conditional.nw.Rds")

#dat$w <- 1 #why would be overide the weights ??


### Since we did not find any predictor condtional
# we will test what we found using classical tree 
#controlling for  other covariates (excluding those detected)
dat$node.cnd  <- dat$node.cls
df1   <-  dat %>%  filter(training.sample == 0)
sd.w  <- svydesign(id = ~id, weights = ~ w, data = df1)

fit.cnd <- svyglm(y ~ node.cnd
   + female + agegrp
   + black + white + hisp + asian + asian_nhpi + othrace +
   + mhighgd_bin
   + rural + mixur
   #+ mhhinco,
  , design = sd.w)
summary(fit.cnd)
anova(fit.cnd, update(fit.cnd, . ~ . - node.cnd))

summary(fit.cnd$data)
unique(dat$node.cnd)

summary(dat[, all.vars(formula(fit.cnd))])
table(dat$urbnrur)

pred.cnd <- predict(fit.cnd,
              newdata = data.frame(node.cnd = unique(dat$node.cnd),
                                   female = 1,
                                   agegrp = mean(dat$agegrp, na.rm = TRUE),
                                   black = 0,
                                   white = 0,
                                   hisp = 1,
                                   asian = 0,
                                   asian_nhpi = 0,
                                   othrace = 0,
                                   mhighgd_bin = 0,
                                   rural = 0,
                                   mixur = 0,
                                   mhhinco = mean(dat$mhhinco, na.rm = TRUE)
                                   )
                    )
                    
cbind(unique(dat$node.cnd), pred.cnd, confint(pred.cnd))[order(unique(dat$node.cnd)),]

#======================================================================================
#3.- Causal tree
#======================================================================================
df1 <-  dat %>% filter(training.sample == 0)

sdw    <- svydesign(id = ~id, weights = ~ wb, data = df1) #! use balanced weights
fit0   <- svyglm(as.numeric(y) ~ anyACE_T,
                 design = sdw, family = gaussian) #!!use gaussian even for binnary

# Is the difference in outcome
# between kids who experience ACE_T and kids who  did not
# statistically significant?
coef(fit0)["anyACE_T"]
confint(fit0)["anyACE_T", ]
anova(fit0, update(fit0, . ~ . - anyACE_T))

fit1 <- svyglm(as.numeric(y) ~ anyACE_T:node.cau + node.cau  - 1,
               design = sdw, family = gaussian)#use gaussian even for binnary

anova(fit0, fit1)

tb <- data.frame(unique(dat$node.cau),
  coef(fit1)[grep("anyACE_T:", names(coef(fit1)))],
  confint(fit1)[grep("anyACE_T:", names(coef(fit1))), ]
  )[order(unique(dat$node.cau)), ]
colnames(tb) <- c("Node", "Est.", "95%CI_LL", "95%CI_UL")

#saving the output in nice format
tb %>%
  gt %>%
  tab_header(title = "Predicted values") %>%
  gtsave(paste0("output/predicted_cau", out, i, ".html"))


table(dat$anyACE_T)
table(dat$bneedin)
table(dat$bneedin)

#======================================================================

dat$W <- NA
dat$W[dat$bneedin == 0 & dat$anyACE_T == 0] <- 0
dat$W[dat$bneedin == 0 & dat$anyACE_T == 1] <- 1
dat$W[dat$bneedin == 1 & dat$anyACE_T == 1] <- 2




df1 <-  dat %>% filter(training.sample == 0)

sdw    <- svydesign(id = ~id, weights = ~ wb, data = df1)

fit3   <- svyglm(as.numeric(y) ~ factor(W),
                 design = sdw, family = gaussian)

summary(fit3 )
