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

#dat <- readRDS('data/test.tree.Rds')

#-------------------------------------------------------------------------------
#dat <- readRDS("C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.unconditional.w.Rds")
dat <- readRDS("C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.unconditional.w.notres.Rds")

table(dat$node.cls)
tapply(dat$preshlth, dat$node.cls,function(x){round(c("mean" = mean(x, na.rm = TRUE),
                                                      "median" = median(x, na.rm = TRUE)), digits = 1)})

df1 <-
  dat %>%
  filter(training.sample == 0)
summary(df1$w)

sd.w     <- svydesign(id = ~id, weights = ~w, data = df1)
fit.cls <- svyglm(y~ node.cls, design=sd.w)

summary(fit.cls)
anova(fit.cls,update(fit.cls,.~1))
pred.cls <- predict(fit.cls,newdata=data.frame(node.cls=unique(dat$node.cls)))
cbind(pred.cls,confint(pred.cls))

#with(df1,ftable(ACEmentalill,female,ACEphysharm,node.cls))

#-------------------------------------------------------------------------------
dat <- readRDS("C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.conditional.nw.Rds")

dat$w <- 1

df1 <-
  dat %>%
  filter(training.sample == 0)
#df1$node.cnd  <- df1$node.cls
sd.w     <- svydesign(id = ~id, weights = ~w, data = df1)
fit.cnd <- svyglm(y~ node.cnd + female+agegrp+black+white+hisp+asian+asian_nhpi+othrace+mhighgd_bin+urbnrur+mhhinco, design=sd.w)
summary(fit.cnd)
anova(fit.cnd,update(fit.cnd,.~1))

pred.cnd <- predict(fit.cnd,newdata=data.frame(node.cnd=unique(dat$node.cnd),
                                               female=1, race=unique(dat$race)[1],ageo=mean(dat$ageo)))
cbind(pred.cnd,confint(pred.cnd))



#-------------------------------------------------------------------------------
#dat <- readRDS("C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.causal.w.Rds")
dat <- readRDS("C:/Users/21983/OneDrive - ICF/ADIA/study 1/data/NLS.tree.causal.w.notres.Rds")
dat$id <- 1:nrow(dat)
df1 <-
  dat %>%
  filter(training.sample == 0)





sdw    <- svydesign(id = ~id, weights = ~ w, data = df1)
fit0   <- svyglm(as.numeric(y) ~ anyACE_T, 
                 design = sdw, family = gaussian) #!!use gaussian even for binnary
coef(summary(fit0))

fit1 <- svyglm(as.numeric(y) ~ anyACE_T:node.cau + node.cau  -1,
               design = sdw, family = gaussian)#use gaussian even for binnary
tb   <- coef(summary(fit1))
tb[grep("anyACETRUE:", rownames(tb)), ]

anova(fit0, fit1)