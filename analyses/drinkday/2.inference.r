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

#dat <- readRDS('data/test.tree.Rds')

df1 <-
  dat %>%
  filter(training.sample==0)
summary(df1$w)

#-------------------------------------------------------------------------------
sd.w     <- svydesign(id = ~id, weights = ~w, data = df1)
fit.cls <- svyglm(y~ node.cls, design=sd.w)

summary(fit.cls)
anova(fit.cls,update(fit.cls,.~1))
pred.cls <- predict(fit.cls,newdata=data.frame(node.cls=unique(dat$node.cls)))
cbind(pred.cls,confint(pred.cls))

#with(df1,ftable(ACEmentalill,female,ACEphysharm,node.cls))

#-------------------------------------------------------------------------------
#df1$node.cnd  <- df1$node.cls
sd.w     <- svydesign(id = ~id, weights = ~w, data = df1)
fit.cnd <- svyglm(y~ node.cnd + sex+ageo+black+white+hisp+asian+asian_nhpi+othrace+mhighgd_bin+urbnrur+mhhinco, design=sd.w)
summary(fit.cnd)
anova(fit.cnd,update(fit.cnd,.~1))

pred.cnd <- predict(fit.cnd,newdata=data.frame(node.cnd=unique(dat$node.cnd),
                                               female=1, race=unique(dat$race)[1],ageo=mean(dat$ageo)))
cbind(pred.cnd,confint(pred.cnd))



#-------------------------------------------------------------------------------
#df1$node.cau  <- df1$node.cls
sd.wb    <- svydesign(id = ~id, weights = ~wb, data = df1)
fit.cau <- svyglm(y~ anyACE:node.cau + node.cau  -1, design=sd.wb)
tb <- coef(summary(fit.cau ))
tb[grep('anyACE:',rownames(tb)),]