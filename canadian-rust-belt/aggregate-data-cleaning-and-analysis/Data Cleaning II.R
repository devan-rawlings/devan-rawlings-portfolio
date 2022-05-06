#! Data Cleaning II.R
## This script takes in aggregate datasets at the CMA/CA level (Scraped manually from Statistics Canada public tabulations) and cleans it, 
## computing additional variables for the analysis (this creates the 'NAICS set'). Then, it conducts the regression analysis of both the 'NOC set' and 'NAICS set,'
## including the identification tests and robustness checks of the paper.

library(dplyr)
library(readxl)
library(foreach)
library(haven)
library(graphics)
library(stargazer)
library(AER)
library(tidyr)
library(tidyverse)
library(bartik.weight)
library(ivmodel)
library(ManyIV)
library(ivpack)
library(lmtest)
library(sandwich)

### Create aggregate dataset

male <- read_xls("Census Aggregate Set Male.xls")
female <- read_xls("Census Aggregate Set Female.xls")

manu_emp <- male$Manu_Employment + female$Manu_Employment
E311 <- male$E311 + female$E311
E312 <- male$E312 + female$E312
E313 <- male$E313 + female$E313
E314 <- male$E314 + female$E314
E315 <- male$E315 + female$E315
E316 <- male$E316 + female$E316
E321 <- male$E321 + female$E321
E322 <- male$E322 + female$E322
E323 <- male$E323 + female$E323
E324 <- male$E324 + female$E324
E325 <- male$E325 + female$E325
E326 <- male$E326 + female$E326
E327 <- male$E327 + female$E327
E331 <- male$E331 + female$E331
E332 <- male$E332 + female$E332
E333 <- male$E333 + female$E333
E334 <- male$E334 + female$E334
E335 <- male$E335 + female$E335
E336 <- male$E336 + female$E336
E337 <- male$E337 + female$E337
E339 <- male$E339 + female$E339

#Employment Rate
erateM <- male$Employed_Male / (male$Employed_Male + male$Unemployed)
erateF <- female$Employed_Female / (female$Employed_Female + female$Unemployed)

#Controls
fe_lfp <- female$not_lf / (female$Employed_Female + female$Unemployed + female$not_lf)
he_share <- male$pop_bach / male$pop
for_bor <- male$pop_for / male$pop

#Manufacturing Share
manu_shr <- manu_emp / (male$Employed_Male + female$Employed_Female)

#Sub-industry Shares
share311 <- E311 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share312 <- E312 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share313 <- E313 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share314 <- E314 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share315 <- E315 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share316 <- E316 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share321 <- E321 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share322 <- E322 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share323 <- E323 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share324 <- E324 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share325 <- E325 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share326 <- E326 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share327 <- E327 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share331 <- E331 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share332 <- E332 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share333 <- E333 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share334 <- E334 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share335 <- E335 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share336 <- E336 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share337 <- E337 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)
share339 <- E339 / (male$Employed_Male + female$Employed_Female + male$Unemployed + female$Unemployed)

employment <- male$Employed_Male + female$Employed_Female

#Mechanism variables
lnpop <- log(male$pop)
lfnpM <- male$NLF_male / (male$Employed_Male + male$Unemployed + male$NLF_male)
lfnpF <- female$not_lf / (female$Employed_Female + female$Unemployed + female$not_lf)

#Putting it together
aggregate_set <- data.frame(male$Name, male$Year, male$CMA, male$Pr, employment, erateM, erateF, manu_shr, share311, share312, share313, share314, share315, share316, share321, share322, share323, share324, share325, share326, share327, share331, share332, share333, share334, share335, share336, share337, share339, E311, E312, E313, E314, E315, E316, E321, E322, E323, E324, E325, E326, E327, E331, E332, E333, E334, E335, E336, E337, E339, fe_lfp, he_share, for_bor, lnpop, lfnpM, lfnpF)
colnames(aggregate_set)[1] <- "Name"
colnames(aggregate_set)[2] <- "Year"
colnames(aggregate_set)[3] <- "CMA"
colnames(aggregate_set)[4] <- "Pr"

###2001-2016

#Shocks

shock311 <- vector()
shock312 <- vector()
shock313 <- vector()
shock314 <- vector()
shock315 <- vector()
shock316 <- vector()
shock321 <- vector()
shock322 <- vector()
shock323 <- vector()
shock324 <- vector()
shock325 <- vector()
shock326 <- vector()
shock327 <- vector()
shock331 <- vector()
shock332 <- vector()
shock333 <- vector()
shock334 <- vector()
shock335 <- vector()
shock336 <- vector()
shock337 <- vector()
shock339 <- vector()

foreach (i = 1:134, j = subset(aggregate_set, Year == 2001)$CMA) %do%  {
  shock311[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E311) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E311) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock312[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E312) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E312) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock313[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E313) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E313) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock314[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E314) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E314) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock315[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E315) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E315) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock316[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E316) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E316) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock321[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E321) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E321) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock322[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E322) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E322) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock323[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E323) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E323) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock324[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E324) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E324) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock325[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E325) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E325) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock326[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E326) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E326) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock327[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E327) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E327) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock331[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E331) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E331) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock332[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E332) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E332) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock333[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E333) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E333) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock334[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E334) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E334) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock335[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E335) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E335) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock336[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E336) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E336) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock337[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E337) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E337) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock339[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E339) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E339) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
}

#Independent Variable

del_manushr <- subset(aggregate_set, Year == 2016)$manu_shr - subset(aggregate_set, Year == 2001)$manu_shr

#Dependent Variables

del_erateM <- subset(aggregate_set, Year == 2016)$erateM - subset(aggregate_set, Year == 2001)$erateM

del_erateF <- subset(aggregate_set, Year == 2016)$erateF - subset(aggregate_set, Year == 2001)$erateF

##Regression ~ 2001-2016

set01_16 <- data.frame(cbind(subset(aggregate_set, Year == 2001), shock311, shock312, shock313, shock314, shock315, shock316, shock321, shock322, shock323, shock324, shock325, shock326, shock327, shock331, shock332, shock333, shock334, shock335, shock336, shock337, shock339, del_manushr, del_erateM, del_erateF)) %>%
  mutate(bartik = share311*shock311 + share312*shock312 + share313*shock313 + share314*shock314 + share315*shock315 + share316*shock316 + share321*shock321 + share322*shock322 + share323*shock323 + share324*shock324 + share325*shock325 + share326*shock326 + share327*shock327 + share331*shock331 + share332*shock332 + share333*shock333 + share334*shock334 + share335*shock335 + share336*shock336 + share337*shock337 + share339*shock339)

#OLS
reg01_16M <- lm(del_erateM ~ del_manushr + fe_lfp + for_bor + he_share, data = set01_16)
reg01_16F <- lm(del_erateF ~ del_manushr + fe_lfp + for_bor + he_share, data = set01_16)

stargazer(reg01_16M, reg01_16F, omit = c("he_share", "fe_lfp", "for_bor"), type = "text")

coeftest(reg01_16M, vcov = vcovHC, type = "HC0")
coeftest(reg01_16F, vcov = vcovHC, type = "HC0")

#Bartik Instrument (BIG dataset)

reg01_16IVM <- ivreg(del_erateM ~ del_manushr + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_16)
reg01_16IVF <- ivreg(del_erateF ~ del_manushr + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_16)

stargazer(reg01_16IVM, reg01_16IVF, omit = c("he_shr", "fe_lfp", "for_bor"), type = "text")

cluster.robust.se(reg01_16IVM, set01_16$Pr) #Heteroskedasticity robust se's clustered by province
cluster.robust.se(reg01_16IVF, set01_16$Pr)

#Bartik Instrument (SMALL dataset)

set01_16s <- subset(set01_16, CMA == 205 | CMA == 421 | CMA == 462 | CMA == 433 | CMA == 505 | CMA == 532 | CMA == 535 | CMA == 537 | CMA == 539 | CMA == 541 | CMA == 555 | CMA == 559 | CMA == 602 | CMA == 725 | CMA == 825 | CMA == 835 | CMA == 933 | CMA == 935)

reg01_16IVMs <- ivreg(del_erateM ~ del_manushr + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_16s)
reg01_16IVFs <- ivreg(del_erateF ~ del_manushr + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_16s)

stargazer(reg01_16IVMs, reg01_16IVFs, omit = c("he_shr", "fe_lfp", "for_bor"), type = "text")

coeftest(reg01_16IVMs, vcov = vcovHC, type = "HC0")
coeftest(reg01_16IVFs, vcov = vcovHC, type = "HC0")

##Regression ~ 2001-2011

aggregate_set11 <- subset(aggregate_set, CMA != 11) #Eliminate Gander (no observation in 2011)

del_manushr11 <-  subset(aggregate_set11, Year == 2011)$manu_shr - subset(aggregate_set11, Year == 2001)$manu_shr

del_erate11M <- subset(aggregate_set11, Year == 2011)$erateM - subset(aggregate_set11, Year == 2001)$erateM

del_erate11F <- subset(aggregate_set11, Year == 2011)$erateF - subset(aggregate_set11, Year == 2001)$erateF

shock311 <- vector()
shock312 <- vector()
shock313 <- vector()
shock314 <- vector()
shock315 <- vector()
shock316 <- vector()
shock321 <- vector()
shock322 <- vector()
shock323 <- vector()
shock324 <- vector()
shock325 <- vector()
shock326 <- vector()
shock327 <- vector()
shock331 <- vector()
shock332 <- vector()
shock333 <- vector()
shock334 <- vector()
shock335 <- vector()
shock336 <- vector()
shock337 <- vector()
shock339 <- vector()

foreach (i = 1:134, j = subset(aggregate_set11, Year == 2001)$CMA) %do%  {
  shock311[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E311) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E311) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock312[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E312) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E312) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock313[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E313) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E313) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock314[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E314) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E314) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock315[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E315) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E315) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock316[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E316) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E316) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock321[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E321) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E321) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock322[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E322) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E322) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock323[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E323) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E323) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock324[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E324) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E324) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock325[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E325) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E325) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock326[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E326) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E326) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock327[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E327) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E327) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock331[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E331) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E331) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock332[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E332) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E332) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock333[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E333) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E333) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock334[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E334) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E334) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock335[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E335) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E335) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock336[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E336) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E336) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock337[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E337) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E337) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
  shock339[i] <- (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E339) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2001 & CMA != j)$E339) / sum(subset(aggregate_set, Year == 2001 & CMA != j)$employment))
}

set01_11 <- data.frame(subset(aggregate_set11, Year == 2001), shock311, shock312, shock313, shock314, shock315, shock316, shock321, shock322, shock323, shock324, shock325, shock326, shock327, shock331, shock332, shock333, shock334, shock335, shock336, shock337, shock339, del_manushr11, del_erate11M, del_erate11F) %>%
  mutate(bartik = share311*shock311 + share312*shock312 + share313*shock313 + share314*shock314 + share315*shock315 + share316*shock316 + share321*shock321 + share322*shock322 + share323*shock323 + share324*shock324 + share325*shock325 + share326*shock326 + share327*shock327 + share331*shock331 + share332*shock332 + share333*shock333 + share334*shock334 + share335*shock335 + share336*shock336 + share337*shock337 + share339*shock339)

#OLS
reg01_11M <- lm(del_erate11M ~ del_manushr11 + fe_lfp + for_bor + he_share, data = set01_11)
reg01_11F <- lm(del_erate11F ~ del_manushr11 + fe_lfp + for_bor + he_share, data = set01_11)

stargazer(reg01_11M, reg01_11F, omit = c("he_share", "fe_lfp", "for_bor"), type = "text")

coeftest(reg01_11M, vcov = vcovHC, type = "HC0")
coeftest(reg01_11F, vcov = vcovHC, type = "HC0")

#Bartik Instrument (BIG Dataset)

reg01_11IVM <- ivreg(del_erate11M ~ del_manushr11 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_11)
reg01_11IVF <- ivreg(del_erate11F ~ del_manushr11 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_11)

stargazer(reg01_11IVM, reg01_11IVF, omit = c("he_share", "fe_lfp", "for_bor"), type = "text")

cluster.robust.se(reg01_11IVM, set01_11$Pr)
cluster.robust.se(reg01_11IVF, set01_11$Pr)

#Bartik Instrument (SMALL Dataset)

set01_11s <- subset(set01_11, CMA == 205 | CMA == 421 | CMA == 462 | CMA == 433 | CMA == 505 | CMA == 532 | CMA == 535 | CMA == 537 | CMA == 539 | CMA == 541 | CMA == 555 | CMA == 559 | CMA == 602 | CMA == 725 | CMA == 825 | CMA == 835 | CMA == 933 | CMA == 935)

reg01_11IVMs <- ivreg(del_erate11M ~ del_manushr11 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_11s)
reg01_11IVFs <- ivreg(del_erate11F ~ del_manushr11 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_11s)

stargazer(reg01_11IVMs, reg01_11IVFs, omit = c("he_share", "fe_lfp", "for_bor"), type = "text")

coeftest(reg01_11IVMs, vcov = vcovHC, type = "HC0")
coeftest(reg01_11IVFs, vcov = vcovHC, type = "HC0")

#Rotemberg Weights

share_set <- data.frame(cbind(set01_11$CMA, set01_11$share311, set01_11$share312, set01_11$share313, set01_11$share314, set01_11$share315, set01_11$share316, set01_11$share321, set01_11$share322, set01_11$share323, set01_11$share324, set01_11$share325, set01_11$share326, set01_11$share327, set01_11$share331, set01_11$share332, set01_11$share333, set01_11$share334, set01_11$share335, set01_11$share336, set01_11$share337, set01_11$share339))
colnames(share_set) <- c("CMA", "shr311", "shr312", "shr313", "shr314", "shr315", "shr316", "shr321", "shr322", "shr323", "shr324", "shr325", "shr326", "shr327", "shr331", "shr332", "shr333", "shr334", "shr335", "shr336", "shr337", "shr339")
share_set$CMA <- as.factor(share_set$CMA)
share_set<- gather(share_set, key = "E", value = "shr", shr311:shr339, factor_key = T)
share_set <- share_set[order(share_set$CMA, share_set$E),]
share_set %>%
  mutate(E = str_glue("c{CMA}n{E}")) %>%
  spread(E, shr, fill = 0) %>%
  print() -> share_set2

Elist <- as.character(str_glue("c{share_set$CMA}n{share_set$E}"))
share_set2 <- share_set2[, Elist]

shock_set <- data.frame(cbind(set01_11$CMA, shock311, shock312, shock313, shock314, shock315, shock316, shock321, shock322, shock323, shock324, shock325, shock326, shock327, shock331, shock332, shock333, shock334, shock335, shock336, shock337, shock339))
colnames(shock_set)[1] <- "CMA"
shock_set$CMA <- as.factor(shock_set$CMA)
shock_set <- gather(shock_set, key = "E", value = "shock", shock311:shock339, factor_key = T)
shock_set <- shock_set[order(shock_set$CMA, shock_set$E),]

y = "del_erate11M"
x = "del_manushr11"
controls = c("he_share", "fe_lfp", "for_bor")
weight = "bartik"
Z = setdiff(names(share_set2), c("CMA", "noc"))
G = "shock"

rotemberg <- bw(set01_11, y, x, controls, weight, share_set2, Z, shock_set, G)

rotembergMed <- aggregate(rotemberg[, 4], list(rotemberg$E), mean) %>% #Median over all CMAs
  arrange(desc(alpha)) %>%
  select(Group.1, alpha)

rotembergMedAlpha <- aggregate(rotemberg[, 4], list(rotemberg$E), median)
rotembergMedBeta <- aggregate(na.omit(rotemberg)[, 5], list(na.omit(rotemberg)$E), mean)

fs311 <- lm(del_manushr11 ~ shock311*share311 + fe_lfp + he_share + for_bor, data = set01_11)
fs312 <- lm(del_manushr11 ~ shock312*share312 + fe_lfp + he_share + for_bor, data = set01_11)
fs313 <- lm(del_manushr11 ~ shock313*share313 + fe_lfp + he_share + for_bor, data = set01_11)
fs314 <- lm(del_manushr11 ~ shock314*share314 + fe_lfp + he_share + for_bor, data = set01_11)
fs315 <- lm(del_manushr11 ~ shock315*share315 + fe_lfp + he_share + for_bor, data = set01_11)
fs316 <- lm(del_manushr11 ~ shock316*share316 + fe_lfp + he_share + for_bor, data = set01_11)
fs321 <- lm(del_manushr11 ~ shock321*share321 + fe_lfp + he_share + for_bor, data = set01_11)
fs322 <- lm(del_manushr11 ~ shock322*share322 + fe_lfp + he_share + for_bor, data = set01_11)
fs323 <- lm(del_manushr11 ~ shock323*share323 + fe_lfp + he_share + for_bor, data = set01_11)
fs324 <- lm(del_manushr11 ~ shock324*share324 + fe_lfp + he_share + for_bor, data = set01_11)
fs325 <- lm(del_manushr11 ~ shock325*share325 + fe_lfp + he_share + for_bor, data = set01_11)
fs326 <- lm(del_manushr11 ~ shock326*share326 + fe_lfp + he_share + for_bor, data = set01_11)
fs327 <- lm(del_manushr11 ~ shock327*share327 + fe_lfp + he_share + for_bor, data = set01_11)
fs331 <- lm(del_manushr11 ~ shock331*share331 + fe_lfp + he_share + for_bor, data = set01_11)
fs332 <- lm(del_manushr11 ~ shock332*share332 + fe_lfp + he_share + for_bor, data = set01_11)
fs333 <- lm(del_manushr11 ~ shock333*share333 + fe_lfp + he_share + for_bor, data = set01_11)
fs334 <- lm(del_manushr11 ~ shock334*share334 + fe_lfp + he_share + for_bor, data = set01_11)
fs335 <- lm(del_manushr11 ~ shock335*share335 + fe_lfp + he_share + for_bor, data = set01_11)
fs336 <- lm(del_manushr11 ~ shock336*share336 + fe_lfp + he_share + for_bor, data = set01_11)
fs337 <- lm(del_manushr11 ~ shock337*share337 + fe_lfp + he_share + for_bor, data = set01_11)
fs339 <- lm(del_manushr11 ~ shock339*share339 + fe_lfp + he_share + for_bor, data = set01_11)

fstats <- c(3.796, 5.067, 8.332, 6, 4.373, 6.539, 3.834, 4.8, 8.093, 3.789, 9.608, 12.61, 4.89, 4.351, 7.772, 6.243, 8.683, 5.359, 8.263, 4.996, 6.928)

hetset <- data.frame(cbind(rotembergMedAlpha, rotembergMedBeta$beta, fstats))
colnames(hetset) <- c("shr", "alpha", "beta", "fstat")

#Mechanisms ~ 2001-2011

set01_11 <- set01_11 %>%
  mutate(del_lnpop = subset(aggregate_set, CMA != 11 & Year == 2011)$lnpop - subset(aggregate_set, CMA != 11 & Year == 2001)$lnpop) %>%
  mutate(del_lfnpM = subset(aggregate_set, CMA != 11 & Year == 2011)$lfnpM - subset(aggregate_set, CMA != 11 & Year == 2001)$lfnpM) %>%
  mutate(del_lfnpF = subset(aggregate_set, CMA != 11 & Year == 2011)$lfnpF - subset(aggregate_set, CMA != 11 & Year == 2001)$lfnpF)

regpop01_11 <- ivreg(del_lnpop ~ del_manushr11 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_11)
reglfnpM01_11 <- ivreg(del_lfnpM ~ del_manushr11 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_11)
reglfnpF01_11 <- ivreg(del_lfnpF ~ del_manushr11 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set01_11)

cluster.robust.se(regpop01_11, set01_11$Pr)
cluster.robust.se(reglfnpM01_11, set01_11$Pr)
cluster.robust.se(reglfnpF01_11, set01_11$Pr)

regpop01_11OLS <- lm(del_lnpop ~ del_manushr11 + fe_lfp + for_bor + he_share, data = set01_11)
reglfnpM01_11OLS <- lm(del_lfnpM ~ del_manushr11 + fe_lfp + for_bor + he_share, data = set01_11)
reglfnpF01_11OLS <- lm(del_lfnpF ~ del_manushr11 + fe_lfp + for_bor + he_share, data = set01_11)

coeftest(regpop01_11OLS, vcov = vcovHC, type = "HC0")
coeftest(reglfnpM01_11OLS, vcov = vcovHC, type = "HC0")
coeftest(reglfnpF01_11OLS, vcov = vcovHC, type = "HC0")

stargazer(regpop01_11OLS, regpop01_11, reglfnpM01_11OLS, reglfnpM01_11, reglfnpF01_11OLS, reglfnpF01_11, omit = c("fe_lfp", "for_bor", "he_share"), title = "Explaining the Effect of Manufacturing Decline on Labour Market Outcomes (2001-2011; NAICS Set)")

##Regression ~ 2011-2016

aggregate_set11 <- subset(aggregate_set, CMA != 11) #Eliminate Gander (no observation in 2011)

del_manushr11_16 <-  subset(aggregate_set11, Year == 2016)$manu_shr - subset(aggregate_set11, Year == 2011)$manu_shr

del_erate11_16M <- subset(aggregate_set11, Year == 2016)$erateM - subset(aggregate_set11, Year == 2011)$erateM

del_erate11_16F <- subset(aggregate_set11, Year == 2016)$erateF - subset(aggregate_set11, Year == 2011)$erateF

shock311 <- vector()
shock312 <- vector()
shock313 <- vector()
shock314 <- vector()
shock315 <- vector()
shock316 <- vector()
shock321 <- vector()
shock322 <- vector()
shock323 <- vector()
shock324 <- vector()
shock325 <- vector()
shock326 <- vector()
shock327 <- vector()
shock331 <- vector()
shock332 <- vector()
shock333 <- vector()
shock334 <- vector()
shock335 <- vector()
shock336 <- vector()
shock337 <- vector()
shock339 <- vector()

foreach (i = 1:134, j = subset(aggregate_set11, Year == 2011)$CMA) %do%  {
  shock311[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E311) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E311) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock312[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E312) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E312) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock313[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E313) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E313) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock314[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E314) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E314) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock315[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E315) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E315) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock316[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E316) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E316) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock321[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E321) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E321) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock322[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E322) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E322) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock323[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E323) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E323) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock324[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E324) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E324) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock325[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E325) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E325) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock326[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E326) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E326) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock327[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E327) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E327) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock331[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E331) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E331) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock332[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E332) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E332) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock333[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E333) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E333) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock334[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E334) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E334) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock335[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E335) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E335) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock336[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E336) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E336) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock337[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E337) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E337) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
  shock339[i] <- (sum(subset(aggregate_set, Year == 2016 & CMA != j)$E339) / sum(subset(aggregate_set, Year == 2016 & CMA != j)$employment)) - (sum(subset(aggregate_set, Year == 2011 & CMA != j)$E339) / sum(subset(aggregate_set, Year == 2011 & CMA != j)$employment))
}

set11_16 <- data.frame(subset(aggregate_set11, Year == 2011), shock311, shock312, shock313, shock314, shock315, shock316, shock321, shock322, shock323, shock324, shock325, shock326, shock327, shock331, shock332, shock333, shock334, shock335, shock336, shock337, shock339, del_manushr11_16, del_erate11_16M, del_erate11_16F) %>%
  mutate(bartik = share311*shock311 + share312*shock312 + share313*shock313 + share314*shock314 + share315*shock315 + share316*shock316 + share321*shock321 + share322*shock322 + share323*shock323 + share324*shock324 + share325*shock325 + share326*shock326 + share327*shock327 + share331*shock331 + share332*shock332 + share333*shock333 + share334*shock334 + share335*shock335 + share336*shock336 + share337*shock337 + share339*shock339)

#OLS
reg11_16M <- lm(del_erate11_16M ~ del_manushr11_16 + fe_lfp + for_bor + he_share, data = set11_16)
reg11_16F <- lm(del_erate11_16F ~ del_manushr11_16 + fe_lfp + for_bor + he_share, data = set11_16)

stargazer(reg11_16M, reg11_16F, omit = c("he_share", "fe_lfp", "for_bor"), type = "text")

coeftest(reg11_16M, vcov = vcovHC, type = "HC0")
coeftest(reg11_16F, vcov = vcovHC, type = "HC0")

#Bartik Instrument

reg11_16IVM <- ivreg(del_erate11_16M ~ del_manushr11_16 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set11_16)
reg11_16IVF <- ivreg(del_erate11_16F ~ del_manushr11_16 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = set11_16)

stargazer(reg11_16IVM, reg11_16IVF, omit = c("he_share", "fe_lfp", "for_bor"), type = "text")

cluster.robust.se(reg11_16IVM, set11_16$Pr) #Heteroskedasticity robust se's clustered by province
cluster.robust.se(reg11_16IVF, set11_16$Pr)

#Rotemberg Weights

share_set <- data.frame(cbind(set11_16$CMA, set11_16$share311, set11_16$share312, set11_16$share313, set11_16$share314, set11_16$share315, set11_16$share316, set11_16$share321, set11_16$share322, set11_16$share323, set11_16$share324, set11_16$share325, set11_16$share326, set11_16$share327, set11_16$share331, set11_16$share332, set11_16$share333, set11_16$share334, set11_16$share335, set11_16$share336, set11_16$share337, set11_16$share339))
colnames(share_set) <- c("CMA", "shr311", "shr312", "shr313", "shr314", "shr315", "shr316", "shr321", "shr322", "shr323", "shr324", "shr325", "shr326", "shr327", "shr331", "shr332", "shr333", "shr334", "shr335", "shr336", "shr337", "shr339")
share_set$CMA <- as.factor(share_set$CMA)
share_set<- gather(share_set, key = "E", value = "shr", shr311:shr339, factor_key = T)
share_set <- share_set[order(share_set$CMA, share_set$E),]
share_set %>%
  mutate(E = str_glue("c{CMA}n{E}")) %>%
  spread(E, shr, fill = 0) %>%
  print() -> share_set2

Elist <- as.character(str_glue("c{share_set$CMA}n{share_set$E}"))
share_set2 <- share_set2[, Elist]

shock_set <- data.frame(cbind(set11_16$CMA, set11_16$shock311, set11_16$shock312, set11_16$shock313, set11_16$shock314, set11_16$shock315, set11_16$shock316, set11_16$shock321, set11_16$shock322, set11_16$shock323, set11_16$shock324, set11_16$shock325, set11_16$shock326, set11_16$shock327, set11_16$shock331, set11_16$shock332, set11_16$shock333, set11_16$shock334, set11_16$shock335, set11_16$shock336, set11_16$shock337, set11_16$shock339))
colnames(shock_set) <- c("CMA", "shock311", "shock312", "shock313", "shock314", "shock315", "shock316", "shock321", "shock322", "shock323", "shock324", "shock325", "shock326", "shock327", "shock331", "shock332", "shock333", "shock334", "shock335", "shock336", "shock337", "shock339")
shock_set$CMA <- as.factor(shock_set$CMA)
shock_set <- gather(shock_set, key = "E", value = "shock", shock311:shock339, factor_key = T)
shock_set <- shock_set[order(shock_set$CMA, shock_set$E),]

y = "del_erate11_16M"
x = "del_manushr11_16"
controls = c("he_share", "fe_lfp", "for_bor")
weight = "bartik"
Z = setdiff(names(share_set2), c("CMA", "E"))
G = "shock"

rotemberg1116 <- bw(set11_16, y, x, controls, weight, share_set2, Z, shock_set, G)

rotembergMed1116 <- aggregate(rotemberg1116[, 4], list(rotemberg1116$E), median) %>% #Median over all CMAs
  arrange(desc(alpha)) %>%
  select(Group.1, alpha)

#TABLE (OLS)
stargazer(reg01_16M, reg01_16F, reg01_11M, reg01_11F, reg11_16M, reg11_16F, omit = c("he_share", "fe_lfp", "for_bor"), title = "Effect of Manufacturing Decline on Employment Rate (OLS Estimates, Large Sample)")

#TABLE (IV)
stargazer(reg01_16IVM, reg01_16IVF, reg01_11IVM, reg01_11IVF, reg11_16IVM, reg11_16IVF, title = "Effect of Manufacturing Decline on Employment Rate (Sub-industry Instrument)")

###Summary Stats and Graphs

#Heat Map data

heat <- data.frame(set01_16$CMA, set01_16$manu_shr) #Manufacturing share map data
colnames(heat) <- c("CMAUID", "Manufacturing Share")
heat <- subset(heat, CMAUID < 997)
writexl::write_xlsx(heat, "heat_shareL.xlsx")

heat2 <- data.frame(set01_16$CMA, set01_16$del_manushr) #Manufacturing decline map data
colnames(heat2) <- c("CMAUID", "Change in Manu Share")
heat2 <- subset(heat2, CMAUID < 997)
writexl::write_xlsx(heat2, "heat_share_changeL.xlsx")

###Identification Tests

male2001 <- subset(male, Year == 2001 & CMA != 11)
female2001 <- subset(female, Year == 2001 & CMA != 11)

covset <- subset(read_xlsx("Aggregate Set Covariates.xlsx"), CMA != 11)
testset <- data.frame(cbind(set01_11, male2001$pop, covset$Commute, covset$Selfemp, covset$Avgage, covset$propval))
colnames(testset)[85:89] <- c("pop", "commute", "selfemp", "avgage", "propval")

testset <- testset %>%
  mutate(maleshr = male2001$Employed_Male / female2001$Employed_Female) %>%
  mutate(urban = case_when(
    pop >= 100000 ~ 1,
    pop < 100000 ~ 0
  )) %>%
  mutate(selfshr = selfemp / employment) %>%
  mutate(lnval = log(propval))
  
#Test 1: Instrument Strength

firststage <- lm(del_manushr11 ~ share311*shock311 + share312*shock312 + share313*shock313 + share314*shock314 + share315*shock315 + share316*shock316 + share321*shock321 + share322*shock322 + share323*shock323 + share324*shock324 + share325*shock325 + share326*shock326 + share327*shock327 + share331*shock331 + share332*shock332 + share333*shock333 + share334*shock334 + share335*shock335 + share336*shock336 + share337*shock337 + share339*shock339 + fe_lfp + he_share + for_bor, data = testset)
firststageB <- lm(del_manushr11 ~ bartik + fe_lfp + for_bor + he_share, data = testset)
stargazer(firststage, type = "text")

#Test 2: Covariates

testset$Pr <- as.factor(testset$Pr)

cov332 <- lm(share332 ~ Pr + for_bor + fe_lfp + he_share + maleshr + urban + lnval + commute + selfshr + avgage, data = testset)
cov334 <- lm(share334 ~ Pr + for_bor + fe_lfp + he_share + maleshr + urban + lnval + commute + selfshr + avgage, data = testset)
cov326 <- lm(share326 ~ Pr + for_bor + fe_lfp + he_share + maleshr + urban + lnval + commute + selfshr + avgage, data = testset)
cov336 <- lm(share336 ~ Pr + for_bor + fe_lfp + he_share + maleshr + urban + lnval + commute + selfshr + avgage, data = testset)
cov331 <- lm(share331 ~ Pr + for_bor + fe_lfp + he_share + maleshr + urban + lnval + commute + selfshr + avgage, data = testset)

stargazer(cov336, cov332, cov326, cov334, cov331, omit = c("Pr"))

coeftest(cov336, vcov = vcovHC, type = "HC0")
coeftest(cov332, vcov = vcovHC, type = "HC0")
coeftest(cov326, vcov = vcovHC, type = "HC0")
coeftest(cov334, vcov = vcovHC, type = "HC0")
coeftest(cov331, vcov = vcovHC, type = "HC0")

#Test 4: Alternate Estimators and Overid test

IVM <- IVreg(del_erate11M ~ del_manushr11 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = testset)
IVF <- IVreg(del_erate11F ~ del_manushr11 + fe_lfp + for_bor + he_share | bartik + fe_lfp + for_bor + he_share, data = testset)

Y = testset[, "del_erate11M"]
D = testset[, "del_manushr11"]
Z = testset[, "bartik"]
X = testset[, c("he_share", "fe_lfp", "for_bor")]

TestModel <- ivmodel(Y = Y, D = D, Z = Z, X = X)
HFULM <- Fuller(TestModel, heteroSE = T)

Y = testset[, "del_erate11F"]
TestModel2 <- ivmodel(Y = Y, D = D, Z = Z, X = X)
HFULF <- Fuller(TestModel2, heteroSE = T)

IVM
HFULM
IVF
HFULF

#Overid test: Sargan's Test

residualsM <- reg01_11IVM$residuals
stest <- lm(residualsM ~ share311*shock311 + share312*shock312 + share313*shock313 + share314*shock314 + share315*shock315 + share316*shock316 + share321*shock321 + share322*shock322 + share323*shock323 + share324*shock324 + share325*shock325 + share326*shock326 + share327*shock327 + share331*shock331 + share332*shock332 + share333*shock333 + share334*shock334 + share335*shock335 + share336*shock336 + share337*shock337 + share339*shock339 + fe_lfp + he_share + for_bor, data = testset)
summary(stest)
teststat <- 0.2503*133 #TR^2 (adjusted) follows approximately chi-square
teststat > qchisq(0.95, df = 19) #Reject null hypothesis; there is statistically significant evidence to suggest that the instruments are endogenous

pchisq(teststat, df = 19)

#Overid test: Anderson-Rubin Test

AR.test(TestModel) #Reject null hypothesis -- there is evidence to suggest that the instrument is endogenous

#Investigating possible heterogeneity

ggplot(hetset, aes(x = fstat, y = beta)) + geom_point(shape = 1, aes(size = alpha), color = "steelblue", show.legend = F) + xlab("First Stage F-Statistic") + ylab("IV Estimate") + ggtitle("Heterogeneity of IV Estimates") + geom_hline(yintercept = 0.421, linetype = "dashed")

###ROBUSTNESS: Adding Controls

##2001-2016

set01_16 <- subset(set01_16, CMA != 11)

setC <- data.frame(cbind(set01_16, male2001$pop, covset$Commute, covset$Selfemp, covset$Avgage, covset$propval))
colnames(setC)[82:86] <- c("pop", "commute", "selfemp", "avgage", "propval")

setC <- setC %>%
  mutate(maleshr = male2001$Employed_Male / female2001$Employed_Female) %>%
  mutate(urban = case_when(
    pop >= 100000 ~ 1,
    pop < 100000 ~ 0
  )) %>%
  mutate(selfshr = selfemp / employment) %>%
  mutate(lnval = log(propval))

reg01_16IVMC <- ivreg(del_erateM ~ del_manushr + Pr +  fe_lfp + for_bor + he_share + maleshr + urban + lnval + commute + selfshr + avgage | bartik + fe_lfp + for_bor + he_share + maleshr + urban + lnval + commute + selfshr + avgage + Pr, data = setC)
reg01_16IVFC <- ivreg(del_erateF ~ del_manushr + fe_lfp + for_bor + he_share + maleshr + urban + lnval + commute + selfshr + avgage + Pr | bartik + fe_lfp + for_bor + he_share + maleshr + urban + lnval + commute + selfshr + avgage + Pr, data = setC)

stargazer(reg01_16IVMC, reg01_16IVFC, omit = c("he_share", "fe_lfp", "for_bor"), type = "text")


##2001-2011

testset$Pr <- as.factor(testset$Pr)

reg01_11IVMC <- ivreg(del_erate11M ~ del_manushr11 + Pr +  fe_lfp + for_bor + he_share + maleshr + urban + lnval + commute + selfshr + avgage | bartik + fe_lfp + for_bor + he_share + maleshr + urban + lnval + commute + selfshr + avgage + Pr, data = testset)
reg01_11IVFC <- ivreg(del_erate11F ~ del_manushr11 + fe_lfp + for_bor + he_share + maleshr + urban + lnval + commute + selfshr + avgage + Pr | bartik + fe_lfp + for_bor + he_share + maleshr + urban + lnval + commute + selfshr + avgage + Pr, data = testset)

stargazer(reg01_11IVMC, reg01_11IVFC, omit = c("he_share", "fe_lfp", "for_bor"), type = "text")

cluster.robust.se(reg01_11IVMC, set01_11$Pr) #Heteroskedasticity robust se's clustered by province
cluster.robust.se(reg01_11IVFC, set01_11$Pr)

##ROBUSTNESS: Morissette IV

reg01_11MMor <- ivreg(del_erate11M ~ del_manushr11 + fe_lfp + for_bor + he_share | manu_shr + fe_lfp + for_bor + he_share, data = set01_11)
reg01_11FMor <- ivreg(del_erate11F ~ del_manushr11 + fe_lfp + for_bor + he_share | manu_shr + fe_lfp + for_bor + he_share, data = set01_11)

cluster.robust.se(reg01_11MMor, set01_11$Pr) #Heteroskedasticity robust se's clustered by province
cluster.robust.se(reg01_11FMor, set01_11$Pr)

#TABLE
stargazer(reg01_16IVMs, reg01_16IVFs, reg01_11IVMC, reg01_11IVFC, reg01_11MMor, reg01_11FMor, omit = "Pr", title = "Robustness Checks II: Large Dataset")
