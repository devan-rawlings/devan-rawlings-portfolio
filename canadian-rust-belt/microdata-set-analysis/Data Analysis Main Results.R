#! Data Analysis Main Results.R
## This script constructs a few variables for regression analysis, then runs all regressions on the 'NOC set,' including the main regressions,
## identification tests and robustness checks.

library(dplyr)
library(readstata13)
library(foreach)
library(haven)
library(graphics)
library(AER)
library(stargazer)
library(tidyr)
library(bartik.weight)
library(tidyverse)
library(foreach)
library(ivmodel) #LIML
library(ManyIV) #IVreg
library(ivpack)
library(clusterSEs)
library(lmtest)
library(sandwich)

##Construct Dataset 1
census <- data.frame(read.dta13("CMA_panel.dta"))

census$Year <- as.numeric(census$Year) #Convert dataframe variables to proper types
census$CMAList <- as.numeric(census$CMAList)
for(i in 4:71)  {
  census[,i] <- as.numeric(census[,i])
}
census$province <- as.factor(census$province)

CMAList <- c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 205, 421, 462, 499, 505, 532, 535, 537, 539, 541, 555, 559, 602, 799, 825, 835, 933, 935)
#Controls
he_shr <- subset(census, Year == 2001)$he_shr
fe_lfp <- subset(census, Year == 2001)$fe_lfs
for_bor <- subset(census, Year == 2001)$for_bor

#Independent variable

del_msMH <- vector()
del_msML <- vector()
del_msFH <- vector()
del_msFL <- vector()
del_ms <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  del_msMH[j] <- subset(census, Year == 2016 & CMAList == i)$manu_shrMH - subset(census, Year == 2001 & CMAList == i)$manu_shrMH
  del_msML[j] <- subset(census, Year == 2016 & CMAList == i)$manu_shrML - subset(census, Year == 2001 & CMAList == i)$manu_shrML
  del_msFH[j] <- subset(census, Year == 2016 & CMAList == i)$manu_shrFH - subset(census, Year == 2001 & CMAList == i)$manu_shrFH
  del_msFL[j] <- subset(census, Year == 2016 & CMAList == i)$manu_shrFL - subset(census, Year == 2001 & CMAList == i)$manu_shrFL
  del_ms[j] <- subset(census, Year == 2016 & CMAList == i)$manu_shr - subset(census, Year == 2001 & CMAList == i)$manu_shr
}

#Dependent variable

del_erateMH <- vector()
del_erateML <- vector()
del_erateFH <- vector()
del_erateFL <- vector()
del_wageMH <- vector()
del_wageML <- vector()
del_wageFH <- vector()
del_wageFL <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  del_erateMH[j] <- subset(census, Year == 2016 & CMAList == i)$erateMH - subset(census, Year == 2001 & CMAList == i)$erateMH
  del_erateML[j] <- subset(census, Year == 2016 & CMAList == i)$erateML - subset(census, Year == 2001 & CMAList == i)$erateML
  del_erateFH[j] <- subset(census, Year == 2016 & CMAList == i)$erateFH - subset(census, Year == 2001 & CMAList == i)$erateFH
  del_erateFL[j] <- subset(census, Year == 2016 & CMAList == i)$erateFL - subset(census, Year == 2001 & CMAList == i)$erateFL
  del_wageMH[j] <- subset(census, Year == 2016 & CMAList == i)$logwageMH - subset(census, Year == 2001 & CMAList == i)$logwageMH
  del_wageML[j] <- subset(census, Year == 2016 & CMAList == i)$logwageML - subset(census, Year == 2001 & CMAList == i)$logwageML
  del_wageFH[j] <- subset(census, Year == 2016 & CMAList == i)$logwageFH - subset(census, Year == 2001 & CMAList == i)$logwageFH
  del_wageFL[j] <- subset(census, Year == 2016 & CMAList == i)$logwageFL - subset(census, Year == 2001 & CMAList == i)$logwageFL
}

#Bartik Components

shock1 <- vector()
shock2 <- vector()
shock3 <- vector()
shock4 <- vector()
shock5 <- vector()
shock6 <- vector()
shock7 <- vector()
shock8 <- vector()
shock9 <- vector()
shock10 <- vector()
shock11 <- vector()
shock12 <- vector()
shock13 <- vector()
shock14 <- vector()
shock15 <- vector()
shock16 <- vector()
shock17 <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  shock1[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm1 - subset(census, Year == 2001 & CMAList == i)$man_shrm1
  shock2[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm2 - subset(census, Year == 2001 & CMAList == i)$man_shrm2
  shock3[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm3- subset(census, Year == 2001 & CMAList == i)$man_shrm3
  shock4[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm4 - subset(census, Year == 2001 & CMAList == i)$man_shrm4
  shock5[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm5 - subset(census, Year == 2001 & CMAList == i)$man_shrm5
  shock6[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm6 - subset(census, Year == 2001 & CMAList == i)$man_shrm6
  shock7[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm7 - subset(census, Year == 2001 & CMAList == i)$man_shrm7
  shock8[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm8 - subset(census, Year == 2001 & CMAList == i)$man_shrm8
  shock9[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm9 - subset(census, Year == 2001 & CMAList == i)$man_shrm9
  shock10[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm10 - subset(census, Year == 2001 & CMAList == i)$man_shrm10
  shock11[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm11 - subset(census, Year == 2001 & CMAList == i)$man_shrm11
  shock12[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm12 - subset(census, Year == 2001 & CMAList == i)$man_shrm12
  shock13[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm13 - subset(census, Year == 2001 & CMAList == i)$man_shrm13
  shock14[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm14 - subset(census, Year == 2001 & CMAList == i)$man_shrm14
  shock15[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm15 - subset(census, Year == 2001 & CMAList == i)$man_shrm15
  shock16[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm16 - subset(census, Year == 2001 & CMAList == i)$man_shrm16
  shock17[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm17 - subset(census, Year == 2001 & CMAList == i)$man_shrm17
}

NOCshr1 <- subset(census, Year == 2001)$NOC_shr1
NOCshr2 <- subset(census, Year == 2001)$NOC_shr2
NOCshr3 <- subset(census, Year == 2001)$NOC_shr3
NOCshr4 <- subset(census, Year == 2001)$NOC_shr4
NOCshr5 <- subset(census, Year == 2001)$NOC_shr5
NOCshr6 <- subset(census, Year == 2001)$NOC_shr6
NOCshr7 <- subset(census, Year == 2001)$NOC_shr7
NOCshr8 <- subset(census, Year == 2001)$NOC_shr8
NOCshr9 <- subset(census, Year == 2001)$NOC_shr9
NOCshr10 <- subset(census, Year == 2001)$NOC_shr10
NOCshr11 <- subset(census, Year == 2001)$NOC_shr11
NOCshr12 <- subset(census, Year == 2001)$NOC_shr12
NOCshr13 <- subset(census, Year == 2001)$NOC_shr13
NOCshr14 <- subset(census, Year == 2001)$NOC_shr14
NOCshr15 <- subset(census, Year == 2001)$NOC_shr15
NOCshr16 <- subset(census, Year == 2001)$NOC_shr16
NOCshr17 <- subset(census, Year == 2001)$NOC_shr17

set1 <- data.frame(cbind(CMAList, he_shr, fe_lfp, for_bor, del_msMH, del_msML, del_msFH, del_msFL, del_ms, del_erateMH, del_erateML, del_erateFH, del_erateFL, del_wageMH, del_wageML, del_wageFH, del_wageFL, shock1, shock2, shock3, shock4, shock5, shock6, shock7, shock8, shock9, shock10, shock11, shock12, shock13, shock14, shock15, shock16, shock17, NOCshr1, NOCshr2, NOCshr3, NOCshr4, NOCshr5, NOCshr6, NOCshr7, NOCshr8, NOCshr9, NOCshr10, NOCshr11, NOCshr12, NOCshr13, NOCshr14, NOCshr15, NOCshr16, NOCshr17))

share_set <- data.frame(cbind(CMAList, NOCshr1, NOCshr2, NOCshr3, NOCshr4, NOCshr5, NOCshr6, NOCshr7, NOCshr8, NOCshr9, NOCshr10, NOCshr11, NOCshr12, NOCshr13, NOCshr14, NOCshr15, NOCshr16, NOCshr17))
share_set$CMAList <- as.factor(share_set$CMAList)
share_set<- gather(share_set, key = "noc", value = "share", NOCshr1:NOCshr17, factor_key = T)
share_set <- share_set[order(share_set$CMAList, share_set$noc),]
share_set %>%
  mutate(noc = str_glue("c{CMAList}n{noc}")) %>%
  spread(noc, share, fill = 0) %>%
  print() -> share_set2

colorder <- c("CMAList", "c9nNOCshr1", "c9nNOCshr2", "c9nNOCshr3", "c9nNOCshr4", "c9nNOCshr5", "c9nNOCshr6", "c9nNOCshr7", "c9nNOCshr8", "c9nNOCshr9", "c9nNOCshr10", "c9nNOCshr11", "c9nNOCshr12", "c9nNOCshr13", "c9nNOCshr14", "c9nNOCshr15", "c9nNOCshr16", "c9nNOCshr17", "c19nNOCshr1", "c19nNOCshr2", "c19nNOCshr3", "c19nNOCshr4", "c19nNOCshr5", "c19nNOCshr6", "c19nNOCshr7", "c19nNOCshr8", "c19nNOCshr9", "c19nNOCshr10", "c19nNOCshr11", "c19nNOCshr12", "c19nNOCshr13", "c19nNOCshr14", "c19nNOCshr15", "c19nNOCshr16", "c19nNOCshr17", 
              "c29nNOCshr1", "c29nNOCshr2", "c29nNOCshr3", "c29nNOCshr4", "c29nNOCshr5", "c29nNOCshr6", "c29nNOCshr7", "c29nNOCshr8", "c29nNOCshr9", "c29nNOCshr10", "c29nNOCshr11", "c29nNOCshr12", "c29nNOCshr13", "c29nNOCshr14", "c29nNOCshr15", "c29nNOCshr16", "c29nNOCshr17", "c39nNOCshr1", "c39nNOCshr2", "c39nNOCshr3", "c39nNOCshr4", "c39nNOCshr5", "c39nNOCshr6", "c39nNOCshr7", "c39nNOCshr8", "c39nNOCshr9", "c39nNOCshr10", "c39nNOCshr11", "c39nNOCshr12", "c39nNOCshr13", "c39nNOCshr14", "c39nNOCshr15", "c39nNOCshr16", "c39nNOCshr17",
              "c49nNOCshr1", "c49nNOCshr2", "c49nNOCshr3", "c49nNOCshr4", "c49nNOCshr5", "c49nNOCshr6", "c49nNOCshr7", "c49nNOCshr8", "c49nNOCshr9", "c49nNOCshr10", "c49nNOCshr11", "c49nNOCshr12", "c49nNOCshr13", "c49nNOCshr14", "c49nNOCshr15", "c49nNOCshr16", "c49nNOCshr17", "c59nNOCshr1", "c59nNOCshr2", "c59nNOCshr3", "c59nNOCshr4", "c59nNOCshr5", "c59nNOCshr6", "c59nNOCshr7", "c59nNOCshr8", "c59nNOCshr9", "c59nNOCshr10", "c59nNOCshr11", "c59nNOCshr12", "c59nNOCshr13", "c59nNOCshr14", "c59nNOCshr15", "c59nNOCshr16", "c59nNOCshr17", 
              "c69nNOCshr1", "c69nNOCshr2", "c69nNOCshr3", "c69nNOCshr4", "c69nNOCshr5", "c69nNOCshr6", "c69nNOCshr7", "c69nNOCshr8", "c69nNOCshr9", "c69nNOCshr10", "c69nNOCshr11", "c69nNOCshr12", "c69nNOCshr13", "c69nNOCshr14", "c69nNOCshr15", "c69nNOCshr16", "c69nNOCshr17", "c79nNOCshr1", "c79nNOCshr2", "c79nNOCshr3", "c79nNOCshr4", "c79nNOCshr5", "c79nNOCshr6", "c79nNOCshr7", "c79nNOCshr8", "c79nNOCshr9", "c79nNOCshr10", "c79nNOCshr11", "c79nNOCshr12", "c79nNOCshr13", "c79nNOCshr14", "c79nNOCshr15", "c79nNOCshr16", "c79nNOCshr17", 
              "c89nNOCshr1", "c89nNOCshr2", "c89nNOCshr3", "c89nNOCshr4", "c89nNOCshr5", "c89nNOCshr6", "c89nNOCshr7", "c89nNOCshr8", "c89nNOCshr9", "c89nNOCshr10", "c89nNOCshr11", "c89nNOCshr12", "c89nNOCshr13", "c89nNOCshr14", "c89nNOCshr15", "c89nNOCshr16", "c89nNOCshr17", "c99nNOCshr1", "c99nNOCshr2", "c99nNOCshr3", "c99nNOCshr4", "c99nNOCshr5", "c99nNOCshr6", "c99nNOCshr7", "c99nNOCshr8", "c99nNOCshr9", "c99nNOCshr10", "c99nNOCshr11", "c99nNOCshr12", "c99nNOCshr13", "c99nNOCshr14", "c99nNOCshr15", "c99nNOCshr16", "c99nNOCshr17", 
              "c205nNOCshr1", "c205nNOCshr2", "c205nNOCshr3", "c205nNOCshr4", "c205nNOCshr5", "c205nNOCshr6", "c205nNOCshr7", "c205nNOCshr8", "c205nNOCshr9", "c205nNOCshr10", "c205nNOCshr11", "c205nNOCshr12", "c205nNOCshr13", "c205nNOCshr14", "c205nNOCshr15", "c205nNOCshr16", "c205nNOCshr17", "c421nNOCshr1", "c421nNOCshr2", "c421nNOCshr3", "c421nNOCshr4", "c421nNOCshr5", "c421nNOCshr6", "c421nNOCshr7", "c421nNOCshr8", "c421nNOCshr9", "c421nNOCshr10", "c421nNOCshr11", "c421nNOCshr12", "c421nNOCshr13", "c421nNOCshr14", "c421nNOCshr15", "c421nNOCshr16", "c421nNOCshr17",
              "c462nNOCshr1", "c462nNOCshr2", "c462nNOCshr3", "c462nNOCshr4", "c462nNOCshr5", "c462nNOCshr6", "c462nNOCshr7", "c462nNOCshr8", "c462nNOCshr9", "c462nNOCshr10", "c462nNOCshr11", "c462nNOCshr12", "c462nNOCshr13", "c462nNOCshr14", "c462nNOCshr15", "c462nNOCshr16", "c462nNOCshr17", "c499nNOCshr1", "c499nNOCshr2", "c499nNOCshr3", "c499nNOCshr4", "c499nNOCshr5", "c499nNOCshr6", "c499nNOCshr7", "c499nNOCshr8", "c499nNOCshr9", "c499nNOCshr10", "c499nNOCshr11", "c499nNOCshr12", "c499nNOCshr13", "c499nNOCshr14", "c499nNOCshr15", "c499nNOCshr16", "c499nNOCshr17",
              "c505nNOCshr1", "c505nNOCshr2", "c505nNOCshr3", "c505nNOCshr4", "c505nNOCshr5", "c505nNOCshr6", "c505nNOCshr7", "c505nNOCshr8", "c505nNOCshr9", "c505nNOCshr10", "c505nNOCshr11", "c505nNOCshr12", "c505nNOCshr13", "c505nNOCshr14", "c505nNOCshr15", "c505nNOCshr16", "c505nNOCshr17", "c532nNOCshr1", "c532nNOCshr2", "c532nNOCshr3", "c532nNOCshr4", "c532nNOCshr5", "c532nNOCshr6", "c532nNOCshr7", "c532nNOCshr8", "c532nNOCshr9", "c532nNOCshr10", "c532nNOCshr11", "c532nNOCshr12", "c532nNOCshr13", "c532nNOCshr14", "c532nNOCshr15", "c532nNOCshr16", "c532nNOCshr17",
              "c535nNOCshr1", "c535nNOCshr2", "c535nNOCshr3", "c535nNOCshr4", "c535nNOCshr5", "c535nNOCshr6", "c535nNOCshr7", "c535nNOCshr8", "c535nNOCshr9", "c535nNOCshr10", "c535nNOCshr11", "c535nNOCshr12", "c535nNOCshr13", "c535nNOCshr14", "c535nNOCshr15", "c535nNOCshr16", "c535nNOCshr17", "c537nNOCshr1", "c537nNOCshr2", "c537nNOCshr3", "c537nNOCshr4", "c537nNOCshr5", "c537nNOCshr6", "c537nNOCshr7", "c537nNOCshr8", "c537nNOCshr9", "c537nNOCshr10", "c537nNOCshr11", "c537nNOCshr12", "c537nNOCshr13", "c537nNOCshr14", "c537nNOCshr15", "c537nNOCshr16", "c537nNOCshr17",
              "c539nNOCshr1", "c539nNOCshr2", "c539nNOCshr3", "c539nNOCshr4", "c539nNOCshr5", "c539nNOCshr6", "c539nNOCshr7", "c539nNOCshr8", "c539nNOCshr9", "c539nNOCshr10", "c539nNOCshr11", "c539nNOCshr12", "c539nNOCshr13", "c539nNOCshr14", "c539nNOCshr15", "c539nNOCshr16", "c539nNOCshr17", "c541nNOCshr1", "c541nNOCshr2", "c541nNOCshr3", "c541nNOCshr4", "c541nNOCshr5", "c541nNOCshr6", "c541nNOCshr7", "c541nNOCshr8", "c541nNOCshr9", "c541nNOCshr10", "c541nNOCshr11", "c541nNOCshr12", "c541nNOCshr13", "c541nNOCshr14", "c541nNOCshr15", "c541nNOCshr16", "c541nNOCshr17", 
              "c555nNOCshr1", "c555nNOCshr2", "c555nNOCshr3", "c555nNOCshr4", "c555nNOCshr5", "c555nNOCshr6", "c555nNOCshr7", "c555nNOCshr8", "c555nNOCshr9", "c555nNOCshr10", "c555nNOCshr11", "c555nNOCshr12", "c555nNOCshr13", "c555nNOCshr14", "c555nNOCshr15", "c555nNOCshr16", "c555nNOCshr17", "c559nNOCshr1", "c559nNOCshr2", "c559nNOCshr3", "c559nNOCshr4", "c559nNOCshr5", "c559nNOCshr6", "c559nNOCshr7", "c559nNOCshr8", "c559nNOCshr9", "c559nNOCshr10", "c559nNOCshr11", "c559nNOCshr12", "c559nNOCshr13", "c559nNOCshr14", "c559nNOCshr15", "c559nNOCshr16", "c559nNOCshr17", 
              "c602nNOCshr1", "c602nNOCshr2", "c602nNOCshr3", "c602nNOCshr4", "c602nNOCshr5", "c602nNOCshr6", "c602nNOCshr7", "c602nNOCshr8", "c602nNOCshr9", "c602nNOCshr10", "c602nNOCshr11", "c602nNOCshr12", "c602nNOCshr13", "c602nNOCshr14", "c602nNOCshr15", "c602nNOCshr16", "c602nNOCshr17", "c799nNOCshr1", "c799nNOCshr2", "c799nNOCshr3", "c799nNOCshr4", "c799nNOCshr5", "c799nNOCshr6", "c799nNOCshr7", "c799nNOCshr8", "c799nNOCshr9", "c799nNOCshr10", "c799nNOCshr11", "c799nNOCshr12", "c799nNOCshr13", "c799nNOCshr14", "c799nNOCshr15", "c799nNOCshr16", "c799nNOCshr17", 
              "c825nNOCshr1", "c825nNOCshr2", "c825nNOCshr3", "c825nNOCshr4", "c825nNOCshr5", "c825nNOCshr6", "c825nNOCshr7", "c825nNOCshr8", "c825nNOCshr9", "c825nNOCshr10", "c825nNOCshr11", "c825nNOCshr12", "c825nNOCshr13", "c825nNOCshr14", "c825nNOCshr15", "c825nNOCshr16", "c825nNOCshr17", "c835nNOCshr1", "c835nNOCshr2", "c835nNOCshr3", "c835nNOCshr4", "c835nNOCshr5", "c835nNOCshr6", "c835nNOCshr7", "c835nNOCshr8", "c835nNOCshr9", "c835nNOCshr10", "c835nNOCshr11", "c835nNOCshr12", "c835nNOCshr13", "c835nNOCshr14", "c835nNOCshr15", "c835nNOCshr16", "c835nNOCshr17",
              "c933nNOCshr1", "c933nNOCshr2", "c933nNOCshr3", "c933nNOCshr4", "c933nNOCshr5", "c933nNOCshr6", "c933nNOCshr7", "c933nNOCshr8", "c933nNOCshr9", "c933nNOCshr10", "c933nNOCshr11", "c933nNOCshr12", "c933nNOCshr13", "c933nNOCshr14", "c933nNOCshr15", "c933nNOCshr16", "c933nNOCshr17", "c935nNOCshr1", "c935nNOCshr2", "c935nNOCshr3", "c935nNOCshr4", "c935nNOCshr5", "c935nNOCshr6", "c935nNOCshr7", "c935nNOCshr8", "c935nNOCshr9", "c935nNOCshr10", "c935nNOCshr11", "c935nNOCshr12", "c935nNOCshr13", "c935nNOCshr14", "c935nNOCshr15", "c935nNOCshr16", "c935nNOCshr17")

share_set2 <- share_set2[, colorder]

shock_set <- data.frame(cbind(CMAList, shock1, shock2, shock3, shock4, shock5, shock6, shock7, shock8, shock9, shock10, shock11, shock12, shock13, shock14, shock15, shock16, shock17))
shock_set$CMAList <- as.factor(shock_set$CMAList)
shock_set <- gather(shock_set, key = "noc", value = "shock", shock1:shock17, factor_key = T)
shock_set <- shock_set[order(shock_set$CMAList, shock_set$noc),]

set1 <- set1 %>%
  mutate(bart = NOCshr1*shock1 + NOCshr2*shock2 + NOCshr3*shock3 + NOCshr4*shock4 + NOCshr5*shock5 + NOCshr6*shock6 + NOCshr7*shock7 + NOCshr8*shock8 + NOCshr9*shock9 + NOCshr10*shock10 + NOCshr11*shock11 + NOCshr12*shock12 + NOCshr13*shock13 + NOCshr14*shock14 + NOCshr15*shock15 + NOCshr16*shock16 + NOCshr17*shock17)

write_dta(set1, "bartikset1.dta")

#Main Regression ~ OLS
regMHOLS <- lm(del_erateMH ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regMLOLS <- lm(del_erateML ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regFHOLS <- lm(del_erateFH ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regFLOLS <- lm(del_erateFL ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)

regMHWOLS <- lm(del_wageMH ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regMLWOLS <- lm(del_wageML ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regFHWOLS <- lm(del_wageFH ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regFLWOLS <- lm(del_wageFL ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)

stargazer(regMHOLS, regMLOLS, regFHOLS, regFLOLS, omit = c("he_shr", "fe_lfp", "for_bor"), title = "OLS Estimates of Main Regression by Demographic Group -- Employment", type = "text")
stargazer(regMHWOLS, regMLWOLS, regFHWOLS, regFLWOLS, omit = c("he_shr", "fe_lfp", "for_bor"), title = "OLS Estimates of Main Regression by Demographic Group -- Wages", type = "text")

#Robust SEs

coeftest(regMHOLS, vcov = vcovHC, type = "HC0")
coeftest(regMLOLS, vcov = vcovHC, type = "HC0")
coeftest(regFHOLS, vcov = vcovHC, type = "HC0")
coeftest(regFLOLS, vcov = vcovHC, type = "HC0")

coeftest(regMHWOLS, vcov = vcovHC, type = "HC0")
coeftest(regMLWOLS, vcov = vcovHC, type = "HC0")
coeftest(regFHWOLS, vcov = vcovHC, type = "HC0")
coeftest(regFLWOLS, vcov = vcovHC, type = "HC0")

#Main Regression ~ IV
regMH <- ivreg(del_erateMH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regML <- ivreg(del_erateML ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regFH <- ivreg(del_erateFH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regFL <- ivreg(del_erateFL ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)

regMHW <- ivreg(del_wageMH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regMLW <- ivreg(del_wageML ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regFHW <- ivreg(del_wageFH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regFLW <- ivreg(del_wageFL ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)

stargazer(regMH, regML, regFH, regFL, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Employment", type = "text")
stargazer(regMHW, regMLW, regFHW, regFLW, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Wages", type = "text")

coeftest(regMH, vcov = vcovHC, type = "HC0")
coeftest(regML, vcov = vcovHC, type = "HC0")
coeftest(regFH, vcov = vcovHC, type = "HC0")
coeftest(regFL, vcov = vcovHC, type = "HC0")

coeftest(regMHW, vcov = vcovHC, type = "HC0")
coeftest(regMLW, vcov = vcovHC, type = "HC0")
coeftest(regFHW, vcov = vcovHC, type = "HC0")
coeftest(regFLW, vcov = vcovHC, type = "HC0")

##TABLE
stargazer(regMHOLS, regMH, regMLOLS, regML, regFHOLS, regFH, regFLOLS, regFL, title = "Effect of Changing Manufacturing Share on Employment Rate by Demographic Group (2001-2016)")
stargazer(regMHWOLS, regMHW, regMLWOLS, regMLW, regFHWOLS, regFHW, regFLWOLS, regFLW, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Effect of Changing Manufacturing Share on (Log) Median Wage by Demographic Group (2001-2016)")

#Rotemberg weights ~ employment
y = "del_erateML"
x = "del_ms"
controls = c("he_shr", "fe_lfp", "for_bor")
weight = "bart"
Z = setdiff(names(share_set2), "CMAList")
G = "shock"

rotemberg <- bw(set1, y, x, controls, weight, share_set2, Z, shock_set, G)

#Take median rotemberg weights over all CMAs for each occupation group
rotembergE1 <- aggregate(rotemberg[, 4], list(rotemberg$noc), mean) %>%
  arrange(desc(alpha)) %>%
  select(Group.1, alpha)

### Morissette-like IV regressions
manu_shr <- subset(census, Year == 2001)$manu_shr
set2 <- data.frame(cbind(CMAList, he_shr, fe_lfp, for_bor, manu_shr, del_msMH, del_msML, del_msFH, del_msFL, del_erateMH, del_erateML, del_erateFH, del_erateFL, del_wageMH, del_wageML, del_wageFH, del_wageFL))

regMH2 <- ivreg(del_erateMH ~ del_ms + he_shr + fe_lfp + for_bor | manu_shr + he_shr + fe_lfp + for_bor, data = set2)
regML2 <- ivreg(del_erateML ~ del_ms + he_shr + fe_lfp + for_bor | manu_shr + he_shr + fe_lfp + for_bor, data = set2)
regFH2 <- ivreg(del_erateFH ~ del_ms + he_shr + fe_lfp + for_bor | manu_shr + he_shr + fe_lfp + for_bor, data = set2)
regFL2 <- ivreg(del_erateFL ~ del_ms + he_shr + fe_lfp + for_bor | manu_shr + he_shr + fe_lfp + for_bor, data = set2)

regMHW2 <- ivreg(del_wageMH ~ del_ms + he_shr + fe_lfp + for_bor | manu_shr + he_shr + fe_lfp + for_bor, data = set2)
regMLW2 <- ivreg(del_wageML ~ del_ms + he_shr + fe_lfp + for_bor | manu_shr + he_shr + fe_lfp + for_bor, data = set2)
regFHW2 <- ivreg(del_wageFH ~ del_ms + he_shr + fe_lfp + for_bor | manu_shr + he_shr + fe_lfp + for_bor, data = set2)
regFLW2 <- ivreg(del_wageFL ~ del_ms + he_shr + fe_lfp + for_bor | manu_shr + he_shr + fe_lfp + for_bor, data = set2)

stargazer(regMH2, regML2, regFH2, regFL2, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Morissette-like Regression by Demographic Group -- Employment", type = "text")
stargazer(regMHW2, regMLW2, regFHW2, regFLW2, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Morissette-like Regression by Demographic Group -- Wages", type = "text")

coeftest(regMH2, vcov = vcovHC, type = "HC0")
coeftest(regML2, vcov = vcovHC, type = "HC0")
coeftest(regFH2, vcov = vcovHC, type = "HC0")
coeftest(regFL2, vcov = vcovHC, type = "HC0")

coeftest(regMHW2, vcov = vcovHC, type = "HC0")
coeftest(regMLW2, vcov = vcovHC, type = "HC0")
coeftest(regFHW2, vcov = vcovHC, type = "HC0")
coeftest(regFLW2, vcov = vcovHC, type = "HC0")

### Main Regression: 2001 -- 2011

##Construct Dataset 3

#Independent variable

del_msMH0111 <- vector()
del_msML0111 <- vector()
del_msFH0111 <- vector()
del_msFL0111 <- vector()
del_ms0111 <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  del_msMH0111[j] <- subset(census, Year == 2011 & CMAList == i)$manu_shrMH - subset(census, Year == 2001 & CMAList == i)$manu_shrMH
  del_msML0111[j] <- subset(census, Year == 2011 & CMAList == i)$manu_shrML - subset(census, Year == 2001 & CMAList == i)$manu_shrML
  del_msFH0111[j] <- subset(census, Year == 2011 & CMAList == i)$manu_shrFH - subset(census, Year == 2001 & CMAList == i)$manu_shrFH
  del_msFL0111[j] <- subset(census, Year == 2011 & CMAList == i)$manu_shrFL - subset(census, Year == 2001 & CMAList == i)$manu_shrFL
  del_ms0111[j] <- subset(census, Year == 2011 & CMAList == i)$manu_shr - subset(census, Year == 2001 & CMAList == i)$manu_shr
}

#Dependent variable

del_erateMH0111 <- vector()
del_erateML0111 <- vector()
del_erateFH0111 <- vector()
del_erateFL0111 <- vector()
del_wageMH0111 <- vector()
del_wageML0111 <- vector()
del_wageFH0111 <- vector()
del_wageFL0111 <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  del_erateMH0111[j] <- subset(census, Year == 2011 & CMAList == i)$erateMH - subset(census, Year == 2001 & CMAList == i)$erateMH
  del_erateML0111[j] <- subset(census, Year == 2011 & CMAList == i)$erateML - subset(census, Year == 2001 & CMAList == i)$erateML
  del_erateFH0111[j] <- subset(census, Year == 2011 & CMAList == i)$erateFH - subset(census, Year == 2001 & CMAList == i)$erateFH
  del_erateFL0111[j] <- subset(census, Year == 2011 & CMAList == i)$erateFL - subset(census, Year == 2001 & CMAList == i)$erateFL
  del_wageMH0111[j] <- subset(census, Year == 2011 & CMAList == i)$logwageMH - subset(census, Year == 2001 & CMAList == i)$logwageMH
  del_wageML0111[j] <- subset(census, Year == 2011 & CMAList == i)$logwageML - subset(census, Year == 2001 & CMAList == i)$logwageML
  del_wageFH0111[j] <- subset(census, Year == 2011 & CMAList == i)$logwageFH - subset(census, Year == 2001 & CMAList == i)$logwageFH
  del_wageFL0111[j] <- subset(census, Year == 2011 & CMAList == i)$logwageFL - subset(census, Year == 2001 & CMAList == i)$logwageFL
}

#Bartik Components

shock1 <- vector()
shock2 <- vector()
shock3 <- vector()
shock4 <- vector()
shock5 <- vector()
shock6 <- vector()
shock7 <- vector()
shock8 <- vector()
shock9 <- vector()
shock10 <- vector()
shock11 <- vector()
shock12 <- vector()
shock13 <- vector()
shock14 <- vector()
shock15 <- vector()
shock16 <- vector()
shock17 <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  shock1[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm1 - subset(census, Year == 2001 & CMAList == i)$man_shrm1
  shock2[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm2 - subset(census, Year == 2001 & CMAList == i)$man_shrm2
  shock3[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm3- subset(census, Year == 2001 & CMAList == i)$man_shrm3
  shock4[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm4 - subset(census, Year == 2001 & CMAList == i)$man_shrm4
  shock5[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm5 - subset(census, Year == 2001 & CMAList == i)$man_shrm5
  shock6[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm6 - subset(census, Year == 2001 & CMAList == i)$man_shrm6
  shock7[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm7 - subset(census, Year == 2001 & CMAList == i)$man_shrm7
  shock8[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm8 - subset(census, Year == 2001 & CMAList == i)$man_shrm8
  shock9[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm9 - subset(census, Year == 2001 & CMAList == i)$man_shrm9
  shock10[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm10 - subset(census, Year == 2001 & CMAList == i)$man_shrm10
  shock11[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm11 - subset(census, Year == 2001 & CMAList == i)$man_shrm11
  shock12[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm12 - subset(census, Year == 2001 & CMAList == i)$man_shrm12
  shock13[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm13 - subset(census, Year == 2001 & CMAList == i)$man_shrm13
  shock14[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm14 - subset(census, Year == 2001 & CMAList == i)$man_shrm14
  shock15[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm15 - subset(census, Year == 2001 & CMAList == i)$man_shrm15
  shock16[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm16 - subset(census, Year == 2001 & CMAList == i)$man_shrm16
  shock17[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm17 - subset(census, Year == 2001 & CMAList == i)$man_shrm17
}

NOCshr1 <- subset(census, Year == 2001)$NOC_shr1
NOCshr2 <- subset(census, Year == 2001)$NOC_shr2
NOCshr3 <- subset(census, Year == 2001)$NOC_shr3
NOCshr4 <- subset(census, Year == 2001)$NOC_shr4
NOCshr5 <- subset(census, Year == 2001)$NOC_shr5
NOCshr6 <- subset(census, Year == 2001)$NOC_shr6
NOCshr7 <- subset(census, Year == 2001)$NOC_shr7
NOCshr8 <- subset(census, Year == 2001)$NOC_shr8
NOCshr9 <- subset(census, Year == 2001)$NOC_shr9
NOCshr10 <- subset(census, Year == 2001)$NOC_shr10
NOCshr11 <- subset(census, Year == 2001)$NOC_shr11
NOCshr12 <- subset(census, Year == 2001)$NOC_shr12
NOCshr13 <- subset(census, Year == 2001)$NOC_shr13
NOCshr14 <- subset(census, Year == 2001)$NOC_shr14
NOCshr15 <- subset(census, Year == 2001)$NOC_shr15
NOCshr16 <- subset(census, Year == 2001)$NOC_shr16
NOCshr17 <- subset(census, Year == 2001)$NOC_shr17

share_set <- data.frame(cbind(CMAList, NOCshr1, NOCshr2, NOCshr3, NOCshr4, NOCshr5, NOCshr6, NOCshr7, NOCshr8, NOCshr9, NOCshr10, NOCshr11, NOCshr12, NOCshr13, NOCshr14, NOCshr15, NOCshr16, NOCshr17))
share_set$CMAList <- as.factor(share_set$CMAList)
share_set <- gather(share_set, key = "noc", value = "share", NOCshr1:NOCshr17, factor_key = T)
share_set <- share_set[order(share_set$CMAList, share_set$noc),]
share_set %>%
  mutate(noc = str_glue("c{CMAList}n{noc}")) %>%
  spread(noc, share, fill = 0) %>%
  print() -> share_set2

colorder <- c("CMAList", "c9nNOCshr1", "c9nNOCshr2", "c9nNOCshr3", "c9nNOCshr4", "c9nNOCshr5", "c9nNOCshr6", "c9nNOCshr7", "c9nNOCshr8", "c9nNOCshr9", "c9nNOCshr10", "c9nNOCshr11", "c9nNOCshr12", "c9nNOCshr13", "c9nNOCshr14", "c9nNOCshr15", "c9nNOCshr16", "c9nNOCshr17", "c19nNOCshr1", "c19nNOCshr2", "c19nNOCshr3", "c19nNOCshr4", "c19nNOCshr5", "c19nNOCshr6", "c19nNOCshr7", "c19nNOCshr8", "c19nNOCshr9", "c19nNOCshr10", "c19nNOCshr11", "c19nNOCshr12", "c19nNOCshr13", "c19nNOCshr14", "c19nNOCshr15", "c19nNOCshr16", "c19nNOCshr17", 
              "c29nNOCshr1", "c29nNOCshr2", "c29nNOCshr3", "c29nNOCshr4", "c29nNOCshr5", "c29nNOCshr6", "c29nNOCshr7", "c29nNOCshr8", "c29nNOCshr9", "c29nNOCshr10", "c29nNOCshr11", "c29nNOCshr12", "c29nNOCshr13", "c29nNOCshr14", "c29nNOCshr15", "c29nNOCshr16", "c29nNOCshr17", "c39nNOCshr1", "c39nNOCshr2", "c39nNOCshr3", "c39nNOCshr4", "c39nNOCshr5", "c39nNOCshr6", "c39nNOCshr7", "c39nNOCshr8", "c39nNOCshr9", "c39nNOCshr10", "c39nNOCshr11", "c39nNOCshr12", "c39nNOCshr13", "c39nNOCshr14", "c39nNOCshr15", "c39nNOCshr16", "c39nNOCshr17",
              "c49nNOCshr1", "c49nNOCshr2", "c49nNOCshr3", "c49nNOCshr4", "c49nNOCshr5", "c49nNOCshr6", "c49nNOCshr7", "c49nNOCshr8", "c49nNOCshr9", "c49nNOCshr10", "c49nNOCshr11", "c49nNOCshr12", "c49nNOCshr13", "c49nNOCshr14", "c49nNOCshr15", "c49nNOCshr16", "c49nNOCshr17", "c59nNOCshr1", "c59nNOCshr2", "c59nNOCshr3", "c59nNOCshr4", "c59nNOCshr5", "c59nNOCshr6", "c59nNOCshr7", "c59nNOCshr8", "c59nNOCshr9", "c59nNOCshr10", "c59nNOCshr11", "c59nNOCshr12", "c59nNOCshr13", "c59nNOCshr14", "c59nNOCshr15", "c59nNOCshr16", "c59nNOCshr17", 
              "c69nNOCshr1", "c69nNOCshr2", "c69nNOCshr3", "c69nNOCshr4", "c69nNOCshr5", "c69nNOCshr6", "c69nNOCshr7", "c69nNOCshr8", "c69nNOCshr9", "c69nNOCshr10", "c69nNOCshr11", "c69nNOCshr12", "c69nNOCshr13", "c69nNOCshr14", "c69nNOCshr15", "c69nNOCshr16", "c69nNOCshr17", "c79nNOCshr1", "c79nNOCshr2", "c79nNOCshr3", "c79nNOCshr4", "c79nNOCshr5", "c79nNOCshr6", "c79nNOCshr7", "c79nNOCshr8", "c79nNOCshr9", "c79nNOCshr10", "c79nNOCshr11", "c79nNOCshr12", "c79nNOCshr13", "c79nNOCshr14", "c79nNOCshr15", "c79nNOCshr16", "c79nNOCshr17", 
              "c89nNOCshr1", "c89nNOCshr2", "c89nNOCshr3", "c89nNOCshr4", "c89nNOCshr5", "c89nNOCshr6", "c89nNOCshr7", "c89nNOCshr8", "c89nNOCshr9", "c89nNOCshr10", "c89nNOCshr11", "c89nNOCshr12", "c89nNOCshr13", "c89nNOCshr14", "c89nNOCshr15", "c89nNOCshr16", "c89nNOCshr17", "c99nNOCshr1", "c99nNOCshr2", "c99nNOCshr3", "c99nNOCshr4", "c99nNOCshr5", "c99nNOCshr6", "c99nNOCshr7", "c99nNOCshr8", "c99nNOCshr9", "c99nNOCshr10", "c99nNOCshr11", "c99nNOCshr12", "c99nNOCshr13", "c99nNOCshr14", "c99nNOCshr15", "c99nNOCshr16", "c99nNOCshr17", 
              "c205nNOCshr1", "c205nNOCshr2", "c205nNOCshr3", "c205nNOCshr4", "c205nNOCshr5", "c205nNOCshr6", "c205nNOCshr7", "c205nNOCshr8", "c205nNOCshr9", "c205nNOCshr10", "c205nNOCshr11", "c205nNOCshr12", "c205nNOCshr13", "c205nNOCshr14", "c205nNOCshr15", "c205nNOCshr16", "c205nNOCshr17", "c421nNOCshr1", "c421nNOCshr2", "c421nNOCshr3", "c421nNOCshr4", "c421nNOCshr5", "c421nNOCshr6", "c421nNOCshr7", "c421nNOCshr8", "c421nNOCshr9", "c421nNOCshr10", "c421nNOCshr11", "c421nNOCshr12", "c421nNOCshr13", "c421nNOCshr14", "c421nNOCshr15", "c421nNOCshr16", "c421nNOCshr17",
              "c462nNOCshr1", "c462nNOCshr2", "c462nNOCshr3", "c462nNOCshr4", "c462nNOCshr5", "c462nNOCshr6", "c462nNOCshr7", "c462nNOCshr8", "c462nNOCshr9", "c462nNOCshr10", "c462nNOCshr11", "c462nNOCshr12", "c462nNOCshr13", "c462nNOCshr14", "c462nNOCshr15", "c462nNOCshr16", "c462nNOCshr17", "c499nNOCshr1", "c499nNOCshr2", "c499nNOCshr3", "c499nNOCshr4", "c499nNOCshr5", "c499nNOCshr6", "c499nNOCshr7", "c499nNOCshr8", "c499nNOCshr9", "c499nNOCshr10", "c499nNOCshr11", "c499nNOCshr12", "c499nNOCshr13", "c499nNOCshr14", "c499nNOCshr15", "c499nNOCshr16", "c499nNOCshr17",
              "c505nNOCshr1", "c505nNOCshr2", "c505nNOCshr3", "c505nNOCshr4", "c505nNOCshr5", "c505nNOCshr6", "c505nNOCshr7", "c505nNOCshr8", "c505nNOCshr9", "c505nNOCshr10", "c505nNOCshr11", "c505nNOCshr12", "c505nNOCshr13", "c505nNOCshr14", "c505nNOCshr15", "c505nNOCshr16", "c505nNOCshr17", "c532nNOCshr1", "c532nNOCshr2", "c532nNOCshr3", "c532nNOCshr4", "c532nNOCshr5", "c532nNOCshr6", "c532nNOCshr7", "c532nNOCshr8", "c532nNOCshr9", "c532nNOCshr10", "c532nNOCshr11", "c532nNOCshr12", "c532nNOCshr13", "c532nNOCshr14", "c532nNOCshr15", "c532nNOCshr16", "c532nNOCshr17",
              "c535nNOCshr1", "c535nNOCshr2", "c535nNOCshr3", "c535nNOCshr4", "c535nNOCshr5", "c535nNOCshr6", "c535nNOCshr7", "c535nNOCshr8", "c535nNOCshr9", "c535nNOCshr10", "c535nNOCshr11", "c535nNOCshr12", "c535nNOCshr13", "c535nNOCshr14", "c535nNOCshr15", "c535nNOCshr16", "c535nNOCshr17", "c537nNOCshr1", "c537nNOCshr2", "c537nNOCshr3", "c537nNOCshr4", "c537nNOCshr5", "c537nNOCshr6", "c537nNOCshr7", "c537nNOCshr8", "c537nNOCshr9", "c537nNOCshr10", "c537nNOCshr11", "c537nNOCshr12", "c537nNOCshr13", "c537nNOCshr14", "c537nNOCshr15", "c537nNOCshr16", "c537nNOCshr17",
              "c539nNOCshr1", "c539nNOCshr2", "c539nNOCshr3", "c539nNOCshr4", "c539nNOCshr5", "c539nNOCshr6", "c539nNOCshr7", "c539nNOCshr8", "c539nNOCshr9", "c539nNOCshr10", "c539nNOCshr11", "c539nNOCshr12", "c539nNOCshr13", "c539nNOCshr14", "c539nNOCshr15", "c539nNOCshr16", "c539nNOCshr17", "c541nNOCshr1", "c541nNOCshr2", "c541nNOCshr3", "c541nNOCshr4", "c541nNOCshr5", "c541nNOCshr6", "c541nNOCshr7", "c541nNOCshr8", "c541nNOCshr9", "c541nNOCshr10", "c541nNOCshr11", "c541nNOCshr12", "c541nNOCshr13", "c541nNOCshr14", "c541nNOCshr15", "c541nNOCshr16", "c541nNOCshr17", 
              "c555nNOCshr1", "c555nNOCshr2", "c555nNOCshr3", "c555nNOCshr4", "c555nNOCshr5", "c555nNOCshr6", "c555nNOCshr7", "c555nNOCshr8", "c555nNOCshr9", "c555nNOCshr10", "c555nNOCshr11", "c555nNOCshr12", "c555nNOCshr13", "c555nNOCshr14", "c555nNOCshr15", "c555nNOCshr16", "c555nNOCshr17", "c559nNOCshr1", "c559nNOCshr2", "c559nNOCshr3", "c559nNOCshr4", "c559nNOCshr5", "c559nNOCshr6", "c559nNOCshr7", "c559nNOCshr8", "c559nNOCshr9", "c559nNOCshr10", "c559nNOCshr11", "c559nNOCshr12", "c559nNOCshr13", "c559nNOCshr14", "c559nNOCshr15", "c559nNOCshr16", "c559nNOCshr17", 
              "c602nNOCshr1", "c602nNOCshr2", "c602nNOCshr3", "c602nNOCshr4", "c602nNOCshr5", "c602nNOCshr6", "c602nNOCshr7", "c602nNOCshr8", "c602nNOCshr9", "c602nNOCshr10", "c602nNOCshr11", "c602nNOCshr12", "c602nNOCshr13", "c602nNOCshr14", "c602nNOCshr15", "c602nNOCshr16", "c602nNOCshr17", "c799nNOCshr1", "c799nNOCshr2", "c799nNOCshr3", "c799nNOCshr4", "c799nNOCshr5", "c799nNOCshr6", "c799nNOCshr7", "c799nNOCshr8", "c799nNOCshr9", "c799nNOCshr10", "c799nNOCshr11", "c799nNOCshr12", "c799nNOCshr13", "c799nNOCshr14", "c799nNOCshr15", "c799nNOCshr16", "c799nNOCshr17", 
              "c825nNOCshr1", "c825nNOCshr2", "c825nNOCshr3", "c825nNOCshr4", "c825nNOCshr5", "c825nNOCshr6", "c825nNOCshr7", "c825nNOCshr8", "c825nNOCshr9", "c825nNOCshr10", "c825nNOCshr11", "c825nNOCshr12", "c825nNOCshr13", "c825nNOCshr14", "c825nNOCshr15", "c825nNOCshr16", "c825nNOCshr17", "c835nNOCshr1", "c835nNOCshr2", "c835nNOCshr3", "c835nNOCshr4", "c835nNOCshr5", "c835nNOCshr6", "c835nNOCshr7", "c835nNOCshr8", "c835nNOCshr9", "c835nNOCshr10", "c835nNOCshr11", "c835nNOCshr12", "c835nNOCshr13", "c835nNOCshr14", "c835nNOCshr15", "c835nNOCshr16", "c835nNOCshr17",
              "c933nNOCshr1", "c933nNOCshr2", "c933nNOCshr3", "c933nNOCshr4", "c933nNOCshr5", "c933nNOCshr6", "c933nNOCshr7", "c933nNOCshr8", "c933nNOCshr9", "c933nNOCshr10", "c933nNOCshr11", "c933nNOCshr12", "c933nNOCshr13", "c933nNOCshr14", "c933nNOCshr15", "c933nNOCshr16", "c933nNOCshr17", "c935nNOCshr1", "c935nNOCshr2", "c935nNOCshr3", "c935nNOCshr4", "c935nNOCshr5", "c935nNOCshr6", "c935nNOCshr7", "c935nNOCshr8", "c935nNOCshr9", "c935nNOCshr10", "c935nNOCshr11", "c935nNOCshr12", "c935nNOCshr13", "c935nNOCshr14", "c935nNOCshr15", "c935nNOCshr16", "c935nNOCshr17")

share_set2 <- share_set2[, colorder]

shock_set <- data.frame(cbind(CMAList, shock1, shock2, shock3, shock4, shock5, shock6, shock7, shock8, shock9, shock10, shock11, shock12, shock13, shock14, shock15, shock16, shock17))
shock_set$CMAList <- as.factor(shock_set$CMAList)
shock_set <- gather(shock_set, key = "noc", value = "shock", shock1:shock17, factor_key = T)
shock_set <- shock_set[order(shock_set$CMAList, shock_set$noc),]

set3 <- data.frame(cbind(CMAList, he_shr, fe_lfp, for_bor, del_msMH0111, del_msML0111, del_msFH0111, del_msFL0111, del_ms0111, del_erateMH0111, del_erateML0111, del_erateFH0111, del_erateFL0111, del_wageMH0111, del_wageML0111, del_wageFH0111, del_wageFL0111, shock1, shock2, shock3, shock4, shock5, shock6, shock7, shock8, shock9, shock10, shock11, shock12, shock13, shock14, shock15, shock16, shock17, NOCshr1, NOCshr2, NOCshr3, NOCshr4, NOCshr5, NOCshr6, NOCshr7, NOCshr8, NOCshr9, NOCshr10, NOCshr11, NOCshr12, NOCshr13, NOCshr14, NOCshr15, NOCshr16, NOCshr17))
set3 <- set3 %>%
  mutate(bart = NOCshr1*shock1 + NOCshr2*shock2 + NOCshr3*shock3 + NOCshr4*shock4 + NOCshr5*shock5 + NOCshr6*shock6 + NOCshr7*shock7 + NOCshr8*shock8 + NOCshr9*shock9 + NOCshr10*shock10 + NOCshr11*shock11 + NOCshr12*shock12 + NOCshr13*shock13 + NOCshr14*shock14 + NOCshr15*shock15 + NOCshr16*shock16 + NOCshr17*shock17)


# IV Regressions
regMH0111 <- ivreg(del_erateMH0111 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regML0111 <- ivreg(del_erateML0111 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regFH0111 <- ivreg(del_erateFH0111 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regFL0111 <- ivreg(del_erateFL0111 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)

regMHW0111 <- ivreg(del_wageMH0111 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regMLW0111 <- ivreg(del_wageML0111 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regFHW0111 <- ivreg(del_wageFH0111 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regFLW0111 <- ivreg(del_wageFL0111 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)

stargazer(regMH0111, regML0111, regFH0111, regFL0111, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Employment (2001-2011)", type = "text")
stargazer(regMHW0111, regMLW0111, regFHW0111, regFLW0111, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Wages (2001-2011)", type = "text")

coeftest(regMH0111, vcov = vcovHC, type = "HC0")
coeftest(regML0111, vcov = vcovHC, type = "HC0")
coeftest(regFH0111, vcov = vcovHC, type = "HC0")
coeftest(regFL0111, vcov = vcovHC, type = "HC0")

coeftest(regMHW0111, vcov = vcovHC, type = "HC0")
coeftest(regMLW0111, vcov = vcovHC, type = "HC0")
coeftest(regFHW0111, vcov = vcovHC, type = "HC0")
coeftest(regFLW0111, vcov = vcovHC, type = "HC0")

#TABLE
stargazer(regMH0111, regMHW0111, regML0111, regMLW0111, regFH0111, regFHW0111, regFL0111, regFLW0111, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Effect of Changing Manufacturing Share on Labour Outcomes by Demographic Group (2001-2011)")

# Rotemberg weights

y = "del_erateML0111"
x = "del_ms0111"
controls = c("he_shr", "fe_lfp", "for_bor")
weight = "bart"
Z = setdiff(names(share_set2), "CMAList")
G = "shock"

rotemberg0111 <- bw(set3, y, x, controls, weight, share_set2, Z, shock_set, G)

#Take median rotemberg weights over all CMAs for each occupation group
rotembergE0111 <- aggregate(rotemberg0111[, 4], list(rotemberg0111$noc), mean) %>%
  arrange(desc(alpha)) %>%
  select(Group.1, alpha)

### Main Regression: 2011 -- 2016

##Construct Dataset 4

#Controls

he_shr <- subset(census, Year == 2011)$he_shr
fe_lfp <- subset(census, Year == 2011)$fe_lfs
for_bor <- subset(census, Year == 2011)$for_bor

#Independent variable

del_msMH1116 <- vector()
del_msML1116 <- vector()
del_msFH1116 <- vector()
del_msFL1116 <- vector()
del_ms1116 <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  del_msMH1116[j] <- subset(census, Year == 2016 & CMAList == i)$manu_shrMH - subset(census, Year == 2011 & CMAList == i)$manu_shrMH
  del_msML1116[j] <- subset(census, Year == 2016 & CMAList == i)$manu_shrML - subset(census, Year == 2011 & CMAList == i)$manu_shrML
  del_msFH1116[j] <- subset(census, Year == 2016 & CMAList == i)$manu_shrFH - subset(census, Year == 2011 & CMAList == i)$manu_shrFH
  del_msFL1116[j] <- subset(census, Year == 2016 & CMAList == i)$manu_shrFL - subset(census, Year == 2011 & CMAList == i)$manu_shrFL
  del_ms1116[j] <- subset(census, Year == 2016 & CMAList == i)$manu_shr - subset(census, Year == 2011 & CMAList == i)$manu_shr
}

#Dependent variable

del_erateMH1116 <- vector()
del_erateML1116 <- vector()
del_erateFH1116 <- vector()
del_erateFL1116 <- vector()
del_wageMH1116 <- vector()
del_wageML1116 <- vector()
del_wageFH1116 <- vector()
del_wageFL1116 <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  del_erateMH1116[j] <- subset(census, Year == 2016 & CMAList == i)$erateMH - subset(census, Year == 2011 & CMAList == i)$erateMH
  del_erateML1116[j] <- subset(census, Year == 2016 & CMAList == i)$erateML - subset(census, Year == 2011 & CMAList == i)$erateML
  del_erateFH1116[j] <- subset(census, Year == 2016 & CMAList == i)$erateFH - subset(census, Year == 2011 & CMAList == i)$erateFH
  del_erateFL1116[j] <- subset(census, Year == 2016 & CMAList == i)$erateFL - subset(census, Year == 2011 & CMAList == i)$erateFL
  del_wageMH1116[j] <- subset(census, Year == 2016 & CMAList == i)$logwageMH - subset(census, Year == 2011 & CMAList == i)$logwageMH
  del_wageML1116[j] <- subset(census, Year == 2016 & CMAList == i)$logwageML - subset(census, Year == 2011 & CMAList == i)$logwageML
  del_wageFH1116[j] <- subset(census, Year == 2016 & CMAList == i)$logwageFH - subset(census, Year == 2011 & CMAList == i)$logwageFH
  del_wageFL1116[j] <- subset(census, Year == 2016 & CMAList == i)$logwageFL - subset(census, Year == 2011 & CMAList == i)$logwageFL
}

#Bartik Components

shock1 <- vector()
shock2 <- vector()
shock3 <- vector()
shock4 <- vector()
shock5 <- vector()
shock6 <- vector()
shock7 <- vector()
shock8 <- vector()
shock9 <- vector()
shock10 <- vector()
shock11 <- vector()
shock12 <- vector()
shock13 <- vector()
shock14 <- vector()
shock15 <- vector()
shock16 <- vector()
shock17 <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  shock1[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm1 - subset(census, Year == 2011 & CMAList == i)$man_shrm1
  shock2[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm2 - subset(census, Year == 2011 & CMAList == i)$man_shrm2
  shock3[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm3- subset(census, Year == 2011 & CMAList == i)$man_shrm3
  shock4[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm4 - subset(census, Year == 2011 & CMAList == i)$man_shrm4
  shock5[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm5 - subset(census, Year == 2011 & CMAList == i)$man_shrm5
  shock6[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm6 - subset(census, Year == 2011 & CMAList == i)$man_shrm6
  shock7[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm7 - subset(census, Year == 2011 & CMAList == i)$man_shrm7
  shock8[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm8 - subset(census, Year == 2011 & CMAList == i)$man_shrm8
  shock9[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm9 - subset(census, Year == 2011 & CMAList == i)$man_shrm9
  shock10[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm10 - subset(census, Year == 2011 & CMAList == i)$man_shrm10
  shock11[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm11 - subset(census, Year == 2011 & CMAList == i)$man_shrm11
  shock12[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm12 - subset(census, Year == 2011 & CMAList == i)$man_shrm12
  shock13[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm13 - subset(census, Year == 2011 & CMAList == i)$man_shrm13
  shock14[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm14 - subset(census, Year == 2011 & CMAList == i)$man_shrm14
  shock15[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm15 - subset(census, Year == 2011 & CMAList == i)$man_shrm15
  shock16[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm16 - subset(census, Year == 2011 & CMAList == i)$man_shrm16
  shock17[j] <- subset(census, Year == 2016 & CMAList == i)$man_shrm17 - subset(census, Year == 2011 & CMAList == i)$man_shrm17
}

NOCshr1 <- subset(census, Year == 2011)$NOC_shr1
NOCshr2 <- subset(census, Year == 2011)$NOC_shr2
NOCshr3 <- subset(census, Year == 2011)$NOC_shr3
NOCshr4 <- subset(census, Year == 2011)$NOC_shr4
NOCshr5 <- subset(census, Year == 2011)$NOC_shr5
NOCshr6 <- subset(census, Year == 2011)$NOC_shr6
NOCshr7 <- subset(census, Year == 2011)$NOC_shr7
NOCshr8 <- subset(census, Year == 2011)$NOC_shr8
NOCshr9 <- subset(census, Year == 2011)$NOC_shr9
NOCshr10 <- subset(census, Year == 2011)$NOC_shr10
NOCshr11 <- subset(census, Year == 2011)$NOC_shr11
NOCshr12 <- subset(census, Year == 2011)$NOC_shr12
NOCshr13 <- subset(census, Year == 2011)$NOC_shr13
NOCshr14 <- subset(census, Year == 2011)$NOC_shr14
NOCshr15 <- subset(census, Year == 2011)$NOC_shr15
NOCshr16 <- subset(census, Year == 2011)$NOC_shr16
NOCshr17 <- subset(census, Year == 2011)$NOC_shr17

share_set <- data.frame(cbind(CMAList, NOCshr1, NOCshr2, NOCshr3, NOCshr4, NOCshr5, NOCshr6, NOCshr7, NOCshr8, NOCshr9, NOCshr10, NOCshr11, NOCshr12, NOCshr13, NOCshr14, NOCshr15, NOCshr16, NOCshr17))
share_set$CMAList <- as.factor(share_set$CMAList)
share_set <- gather(share_set, key = "noc", value = "share", NOCshr1:NOCshr17, factor_key = T)
share_set <- share_set[order(share_set$CMAList, share_set$noc),]
share_set %>%
  mutate(noc = str_glue("c{CMAList}n{noc}")) %>%
  spread(noc, share, fill = 0) %>%
  print() -> share_set2

colorder <- c("CMAList", "c9nNOCshr1", "c9nNOCshr2", "c9nNOCshr3", "c9nNOCshr4", "c9nNOCshr5", "c9nNOCshr6", "c9nNOCshr7", "c9nNOCshr8", "c9nNOCshr9", "c9nNOCshr10", "c9nNOCshr11", "c9nNOCshr12", "c9nNOCshr13", "c9nNOCshr14", "c9nNOCshr15", "c9nNOCshr16", "c9nNOCshr17", "c19nNOCshr1", "c19nNOCshr2", "c19nNOCshr3", "c19nNOCshr4", "c19nNOCshr5", "c19nNOCshr6", "c19nNOCshr7", "c19nNOCshr8", "c19nNOCshr9", "c19nNOCshr10", "c19nNOCshr11", "c19nNOCshr12", "c19nNOCshr13", "c19nNOCshr14", "c19nNOCshr15", "c19nNOCshr16", "c19nNOCshr17", 
              "c29nNOCshr1", "c29nNOCshr2", "c29nNOCshr3", "c29nNOCshr4", "c29nNOCshr5", "c29nNOCshr6", "c29nNOCshr7", "c29nNOCshr8", "c29nNOCshr9", "c29nNOCshr10", "c29nNOCshr11", "c29nNOCshr12", "c29nNOCshr13", "c29nNOCshr14", "c29nNOCshr15", "c29nNOCshr16", "c29nNOCshr17", "c39nNOCshr1", "c39nNOCshr2", "c39nNOCshr3", "c39nNOCshr4", "c39nNOCshr5", "c39nNOCshr6", "c39nNOCshr7", "c39nNOCshr8", "c39nNOCshr9", "c39nNOCshr10", "c39nNOCshr11", "c39nNOCshr12", "c39nNOCshr13", "c39nNOCshr14", "c39nNOCshr15", "c39nNOCshr16", "c39nNOCshr17",
              "c49nNOCshr1", "c49nNOCshr2", "c49nNOCshr3", "c49nNOCshr4", "c49nNOCshr5", "c49nNOCshr6", "c49nNOCshr7", "c49nNOCshr8", "c49nNOCshr9", "c49nNOCshr10", "c49nNOCshr11", "c49nNOCshr12", "c49nNOCshr13", "c49nNOCshr14", "c49nNOCshr15", "c49nNOCshr16", "c49nNOCshr17", "c59nNOCshr1", "c59nNOCshr2", "c59nNOCshr3", "c59nNOCshr4", "c59nNOCshr5", "c59nNOCshr6", "c59nNOCshr7", "c59nNOCshr8", "c59nNOCshr9", "c59nNOCshr10", "c59nNOCshr11", "c59nNOCshr12", "c59nNOCshr13", "c59nNOCshr14", "c59nNOCshr15", "c59nNOCshr16", "c59nNOCshr17", 
              "c69nNOCshr1", "c69nNOCshr2", "c69nNOCshr3", "c69nNOCshr4", "c69nNOCshr5", "c69nNOCshr6", "c69nNOCshr7", "c69nNOCshr8", "c69nNOCshr9", "c69nNOCshr10", "c69nNOCshr11", "c69nNOCshr12", "c69nNOCshr13", "c69nNOCshr14", "c69nNOCshr15", "c69nNOCshr16", "c69nNOCshr17", "c79nNOCshr1", "c79nNOCshr2", "c79nNOCshr3", "c79nNOCshr4", "c79nNOCshr5", "c79nNOCshr6", "c79nNOCshr7", "c79nNOCshr8", "c79nNOCshr9", "c79nNOCshr10", "c79nNOCshr11", "c79nNOCshr12", "c79nNOCshr13", "c79nNOCshr14", "c79nNOCshr15", "c79nNOCshr16", "c79nNOCshr17", 
              "c89nNOCshr1", "c89nNOCshr2", "c89nNOCshr3", "c89nNOCshr4", "c89nNOCshr5", "c89nNOCshr6", "c89nNOCshr7", "c89nNOCshr8", "c89nNOCshr9", "c89nNOCshr10", "c89nNOCshr11", "c89nNOCshr12", "c89nNOCshr13", "c89nNOCshr14", "c89nNOCshr15", "c89nNOCshr16", "c89nNOCshr17", "c99nNOCshr1", "c99nNOCshr2", "c99nNOCshr3", "c99nNOCshr4", "c99nNOCshr5", "c99nNOCshr6", "c99nNOCshr7", "c99nNOCshr8", "c99nNOCshr9", "c99nNOCshr10", "c99nNOCshr11", "c99nNOCshr12", "c99nNOCshr13", "c99nNOCshr14", "c99nNOCshr15", "c99nNOCshr16", "c99nNOCshr17", 
              "c205nNOCshr1", "c205nNOCshr2", "c205nNOCshr3", "c205nNOCshr4", "c205nNOCshr5", "c205nNOCshr6", "c205nNOCshr7", "c205nNOCshr8", "c205nNOCshr9", "c205nNOCshr10", "c205nNOCshr11", "c205nNOCshr12", "c205nNOCshr13", "c205nNOCshr14", "c205nNOCshr15", "c205nNOCshr16", "c205nNOCshr17", "c421nNOCshr1", "c421nNOCshr2", "c421nNOCshr3", "c421nNOCshr4", "c421nNOCshr5", "c421nNOCshr6", "c421nNOCshr7", "c421nNOCshr8", "c421nNOCshr9", "c421nNOCshr10", "c421nNOCshr11", "c421nNOCshr12", "c421nNOCshr13", "c421nNOCshr14", "c421nNOCshr15", "c421nNOCshr16", "c421nNOCshr17",
              "c462nNOCshr1", "c462nNOCshr2", "c462nNOCshr3", "c462nNOCshr4", "c462nNOCshr5", "c462nNOCshr6", "c462nNOCshr7", "c462nNOCshr8", "c462nNOCshr9", "c462nNOCshr10", "c462nNOCshr11", "c462nNOCshr12", "c462nNOCshr13", "c462nNOCshr14", "c462nNOCshr15", "c462nNOCshr16", "c462nNOCshr17", "c499nNOCshr1", "c499nNOCshr2", "c499nNOCshr3", "c499nNOCshr4", "c499nNOCshr5", "c499nNOCshr6", "c499nNOCshr7", "c499nNOCshr8", "c499nNOCshr9", "c499nNOCshr10", "c499nNOCshr11", "c499nNOCshr12", "c499nNOCshr13", "c499nNOCshr14", "c499nNOCshr15", "c499nNOCshr16", "c499nNOCshr17",
              "c505nNOCshr1", "c505nNOCshr2", "c505nNOCshr3", "c505nNOCshr4", "c505nNOCshr5", "c505nNOCshr6", "c505nNOCshr7", "c505nNOCshr8", "c505nNOCshr9", "c505nNOCshr10", "c505nNOCshr11", "c505nNOCshr12", "c505nNOCshr13", "c505nNOCshr14", "c505nNOCshr15", "c505nNOCshr16", "c505nNOCshr17", "c532nNOCshr1", "c532nNOCshr2", "c532nNOCshr3", "c532nNOCshr4", "c532nNOCshr5", "c532nNOCshr6", "c532nNOCshr7", "c532nNOCshr8", "c532nNOCshr9", "c532nNOCshr10", "c532nNOCshr11", "c532nNOCshr12", "c532nNOCshr13", "c532nNOCshr14", "c532nNOCshr15", "c532nNOCshr16", "c532nNOCshr17",
              "c535nNOCshr1", "c535nNOCshr2", "c535nNOCshr3", "c535nNOCshr4", "c535nNOCshr5", "c535nNOCshr6", "c535nNOCshr7", "c535nNOCshr8", "c535nNOCshr9", "c535nNOCshr10", "c535nNOCshr11", "c535nNOCshr12", "c535nNOCshr13", "c535nNOCshr14", "c535nNOCshr15", "c535nNOCshr16", "c535nNOCshr17", "c537nNOCshr1", "c537nNOCshr2", "c537nNOCshr3", "c537nNOCshr4", "c537nNOCshr5", "c537nNOCshr6", "c537nNOCshr7", "c537nNOCshr8", "c537nNOCshr9", "c537nNOCshr10", "c537nNOCshr11", "c537nNOCshr12", "c537nNOCshr13", "c537nNOCshr14", "c537nNOCshr15", "c537nNOCshr16", "c537nNOCshr17",
              "c539nNOCshr1", "c539nNOCshr2", "c539nNOCshr3", "c539nNOCshr4", "c539nNOCshr5", "c539nNOCshr6", "c539nNOCshr7", "c539nNOCshr8", "c539nNOCshr9", "c539nNOCshr10", "c539nNOCshr11", "c539nNOCshr12", "c539nNOCshr13", "c539nNOCshr14", "c539nNOCshr15", "c539nNOCshr16", "c539nNOCshr17", "c541nNOCshr1", "c541nNOCshr2", "c541nNOCshr3", "c541nNOCshr4", "c541nNOCshr5", "c541nNOCshr6", "c541nNOCshr7", "c541nNOCshr8", "c541nNOCshr9", "c541nNOCshr10", "c541nNOCshr11", "c541nNOCshr12", "c541nNOCshr13", "c541nNOCshr14", "c541nNOCshr15", "c541nNOCshr16", "c541nNOCshr17", 
              "c555nNOCshr1", "c555nNOCshr2", "c555nNOCshr3", "c555nNOCshr4", "c555nNOCshr5", "c555nNOCshr6", "c555nNOCshr7", "c555nNOCshr8", "c555nNOCshr9", "c555nNOCshr10", "c555nNOCshr11", "c555nNOCshr12", "c555nNOCshr13", "c555nNOCshr14", "c555nNOCshr15", "c555nNOCshr16", "c555nNOCshr17", "c559nNOCshr1", "c559nNOCshr2", "c559nNOCshr3", "c559nNOCshr4", "c559nNOCshr5", "c559nNOCshr6", "c559nNOCshr7", "c559nNOCshr8", "c559nNOCshr9", "c559nNOCshr10", "c559nNOCshr11", "c559nNOCshr12", "c559nNOCshr13", "c559nNOCshr14", "c559nNOCshr15", "c559nNOCshr16", "c559nNOCshr17", 
              "c602nNOCshr1", "c602nNOCshr2", "c602nNOCshr3", "c602nNOCshr4", "c602nNOCshr5", "c602nNOCshr6", "c602nNOCshr7", "c602nNOCshr8", "c602nNOCshr9", "c602nNOCshr10", "c602nNOCshr11", "c602nNOCshr12", "c602nNOCshr13", "c602nNOCshr14", "c602nNOCshr15", "c602nNOCshr16", "c602nNOCshr17", "c799nNOCshr1", "c799nNOCshr2", "c799nNOCshr3", "c799nNOCshr4", "c799nNOCshr5", "c799nNOCshr6", "c799nNOCshr7", "c799nNOCshr8", "c799nNOCshr9", "c799nNOCshr10", "c799nNOCshr11", "c799nNOCshr12", "c799nNOCshr13", "c799nNOCshr14", "c799nNOCshr15", "c799nNOCshr16", "c799nNOCshr17", 
              "c825nNOCshr1", "c825nNOCshr2", "c825nNOCshr3", "c825nNOCshr4", "c825nNOCshr5", "c825nNOCshr6", "c825nNOCshr7", "c825nNOCshr8", "c825nNOCshr9", "c825nNOCshr10", "c825nNOCshr11", "c825nNOCshr12", "c825nNOCshr13", "c825nNOCshr14", "c825nNOCshr15", "c825nNOCshr16", "c825nNOCshr17", "c835nNOCshr1", "c835nNOCshr2", "c835nNOCshr3", "c835nNOCshr4", "c835nNOCshr5", "c835nNOCshr6", "c835nNOCshr7", "c835nNOCshr8", "c835nNOCshr9", "c835nNOCshr10", "c835nNOCshr11", "c835nNOCshr12", "c835nNOCshr13", "c835nNOCshr14", "c835nNOCshr15", "c835nNOCshr16", "c835nNOCshr17",
              "c933nNOCshr1", "c933nNOCshr2", "c933nNOCshr3", "c933nNOCshr4", "c933nNOCshr5", "c933nNOCshr6", "c933nNOCshr7", "c933nNOCshr8", "c933nNOCshr9", "c933nNOCshr10", "c933nNOCshr11", "c933nNOCshr12", "c933nNOCshr13", "c933nNOCshr14", "c933nNOCshr15", "c933nNOCshr16", "c933nNOCshr17", "c935nNOCshr1", "c935nNOCshr2", "c935nNOCshr3", "c935nNOCshr4", "c935nNOCshr5", "c935nNOCshr6", "c935nNOCshr7", "c935nNOCshr8", "c935nNOCshr9", "c935nNOCshr10", "c935nNOCshr11", "c935nNOCshr12", "c935nNOCshr13", "c935nNOCshr14", "c935nNOCshr15", "c935nNOCshr16", "c935nNOCshr17")

share_set2 <- share_set2[, colorder]

shock_set <- data.frame(cbind(CMAList, shock1, shock2, shock3, shock4, shock5, shock6, shock7, shock8, shock9, shock10, shock11, shock12, shock13, shock14, shock15, shock16, shock17))
shock_set$CMAList <- as.factor(shock_set$CMAList)
shock_set <- gather(shock_set, key = "noc", value = "shock", shock1:shock17, factor_key = T)
shock_set <- shock_set[order(shock_set$CMAList, shock_set$noc),]

set4 <- data.frame(cbind(CMAList, he_shr, fe_lfp, for_bor, del_msMH1116, del_msML1116, del_msFH1116, del_msFL1116, del_ms1116, del_erateMH1116, del_erateML1116, del_erateFH1116, del_erateFL1116, del_wageMH1116, del_wageML1116, del_wageFH1116, del_wageFL1116, shock1, shock2, shock3, shock4, shock5, shock6, shock7, shock8, shock9, shock10, shock11, shock12, shock13, shock14, shock15, shock16, shock17, NOCshr1, NOCshr2, NOCshr3, NOCshr4, NOCshr5, NOCshr6, NOCshr7, NOCshr8, NOCshr9, NOCshr10, NOCshr11, NOCshr12, NOCshr13, NOCshr14, NOCshr15, NOCshr16, NOCshr17))
set4 <- set4 %>%
  mutate(bart = NOCshr1*shock1 + NOCshr2*shock2 + NOCshr3*shock3 + NOCshr4*shock4 + NOCshr5*shock5 + NOCshr6*shock6 + NOCshr7*shock7 + NOCshr8*shock8 + NOCshr9*shock9 + NOCshr10*shock10 + NOCshr11*shock11 + NOCshr12*shock12 + NOCshr13*shock13 + NOCshr14*shock14 + NOCshr15*shock15 + NOCshr16*shock16 + NOCshr17*shock17)


# IV Regressions
regMH1116 <- ivreg(del_erateMH1116 ~ del_ms1116 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set4)
regML1116 <- ivreg(del_erateML1116 ~ del_ms1116 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set4)
regFH1116 <- ivreg(del_erateFH1116 ~ del_ms1116 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set4)
regFL1116 <- ivreg(del_erateFL1116 ~ del_ms1116 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set4)

regMHW1116 <- ivreg(del_wageMH1116 ~ del_ms1116 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set4)
regMLW1116 <- ivreg(del_wageML1116 ~ del_ms1116 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set4)
regFHW1116 <- ivreg(del_wageFH1116 ~ del_ms1116 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set4)
regFLW1116 <- ivreg(del_wageFL1116 ~ del_ms1116 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set4)

stargazer(regMH1116, regML1116, regFH1116, regFL1116, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Employment (2011-2016)", type = "text")
stargazer(regMHW1116, regMLW1116, regFHW1116, regFLW1116, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Wages (2011-2016)", type = "text")

coeftest(regMH1116, vcov = vcovHC, type = "HC0")
coeftest(regML1116, vcov = vcovHC, type = "HC0")
coeftest(regFH1116, vcov = vcovHC, type = "HC0")
coeftest(regFL1116, vcov = vcovHC, type = "HC0")

coeftest(regMHW1116, vcov = vcovHC, type = "HC0")
coeftest(regMLW1116, vcov = vcovHC, type = "HC0")
coeftest(regFHW1116, vcov = vcovHC, type = "HC0")
coeftest(regFLW1116, vcov = vcovHC, type = "HC0")

#TABLE
stargazer(regMH1116, regMHW1116, regML1116, regMLW1116, regFH1116, regFHW1116, regFL1116, regFLW1116, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Effect of Changing Manufacturing Share on Labour Outcomes by Demographic Group (2011-2016)")


# Rotemberg weights

y = "del_erateML1116"
x = "del_ms1116"
controls = c("he_shr", "fe_lfp", "for_bor")
weight = "bart"
Z = setdiff(names(share_set2), "CMAList")
G = "shock"

rotemberg1116 <- bw(set4, y, x, controls, weight, share_set2, Z, shock_set, G)

#Take median rotemberg weights over all CMAs for each occupation group
rotembergE1116 <- aggregate(rotemberg1116[, 4], list(rotemberg1116$noc), median) %>%
  arrange(desc(alpha)) %>%
  select(Group.1, alpha)

### Mechanisms (2001-2016)

del_UIC2039M <- vector()
del_UIC2039F <- vector()
del_UIC4054M <- vector()
del_UIC4054F <- vector()
del_LFNPMH <- vector()
del_LFNPML <- vector()
del_LFNPFH <- vector()
del_LFNPFL <- vector()

del_UIC2039M2 <- vector()
del_UIC2039F2 <- vector()
del_UIC4054M2 <- vector()
del_UIC4054F2 <- vector()
del_LFNPMH2 <- vector()
del_LFNPML2 <- vector()
del_LFNPFH2 <- vector()
del_LFNPFL2 <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  del_UIC2039M[j] <- subset(census, Year == 2016 & CMAList == i)$UICrate2039M - subset(census, Year == 2001 & CMAList == i)$UICrate2039M
  del_UIC2039F[j] <- subset(census, Year == 2016 & CMAList == i)$UICrate2039F - subset(census, Year == 2001 & CMAList == i)$UICrate2039F
  del_UIC4054M[j] <- subset(census, Year == 2016 & CMAList == i)$UICrate4054M - subset(census, Year == 2001 & CMAList == i)$UICrate4054M
  del_UIC4054F[j] <- subset(census, Year == 2016 & CMAList == i)$UICrate4054F - subset(census, Year == 2001 & CMAList == i)$UICrate4054F
  del_LFNPMH[j] <- subset(census, Year == 2016 & CMAList == i)$LFNPMH - subset(census, Year == 2001 & CMAList == i)$LFNPMH
  del_LFNPML[j] <- subset(census, Year == 2016 & CMAList == i)$LFNPML - subset(census, Year == 2001 & CMAList == i)$LFNPML
  del_LFNPFH[j] <- subset(census, Year == 2016 & CMAList == i)$LFNPFH - subset(census, Year == 2001 & CMAList == i)$LFNPFH
  del_LFNPFL[j] <- subset(census, Year == 2016 & CMAList == i)$LFNPFL - subset(census, Year == 2001 & CMAList == i)$LFNPFL
  del_UIC2039M2[j] <- subset(census, Year == 2011 & CMAList == i)$UICrate2039M - subset(census, Year == 2001 & CMAList == i)$UICrate2039M
  del_UIC2039F2[j] <- subset(census, Year == 2011 & CMAList == i)$UICrate2039F - subset(census, Year == 2001 & CMAList == i)$UICrate2039F
  del_UIC4054M2[j] <- subset(census, Year == 2011 & CMAList == i)$UICrate4054M - subset(census, Year == 2001 & CMAList == i)$UICrate4054M
  del_UIC4054F2[j] <- subset(census, Year == 2011 & CMAList == i)$UICrate4054F - subset(census, Year == 2001 & CMAList == i)$UICrate4054F
  del_LFNPMH2[j] <- subset(census, Year == 2011 & CMAList == i)$LFNPMH - subset(census, Year == 2001 & CMAList == i)$LFNPMH
  del_LFNPML2[j] <- subset(census, Year == 2011 & CMAList == i)$LFNPML - subset(census, Year == 2001 & CMAList == i)$LFNPML
  del_LFNPFH2[j] <- subset(census, Year == 2011 & CMAList == i)$LFNPFH - subset(census, Year == 2001 & CMAList == i)$LFNPFH
  del_LFNPFL2[j] <- subset(census, Year == 2011 & CMAList == i)$LFNPFL - subset(census, Year == 2001 & CMAList == i)$LFNPFL
}

#Population

pop01 <- log(c(522046, 136665, 563242, 749820, 2737530, 3277611, 455624, 569646, 1117943, 1676650, 369252, 705206, 327021, 3626699, 1127684, 308611, 4882618, 689178, 391844, 446937, 452101, 320950, 695830, 430593, 977775, 962390, 2074568, 325732))
pop16 <- log(c(529426, 146969, 528775, 763350, 2910493, 3436736, 512962, 583392, 1393507, 1892416, 414015, 804875, 370157, 4140425, 1348100, 391942, 6125013, 768982, 416384, 542034, 509009, 337194, 801177, 552595, 1438160, 1364394, 2582202, 384632))
pop11 <- log(c(514536, 140204, 519653, 751171, 2772718, 2953873, 462494, 542996, 1175980, 1673365, 402074, 777938, 357853, 3994492, 1288388, 367052, 5768001, 741956, 402623, 512547, 489086, 328295, 745774, 490385, 1263935, 1205342, 2374642, 352050))

pop0116 <- pop16 - pop01
pop0111 <- pop11 - pop01

#2001-2016

set1 <- data.frame(cbind(set1, del_UIC2039M, del_UIC2039F, del_UIC4054M, del_UIC4054F, del_LFNPMH, del_LFNPML, del_LFNPFH, del_LFNPFL))

set4 <- data.frame(cbind(set1, pop0116))

#IV
regUIC2039M <- ivreg(del_UIC2039M ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regUIC2039F <- ivreg(del_UIC2039F ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regUIC4054M <- ivreg(del_UIC4054M ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regUIC4054F <- ivreg(del_UIC4054F ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)

regLFNPMH <- ivreg(del_LFNPMH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regLFNPML <- ivreg(del_LFNPML ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regLFNPFH <- ivreg(del_LFNPFH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regLFNPFL <- ivreg(del_LFNPFL ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)

regpop <- ivreg(pop0116 ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set4)

#OLS
regUIC2039MOLS <- lm(del_UIC2039M ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regUIC2039FOLS <- lm(del_UIC2039F ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regUIC4054MOLS <- lm(del_UIC4054M ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regUIC4054FOLS <- lm(del_UIC4054F ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)

regLFNPMHOLS <- lm(del_LFNPMH ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regLFNPMLOLS <- lm(del_LFNPML ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regLFNPFHOLS <- lm(del_LFNPFH ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)
regLFNPFLOLS <- lm(del_LFNPFL ~ del_ms + he_shr + fe_lfp + for_bor, data = set1)

regpopOLS <- lm(pop0116 ~ del_ms + he_shr + fe_lfp + for_bor, data = set4)

stargazer(regUIC2039MOLS, regUIC2039FOLS, regUIC4054MOLS, regUIC4054FOLS, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Effect of Manufacturing Decline on UI Receipt by Sex-Age Group", type = "text")
stargazer(regLFNPMHOLS, regLFNPMLOLS, regLFNPFHOLS, regLFNPFLOLS, regpopOLS, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Effect of Manufacturing Decline on Labour Force Nonparticipation by Sex-Education Group", type = "text")

stargazer(regUIC2039M, regUIC2039F, regUIC4054M, regUIC4054F, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Effect of Manufacturing Decline on UI Receipt by Sex-Age Group", type = "text")
stargazer(regLFNPMH, regLFNPML, regLFNPFH, regLFNPFL, regpop, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Effect of Manufacturing Decline on Labour Force Nonparticipation by Sex-Education Group", type = "text")

coeftest(regUIC2039M, vcov = vcovHC, type = "HC0")
coeftest(regUIC2039F, vcov = vcovHC, type = "HC0")
coeftest(regUIC4054M, vcov = vcovHC, type = "HC0")
coeftest(regUIC4054F, vcov = vcovHC, type = "HC0")

coeftest(regLFNPMH, vcov = vcovHC, type = "HC0")
coeftest(regLFNPML, vcov = vcovHC, type = "HC0")
coeftest(regLFNPFH, vcov = vcovHC, type = "HC0")
coeftest(regLFNPFL, vcov = vcovHC, type = "HC0")

coeftest(regUIC2039MOLS, vcov = vcovHC, type = "HC0")
coeftest(regUIC2039FOLS, vcov = vcovHC, type = "HC0")
coeftest(regUIC4054MOLS, vcov = vcovHC, type = "HC0")
coeftest(regUIC4054FOLS, vcov = vcovHC, type = "HC0")

coeftest(regLFNPMHOLS, vcov = vcovHC, type = "HC0")
coeftest(regLFNPMLOLS, vcov = vcovHC, type = "HC0")
coeftest(regLFNPFHOLS, vcov = vcovHC, type = "HC0")
coeftest(regLFNPFLOLS, vcov = vcovHC, type = "HC0")

coeftest(regpopOLS, vcov = vcovHC, type = "HC0")
coeftest(regpop, vcov = vcovHC, type = "HC0")

#TABLE

stargazer(regUIC2039MOLS, regUIC2039M, regUIC4054MOLS, regUIC4054M, regLFNPMLOLS, regLFNPML, regpopOLS, regpop, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Explaining the Effect of Manufacturing Decline on Labour Outcomes (2001-2016)")

#2001-2011

set3 <- data.frame(cbind(set3, del_UIC2039M2, del_UIC2039F2, del_UIC4054M2, del_UIC4054F2, del_LFNPMH2, del_LFNPML2, del_LFNPFH2, del_LFNPFL2))

set5 <- data.frame(cbind(set3, pop0111))


regUIC2039M2 <- ivreg(del_UIC2039M2 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regUIC2039F2 <- ivreg(del_UIC2039F2 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regUIC4054M2 <- ivreg(del_UIC4054M2 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regUIC4054F2 <- ivreg(del_UIC4054F2 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)

regLFNPMH2 <- ivreg(del_LFNPMH2 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regLFNPML2 <- ivreg(del_LFNPML2 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regLFNPFH2 <- ivreg(del_LFNPFH2 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)
regLFNPFL2 <- ivreg(del_LFNPFL2 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set3)

regpop11 <- ivreg(pop0111 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set5)

stargazer(regUIC2039M2, regUIC2039F2, regUIC4054M2, regUIC4054F2, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Effect of Manufacturing Decline on UI Receipt by Sex-Age Group", type = "text")
stargazer(regLFNPMH2, regLFNPML2, regLFNPFH2, regLFNPFL2, regpop11, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Effect of Manufacturing Decline on Labour Force Nonparticipation by Sex-Education Group", type = "text")

coeftest(regUIC2039M2, vcov = vcovHC, type = "HC0")
coeftest(regUIC2039F2, vcov = vcovHC, type = "HC0")
coeftest(regUIC4054M2, vcov = vcovHC, type = "HC0")
coeftest(regUIC4054F2, vcov = vcovHC, type = "HC0")

coeftest(regLFNPMH2, vcov = vcovHC, type = "HC0")
coeftest(regLFNPML2, vcov = vcovHC, type = "HC0")
coeftest(regLFNPFH2, vcov = vcovHC, type = "HC0")
coeftest(regLFNPFL2, vcov = vcovHC, type = "HC0")

###Identifcation Tests

##Regression 1: 2001-2016

#Test 1:Instrument Strength / Relevance ~ First-stage

firststage1 <- lm(del_ms ~ bart + fe_lfp + he_shr + for_bor, data = set1)

stargazer(firststage1, omit = c("he_shr", "felfp", "for_bor"), title = "First-Stage: Main Regression (2001-2016)", type = "text")

#Test 2: Covariates

pop2001 <- log(c(512930, 135294, 645530, 729498, 3993069, 5758528, 648975, 686590, 1598277, 2213495, 262477, 501832, 2516764, 225814, 792969, 208778, 3402164, 462598, 256234, 307167, 307903, 218205, 470608, 292343, 701946, 674584, 1473515, 220728)) 
maleshr01 <- subset(census, Year == 2001)$maleshr
propval01 <- log(subset(census, Year == 2001)$propval)
commute01 <- subset(census, Year == 2001)$commute
selfemp01 <- subset(census, Year == 2001)$selfemp
avgage01 <- subset(census, Year == 2001)$avgage
pr01 <- as.factor(subset(census, Year == 2001)$province)

covset1 <- data.frame(cbind(set1$NOCshr17, set1$NOCshr14, set1$NOCshr4, set1$NOCshr5, set1$NOCshr2, pop2001, maleshr01, propval01, commute01, selfemp01, avgage01, set1$he_shr, set1$fe_lfp, set1$for_bor))
colnames(covset1) <- c("shr17", "shr14", "shr4", "shr5", "shr2", "pop", "maleshr", "propval", "commute", "selfemp", "avgage", "he_shr", "fe_lfp", "for_bor")

cov17 <- lm(shr17 ~ pr01 + he_shr + fe_lfp + for_bor + maleshr + commute + propval + avgage + selfemp + pop, data = covset1)
cov14 <- lm(shr14 ~ pr01 + he_shr + fe_lfp + for_bor + maleshr + commute + propval + avgage + selfemp + pop, data = covset1)
cov4 <- lm(shr4 ~ pr01 + he_shr + fe_lfp + for_bor + maleshr + commute + propval + avgage + selfemp + pop, data = covset1)
cov5 <- lm(shr5 ~ pr01 + he_shr + fe_lfp + for_bor + maleshr + commute + propval + avgage + selfemp + pop, data = covset1)
cov2 <- lm(shr2 ~ pr01 + he_shr + fe_lfp + for_bor + maleshr + commute + propval + avgage + selfemp + pop, data = covset1)

stargazer(cov17, cov14, cov4, cov5, cov2, omit = c("pr01"), title = "Relationship between Occupation Shares and Characteristics")

coeftest(cov17, vcov = vcovHC, type = "HC0")
coeftest(cov14, vcov = vcovHC, type = "HC0")
coeftest(cov4, vcov = vcovHC, type = "HC0")
coeftest(cov5, vcov = vcovHC, type = "HC0")
coeftest(cov2, vcov = vcovHC, type = "HC0")

#Test 4: Alternate Estimators and Overid test

IVMH <- IVreg(del_erateMH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1, inference = "standard") #Alternate estimators
IVML <- IVreg(del_erateML ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1, inference = "standard")
IVFH <- IVreg(del_erateFH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1, inference = "standard")
IVFL <- IVreg(del_erateFL ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1, inference = "standard")

IVMHW <- IVreg(del_wageMH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1, inference = "standard")
IVMLW <- IVreg(del_wageML ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1, inference = "standard")
IVFHW <- IVreg(del_wageFH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1, inference = "standard")
IVFLW <- IVreg(del_wageFL ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1, inference = "standard")

IVMH
IVML
IVFH
IVFL

IVMHW
IVMLW
IVFHW
IVFLW

#HFUL
Y = set1[, "del_erateML"]
D = set1[, "del_ms"]
Z = set1[, "bart"]
X = set1[, c("he_shr", "fe_lfp", "for_bor")]

TestModel <- ivmodel(Y = Y, D = D, Z = Z, X = X)
HFUL1 <- Fuller(TestModel, heteroSE = T)
HFUL1

#Overid test: Sargan's Test

residualsML <- regML$residuals
stest1 <- lm(residualsML ~ bart + he_shr + fe_lfp + for_bor, data = set1)
summary(stest1)
teststat1 <- 0.1739*28 #TR^2 (adjusted) follows approximately chi-square
teststat1 > qchisq(0.95, df = 16) #Do not reject null hypothesis -- there is no evidence to suggest that the instrument is not exogenous

pchisq(teststat1, df=16, lower.tail=FALSE)
#Overid test: Anderson-Rubin Test

AR.test(TestModel) #Do not reject null hypothesis -- there is no evidence to suggest that the instrument is not exogenous

#ROBUSTNESS: 2001-2016 Regression with added controls

set1C <- data.frame(cbind(set1, pr01, pop2001, maleshr01, propval01, commute01, selfemp01, avgage01))
set1C$pr01 <- as.factor(set1C$pr01)

regMHC <- ivreg(del_erateMH ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regMLC <- ivreg(del_erateML ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regFHC <- ivreg(del_erateFH ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regFLC <- ivreg(del_erateFL ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)

regMHWC <- ivreg(del_wageMH ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regMLWC <- ivreg(del_wageML ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regFHWC <- ivreg(del_wageFH ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regFLWC <- ivreg(del_wageFL ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)

stargazer(regMHC, regMLC, regFHC, regFLC, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Employment (Added Controls)", type = "text")
stargazer(regMHWC, regMLWC, regFHWC, regFLWC, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Wages (Added Controls)", type = "text")

coeftest(regMHC, vcov = vcovHC, type = "HC0")
coeftest(regMLC, vcov = vcovHC, type = "HC0")
coeftest(regFHC, vcov = vcovHC, type = "HC0")
coeftest(regFLC, vcov = vcovHC, type = "HC0")

coeftest(regMHWC, vcov = vcovHC, type = "HC0")
coeftest(regMLWC, vcov = vcovHC, type = "HC0")
coeftest(regFHWC, vcov = vcovHC, type = "HC0")
coeftest(regFLWC, vcov = vcovHC, type = "HC0")

#OLS

regMHCOLS <- lm(del_erateMH ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regMLCOLS <- lm(del_erateML ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regFHCOLS <- lm(del_erateFH ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regFLCOLS <- lm(del_erateFL ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)

regMHWCOLS <- lm(del_wageMH ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regMLWCOLS <- lm(del_wageML ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regFHWCOLS <- lm(del_wageFH ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)
regFLWCOLS <- lm(del_wageFL ~ del_ms + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set1C)

stargazer(regMHCOLS, regMLCOLS, regFHCOLS, regFLCOLS, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Employment (Added Controls)", type = "text")
stargazer(regMHWCOLS, regMLWCOLS, regFHWCOLS, regFLWCOLS, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Wages (Added Controls)", type = "text")

coeftest(regMLCOLS, vcov = vcovHC, type = "HC0")

#ROBUSTNESS: 2001-2011 Regression with Added Controls

set3C <- data.frame(cbind(set3, pr01, pop2001, maleshr01, propval01, commute01, selfemp01, avgage01))
set3C$pr01 <- as.factor(set3C$pr01)

regMHC11 <- ivreg(del_erateMH0111 ~ del_ms0111 + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set3C)
regMLC11 <- ivreg(del_erateML0111 ~ del_ms0111 + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set3C)
regFHC11 <- ivreg(del_erateFH0111 ~ del_ms0111 + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set3C)
regFLC11 <- ivreg(del_erateFL0111 ~ del_ms0111 + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set3C)

regMHWC11 <- ivreg(del_wageMH0111 ~ del_ms0111 + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set3C)
regMLWC11 <- ivreg(del_wageML0111 ~ del_ms0111 + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set3C)
regFHWC11 <- ivreg(del_wageFH0111 ~ del_ms0111 + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set3C)
regFLWC11 <- ivreg(del_wageFL0111 ~ del_ms0111 + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01 | bart + pr01 + he_shr + fe_lfp + for_bor + pop2001 + maleshr01 + commute01 + selfemp01 + avgage01 + propval01, data = set3C)

stargazer(regMHC11, regMLC11, regFHC11, regFLC11, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Employment (Added Controls)", type = "text")
stargazer(regMHWC11, regMLWC11, regFHWC11, regFLWC11, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Wages (Added Controls)", type = "text")

coeftest(regMHC11, vcov = vcovHC, type = "HC0")
coeftest(regMLC11, vcov = vcovHC, type = "HC0")
coeftest(regFHC11, vcov = vcovHC, type = "HC0")
coeftest(regFLC11, vcov = vcovHC, type = "HC0")

coeftest(regMHWC11, vcov = vcovHC, type = "HC0")
coeftest(regMHWC11, vcov = vcovHC, type = "HC0")
coeftest(regFHWC11, vcov = vcovHC, type = "HC0")
coeftest(regFLWC11, vcov = vcovHC, type = "HC0")
