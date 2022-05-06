#! Robustness Check.R
## Executes the robustness checks for the 'NOC set.'
## NOTE: The last two Stargazer tables require that all of 'Data Analysis Main Results.R' is loaded into the environment. This is because some robustness checks
## (like the addition of controls) is executed by this other file.

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
library(clusterSEs)

census <- data.frame(read.dta13("CMA_panel.dta"))

census$Year <- as.numeric(census$Year) #Convert dataframe variables to proper types
census$CMAList <- as.numeric(census$CMAList)
for(i in 4:71)  {
  census[,i] <- as.numeric(census[,i])
}
census$province <- as.factor(census$province)

CMAList <- c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 205, 421, 462, 499, 505, 532, 535, 537, 539, 541, 555, 559, 602, 799, 825, 835, 933, 935)

### Robustness Check 1: Finer time periods

## 2001-2006

#Controls
he_shr <- subset(census, Year == 2001)$he_shr
fe_lfp <- subset(census, Year == 2001)$fe_lfs
for_bor <- subset(census, Year == 2001)$for_bor

#Independent variable

del_ms <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  del_ms[j] <- subset(census, Year == 2006 & CMAList == i)$manu_shr - subset(census, Year == 2001 & CMAList == i)$manu_shr
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
  del_erateMH[j] <- subset(census, Year == 2006 & CMAList == i)$erateMH - subset(census, Year == 2001 & CMAList == i)$erateMH
  del_erateML[j] <- subset(census, Year == 2006 & CMAList == i)$erateML - subset(census, Year == 2001 & CMAList == i)$erateML
  del_erateFH[j] <- subset(census, Year == 2006 & CMAList == i)$erateFH - subset(census, Year == 2001 & CMAList == i)$erateFH
  del_erateFL[j] <- subset(census, Year == 2006 & CMAList == i)$erateFL - subset(census, Year == 2001 & CMAList == i)$erateFL
  del_wageMH[j] <- subset(census, Year == 2006 & CMAList == i)$logwageMH - subset(census, Year == 2001 & CMAList == i)$logwageMH
  del_wageML[j] <- subset(census, Year == 2006 & CMAList == i)$logwageML - subset(census, Year == 2001 & CMAList == i)$logwageML
  del_wageFH[j] <- subset(census, Year == 2006 & CMAList == i)$logwageFH - subset(census, Year == 2001 & CMAList == i)$logwageFH
  del_wageFL[j] <- subset(census, Year == 2006 & CMAList == i)$logwageFL - subset(census, Year == 2001 & CMAList == i)$logwageFL
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
  shock1[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm1 - subset(census, Year == 2001 & CMAList == i)$man_shrm1
  shock2[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm2 - subset(census, Year == 2001 & CMAList == i)$man_shrm2
  shock3[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm3- subset(census, Year == 2001 & CMAList == i)$man_shrm3
  shock4[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm4 - subset(census, Year == 2001 & CMAList == i)$man_shrm4
  shock5[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm5 - subset(census, Year == 2001 & CMAList == i)$man_shrm5
  shock6[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm6 - subset(census, Year == 2001 & CMAList == i)$man_shrm6
  shock7[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm7 - subset(census, Year == 2001 & CMAList == i)$man_shrm7
  shock8[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm8 - subset(census, Year == 2001 & CMAList == i)$man_shrm8
  shock9[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm9 - subset(census, Year == 2001 & CMAList == i)$man_shrm9
  shock10[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm10 - subset(census, Year == 2001 & CMAList == i)$man_shrm10
  shock11[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm11 - subset(census, Year == 2001 & CMAList == i)$man_shrm11
  shock12[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm12 - subset(census, Year == 2001 & CMAList == i)$man_shrm12
  shock13[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm13 - subset(census, Year == 2001 & CMAList == i)$man_shrm13
  shock14[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm14 - subset(census, Year == 2001 & CMAList == i)$man_shrm14
  shock15[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm15 - subset(census, Year == 2001 & CMAList == i)$man_shrm15
  shock16[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm16 - subset(census, Year == 2001 & CMAList == i)$man_shrm16
  shock17[j] <- subset(census, Year == 2006 & CMAList == i)$man_shrm17 - subset(census, Year == 2001 & CMAList == i)$man_shrm17
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

set1 <- data.frame(cbind(CMAList, subset(census, Year == 2001)$province, he_shr, fe_lfp, for_bor, del_ms, del_erateMH, del_erateML, del_erateFH, del_erateFL, del_wageMH, del_wageML, del_wageFH, del_wageFL, shock1, shock2, shock3, shock4, shock5, shock6, shock7, shock8, shock9, shock10, shock11, shock12, shock13, shock14, shock15, shock16, shock17, NOCshr1, NOCshr2, NOCshr3, NOCshr4, NOCshr5, NOCshr6, NOCshr7, NOCshr8, NOCshr9, NOCshr10, NOCshr11, NOCshr12, NOCshr13, NOCshr14, NOCshr15, NOCshr16, NOCshr17))
colnames(set1)[2] <- "pr"
set1 <- set1 %>%
  mutate(bart = NOCshr1*shock1 + NOCshr2*shock2 + NOCshr3*shock3 + NOCshr4*shock4 + NOCshr5*shock5 + NOCshr6*shock6 + NOCshr7*shock7 + NOCshr8*shock8 + NOCshr9*shock9 + NOCshr10*shock10 + NOCshr11*shock11 + NOCshr12*shock12 + NOCshr13*shock13 + NOCshr14*shock14 + NOCshr15*shock15 + NOCshr16*shock16 + NOCshr17*shock17)

#Main Regression ~ IV
regMH0106 <- ivreg(del_erateMH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regML0106 <- ivreg(del_erateML ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regFH0106 <- ivreg(del_erateFH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regFL0106 <- ivreg(del_erateFL ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)

regMHW0106 <- ivreg(del_wageMH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regMLW0106 <- ivreg(del_wageML ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regFHW0106 <- ivreg(del_wageFH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)
regFLW0106 <- ivreg(del_wageFL ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set1)

stargazer(regMH0106, regML0106, regFH0106, regFL0106, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Employment", type = "text")
stargazer(regMHW0106, regMLW0106, regFHW0106, regFLW0106, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Wages", type = "text")

coeftest(regMH0106, vcov = vcovHC, type = "HC0")
coeftest(regML0106, vcov = vcovHC, type = "HC0")
coeftest(regFH0106, vcov = vcovHC, type = "HC0")
coeftest(regFL0106, vcov = vcovHC, type = "HC0")

coeftest(regMHW0106, vcov = vcovHC, type = "HC0")
coeftest(regMLW0106, vcov = vcovHC, type = "HC0")
coeftest(regFHW0106, vcov = vcovHC, type = "HC0")
coeftest(regFLW0106, vcov = vcovHC, type = "HC0")

## 2006-2011

#Controls
he_shr <- subset(census, Year == 2006)$he_shr
fe_lfp <- subset(census, Year == 2006)$fe_lfs
for_bor <- subset(census, Year == 2006)$for_bor

#Independent variable

del_ms <- vector()

foreach(i = CMAList, j = 1:29) %do%  {
  del_ms[j] <- subset(census, Year == 2011 & CMAList == i)$manu_shr - subset(census, Year == 2006 & CMAList == i)$manu_shr
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
  del_erateMH[j] <- subset(census, Year == 2011 & CMAList == i)$erateMH - subset(census, Year == 2006 & CMAList == i)$erateMH
  del_erateML[j] <- subset(census, Year == 2011 & CMAList == i)$erateML - subset(census, Year == 2006 & CMAList == i)$erateML
  del_erateFH[j] <- subset(census, Year == 2011 & CMAList == i)$erateFH - subset(census, Year == 2006 & CMAList == i)$erateFH
  del_erateFL[j] <- subset(census, Year == 2011 & CMAList == i)$erateFL - subset(census, Year == 2006 & CMAList == i)$erateFL
  del_wageMH[j] <- subset(census, Year == 2011 & CMAList == i)$logwageMH - subset(census, Year == 2006 & CMAList == i)$logwageMH
  del_wageML[j] <- subset(census, Year == 2011 & CMAList == i)$logwageML - subset(census, Year == 2006 & CMAList == i)$logwageML
  del_wageFH[j] <- subset(census, Year == 2011 & CMAList == i)$logwageFH - subset(census, Year == 2006 & CMAList == i)$logwageFH
  del_wageFL[j] <- subset(census, Year == 2011 & CMAList == i)$logwageFL - subset(census, Year == 2006 & CMAList == i)$logwageFL
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
  shock1[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm1 - subset(census, Year == 2006 & CMAList == i)$man_shrm1
  shock2[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm2 - subset(census, Year == 2006 & CMAList == i)$man_shrm2
  shock3[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm3- subset(census, Year == 2006 & CMAList == i)$man_shrm3
  shock4[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm4 - subset(census, Year == 2006 & CMAList == i)$man_shrm4
  shock5[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm5 - subset(census, Year == 2006 & CMAList == i)$man_shrm5
  shock6[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm6 - subset(census, Year == 2006 & CMAList == i)$man_shrm6
  shock7[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm7 - subset(census, Year == 2006 & CMAList == i)$man_shrm7
  shock8[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm8 - subset(census, Year == 2006 & CMAList == i)$man_shrm8
  shock9[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm9 - subset(census, Year == 2006 & CMAList == i)$man_shrm9
  shock10[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm10 - subset(census, Year == 2006 & CMAList == i)$man_shrm10
  shock11[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm11 - subset(census, Year == 2006 & CMAList == i)$man_shrm11
  shock12[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm12 - subset(census, Year == 2006 & CMAList == i)$man_shrm12
  shock13[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm13 - subset(census, Year == 2006 & CMAList == i)$man_shrm13
  shock14[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm14 - subset(census, Year == 2006 & CMAList == i)$man_shrm14
  shock15[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm15 - subset(census, Year == 2006 & CMAList == i)$man_shrm15
  shock16[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm16 - subset(census, Year == 2006 & CMAList == i)$man_shrm16
  shock17[j] <- subset(census, Year == 2011 & CMAList == i)$man_shrm17 - subset(census, Year == 2006 & CMAList == i)$man_shrm17
}

NOCshr1 <- subset(census, Year == 2006)$NOC_shr1
NOCshr2 <- subset(census, Year == 2006)$NOC_shr2
NOCshr3 <- subset(census, Year == 2006)$NOC_shr3
NOCshr4 <- subset(census, Year == 2006)$NOC_shr4
NOCshr5 <- subset(census, Year == 2006)$NOC_shr5
NOCshr6 <- subset(census, Year == 2006)$NOC_shr6
NOCshr7 <- subset(census, Year == 2006)$NOC_shr7
NOCshr8 <- subset(census, Year == 2006)$NOC_shr8
NOCshr9 <- subset(census, Year == 2006)$NOC_shr9
NOCshr10 <- subset(census, Year == 2006)$NOC_shr10
NOCshr11 <- subset(census, Year == 2006)$NOC_shr11
NOCshr12 <- subset(census, Year == 2006)$NOC_shr12
NOCshr13 <- subset(census, Year == 2006)$NOC_shr13
NOCshr14 <- subset(census, Year == 2006)$NOC_shr14
NOCshr15 <- subset(census, Year == 2006)$NOC_shr15
NOCshr16 <- subset(census, Year == 2006)$NOC_shr16
NOCshr17 <- subset(census, Year == 2006)$NOC_shr17

set2 <- data.frame(cbind(CMAList, he_shr, fe_lfp, for_bor, del_ms, del_erateMH, del_erateML, del_erateFH, del_erateFL, del_wageMH, del_wageML, del_wageFH, del_wageFL, shock1, shock2, shock3, shock4, shock5, shock6, shock7, shock8, shock9, shock10, shock11, shock12, shock13, shock14, shock15, shock16, shock17, NOCshr1, NOCshr2, NOCshr3, NOCshr4, NOCshr5, NOCshr6, NOCshr7, NOCshr8, NOCshr9, NOCshr10, NOCshr11, NOCshr12, NOCshr13, NOCshr14, NOCshr15, NOCshr16, NOCshr17))

set2 <- set2 %>%
  mutate(bart = NOCshr1*shock1 + NOCshr2*shock2 + NOCshr3*shock3 + NOCshr4*shock4 + NOCshr5*shock5 + NOCshr6*shock6 + NOCshr7*shock7 + NOCshr8*shock8 + NOCshr9*shock9 + NOCshr10*shock10 + NOCshr11*shock11 + NOCshr12*shock12 + NOCshr13*shock13 + NOCshr14*shock14 + NOCshr15*shock15 + NOCshr16*shock16 + NOCshr17*shock17)

#Main Regression ~ IV
regMH0611 <- ivreg(del_erateMH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set2)
regML0611 <- ivreg(del_erateML ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set2)
regFH0611 <- ivreg(del_erateFH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set2)
regFL0611 <- ivreg(del_erateFL ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set2)

regMHW0611 <- ivreg(del_wageMH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set2)
regMLW0611 <- ivreg(del_wageML ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set2)
regFHW0611 <- ivreg(del_wageFH ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set2)
regFLW0611 <- ivreg(del_wageFL ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = set2)

stargazer(regMH0611, regML0611, regFH0611, regFL0611, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Employment", type = "text")
stargazer(regMHW0611, regMLW0611, regFHW0611, regFLW0611, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates of Main Regression by Demographic Group -- Wages", type = "text")

coeftest(regMH0611, vcov = vcovHC, type = "HC0")
coeftest(regML0611, vcov = vcovHC, type = "HC0")
coeftest(regFH0611, vcov = vcovHC, type = "HC0")
coeftest(regFL0611, vcov = vcovHC, type = "HC0")

coeftest(regMHW0611, vcov = vcovHC, type = "HC0")
coeftest(regMLW0611, vcov = vcovHC, type = "HC0")
coeftest(regFHW0611, vcov = vcovHC, type = "HC0")
coeftest(regFLW0611, vcov = vcovHC, type = "HC0")

#TABLE

stargazer(regML0106, regMLW0106, regML0611, regML0611, omit = c("he_shr", "fe_lfp", "for_bor"), title = "IV Estimates - Effect of Manufacturing Decline on Employment Rate in Short Periods")

###Robustness Check 2: Effects on Different Education and Age Groups

##2001 Outcome Construction

census2001 <- read.dta13("Census_2001.dta")
census2001 <- subset(census2001, select = c("PROVP", "CMAP",  "NAICSP", "NOCS01P", "LFACTP", "HRSWKP", "WAGESP", "SEXP", "HLOSP", "IMMPOPP", "AGEP", "YRIMMIG", "VISMINP", "UICBNP", "WKSWKP", "FPTWKP", "VALUEP", "DISTP", "COWP")) %>%
  mutate(loghrs = ifelse(HRSWKP*WKSWKP != 0, log(HRSWKP*WKSWKP), 0)) %>%
  mutate(logwage = ifelse(WAGESP != 0, log(WAGESP), 0)) %>%
  mutate(CMAP = case_when(
    PROVP == 10 ~ 9,
    PROVP == 11 ~ 19,
    PROVP == 12 & CMAP == 999 ~ 29, 
    PROVP == 13 ~ 39,
    PROVP == 24 & CMAP == 999 ~ 49,
    PROVP == 35 & CMAP == 999 ~ 59, 
    PROVP == 46 & CMAP == 999 ~ 69,
    PROVP == 47 & CMAP == 999 ~ 79,
    PROVP == 48 & CMAP == 999 ~ 89,
    PROVP == 59 & CMAP == 999 ~ 99,
    CMAP == 205 ~ 205,
    CMAP == 421 ~ 421,
    CMAP == 462 ~ 462,
    CMAP == 499 ~ 499,
    CMAP == 505 ~ 505,
    CMAP == 532 ~ 532,
    CMAP == 535 ~ 535,
    CMAP == 537 ~ 537,
    CMAP == 539 ~ 539,
    CMAP == 541 ~ 541,
    CMAP == 555 ~ 555,
    CMAP == 559 ~ 559,
    CMAP == 602 ~ 602,
    CMAP == 799 ~ 799,
    CMAP == 825 ~ 825,
    CMAP == 835 ~ 835,
    CMAP == 933 ~ 933,
    CMAP == 935 ~ 935
  ))

census2001 <- subset(census2001, LFACTP!= 99) #Eliminate obs. with no wage recorded

#Males, Trade certifications only
census01MTrades <- subset(census2001, SEXP == 2 & (HLOSP == 5 | HLOSP == 7) & HLOSP != 99) #Trades certificate or diploma
census01MTrades <- subset(census01MTrades, AGEP >= 20 & AGEP <= 54)
#Males, Trade certifications or college diploma
census01MCollege <- subset(census2001, SEXP == 2 & HLOSP >= 5 & HLOSP <= 8)
census01MCollege <- subset(census01MCollege, AGEP >= 20 & AGEP <= 54)
#Males, High education (40-54)
census01MHO <- subset(census2001, SEXP == 2 & HLOSP >= 10 & HLOSP != 99) #Males aged 40-54
census01MHO <- subset(census01MHO, AGEP >= 40 & AGEP <= 54)
#Males, Low education (40-54)
census01MLO <- subset(census2001, SEXP == 2 & HLOSP < 10)
census01MLO <- subset(census01MLO, AGEP >= 40 & AGEP <= 54)
#Females, Trade certifications only
census01FTrades <- subset(census2001, SEXP == 1 & (HLOSP == 5 | HLOSP == 7) & HLOSP != 99) #Trades certificate or diploma
census01FTrades <- subset(census01FTrades, AGEP >= 20 & AGEP <= 54)
#Females, Trade certifications or college diploma
census01FCollege <- subset(census2001, SEXP == 1 & HLOSP >= 5 & HLOSP <= 8)
census01FCollege <- subset(census01FCollege, AGEP >= 20 & AGEP <= 54)
#Females, High education (40-54)
census01FHO <- subset(census2001, SEXP == 1 & HLOSP >= 10 & HLOSP != 99)
census01FHO <- subset(census01FHO, AGEP >= 40 & AGEP <= 54)
#Females, Low education (40-54)
census01FLO <- subset(census2001, SEXP == 1 & HLOSP < 10)
census01FLO <- subset(census01FLO, AGEP >= 40 & AGEP <= 54)

CMAList <- c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 205, 421, 462, 499, 505, 532, 535, 537, 539, 541, 555, 559, 602, 799, 825, 835, 933, 935)

logwageMHO <- vector()
logwageMLO <- vector()
logwageFHO <- vector()
logwageFLO <- vector()
erateMHO <- vector()
erateMLO <- vector()
erateFHO <- vector()
erateFLO <- vector()
logwageMCollege <- vector()
logwageMTrades <- vector()
logwageFCollege <- vector()
logwageFTrades <- vector()
erateMCollege <- vector()
erateMTrades <- vector()
erateFCollege <- vector()
erateFTrades <- vector()

foreach(i = 1:29, j = CMAList) %do%  {
  logwageMHO[i] <- median(subset(census01MHO, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  logwageMLO[i] <- median(subset(census01MLO, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  logwageFHO[i] <- median(subset(census01FHO, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  logwageFLO[i] <- median(subset(census01FLO, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  erateMHO[i] <- nrow(subset(census01MHO, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01MHO, LFACTP <= 10 & CMAP == j))
  erateMLO[i] <- nrow(subset(census01MLO, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01MLO, LFACTP <= 10 & CMAP == j))
  erateFHO[i] <- nrow(subset(census01FHO, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01FHO, LFACTP <= 10 & CMAP == j))
  erateFLO[i] <- nrow(subset(census01FLO, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01FLO, LFACTP <= 10 & CMAP == j))
  logwageMTrades[i] <- median(subset(census01MTrades, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  logwageMCollege[i] <- median(subset(census01MCollege, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  logwageFTrades[i] <- median(subset(census01FTrades, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  logwageFCollege[i] <- median(subset(census01FCollege, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  erateMTrades[i] <- nrow(subset(census01MTrades, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01MTrades, LFACTP <= 10 & CMAP == j))
  erateMCollege[i] <- nrow(subset(census01MCollege, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01MCollege, LFACTP <= 10 & CMAP == j))
  erateFTrades[i] <- nrow(subset(census01FTrades, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01FTrades, LFACTP <= 10 & CMAP == j))
  erateFCollege[i] <- nrow(subset(census01FCollege, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01FCollege, LFACTP <= 10 & CMAP == j))
}

##2011 Outcome Construction

census2011  <- read.dta13("NHS_2011.dta")
census2011 <- subset(census2011, select = c("PR", "CMA",  "NAICS", "NOC11", "LFACT", "WAGES", "SEX", "HDGREE", "IMMSTAT", "AGEGRP", "YRIMM", "VISMIN", "EICBN", "FPTWK", "VALUE", "DIST", "COW")) %>%
  mutate(logwage = ifelse(WAGES != 0, log(WAGES * 0.815679733), 0)) %>%
  mutate(CMA = case_when(
    PR == 10 ~ 9,
    PR == 11 ~ 19,
    PR == 12 & CMA == 999 ~ 29, 
    PR == 13 ~ 39,
    PR == 24 & CMA == 999 ~ 49,
    PR == 35 & (CMA == 999 | CMA == 577 | CMA == 588) ~ 59, 
    PR == 46 & CMA == 999 ~ 69,
    PR == 47 & CMA == 999 ~ 79,
    PR == 48 & CMA == 999 ~ 89,
    PR == 59 & (CMA == 999 | CMA == 988) ~ 99,
    CMA == 205 ~ 205,
    CMA == 421 ~ 421,
    CMA == 462 ~ 462,
    CMA == 499 ~ 499,
    CMA == 505 ~ 505,
    CMA == 532 ~ 532,
    CMA == 535 ~ 535,
    CMA == 537 ~ 537,
    CMA == 539 ~ 539,
    CMA == 541 ~ 541,
    CMA == 555 ~ 555,
    CMA == 559 ~ 559,
    CMA == 602 ~ 602,
    CMA == 799 ~ 799,
    CMA == 825 ~ 825,
    CMA == 835 ~ 835,
    CMA == 933 ~ 933,
    CMA == 935 ~ 935
  ))

census2011 <- subset(census2011, LFACT != 99) #Eliminate obs. with no wage recorded

#Males, High education
census11MHO <- subset(census2011, SEX == 2 & HDGREE >= 8 & HDGREE != 88 & HDGREE != 99)
census11MHO <- subset(census11MHO, AGEGRP >= 12 & AGEGRP <= 14)
#Males, Low education
census11MLO <- subset(census2011, SEX == 2 & HDGREE < 8)
census11MLO <- subset(census11MLO, AGEGRP >= 12 & AGEGRP <= 14)
#Females, High education
census11FHO <- subset(census2011, SEX == 1 & HDGREE >= 8 & HDGREE != 88 & HDGREE != 99)
census11FHO <- subset(census11FHO, AGEGRP >= 12 & AGEGRP <= 14)
#Females, Low education
census11FLO <- subset(census2011, SEX == 1 & HDGREE < 8)
census11FLO <- subset(census11FLO, AGEGRP >= 12 & AGEGRP <= 14)
#Males, Trade certifications only
census11MTrades <- subset(census2011, SEX == 2 & (HDGREE == 3 | HDGREE == 4)) #Trades certificate or diploma
census11MTrades <- subset(census11MTrades, AGEGRP >= 8 & AGEGRP <= 14)
#Males, Trade certifications or college diploma
census11MCollege <- subset(census2011, SEX == 2 & HDGREE >= 3 & HDGREE <= 7)
census11MCollege <- subset(census11MCollege, AGEGRP >= 8 & AGEGRP <= 14)
#Females, Trade certifications only
census11FTrades <- subset(census2011, SEX == 1 & (HDGREE == 3 | HDGREE == 4)) #Trades certificate or diploma
census11FTrades <- subset(census11FTrades, AGEGRP >= 8 & AGEGRP <= 14)
#Females, Trade certifications or college diploma
census11FCollege <- subset(census2011, SEX == 1 & HDGREE >= 3 & HDGREE <= 7)
census11FCollege <- subset(census11FCollege, AGEGRP >= 8 & AGEGRP <= 14)

logwageMHO11 <- vector()
logwageMLO11 <- vector()
logwageFHO11 <- vector()
logwageFLO11 <- vector()
erateMHO11 <- vector()
erateMLO11 <- vector()
erateFHO11 <- vector()
erateFLO11 <- vector()
logwageMCollege11 <- vector()
logwageMTrades11 <- vector()
logwageFCollege11 <- vector()
logwageFTrades11 <- vector()
erateMCollege11 <- vector()
erateMTrades11 <- vector()
erateFCollege11 <- vector()
erateFTrades11 <- vector()

foreach(i = 1:29, j = CMAList) %do%  {
  logwageMHO11[i] <- median(subset(census11MHO, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageMLO11[i] <- median(subset(census11MLO, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFHO11[i] <- median(subset(census11FHO, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFLO11[i] <- median(subset(census11FLO, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  erateMHO11[i] <- nrow(subset(census11MHO, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11MHO, LFACT <= 10 & CMA == j))
  erateMLO11[i] <- nrow(subset(census11MLO, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11MLO, LFACT <= 10 & CMA == j))
  erateFHO11[i] <- nrow(subset(census11FHO, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11FHO, LFACT <= 10 & CMA == j))
  erateFLO11[i] <- nrow(subset(census11FLO, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11FLO, LFACT <= 10 & CMA == j))
  logwageMTrades11[i] <- median(subset(census11MTrades, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageMCollege11[i] <- median(subset(census11MCollege, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFTrades11[i] <- median(subset(census11FTrades, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFCollege11[i] <- median(subset(census11FCollege, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  erateMTrades11[i] <- nrow(subset(census11MTrades, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11MTrades, LFACT <= 10 & CMA == j))
  erateMCollege11[i] <- nrow(subset(census11MCollege, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11MCollege, LFACT <= 10 & CMA == j))
  erateFTrades11[i] <- nrow(subset(census11FTrades, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11FTrades, LFACT <= 10 & CMA == j))
  erateFCollege11[i] <- nrow(subset(census11FCollege, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11FCollege, LFACT <= 10 & CMA == j))
}

del_wageMHO11 <- logwageMHO11 - logwageMHO
del_wageMLO11 <- logwageMLO11 - logwageMLO
del_wageFHO11 <- logwageFHO11 - logwageFHO
del_wageFLO11 <- logwageFLO11 - logwageFLO
del_erateMHO11 <- erateMHO11 - erateMHO
del_erateMLO11 <- erateMLO11 - erateMLO
del_erateFHO11 <- erateFHO11 - erateFHO
del_erateFLO11 <- erateFLO11 - erateFLO
del_wageMTrades11 <- logwageMTrades11 - logwageMTrades
del_wageMCollege11 <- logwageMCollege11 - logwageMCollege
del_wageFTrades11 <- logwageFTrades11 - logwageFTrades
del_wageFCollege11 <- logwageFCollege11 - logwageFCollege
del_erateMTrades11 <- erateMTrades11 - erateMTrades
del_erateMCollege11 <- erateMCollege11 - erateMCollege
del_erateFTrades11 <- erateFTrades11 - erateFTrades
del_erateFCollege11 <- erateFCollege11 - erateFCollege

##2016 Outcome Construction

census2016 <- read.dta13("Census_2016.dta")
census2016 <- subset(census2016, select = c("PR", "CMA",  "NAICS", "NOC16", "LFACT", "Wages", "Sex", "HDGREE", "IMMSTAT", "AGEGRP", "YRIMM", "VisMin", "EICBN", "WKSWRK", "FPTWK", "VALUE", "DIST", "COW")) %>%
  mutate(logwage = ifelse(Wages != 0, log(Wages * 0.761682243), 0)) %>%
  mutate(CMA = case_when(
    PR == 10 ~ 9,
    PR == 11 ~ 19,
    PR == 12 & CMA == 999 ~ 29, 
    PR == 13 ~ 39,
    PR == 24 & CMA == 999 ~ 49,
    PR == 35 & (CMA == 999 | CMA == 577 | CMA == 588) ~ 59, 
    PR == 46 & CMA == 999 ~ 69,
    PR == 47 & CMA == 999 ~ 79,
    PR == 48 & CMA == 999 ~ 89,
    PR == 59 & (CMA == 999 | CMA == 988) ~ 99,
    CMA == 205 ~ 205,
    CMA == 421 ~ 421,
    CMA == 462 ~ 462,
    CMA == 499 ~ 499,
    CMA == 505 ~ 505,
    CMA == 532 ~ 532,
    CMA == 535 ~ 535,
    CMA == 537 ~ 537,
    CMA == 539 ~ 539,
    CMA == 541 ~ 541,
    CMA == 555 ~ 555,
    CMA == 559 ~ 559,
    CMA == 602 ~ 602,
    CMA == 799 ~ 799,
    CMA == 825 ~ 825,
    CMA == 835 ~ 835,
    CMA == 933 ~ 933,
    CMA == 935 ~ 935
  ))

census2016 <- subset(census2016, LFACT != 99) #Eliminate obs. with no wage recorded

#Males, Trade certifications only
census16MTrades <- subset(census2016, Sex == 2 & (HDGREE == 3 | HDGREE == 4)) #Trades certificate or diploma
census16MTrades <- subset(census16MTrades, AGEGRP >= 8 & AGEGRP <= 14)
#Males, Trade certifications or college diploma
census16MCollege <- subset(census2016, Sex == 2 & HDGREE >= 3 & HDGREE <= 7)
census16MCollege <- subset(census16MCollege, AGEGRP >= 8 & AGEGRP <= 14)
#Males, High education (40-54)
census16MHO <- subset(census2016, Sex == 2 & HDGREE >= 8 & HDGREE != 88 & HDGREE != 99) #Males aged 40-54
census16MHO <- subset(census16MHO, AGEGRP >= 12 & AGEGRP <= 14)
#Males, Low education (40-54)
census16MLO <- subset(census2016, Sex == 2 & HDGREE < 8)
census16MLO <- subset(census16MLO, AGEGRP >= 12 & AGEGRP <= 14)
#Females, Trade certifications only
census16FTrades <- subset(census2016, Sex == 1 & (HDGREE == 3 | HDGREE == 4)) #Trades certificate or diploma
census16FTrades <- subset(census16FTrades, AGEGRP >= 8 & AGEGRP <= 14)
#Females, Trade certifications or college diploma
census16FCollege <- subset(census2016, Sex == 1 & HDGREE >= 3 & HDGREE <= 7)
census16FCollege <- subset(census16FCollege, AGEGRP >= 8 & AGEGRP <= 14)
#Females, High education (40-54)
census16FHO <- subset(census2016, Sex == 1 & HDGREE >= 8 & HDGREE != 88 & HDGREE != 99)
census16FHO <- subset(census16FHO, AGEGRP >= 12 & AGEGRP <= 14)
#Females, Low education (40-54)
census16FLO <- subset(census2016, Sex == 1 & HDGREE < 8)
census16FLO <- subset(census16FLO, AGEGRP >= 12 & AGEGRP <= 14)

logwageMHO16 <- vector()
logwageMLO16 <- vector()
logwageFHO16 <- vector()
logwageFLO16 <- vector()
erateMHO16 <- vector()
erateMLO16 <- vector()
erateFHO16 <- vector()
erateFLO16 <- vector()
logwageMCollege16 <- vector()
logwageMTrades16 <- vector()
logwageFCollege16 <- vector()
logwageFTrades16 <- vector()
erateMCollege16 <- vector()
erateMTrades16 <- vector()
erateFCollege16 <- vector()
erateFTrades16 <- vector()

foreach(i = 1:29, j = CMAList) %do%  {
  logwageMHO16[i] <- median(subset(census16MHO, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageMLO16[i] <- median(subset(census16MLO, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFHO16[i] <- median(subset(census16FHO, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFLO16[i] <- median(subset(census16FLO, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  erateMHO16[i] <- nrow(subset(census16MHO, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16MHO, LFACT <= 10 & CMA == j))
  erateMLO16[i] <- nrow(subset(census16MLO, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16MLO, LFACT <= 10 & CMA == j))
  erateFHO16[i] <- nrow(subset(census16FHO, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16FHO, LFACT <= 10 & CMA == j))
  erateFLO16[i] <- nrow(subset(census16FLO, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16FLO, LFACT <= 10 & CMA == j))
  logwageMTrades16[i] <- median(subset(census16MTrades, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageMCollege16[i] <- median(subset(census16MCollege, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFTrades16[i] <- median(subset(census16FTrades, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFCollege16[i] <- median(subset(census16FCollege, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  erateMTrades16[i] <- nrow(subset(census16MTrades, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16MTrades, LFACT <= 10 & CMA == j))
  erateMCollege16[i] <- nrow(subset(census16MCollege, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16MCollege, LFACT <= 10 & CMA == j))
  erateFTrades16[i] <- nrow(subset(census16FTrades, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16FTrades, LFACT <= 10 & CMA == j))
  erateFCollege16[i] <- nrow(subset(census16FCollege, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16FCollege, LFACT <= 10 & CMA == j))
}

del_wageMHO <- logwageMHO16 - logwageMHO
del_wageMLO <- logwageMLO16 - logwageMLO
del_wageFHO <- logwageFHO16 - logwageFHO
del_wageFLO <- logwageFLO16 - logwageFLO
del_erateMHO <- erateMHO16 - erateMHO
del_erateMLO <- erateMLO16 - erateMLO
del_erateFHO <- erateFHO16 - erateFHO
del_erateFLO <- erateFLO16 - erateFLO
del_wageMTrades <- logwageMTrades16 - logwageMTrades
del_wageMCollege <- logwageMCollege16 - logwageMCollege
del_wageFTrades <- logwageFTrades16 - logwageFTrades
del_wageFCollege <- logwageFCollege16 - logwageFCollege
del_erateMTrades <- erateMTrades16 - erateMTrades
del_erateMCollege <- erateMCollege16 - erateMCollege
del_erateFTrades <- erateFTrades16 - erateFTrades
del_erateFCollege <- erateFCollege16 - erateFCollege

set1 <- read.dta13("bartikset1.dta")
set2 <- read.dta13("bartikset2.dta")

setrobust <- data.frame(cbind(set1, del_wageMHO, del_wageMLO, del_wageFHO, del_wageFLO, del_erateMHO, del_erateMLO, del_erateFHO, del_erateFLO, del_wageMTrades, del_wageMCollege, del_wageFTrades, del_wageFCollege, del_erateMTrades, del_erateMCollege, del_erateFTrades, del_erateFCollege))
setrobust11 <- data.frame(cbind(set2, del_wageMHO11, del_wageMLO11, del_wageFHO11, del_wageFLO11, del_erateMHO11, del_erateMLO11, del_erateFHO11, del_erateFLO11, del_wageMTrades11, del_wageMCollege11, del_wageFTrades11, del_wageFCollege11, del_erateMTrades11, del_erateMCollege11, del_erateFTrades11, del_erateFCollege11))


##Regressions ~ 2001-2016

regMHO <- ivreg(del_erateMHO ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regMLO <- ivreg(del_erateMLO ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regFHO <- ivreg(del_erateFHO ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regFLO <- ivreg(del_erateFLO ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regMTrades <- ivreg(del_erateMTrades ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regMCollege <- ivreg(del_erateMCollege ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regFTrades <- ivreg(del_erateFTrades ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regFCollege <- ivreg(del_erateFCollege ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)

regMHOW <- ivreg(del_wageMHO ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regMLOW <- ivreg(del_wageMLO ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regFHOW <- ivreg(del_wageFHO ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regFLOW <- ivreg(del_wageFLO ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regMTradesW <- ivreg(del_wageMTrades ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regMCollegeW <- ivreg(del_wageMCollege ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regFTradesW <- ivreg(del_wageFTrades ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)
regFCollegeW <- ivreg(del_wageFCollege ~ del_ms + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust)

stargazer(regMHO, regMLO, regFHO, regFLO, regMTrades, regMCollege, regFTrades, regFCollege, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Robustness Checks: Impact of Manufacturing Decline on Employment Rate (2001-2016)", type = "text")
stargazer(regMHOW, regMLOW, regFHOW, regFLOW, regMTradesW, regMCollegeW, regFTradesW, regFCollegeW, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Robustness Checks: Impact of Manufacturing Decline on Median Wage (2001-2016)", type = "text")

coeftest(regMHO, vcov = vcovHC, type = "HC0")
coeftest(regMLO, vcov = vcovHC, type = "HC0")
coeftest(regFHO, vcov = vcovHC, type = "HC0")
coeftest(regFLO, vcov = vcovHC, type = "HC0")
coeftest(regMTrades, vcov = vcovHC, type = "HC0")
coeftest(regMCollege, vcov = vcovHC, type = "HC0")
coeftest(regFTrades, vcov = vcovHC, type = "HC0")
coeftest(regFCollege, vcov = vcovHC, type = "HC0")

coeftest(regMHOW, vcov = vcovHC, type = "HC0")
coeftest(regMLOW, vcov = vcovHC, type = "HC0")
coeftest(regFHOW, vcov = vcovHC, type = "HC0")
coeftest(regFLOW, vcov = vcovHC, type = "HC0")
coeftest(regMTradesW, vcov = vcovHC, type = "HC0")
coeftest(regMCollegeW, vcov = vcovHC, type = "HC0")
coeftest(regMTradesW, vcov = vcovHC, type = "HC0")
coeftest(regMCollegeW, vcov = vcovHC, type = "HC0")

#OLS

regMHOols <- lm(del_erateMHO ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regMLOols <- lm(del_erateMLO ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regFHOols <- lm(del_erateFHO ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regFLOols <- lm(del_erateFLO ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regMTradesols <- lm(del_erateMTrades ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regMCollegeols <- lm(del_erateMCollege ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regFTradesols <- lm(del_erateFTrades ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regFCollegeols <- lm(del_erateFCollege ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)

regMHOWols <- lm(del_wageMHO ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regMLOWols <- lm(del_wageMLO ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regFHOWols <- lm(del_wageFHO ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regFLOWols <- lm(del_wageFLO ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regMTradesWols <- lm(del_wageMTrades ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regMCollegeWols <- lm(del_wageMCollege ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regFTradesWols <- lm(del_wageFTrades ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)
regFCollegeWols <- lm(del_wageFCollege ~ del_ms + he_shr + fe_lfp + for_bor, data = setrobust)

stargazer(regMHOols, regMLOols, regFHOols, regFLOols, regMTradesols, regMCollegeols, regFTradesols, regFCollegeols, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Robustness Checks: Impact of Manufacturing Decline on Employment Rate (2001-2016)", type = "text")
stargazer(regMHOWols, regMLOWols, regFHOWols, regFLOWols, regMTradesWols, regMCollegeWols, regFTradesWols, regFCollegeWols, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Robustness Checks: Impact of Manufacturing Decline on Median Wage (2001-2016)", type = "text")

coeftest(regMLOols, vcov = vcovHC, type = "HC0")
coeftest(regMTradesols, vcov = vcovHC, type = "HC0")
coeftest(regMCollegeols, vcov = vcovHC, type = "HC0")

##Regressions ~ 2001-2011

regMHO11 <- ivreg(del_erateMHO11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regMLO11 <- ivreg(del_erateMLO11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regFHO11 <- ivreg(del_erateFHO11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regFLO11 <- ivreg(del_erateFLO11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regMTrades11 <- ivreg(del_erateMTrades11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regMCollege11 <- ivreg(del_erateMCollege11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regFTrades11 <- ivreg(del_erateFTrades11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regFCollege11 <- ivreg(del_erateFCollege11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)

regMHOW11 <- ivreg(del_wageMHO11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regMLOW11 <- ivreg(del_wageMLO11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regFHOW11 <- ivreg(del_wageFHO11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regFLOW11 <- ivreg(del_wageFLO11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regMTradesW11 <- ivreg(del_wageMTrades11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regMCollegeW11 <- ivreg(del_wageMCollege11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regFTradesW11 <- ivreg(del_wageFTrades11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)
regFCollegeW11 <- ivreg(del_wageFCollege11 ~ del_ms0111 + he_shr + fe_lfp + for_bor | bart + he_shr + fe_lfp + for_bor, data = setrobust11)

stargazer(regMHO11, regMLO11, regFHO11, regFLO11, regMTrades11, regMCollege11, regFTrades11, regFCollege11, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Robustness Checks: Impact of Manufacturing Decline on Employment Rate (2001-2011)", type = "text")
stargazer(regMHOW11, regMLOW11, regFHOW11, regFLOW11, regMTradesW11, regMCollegeW11, regFTradesW11, regFCollegeW11, omit = c("he_shr", "fe_lfp", "for_bor"), title = "Robustness Checks: Impact of Manufacturing Decline on Median Wage (2001-2011)", type = "text")

coeftest(regMHO11, vcov = vcovHC, type = "HC0")
coeftest(regMLO11, vcov = vcovHC, type = "HC0")
coeftest(regFHO11, vcov = vcovHC, type = "HC0")
coeftest(regFLO11, vcov = vcovHC, type = "HC0")
coeftest(regMTrades11, vcov = vcovHC, type = "HC0")
coeftest(regMCollege11, vcov = vcovHC, type = "HC0")
coeftest(regFTrades11, vcov = vcovHC, type = "HC0")
coeftest(regFCollege11, vcov = vcovHC, type = "HC0")

coeftest(regMHOW11, vcov = vcovHC, type = "HC0")
coeftest(regMLOW11, vcov = vcovHC, type = "HC0")
coeftest(regFHOW11, vcov = vcovHC, type = "HC0")
coeftest(regFLOW11, vcov = vcovHC, type = "HC0")
coeftest(regMTradesW11, vcov = vcovHC, type = "HC0")
coeftest(regMCollegeW11, vcov = vcovHC, type = "HC0")
coeftest(regMTradesW11, vcov = vcovHC, type = "HC0")
coeftest(regMCollegeW11, vcov = vcovHC, type = "HC0")

#TABLE (DATA ANALYSIS MAIN RESULTS MUST BE IN ENVIRONMENT)
stargazer(regMLC, regML2, regMLO, regMTrades, regMCollege, omit = "pr01", title = "Robustness Checks: Effect of Manufacturing Decline on Employment Rate (2001-2016)")

stargazer(regMLCOLS, regMLOols, regMTradesols, regMCollegeols, omit = "pr01", title = "Robustness Checks: OLS Effect of Manufacturing Decline on Employment Rate (2001-2016)")
