library(dplyr)
library(readstata13)
library(foreach)
library(haven)
library(graphics)

##2001 Census Data
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

#Males, High education
census01MH <- subset(census2001, SEXP == 2 & HLOSP >= 10 & HLOSP != 99)
census01MH <- subset(census01MH, AGEP >= 20 & AGEP <= 54)
#Males, Low education
census01ML <- subset(census2001, SEXP == 2 & HLOSP < 10)
census01ML <- subset(census01ML, AGEP >= 20 & AGEP <= 54)
#Females, High education
census01FH <- subset(census2001, SEXP == 1 & HLOSP >= 10 & HLOSP != 99)
census01FH <- subset(census01FH, AGEP >= 20 & AGEP <= 54)
#Females, Low education
census01FL <- subset(census2001, SEXP == 1 & HLOSP < 10)
census01ML <- subset(census01ML, AGEP >= 20 & AGEP <= 54)

##Stats ~ 2001
CMAList <- c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 205, 421, 462, 499, 505, 532, 535, 537, 539, 541, 555, 559, 602, 799, 825, 835, 933, 935)

he_shr <- vector()
fe_lfs <- vector()
for_bor <- vector()
avghrsMH <- vector()
avghrsML <- vector()
avghrsFH <- vector()
avghrsFL <- vector()
logwageMH <- vector()
logwageML <- vector()
logwageFH <- vector()
logwageFL <- vector()
erateMH <- vector()
erateML <- vector()
erateFH <- vector()
erateFL <- vector()
manu_shrMH <- vector()
manu_shrML <- vector()
manu_shrFH <- vector()
manu_shrFL <- vector()
manu_shr <- vector()
manu_shr_pop <- vector()

foreach(i = 1:29, j = CMAList) %do% {
  he_shr[i] <- nrow(subset(census2001, HLOSP >= 5 & CMAP == j)) / nrow(subset(census2001, CMAP == j)) 
  fe_lfs[i] <- nrow(subset(census2001, SEXP == 1 & LFACTP <= 10 & CMAP == j)) / nrow(subset(census2001, SEXP == 1 & CMAP == j))
  for_bor[i] <- nrow(subset(census2001, (IMMPOPP == 2 & CMAP == j) | (IMMPOPP == 3 & CMAP == j))) / nrow(subset(census2001, CMAP == j))
  avghrsMH[i] <- mean(subset(census01MH, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$loghrs)
  avghrsML[i] <- mean(subset(census01ML, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$loghrs)
  avghrsFH[i] <- mean(subset(census01FH, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$loghrs)
  avghrsFL[i] <- mean(subset(census01FL, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$loghrs)
  logwageMH[i] <- median(subset(census01MH, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  logwageML[i] <- median(subset(census01ML, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  logwageFH[i] <- median(subset(census01FH, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  logwageFL[i] <- median(subset(census01FL, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)$logwage)
  erateMH[i] <- nrow(subset(census01MH, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01MH, LFACTP <= 10 & CMAP == j))
  erateML[i] <- nrow(subset(census01ML, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01ML, LFACTP <= 10 & CMAP == j))
  erateFH[i] <- nrow(subset(census01FH, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01FH, LFACTP <= 10 & CMAP == j))
  erateFL[i] <- nrow(subset(census01FL, LFACTP <= 2 & FPTWKP == 1 & CMAP == j)) / nrow(subset(census01FL, LFACTP <= 10 & CMAP == j))
  manu_shrMH[i] <- nrow(subset(census01MH, LFACTP <= 2 & FPTWKP == 1 & CMAP == j & NAICSP == 5)) / nrow(subset(census01MH, LFACTP <= 2 & FPTWKP == 1 & CMAP == j))
  manu_shrML[i] <- nrow(subset(census01ML, LFACTP <= 2 & FPTWKP == 1 & CMAP == j & NAICSP == 5)) / nrow(subset(census01ML, LFACTP <= 2 & FPTWKP == 1 & CMAP == j))
  manu_shrFH[i] <- nrow(subset(census01FH, LFACTP <= 2 & FPTWKP == 1 &  CMAP == j & NAICSP == 5)) / nrow(subset(census01FH, LFACTP <= 2 & FPTWKP == 1 & CMAP == j))
  manu_shrFL[i] <- nrow(subset(census01FL, LFACTP <= 2 & FPTWKP == 1 & CMAP == j & NAICSP == 5)) / nrow(subset(census01FL, LFACTP <= 2 & FPTWKP == 1 & CMAP == j))
  manu_shr[i] <- nrow(subset(census2001, LFACTP <=2 & FPTWKP == 1 & CMAP == j & NAICSP == 5)) / nrow(subset(census2001, LFACTP <= 2 & FPTWKP == 1 & CMAP == j))
  manu_shr_pop[i] <- nrow(subset(census2001, LFACTP <= 2 & FPTWKP == 1 & CMAP == j & NAICSP == 5)) / nrow(subset(census2001, CMAP == j))
}

#Aggregate NOC Employment Shares
NOC_shr1 <- vector()
NOC_shr2 <- vector()
NOC_shr3 <- vector()
NOC_shr4 <- vector()
NOC_shr5 <- vector()
NOC_shr6 <- vector()
NOC_shr7 <- vector()
NOC_shr8 <- vector()
NOC_shr9 <- vector()
NOC_shr10 <- vector()
NOC_shr11 <- vector()
NOC_shr12 <- vector()
NOC_shr13 <- vector()
NOC_shr14 <- vector()
NOC_shr15 <- vector()
NOC_shr16 <- vector()
NOC_shr17 <- vector()

foreach(y = CMAList, v = 1:29) %do% { 
  NOC_shr1[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 1 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr2[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 2 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr3[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 3 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr4[v] <- nrow(subset(census2001, NAICSP == 5 & CMAP == y & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 4 | NOCS01P == 5))) / nrow(subset(census2001, CMAP == y))
  NOC_shr5[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 6 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr6[v] <- nrow(subset(census2001, NAICSP == 5 & CMAP == y & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 7 | NOCS01P == 8))) / nrow(subset(census2001, CMAP == y))
  NOC_shr7[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 9 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr8[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 10 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr9[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 11 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr10[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 12 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr11[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 13 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr12[v] <- nrow(subset(census2001, NAICSP == 5 & CMAP == y & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 14 | NOCS01P == 17))) / nrow(subset(census2001, CMAP == y))
  NOC_shr13[v] <- nrow(subset(census2001, NAICSP == 5 & CMAP == y & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 15 | NOCS01P == 16))) / nrow(subset(census2001, CMAP == y))
  NOC_shr14[v] <- nrow(subset(census2001, NAICSP == 5 & CMAP == y & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 18 | NOCS01P == 19 | NOCS01P == 20 | NOCS01P == 21))) / nrow(subset(census2001, CMAP == y))
  NOC_shr15[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 22 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr16[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 23 & CMAP == y & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP == y))
  NOC_shr17[v] <- nrow(subset(census2001, NAICSP == 5 & CMAP == y & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 24 | NOCS01P == 25))) / nrow(subset(census2001, CMAP == y))
  }

#Leave-out Manufacturing share
man_shrm1 <- vector()
man_shrm2 <- vector()
man_shrm3 <- vector()
man_shrm4 <- vector()
man_shrm5 <- vector()
man_shrm6 <- vector()
man_shrm7 <- vector()
man_shrm8 <- vector()
man_shrm9 <- vector()
man_shrm10 <- vector()
man_shrm11 <- vector()
man_shrm12 <- vector()
man_shrm13 <- vector()
man_shrm14 <- vector()
man_shrm15 <- vector()
man_shrm16 <- vector()
man_shrm17 <- vector()

foreach(x = CMAList, q = 1:29) %do% { 
  man_shrm1[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 1 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm2[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 2 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm3[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 3 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm4[q] <- nrow(subset(census2001, NAICSP == 5  & CMAP != x & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 4 | NOCS01P == 5))) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm5[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 6 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm6[q] <- nrow(subset(census2001, NAICSP == 5 & CMAP != x & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 7 | NOCS01P == 8))) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm7[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 9 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm8[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 10 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm9[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 11 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm10[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 12 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm11[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 13 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm12[q] <- nrow(subset(census2001, NAICSP == 5 & CMAP != x & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 14 | NOCS01P == 17))) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm13[q] <- nrow(subset(census2001, NAICSP == 5 & CMAP != x & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 15 | NOCS01P == 16))) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm14[q] <- nrow(subset(census2001, NAICSP == 5 & CMAP != x & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 18 | NOCS01P == 19 | NOCS01P == 20 | NOCS01P == 21))) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm15[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 22 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm16[q] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 23 & CMAP != x & LFACTP <= 2 & FPTWKP == 1)) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
  man_shrm17[q] <- nrow(subset(census2001, NAICSP == 5 & CMAP != x & LFACTP <= 2 & FPTWKP == 1 & (NOCS01P == 24 | NOCS01P == 25))) / nrow(subset(census2001, CMAP != x  & LFACTP <= 2 & FPTWKP == 1))
}

#Mechanism dependent variables

UICrate2039M <- vector()
UICrate2039F <- vector()
UICrate4054M <- vector()
UICrate4054F <- vector()

LFNPMH <- vector()
LFNPML <- vector()
LFNPFH <- vector()
LFNPFL <- vector()

foreach(x = CMAList, q = 1:29) %do%  {
  UICrate2039M[q] <- nrow(subset(census2001, CMAP == x & AGEP >= 20 & AGEP < 40 & SEXP == 2 & UICBNP != 0 & UICBNP != 9999999)) / nrow(subset(census2001, CMAP == x & AGEP >= 20 & AGEP < 40 & SEXP == 2))
  UICrate2039F[q] <- nrow(subset(census2001, CMAP == x & AGEP >= 20 & AGEP < 40 & SEXP == 1 & UICBNP != 0 & UICBNP != 9999999)) / nrow(subset(census2001, CMAP == x & AGEP >= 20 & AGEP < 40 & SEXP == 1))
  UICrate4054M[q] <- nrow(subset(census2001, CMAP == x & AGEP >= 40 & AGEP < 55 & SEXP == 2 & UICBNP != 0 & UICBNP != 9999999)) / nrow(subset(census2001, CMAP == x & AGEP >= 20 & AGEP < 40 & SEXP == 2))
  UICrate4054F[q] <- nrow(subset(census2001, CMAP == x & AGEP >= 40 & AGEP < 55 & SEXP == 1 & UICBNP != 0 & UICBNP != 9999999)) / nrow(subset(census2001, CMAP == x & AGEP >= 40 & AGEP < 55 & SEXP == 1))
  LFNPMH[q] <- nrow(subset(census01MH, CMAP == x & LFACTP >= 11 & LFACTP != 99)) / nrow(census01MH)
  LFNPML[q] <- nrow(subset(census01ML, CMAP == x & LFACTP >= 11 & LFACTP != 99)) / nrow(census01ML)
  LFNPFH[q] <- nrow(subset(census01FH, CMAP == x & LFACTP >= 11 & LFACTP != 99)) / nrow(census01FH)
  LFNPFL[q] <- nrow(subset(census01FL, CMAP == x & LFACTP >= 11 & LFACTP != 99)) / nrow(census01FL)
}

#Extra covariates for identification tests

maleshr <- vector() #Male share of employment
propval <- vector() #Median property value
commute <- vector() #Average commuting distance
selfemp <- vector() #share of self-employed workers
avgage <- vector() #Average age of workers

foreach(i = CMAList, j = 1:29) %do%  {
  maleshr[j] <- nrow(subset(census2001, CMAP == i & LFACTP <= 2 & SEXP == 2)) / nrow(subset(census2001, CMAP == i & LFACTP <= 2))
  propval[j] <- median(subset(census2001, CMAP == i & VALUEP != 999999 & VALUEP != 0)$VALUEP)
  commute[j] <- mean(subset(census2001, CMAP == i & DISTP != 9)$DISTP)
  selfemp[j] <- nrow(subset(census2001, CMAP == i & LFACTP <= 2 & COWP != 9 & COWP != 1)) / nrow(subset(census2001, CMAP == i & LFACTP <= 2))
  avgage[j] <- mean(subset(census2001, CMAP == i & LFACTP <= 2)$AGEP)
}

#Compile Dataframe (2001)

province <- c("NFL", "PEI", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "NS", "QC", "QC", "QC", "ON", "ON", "ON", "ON", "ON", "ON", "ON", "ON", "MB", "SK", "AB", "AB", "BC", "BC")
vec2001 <- c(2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001, 2001)

set_2001 <- data.frame(cbind(vec2001, CMAList, province, manu_shr, manu_shr_pop, he_shr, fe_lfs, for_bor, avghrsMH, avghrsML, avghrsFH, avghrsFL, logwageMH, logwageML, logwageFH, logwageFL, erateMH, erateML, erateFH, erateFL, manu_shrMH, manu_shrML, manu_shrFH, manu_shrFL, NOC_shr1, NOC_shr2, NOC_shr3, NOC_shr4, NOC_shr5, NOC_shr6, NOC_shr7, NOC_shr8, NOC_shr9, NOC_shr10, NOC_shr11, NOC_shr12, NOC_shr13, NOC_shr14, NOC_shr15, NOC_shr16, NOC_shr17, man_shrm1, man_shrm2, man_shrm3, man_shrm4, man_shrm5, man_shrm6, man_shrm7, man_shrm8, man_shrm9, man_shrm10, man_shrm11, man_shrm12, man_shrm13, man_shrm14, man_shrm15, man_shrm16, man_shrm17, UICrate2039M, UICrate2039F, UICrate4054M, UICrate4054F, LFNPMH, LFNPML, LFNPFH, LFNPFL, maleshr, propval, commute, selfemp, avgage))

##2006 Census Data
census2006 <- read.dta13("Census_2006.dta")
census2006 <- subset(census2006, select = c("PR", "CMA",  "NAICS", "NOCS", "LFACT", "HRSWRK", "WAGES", "SEX", "HDGREE", "IMMSTAT", "AGEGRP", "YRIMM", "VISMIN", "EICBN", "WKSWRK", "FPTWK", "VALUE", "DIST", "COW")) %>%
  mutate(loghrs = ifelse(HRSWRK*WKSWRK != 0, log(HRSWRK*WKSWRK), 0)) %>%
  mutate(logwage = ifelse(WAGES != 0, log(WAGES*0.896425297), 0)) %>%
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

census2006 <- subset(census2006, LFACT != 99) #Eliminate obs. with no wage recorded

#Males, High education
census06MH <- subset(census2006, SEX == 2 & HDGREE >= 8 & HDGREE != 88 & HDGREE != 99)
census06MH <- subset(census06MH, AGEGRP >= 8 & AGEGRP <= 14)
#Males, Low education
census06ML <- subset(census2006, SEX == 2 & HDGREE < 8)
census06ML <- subset(census06ML, AGEGRP >= 8 & AGEGRP <= 14)
#Females, High education
census06FH <- subset(census2006, SEX == 1 & HDGREE >= 8 & HDGREE != 88 & HDGREE != 99)
census06FH <- subset(census06FH, AGEGRP >= 8 & AGEGRP <= 14)
#Females, Low education
census06FL <- subset(census2006, SEX == 1 & HDGREE < 8)
census06FL <- subset(census06FL, AGEGRP >= 8 & AGEGRP <= 14)

##Stats ~ 2006
CMAList <- c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 205, 421, 462, 499, 505, 532, 535, 537, 539, 541, 555, 559, 602, 799, 825, 835, 933, 935)

he_shr <- vector()
fe_lfs <- vector()
for_bor <- vector()
avghrsMH <- vector()
avghrsML <- vector()
avghrsFH <- vector()
avghrsFL <- vector()
logwageMH <- vector()
logwageML <- vector()
logwageFH <- vector()
logwageFL <- vector()
erateMH <- vector()
erateML <- vector()
erateFH <- vector()
erateFL <- vector()
manu_shrMH <- vector()
manu_shrML <- vector()
manu_shrFH <- vector()
manu_shrFL <- vector()

foreach(i = 1:29, j = CMAList) %do% {
  he_shr[i] <- nrow(subset(census2006, HDGREE >= 3 & CMA == j)) / nrow(subset(census2006, CMA == j)) 
  fe_lfs[i] <- nrow(subset(census2006, SEX == 1 & LFACT <= 10 & CMA == j)) / nrow(subset(census2006, SEX == 1 & CMA == j))
  for_bor[i] <- nrow(subset(census2006, CMA == j & (IMMSTAT == 1 | IMMSTAT == 3))) / nrow(subset(census2006, CMA == j))
  avghrsMH[i] <- mean(subset(census06MH, LFACT <= 2 & FPTWK == 1 & CMA == j)$loghrs)
  avghrsML[i] <- mean(subset(census06ML, LFACT <= 2 & FPTWK == 1 & CMA == j)$loghrs)
  avghrsFH[i] <- mean(subset(census06FH, LFACT <= 2 & FPTWK == 1 & CMA == j)$loghrs)
  avghrsFL[i] <- mean(subset(census06FL, LFACT <= 2 & FPTWK == 1 & CMA == j)$loghrs)
  logwageMH[i] <- median(subset(census06MH, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageML[i] <- median(subset(census06ML, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFH[i] <- median(subset(census06FH, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFL[i] <- median(subset(census06FL, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  erateMH[i] <- nrow(subset(census06MH, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census06MH, LFACT <= 10 & CMA == j))
  erateML[i] <- nrow(subset(census06ML, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census06ML, LFACT <= 10 & CMA == j))
  erateFH[i] <- nrow(subset(census06FH, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census06FH, LFACT <= 10 & CMA == j))
  erateFL[i] <- nrow(subset(census06FL, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census06FL, LFACT <= 10 & CMA == j))
  manu_shrMH[i] <- nrow(subset(census06MH, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census06MH, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shrML[i] <- nrow(subset(census06ML, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census06ML, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shrFH[i] <- nrow(subset(census06FH, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census06FH, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shrFL[i] <- nrow(subset(census06FL, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census06FL, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shr[i] <- nrow(subset(census2006, LFACT <=2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census2006, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shr_pop[i] <- nrow(subset(census2006, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census2006, CMA == j))
}

#Aggregate NOC Employment Shares
NOC_shr1 <- vector()
NOC_shr2 <- vector()
NOC_shr3 <- vector()
NOC_shr4 <- vector()
NOC_shr5 <- vector()
NOC_shr6 <- vector()
NOC_shr7 <- vector()
NOC_shr8 <- vector()
NOC_shr9 <- vector()
NOC_shr10 <- vector()
NOC_shr11 <- vector()
NOC_shr12 <- vector()
NOC_shr13 <- vector()
NOC_shr14 <- vector()
NOC_shr15 <- vector()
NOC_shr16 <- vector()
NOC_shr17 <- vector()

foreach(y = CMAList, v = 1:29) %do% { 
  NOC_shr1[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 1 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr2[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 2 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr3[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 3 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr4[v] <- nrow(subset(census2006, NAICS == 5  & CMA == y & LFACT <= 2 & FPTWK == 1 & (NOCS == 4 | NOCS == 5))) / nrow(subset(census2006, CMA == y))
  NOC_shr5[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 6 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr6[v] <- nrow(subset(census2006, NAICS == 5 & CMA == y & LFACT <= 2 & FPTWK == 1 & (NOCS == 7 | NOCS == 8))) / nrow(subset(census2006, CMA == y))
  NOC_shr7[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 9 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr8[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 10 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr9[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 11 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr10[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 12 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr11[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 13 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr12[v] <- nrow(subset(census2006, NAICS == 5 & CMA == y & LFACT <= 2 & FPTWK == 1 & (NOCS == 14 | NOCS == 17))) / nrow(subset(census2006, CMA == y))
  NOC_shr13[v] <- nrow(subset(census2006, NAICS == 5 & CMA == y & LFACT <= 2 & FPTWK == 1 & (NOCS == 15 | NOCS == 16))) / nrow(subset(census2006, CMA == y))
  NOC_shr14[v] <- nrow(subset(census2006, NAICS == 5 & CMA == y & LFACT <= 2 & FPTWK == 1 & (NOCS == 18 | NOCS == 19 | NOCS == 20 | NOCS == 21))) / nrow(subset(census2006, CMA == y))
  NOC_shr15[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 22 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr16[v] <- nrow(subset(census2006, NAICS == 5 & NOCS == 23 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA == y))
  NOC_shr17[v] <- nrow(subset(census2006, NAICS == 5 & CMA == y & LFACT <= 2 & FPTWK == 1 & (NOCS == 24 | NOCS == 25))) / nrow(subset(census2006, CMA == y))
}

#Leave-out Manufacturing share
man_shrm1 <- vector()
man_shrm2 <- vector()
man_shrm3 <- vector()
man_shrm4 <- vector()
man_shrm5 <- vector()
man_shrm6 <- vector()
man_shrm7 <- vector()
man_shrm8 <- vector()
man_shrm9 <- vector()
man_shrm10 <- vector()
man_shrm11 <- vector()
man_shrm12 <- vector()
man_shrm13 <- vector()
man_shrm14 <- vector()
man_shrm15 <- vector()
man_shrm16 <- vector()
man_shrm17 <- vector()

foreach(x = CMAList, q = 1:29) %do% { 
  man_shrm1[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 1 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm2[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 2 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm3[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 3 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm4[q] <- nrow(subset(census2006, NAICS == 5 & CMA != x & LFACT <= 2 & FPTWK == 1 & (NOCS == 4 | NOCS == 5))) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm5[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 6 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm6[q] <- nrow(subset(census2006, NAICS == 5 & CMA != x & LFACT <= 2 & FPTWK == 1 & (NOCS == 7 | NOCS == 8))) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm7[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 9 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm8[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 10 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm9[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 11 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm10[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 12 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm11[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 13 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm12[q] <- nrow(subset(census2006, NAICS == 5  & CMA != x & LFACT <= 2 & FPTWK == 1 & (NOCS == 14 | NOCS == 17))) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm13[q] <- nrow(subset(census2006, NAICS == 5 & CMA != x & LFACT <= 2 & FPTWK == 1 & (NOCS == 15 | NOCS == 16))) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm14[q] <- nrow(subset(census2006, NAICS == 5 & CMA != x & LFACT <= 2 & FPTWK == 1 & (NOCS == 18 | NOCS == 19 | NOCS == 20 | NOCS == 21))) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm15[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 22 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm16[q] <- nrow(subset(census2006, NAICS == 5 & NOCS == 23 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm17[q] <- nrow(subset(census2006, NAICS == 5 & CMA != x & LFACT <= 2 & FPTWK == 1 & (NOCS == 24 | NOCS == 25))) / nrow(subset(census2006, CMA != x  & LFACT <= 2 & FPTWK == 1))
}

#Mechanism dependent variables

UICrate2039M <- vector()
UICrate2039F <- vector()
UICrate4054M <- vector()
UICrate4054F <- vector()

LFNPMH <- vector()
LFNPML <- vector()
LFNPFH <- vector()
LFNPFL <- vector()

foreach(x = CMAList, q = 1:29) %do%  {
  UICrate2039M[q] <- nrow(subset(census2006, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & SEX == 2 & EICBN != 0 & EICBN != 9999999)) / nrow(subset(census2006, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & SEX == 2))
  UICrate2039F[q] <- nrow(subset(census2006, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & SEX == 1 & EICBN != 0 & EICBN != 9999999)) / nrow(subset(census2006, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & SEX == 1))
  UICrate4054M[q] <- nrow(subset(census2006, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & SEX == 2 & EICBN != 0 & EICBN != 9999999)) / nrow(subset(census2006, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & SEX == 2))
  UICrate4054F[q] <- nrow(subset(census2006, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & SEX == 1 & EICBN != 0 & EICBN != 9999999)) / nrow(subset(census2006, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & SEX == 1))
  LFNPMH[q] <- nrow(subset(census06MH, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census06MH)
  LFNPML[q] <- nrow(subset(census06ML, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census06ML)
  LFNPFH[q] <- nrow(subset(census06FH, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census06FH)
  LFNPFL[q] <- nrow(subset(census06FL, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census06FL)
}

#Extra covariates for identification tests

maleshr <- vector() #Male share of employment
propval <- vector() #Median property value
commute <- vector() #Average commuting distance
selfemp <- vector() #share of self-employed workers
avgage <- vector() #Average age of workers

foreach(i = CMAList, j = 1:29) %do%  {
  maleshr[j] <- nrow(subset(census2006, CMA == i & LFACT <= 2 & SEX == 2)) / nrow(subset(census2006, CMA == i & LFACT <= 2))
  propval[j] <- median(subset(census2006, CMA == i & VALUE != 9999999 & VALUE != 0)$VALUE)
  commute[j] <- mean(subset(census2006, CMA == i & DIST != 9)$DIST)
  selfemp[j] <- nrow(subset(census2006, CMA == i & LFACT <= 2 & (COW == 2 | COW == 3 | COW == 5 | COW == 6))) / nrow(subset(census2006, CMA == i & LFACT <= 2))
  avgage[j] <- mean(subset(census2006, CMA == i & LFACT <= 2)$AGEGRP)
}

#Compile Dataframe (2006)

province <- c("NFL", "PEI", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "NS", "QC", "QC", "QC", "ON", "ON", "ON", "ON", "ON", "ON", "ON", "ON", "MB", "SK", "AB", "AB", "BC", "BC")
vec2006 <- c(2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006, 2006)

set_2006 <- data.frame(cbind(vec2006, CMAList, province, manu_shr, manu_shr_pop, he_shr, fe_lfs, for_bor, avghrsMH, avghrsML, avghrsFH, avghrsFL, logwageMH, logwageML, logwageFH, logwageFL, erateMH, erateML, erateFH, erateFL, manu_shrMH, manu_shrML, manu_shrFH, manu_shrFL, NOC_shr1, NOC_shr2, NOC_shr3, NOC_shr4, NOC_shr5, NOC_shr6, NOC_shr7, NOC_shr8, NOC_shr9, NOC_shr10, NOC_shr11, NOC_shr12, NOC_shr13, NOC_shr14, NOC_shr15, NOC_shr16, NOC_shr17, man_shrm1, man_shrm2, man_shrm3, man_shrm4, man_shrm5, man_shrm6, man_shrm7, man_shrm8, man_shrm9, man_shrm10, man_shrm11, man_shrm12, man_shrm13, man_shrm14, man_shrm15, man_shrm16, man_shrm17,  UICrate2039M, UICrate2039F, UICrate4054M, UICrate4054F, LFNPMH, LFNPML, LFNPFH, LFNPFL, maleshr, propval, commute, selfemp, avgage))

##2011 NHS Data (HOURS WORKED NA)
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
census11MH <- subset(census2011, SEX == 2 & HDGREE >= 8 & HDGREE != 88 & HDGREE != 99)
census11MH <- subset(census11MH, AGEGRP >= 8 & AGEGRP <= 14)
#Males, Low education
census11ML <- subset(census2011, SEX == 2 & HDGREE < 8)
census11ML <- subset(census11ML, AGEGRP >= 8 & AGEGRP <= 14)
#Females, High education
census11FH <- subset(census2011, SEX == 1 & HDGREE >= 8 & HDGREE != 88 & HDGREE != 99)
census11FH <- subset(census11FH, AGEGRP >= 8 & AGEGRP <= 14)
#Females, Low education
census11FL <- subset(census2011, SEX == 1 & HDGREE < 8)
census11FL <- subset(census11FL, AGEGRP >= 8 & AGEGRP <= 14)

##Stats ~ 2011
CMAList <- c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 205, 421, 462, 499, 505, 532, 535, 537, 539, 541, 555, 559, 602, 799, 825, 835, 933, 935)

he_shr <- vector()
fe_lfs <- vector()
for_bor <- vector()
logwageMH <- vector()
logwageML <- vector()
logwageFH <- vector()
logwageFL <- vector()
erateMH <- vector()
erateML <- vector()
erateFH <- vector()
erateFL <- vector()
manu_shrMH <- vector()
manu_shrML <- vector()
manu_shrFH <- vector()
manu_shrFL <- vector()

foreach(i = 1:29, j = CMAList) %do% {
  he_shr[i] <- nrow(subset(census2011, HDGREE >= 3 & CMA == j)) / nrow(subset(census2011, CMA == j)) 
  fe_lfs[i] <- nrow(subset(census2011, SEX == 1 & LFACT <= 10 & CMA == j)) / nrow(subset(census2011, SEX == 1 & CMA == j))
  for_bor[i] <- nrow(subset(census2011, CMA == j & (IMMSTAT == 2 | IMMSTAT == 3))) / nrow(subset(census2011, CMA == j))
  logwageMH[i] <- median(subset(census11MH, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageML[i] <- median(subset(census11ML, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFH[i] <- median(subset(census11FH, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFL[i] <- median(subset(census11FL, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  erateMH[i] <- nrow(subset(census11MH, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11MH, LFACT <= 10 & CMA == j))
  erateML[i] <- nrow(subset(census11ML, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11ML, LFACT <= 10 & CMA == j))
  erateFH[i] <- nrow(subset(census11FH, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11FH, LFACT <= 10 & CMA == j))
  erateFL[i] <- nrow(subset(census11FL, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census11FL, LFACT <= 10 & CMA == j))
  manu_shrMH[i] <- nrow(subset(census11MH, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 31)) / nrow(subset(census11MH, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shrML[i] <- nrow(subset(census11ML, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 31)) / nrow(subset(census11ML, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shrFH[i] <- nrow(subset(census11FH, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 31)) / nrow(subset(census11FH, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shrFL[i] <- nrow(subset(census11FL, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 31)) / nrow(subset(census11FL, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shr[i] <- nrow(subset(census2011, LFACT <=2 & FPTWK == 1 & CMA == j & NAICS == 31)) / nrow(subset(census2011, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shr_pop[i] <- nrow(subset(census2011, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 31)) / nrow(subset(census2011, CMA == j))
}

#Aggregate NOC Employment Shares
NOC_shr1 <- vector()
NOC_shr2 <- vector()
NOC_shr3 <- vector()
NOC_shr4 <- vector()
NOC_shr5 <- vector()
NOC_shr6 <- vector()
NOC_shr7 <- vector()
NOC_shr8 <- vector()
NOC_shr9 <- vector()
NOC_shr10 <- vector()
NOC_shr11 <- vector()
NOC_shr12 <- vector()
NOC_shr13 <- vector()
NOC_shr14 <- vector()
NOC_shr15 <- vector()
NOC_shr16 <- vector()
NOC_shr17 <- vector()

foreach(y = CMAList, v = 1:29) %do% { 
  NOC_shr1[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 1)) / nrow(subset(census2011, CMA == y))
  NOC_shr2[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 2 | NOC11 == 3 | NOC11 == 4))) / nrow(subset(census2011, CMA == y))
  NOC_shr3[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 5)) / nrow(subset(census2011, CMA == y))
  NOC_shr4[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 6 | NOC11 == 7 | NOC11 == 8))) / nrow(subset(census2011, CMA == y))
  NOC_shr5[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 9 | NOC11 == 10))) / nrow(subset(census2011, CMA == y))
  NOC_shr6[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 11 |NOC11 == 12))) / nrow(subset(census2011, CMA == y))
  NOC_shr7[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 14 | NOC11 == 15))) / nrow(subset(census2011, CMA == y))
  NOC_shr8[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 13)) / nrow(subset(census2011, CMA == y))
  NOC_shr9[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 17)) / nrow(subset(census2011, CMA == y))
  NOC_shr10[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 18)) / nrow(subset(census2011, CMA == y))
  NOC_shr11[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 20)) / nrow(subset(census2011, CMA == y))
  NOC_shr12[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 21 | NOC11 == 19 | NOC11 == 23 | NOC11 == 22))) / nrow(subset(census2011, CMA == y))
  NOC_shr13[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 16)) / nrow(subset(census2011, CMA == y))
  NOC_shr14[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 24 | NOC11 == 25 | NOC11 == 27))) / nrow(subset(census2011, CMA == y))
  NOC_shr15[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 26)) / nrow(subset(census2011, CMA == y))
  NOC_shr16[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 28)) / nrow(subset(census2011, CMA == y))
  NOC_shr17[v] <- nrow(subset(census2011, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 29 | NOC11 == 30))) / nrow(subset(census2011, CMA == y))
}

#Leave-out Manufacturing share
man_shrm1 <- vector()
man_shrm2 <- vector()
man_shrm3 <- vector()
man_shrm4 <- vector()
man_shrm5 <- vector()
man_shrm6 <- vector()
man_shrm7 <- vector()
man_shrm8 <- vector()
man_shrm9 <- vector()
man_shrm10 <- vector()
man_shrm11 <- vector()
man_shrm12 <- vector()
man_shrm13 <- vector()
man_shrm14 <- vector()
man_shrm15 <- vector()
man_shrm16 <- vector()
man_shrm17 <- vector()

foreach(x = CMAList, q = 1:29) %do% { 
  man_shrm1[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 1)) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm2[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 2 | NOC11 == 3 | NOC11 == 4))) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm3[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 5)) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm4[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 6 | NOC11 == 7 | NOC11 == 8))) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm5[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 9 | NOC11 == 10))) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm6[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 11 |NOC11 == 12))) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm7[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 14 | NOC11 == 15))) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm8[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 13)) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm9[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 17)) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm10[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 18)) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm11[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 20)) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm12[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 21 | NOC11 == 19 | NOC11 == 23 | NOC11 == 22))) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm13[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 16)) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm14[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 24 | NOC11 == 25 | NOC11 == 27))) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm15[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 26)) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm16[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & NOC11 == 28)) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
  man_shrm17[q] <- nrow(subset(census2011, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 31 & (NOC11 == 29 | NOC11 == 30))) / nrow(subset(census2011, CMA != x  & LFACT <= 2))
}

#Mechanism dependent variables

UICrate2039M <- vector()
UICrate2039F <- vector()
UICrate4054M <- vector()
UICrate4054F <- vector()

LFNPMH <- vector()
LFNPML <- vector()
LFNPFH <- vector()
LFNPFL <- vector()

foreach(x = CMAList, q = 1:29) %do%  {
  UICrate2039M[q] <- nrow(subset(census2011, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & SEX == 2 & EICBN != 0 & EICBN != 9999999)) / nrow(subset(census2011, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & SEX == 2))
  UICrate2039F[q] <- nrow(subset(census2011, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & SEX == 1 & EICBN != 0 & EICBN != 9999999)) / nrow(subset(census2011, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & SEX == 1))
  UICrate4054M[q] <- nrow(subset(census2011, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & SEX == 2 & EICBN != 0 & EICBN != 9999999)) / nrow(subset(census2011, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & SEX == 2))
  UICrate4054F[q] <- nrow(subset(census2011, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & SEX == 1 & EICBN != 0 & EICBN != 9999999)) / nrow(subset(census2011, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & SEX == 1))
  LFNPMH[q] <- nrow(subset(census11MH, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census11MH)
  LFNPML[q] <- nrow(subset(census11ML, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census11ML)
  LFNPFH[q] <- nrow(subset(census11FH, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census11FH)
  LFNPFL[q] <- nrow(subset(census11FL, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census11FL)
}

#Extra covariates for identification tests

maleshr <- vector() #Male share of employment
propval <- vector() #Median property value
commute <- vector() #Average commuting distance
selfemp <- vector() #share of self-employed workers
avgage <- vector() #Average age of workers

foreach(i = CMAList, j = 1:29) %do%  {
  maleshr[j] <- nrow(subset(census2011, CMA == i & LFACT <= 2 & SEX == 2)) / nrow(subset(census2011, CMA == i & LFACT <= 2))
  propval[j] <- median(subset(census2011, CMA == i & VALUE != 9999999 & VALUE != 0)$VALUE)
  commute[j] <- mean(subset(census2011, CMA == i & DIST != 9)$DIST)
  selfemp[j] <- nrow(subset(census2011, CMA == i & LFACT <= 2 & (COW == 3 | COW == 4 | COW == 5 | COW == 6))) / nrow(subset(census2011, CMA == i & LFACT <= 2))
  avgage[j] <- mean(subset(census2011, CMA == i & LFACT <= 2)$AGEGRP)
}

#Compile Dataframe (2011)

province <- c("NFL", "PEI", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "NS", "QC", "QC", "QC", "ON", "ON", "ON", "ON", "ON", "ON", "ON", "ON", "MB", "SK", "AB", "AB", "BC", "BC")

vec2011 <- c(2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011, 2011)

set_2011 <- data.frame(cbind(vec2011, CMAList, province, manu_shr, manu_shr_pop, he_shr, fe_lfs, for_bor, avghrsMH, avghrsML, avghrsFH, avghrsFL, logwageMH, logwageML, logwageFH, logwageFL, erateMH, erateML, erateFH, erateFL, manu_shrMH, manu_shrML, manu_shrFH, manu_shrFL, NOC_shr1, NOC_shr2, NOC_shr3, NOC_shr4, NOC_shr5, NOC_shr6, NOC_shr7, NOC_shr8, NOC_shr9, NOC_shr10, NOC_shr11, NOC_shr12, NOC_shr13, NOC_shr14, NOC_shr15, NOC_shr16, NOC_shr17, man_shrm1, man_shrm2, man_shrm3, man_shrm4, man_shrm5, man_shrm6, man_shrm7, man_shrm8, man_shrm9, man_shrm10, man_shrm11, man_shrm12, man_shrm13, man_shrm14, man_shrm15, man_shrm16, man_shrm17, UICrate2039M, UICrate2039F, UICrate4054M, UICrate4054F, LFNPMH, LFNPML, LFNPFH, LFNPFL, maleshr, propval, commute, selfemp, avgage))

##2016 Census Data (FIGURE OUT HOURS WORKED WEEKS)
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

# mutate(loghrs = ifelse(HRSWRK*WKSWRK != 0, log(HRSWRK*WKSWRK), 0)) %>%

census2016 <- subset(census2016, LFACT != 99) #Eliminate obs. with no wage recorded

#Males, High education
census16MH <- subset(census2016, Sex == 2 & HDGREE >= 8 & HDGREE != 88 & HDGREE != 99)
census16MH <- subset(census16MH, AGEGRP >= 8 & AGEGRP <= 14)
#Males, Low education
census16ML <- subset(census2016, Sex == 2 & HDGREE < 8)
census16ML <- subset(census16ML, AGEGRP >= 8 & AGEGRP <= 14)
#Females, High education
census16FH <- subset(census2016, Sex == 1 & HDGREE >= 8 & HDGREE != 88 & HDGREE != 99)
census16FH <- subset(census16FH, AGEGRP >= 8 & AGEGRP <= 14)
#Females, Low education
census16FL <- subset(census2016, Sex == 1 & HDGREE < 8)
census16FL <- subset(census16FL, AGEGRP >= 8 & AGEGRP <= 14)

##Stats ~ 2016
CMAList <- c(9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 205, 421, 462, 499, 505, 532, 535, 537, 539, 541, 555, 559, 602, 799, 825, 835, 933, 935)

he_shr <- vector()
fe_lfs <- vector()
for_bor <- vector()
avghrsMH <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
avghrsML <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
avghrsFH <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
avghrsFL <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
alogwageMH <- vector()
logwageML <- vector()
logwageFH <- vector()
logwageFL <- vector()
erateMH <- vector()
erateML <- vector()
erateFH <- vector()
erateFL <- vector()
manu_shrMH <- vector()
manu_shrML <- vector()
manu_shrFH <- vector()
manu_shrFL <- vector()
manu_shr <- vector()

foreach(i = 1:29, j = CMAList) %do% {
  he_shr[i] <- nrow(subset(census2016, HDGREE >= 3 & CMA == j)) / nrow(subset(census2016, CMA == j)) 
  fe_lfs[i] <- nrow(subset(census2016, Sex == 1 & LFACT <= 10 & CMA == j)) / nrow(subset(census2016, Sex == 1 & CMA == j))
  for_bor[i] <- nrow(subset(census2016, CMA == j & (IMMSTAT == 2 | IMMSTAT == 3))) / nrow(subset(census2016, CMA == j))
  #avghrsMH[i] <- mean(subset(census16MH, LFACT <= 2 & CMA == j)$loghrs)
  #avghrsML[i] <- mean(subset(census16ML, LFACT <= 2 & CMA == j)$loghrs)
  #avghrsFH[i] <- mean(subset(census16FH, LFACT <= 2 & CMA == j)$loghrs)
  #avghrsFL[i] <- mean(subset(census16FL, LFACT <= 2 & CMA == j)$loghrs)
  logwageMH[i] <- median(subset(census16MH, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageML[i] <- median(subset(census16ML, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFH[i] <- median(subset(census16FH, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  logwageFL[i] <- median(subset(census16FL, LFACT <= 2 & FPTWK == 1 & CMA == j)$logwage)
  erateMH[i] <- nrow(subset(census16MH, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16MH, LFACT <= 10 & CMA == j))
  erateML[i] <- nrow(subset(census16ML, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16ML, LFACT <= 10 & CMA == j))
  erateFH[i] <- nrow(subset(census16FH, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16FH, LFACT <= 10 & CMA == j))
  erateFL[i] <- nrow(subset(census16FL, LFACT <= 2 & FPTWK == 1 & CMA == j)) / nrow(subset(census16FL, LFACT <= 10 & CMA == j))
  manu_shrMH[i] <- nrow(subset(census16MH, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census16MH, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shrML[i] <- nrow(subset(census16ML, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census16ML, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shrFH[i] <- nrow(subset(census16FH, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census16FH, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shrFL[i] <- nrow(subset(census16FL, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census16FL, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shr[i] <- nrow(subset(census2016, LFACT <=2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census2016, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shr[i] <- nrow(subset(census2016, LFACT <=2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census2016, LFACT <= 2 & FPTWK == 1 & CMA == j))
  manu_shr_pop[i] <- nrow(subset(census2016, LFACT <= 2 & FPTWK == 1 & CMA == j & NAICS == 5)) / nrow(subset(census2016, CMA == j))
}

#Aggregate NOC Employment Shares
NOC_shr1 <- vector()
NOC_shr2 <- vector()
NOC_shr3 <- vector()
NOC_shr4 <- vector()
NOC_shr5 <- vector()
NOC_shr6 <- vector()
NOC_shr7 <- vector()
NOC_shr8 <- vector()
NOC_shr9 <- vector()
NOC_shr10 <- vector()
NOC_shr11 <- vector()
NOC_shr12 <- vector()
NOC_shr13 <- vector()
NOC_shr14 <- vector()
NOC_shr15 <- vector()
NOC_shr16 <- vector()
NOC_shr17 <- vector()

foreach(y = CMAList, v = 1:29) %do% { 
  NOC_shr1[v] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 1 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA == y))
  NOC_shr2[v] <- nrow(subset(census2016, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 2 | NOC16 == 3 | NOC16 == 4))) / nrow(subset(census2016, CMA == y))
  NOC_shr3[v] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 5 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA == y))
  NOC_shr4[v] <- nrow(subset(census2016, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 6 | NOC16 == 7 | NOC16 == 8))) / nrow(subset(census2016, CMA == y))
  NOC_shr5[v] <- nrow(subset(census2016, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 9 | NOC16 == 10))) / nrow(subset(census2016, CMA == y))
  NOC_shr6[v] <- nrow(subset(census2016, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 11 |NOC16 == 12))) / nrow(subset(census2016, CMA == y))
  NOC_shr7[v] <- nrow(subset(census2016, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 14 | NOC16 == 15))) / nrow(subset(census2016, CMA == y))
  NOC_shr8[v] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 13 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA == y))
  NOC_shr9[v] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 17 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA == y))
  NOC_shr10[v] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 18 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA == y))
  NOC_shr11[v] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 20 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA == y))
  NOC_shr12[v] <- nrow(subset(census2016, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 21 | NOC16 == 19 | NOC16 == 23 | NOC16 == 22))) / nrow(subset(census2016, CMA == y))
  NOC_shr13[v] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 16 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA == y))
  NOC_shr14[v] <- nrow(subset(census2016, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 24 | NOC16 == 25 | NOC16 == 27))) / nrow(subset(census2016, CMA == y))
  NOC_shr15[v] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 26 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA == y))
  NOC_shr16[v] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 28 & CMA == y & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA == y))
  NOC_shr17[v] <- nrow(subset(census2016, CMA == y & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 29 | NOC16 == 30))) / nrow(subset(census2016, CMA == y))
}

#Leave-out Manufacturing share
man_shrm1 <- vector()
man_shrm2 <- vector()
man_shrm3 <- vector()
man_shrm4 <- vector()
man_shrm5 <- vector()
man_shrm6 <- vector()
man_shrm7 <- vector()
man_shrm8 <- vector()
man_shrm9 <- vector()
man_shrm10 <- vector()
man_shrm11 <- vector()
man_shrm12 <- vector()
man_shrm13 <- vector()
man_shrm14 <- vector()
man_shrm15 <- vector()
man_shrm16 <- vector()
man_shrm17 <- vector()

foreach(x = CMAList, q = 1:29) %do% { 
  man_shrm1[q] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 1 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm2[q] <- nrow(subset(census2016, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 2 | NOC16 == 3 | NOC16 == 4))) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm3[q] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 5 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm4[q] <- nrow(subset(census2016, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 6 | NOC16 == 7 | NOC16 == 8))) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm5[q] <- nrow(subset(census2016, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 9 | NOC16 == 10))) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm6[q] <- nrow(subset(census2016, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 11 |NOC16 == 12))) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm7[q] <- nrow(subset(census2016, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 14 | NOC16 == 15))) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm8[q] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 13 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm9[q] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 17 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm10[q] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 18 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm11[q] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 20 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm12[q] <- nrow(subset(census2016, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 21 | NOC16 == 19 | NOC16 == 23 | NOC16 == 22))) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm13[q] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 16 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA != x  & LFACT <= 2))
  man_shrm14[q] <- nrow(subset(census2016, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 24 | NOC16 == 25 | NOC16 == 27))) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm15[q] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 26 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm16[q] <- nrow(subset(census2016, NAICS == 5 & NOC16 == 28 & CMA != x & LFACT <= 2 & FPTWK == 1)) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
  man_shrm17[q] <- nrow(subset(census2016, CMA != x & LFACT <= 2 & FPTWK == 1 & NAICS == 5 & (NOC16 == 29 | NOC16 == 30))) / nrow(subset(census2016, CMA != x  & LFACT <= 2 & FPTWK == 1))
}

#Mechanism dependent variables

UICrate2039M <- vector()
UICrate2039F <- vector()
UICrate4054M <- vector()
UICrate4054F <- vector()

LFNPMH <- vector()
LFNPML <- vector()
LFNPFH <- vector()
LFNPFL <- vector()

foreach(x = CMAList, q = 1:29) %do%  {
  UICrate2039M[q] <- nrow(subset(census2016, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & Sex == 2 & EICBN != 0 & EICBN != 99999999)) / nrow(subset(census2016, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & Sex == 2))
  UICrate2039F[q] <- nrow(subset(census2016, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & Sex == 1 & EICBN != 0 & EICBN != 99999999)) / nrow(subset(census2016, CMA == x & AGEGRP >= 8 & AGEGRP < 12 & Sex == 1))
  UICrate4054M[q] <- nrow(subset(census2016, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & Sex == 2 & EICBN != 0 & EICBN != 99999999)) / nrow(subset(census2016, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & Sex == 2))
  UICrate4054F[q] <- nrow(subset(census2016, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & Sex == 1 & EICBN != 0 & EICBN != 99999999)) / nrow(subset(census2016, CMA == x & AGEGRP >= 12 & AGEGRP < 15 & Sex == 1))
  LFNPMH[q] <- nrow(subset(census16MH, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census16MH)
  LFNPML[q] <- nrow(subset(census16ML, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census16ML)
  LFNPFH[q] <- nrow(subset(census16FH, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census16FH)
  LFNPFL[q] <- nrow(subset(census16FL, CMA == x & LFACT >= 11 & LFACT != 99)) / nrow(census16FL)
}

#Extra covariates for identification tests

maleshr <- vector() #Male share of employment
propval <- vector() #Median property value
commute <- vector() #Average commuting distance
selfemp <- vector() #share of self-employed workers
avgage <- vector() #Average age of workers

foreach(i = CMAList, j = 1:29) %do%  {
  maleshr[j] <- nrow(subset(census2016, CMA == i & LFACT <= 2 & Sex == 2)) / nrow(subset(census2016, CMA == i & LFACT <= 2))
  propval[j] <- median(subset(census2016, CMA == i & VALUE != 9999999 & VALUE != 0)$VALUE)
  commute[j] <- mean(subset(census2016, CMA == i & DIST != 9)$DIST)
  selfemp[j] <- nrow(subset(census2016, CMA == i & LFACT <= 2 & (COW == 3 | COW == 4 | COW == 5 | COW == 6))) / nrow(subset(census2016, CMA == i & LFACT <= 2))
  avgage[j] <- mean(subset(census2016, CMA == i & LFACT <= 2)$AGEGRP)
}

#Compile Dataframe (2016)

province <- c("NFL", "PEI", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "NS", "QC", "QC", "QC", "ON", "ON", "ON", "ON", "ON", "ON", "ON", "ON", "MB", "SK", "AB", "AB", "BC", "BC")

vec2016 <- c(2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016)

set_2016 <- data.frame(cbind(vec2016, CMAList, province, manu_shr, manu_shr_pop, he_shr, fe_lfs, for_bor, avghrsMH, avghrsML, avghrsFH, avghrsFL, logwageMH, logwageML, logwageFH, logwageFL, erateMH, erateML, erateFH, erateFL, manu_shrMH, manu_shrML, manu_shrFH, manu_shrFL, NOC_shr1, NOC_shr2, NOC_shr3, NOC_shr4, NOC_shr5, NOC_shr6, NOC_shr7, NOC_shr8, NOC_shr9, NOC_shr10, NOC_shr11, NOC_shr12, NOC_shr13, NOC_shr14, NOC_shr15, NOC_shr16, NOC_shr17, man_shrm1, man_shrm2, man_shrm3, man_shrm4, man_shrm5, man_shrm6, man_shrm7, man_shrm8, man_shrm9, man_shrm10, man_shrm11, man_shrm12, man_shrm13, man_shrm14, man_shrm15, man_shrm16, man_shrm17,  UICrate2039M, UICrate2039F, UICrate4054M, UICrate4054F, LFNPMH, LFNPML, LFNPFH, LFNPFL, maleshr, propval, commute, selfemp, avgage))

#Compile Panel Data Set

colnames(set_2001)[1] <- "Year"
colnames(set_2006)[1] <- "Year"
colnames(set_2011)[1] <- "Year"
colnames(set_2016)[1] <- "Year"

CMA_Panel <- data.frame(rbind(set_2001, set_2006, set_2011, set_2016))
CMA_Panel <- CMA_Panel[order(CMA_Panel$CMAList, CMA_Panel$Year),]

#Export to .dta
write_dta(CMA_Panel, "CMA_panel.dta")

##Extra: Pie Graphs
indcount1 <- vector()
indcount2 <- vector()
indcount3 <- vector()
indcount4 <- vector()
indcount5 <- vector()
indcount6 <- vector()
indcount7 <- vector()
indcount8 <- vector()
indcount9 <- vector()
indcount10 <- vector()
indcount11 <- vector()
indcount12 <- vector()
indcount13 <- vector()
indcount14 <- vector()
indcount15 <- vector()
indcount16 <- vector()
indcount17 <- vector()

foreach(v = 1:18, y = CMAList) %do%  {
  indcount1[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 1 & CMAP == y & LFACTP <= 2))
  indcount2[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 2 & CMAP == y & LFACTP <= 2))
  indcount3[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 3 & CMAP == y & LFACTP <= 2))
  indcount4[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 4 | NOCS01P == 5 & CMAP == y & LFACTP <= 2))
  indcount5[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 6 & CMAP == y & LFACTP <= 2))
  indcount6[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 7 | NOCS01P == 8 & CMAP == y & LFACTP <= 2))
  indcount7[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 9 & CMAP == y & LFACTP <= 2))
  indcount8[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 10 & CMAP == y & LFACTP <= 2))
  indcount9[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 11 & CMAP == y & LFACTP <= 2))
  indcount10[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 12 & CMAP == y & LFACTP <= 2))
  indcount11[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 13 & CMAP == y & LFACTP <= 2))
  indcount12[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 14 | NOCS01P == 17 & CMAP == y & LFACTP <= 2))
  indcount13[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 15 | NOCS01P == 16 & CMAP == y & LFACTP <= 2))
  indcount14[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 18 | NOCS01P == 19 | NOCS01P == 20 | NOCS01P == 21 & CMAP == y & LFACTP <= 2))
  indcount15[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 22 & CMAP == y & LFACTP <= 2))
  indcount16[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 23 & CMAP == y & LFACTP <= 2))
  indcount17[v] <- nrow(subset(census2001, NAICSP == 5 & NOCS01P == 24 | NOCS01P == 25 & CMAP == y & LFACTP <= 2))
}

mancountdf <- data.frame(cbind(CMAList, indcount1, indcount2, indcount3, indcount4, indcount5, indcount6, indcount7, indcount8, indcount9, indcount10, indcount11, indcount12, indcount13, indcount14, indcount15, indcount16, indcount17))
#Toronto pie
torvec <- subset(mancountdf, CMAList == 535)
torvec <- c(188, 965, 183, 9599, 979, 1209, 55, 3, 109, 241, 89, 4922, 971, 37304, 283, 9, 21024)
lbls <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)", "(13)", "(14)", "(15)", "(16)", "(17)")
pct <- round(torvec/sum(torvec)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(torvec, main = "Occupations of Manufacturing Industry, Toronto (2001)", labels = lbls, cex = 0.6)

#Windsor pie
windvec <- subset(mancountdf, CMAList == 559)
windvec <- c(12, 63, 17, 1962, 94, 141, 2, 1, 7, 11, 5, 582, 142, 35603, 30, 0, 20003)
lbls <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)", "(13)", "(14)", "(15)", "(16)", "(17)")
pct <- round(windvec/sum(windvec)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(windvec, main = "Occupations of Manufacturing Industry, Windsor (2001)", labels = lbls, cex = 0.6)
