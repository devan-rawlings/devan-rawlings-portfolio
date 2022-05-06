#! datasetCompiler.R
## This script takes in several public-use CPS files (Monthly, January 2020 -- December 2021), as well as OxCGRT US data 
## (available from https://github.com/OxCGRT/covid-policy-tracker) and state aggregates scraped from US CPS and JOLTS aggregate tabulations
## (State Aggregate.xlsx, available in this repo). It cleans all data and constructs a state-by-month panel dataset for the paper's analysis.

library(tidyverse)
library(foreach)
library(base) # For get()

## READ IN DATA

#CPS PUMFs (24 months, Jan. 2020 - Dec. 2021)
cps_01_2020 <- read.csv("CPS/jan20pub.csv")
cps_02_2020 <- read.csv("CPS/feb20pub.csv")
cps_03_2020 <- read.csv("CPS/mar20pub.csv")
cps_04_2020 <- read.csv("CPS/apr20pub.csv")
cps_05_2020 <- read.csv("CPS/may20pub.csv")
cps_06_2020 <- read.csv("CPS/jun20pub.csv")
cps_07_2020 <- read.csv("CPS/jul20pub.csv")
cps_08_2020 <- read.csv("CPS/aug20pub.csv")
cps_09_2020 <- read.csv("CPS/sep20pub.csv")
cps_10_2020 <- read.csv("CPS/oct20pub.csv")
cps_11_2020 <- read.csv("CPS/nov20pub.csv")
cps_12_2020 <- read.csv("CPS/dec20pub.csv")
cps_01_2021 <- read.csv("CPS/jan21pub.csv")
cps_02_2021 <- read.csv("CPS/feb21pub.csv")
cps_03_2021 <- read.csv("CPS/mar21pub.csv")
cps_04_2021 <- read.csv("CPS/apr21pub.csv")
cps_05_2021 <- read.csv("CPS/may21pub.csv")
cps_06_2021 <- read.csv("CPS/jun21pub.csv")
cps_07_2021 <- read.csv("CPS/jul21pub.csv")
cps_08_2021 <- read.csv("CPS/aug21pub.csv")
cps_09_2021 <- read.csv("CPS/sep21pub.csv")
cps_10_2021 <- read.csv("CPS/oct21pub.csv")
cps_11_2021 <- read.csv("CPS/nov21pub.csv")
cps_12_2021 <- read.csv("CPS/dec21pub.csv")

# COVID-19 Cases and Deaths dataset, CDC
covidset <- read.csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")

# Oxford Stringency Index
stringencyset <- read.csv('OxCGRT_US_latest.csv')

# Labor Market Aggregates
laborAgg <- read.csv('State Aggregates.csv')

## FUNCTIONS

# 1. Computing and organizing demographic aggregates by state-month

stateDemo <- function(datasetList, state)  { # Computes several statistics for the specified state (FIPS code) from a list of datasets (cpsList: list of strings indicating datasets to draw from; state: integer FIPS state code)
  
  femaleRate <- vector()  #Female labor force participation: Share of working-age (16+) women in the labor force
  educRate <- vector()  #Higher education rate: Share of working-age (16+) population with a bachelor's degree or more
  lhShr <- vector() #Leisure and Hospitality employment share: Share of people employed (AND WORKING) in the state who are employed in the Hospitality and Leisure industry (first or second job)
  age <- vector() #Average age
  oldRate <- vector() #Old-age labor participation rate (i.e., that of people over 55)
  setIndicator <- vector() #Indicator for which data set the data was drawn from
  
  foreach(set = datasetList, period = c(1:length(cpsList))) %do%  { # Iterate over each data set to have stats for each month on a given state
    femaleRate[period] <- nrow(subset(get(set), gestfips == state & pesex == 2 & pemlr <= 4 & prtage >= 16)) / nrow(subset(get(set), gestfips == state & pesex == 2 & pemlr >= 1 & prtage >= 16))
    educRate[period] <- nrow(subset(get(set), gestfips == state & peeduca >= 43 & prtage >= 16)) / nrow(subset(get(set), gestfips == state & prtage >= 16))
    lhShr[period] <- nrow(subset(get(set), gestfips == state & (pemlr == 1 & prmjind1 == 11 | prmjind2 == 11))) / nrow(subset(get(set), gestfips == state & pemlr == 1))
    age[period] <- median(subset(get(set), gestfips == state & prtage >= 0)$prtage)
    oldRate[period] <-  nrow(subset(get(set), gestfips == state & pemlr <= 4 & prtage >= 55)) / nrow(subset(get(set), gestfips == state & pesex == 2 & pemlr >= 1 & prtage >= 55))
    setIndicator[period] <- set
  }
  #Compile states into a data frame
  stateNum <- replicate(length(cpsList), state)
  stateDemoSet <- data.frame(cbind(stateNum, setIndicator, femaleRate, educRate, lhShr, age, oldRate))
  return(stateDemoSet)
}
  
# 2. Total COVID-19 Cases (monthly average, for given state, 2020 and 2021)
covidTotal <- function(set, st)  {
  set$submission_date <- as.POSIXct(set$submission_date, format = "%m/%d/%Y")
  # 2020 averages
  covid2020 <- vector()
  months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  foreach(i = 1:12, m = months) %do%  { #Compute and store the median total number of COVID-19 cases at the state level by iterating over each month in 2020
    covid2020[i] <- median(subset(set, format(submission_date, format = "%Y") == "2020" & format(submission_date, format = "%m") == m & state == st)$tot_cases)
  }
  # 2021 averages
  covid2021 <- vector()
  foreach(i = 1:12, m = months) %do%  { #Similar to loop above, but for months in 2021 
    covid2021[i] <- median(subset(set, format(submission_date, format = "%Y") == "2021" & format(submission_date, format = "%m") == m & state == st)$tot_cases)
  }
  # Put it together
  stateHead <- replicate(24, st)
  tPeriod <- c('Jan20', 'Feb20', 'Mar20', 'Apr20', 'May20', 'Jun20', 'Jul20', 'Aug20', 'Sep20', 'Oct20', 'Nov20', 'Dec20',
               'Jan21', 'Feb21', 'Mar21', 'Apr21', 'May21', 'Jun21', 'Jul21', 'Aug21', 'Sep21', 'Oct21', 'Nov21', 'Dec21')
  covidts <- data.frame(cbind(stateHead, tPeriod, c(covid2020, covid2021)))
  return(covidts)
}

# 3. Monthly Average COVID Restriction stringency index (and individual indicators)
stateStringency <- function(set, st)  { # set: stringency index dataset; st: state of interest
  set$Date <- as.POSIXct(paste(set$Date), format = "%Y%m%d") #recognize this column as list of date objects
  months <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  #2020 measurements
  stringency2020 <- vector() #stringency index
  c1_2020 <- vector() #stringency components
  c2_2020 <- vector()
  c3_2020 <- vector()
  c4_2020 <- vector()
  c5_2020 <- vector()
  c6_2020 <- vector()
  c7_2020 <- vector()
  h1_2020 <- vector()
  foreach(i = 1:12, m = months) %do%  {
    stringency2020[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2020" & format(Date, format = "%m") == m & !is.na(set['StringencyIndex']))$StringencyIndex)
    c1_2020[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2020" & format(Date, format = "%m") == m & !is.na(set['C1_School.closing']))$C1_School.closing)
    c2_2020[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2020" & format(Date, format = "%m") == m & !is.na(set['C2_Workplace.closing']))$C2_Workplace.closing)
    c3_2020[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2020" & format(Date, format = "%m") == m & !is.na(set['C3_Cancel.public.events']))$C3_Cancel.public.events)
    c4_2020[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2020" & format(Date, format = "%m") == m & !is.na(set['C4_Restrictions.on.gatherings']))$C4_Restrictions.on.gatherings)
    c5_2020[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2020" & format(Date, format = "%m") == m & !is.na(set['C5_Close.public.transport']))$C5_Close.public.transport)
    c6_2020[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2020" & format(Date, format = "%m") == m & !is.na(set['C6_Stay.at.home.requirements']))$C6_Stay.at.home.requirements)
    c7_2020[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2020" & format(Date, format = "%m") == m & !is.na(set['C7_Restrictions.on.internal.movement']))$C7_Restrictions.on.internal.movement)
    h1_2020[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2020" & format(Date, format = "%m") == m & !is.na(set['H1_Public.information.campaigns']))$H1_Public.information.campaigns)
  }
  #2021 measurements
  stringency2021 <- vector() #stringency index
  c1_2021 <- vector() #stringency components
  c2_2021 <- vector()
  c3_2021 <- vector()
  c4_2021 <- vector()
  c5_2021 <- vector()
  c6_2021 <- vector()
  c7_2021 <- vector()
  h1_2021 <- vector()
  foreach(i = 1:12, m = months) %do%  {
    stringency2021[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2021" & format(Date, format = "%m") == m & !is.na(set['StringencyIndex']))$StringencyIndex)
    c1_2021[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2021" & format(Date, format = "%m") == m & !is.na(set['C1_School.closing']))$C1_School.closing)
    c2_2021[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2021" & format(Date, format = "%m") == m & !is.na(set['C2_Workplace.closing']))$C2_Workplace.closing)
    c3_2021[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2021" & format(Date, format = "%m") == m & !is.na(set['C3_Cancel.public.events']))$C3_Cancel.public.events)
    c4_2021[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2021" & format(Date, format = "%m") == m & !is.na(set['C4_Restrictions.on.gatherings']))$C4_Restrictions.on.gatherings)
    c5_2021[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2021" & format(Date, format = "%m") == m & !is.na(set['C5_Close.public.transport']))$C5_Close.public.transport)
    c6_2021[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2021" & format(Date, format = "%m") == m & !is.na(set['C6_Stay.at.home.requirements']))$C6_Stay.at.home.requirements)
    c7_2021[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2021" & format(Date, format = "%m") == m & !is.na(set['C7_Restrictions.on.internal.movement']))$C7_Restrictions.on.internal.movement)
    h1_2021[i] <- mean(subset(set, RegionCode == st & format(Date, format = "%Y") == "2021" & format(Date, format = "%m") == m & !is.na(set['H1_Public.information.campaigns']))$H1_Public.information.campaigns)
  }
  #Put it together
  stateName <- replicate(24, st)
  tPeriod <- c('Jan20', 'Feb20', 'Mar20', 'Apr20', 'May20', 'Jun20', 'Jul20', 'Aug20', 'Sep20', 'Oct20', 'Nov20', 'Dec20',
               'Jan21', 'Feb21', 'Mar21', 'Apr21', 'May21', 'Jun21', 'Jul21', 'Aug21', 'Sep21', 'Oct21', 'Nov21', 'Dec21')
  stringencyts <- data.frame(cbind(stateName, tPeriod, c(stringency2020, stringency2021), c(c1_2020, c1_2021), c(c2_2020, c2_2021), c(c3_2020, c3_2021), c(c4_2020, c4_2021),
                                   c(c5_2020, c5_2021), c(c6_2020, c6_2021), c(c7_2020, c7_2021), c(h1_2020, h1_2021)))
  return(stringencyts)
}

## MAIN BODY

states <- setdiff(c(1:56), c(3, 7, 14, 43, 52)) # Excludes gaps in state coding, includes DC (only has a county FIPS)

# Controls ~ Demographic aggregates

cpsList <- c('cps_01_2020', 'cps_02_2020', 'cps_03_2020', 'cps_04_2020', 'cps_05_2020', 'cps_06_2020',
             'cps_07_2020', 'cps_08_2020', 'cps_09_2020', 'cps_10_2020', 'cps_11_2020', 'cps_12_2020',
             'cps_01_2021', 'cps_02_2021', 'cps_03_2021', 'cps_04_2021', 'cps_05_2021', 'cps_06_2021',
             'cps_07_2021', 'cps_08_2021', 'cps_09_2021', 'cps_10_2021', 'cps_11_2021', 'cps_12_2021')

for(s in states)  {
  assign(paste('Demo', s, sep = ""), stateDemo(cpsList, s))
}

demoPanel <- data.frame(rbind(Demo1, Demo2, Demo4, Demo5, Demo6, Demo8, Demo9, Demo10, Demo11, Demo12, Demo13, Demo15, Demo16,
                              Demo17, Demo18, Demo19, Demo20, Demo21, Demo22, Demo23, Demo24, Demo25, Demo26, Demo27, Demo28,
                              Demo29, Demo30, Demo31, Demo32, Demo33, Demo34, Demo35, Demo36, Demo37, Demo38, Demo39, Demo40,
                              Demo41, Demo42, Demo44, Demo45, Demo46, Demo47, Demo48, Demo49, Demo50, Demo51, Demo53, Demo54,
                              Demo55, Demo56))

# Controls ~ Pandemic Severity

for(i in unique(covidset$state))  {
  assign(paste(i, "cset", sep = ""), covidTotal(covidset, i))
}

#Compile the state data sets into a large panel
covidPanel <- data.frame(rbind(ALcset, AKcset, AZcset, ARcset, CAcset, COcset, CTcset, DEcset, DCcset, FLcset,
                               GAcset, HIcset, IDcset, ILcset, INcset, IAcset, KScset, KYcset, LAcset, MEcset,
                               MDcset, MAcset, MIcset, MNcset, MScset, MOcset, MTcset, NEcset, NVcset, NHcset,
                               NJcset, NMcset, NYcset, NCcset, NDcset, OHcset, OKcset, ORcset, PAcset, RIcset,
                               SCcset, SDcset, TNcset, TXcset, UTcset, VTcset, VAcset, WAcset, WVcset, WIcset,
                               WYcset))

# COVID Restriction Level Data (Stringency Index, C1-C8, H1)

for(t in unique(stringencyset$RegionCode))  {
  assign(paste(t, "rset", sep = ""), stateStringency(stringencyset, t))
}

restrictionPanel <- data.frame(rbind(US_ALrset, US_AKrset, US_AZrset, US_ARrset, US_CArset, US_COrset, US_CTrset,
                                     US_DErset, US_DCrset, US_FLrset, US_GArset, US_HIrset, US_IDrset, US_ILrset,
                                     US_INrset, US_IArset, US_KSrset, US_KYrset, US_LArset, US_MErset, US_MDrset,
                                     US_MArset, US_MIrset, US_MNrset, US_MSrset, US_MOrset, US_MTrset, US_NErset,
                                     US_NVrset, US_NHrset, US_NJrset, US_NMrset, US_NYrset, US_NCrset, US_NDrset,
                                     US_OHrset, US_OKrset, US_ORrset, US_PArset, US_RIrset, US_SCrset, US_SDrset,
                                     US_TNrset, US_TXrset, US_UTrset, US_VTrset, US_VArset, US_WArset, US_WVrset,
                                     US_WIrset, US_WYrset))

# Compile a Single Data Set

panel <- data.frame(cbind(laborAgg, restrictionPanel$V3, restrictionPanel$V4, restrictionPanel$V5, restrictionPanel$V6, restrictionPanel$V7, restrictionPanel$V8, restrictionPanel$V9, restrictionPanel$V10, restrictionPanel$V11,
                          demoPanel$femaleRate, demoPanel$educRate, demoPanel$lhShr, demoPanel$age, demoPanel$oldRate, covidPanel$V3))

panel$job_openings_1000s <- panel$job_openings_1000s * 1000 #convert to thousands

# Rename columns for legibility
colnames(panel) <- c('code', 'state', 'period', 'lforce', 'unemployed', 'urate', 'openings', 'jobrate', 'pop', 'stindex', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'H1', 'f_lfp', 'educrate', 'lhshr', 'avgage', 'o_lfp', 'covid')


# Export to .csv

write.csv (panel, 'usrestrictions.csv', row.names = T)
