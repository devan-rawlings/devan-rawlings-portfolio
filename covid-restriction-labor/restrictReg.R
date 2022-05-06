#! restrictReg.R
## This script computes all regressions, including robustness checks, for the "COVID restrictions" paper.

library(tidyverse)
library(plm)
library(fixest) # Two-way fixed effects regression functions

## READ IN DATA
main <- read.csv('usrestrictions.csv')[, 2:25] # Exclude row number column
gov <- read.csv('usgovernors.csv')

# Construct additional variables
covidcap <- main$covid / main$pop # Compute COVID cases per capita
tightness <- main$openings / main$unemployed # Compute labor market tightness

# Multiply jobrate by 100 to get percentage
main$jobrate <- main$jobrate * 100

## Construct dataset
panel <- data.frame(cbind(main, covidcap, tightness, gov$partisan))
panel$period <- paste0(panel$period, '-1')
panel$period <- as.Date(panel$period, format = "%y-%b-%d") # convert period to date object
year <- format(panel$period, "%Y") # Year column
panel$state <- as.factor(panel$state)
panel$gov.partisan <- as.factor(panel$gov.partisan)

# Construct lagged variables that account for panel structure
panel <- pdata.frame(panel, index = c('state', 'period')) 
panel <- cbind(panel, plm::lead(panel$stindex, -1), plm::lead(panel$covidcap, -1), plm::lead(panel$lhshr, -1), plm::lead(panel$C1, -1), plm::lead(panel$C2, -1), plm::lead(panel$C3, -1),
               plm::lead(panel$C4, -1), plm::lead(panel$C5, -1), plm::lead(panel$C6, -1), plm::lead(panel$C7, -1), plm::lead(panel$H1, -1), year)
colnames(panel)[28:38] <- c('stindexLag', 'covidcapLag', 'lhshrLag', 'C1Lag', 'C2Lag', 'C3Lag', 'C4Lag', 'C5Lag', 'C6Lag', 'C7Lag', 'H1Lag')
panel <- pdata.frame(panel, index = c('state', 'period'))

## MAIN REGRESSION: twfe on unemployment rate
rurate1 <- feols(urate ~ stindexLag | state + period, data = panel, cluster = ~state)
summary(rurate1) # No controls

#Significant positive relationship between policy strictness and unemployment rates -- in-line with the literature

# Add controls
rurate2 <- feols(urate ~ stindexLag + covidcap + covidcapLag + gov.partisan + avgage + o_lfp + f_lfp + educrate | state + period, data = panel, cluster = ~state)
summary(rurate2)

#Positive relationship holds when controls are added

## MAIN REGRESSION: twfe on vacancy rate
rjrate1 <- feols(jobrate ~ stindexLag | state + period, data = panel, cluster = ~state)
summary(rjrate1) # No controls

#No significant relationship between strictness and vacancy rate -- high p-value may suggest that there is no relationship between them, but could
#also be ambiguous (strictness decreases employment and job openings)

# Add controls
rjrate2 <- feols(jobrate ~ stindexLag + covidcap + covidcapLag + gov.partisan + avgage + o_lfp + f_lfp + educrate | state + period, data = panel, cluster = ~state)
summary(rjrate2)

#Interesting: Having a Republican governor significantly increases the job vacancy rate of a state

## MAIN REGRESSION: twfe on tightness
rtight1 <- feols(tightness ~ stindexLag | state + period, data = panel, cluster = ~state)
summary(rtight1) # No controls

#Insignificant (nearly significant) effect of strictness on tightness ~ increased strictness could affect unemployment more than job openings in the medium run

# Add controls
rtight2 <- feols(tightness ~ stindexLag + covidcap + covidcapLag + gov.partisan + avgage + o_lfp + f_lfp + educrate | state + period, data = panel, cluster = ~state)
summary(rtight2)

## SECONDARY REGRESSION: (log) Job openings as outcome
ropen1 <- feols(log(openings) ~ stindexLag | state + period, data = panel, cluster = ~state)
summary(ropen1)

#(Highly) insignificant effect of strictness on openings ~ No medium-run effect of strictness on job openings (?)

ropen2 <- feols(log(openings) ~ stindexLag + covidcap + covidcapLag + gov.partisan + avgage + o_lfp + f_lfp + educrate | state + period, data = panel, cluster = ~state)
summary(ropen2) #With controls

## SECONDARY REGRESSION: lhshr as outcome
rshr1 <- feols(lhshr ~ stindexLag | state + period, data = panel, cluster = ~state)
summary(rshr1)

#(Highly) significant effect of strictness on leisure and hospitality share of employment - higher strictness has tended to lead to decreases in employment share of vulnerable industries
#This holds when controls are added

rshr2 <- feols(lhshr ~ stindexLag + covidcap + covidcapLag + gov.partisan + avgage + o_lfp + f_lfp + educrate | state + period, data = panel, cluster = ~state)
summary(rshr2) #With controls

## SECONDARY REGRESSION: 2020 regressions
rurate2020 <- feols(urate ~ stindexLag | state + period, data = subset(panel, year == '2020'), cluster = ~state)
summary(rurate2020)

rjrate2020 <- feols(jobrate ~ stindexLag | state + period, data = subset(panel, year == '2020'), cluster = ~state)
summary(rjrate2020)

rtight2020 <- feols(tightness ~ stindexLag | state + period, data = subset(panel, year == '2020'), cluster = ~state)
summary(rtight2020)

ropen2020 <- feols(log(openings) ~ stindexLag | state + period, data = subset(panel, year == '2020'), cluster = ~state)
summary(ropen2020)

## SECONDARY REGRESSION 2021 regressions
rurate2021 <- feols(urate ~ stindexLag | state + period, data = subset(panel, year == '2021'), cluster = ~state)
summary(rurate2021)

#Strong relationship between strictness and unemployment in 2020 falls apart in 2021 - the economy may have adjusted to covid restrictions by the second year

rjrate2021 <- feols(jobrate ~ stindexLag | state + period, data = subset(panel, year == '2021'), cluster = ~state)
summary(rjrate2021)

rtight2021 <- feols(tightness ~ stindexLag | state + period, data = subset(panel, year == '2021'), cluster = ~state)
summary(rtight2021)

#While strictness led to a decrease in tightness in 2020, it led to a significant increase in 2021 

ropen2021 <- feols(log(openings) ~ stindexLag | state + period, data = subset(panel, year == '2021'), cluster = ~state)
summary(ropen2021)

## SECONDARY REGRESSION: interaction with lhshr
rurateshr <- feols(urate ~ stindexLag + lhshrLag + stindexLag*lhshrLag | state + period, data = panel, cluster = ~state) #unemployment
summary(rurateshr)

#Positive, significant coefficient on the interaction of stringency with leisure/hospitality share - the more dependent a state is on leisure and hospitality industries 
#for employment, the greater of an effect a lockdown has on unemployment

rtightshr <- feols(tightness ~ stindexLag + lhshrLag + stindexLag * lhshrLag | state + period, data = panel, cluster = ~state) #tightness
summary(rtightshr) #No meaningful relationship here

rurate2020shr <- feols(urate ~ stindexLag + lhshrLag + stindexLag * lhshrLag | state + period, data = subset(panel, year == '2020'), cluster = ~state) #2020 urate
summary(rurate2020shr)
#Relationship between the interaction and unemployment is considerably stronger in 2020 than in 2021 (relationship becomes insignificant in 2021)

rurate2021shr <- feols(urate ~ stindexLag + lhshrLag + stindexLag * lhshrLag | state + period, data = subset(panel, year == '2021'), cluster = ~state)
summary(rurate2021shr)

### STRINGENCY COMPONENT REGRESSIONS

## urate, 2020-2021
ruratec <- feols(urate ~ C1Lag + C2Lag + C3Lag + C4Lag + C5Lag + C6Lag + C7Lag + H1Lag | state + period, data = panel, cluster = ~state)
summary(ruratec)
#Stay at home requirements are the only singificant factor in increasing unemployment rates in the medium run

## urate, 2020
ruratec2020 <- feols(urate ~ C1Lag + C2Lag + C3Lag + C4Lag + C5Lag + C6Lag + C7Lag + H1Lag | state + period, data = subset(panel, year == '2020'), cluster = ~state)
summary(ruratec2020)
#restrictions on internal movement appears to be the most significant factor (?) (business closures and stay at home requirements are nearly significant)

## urate, 2021
ruratec2021 <- feols(urate ~ C1Lag + C2Lag + C3Lag + C4Lag + C5Lag + C6Lag + C7Lag + H1Lag | state + period, data = subset(panel, year == '2021'), cluster = ~state)
summary(ruratec2021)
#No significant factors in 2021

## tightness, 2020-2021
rtightnessc <- feols(tightness ~ C1Lag + C2Lag + C3Lag + C4Lag + C5Lag + C6Lag + C7Lag + H1Lag | state + period, data = panel, cluster = ~state)
summary(rtightnessc)
#Workplace closures have had a significant upward pressure on tightness (increasing unemployment, static or declining vacancies)

## tightness, 2020
rtightnessc2020 <- feols(tightness ~ C1Lag + C2Lag + C3Lag + C4Lag + C5Lag + C6Lag + C7Lag + H1Lag | state + period, data = subset(panel, year == '2020'), cluster = ~state)
summary(rtightnessc2020)

## tightness, 2021
rtightnessc2021 <- feols(tightness ~ C1Lag + C2Lag + C3Lag + C4Lag + C5Lag + C6Lag + C7Lag + H1Lag | state + period, data = subset(panel, year == '2021'), cluster = ~state)
summary(rtightnessc2021)
