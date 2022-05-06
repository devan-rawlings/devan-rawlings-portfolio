#! sumStats.R
## This script takes in the cleaned panel data and generates summary statistics tables, graphs and GIS datasets.

library(tidyverse)
library(qwraps2) # summary_table()

## READ IN DATA

panel <- read.csv('usrestrictions.csv')

# Convert period to date format
panel$period <- paste0(panel$period, "-01")
panel$period <- as.Date(panel$period, format = "%y-%b-%d")

#Convert vacancy rate to percentage
panel$jobrate <- panel$jobrate * 100

## GENERATE ADDITIONAL VARIABLES

covidcap <- panel$covid / panel$pop
tightness <- panel$openings / panel$unemployed # Compute labor market tightness
year <- format(panel$period, "%Y") # Year column

panel <- data.frame(cbind(panel, covidcap, tightness, year))

## SUMMARY TABLES
# Outcomes and stringency measures

sum_content1 <-
  list("Unemployment Rate" =
         list("min"       = ~ round(min(urate), 3),
              "max"       = ~ round(max(urate), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(urate)),
       "Vacancy Rate" =
         list("min"       = ~ round(min(jobrate), 3),
              "max"       = ~ round(max(jobrate), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(jobrate)),
       "Labor Market Tightness (V/U)" =
         list("min"       = ~ round(min(openings/unemployed), 3),
              "max"       = ~ round(max(openings/unemployed), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(openings/unemployed)),
       "Stringency Index" =
         list("min"       = ~ round(min(stindex), 2),
              "max"       = ~ round(max(stindex), 2),
              "mean (sd)" = ~ qwraps2::mean_sd(stindex)),
       "C1 (School Closures)" =
         list("min"       = ~ round(min(C1), 2),
              "max"       = ~ round(max(C1), 2),
              "mean (sd)" = ~ qwraps2::mean_sd(C1)),
       "C2 (Workplace Closures)" =
         list("min"       = ~ round(min(C2), 2),
              "max"       = ~ round(max(C2), 2),
              "mean (sd)" = ~ qwraps2::mean_sd(C2)),
       "C3 (Public Event Cancellations)" =
         list("min"       = ~ round(min(C3), 2),
              "max"       = ~ round(max(C3), 2),
              "mean (sd)" = ~ qwraps2::mean_sd(C3)),
       "C4 (Restrictions of Gathering Size)" =
         list("min"       = ~ round(min(C4), 2),
              "max"       = ~ round(max(C4), 2),
              "mean (sd)" = ~ qwraps2::mean_sd(C4)),
       "C5 (Public Transport Closures)" =
         list("min"       = ~ round(min(C5), 2),
              "max"       = ~ round(max(C5), 2),
              "mean (sd)" = ~ qwraps2::mean_sd(C5)),
       "C6 (Stay at Home Requirements)" =
         list("min"       = ~ round(min(C6), 2),
              "max"       = ~ round(max(C6), 2),
              "mean (sd)" = ~ qwraps2::mean_sd(C6)),
       "C7 (Restrictions on Internal Movement)" =
         list("min"       = ~ round(min(C7), 2),
              "max"       = ~ round(max(C7), 2),
              "mean (sd)" = ~ qwraps2::mean_sd(C7)),
       "H1 (Public Information Campaign)" =
         list("min"       = ~ round(min(H1), 2),
              "max"       = ~ round(max(H1), 2),
              "mean (sd)" = ~ qwraps2::mean_sd(H1))
       
  )

summary_table(group_by(panel, year), sum_content1)

# Controls

sum_content3 <-
  list("Female Labor Force Participation Rate" =
         list("min"       = ~ round(min(f_lfp), 3),
              "max"       = ~ round(max(f_lfp), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(f_lfp)),
       "Higher Education Rate" =
         list("min"       = ~ round(min(educrate), 3),
              "max"       = ~ round(max(educrate), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(educrate)),
       "Hospitality Employment Share" =
         list("min"       = ~ round(min(lhshr), 3),
              "max"       = ~ round(max(lhshr), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(lhshr)),
       "Average Age" =
         list("min"       = ~ round(min(avgage), 3),
              "max"       = ~ round(max(avgage), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(avgage)),
       "Old-Age Labor Force Participation" =
         list("min"       = ~ round(min(o_lfp), 3),
              "max"       = ~ round(max(o_lfp), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(o_lfp)),
       "COVID-19 Cases Per Capita" =
         list("min"       = ~ round(min(covidcap), 3),
              "max"       = ~ round(max(covidcap), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(covidcap))
  )

summary_table(group_by(panel, year), sum_content3)

## GRAPHS

# National Average Stingency Level and Outcomes Over Time

panel %>% group_by(period) %>% # Compute average and standard deviation of stringency index and labor tightness by period
  summarize(avgs = mean(stindex, na.rm = TRUE),
            stds = sd(stindex, na.rm = TRUE),
            avgt = mean(tightness, na.rm = TRUE),
            stdt = sd(tightness, na.rm = TRUE),
            n.panel = n()) -> sumTab

sumTab %>% # Compute 95% confidence interval bounds for each variable of interest
  mutate(margins = qt(0.975, df = n.panel - 1)*stds/sqrt(n.panel)) %>%
  mutate(margint = qt(0.975, df = n.panel - 1)* stdt / sqrt(n.panel)) %>%
  mutate(cius = avgs + margins) %>%
  mutate(cils = avgs - margins) %>%
  mutate(ciut = avgt + margint) %>%
  mutate(cilt = avgt - margint) -> sumTab

ggplot(sumTab, aes(x = period)) + geom_line(aes(y = avgs), linetype = 'solid') + geom_line(aes(y = cius), linetype = 'dotdash') + 
  geom_line(aes(y = cils), linetype = 'dotdash') +  xlab("Month (yyyy-mm)") + ylab("Stringency Index") + ggtitle("Average COVID-19 Policy Stringency, US, 2020-2021")

ggplot(sumTab, aes(x = period)) + geom_line(aes(y = avgt), linetype = 'solid') + geom_line(aes(y = ciut), linetype = 'dotdash') + 
  geom_line(aes(y = cilt), linetype = 'dotdash') +  xlab("Month (yyyy-mm)") + ylab("Labor Market Tightness (V/U)") + ggtitle("Average Labor Market Tightness, US, 2020-2021")

# Variation in stringency and labor outcomes across states ~ Summer 2020 (late first wave)

ggplot(subset(panel, period == '2020-07-01'), aes(x = code, y = stindex)) + geom_bar(stat = 'identity') + ylab('Stringency Index (0-100)') +
  ggtitle('COVID-19 Policy Stringency By State ~ July 2020')

ggplot(subset(panel, period == '2020-08-01'), aes(x = code, y = tightness)) + geom_bar(stat = 'identity') + ylab('Labor Market Tightness (V/U)') +
  ggtitle('Labor Market Tightness By State ~ August 2020')

# Variation in stringency and labor outcomes across states ~ January-February 2021 (third wave)

ggplot(subset(panel, period == '2021-01-01'), aes(x = code, y = stindex)) + geom_bar(stat = 'identity') + ylab('Stringency Index (0-100)') +
  ggtitle('COVID-19 Policy Stringency By State ~ January 2021')

ggplot(subset(panel, period == '2020-02-01'), aes(x = code, y = tightness)) + geom_bar(stat = 'identity') + ylab('Labor Market Tightness (V/U)') +
  ggtitle('Labor Market Tightness By State ~ February 2021')

# Stringency and Outcomes over time ~ New York

ggplot(subset(panel, state == 'New York'), aes(x = period, y = stindex)) + geom_line() # stringency
ggplot(subset(panel, state == 'New York'), aes(x = period, y = urate)) + geom_line() # unemployment rate
ggplot(subset(panel, state == 'New York'), aes(x = period, y = jobrate)) + geom_line() # vacancy rate
ggplot(subset(panel, state == 'New York'), aes(x = period, y = tightness)) + geom_line() # tightness

# Stringency and Outcomes over time ~ Florida

ggplot(subset(panel, state == 'Florida'), aes(x = period, y = stindex)) + geom_line() # stringency
ggplot(subset(panel, state == 'Florida'), aes(x = period, y = urate)) + geom_line() # unemployment rate
ggplot(subset(panel, state == 'Florida'), aes(x = period, y = jobrate)) + geom_line() # vacancy rate
ggplot(subset(panel, state == 'Florida'), aes(x = period, y = tightness)) + geom_line() # tightness

# Map I: Stringency Level, first trough (July 2020)

mapset1 <- data.frame(cbind(subset(panel, period == '2020-07-01')$state, subset(panel, period == '2020-07-01')$stindex))
colnames(mapset1) <- c('state', 'stindex')

# Map II: Tightness, first trough (July 2020)

mapset2 <- data.frame(cbind(subset(panel, period == '2020-07-01')$state, subset(panel, period == '2020-07-01')$tightness))
colnames(mapset2) <- c('state', 'tightness')

# Map III: Stringency Level, third wave (January 2021)

mapset3 <- data.frame(cbind(subset(panel, period == '2021-01-01')$state, subset(panel, period == '2021-01-01')$stindex))
colnames(mapset3) <- c('state', 'stindex')

# Map IV: Tightness, third wave (January 2021)

mapset4 <- data.frame(cbind(subset(panel, period == '2021-01-01')$state, subset(panel, period == '2021-01-01')$tightness))
colnames(mapset4) <- c('state', 'tightness')

# Map V: Stringency Level, July 2021

mapset5 <- data.frame(cbind(subset(panel, period == '2021-07-01')$state, subset(panel, period == '2021-07-01')$stindex))
colnames(mapset5) <- c('state', 'stindex')

# Map VI: Tightness, July 2021

mapset6 <- data.frame(cbind(subset(panel, period == '2021-07-01')$state, subset(panel, period == '2021-07-01')$tightness))
colnames(mapset6) <- c('state', 'tightness')

# Map VII: Unemployment Rate, July 2020

mapset7 <- data.frame(cbind(subset(panel, period == '2020-07-01')$state, subset(panel, period == '2020-07-01')$urate))
colnames(mapset7) <- c('state', 'urate')

# Map VIII: Vacancy Rate, July 2020

mapset8 <- data.frame(cbind(subset(panel, period == '2020-07-01')$state, subset(panel, period == '2020-07-01')$jobrate))
colnames(mapset8) <- c('state', 'jobrate')

# Map IX: Unemployment Rate, July 2021

mapset9 <- data.frame(cbind(subset(panel, period == '2021-07-01')$state, subset(panel, period == '2021-07-01')$urate))
colnames(mapset9) <- c('state', 'urate')

# Map X: Vacancy Rate, July 2021

mapset10 <- data.frame(cbind(subset(panel, period == '2021-07-01')$state, subset(panel, period == '2021-07-01')$jobrate))
colnames(mapset10) <- c('state', 'jobrate')

# Replace DC with 'District of Columbia' to work with ArcGIS shapefile

mapset1$state[9] <- 'District of Columbia'
mapset2$state[9] <- 'District of Columbia'
mapset3$state[9] <- 'District of Columbia'
mapset4$state[9] <- 'District of Columbia'
mapset5$state[9] <- 'District of Columbia'
mapset6$state[9] <- 'District of Columbia'
mapset7$state[9] <- 'District of Columbia'
mapset8$state[9] <- 'District of Columbia'
mapset9$state[9] <- 'District of Columbia'
mapset10$state[9] <- 'District of Columbia'

write.csv(mapset1, 'mapset1.csv')
write.csv(mapset2, 'mapset2.csv')
write.csv(mapset3, 'mapset3.csv')
write.csv(mapset4, 'mapset4.csv')
write.csv(mapset5, 'mapset5.csv')
write.csv(mapset6, 'mapset6.csv')
write.csv(mapset7, 'mapset7.csv')
write.csv(mapset8, 'mapset8.csv')
write.csv(mapset9, 'mapset9.csv')
write.csv(mapset10, 'mapset10.csv')
