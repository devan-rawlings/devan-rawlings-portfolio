#! Graphs and Stats II.R
## This script renders all graphs and tables (except the heat maps) used for the summary statistics analysis of the 'NAICS set.'

library(tidyselect)
library(haven)
library(ggplot2)
library(dplyr)
library(qwraps2)

##Load datasets
aggset <- read_dta("NAICS_set_agg.dta")
set0116 <- read_dta("regset0116.dta")
popset <- readxl::read_xls("Census Aggregate Set Male.xls")

set0116 <- data.frame(cbind(set0116, subset(popset, Year == 2001)$pop))
colnames(set0116)[79] <- "pop"

##Graph 1: Initial vs Change ~ Manufacturing Share

ggplot(set0116, aes(x = manu_shr, y = del_manushr)) + geom_point(aes(size = pop))+ geom_smooth(method = "lm") +xlab("Initial Manufacturing Share (2001)") + ylab("Change in Manufacturing Share (2001-2016)") + ggtitle("Importance of Manufacturing and Decline, Canadian CMAs and CAs")

##Graph 2: Relationship between IV and Independent Variable

ggplot(set0116, aes(x = bartik, y = del_manushr)) + geom_point() + geom_smooth(method = "lm") + xlab("Predicted Change in Manufacturing Share") + ylab("Actual Change in Manufacturing Share") + ggtitle("Relationship between Predicted and Actual Changes in Manufacturing Share, 2001-2016")

##Graph 3 + 4 + 5: Differential sub-industrial compositions among CMAs

ggplot(set0116, aes(x = CMA, y = share311)) + geom_bar(stat = "identity", fill = "steelblue") + xlab("CMA/CA Code") + ylab("Employment Share (2001)") + ggtitle("Variation in Employment Share of Food Manufacturing Among Local Economies (2001)")

ggplot(set0116, aes(x = CMA, y = share321)) + geom_bar(stat = "identity") + xlab("CMA/CA Code") + ylab("Employment Share (2001)") + ggtitle("Variation in Employment Share of Wood Product Manufacturing Among Local Economies (2001)")

ggplot(set0116, aes(x = CMA, y = share336)) + geom_bar(stat = "identity", fill = "darkred") + xlab("CMA/CA Code") + ylab("Employment Share (2001)") + ggtitle("Variation in Employment Share of Transportation Equipment Manufacturing Among Local Economies (2001)")

##Graph 6: Differential Shocks among sub-industries

shocks <- subset(set0116, CMA == 11) %>% #Choose shocks that exclude Gander because it has a small manufacturing share (low impact on the aggregate shocks)
  select(shock311:shock339)

shocks <- as.numeric(round(abs(shocks), 5))
NAICS <- c("311", "312", "313", "314", "315", "316", "321", "322", "323", "324", "325", "326", "327", "331", "332", "333", "334", "335", "336", "337", "339")

shockset <- data.frame(cbind(NAICS, shocks))

ggplot(shockset, aes(x = NAICS, y = shocks)) + geom_bar(stat = "identity", fill = "steelblue") + xlab("NAICS Manufacturing Subindustry") + ylab("Magnitude of Negative Employment Shock (Percent of Employment, 2001-2016)") + ggtitle("Variation in Employment Shocks Among Manufacturing Sub-industries, 2001-2016")

##Summary Stats
#Stocks

aggset2 <- subset(aggset, CMA != 11)

sum_content1 <-
  list("Higher Education Share" =
         list("min"       = ~ round(min(he_share), 3),
              "max"       = ~ round(max(he_share), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(he_share)),
       "Female Labour Participation Rate" =
         list("min"       = ~ round(min(fe_lfp), 3),
              "max"       = ~ round(max(fe_lfp), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(fe_lfp)),
       "Share Foreign Born" =
         list("min"       = ~ round(min(for_bor), 3),
              "max"       = ~ round(max(for_bor), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(for_bor))
  )
sum_content2 <-
  list("Employment Rate (Male)" =
         list("min"       = ~ round(min(erateM), 3),
              "max"       = ~ round(max(erateM), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(erateM)),
       "Employment Rate (Female)" =
         list("min"       = ~ round(min(erateF), 3),
              "max"       = ~ round(max(erateF), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(erateF)),
       "Manufacturing Share" =
              list("min"       = ~ round(min(manu_shr), 3),
                   "max"       = ~ round(max(manu_shr), 3),
                   "mean (sd)" = ~ qwraps2::mean_sd(manu_shr))
  )

summary_table(group_by(aggset2, Year), sum_content1)
summary_table(group_by(aggset2, Year), sum_content2)

#Flows
manu0116 <- subset(aggset2, Year == 2016)$manu_shr - subset(aggset2, Year == 2001)$manu_shr
manu0111 <- subset(aggset2, Year == 2011)$manu_shr - subset(aggset2, Year == 2001)$manu_shr
manu1116 <- subset(aggset2, Year == 2016)$manu_shr - subset(aggset2, Year == 2011)$manu_shr

erateM0116 <- subset(aggset2, Year == 2016)$erateM - subset(aggset2, Year == 2001)$erateM
erateM0111 <- subset(aggset2, Year == 2011)$erateM - subset(aggset2, Year == 2001)$erateM
erateM1116 <- subset(aggset2, Year == 2016)$erateM - subset(aggset2, Year == 2011)$erateM

erateF0116 <- subset(aggset2, Year == 2016)$erateF - subset(aggset2, Year == 2001)$erateF
erateF0111 <- subset(aggset2, Year == 2011)$erateF - subset(aggset2, Year == 2001)$erateF
erateF1116 <- subset(aggset2, Year == 2016)$erateF - subset(aggset2, Year == 2011)$erateF

interval1 <- sprintf("2001-2016", 1:133)
interval2 <- sprintf("2001-2011", 1:133)
interval3 <- sprintf("2011-2016", 1:133)

flowset1 <- data.frame(cbind(interval1, manu0116, erateM0116, erateF0116))
colnames(flowset1) <- c("Interval", "manu_shr", "erateM", "erateF")
flowset2 <- data.frame(cbind(interval2, manu0111, erateM0111, erateF0111))
colnames(flowset2) <- c("Interval", "manu_shr", "erateM", "erateF")
flowset3 <- data.frame(cbind(interval3, manu1116, erateM1116, erateF1116))
colnames(flowset3) <- c("Interval", "manu_shr", "erateM", "erateF")

flowset <- data.frame(rbind(flowset1, flowset2, flowset3))
flowset$manu_shr <- as.numeric(flowset$manu_shr)
flowset$erateM <- as.numeric(flowset$erateM)
flowset$erateF <- as.numeric(flowset$erateF)

sum_content3 <-
  list("Change in Manufacturing Share" =
         list("min"       = ~ round(min(manu_shr), 3),
              "max"       = ~ round(max(manu_shr), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(manu_shr)),
       "Change in Employment Rate (Female)" =
         list("min"       = ~ round(min(erateM), 3),
              "max"       = ~ round(max(erateM), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(erateM)),
       "Change in Employment Rate (Male)" =
         list("min"       = ~ round(min(erateF), 3),
              "max"       = ~ round(max(erateF), 3),
              "mean (sd)" = ~ qwraps2::mean_sd(erateF))
  )

summary_table(group_by(flowset, Interval), sum_content3)
