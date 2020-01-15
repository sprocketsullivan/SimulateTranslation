setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/data")

rm(list = ls())


### read in data sets from the different trajectories 
### using fixed-N design in confirmatory study
final <- read.csv(file = "Szucs_distribution_equiv_method2_0.7")

final_without_outliers <-
  final %>% 
  filter(ES_true < 10)

hist(final$ES_true, breaks = 100)

hist(final$effect, breaks = 100)

hist(final_without_outliers$ES_true, breaks = 100)

hist(final_without_outliers$effect, breaks = 100)
