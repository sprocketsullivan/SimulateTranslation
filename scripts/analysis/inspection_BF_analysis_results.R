setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/")

library(tidyverse)

BF_analysis_data_sig <- 
  read.csv(file = "./data/Carneiro_distribution/Bayes_factor_analysis/Carneiro_distribution_sig_p0.05_method1")

BF_analysis_data_equiv <- 
  read.csv(file = "./data/Carneiro_distribution/Bayes_factor_analysis/Carneiro_distribution_equiv_method1_0.5")
### add column that codes decision criterion from exploratory stage to confirmatory stage


BF_analysis_data_sig$decision_crit <- "significance"

BF_analysis_data_equiv$decision_crit <- "equivalence"

### add column that codes approach to determine sample size for confirmatory study
BF_analysis_data_sig$sampsize_approach <- "standard"

BF_analysis_data_equiv$sampsize_approach <- "SESOI"

BF_data <- bind_rows(BF_analysis_data_sig, BF_analysis_data_equiv)

BF_data$trajectory <- interaction(BF_data$decision_crit, BF_data$sampsize_approach)

BF_summary <- 
  BF_data %>% 
  group_by(trajectory, init_sample_size) %>% 
  summarize(max_BF = max(BF),
            mean_BF = mean(BF),
            sd_BF = sd(BF),
            median_BF = median(BF))


BF_data <-
  BF_data %>% 
  filter(init_sample_size == 10) %>% 
  filter(BF < 1e+2)

hist(BF_data$BF, breaks = 200)

ggplot(data = BF_data) +
  geom_histogram(aes(x = BF), bins = 100) +
  facet_wrap(~ trajectory)



ggplot(data = BF_data) +
  geom_boxplot(aes(x = trajectory, y = BF))