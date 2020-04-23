setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/data")

rm(list = ls())
library(tidyverse)
library(BayesFactor)

### read in data sets from the different trajectories 
### using fixed-N design in confirmatory study
final <- 
  read.csv(file = "./Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_sig_method1")

final$t_value <- ifelse(final$t_value < 0,
                                 -final$t_value, -final$t_value)


BF <- NULL

for (i in 1:nrow(final)) {
  
  BF[i] <- ttest.tstat(final$t_value[i], 
                    n1 = final$rep_sample_size[i],
                    n2 = final$rep_sample_size[i], 
                    rscale = "medium",
                    simple = TRUE)
  
}


# bf <- ttest.tstat(4.1320, n1 = 6, n2 = 6, rscale = "medium", simple = TRUE)


final$BF <- round(unlist(BF), 4)

# write.csv(final, file = "./Carneiro_distribution_sig_method1_BF_from_t_value")

final_significants <-
  final %>% 
  filter(p_value <= .05)

final_nonsignificants <-
  final %>% 
  filter(p_value > .05)

ggplot(aes(y = effect, x = ES_true, col = BF > 3),
       data = final_significants) +
  facet_wrap(~ factor(init_sample_size)) +
  geom_point(alpha = 0.2) +
  geom_hline(aes(yintercept = .5), color = "red") +
  theme_bw()

BF_low <-
  final %>% 
  filter(BF < 15 & t_value < 15)

ggplot(aes(x = t_value, y = BF, col = p_value < .05),
       data = BF_low) +
  facet_wrap(~ factor(init_sample_size)) +
  geom_point(alpha = 0.2) +
  theme_bw()
