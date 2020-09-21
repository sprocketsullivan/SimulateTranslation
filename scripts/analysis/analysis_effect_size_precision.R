setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/")

rm(list = ls())

library(tidyverse)
library(compute.es)


### read in data sets from the different trajectories 
### using fixed-N design in replication study

# final <- read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_equiv_method2_1.0")

final_sig_Szucs <- read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_distribution_sig_method1")
final_sig_Szucs$distribution <- "Optimistic"
final_sig_Szucs$trajectory <- "T1"

final_equiv_Szucs <- read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_distribution_equiv_method2_0.5")
final_equiv_Szucs$distribution <- "Optimistic"
final_equiv_Szucs$trajectory <- "T2 SESOI = 0.5"

final_equiv_Szucs_1 <- read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_distribution_equiv_method2_1.0")
final_equiv_Szucs_1$distribution <- "Optimistic"
final_equiv_Szucs_1$trajectory <- "T2 SESOI = 1.0"

final_sig_Carneiro <- read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_sig_method1")
final_sig_Carneiro$distribution <- "Pessimistic"
final_sig_Carneiro$trajectory <- "T1"

final_equiv_Carneiro <- read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_equiv_method2_0.5")
final_equiv_Carneiro$distribution <- "Pessimistic"
final_equiv_Carneiro$trajectory <- "T2 SESOI = 0.5"

final_equiv_Carneiro_1 <- read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_equiv_method2_1.0")
final_equiv_Carneiro_1$distribution <- "Pessimistic"
final_equiv_Carneiro_1$trajectory <- "T2 SESOI = 1.0"

final <- bind_rows(final_sig_Szucs,
                   final_equiv_Szucs,
                   final_equiv_Szucs_1,
                   final_sig_Carneiro,
                   final_equiv_Carneiro,
                   final_equiv_Carneiro_1)


final_init_samp_size_10 <-
  final %>% 
  filter(init_sample_size == 10)

### add column for outcome significant / not significant to match outcome column
### of sequential design data set
final_init_samp_size_10$H0 <- ifelse(final_init_samp_size_10$p_value <= .05, 2, 1)


final_init_samp_size_10 <-
  final_init_samp_size_10 %>% 
  group_by(distribution, trajectory) %>% 
  filter(H0 == 2)

# hist(final_init_samp_size_10$ES_true, breaks = 100)
# max(final_init_samp_size_10$ES_true)
# median(final_init_samp_size_10$ES_true)
# mean(final_init_samp_size_10$ES_true)
# 
# hist(final_init_samp_size_10$effect, breaks = 100)
# max(final_init_samp_size_10$effect)
# median(final_init_samp_size_10$effect)
# mean(final_init_samp_size_10$effect)
# 
# plot(final_init_samp_size_10$effect)
# plot(final_init_samp_size_10$ES_true)

### compute CI around effect size

final_init_samp_size_10$t_value <- ifelse(final_init_samp_size_10$t_value < 0,
                                          - final_init_samp_size_10$t_value, 
                                          - final_init_samp_size_10$t_value)

final_init_samp_size_10$CI_lower <- tes(t = final_init_samp_size_10$t_value, 
                        n.1 = final_init_samp_size_10$rep_sample_size, 
                        n.2 = final_init_samp_size_10$rep_sample_size)$l.d

final_init_samp_size_10$CI_upper <- tes(t = final_init_samp_size_10$t_value, 
                        n.1 = final_init_samp_size_10$rep_sample_size, 
                        n.2 = final_init_samp_size_10$rep_sample_size)$u.d

# final_init_samp_size_10$median_ES_true <- median(final_init_samp_size_10$ES_true)
# final_init_samp_size_10$median_effect <- median(final_init_samp_size_10$effect)


plot_data <-
  final_init_samp_size_10 %>% 
  group_by(distribution, trajectory) %>% 
  summarize(mean_effect = mean(effect),
            median_effect = median(effect),
            mean_CI_lower = mean(CI_lower),
            mean_CI_upper = mean(CI_upper),
            median_ES_true = median(ES_true),
            mean_ES_true = mean(ES_true))

ggplot(data = plot_data) +
  facet_wrap(~ distribution) +
  geom_errorbar(aes(x = trajectory, 
                    ymin = mean_effect - mean_CI_lower,
                    ymax = mean_effect + mean_CI_upper),
                width = 0.09,
                color = "red",
                position = "dodge") +
  geom_point(aes(x = trajectory, y = mean_effect), 
             size = 2.5, color = "red") +
  geom_point(aes(x = trajectory, y = median_ES_true),
             size = 2.5, color = "blue") +
  labs(x = "Trajectory", y = "Mean effect size (with 95% CI)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(strip.text.x = element_text(size = 15, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black"))
  # theme(legend.title = element_text(size = 15, face = "bold")) +
  # theme(legend.text = element_text(size = 14))

# sum(final_init_samp_size_10$effect > final_init_samp_size_10$ES_true)
# sum(final_init_samp_size_10$effect < final_init_samp_size_10$ES_true)
