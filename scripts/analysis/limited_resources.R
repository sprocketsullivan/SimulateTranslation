setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

rm(list = ls())

library(tidyverse)
library(viridis)

#source("./scripts/calculate_outcomes_onesided_across_trajectory.R")

outcomes <- read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_outcomes_all_SESOI_combined")

n_animals    <- 1000000
n_exp        <- 10000


outcomes <-
  outcomes %>% 
  filter(init_sample_size == 10 &
         SESOI != 0.3) %>% 
  filter(SESOI != 0.7)


outcomes$trajectory <- interaction(outcomes$decision_crit, outcomes$sampsize_approach)

outcomes <- 
  outcomes %>% 
  select(init_sample_size, SESOI, trajectory,
         Sensitivity, Specificity, FPR, FNR, 
         rep_attempts, Prevalence, PPV_pop_prev, mean_N) %>% 
  mutate(per_not_sel = floor(n_exp - rep_attempts) / 10000,
         n_stage1 = init_sample_size * 2,
         n_trajectory = ceiling(init_sample_size * 2 + mean_N * 2),
         n_poss_exp = floor(n_animals / (per_not_sel * n_stage1 + (1 - per_not_sel) * n_trajectory)),
         ES_detected = n_poss_exp * PPV_pop_prev,
         waste = floor(per_not_sel * n_poss_exp * n_stage1),
         detection_rate = ES_detected / n_poss_exp * 100 )
         

# outcomes$decision_crit <- as.factor(outcomes$decision_crit)
# levels(outcomes$decision_crit)
# levels(outcomes$decision_crit) <- c("Equivalence", "Significance")

facet_names <- c(
  "0.5" = "SESOI = 0.5",
  "1" = "SESOI = 1.0")

ggplot(data = outcomes,
       aes(x = trajectory, y = detection_rate)) +
  geom_point(size = 3.5, alpha = .6) +
  facet_wrap(~ SESOI, labeller = as_labeller(facet_names)) +
  ggtitle("Detection rate for each trajectory") +
  labs(x = "Trajectory", y = "% of experiments in which \ntrue effect was detected", 
       fill = "SESOI") +
  scale_x_discrete(labels = c("Equivalence \nSESOI", "Significance \nSESOI",
                              "Equivalence \nStandard", "Significance \nStandard")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, colour = "black")) +
  theme(axis.text.y = element_text(size = 14, colour = "black")) +
  theme(strip.text.x = element_text(size = 16, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 14, face = "bold")) +
  theme(legend.text = element_text(size = 14)) +
  theme(title = element_text(size = 15))

ggsave("./plots/Carneiro_limited_resources.svg")


ggplot(data = outcomes,
       aes(x = mean_N, y = waste, 
           color = PPV_pop_prev, shape = factor(sampsize_approach))) +
  geom_point(size = 4) +
  facet_grid(SESOI ~ decision_crit) +
  labs(x = "Mean # of animals", y = "# of experiments terminated \nafter exploratory stage", 
       color = "Positive \npreditive value",
       shape = "Approach to determine \nsample size") +
  # scale_x_discrete(labels = c("SESOI",
  #                             "Standard")) +
  scale_color_gradient2(low = "steelblue", high = "deeppink3",
                        mid = "grey17", midpoint = 0.5) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 11, face = "bold")) +
  theme(legend.text = element_text(size = 11)) +
  theme(legend.position = "top")


ggplot(data = outcomes,
       aes(x = ES_detected, y = waste, 
           color = mean_N,
           shape = factor(sampsize_approach))) +
  geom_point(size = 3.5) +
  # scale_color_viridis(option = "D") +
  facet_grid( SESOI ~ decision_crit) +
  labs(x = "# of experiments in which true effect \nwas detected (thousands) ",
       y = "# of experiments terminated \nafter exploratory stage (thousands)", 
       color = "Mean # of \nanimals",
       shape = "Approach to determine \nsample size") +
  # scale_x_discrete(labels = c("SESOI",
  #                             "Standard")) +
  scale_color_gradient2(low = "steelblue", high = "deeppink3",
                        mid = "grey17", midpoint = 30) +
  scale_x_continuous(breaks = c(4000, 8000, 12000, 16000), labels = c(4, 8, 12, 16)) +
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000, 500000), 
                     labels = c(100, 200, 300, 400, 500)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 11, face = "bold")) +
  theme(legend.text = element_text(size = 11)) +
  theme(legend.position = "right")

ggsave("./plots/Szucs_limited_resources_vs_waste.svg")

  
  
  
  