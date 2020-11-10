setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

rm(list = ls())

library(pwr)
library(tidyverse)

outcomes <- read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_distribution_outcomes_all_SESOI_combined")

outcomes$trajectory <- interaction(outcomes$decision_crit, outcomes$sampsize_approach)


outcomes_10EU <- 
  outcomes %>% 
  filter(init_sample_size == 10) %>% 
  filter(mean_N != 25)

# save(outcomes_10EU,
#      file = "./Manuscript/internal_revision/outcomes_Szucs_all_trajectories.RData")

plot_PPV <- 
  ggplot(data = outcomes_10EU,
         aes(x = trajectory, y = PPV_pop_prev)) + 
  geom_point(size = 2) + 
  facet_wrap(~ SESOI) + 
  labs(x = "Trajectory", y = "Positive predictive value") +
  scale_x_discrete(labels = c("SESOI within CI \nSESOI",
                              "Significance \nSESOI",
                              "SESOI within CI \nStandard",
                              "Significance \nStandard")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(strip.text.x = element_text(size = 15, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 15, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 14))

hlines <- data.frame(pre_study_odds = c(outcomes_10EU$Prevalence[1], outcomes_10EU$Prevalence[5],
                                        outcomes_10EU$Prevalence[9], outcomes_10EU$Prevalence[13]), 
                     # distribution   = c(rep(plot_data$distribution[1], 2), 
                     #                    rep(plot_data$distribution[5], 2)),
                     SESOI          = rep(c("1", "0.7", "0.5", "0.3"))) 


plot_PPV <- 
  plot_PPV + 
  geom_hline(data = hlines, 
             aes(yintercept = pre_study_odds),
             color = "red", lty = 2, size = .5)
