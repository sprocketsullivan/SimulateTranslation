setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")


load("./data/all_outcomes.RData")

plot_data <-
  outcomes_all_distributions %>% 
  filter(init_sample_size == 10) %>% 
  group_by(distribution, SESOI)


plot_mean_N <-
  ggplot(data = plot_data,
         aes(x = factor(trajectory), y = mean_N, fill = factor(SESOI))) +
  geom_bar(stat = "identity", position = "dodge",
           size = .3,
           color = "black") +
  geom_errorbar(aes(ymax = mean_N_max, ymin = mean_N_min), width = 0.1,
                position = position_dodge(width = 0.9)) +
  facet_grid(~ distribution) +
  ggtitle("Mean number of animals for each trajectory") +
  labs(x = "Trajectory", y = "Mean # of animals in replication",
       fill = "SESOI") +
  scale_x_discrete(labels = c("SESOI within CI \nSESOI", "Significance \nStandard")) +
  scale_fill_manual(labels = c("0.5", "1.0"), values = c("grey17", "grey47")) + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 18, colour = "black")) +
  theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 20, face = "bold")) +
  theme(legend.text = element_text(size = 18)) +
  theme(title = element_text(size = 15))

plot_mean_N

# ggsave("./Manuscript/plot_mean_N.png")



facet_names <- c(
  "0.5" = "SESOI = 0.5",
  "1" = "SESOI = 1.0")

plot_PPV <-
  ggplot(data = plot_data, 
         aes(x = factor(trajectory), y = PPV_pop_prev)) +
  geom_point(size = 4, alpha = .8) +
  facet_grid(SESOI ~ distribution, labeller = labeller(.rows = facet_names)) +
  ggtitle("Positive predictive value for each trajectory") +
  labs(x = "Trajectory", y = "Positive predictive value") +
  scale_x_discrete(labels = c("SESOI within CI \nSESOI", "Significance \nStandard")) +
  # scale_x_discrete(labels = c("Equivalence \nSESOI", "Significance \nSESOI",
  #                             "Equivalence \nStandard", "Significance \nStandard")) +
  # scale_fill_manual(breaks = c("0.5", "1"), 
  #                   values = c("steelblue", "deeppink3")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 18, colour = "black")) +
  theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 20, face = "bold")) +
  theme(legend.text = element_text(size = 18)) +
  theme(title = element_text(size = 15))

plot_PPV

hlines <- data.frame(pre_study_odds = c(plot_data$Prevalence[1], plot_data$Prevalence[2],
                                        plot_data$Prevalence[5], plot_data$Prevalence[6]), 
                     distribution   = c(rep(plot_data$distribution[1], 2), 
                                        rep(plot_data$distribution[5], 2)),
                     SESOI          = rep(c("1", "0.5"))) 

plot_PPV <- 
  plot_PPV + 
  geom_hline(data = hlines, 
             aes(yintercept = pre_study_odds),
             color = "red", lty = 2, size = 1)

plot_PPV

ggsave("./Manuscript/plot_PPV_across_trajectory.png")


plot_FPR <-
  ggplot(data = plot_data, 
         aes(x = factor(trajectory), y = FPR)) +
  geom_point(size = 4, alpha = .8) +
  # facet_grid(SESOI ~ distribution, labeller = labeller(.rows = facet_names)) +
  # ggtitle("False positive rate for each trajectory") +
  labs(x = "Trajectory", y = "False positive rate") +
  scale_x_discrete(labels = c("SESOI within CI \nSESOI", "Significance \nStandard")) +
  # scale_x_discrete(labels = c("Equivalence \nSESOI", "Significance \nSESOI",
  #                             "Equivalence \nStandard", "Significance \nStandard")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 18, colour = "black")) +
  theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 20, face = "bold")) +
  theme(legend.text = element_text(size = 18)) +
  theme(title = element_text(size = 15))


plot_FPR

# ggsave("./plots/Carneiro_FPR_across_trajectory_REC.svg")
