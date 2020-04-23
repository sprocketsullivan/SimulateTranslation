setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

rm(list = ls())

outcomes_Szucs    <- read.csv(file = "./data/Szucs_distribution_plot_data_Zurich")

# outcomes_Carneiro <- read.csv(file = "./data/Carneiro_distribution_plot_data_Zurich")

plot_data <- bind_rows(outcomes_Szucs, outcomes_Carneiro)

plot_data <- outcomes_Szucs


plot_mean_N <-
  ggplot(data = plot_data,
         aes(x = factor(trajectory), y = mean_N, fill = factor(SESOI))) +
  geom_bar(stat = "identity", position = "dodge", 
           alpha = 0.7, color = "black", size = .3) +
  # facet_wrap(~ distribution) +
  # ggtitle("Mean number of animals for each trajectory") +
  labs(x = "Trajectory", y = "Mean # of animals", 
       fill = "SESOI") +
  scale_x_discrete(labels = c("Equivalence \nSESOI", "Equivalence \nStandard", 
                              "Significance \nSESOI", "Significance \nStandard")) +
  # scale_x_discrete(labels = c("Equivalence \nSESOI", "Significance \nStandard")) +
  scale_fill_manual(breaks = c("0.5", "1"), 
                    values = c("steelblue", "deeppink3")) +
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

ggsave("./plots/mean_N.svg")



facet_names <- c(
  "0.5" = "SESOI = 0.5",
  "1" = "SESOI = 1.0")

plot_PPV <-
  ggplot(data = plot_data, 
         aes(x = factor(trajectory), y = PPV_pop_prev)) +
  geom_point(size = 3.5, alpha = .7) +
  # facet_grid(SESOI ~ distribution, labeller = labeller(.rows = facet_names)) +
  facet_wrap(~ SESOI, labeller = as_labeller(facet_names)) +
  # ggtitle("Positive predictive value for each trajectory") +
  labs(x = "Trajectory", y = "Positive predictive value") +
  # scale_x_discrete(labels = c("Equivalence \nSESOI", "Significance \nStandard")) +
  scale_x_discrete(labels = c("Equivalence \nSESOI", "Equivalence \nStandard", 
                              "Significance \nSESOI", "Significance \nStandard")) +
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

# hlines <- data.frame(pre_study_odds = c(plot_data$Prevalence[3], plot_data$Prevalence[1],
#                                         plot_data$Prevalence[7], plot_data$Prevalence[5]), 
#                      SESOI = c("0.5", "1"),
#                      distribution = rep(c("Optimistic", "Pessimistic"), each = 2)) 

hlines <- data.frame(pre_study_odds = c(plot_data$Prevalence[5], plot_data$Prevalence[1]), 
                     SESOI = c("0.5", "1")) 

plot_PPV <- 
  plot_PPV + 
  geom_hline(data = hlines, 
             aes(yintercept = pre_study_odds),
             color = "red", lty = 2, size = 1)

plot_PPV

ggsave("./plots/PPV_across_trajectory.svg")


plot_FPR <-
  ggplot(data = plot_data, 
         aes(x = factor(trajectory), y = FPR)) +
  geom_point(size = 3.5, alpha = .7) +
  facet_wrap(~ SESOI, labeller = as_labeller(facet_names)) +
  # facet_grid(SESOI ~ distribution, labeller = labeller(.rows = facet_names)) +
  # ggtitle("False positive rate for each trajectory") +
  labs(x = "Trajectory", y = "False positive rate") +
  # scale_x_discrete(labels = c("Equivalence \nSESOI", "Significance \nStandard")) +
  scale_x_discrete(labels = c("Equivalence \nSESOI", "Equivalence \nStandard", 
                              "Significance \nSESOI", "Significance \nStandard")) +
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

ggsave("./plots/FPR_across_trajectory.svg")



