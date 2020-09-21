setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")


outcomes_BF3  <- read.csv(file = "./data/Carneiro_distribution/Bayes_factor_analysis/outcomes_all_SESOI_BF3")
outcomes_BF10 <- read.csv(file = "./data/Carneiro_distribution/Bayes_factor_analysis/outcomes_all_SESOI_BF10")


outcomes_BF3$BF_crit <- 3
outcomes_BF10$BF_crit <- 10

outcomes <- bind_rows(outcomes_BF3, outcomes_BF10)

outcomes <-
  outcomes %>% 
  filter(init_sample_size == 10)

outcomes$trajectory <- interaction(outcomes$decision_crit, outcomes$sampsize_approach)

outcomes <-
  outcomes %>% 
  filter(trajectory != "significance.SESOI") %>% 
  filter(trajectory != "equivalence.standard") %>% 
  filter(trajectory != "significance_0.1.SESOI")

plot_data <-
  outcomes %>% 
  select(SESOI, init_sample_size, trajectory, 
         Prevalence, PPV_pop_prev, mean_N, FPR, BF_crit) 

plot_mean_N <-
  ggplot(data = plot_data,
         aes(x = factor(trajectory), y = mean_N, fill = factor(SESOI))) +
  geom_bar(stat = "identity", position = "dodge", 
           alpha = 0.7, color = "black", size = .3) +
  facet_wrap(~ BF_crit) +
  # ggtitle("Mean number of animals for each trajectory") +
  labs(x = "Trajectory", y = "Mean # of animals", 
       fill = "SESOI") +
  scale_x_discrete(labels = c("Equivalence \nSESOI", "Significance \nStandard", "Significance at 0.1 \nStandard")) +
  scale_fill_manual(breaks = c("0.5", "1"), 
                    values = c("steelblue", "deeppink3")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 18, colour = "black")) +
  # theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 20, face = "bold")) +
  theme(legend.text = element_text(size = 18)) +
  theme(title = element_text(size = 15))

plot_mean_N

###------------------

facet_names <- c(
  "0.5" = "SESOI = 0.5",
  "1" = "SESOI = 1.0")

plot_PPV <-
  ggplot(data = plot_data, 
         aes(x = factor(trajectory), y = PPV_pop_prev)) +
  geom_point(size = 4, alpha = .8) +
  facet_grid(SESOI ~ BF_crit, labeller = labeller(.rows = facet_names)) +
  # facet_wrap(~ SESOI, labeller = as_labeller(facet_names)) +
  # ggtitle("Positive predictive value for each trajectory") +
  labs(x = "Trajectory", y = "Positive predictive value") +
  scale_x_discrete(labels = c("SESOI within CI \nSESOI", "Significance \nStandard", "Significance at 0.1 \nStandard")) +
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

hlines <- data.frame(pre_study_odds = c(plot_data$Prevalence[4], plot_data$Prevalence[1]), SESOI = c("0.5", "1")) 

plot_PPV <- 
  plot_PPV + 
  geom_hline(data = hlines, 
             aes(yintercept = pre_study_odds),
             color = "red", lty = 2, size = 1)

plot_PPV

###------------------

plot_FPR <-
  ggplot(data = plot_data, 
         aes(x = factor(trajectory), y = FPR, color = factor(BF_crit))) +
  geom_point(size = 4, alpha = .8) +
  facet_wrap(~ SESOI, labeller = as_labeller(facet_names)) +
  # facet_grid(SESOI ~ distribution, labeller = labeller(.rows = facet_names)) +
  # ggtitle("False positive rate for each trajectory") +
  labs(x = "Trajectory", y = "False positive rate") +
  scale_x_discrete(labels = c("SESOI within CI \nSESOI", "Significance \nStandard", "Significance at 0.1 \nStandard")) +
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





