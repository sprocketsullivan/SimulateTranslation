setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

rm(list = ls())

library(svglite)
library(tidyverse)


#source("./scripts/calculate_outcomes_onesided_across_trajectory.R")

out1 <- read.csv(file = "./data/Carneiro_outcomes_equiv_method1_1.0")
out2 <- read.csv(file = "./data/Carneiro_outcomes_equiv_method2_1.0")
out3 <- read.csv(file = "./data/Carneiro_outcomes_sig_method1_1.0")
out4 <- read.csv(file = "./data/Carneiro_outcomes_sig_method2_1.0")


outcomes1 <- bind_rows(out1, out2, out3, out4)
outcomes1$SESOI <- 1.0

out5 <- read.csv(file = "./data/Carneiro_outcomes_equiv_method1_0.7")
out6 <- read.csv(file = "./data/Carneiro_outcomes_equiv_method2_0.7")
out7 <- read.csv(file = "./data/Carneiro_outcomes_sig_method1_0.7")
out8 <- read.csv(file = "./data/Carneiro_outcomes_sig_method2_0.7")


outcomes2 <- bind_rows(out5, out6, out7, out8)
outcomes2$SESOI <- 0.7

out9  <- read.csv(file = "./data/Carneiro_outcomes_equiv_method1_0.5")
out10 <- read.csv(file = "./data/Carneiro_outcomes_equiv_method2_0.5")
out11 <- read.csv(file = "./data/Carneiro_outcomes_sig_method1_0.5")
out12 <- read.csv(file = "./data/Carneiro_outcomes_sig_method2_0.5")


outcomes3 <- bind_rows(out9, out10, out11, out12)
outcomes3$SESOI <- 0.5

out13 <- read.csv(file = "./data/Carneiro_outcomes_equiv_method1_0.3")
out14 <- read.csv(file = "./data/Carneiro_outcomes_equiv_method2_0.3")
out15 <- read.csv(file = "./data/Carneiro_outcomes_sig_method1_0.3")
out16 <- read.csv(file = "./data/Carneiro_outcomes_sig_method2_0.3")

outcomes4 <- bind_rows(out13, out14, out15, out16)
outcomes4$SESOI <- 0.3

# out17 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_25EU_per_group_0.3")
# out18 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_25EU_per_group_0.5")
# out19 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_25EU_per_group_0.7")
# out20 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_25EU_per_group_1.0")
# 
# outcomes5 <- bind_rows(out17, out18, out19, out20)
# outcomes5$SESOI <- rep(c(0.3, 0.5, 0.7, 1.0), each = 3)

outcomes <- bind_rows(outcomes1, outcomes2, outcomes3, outcomes4)

#write.csv(outcomes, file = "./data/Carneiro_distribution_outcomes_all_SESOI_combined")

outcomes <-
  outcomes %>% 
  filter(init_sample_size == 10 &
           SESOI != 0.3) %>% 
  filter(SESOI != 0.7)

outcomes$trajectory <- interaction(outcomes$decision_crit, outcomes$sampsize_approach)

outcomes <-
  outcomes %>% 
  filter(trajectory != "significance.SESOI") %>% 
  filter(trajectory != "equivalence.standard")

plot_data <-
  outcomes %>% 
  filter(H0 == 2, SESOI == 0.5) %>% 
  select(SESOI, init_sample_size, trajectory, 
         Prevalence, PPV_pop_prev, mean_N, FPR, FNR, FDR) %>% 
  mutate(distribution = "Pessimistic")

# write.csv(plot_data, file = "./data/Carneiro_distribution_plot_data_Zurich")


# plot_data$decision_crit <- as.factor(plot_data$decision_crit)
# levels(plot_data$decision_crit)
# levels(plot_data$decision_crit) <- c("Equivalence", "Significance")

plot_mean_N <-
  ggplot(data = plot_data,
         aes(x = factor(trajectory), y = mean_N, fill = factor(SESOI))) +
  geom_bar(stat = "identity", position = "dodge", 
           alpha = 0.7, color = "black", size = .3) +
  # facet_wrap(~ distribution) +
  # ggtitle("Mean number of animals for each trajectory") +
  labs(x = "Trajectory", y = "Mean # of animals", 
       fill = "SESOI") +
  scale_x_discrete(labels = c("Equivalence \nSESOI", "Significance \nStandard")) +
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

# ggsave("./plots/Carneiro_mean_N_REC.svg")



facet_names <- c(
  "0.5" = "SESOI = 0.5",
  "1" = "SESOI = 1.0")

plot_PPV <-
  ggplot(data = plot_data, 
         aes(x = factor(trajectory), y = PPV_pop_prev)) +
  geom_point(size = 4, alpha = .8) +
  # facet_grid(SESOI ~ distribution, labeller = labeller(.rows = facet_names)) +
  # facet_wrap(~ SESOI, labeller = as_labeller(facet_names)) +
  # ggtitle("Positive predictive value for each trajectory") +
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

hlines <- data.frame(pre_study_odds = c(plot_data$Prevalence[1]), SESOI = c("0.5")) 

plot_PPV <- 
  plot_PPV + 
  geom_hline(data = hlines, 
             aes(yintercept = pre_study_odds),
             color = "red", lty = 2, size = 1)

plot_PPV

ggsave("./plots/Carneiro_PPV_across_trajectory_REC.svg")


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

ggsave("./plots/Carneiro_FPR_across_trajectory_REC.svg")



ggplot(data = plot_data,
       aes(y = mean_N, x = factor(SESOI),
           color = PPV_pop_prev)) +
  geom_point(size = 3) +
  scale_color_gradient2(low = "darkblue", high = "orange",
                        mid = "blue", midpoint = 0.5) +
  facet_grid(sampsize_approach ~ decision_crit) +
  labs(x = "SESOI", y = "Mean # of animals", 
       color = "Positive predictive \nvalue") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 10, colour = "black")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 11, face = "bold")) +
  theme(legend.text = element_text(size = 11))



plot1 <- 
  ggplot(outcomes, aes(x = factor(design), y = factor(sampsize_approach))) +
  geom_raster(aes(fill = mean_N), interpolate = F) +
  # scale_fill_gradient2(low = "navy", high = "darkgoldenrod1", 
  #                      mid = "white", midpoint = 100) +
  labs(x = "Design", y = "Sample size calculation",
       fill = "Mean # of \nanimals needed") +
  scale_y_discrete(labels = c("Safeguard", "Standard", "SESOI")) +
  facet_wrap(~ decision_crit) +
  theme_classic() +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size = 10, colour = "black",
                                   angle = 45)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  guides(fill = guide_colorbar(barwidth = 0.7, barheight = 6))


plot2 <-
  ggplot(outcomes, aes(x = factor(design), y = factor(sampsize_approach))) +
  geom_raster(aes(fill = 1-Sensitivity), interpolate = F) +
  # scale_fill_gradient2(low = "navy", high = "darkgoldenrod1",
  #                      mid = "white", midpoint = 40) +
  labs(x = "Design", y = "Approach to determine \nsample size",
       fill = "False negative \nrate") +
  scale_y_discrete(labels = c("Safeguard", "Standard", "SESOI")) +
  facet_wrap(~ decision_crit) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size = 10, colour = "black",
                                   angle = 45)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  guides(fill = guide_colorbar(barwidth = 0.7, barheight = 6))


plot3 <- 
  ggplot(outcomes, aes(x = factor(design), y = factor(sampsize_approach))) +
  geom_raster(aes(fill = PPV_pop_prev), interpolate = F) +
  # scale_fill_gradient2(low = "navy", high = "darkgoldenrod1", 
  #                      mid = "white", midpoint = 45) +
  labs(x = "Design", y = "Sample size calculation",
       fill = "Positive predictive \nvalue") +
  scale_x_discrete(labels = c("Fixed-N",
                              "Sequential",
                              "Sequential \nw/ futility")) +
  scale_y_discrete(labels = c("Safeguard", "Standard", "SESOI")) +
  facet_wrap(~ decision_crit) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y =element_blank()) +
  theme(axis.text.x = element_text(size = 10, colour = "black")) +
  theme(axis.text.y = element_text(size = 10, colour = "black",
                                   angle = 45)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  guides(fill = guide_colorbar(barwidth = 0.7, barheight = 6))

#plot3

final_plot <-
  ggarrange(plot1, plot2, plot3, 
            nrow = 3, ncol = 1,
            align = "v",
            heights = c(1, .9, 1.05),
            legend = "right")

#ggsave("./plots/final_plot_across_trajectory.png")


# ggplot(data = outcomes, aes(x = mean_N, y = PPV_pop_prev, color = factor(design))) +
#   geom_point(size = 2) +
#   facet_grid(sampsize_approach ~ decision_crit) +
#   labs(x = "Mean # of animals needed", y = "Positive predictive value") +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "white", color = "black")) +
#   theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
#   theme(strip.text.y = element_text(size = 11, colour = "black", face = "bold")) +
#   theme(axis.title.x = element_text(size = 13)) +
#   theme(axis.title.y = element_text(size = 13)) +
#   theme(axis.text.x = element_text(size = 10, colour = "black")) +
#   theme(axis.text.y = element_text(size = 10, colour = "black")) +
#   theme(legend.title = element_text(size = 11, face = "bold")) +
#   theme(legend.text = element_text(size = 11)) +
#   theme(legend.position = "top")

