setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

rm(list = ls())

library(svglite)


#source("./scripts/calculate_outcomes_onesided_across_trajectory.R")

out1 <- read.csv(file = "./data/Szucs_outcomes_equiv_method1_1.0")
out2 <- read.csv(file = "./data/Szucs_outcomes_equiv_method2_1.0")
out3 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_1.0")
out4 <- read.csv(file = "./data/Szucs_outcomes_sig_method2_1.0")


outcomes1 <- bind_rows(out1, out2, out3, out4)
outcomes1$SESOI <- 1.0

out5 <- read.csv(file = "./data/Szucs_outcomes_equiv_method1_0.7")
out6 <- read.csv(file = "./data/Szucs_outcomes_equiv_method2_0.7")
out7 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_0.7")
out8 <- read.csv(file = "./data/Szucs_outcomes_sig_method2_0.7")


outcomes2 <- bind_rows(out5, out6, out7, out8)
outcomes2$SESOI <- 0.7

out9  <- read.csv(file = "./data/Szucs_outcomes_equiv_method1_0.5")
out10 <- read.csv(file = "./data/Szucs_outcomes_equiv_method2_0.5")
out11 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_0.5")
out12 <- read.csv(file = "./data/Szucs_outcomes_sig_method2_0.5")


outcomes3 <- bind_rows(out9, out10, out11, out12)
outcomes3$SESOI <- 0.5

out13 <- read.csv(file = "./data/Szucs_outcomes_equiv_method1_0.3")
out14 <- read.csv(file = "./data/Szucs_outcomes_equiv_method2_0.3")
out15 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_0.3")
out16 <- read.csv(file = "./data/Szucs_outcomes_sig_method2_0.3")

outcomes4 <- bind_rows(out13, out14, out15, out16)
outcomes4$SESOI <- 0.3

out17 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_25EU_per_group_0.3")
out18 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_25EU_per_group_0.5")
out19 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_25EU_per_group_0.7")
out20 <- read.csv(file = "./data/Szucs_outcomes_sig_method1_25EU_per_group_1.0")

outcomes5 <- bind_rows(out17, out18, out19, out20)
outcomes5$SESOI <- rep(c(0.3, 0.5, 0.7, 1.0), each = 3)

outcomes <- bind_rows(outcomes1, outcomes2, outcomes3, outcomes4, outcomes5)

#write.csv(outcomes, file = "./data/Szucs_distribution_outcomes_all_SESOI_combined")

# outcomes <-
#   outcomes %>% 
#   filter(init_sample_size == 10)

outcomes <-
  outcomes %>% 
  filter(init_sample_size == 10 &
           SESOI != 0.3) %>% 
  filter(SESOI != 0.7)

plot_data <-
  outcomes %>% 
  select(SESOI, init_sample_size, decision_crit, sampsize_approach, 
         Prevalence, PPV_pop_prev, mean_N, FPR, FNR, FDR)
  
  # %>% 
  # filter(mean_N != 25)

plot_data$decision_crit <- as.factor(plot_data$decision_crit)
levels(plot_data$decision_crit)
levels(plot_data$decision_crit) <- c("Equivalence", "Significance")

plot_mean_N <-
  ggplot(data = plot_data,
         aes(x = factor(sampsize_approach), y = mean_N, fill = factor(SESOI))) +
  geom_bar(stat = "identity", position = "dodge", 
           alpha = 0.7, color = "black", size = .3) +
  facet_wrap(~ decision_crit) +
  ggtitle("Mean number of animals for each trajectory") +
  labs(x = "Approach to determine sample size", y = "Mean # of animals", 
       fill = "SESOI") +
  scale_x_discrete(labels = c("SESOI",
                              "Standard")) +
  scale_fill_manual(breaks = c("0.5", "1"), 
                    values = c("steelblue", "deeppink3")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 11, face = "bold")) +
  theme(legend.text = element_text(size = 11))

plot_mean_N

# ggsave("./plots/Szucs_mean_N.svg")


plot_PPV <-
  ggplot(data = plot_data, 
         aes(x = factor(sampsize_approach), y = PPV_pop_prev, 
             color = mean_N)) +
  geom_point(size = 3.5) +
  facet_grid(SESOI ~ decision_crit) +
  ggtitle("Positive predictive value for each trajectory") +
  labs(x = "Approach to determine sample size", y = "Positive predictive value", 
       color = "Mean # of \n animals") +
  scale_x_discrete(labels = c("SESOI",
                              "Standard")) +
  scale_color_gradient2(low = "steelblue", high = "deeppink3",
                        mid = "grey17", midpoint = 30) +
  # scale_color_gradient2(low = "navy", high = "darkorange",
  #                       midpoint = 35) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 11, face = "bold")) +
  theme(legend.text = element_text(size = 11))

plot_PPV

hlines <- data.frame(pre_study_odds = c(outcomes$prev_pop[5], outcomes$prev_pop[1]), 
                     SESOI = c("0.5", "1")) 

plot_PPV <- 
  plot_PPV + 
  geom_hline(data = hlines, 
             aes(yintercept = pre_study_odds),
             color = "red", lty = 2)

plot_PPV

# ggsave("./plots/Szucs_PPV_across_trajectory.svg")
# ggsave("./plots/Szucs_PPV_across_trajectory_with_25.svg")


plot_FPR <-
  ggplot(data = plot_data, 
         aes(x = factor(sampsize_approach), y = FPR, 
             color = mean_N)) +
  geom_point(size = 3.5) +
  facet_grid(SESOI ~ decision_crit) +
  ggtitle("False positive rate for each trajectory") +
  labs(x = "Approach to determine sample size", y = "False positive rate", 
       color = "Mean # of \nanimals") +
  scale_x_discrete(labels = c("SESOI",
                              "Standard")) +
  scale_color_gradient2(low = "steelblue", high = "deeppink3",
                        mid = "grey17", midpoint = 30) +
  # scale_color_gradient2(low = "navy", high = "darkorange",
  #                       mid = "white", midpoint = 35) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 11, face = "bold")) +
  theme(legend.text = element_text(size = 11))

plot_FPR

# ggsave("./plots/Szucs_FPR_across_trajectory.svg")
# ggsave("./plots/Szucs_FPR_across_trajectory_with_25.svg")



plot_FNR <-
  ggplot(data = plot_data, 
         aes(x = factor(sampsize_approach), y = FNR, 
             color = mean_N)) +
  geom_point(size = 3.5) +
  facet_grid(SESOI ~ decision_crit) +
  ggtitle("False negative rate for each trajectory") +
  labs(x = "Approach to determine sample size", y = "False negative rate", 
       color = "Mean # of \nanimals") +
  scale_x_discrete(labels = c("SESOI",
                              "Standard")) +
  scale_color_gradient2(low = "steelblue", high = "deeppink3",
                        mid = "grey17", midpoint = 30) +
  # scale_color_gradient2(low = "navy", high = "darkorange",
  #                       mid = "white", midpoint = 35) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 11, face = "bold")) +
  theme(legend.text = element_text(size = 11))

plot_FNR

# ggsave("./plots/Szucs_FNR_across_trajectory.svg")



# ggplot(data = plot_data,
#        aes(y = mean_N, x = factor(SESOI),
#            color = PPV_pop_prev)) +
#   geom_point(size = 3) +
#   scale_color_gradient2(low = "darkblue", high = "orange",
#                         mid = "blue", midpoint = 0.5) +
#   facet_grid(sampsize_approach ~ decision_crit) +
#   labs(x = "SESOI", y = "Mean # of animals", 
#        color = "Positive predictive \nvalue") +
#   theme_bw() +
#   theme(axis.title.x = element_text(size = 13)) +
#   theme(axis.title.y = element_text(size = 13)) +
#   theme(axis.text.x = element_text(size = 10, colour = "black")) +
#   theme(axis.text.y = element_text(size = 10, colour = "black")) +
#   theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
#   theme(strip.background = element_rect(fill = "white", color = "black")) +
#   theme(legend.title = element_text(size = 11, face = "bold")) +
#   theme(legend.text = element_text(size = 11))
  


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

