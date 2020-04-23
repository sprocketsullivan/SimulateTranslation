setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

library(svglite)


source("./scripts/calculate_outcomes_onesided_across_trajectory.R")

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

ggsave("./plots/final_plot_across_trajectory.png")


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



PPV_plot <-
  outcomes %>% 
  select(decision_crit, sampsize_approach, design, Prevalence, PPV_pop_prev, mean_N, FPR, -H0)
  

PPV_plot$trajectory <- 
  interaction(PPV_plot$sampsize_approach, PPV_plot$design)

plot_data <-
  PPV_plot %>% 
  select(trajectory, Prevalence, PPV_pop_prev, mean_N, FPR)

plot_data$decision_crit <- as.factor(plot_data$decision_crit)
levels(plot_data$decision_crit)
levels(plot_data$decision_crit) <- c("Equivalence", "Significance")

plot_PPV <-
ggplot(data = plot_data, 
       aes(x = design, y = PPV_pop_prev, 
           color = (mean_N), shape = factor(sampsize_approach))) +
  geom_point(size = 3.7) +
  scale_color_gradient(low = "navyblue", high = "orange") +
  scale_shape_discrete(breaks = c("1", "2", "3"), 
                       labels = c("Safeguard", "Standard", "SESOI")) +
  facet_wrap(~ decision_crit, nrow = 1, ncol = 2) +
  labs(x = "Design", y = "Positive predictive value", 
       color = "Mean # of animals", shape = "Approach to calculate \nsample size") +
  scale_x_discrete(labels = c("Fixed-N",
                              "Sequential",
                              "Sequential \nw/ futility")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 10, colour = "black")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 11, face = "bold")) +
  theme(legend.text = element_text(size = 11))

plot_PPV

#ggsave("./plots/outcomes_across_trajectory.svg")


plot_FPR <-
  ggplot(data = plot_data, 
         aes(x = design, y = FPR, 
             color = (mean_N), shape = factor(sampsize_approach))) +
  geom_point(size = 3.7) +
  scale_color_gradient(low = "navyblue", high = "orange") +
  scale_shape_discrete(breaks = c("1", "2", "3"), 
                       labels = c("Safeguard", "Standard", "SESOI")) +
  facet_wrap(~ decision_crit, nrow = 1, ncol = 2) +
  labs(x = "Design", y = "False positive rate", 
       color = "Mean # of animals", shape = "Approach to calculate \nsample size") +
  scale_x_discrete(labels = c("Fixed-N",
                              "Sequential",
                              "Sequential \nw/ futility")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 10, colour = "black")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 11, face = "bold")) +
  theme(legend.text = element_text(size = 11))

plot_FPR

plot_FPR <-
  ggplot(data = plot_data, 
         aes(x = design, y = FPR, 
             color = (mean_N), shape = factor(sampsize_approach))) +
  geom_point(size = 3.7) +
  scale_color_gradient(low = "navyblue", high = "orange") +
  scale_shape_discrete(breaks = c("1", "2", "3"), 
                       labels = c("Safeguard", "Standard", "SESOI")) +
  facet_wrap(~ decision_crit, nrow = 1, ncol = 2) +
  labs(x = "Design", y = "False positive rate", 
       color = "Mean # of animals", shape = "Approach to calculate \nsample size") +
  scale_x_discrete(labels = c("Fixed-N",
                              "Sequential",
                              "Sequential \nw/ futility")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 10, colour = "black")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 11, face = "bold")) +
  theme(legend.text = element_text(size = 11))

plot_FPR


