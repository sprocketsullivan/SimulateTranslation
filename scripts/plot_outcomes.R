setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")


source("./scripts/calculating_outcomes_onesided.R")

plot1 <- 
  ggplot(outcomes, aes(x = factor(design), y = factor(sampsize_approach))) +
  geom_raster(aes(fill = mean_N), interpolate = F) +
  # scale_fill_gradient2(low = "navy", high = "darkgoldenrod1", 
  #                      mid = "white", midpoint = 100) +
  labs(x = "Design", y = "Sample size calculation",
       fill = "Mean # of \nanimals needed") +
  scale_x_discrete(labels = c("Fixed-N \ndesign",
                              "Sequential \ndesign")) +
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
  scale_x_discrete(labels = c("Fixed-N \ndesign",
                              "Sequential \ndesign")) +
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
  geom_raster(aes(fill = PPV_sample_prev), interpolate = F) +
  # scale_fill_gradient2(low = "navy", high = "darkgoldenrod1", 
  #                      mid = "white", midpoint = 45) +
  labs(x = "Design", y = "Sample size calculation",
       fill = "Positive predictive \nvalue") +
  scale_x_discrete(labels = c("Fixed-N \ndesign",
                              "Sequential \ndesign")) +
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

ggsave("./plots/final_plot.png")



source("./scripts/calculating_outcomes_onesided_high_prestudy_odds.R")

plot1 <- 
  ggplot(outcomes, aes(x = factor(design), y = factor(sampsize_approach))) +
  geom_raster(aes(fill = mean_N), interpolate = F) +
  # scale_fill_gradient2(low = "navy", high = "darkgoldenrod1", 
  #                      mid = "white", midpoint = 100) +
  labs(x = "Design", y = "Sample size calculation",
       fill = "Mean # of \nanimals needed") +
  scale_x_discrete(labels = c("Fixed-N \ndesign",
                              "Sequential \ndesign")) +
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
  scale_x_discrete(labels = c("Fixed-N \ndesign",
                              "Sequential \ndesign")) +
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
  geom_raster(aes(fill = PPV_sample_prev), interpolate = F) +
  # scale_fill_gradient2(low = "navy", high = "darkgoldenrod1", 
  #                      mid = "white", midpoint = 45) +
  labs(x = "Design", y = "Sample size calculation",
       fill = "Positive predictive \nvalue") +
  scale_x_discrete(labels = c("Fixed-N \ndesign",
                              "Sequential \ndesign")) +
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

ggsave("./plots/final_plot_high_prestudy_odds.png")


