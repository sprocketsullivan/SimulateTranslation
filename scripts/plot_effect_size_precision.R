setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

#rm(list = ls())
library(tidyverse)

### read in data sets from the different trajectories 
### using fixed-N design in confirmatory study

final <- read.csv(file = "./data/Szucs_distribution_sig_method1")


final <-
  final %>% 
  filter(init_sample_size == 10)

# & p_value < .05

max(final$effect)
md_szucs   = median(final$effect)
mean_szucs = mean(final$effect)

final <-
  final %>% 
  filter(effect < 5)


ggplot(data = final,
       aes(x = effect)) +
  geom_histogram(bins = 50, color = "black", fill = "white", size = 0.3) +
  labs(x = "Empirical effect size", y = "Frequency") +
  ggtitle("Distribution of effect sizes after replication") +
  geom_vline(xintercept = md_szucs, color = "darkcyan", size = 1) +
  geom_vline(xintercept = mean_szucs, color = "blueviolet", size = 1) +
  # facet_wrap( ~ SESOI) +
  annotate("text", x = 3.5, y = 400,
           label = "paste(italic(Median), \" = 0.66\")", parse = TRUE,
           color = "darkcyan", size = 4.5) +
  annotate("text", x = 3.65, y = 350,
           label = "paste(italic(Mean), \" = 1.04\")", parse = TRUE,
           color = "blueviolet", size = 4.5) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black"))

ggsave("./plots/Szucs_distribution_after_replication_sig_method1.png")
