setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

#rm(list = ls())
library(tidyverse)

### read in data sets from the different trajectories 
### using fixed-N design in confirmatory study

before_equiv <- read.csv(file = "./data/Szucs_distribution_equiv_method1_0.5")
# before_equiv <- read.csv(file = "./data/Carneiro_distribution_equiv_method1_0.5")

before_equiv <-
  before_equiv %>% 
  filter(init_sample_size == 10)

max(before_equiv$ES_true)
md_stage1   <- median(before_equiv$ES_true)
mean_stage1 <- mean(before_equiv$ES_true)
mean_N_rep  <- mean(before_equiv$rep_sample_size)

before_equiv <-
  before_equiv %>% 
  filter(ES_true < 5)

ggplot(data = before_equiv,
       aes(x = ES_true)) +
  geom_histogram(bins = 50, color = "black", fill = "white", size = 0.3) +
  labs(x = "True effect size", y = "Frequency") +
  ggtitle("Distribution of effect sizes after equivalence test") +
  geom_vline(xintercept = md_stage1, color = "darkcyan", size = 1) +
  geom_vline(xintercept = mean_stage1, color = "blueviolet", size = 1) +
  annotate("text", x = 3.0, y = 550,
           label = "paste(italic(Median), \" = 0.87\")", parse = TRUE,
           color = "darkcyan", size = 4.5) +
  annotate("text", x = 3.1, y = 500,
           label = "paste(italic(Mean), \" = 0.96\")", parse = TRUE,
           color = "blueviolet", size = 4.5) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black"))
 

# dev.off()

# ggsave("./plots/Szucs_distribution_after_equivalence_test_SESOI_0.5.svg")
# ggsave("./plots/Carneiro_distribution_after_equivalence_test_SESOI_0.5.svg")

ggplot(data = before_equiv,
       aes(x = rep_sample_size)) +
  geom_histogram(bins = 50, color = "black", fill = "white", size = 0.3) +
  labs(x = "Replication sample size", y = "Frequency") +
  ggtitle("Distribution of samples sizes for replication") +
  geom_vline(xintercept = mean_N_rep, color = "blueviolet", size = 1) +
  annotate("text", x = 160, y = 1500,
           label = "paste(italic(Mean), \" = 51.7\")", parse = TRUE,
           color = "blueviolet", size = 4.5) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black"))

# ggsave("./plots/Szucs_distribution_of_sample_sizes_for_rep_after_equivalence_test.png")
# ggsave("./plots/Carneiro_distribution_of_sample_sizes_for_rep_after_equivalence_test.png")

before_sig <- read.csv(file = "./data/Szucs_distribution_sig_method1")
# before_sig <- read.csv(file = "./data/Carneiro_distribution_sig_method1")


before_sig <-
  before_sig %>% 
  filter(init_sample_size == 10)

max(before_sig$ES_true)
md_stage1   <- median(before_sig$ES_true)
mean_stage1 <- mean(before_sig$ES_true)
mean_N_rep  <- mean(before_sig$rep_sample_size)

before_sig <-
  before_sig %>% 
  filter(ES_true < 7)

ggplot(data = before_sig,
       aes(x = ES_true)) +
  geom_histogram(bins = 50, color = "black", fill = "white", size = 0.3) +
  labs(x = "True effect size", y = "Frequency") +
  ggtitle("Distribution of effect sizes after significance test") +
  geom_vline(xintercept = md_stage1, color = "darkcyan", size = 1) +
  geom_vline(xintercept = mean_stage1, color = "blueviolet", size = 1) +
  annotate("text", x = 3.2, y = 320,
           label = "paste(italic(Median), \" = 1.3\")", parse = TRUE,
           color = "darkcyan", size = 4.5) +
  annotate("text", x = 3.35, y = 290,
           label = "paste(italic(Mean), \" = 1.48\")", parse = TRUE,
           color = "blueviolet", size = 4.5) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black"))
 
# ggsave("./plots/Szucs_distribution_after_significance_test.png")
# ggsave("./plots/Carneiro_distribution_after_significance_test.png")


ggplot(data = before_sig,
       aes(x = rep_sample_size)) +
  geom_histogram(bins = 12, color = "black", fill = "white", size = 0.3) +
  labs(x = "Replication sample size", y = "Frequency") +
  ggtitle("Distribution of samples sizes for replication") +
  geom_vline(xintercept = mean_N_rep, color = "blueviolet", size = 1) +
  annotate("text", x = 14, y = 750,
           label = "paste(italic(Mean), \" = 7.19\")", parse = TRUE,
           color = "blueviolet", size = 4.5) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black"))

# ggsave("./plots/Szucs_distribution_of_sample_sizes_for_rep_after_significance_test.png")
# ggsave("./plots/Carneiro_distribution_of_sample_sizes_for_rep_after_significance_test.png")
