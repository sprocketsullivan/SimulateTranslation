
library(tidyverse)

source("./scripts/functions_for_sim.R")

# create some ES distribution
ES_true <- c(rbeta(1000000, 4.5, 5))
hist(ES_true)

ES_true <- data.frame(ES_true)
median_true <- median(ES_true$ES_true)
mean_true   <- mean(ES_true$ES_true)

ggplot(data = ES_true,
       aes(x = ES_true)) +
  geom_density(aes(y = 0.01 * ..density..), 
               color = "black", fill = "black", 
               alpha = .1, size = .8) +
  # geom_histogram(bins = 50, color = "black", fill = "white", size = 0.3) +
  labs(x = expression(paste("Effect size (Cohen's ", italic("d"),") ")),
       y = "Density") +
  # ggtitle("") +
  geom_vline(xintercept = mean_true, color = "steelblue", size = 1.5) +
  geom_vline(xintercept = median_true, color = "deeppink3", size = 1.1) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank())

ggsave("./plots/ES_population.svg")



n_exp <- 10000
current_ES <- sample(ES_true, n_exp)
current_ES <- data.frame(current_ES)

median_sample <- median(current_ES$current_ES)
mean_sample <- mean(current_ES$current_ES)

ggplot(data = current_ES,
       aes(x = current_ES)) +
  geom_histogram(bins = 50, color = "black", fill = "white", size = 0.3) +
  labs(x = expression(paste("Sampled effect sizes (Cohen's ", italic("d"),") ")),
       y = "Frequency") +
  # ggtitle("Distribution of effct sizes for replication") +
  geom_vline(xintercept = median_sample, color = "deeppink3", size = 1.1) +
  geom_vline(xintercept = mean_sample, color = "steelblue", size = .8) +
  # annotate("text", x = 160, y = 1500,
  #          label = "paste(italic(Mean), \" = 51.7\")", parse = TRUE,
  #          color = "blueviolet", size = 4.5) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank())

ggsave("./plots/ES_sample.svg")


dat <-
  dat %>% 
  filter(init_sample_size == 10)

dat$effect <- ifelse(dat$effect < 0, -dat$effect, -dat$effect)

median_exploratory <- median(dat$effect)
mean_exploratory <- mean(dat$effect)


ggplot(data = dat,
       aes(x = effect)) +
  geom_histogram(bins = 50, color = "black", fill = "white", size = 0.3) +
  labs(x = expression(paste("Effect sizes (Cohen's ", italic("d"), ") from exploratory studies ")),
       y = "Frequency") +
  # ggtitle("Distribution of effct sizes for replication") +
  # geom_vline(xintercept = median_exploratory, color = "deeppink3", size = 1.1) +
  # geom_vline(xintercept = mean_exploratory, color = "steelblue", size = .8) +
  xlim(-3, 5) +
  # annotate("text", x = 160, y = 1500,
  #          label = "paste(italic(Mean), \" = 51.7\")", parse = TRUE,
  #          color = "blueviolet", size = 4.5) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank())

ggsave("./plots/exploratory_ES.svg")

