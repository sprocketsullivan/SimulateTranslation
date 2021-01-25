setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/")

rm(list = ls())

library(tidyverse)
library(compute.es)
library(pwr)
library(ggbeeswarm)


### read in data sets from the different trajectories 
### using fixed-N design in replication study

final_sig_Szucs <- read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_distribution_sig_method1")
final_sig_Szucs$distribution <- "Optimistic"
final_sig_Szucs$trajectory <- "T1"

final_equiv_Szucs <- read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_distribution_equiv_method2_0.5")
final_equiv_Szucs$distribution <- "Optimistic"
final_equiv_Szucs$trajectory <- "T2 SESOI = 0.5"

final_equiv_Szucs_1 <- read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_distribution_equiv_method2_1.0")
final_equiv_Szucs_1$distribution <- "Optimistic"
final_equiv_Szucs_1$trajectory <- "T2 SESOI = 1.0"

final_sig_Carneiro <- read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_sig_method1")
final_sig_Carneiro$distribution <- "Pessimistic"
final_sig_Carneiro$trajectory <- "T1"

final_equiv_Carneiro <- read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_equiv_method2_0.5")
final_equiv_Carneiro$distribution <- "Pessimistic"
final_equiv_Carneiro$trajectory <- "T2 SESOI = 0.5"

final_equiv_Carneiro_1 <- read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_equiv_method2_1.0")
final_equiv_Carneiro_1$distribution <- "Pessimistic"
final_equiv_Carneiro_1$trajectory <- "T2 SESOI = 1.0"

final <- bind_rows(final_sig_Szucs,
                   final_equiv_Szucs,
                   final_equiv_Szucs_1,
                   final_sig_Carneiro,
                   final_equiv_Carneiro,
                   final_equiv_Carneiro_1)


final_init_samp_size_10 <-
  final %>% 
  filter(init_sample_size == 10)

### add column for outcome significant / not significant
final_init_samp_size_10$H0 <- ifelse(final_init_samp_size_10$p_value <= .05, 2, 1)

### filter only significant outcomes 
final_init_samp_size_10 <-
  final_init_samp_size_10 %>% 
  group_by(distribution, trajectory) %>% 
  filter(H0 == 2)

hist(final_init_samp_size_10$ES_true, breaks = 100)
max(final_init_samp_size_10$ES_true)
median(final_init_samp_size_10$ES_true)
mean(final_init_samp_size_10$ES_true)


hist(final_init_samp_size_10$effect, breaks = 100)
max(final_init_samp_size_10$effect)
median(final_init_samp_size_10$effect)
mean(final_init_samp_size_10$effect)

# plot(final_init_samp_size_10$effect)
# plot(final_init_samp_size_10$ES_true)

### compute CI around effect size
final_init_samp_size_10$t_value <- ifelse(final_init_samp_size_10$t_value < 0,
                                          - final_init_samp_size_10$t_value, 
                                          - final_init_samp_size_10$t_value)

final_init_samp_size_10$CI_lower <- tes(t = final_init_samp_size_10$t_value, 
                        n.1 = final_init_samp_size_10$rep_sample_size, 
                        n.2 = final_init_samp_size_10$rep_sample_size)$l.d

final_init_samp_size_10$CI_upper <- tes(t = final_init_samp_size_10$t_value, 
                        n.1 = final_init_samp_size_10$rep_sample_size, 
                        n.2 = final_init_samp_size_10$rep_sample_size)$u.d

# save(final_init_samp_size_10,
#      file = "./Manuscript/effect_sizes_with_CI_after_replication.RData")

# final_init_samp_size_10$median_ES_true <- median(final_init_samp_size_10$ES_true)
# final_init_samp_size_10$median_effect <- median(final_init_samp_size_10$effect)

plot_data <-
  final_init_samp_size_10 %>% 
  group_by(distribution, trajectory) %>% 
  summarize(mean_effect = mean(effect),
            median_effect = median(effect),
            mean_CI_lower = mean(CI_lower),
            mean_CI_upper = mean(CI_upper),
            median_ES_true = median(ES_true),
            mean_ES_true = mean(ES_true))

colors <- c("Median of true effect sizes" = "blue", 
            "Mean of empirical effect sizes \nwith 95% CI" = "red")

ggplot(data = plot_data,
       aes(x = trajectory)) +
  facet_wrap(~ distribution) +
  geom_errorbar(aes(ymin = mean_effect - mean_CI_lower,
                    ymax = mean_effect + mean_CI_upper),
                width = 0.09,
                color = "red",
                position = "dodge") +
  geom_point(aes(y = mean_effect, color = "Mean of empirical effect sizes \nwith 95% CI"), size = 2.5) +
  geom_point(aes(y = median_ES_true, color = "Median of true effect sizes"), size = 2.5) +
  labs(x = "Trajectory",
       y = "Mean effect size (with 95% CI)",
       color = " ") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12, colour = "black")) +
  theme(axis.text.y = element_text(size = 12, colour = "black")) +
  theme(strip.text.x = element_text(size = 15, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  scale_color_manual(values = colors) + 
  theme(legend.position =  "top") +
  theme(legend.text = element_text(size = 12))

# sum(final_init_samp_size_10$effect > final_init_samp_size_10$ES_true)
# sum(final_init_samp_size_10$effect < final_init_samp_size_10$ES_true)


T1_data <-
  final_init_samp_size_10 %>% 
  filter(trajectory == "T1") %>% 
  filter(effect < 5)

mean_N <- 
  T1_data %>% 
  group_by(distribution) %>% 
  summarize(mean_N = mean(rep_sample_size))

ceiling(mean(T1_data$rep_sample_size))

powered_d_50 <-
  pwr.t.test(n = ceiling(mean(T1_data$rep_sample_size)),
             d = NULL, sig.level = .05, power = .5,
             type = "two.sample",
             alternative = "greater")$d

powered_d_80 <-
  pwr.t.test(n = ceiling(mean(T1_data$rep_sample_size)),
             d = NULL, sig.level = .05, power = .8,
             type = "two.sample",
             alternative = "greater")$d
  
ggplot(data = T1_data,
       aes(x = distribution, y = effect)) +
  # facet_wrap(~ distribution) +
  geom_violin(position = position_dodge(width = 0.9)) +
  geom_rect(fill = "lightgrey", alpha = .01, 
            aes(xmin = -Inf, xmax = Inf, 
                ymin = powered_d_80, ymax = Inf)) +
  # geom_rect(fill = "lightpink", alpha = .01, 
  #           aes(xmin = -Inf, xmax = Inf, 
  #               ymin = powered_d_50, ymax = powered_d_80)) +
  theme_bw()
  # geom_quasirandom(dodge.width = 0.9,
  #                  varwidth = FALSE,
  #                  size = .5, alpha = 0.3)
  

  
delta_powered <- NULL

for(i in 1:nrow(T1_data)) {
  
  delta_powered[i] <- 
    pwr.t.test(n = T1_data$rep_sample_size[i],
               d = NULL, sig.level = .05, power = .8,
               type = "two.sample",
               alternative = "greater")$d
  
}

T1_data$delta_powered <- delta_powered

T1_data_optimistic <-
  T1_data %>% 
  filter(distribution == "Optimistic")

significant_effect <- data.frame(effect = T1_data_optimistic$effect)
significant_effect$type <- "significant"

powered_effect <- data.frame(effect = T1_data_optimistic$delta_powered)
powered_effect$type <- "powered"

test <- bind_rows(significant_effect, powered_effect)

ggplot(data = test,
       aes(x = type,
           y = effect)) +
  geom_violin()

ggplot(data = test,
       aes(x = effect, fill = type)) +
  geom_density(alpha = 0.7) +
  theme_bw()


T1_data_pessimistic <-
  T1_data %>% 
  filter(distribution == "Pessimistic")

significant_effect <- data.frame(effect = T1_data_pessimistic$effect)
significant_effect$type <- "significant"

powered_effect <- data.frame(effect = T1_data_pessimistic$delta_powered)
powered_effect$type <- "powered"

test <- bind_rows(significant_effect, powered_effect)

ggplot(data = test,
       aes(x = type,
           y = effect)) +
  geom_violin()

ggplot(data = test,
       aes(x = effect, fill = type)) +
  geom_density(alpha = 0.7) +
  theme_bw()

T1_data_optimistic$sufficiently_powered <-
  ifelse(T1_data_optimistic$effect < T1_data_optimistic$delta_powered, 1, 0)

sum(T1_data_optimistic$sufficiently_powered == 1) / nrow(T1_data_optimistic)
  
T1_data_pessimistic$sufficiently_powered <-
  ifelse(T1_data_pessimistic$effect < T1_data_pessimistic$delta_powered, 1, 0)

sum(T1_data_pessimistic$sufficiently_powered == 1) / nrow(T1_data_pessimistic)

ggplot(data = T1_data_optimistic,
       aes(x = effect, y = delta_powered, color = factor(sufficiently_powered))) +
  geom_point(size = 1, alpha = 0.6) +
  theme_bw()

ggplot(data = T1_data_pessimistic,
       aes(x = effect, y = delta_powered, color = factor(sufficiently_powered))) +
  geom_point(size = 1, alpha = 0.6) +
  theme_bw()
