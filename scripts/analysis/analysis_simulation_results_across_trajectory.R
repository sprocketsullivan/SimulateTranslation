setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/")

rm(list = ls())
library(tidyverse)

source("./scripts/analysis/prior_odds_for_analysis_Szucs.R")

# source("./scripts/analysis/prior_odds_for_analysis_Carneiro.R")

### read in data sets from the different trajectories 
### using fixed-N design in replication study

# final <- read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_equiv_method2_1.0")

final <- read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_distribution_sig_method1")


hist(final$effect, breaks = 100)

hist(final$ES_true, breaks = 100)


### add column that codes decision criterion from exploratory stage to confirmatory stage
final$decision_crit <- "significance"

### add column that codes approach to determine sample size for confirmatory study
final$sampsize_approach <- "standard"

### add column for outcome significant / not significant to match outcome column
### of sequential design data set
final$H0 <- ifelse(final$p_value <= .05, 2, 1)

### add column for prevalence 

final$prev_pop      <- mat[1, 2]
final$all_positives <- mat[2, 2]
final$all_negatives <- mat[3, 2]

mat


### select columns relevant for plotting
dat <-
  final %>% 
  select(init_sample_size, rep_no, decision_crit, sampsize_approach, 
         rep_sample_size, ES_true, effect, H0,
         prev_pop, all_positives, all_negatives, rep_attempts)

# write.csv(dat, file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_replication_sig_method1")

# write.csv(dat, file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_replication_equiv_method2_0.5")


# data_false_positives <-
#   dat %>%
#   filter(H0 == 2 & ES_true < .5)
# 
# hist(data_false_positives$effect)
# 
# hist(data_false_positives$ES_true)

################################################################################################################


true_positives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
  filter(H0 == 2 & ES_true > 0) %>%
  summarize(true_pos = n())

false_negatives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
  filter(H0 == 1 & ES_true > 0) %>%
  summarize(false_neg = n())

true_negatives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
  filter(H0 == 1 & ES_true < 0) %>%
  summarize(true_neg = n())

false_positives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
  filter(H0 == 2 & ES_true < 1) %>%
  summarize(false_pos = n())

true_positives$false_neg <- false_negatives$false_neg
true_positives$true_neg  <- true_negatives$true_neg
true_positives$false_pos <- false_positives$false_pos

outcomes <-
  true_positives %>% 
  select(-H0) %>% 
  mutate(Sensitivity = true_pos/ all_positives,
         Specificity = true_neg/ all_negatives,
         FNR = false_neg/ all_positives,
         FPR = false_pos/ all_negatives,
         FDR = false_pos/(false_pos + true_pos),
         Prevalence = prev_pop,
         Sample_prev = (true_pos + false_neg) /
           (all_positives + all_negatives))

# False Discovery Rate (FDR) = B / (A + B)
# The false discovery rate is the proportion of the individuals with a 
# positive test result for which the true condition is negative

# False Positive Rate (FPR) or Fall-out = B / (B + D) 
# The false positive rate is the proportion of the individuals with a known 
# negative condition for which the test result is positive. 
# rate is sometimes called the fall-out.  

# False Omission Rate (FOR) = C / (C + D)
# The false omission rate is the proportion of the individuals with a 
# negative test result for which the true condition is positive.


outcomes <- 
  outcomes %>% 
  mutate(PPV_pop_prev = (Sensitivity * Prevalence) / 
           (Sensitivity * Prevalence + (1 - Specificity) * (1 - Prevalence)))

outcomes <- 
  outcomes %>% 
  mutate(PPV_sample_prev = (Sensitivity * Sample_prev) / 
           (Sensitivity * Sample_prev + (1 - Specificity) * (1 - Sample_prev)))

# outcomes <- 
#   outcomes %>% 
#   mutate(NPV_pop_prev = (Specificity * (1 - Prevalence)) / 
#            ((1 -  Sensitivity) * Prevalence + (Specificity) * (1 - Prevalence)))
# 
# outcomes <- 
#   outcomes %>% 
#   mutate(NPV_sample_prev = (Specificity * (1 - Sample_prev)) / 
#            ((1 -  Sensitivity) * Sample_prev + (Specificity) * (1 - Sample_prev)))

animal_numbers <-
  dat %>% 
  group_by(init_sample_size, decision_crit, sampsize_approach) %>% 
  summarize(mean_N = mean(rep_sample_size),
            sd_N = sd(rep_sample_size),
            ymin = mean_N - sd_N,
            ymax = mean_N + sd_N)

median_effect <-
  dat %>% 
  group_by(init_sample_size, decision_crit, sampsize_approach) %>% 
  summarize(median_effect = median(effect))

median_effect_success <-
  dat %>%
  group_by(init_sample_size, decision_crit, sampsize_approach) %>% 
  filter(H0 == 2) %>%
  summarize(median_effect = median(effect))

median_ES_true <-
  dat %>% 
  group_by(init_sample_size, decision_crit, sampsize_approach) %>% 
  summarize(median_ES_true = median(ES_true))

outcomes$mean_N <- animal_numbers$mean_N
outcomes$mean_N_min <- animal_numbers$ymin
outcomes$mean_N_max <- animal_numbers$ymax
outcomes$median_effect <- median_effect$median_effect
outcomes$median_effect_success <- median_effect_success$median_effect
outcomes$median_ES_true <- median_ES_true$median_ES_true

# write.csv(outcomes, file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_outcomes_sig_method1_0.5")

write.csv(outcomes, file = "./data/Carneiro_distribution/Frequentist_analysis//Carneiro_outcomes_equiv_method2_1.0")


final <-
  final %>% 
  filter(init_sample_size == 10)

final$H0 <- ifelse(final$p_value < .05, 2, 1)

final_sig <-
  final %>% 
  filter(p_value < .05)


ggplot(data = final, aes(x = final$ES_true, fill = factor(H0))) +
  geom_histogram(bins = 50, color = "black", size = 0.3, alpha = 0.8) +
  labs(x = expression(paste("Sampled effect sizes (Cohen's ", italic("d"), ") ")),
       y = "Frequency",
       fill = "Significant outcome \nat replication") +
  scale_fill_manual(breaks = c("1", "2"),
                    labels = c("no",
                               "yes"),
                    values = c("grey", "steelblue")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 18, colour = "black")) +
  # theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
  # theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold")) +
  # theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 17, face = "bold")) +
  theme(legend.text = element_text(size = 17)) +
  theme(title = element_text(size = 15))

ggplot(data = dat, aes(x = dat$ES_true, fill = factor(selection_sig))) +
  geom_histogram(bins = 50, color = "black", size = 0.3, alpha = 0.8) +
  labs(x = expression(paste("Sampled effect sizes (Cohen's ", italic("d"), ") ")),
       y = "Frequency",
       fill = "Selected for \nreplication") +
  scale_fill_manual(breaks = c("0", "1"),
                    labels = c("no",
                               "yes"),
                    values = c("grey", "deeppink")) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 18, colour = "black")) +
  # theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
  # theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold")) +
  # theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 18, face = "bold")) +
  theme(legend.text = element_text(size = 18)) +
  theme(title = element_text(size = 15))


dat_short <-
  dat %>% 
  select(init_sample_size, ES_true, selection_sig)

final_short <-
  final %>% 
  select(init_sample_size, ES_true, H0) %>% 
  filter(H0 == 2 & ES_true >= .5)

dat_new <- bind_rows(dat_short, final_short)

ggplot(data = dat_short, aes(x = ES_true)) +
  geom_histogram(bins = 50, color = "black", fill = "grey", size = 0.3, alpha = 0.8) +
  geom_histogram(data = final_short, aes(x = ES_true),
                 bins = 50, color = "black", fill = "steelblue", size = 0.3, alpha = 0.8) +
  labs(x = expression(paste("Sampled effect sizes (Cohen's ", italic("d"), ") ")),
       y = "Frequency",
       fill = "Selected for \nreplication") +
  # scale_fill_manual(breaks = c("0", "1"),
  #                   labels = c("no",
  #                              "yes"),
  #                   values = c("grey", "deeppink3")) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 18, colour = "black")) +
  # theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
  # theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold")) +
  # theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 18, face = "bold")) +
  theme(legend.text = element_text(size = 18)) +
  theme(title = element_text(size = 15))

# setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")
