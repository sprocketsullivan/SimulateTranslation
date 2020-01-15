setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/data")

#rm(list = ls())
library(tidyverse)

### read in data sets from the different trajectories 
### using fixed-N design in confirmatory study
final <- read.csv(file = "Szucs_distribution_equiv_method1_1.0")


hist(final$effect, breaks = 100)

hist(final$ES_true, breaks = 100)


### add column that codes decision criterion from exploratory stage to confirmatory stage
final$decision_crit <- "equivalence"

### add column that codes approach to determine sample size for confirmatory study
final$sampsize_approach <- "standard"

### add column that codes design applied in confirmatory study
#finalfix$design <- rep("fixN")

### add column for outcome significant / not significant to match outcome column
### of sequential design data set
final$H0 <- ifelse(final$p_value <= .05, 2, 1)

### add column for prevalence 

final$prev_pop      <- mat[1, 4]
final$all_positives <- mat[2, 4]
final$all_negatives <- mat[3, 4]

mat


### select columns relevant for plotting
dat <-
  final %>% 
  select(init_sample_size, rep_no, decision_crit, sampsize_approach, 
         rep_sample_size, ES_true, effect, H0,
         prev_pop, all_positives, all_negatives, rep_attempts)


# data_false_positives <-
#   dat %>% 
#   filter(H0 == 2 & ES_true < .5)
# 
# hist(data_false_positives$effect)
# 
# hist(data_false_positives$ES_true)

################################################################################################################

### combine data sets containing sequential and fixed-N experiments

true_positives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
  filter(H0 == 2 & ES_true > 1) %>%
  summarize(true_pos = n())

false_negatives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
  filter(H0 == 1 & ES_true > 1) %>%
  summarize(false_neg = n())

true_negatives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
  filter(H0 == 1 & ES_true < 1) %>%
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


# outcomes <-
#   true_positives %>% 
#   select(-H0) %>% 
#   mutate(Sensitivity = true_pos/(true_pos + false_neg),
#          Specificity = true_neg/(true_neg + false_pos),
#          FNR = false_neg/(true_pos + false_neg),
#          FPR = false_pos/(true_neg + false_pos),
#          FDR = false_pos/(false_pos + true_pos),
#          Prevalence = prev_pop,
#          Sample_prev = (true_pos + false_neg) /
#            (true_pos + false_neg + false_pos + true_neg))

# False Discovery Rate (FDR) = B / (A + B)
# The false discovery rate is the proportion of the individuals with a 
# positive test result for which the true condition is negative

# False Positive Rate (FPR) or Fall-out = B / (B + D) 
# The false positive rate is the proportion of the individuals with a known 
# negative condition for which the test result is positive. 
# rate is sometimes called the fall-out.  


outcomes <- 
  outcomes %>% 
  mutate(PPV_pop_prev = (Sensitivity * Prevalence) / 
           (Sensitivity * Prevalence + (1 - Specificity) * (1 - Prevalence)))


outcomes <- 
  outcomes %>% 
  mutate(PPV_sample_prev = (Sensitivity * Sample_prev) / 
           (Sensitivity * Sample_prev + (1 - Specificity) * (1 - Sample_prev)))

# PPV_sample_prev = true_pos/(true_pos + false_pos)

# outcomes <- 
#   outcomes %>% 
#   mutate(PPV_pop_prev = (Sensitivity * Prevalence) / 
#            (Sensitivity * Prevalence + (1 - Specificity) * (1 - Prevalence)),
#          PPV_alternative = true_pos / (true_pos + false_pos))


animal_numbers <-
  dat %>% 
  group_by(init_sample_size, decision_crit, sampsize_approach) %>% 
  summarize(mean_N = mean(rep_sample_size))

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
outcomes$median_effect <- median_effect$median_effect
outcomes$median_effect_success <- median_effect_success$median_effect
outcomes$median_ES_true <- median_ES_true$median_ES_true

write.csv(outcomes, file = "./Szucs_outcomes_equiv_method1_1.0")
#write.csv(outcomes, file = "./Carneiro_outcomes_equiv_method1_0.3")

# outcomes$design <- as.factor(outcomes$design)
# levels(outcomes$design)
# levels(outcomes$design) <- c("Fixed-N \ndesign", "Group sequential \ndesign", 
#                              "Group sequential \ndesign with futility bound")

# outcomes$decision_crit <- as.factor(outcomes$decision_crit)
# levels(outcomes$decision_crit)
# levels(outcomes$decision_crit) <- c("Equivalence", "Significance")

setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")
