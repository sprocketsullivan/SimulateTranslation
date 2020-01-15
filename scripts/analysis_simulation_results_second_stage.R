setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/data")

#rm(list = ls())
library(tidyverse)


final <- read.csv(file = "Carneiro_distribution_sig_method2_0.7")


hist(final$effect, breaks = 100)

hist(final$ES_true, breaks = 100)


### add column that codes decision criterion from exploratory stage to confirmatory stage
final$decision_crit <- "significance"

### add column that codes approach to determine sample size for confirmatory study
final$sampsize_approach <- "SESOI"

### add column that codes design applied in confirmatory study
#finalfix$design <- rep("fixN")

### add column for outcome significant / not significant to match outcome column
### of sequential design data set
final$H0 <- ifelse(final$p_value <= .05, 2, 1)

### add column for prevalence 

final$prev_pop      <- mat[1, 3]
final$all_positives <- mat[2, 3]
final$all_negatives <- mat[3, 3]

mat

dat <-
  final %>% 
  select(init_sample_size, rep_no, decision_crit, sampsize_approach, 
         rep_sample_size, ES_true, effect, H0,
         prev_pop, rep_attempts)

true_positives <-
  dat %>%
  group_by(init_sample_size, decision_crit, sampsize_approach, H0, rep_attempts, prev_pop) %>%
  filter(H0 == 2 & ES_true > .7) %>%
  summarize(true_pos = n())

false_positives <-
  dat %>%
  group_by(init_sample_size, decision_crit, sampsize_approach, H0, rep_attempts, prev_pop) %>%
  filter(H0 == 2 & ES_true < .7) %>%
  summarize(false_pos = n())

false_negatives <-
  dat %>%
  group_by(init_sample_size, decision_crit, sampsize_approach, H0, rep_attempts, prev_pop) %>%
  filter(H0 == 1 & ES_true > .7) %>%
  summarize(false_neg = n())

true_negatives <-
  dat %>%
  group_by(init_sample_size, decision_crit, sampsize_approach, H0, rep_attempts, prev_pop) %>%
  filter(H0 == 1 & ES_true < .7) %>%
  summarize(true_neg = n())

true_positives$false_pos <- false_positives$false_pos
true_positives$false_neg <- false_negatives$false_neg
true_positives$true_neg <- true_negatives$true_neg


outcomes <-
  true_positives %>% 
  select(-H0) %>% 
  mutate(Sensitivity = true_pos/(true_pos + false_neg),
         Specificity = true_neg/(true_neg + false_pos),
         FNR = false_neg/(true_pos + false_neg),
         FPR = false_pos/(true_neg + false_pos),
         FDR = false_pos/(false_pos + true_pos),
         Prevalence = prev_pop,
         Sample_prev = (true_pos + false_neg) /
           (true_pos + false_neg + false_pos + true_neg))


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
           (Sensitivity * Prevalence + (1 - Specificity) * (1 - Prevalence)),
         PPV_alternative = true_pos / (true_pos + false_pos))


outcomes <-
  outcomes %>%
  mutate(PPV_sample_prev = (Sensitivity * Sample_prev) /
           (Sensitivity * Sample_prev + (1 - Specificity) * (1 - Sample_prev)))

