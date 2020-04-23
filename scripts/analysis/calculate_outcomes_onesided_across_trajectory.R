setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/data")

rm(list = ls())

library(tidyverse)
library(ggpubr)
library(ggridges)

### read in data sets from the different trajectories 
### using sequential design with futility criterion in confirmatory study
final_1_ft <- read.csv(file = "equiv_method1_seq_onesided_futility")
final_2_ft <- read.csv(file = "equiv_method2_seq_onesided_futility")
final_3_ft <- read.csv(file = "equiv_method3_seq_onesided_futility")

final_4_ft <- read.csv(file = "sig_method1_seq_onesided_futility")
final_5_ft <- read.csv(file = "sig_method2_seq_onesided_futility")
final_6_ft <- read.csv(file = "sig_method3_seq_onesided_futility")


### create one data frame containing all data
final <- rbind(final_1_ft, final_2_ft, final_3_ft, final_4_ft, final_5_ft, final_6_ft)

### add column that codes decision criterion from exploratory stage to confirmatory stage
final$decision_crit <- c(rep("equivalence", nrow(final_1_ft) + nrow(final_2_ft) + nrow(final_3_ft)),
                         rep("significance", nrow(final_4_ft) + nrow(final_5_ft) + nrow(final_6_ft)))

### add column that codes approach to determine sample size for confirmatory study
final$sampsize_approach <- c(rep(1, nrow(final_1_ft)), rep(2, nrow(final_2_ft)), rep(3, nrow(final_3_ft)),
                             rep(1, nrow(final_4_ft)), rep(2, nrow(final_5_ft)), rep(3, nrow(final_6_ft)))

### add column that codes design applied in confirmatory study
final$design <- rep("sequential_futility")

### exclude all cases that do not have either a significant or futile outcome
### select columns relevant for plotting
datseq_futility <-
  final %>% 
  filter(H0 != 0) %>% 
  select(rep_no, decision_crit, sampsize_approach, 
         design, totalN, nstage, ES_true, d_emp, H0,
         prev_pop, rep_attempts, 
         all_positives, all_negatives)


################################################################################################################

### read in data sets from the different trajectories 
### using sequential design with futility criterion in confirmatory study
final_1 <- read.csv(file = "equiv_method1_seq_onesided")
final_2 <- read.csv(file = "equiv_method2_seq_onesided")
final_3 <- read.csv(file = "equiv_method3_seq_onesided")

final_4 <- read.csv(file = "sig_method1_seq_onesided")
final_5 <- read.csv(file = "sig_method2_seq_onesided")
final_6 <- read.csv(file = "sig_method3_seq_onesided")


### create one data frame containing all data
final <- rbind(final_1, final_2, final_3, final_4, final_5, final_6)




### add column that codes decision criterion from exploratory stage to confirmatory stage
final$decision_crit <- c(rep("equivalence", nrow(final_1) + nrow(final_2) + nrow(final_3)),
                         rep("significance", nrow(final_4) + nrow(final_5) + nrow(final_6)))

### add column that codes approach to determine sample size for confirmatory study
final$sampsize_approach <- c(rep(1, nrow(final_1)), rep(2, nrow(final_2)), rep(3, nrow(final_3)),
                             rep(1, nrow(final_4)), rep(2, nrow(final_5)), rep(3, nrow(final_6)))

### add column that codes design applied in confirmatory study
final$design <- rep("sequential")

### exclude all cases that do not have either a significant or futile outcome
### select columns relevant for plotting
datseq <-
  final %>% 
  filter(H0 != 0) %>% 
  select(rep_no, decision_crit, sampsize_approach, 
         design, totalN, nstage, ES_true, d_emp, H0,
         prev_pop, rep_attempts, 
         all_positives, all_negatives)

################################################################################################################

### read in data sets from the different trajectories 
### using fixed-N design in confirmatory study
final_fix1 <- read.csv(file = "equiv_method1_fixN_onesided")
final_fix2 <- read.csv(file = "equiv_method2_fixN_onesided")
final_fix3 <- read.csv(file = "equiv_method3_fixN_onesided")

final_fix4 <- read.csv(file = "sig_method1_fixN_onesided")
final_fix5 <- read.csv(file = "sig_method2_fixN_onesided")
final_fix6 <- read.csv(file = "sig_method3_fixN_onesided")


### create one data frame containing all data
finalfix <- rbind(final_fix1, final_fix2, final_fix3, final_fix4, final_fix5, final_fix6)

### add column that codes decision criterion from exploratory stage to confirmatory stage
finalfix$decision_crit <- c(rep("equivalence", nrow(final_fix1) + nrow(final_fix2) + nrow(final_fix3)),
                            rep("significance", nrow(final_fix4) + nrow(final_fix5) + nrow(final_fix6)))

### add column that codes approach to determine sample size for confirmatory study
finalfix$sampsize_approach <- c(rep(1, nrow(final_fix1)), rep(2, nrow(final_fix2)), rep(3, nrow(final_fix3)),
                                rep(1, nrow(final_fix4)), rep(2, nrow(final_fix5)), rep(3, nrow(final_fix6)))

### add column that codes design applied in confirmatory study
finalfix$design <- rep("fixN")

### add column for outcome significant / not significant to match outcome column
### of sequential design data set
finalfix$H0 <- ifelse(finalfix$p_value <= .05, 2, 1)

### rename column to match column of sequential design data set
finalfix$nstage <- finalfix$totalN

### select columns relevant for plotting
datfix <-
  finalfix %>% 
  select(rep_no, decision_crit, sampsize_approach, 
         design, totalN, nstage, ES_true, d_emp, H0,
         prev_pop, rep_attempts, 
         all_positives, all_negatives)

################################################################################################################

### combine data sets containing sequential and fixed-N experiments
dat <- rbind(datseq, datseq_futility, datfix)


large_ES_true <-
  dat %>% 
  filter(H0 != 0 & ES_true > .3)

small_ES_true <-
  dat %>% 
  filter(H0 != 0 & ES_true < .3)


true_positives <-
  dat %>%
  group_by(decision_crit, sampsize_approach, design, H0, 
           all_positives, all_negatives, rep_attempts) %>%
  filter(H0 == 2 & ES_true > .3) %>%
  summarize(true_pos = n())

false_negatives <-
  dat %>%
  group_by(decision_crit, sampsize_approach, design, H0,
           all_positives, all_negatives, rep_attempts) %>%
  filter(H0 == 1 & ES_true > .3) %>%
  summarize(false_neg = n())

true_negatives <-
  dat %>%
  group_by(decision_crit, sampsize_approach, design, H0,
           all_positives, all_negatives, rep_attempts) %>%
  filter(H0 == 1 & ES_true < .3) %>%
  summarize(true_neg = n())

false_positives <-
  dat %>%
  group_by(decision_crit, sampsize_approach, design, H0,
           all_positives, all_negatives, rep_attempts) %>%
  filter(H0 == 2 & ES_true < .3) %>%
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
         Prevalence = .17,
         Sample_prev = (true_pos + false_neg) /
           (all_positives + all_negatives))


outcomes <- 
  outcomes %>% 
  mutate(PPV_pop_prev = (Sensitivity * Prevalence) / 
           (Sensitivity * Prevalence + (1 - Specificity) * (1 - Prevalence)))


outcomes <- 
  outcomes %>% 
  mutate(PPV_sample_prev = (Sensitivity * Sample_prev) / 
           (Sensitivity * Sample_prev + (1 - Specificity) * (1 - Sample_prev)))

#PPV_sample_prev = true_pos/(true_pos + false_pos)

animal_numbers <-
  dat %>% 
  group_by(decision_crit, sampsize_approach, design) %>% 
  summarize(mean_N = mean(nstage))

median_d_emp <-
  dat %>% 
  group_by(decision_crit, sampsize_approach, design) %>% 
  summarize(median_d_emp = median(d_emp))

# median_d_emp_success <-
#   dat %>% 
#   group_by(decision_crit, sampsize_approach, design) %>% 
#   filter(H0 == 2) %>% 
#   summarize(median_d_emp = median(d_emp))

median_ES_true <-
  dat %>% 
  group_by(decision_crit, sampsize_approach, design) %>% 
  summarize(median_ES_true = median(ES_true))

outcomes$mean_N <- animal_numbers$mean_N
outcomes$median_d_emp <- median_d_emp$median_d_emp
#outcomes$median_d_emp_success <- median_d_emp_success$median_d_emp
outcomes$median_ES_true <- median_ES_true$median_ES_true

#write.csv(outcomes, file = "./final_outcomes_SESOI_0.3_across_trajectory")

# outcomes$design <- as.factor(outcomes$design)
# levels(outcomes$design)
# levels(outcomes$design) <- c("Fixed-N \ndesign", "Group sequential \ndesign", 
#                              "Group sequential \ndesign with futility bound")

outcomes$decision_crit <- as.factor(outcomes$decision_crit)
levels(outcomes$decision_crit)
levels(outcomes$decision_crit) <- c("Equivalence", "Significance")

setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")
