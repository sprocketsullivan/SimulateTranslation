setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/")


BF_analysis_data <- 
  read.csv(file = "./data/Carneiro_distribution/Bayes_factor_analysis/Carneiro_distribution_sig_p0.1_method1")

### add column that codes decision criterion from exploratory stage to confirmatory stage
BF_analysis_data$decision_crit <- "significance"

### add column that codes approach to determine sample size for confirmatory study
BF_analysis_data$sampsize_approach <- "standard"

BF_analysis_data$H0 <- ifelse(BF_analysis_data$BF >= 3, 2, 1)

### add columns for prevalence and positives and negatives

BF_analysis_data$prev_pop      <- mat[1, 1]
BF_analysis_data$all_positives <- mat[2, 1]
BF_analysis_data$all_negatives <- mat[3, 1]

mat

### select columns relevant for plotting
dat <-
  BF_analysis_data %>% 
  select(init_sample_size, rep_no, decision_crit, sampsize_approach, 
         rep_sample_size, ES_true, mean_effect, H0,
         prev_pop, all_positives, all_negatives, rep_attempts)



data_false_positives <-
  dat %>%
  filter(H0 == 2 & ES_true < .3)

hist(data_false_positives$mean_effect)
hist(data_false_positives$ES_true)

### compute TP, FN, TN, and FP given a SESOI

true_positives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
  filter(H0 == 2 & ES_true > .3) %>%
  summarize(true_pos = n())

false_negatives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
  filter(H0 == 1 & ES_true > .3) %>%
  summarize(false_neg = n())

true_negatives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
  filter(H0 == 1 & ES_true < .3) %>%
  summarize(true_neg = n())

false_positives <-
  dat %>%
  group_by(init_sample_size, 
           decision_crit, sampsize_approach, H0, 
           all_positives, all_negatives, 
           prev_pop, rep_attempts) %>%
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
         FDR = false_pos/(false_pos + true_pos),
         Prevalence = prev_pop,
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

animal_numbers <-
  dat %>% 
  group_by(init_sample_size, decision_crit, sampsize_approach) %>% 
  summarize(mean_N = mean(rep_sample_size))

median_effect <-
  dat %>% 
  group_by(init_sample_size, decision_crit, sampsize_approach) %>% 
  summarize(median_effect = median(mean_effect))

median_effect_success <-
  dat %>%
  group_by(init_sample_size, decision_crit, sampsize_approach) %>% 
  filter(H0 == 2) %>%
  summarize(median_effect = median(mean_effect))

median_ES_true <-
  dat %>% 
  group_by(init_sample_size, decision_crit, sampsize_approach) %>% 
  summarize(median_ES_true = median(ES_true))

outcomes$mean_N <- animal_numbers$mean_N
outcomes$median_effect <- median_effect$median_effect
outcomes$median_effect_success <- median_effect_success$median_effect
outcomes$median_ES_true <- median_ES_true$median_ES_true


BF_analysis_data <-
  BF_analysis_data %>% 
  filter(init_sample_size == 10)

BF_analysis_data$H0 <- ifelse(BF_analysis_data$BF >= 3, 2, 1)

# BF_large <-
#   BF_analysis_data %>% 
#   filter(BF >= 6)


ggplot(data = BF_analysis_data, aes(x = BF_analysis_data$ES_true, fill = factor(H0))) +
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


