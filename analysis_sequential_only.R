final <- read.csv(file = "./data/equiv_method1_seq_onesided_futility")


### add column that codes decision criterion from exploratory stage to confirmatory stage
final$decision_crit <- rep("equivalence")

### add column that codes approach to determine sample size for confirmatory study
final$sampsize_approach <- 1

### add column that codes design applied in confirmatory study
final$design <- rep("group_sequential")


dat <-
  final %>% 
  filter(H0 != 0) %>% 
  select(rep_no, decision_crit, sampsize_approach, 
         design, totalN, nstage, ES_true, d_emp, H0,
         prev_pop, rep_attempts, 
         false_omission_rate, true_selection_rate)


true_positives <-
  dat %>%
  group_by(decision_crit, sampsize_approach, design, H0) %>%
  filter(H0 == 2 & ES_true > .3) %>%
  summarize(true_pos = n())

false_positives <-
  dat %>%
  group_by(decision_crit, sampsize_approach, design, H0) %>%
  filter(H0 == 2 & ES_true < .3) %>%
  summarize(false_pos = n())

false_negatives <-
  dat %>%
  group_by(decision_crit, sampsize_approach, design, H0) %>%
  filter(H0 == 1 & ES_true > .3) %>%
  summarize(false_neg = n())

true_negatives <-
  dat %>%
  group_by(decision_crit, sampsize_approach, design, H0) %>%
  filter(H0 == 1 & ES_true < .3) %>%
  summarize(true_neg = n())

true_positives$false_pos <- false_positives$false_pos
true_positives$false_neg <- false_negatives$false_neg
true_positives$true_neg <- true_negatives$true_neg

outcomes <-
  true_positives %>% 
  select(-H0) %>% 
  mutate(Sensitivity = true_pos/(true_pos + false_neg),
         Specificity = true_neg/(true_neg + false_pos),
         Prevalence = .17,
         Sample_prev = (true_pos + false_neg) / 
           (true_pos + false_neg + false_pos + true_neg))


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
  group_by(decision_crit, sampsize_approach, design) %>% 
  summarize(mean_N = mean(nstage))

median_d_emp <-
  dat %>% 
  group_by(decision_crit, sampsize_approach, design) %>% 
  summarize(median_d_emp = median(d_emp))

median_ES_true <-
  dat %>% 
  group_by(decision_crit, sampsize_approach, design) %>% 
  summarize(median_ES_true = median(ES_true))

outcomes$mean_N <- animal_numbers$mean_N
outcomes$median_d_emp <- median_d_emp$median_d_emp
outcomes$median_ES_true <- median_ES_true$median_ES_true
