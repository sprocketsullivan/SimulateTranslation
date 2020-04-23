

outcomes_Carneiro_exploratory <- read.csv(file = "./data/Carneiro_distribution_only_exploratory_stage")

outcomes_10 <-
  outcomes_Carneiro_exploratory %>% 
  filter(init_sample_size == 10)

outcomes_10$H0 <- ifelse(outcomes_10$p_value <= .05, 2, 1)

sum(outcomes_10$H0 == 2)

outcomes_10$prev_pop      <- mat[1, 2]
outcomes_10$all_positives <- mat[2, 2]
outcomes_10$all_negatives <- mat[3, 2]

mat


dat <-
  outcomes_10 %>% 
  select(init_sample_size, ES_true, effect, H0,
         prev_pop, all_positives, all_negatives)

dat$effect <- ifelse(dat$effect < 0, -dat$effect, -dat$effect)

data_false_positives <-
  dat %>%
  filter(H0 == 2 & ES_true < .5)

data_false_negatives <-
  dat %>%
  filter(H0 == 1 & ES_true > .5)

hist(data_false_positives$effect)

hist(data_false_positives$ES_true)


true_positives <-
  dat %>%
  # group_by(init_sample_size, ES_true, effect, H0,
  #          prev_pop, all_positives, all_negatives) %>%
  filter(H0 == 2 & ES_true > .5) %>%
  summarize(true_pos = n())

false_negatives <-
  dat %>%
  # group_by(init_sample_size, ES_true, effect, H0,
  #          prev_pop, all_positives, all_negatives) %>%
  filter(H0 == 1 & ES_true > .5) %>%
  summarize(false_neg = n())

true_negatives <-
  dat %>%
  # group_by(init_sample_size, ES_true, effect, H0,
  #          prev_pop, all_positives, all_negatives) %>%
  filter(H0 == 1 & ES_true < .5) %>%
  summarize(true_neg = n())

false_positives <-
  dat %>%
  # group_by(init_sample_size, ES_true, effect, H0,
  #          prev_pop, all_positives, all_negatives) %>%
  filter(H0 == 2 & ES_true < .5) %>%
  summarize(false_pos = n())

true_positives$false_neg <- false_negatives$false_neg
true_positives$true_neg  <- true_negatives$true_neg
true_positives$false_pos <- false_positives$false_pos

outcomes <-
  true_positives %>% 
  # select(-H0) %>% 
  mutate(Sensitivity = true_pos/ (true_pos + false_neg),
         Specificity = true_neg/ (true_neg + false_pos),
         FNR = false_neg/ (false_neg + true_pos),
         FPR = false_pos/ (false_pos + true_neg),
         FDR = false_pos/ (false_pos + true_pos),
         Prevalence = 0.458,
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

outcomes <- 
  outcomes %>% 
  mutate(NPV_pop_prev = (Specificity * (1 - Prevalence)) / 
           ((1 -  Sensitivity) * Prevalence + (Specificity) * (1 - Prevalence)))

outcomes <- 
  outcomes %>% 
  mutate(NPV_sample_prev = (Specificity * (1 - Sample_prev)) / 
           ((1 -  Sensitivity) * Sample_prev + (Specificity) * (1 - Sample_prev)))


outcomes <- 
  outcomes %>% 
  mutate(NPV_sample_prev_altern = true_neg / (true_neg + false_neg))



