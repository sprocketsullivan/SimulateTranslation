setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

rm(list = ls())

# Here we simulate a preclinical research trajectory
# in the first stage an exploratory study is conducted with a limited number of animals
# in the second stage this experiment is replicated based on some information from the first experiment
# in the third stage a multi center study is conducted
# outcome for each experiment is 
# 1. the measured effect size after each run (with associated CI) and p-value
# 2. the number of animals 
# 3. the true effect size


# source additional functions
source("./scripts/load_packages.R")
source("./scripts/load_data_Szucs.R")
#source("./scripts/safeguard_function.R")
#source("./scripts/functions_for_simulation_onesided.R")
source("./scripts/functions_for_sim_Szucs.R")


# Step 1 Generate an effect size distribution
ES_true <- ES_data_Szucs$D # empirical effect size distribution taken from Szucs & Ioannidis, 2017
hist(ES_true, breaks = 300)
sum(ES_true > 16)



SESOI       <- c(.3, .5, .7, 1)
samp_size   <- c(7, 10, 15)
#samp_size   <- c(7, 10, 15)

mat <- matrix(NA, nrow = 3, ncol = length(SESOI),
              dimnames = list(c("prev_pop", "all_positives", "all_negatives"), 
                              c(.3, .5, .7, 1)))
    

#how many hypothesis over SESOI threshold
#prev_pop <- round(sum(ES_true > SESOI)/length(ES_true), 2)
n_exp <- 100
current_ES <- sample(ES_true, n_exp)
hist(current_ES)
#all_positives <- sum(current_ES > SESOI)
#all_negatives <- n_exp - all_positives

prev_pop <- vector()
all_positives <- vector()
all_negatives <- vector()

counter = 0

for (ES in SESOI) {
  
  counter = counter + 1
  
  prev <- round(sum(ES_true > ES)/length(ES_true), 3)
  all_pos <- sum(current_ES > ES)
  all_neg <- n_exp - all_pos
  
  print(ES)
  
  prev_pop[counter] <- prev
  all_positives[counter] <- all_pos
  all_negatives[counter] <- all_neg
  
}


mat[1, ] <- prev_pop
mat[2, ] <- all_positives
mat[3, ] <- all_negatives

mat

#conduct intial study for each initial sample size with foreach function
exploratory_data <- list()

for(i in 1:n_exp) {
  
  exploratory_data[[i]] <- generate_study(current_ES[i])
  
  exploratory_data[[i]] <-
    exploratory_data[[i]] %>% 
    mutate(study_id = i)
  
  print(i)
}

 
#the confidence interval generated here is used in the equivalence test
plan(multiprocess)
exploratory_data_summary <- 
  future_map(exploratory_data, get_summary_study)

# now estimate sample size for replication study
# A. Initial effect size (probably the worst solution but good as a floor benchmark, 1 in function)
# B. Set a SESOI. With this all experiments will have the same number of EU (2 as function parameter)

rep_sample_size_std <- NULL

for(i in 1:n_exp) {
  
  rep_sample_size_std[i] <- 
    ceiling(calc_sample_size(study_summary = exploratory_data_summary[[i]],
                             study_data = exploratory_data[[i]],
                             method = 1))
  
  exploratory_data_summary[[i]] <-
    exploratory_data_summary[[i]] %>%
    mutate(rep_sample_size_std = rep_sample_size_std[[i]])
}
  
mean(rep_sample_size_std)


rep_sample_size_SESOI <- NULL

for(i in 1:n_exp) {
  
  rep_sample_size_SESOI[i] <- 
    ceiling(calc_sample_size(study_summary = exploratory_data_summary[[i]],
                             study_data = exploratory_data[[i]], 
                             SESOI = SESOI[2], method = 2))
  
  exploratory_data_summary[[i]] <-
    exploratory_data_summary[[i]] %>%
    mutate(rep_sample_size_SESOI = rep_sample_size_SESOI[[i]])
}

mean(rep_sample_size_SESOI)

# decision to go on
# this decision depends on whether exploratory result is significant (p <= .05) or not
# select studies for replication if p-value < .05
selection_sig <- list()

for (i in 1:n_exp) {
  
  selection_sig[[i]] <- get_decision_sig(exploratory_data_summary[[i]])
  
  exploratory_data_summary[[i]] <-
    exploratory_data_summary[[i]] %>%
    mutate(selection_sig = selection_sig[[i]])
  
}


# decision to go on
# this decision depends on an equivalence test with a bound of .3
# only experiments replicated that include .3 in the CI around the ES measured

selection_equiv <- list()

for (i in 1:n_exp) {
  
  selection_equiv[[i]] <- get_decision_equiv(exploratory_data_summary[[i]])
  
  exploratory_data_summary[[i]] <-
    exploratory_data_summary[[i]] %>%
    mutate(selection_equiv = selection_equiv[[i]])
  
}

dat <- bind_rows(exploratory_data_summary)

dat <-
  dat %>% 
  mutate(ES_true = current_ES[study_id])

dat$effect <- ifelse(dat$effect < 0, -dat$effect, -dat$effect)


replication_data <- list()
rep_exp_no <- 0

select_experiments <- which(dat$selection_equiv == 1)

select_experiments <- select_experiments[dat$effect[select_experiments] > 0 ]

rep_attempts <- length(select_experiments)


for(i in select_experiments) {
  
  rep_exp_no <- rep_exp_no + 1
  
  replication_data[[rep_exp_no]] <-
    generate_study(ES_true = current_ES[i],
                   sample_size = rep_sample_size_std[i])
  
  replication_data[[rep_exp_no]] <-
    replication_data[[rep_exp_no]] %>% 
    mutate(study_id = rep_exp_no)
}


plan(multiprocess)
rep_data_summary <- 
  future_map(replication_data, get_summary_study_rep)


res_summary_rep <-
  data.frame(rep_no = c(1:rep_exp_no),
             totalN = rep_sample_size_std[select_experiments],
             p_value = unlist(map(rep_data_summary, "p_value")), #[seq(1, 2*rep_exp_no, 2)],
             effect = unlist(map(rep_data_summary, "effect")),
             ES_true = current_ES[select_experiments])


res_summary_rep <-
  res_summary_rep %>% 
  mutate(prev_pop[1], rep_attempts,
         all_positives[1], all_negatives[1])



res_summary_rep$effect <- ifelse(res_summary_rep$effect < 0, -res_summary_rep$effect, -res_summary_rep$effect)

write.csv(res_summary_rep, file = "./data/Szucs_distribution_0.3_method1")


res_summary_rep <-
  res_summary_rep %>% 
  filter(effect < 20)

ggplot(aes(y = effect, x = ES_true, col = p_value < .05),
       data = res_summary_rep) +
  geom_point(alpha = 0.4) +
  geom_hline(aes(yintercept = .3), color = "red") +
  theme_bw()


#write.csv(res_summary_rep, file = "./data/")

