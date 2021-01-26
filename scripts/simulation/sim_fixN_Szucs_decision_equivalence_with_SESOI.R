setwd("~/Documents/SimulateTranslation")

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
source("./scripts/simulation/load_packages.R")
source("./scripts/simulation/load_data_Szucs.R")
source("./scripts/simulation/functions_for_sim_BF.R")


library(foreach)
library(doMC)
registerDoMC(cores = 4)
library(doParallel)
registerDoParallel()
getDoParWorkers()

n_exp <- 10000
ES_true <- ES_data_Szucs$D

set.seed(4321)
current_ES <- sample(ES_true, n_exp)
hist(ES_true, breaks = 200)
hist(current_ES, breaks = 200)

#how many hypothesis over SESOI threshold
SESOI <- c(.1, .3, .5, .7, 1)

mat <- matrix(NA, nrow = 3, ncol = length(SESOI),
              dimnames = list(c("prev_pop", "all_positives", "all_negatives"), 
                              c(.1, .3, .5, .7, 1)))

prev_pop      <- vector()
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

samp_size_vector <- c(7, 10, 15)

list_exploratory_data <- 
  
  foreach(samp_size = samp_size_vector) %do% {
    
    exploratory_data <- list()
    
    for(i in 1:n_exp) {
      
      exploratory_data[[i]] <- generate_study(current_ES[i])
      
      exploratory_data[[i]] <-
        exploratory_data[[i]] %>% 
        mutate(study_id = i,
               ES_true = current_ES[i])
      
    }
    
    list_exploratory_data <- exploratory_data
  }


#the confidence interval generated here is used in the equivalence test
exploratory_data_summary <- list()

plan(multiprocess)
for (i in 1:length(samp_size_vector)) {
  
  exploratory_data_summary[[i]] <- 
    future_map(list_exploratory_data[[i]], get_summary_study)
  
}

# decision to go on
# this decision depends on whether our SESOI is within the 95 % CI
# select studies for replication if SESOI lies within the 95 % CI

selection_equiv <- list()
for (i in 1:length(samp_size_vector)) {
  
  selection_equiv[[i]] <- future_map(exploratory_data_summary[[i]], get_decision_equiv,
                                     SESOI = 1.0)
  
}

row_names <- NULL
col_names <- c("init_sample_size", "study_id", "t_value",
               "p_value", "CI_lower", "CI_upper", "effect")

df <- as_tibble(matrix(unlist(exploratory_data_summary), 
                       nrow = n_exp*length(samp_size_vector), byrow = TRUE,
                       dimnames = list(c(row_names),
                                       c(col_names))))

col_name <- "selection_equiv"

df_equiv <- as_tibble(matrix(unlist(selection_equiv), 
                           nrow = n_exp*length(samp_size_vector), byrow = TRUE,
                           dimnames = list(c(row_names),
                                           c(col_name))))


dat <- bind_cols(df, df_equiv)

dat$ES_true <- rep(current_ES, 3)

# write.csv(dat, file = "./data/Szucs_distribution/Frequentist_analysis/exploratory_stage_equiv_1.0")

hist(dat$ES_true, breaks = 100)
