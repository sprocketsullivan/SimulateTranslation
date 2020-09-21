setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

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
source("./scripts/simulation/load_data_Carneiro.R")

library(foreach)
library(doMC)
registerDoMC(cores = 4)
library(doParallel)
registerDoParallel()
getDoParWorkers()

n_exp <- 10000
ES_true <- ES_data_Carneiro$ES_d
hist(ES_true, breaks = 200)

sum(ES_true < 0)
sum(ES_true > 0)
sum(ES_true == 0)

ES_true <- ifelse(ES_true < 0, -ES_true, -ES_true)
sum(ES_true > 0)
sum(ES_true < 0)
sum(ES_true == 0)

hist(ES_true, breaks = 200)
median(ES_true)
mean(ES_true)

set.seed(4321)
current_ES <- sample(ES_true, n_exp, replace = TRUE)

hist(current_ES, breaks = 200)
median(current_ES)
mean(current_ES)
sum(current_ES > 0)
sum(current_ES < 0)
sum(current_ES == 0)

#how many hypothesis over SESOI threshold
SESOI <- c(.5, 1)

mat <- matrix(NA, nrow = 3, ncol = length(SESOI),
              dimnames = list(c("prev_pop", "all_positives", "all_negatives"), 
                              c(.5, 1)))

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