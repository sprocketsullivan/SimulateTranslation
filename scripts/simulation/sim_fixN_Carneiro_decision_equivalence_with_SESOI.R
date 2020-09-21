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
source("./scripts/simulation/load_packages.R")
source("./scripts/simulation/load_data_Carneiro.R")
source("./scripts/simulation/functions_for_sim_BF.R")


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
SESOI       <- c(.5, 1)

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


#list_exploratory_data[[3]][[999]]

#the confidence interval generated here is used in the equivalence test
exploratory_data_summary <- list()

plan(multiprocess)
for (i in 1:length(samp_size_vector)) {
  
  exploratory_data_summary[[i]] <- 
    future_map(list_exploratory_data[[i]], get_summary_study)
  
}

#exploratory_data_summary[[3]][[999]]

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

dat$ES_true <- current_ES


hist(dat$ES_true, breaks = 100)

# dat <-
#   dat %>% 
#   filter(init_sample_size == 10)
# 
# ggplot(data = dat, aes(x = dat$ES_true, fill = factor(selection_equiv))) +
#   geom_histogram(bins = 50, color = "black", size = 0.3, alpha = 0.8) +
#   labs(x = expression(paste("Sampled effect sizes (Cohen's ", italic("d"), ") ")),
#        y = "Frequency",
#        fill = "Selected for \nreplication") +
#   scale_fill_manual(breaks = c("0", "1"),
#                     labels = c("no",
#                                "yes"),
#                     values = c("grey", "deeppink3")) +
#   theme_bw() +
#   theme(axis.title.x = element_blank()) +
#   theme(axis.title.y = element_text(size = 20)) +
#   theme(axis.text.x = element_text(size = 18, colour = "black")) +
#   theme(axis.text.y = element_text(size = 18, colour = "black")) +
#   # theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
#   # theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold")) +
#   # theme(strip.background = element_rect(fill = "white", color = "black")) +
#   theme(legend.title = element_text(size = 18, face = "bold")) +
#   theme(legend.text = element_text(size = 18)) +
#   theme(title = element_text(size = 15)) +
#   theme(legend.position = "none")
# 
# 
# ggplot(data = dat, aes(x = dat$ES_true)) +
#   geom_histogram(bins = 50, color = "black", fill = "grey", size = 0.3, alpha = 0.8) +
#   labs(x = expression(paste("Sampled effect sizes (Cohen's ", italic("d"), ") ")),
#        y = "Frequency",
#        fill = "Selected for \nreplication") +
#   # scale_fill_manual(breaks = c("0", "1"),
#   #                   labels = c("no",
#   #                              "yes"),
#   #                   values = c("grey", "deeppink3")) +
#   theme_bw() +
#   theme(axis.title.x = element_blank()) +
#   # theme(axis.title.y = element_text(size = 20)) +
#   theme(axis.text.x = element_text(size = 18, colour = "black")) +
#   # theme(axis.text.y = element_text(size = 18, colour = "black")) +
#   # theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
#   # theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold")) +
#   # theme(strip.background = element_rect(fill = "white", color = "black")) +
#   theme(legend.title = element_text(size = 18, face = "bold")) +
#   theme(legend.text = element_text(size = 18)) +
#   theme(title = element_text(size = 15)) +
#   theme(axis.title.y = element_blank()) +
#   theme(axis.text.y = element_blank()) +
#   theme(axis.ticks.y = element_blank())


# write.csv(dat, file = "./data/Carneiro_distribution_only_exploratory_stage")
