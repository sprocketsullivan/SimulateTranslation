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


# list_exploratory_data[[1]][[999]]

# the confidence interval generated here is used in the equivalence test
exploratory_data_summary <- list()

plan(multiprocess)
for (i in 1:length(samp_size_vector)) {
  
  exploratory_data_summary[[i]] <- 
    future_map(list_exploratory_data[[i]], get_summary_study)
  
}

# exploratory_data_summary[[2]][[99]]

# decision to go on
# this decision depends on whether exploratory result is significant (p <= .05) or not
# select studies for replication if p-value < .05
selection_sig <- list()

for (i in 1:length(samp_size_vector)) {
  
  selection_sig[[i]] <- 
    future_map(exploratory_data_summary[[i]],
               get_decision_sig,
               pval_threshold = 0.05)
  
}

row_names <- NULL
col_names <- c("init_sample_size", "study_id", "t_value",
               "p_value", "CI_lower", "CI_upper", "effect")

df <- as_tibble(matrix(unlist(exploratory_data_summary), 
                       nrow = n_exp*length(samp_size_vector), byrow = TRUE,
                       dimnames = list(c(row_names),
                                       c(col_names))))


col_name <- "selection_sig"

df_sig <- as_tibble(matrix(unlist(selection_sig), 
                           nrow = n_exp*length(samp_size_vector), byrow = TRUE,
                           dimnames = list(c(row_names),
                                           c(col_name))))


dat <- bind_cols(df, df_sig)

dat$ES_true <- rep(current_ES, 3)
# 
# # write.csv(dat, file = "./data/Carneiro_distribution/Frequentist_analysis/exploratory_stage_sig_0.05")
# 
# 
# dat <-
#   dat %>%
#   group_by(init_sample_size)
#   # filter(init_sample_size == 10) %>% 
#   # filter(ES_true > 0)
# 
# dat$effect <- ifelse(dat$effect < 0, -dat$effect, -dat$effect)
# 
# ggplot(data = dat,
#        aes(x = ES_true, y = effect, color = factor(selection_sig))) +
#   geom_point(alpha = 0.4, size = 0.5) +
#   facet_wrap(~ init_sample_size) +
#   theme_bw()
# 
# dat_selected <-
#   dat %>%
#   filter(selection_sig == 1)
# 
# dat_not_selected <-
#   dat %>% 
#   filter(selection_sig == 0)
# 
# false_positives <-
#   dat_selected %>% 
#   filter(selection_sig == 1 & ES_true < 0)
# 
# ggplot(data = false_positives,
#        aes(x = ES_true, y = effect)) +
#   geom_point(alpha = 0.4, size = 0.5) +
#   facet_wrap(~ init_sample_size) +
#   theme_bw()
# 
# false_negatives <-
#   dat_not_selected %>% 
#   filter(selection_sig == 0 & ES_true > 0)
# 
# ggplot(data = false_negatives,
#        aes(x = ES_true, y = effect)) +
#   geom_point(alpha = 0.4, size = 0.5) +
#   facet_wrap(~ init_sample_size) +
#   theme_bw()
# 
# FPR <- nrow(false_positives) / nrow(dat_selected)
# 
# FNR <- nrow(false_negatives) / nrow(dat_not_selected)




# dat$effect <- ifelse(dat$effect < 0, -dat$effect, -dat$effect)

# dat_large_ES <-
#   dat %>% 
#   filter(ES_true > 0) 
# 
# dat_selected <-
#   dat %>% 
#   filter(selection_sig == 1)
# 
# 3169/6844*100
# 
# ggplot(data = dat_large_ES, aes(x = dat_large_ES$ES_true, fill = factor(selection_sig))) +
#   geom_histogram(bins = 50, color = "black",
#                  size = 0.3, alpha = 0.8) +
#   labs(x = expression(paste("Sampled effect sizes (Cohen's ", italic("d"), ") ")),
#        y = "Frequency",
#        fill = "Selected for \nreplication") +
#   scale_fill_manual(breaks = c("0", "1"),
#                     labels = c("no",
#                                "yes"),
#                     values = c("lightgrey", "deeppink3")) +
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
#   theme(legend.position = "right")



# dat <-
#   dat %>%
#   filter(init_sample_size == 10)

# dat$effect <- ifelse(dat$effect < 0, -dat$effect, -dat$effect)
#
# ggplot(data = dat, aes(x = dat$effect)) +
#   geom_histogram(bins = 50, color = "black", fill = "white", size = 0.3, alpha = 0.8) +
#   # labs(x = expression(paste("Sampled effect sizes (Cohen's ", italic("d"), ") ")),
#   #      y = "Frequency",
#   #      fill = "Selected for \nreplication") +
#   labs(x = expression(paste("Effect sizes (Cohen's ", italic("d"), ") from exploratory studies ")),
#        y = "Frequency") +
#   scale_fill_manual(breaks = c("0", "1"),
#                     labels = c("no",
#                                "yes"),
#                     values = c("grey", "deeppink3")) +
#   theme_bw() +
#   theme(axis.title.x = element_text(size = 15)) +
#   theme(axis.title.y = element_text(size = 15)) +
#   theme(axis.text.x = element_text(size = 14, colour = "black")) +
#   theme(axis.text.y = element_text(size = 14, colour = "black")) +
#   # theme_bw() +
#   # theme(axis.title.x = element_blank()) +
#   # theme(axis.title.y = element_blank()) +
#   # theme(axis.text.x = element_text(size = 18, colour = "black")) +
#   # theme(axis.text.y = element_blank()) +
#   # theme(axis.ticks.y = element_blank()) +
#   # theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
#   # theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold")) +
#   # theme(strip.background = element_rect(fill = "white", color = "black")) +
#   theme(legend.title = element_text(size = 18, face = "bold")) +
#   theme(legend.text = element_text(size = 18)) +
#   theme(title = element_text(size = 15)) +
#   theme(legend.position = "none")
