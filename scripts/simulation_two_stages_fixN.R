setwd("~/Documents/QUEST/PhD/R/Simulation_framework")

rm(list = ls())


# Here we simulate a preclinical study
# in the first stage an exploratory study is cinducted with a limited number of animals
# in the second stage this experiment is replicated based on some information from the first experiment
# in the third stage a multi center study is conducted
# outcome for each experiment is 
# 1. the measured effect size after each run (with associated CI) and p-value
# 2. the number of animals 
# 3. the true effect size


library(pwr)
library(gsDesign)


#source additional functions
source("./scripts/safeguard_function.R")
source("./scripts/functions_for_sim.R")

# Step  1 Generate an effect size distribution
#ES_true <- c(rbeta(100000,6,5),rbeta(100000,1,5))
ES_true <- c(rbeta(100000, 1, 5))
hist(ES_true)

#how many hypothesis over .3 threshold
sum(ES_true > 0.3)
n_exp <- 10000
current_ES <- sample(ES_true, n_exp)
hist(current_ES)
sum(current_ES > 0.3)

#conduct intial study
exploratory_data <- list()
for(i in 1:n_exp)
  exploratory_data[[i]] <- 
  generate_study(current_ES[i])


#the confidence interval generated here is used in the equivalence test
exp_data_summary <- list()
for(i in 1:n_exp)
  exp_data_summary[[i]] <- 
  get_summary_study(study_data = exploratory_data[[i]], 
                    confidence = .8)


#now estimate sample size
#A. Safeguard (1 in function)
#B. Initial effect size (probably the worst solution but good as a floor benchmark, 2 in function)
#C. Set a minimum detectable effect size. With this all experiments will have the same number of EU (3 as function parameter)
rep_sample_size <- NULL
for(i in 1:n_exp)
  rep_sample_size[i] <- 
  ceiling(calc_sample_size(study_summary = exp_data_summary[[i]],
                           study_data = exploratory_data[[i]],
                           method = 1))
rep_sample_size
mean(rep_sample_size)

### combine effect from exploratory study with calculated sample sizes  
ff <- (unlist(map(exp_data_summary, "mean_effect")))
ff <- cbind(ff[control = seq(1, length(ff)-1, 2)],
            ff[treatment = seq(2, length(ff), 2)])

df <- data.frame(control = ff[ , 1], treatment = ff[ , 2])
df$effect <- (df$treatment - df$control)
df$rep_sample_size <- rep_sample_size


#Decision to go on
#this decision depends on an equivalence test with a bound of .3
#only experiments replicated that include .3 in the CI around the ES measured
aa <- (unlist(map(exp_data_summary, "CI")))
select_experiments <- which(((apply(cbind(aa[seq(1, length(aa)-1, 2)],
                                          aa[seq(2, length(aa), 2)]),
                                    1, function(x) {min((x))} ) < -.3)))


#alternative: this decision depends on whether exploratory result is significant (p <= .05) or not
# bb <- (unlist(map(exp_data_summary, "p_value")))
# select_experiments <- which(((apply(cbind(bb[seq(1, length(bb)-1, 2)],
#                                               bb[seq(2, length(bb), 2)]),
#                                         1, function(x){min((x))}) < .05)))

length(select_experiments)
df$effect[select_experiments]

select_experiments <- select_experiments[df$effect[select_experiments] > 0]

length(select_experiments)

sum(current_ES[-select_experiments] > .3)/
  length(current_ES[-select_experiments])
hist(current_ES[-select_experiments])

sum(current_ES[select_experiments] > .3)/
  length(current_ES[select_experiments])
hist(current_ES[select_experiments])

sum(df$effect[select_experiments] > .3)/
  length(df$effect[select_experiments])
hist(df$effect[select_experiments])

sum(df$effect[-select_experiments] > .3)/
  length(df$effect[-select_experiments])
hist(df$effect[-select_experiments])

hist(rep_sample_size[select_experiments])



#conduct replication experiments
#A. fixed sample size
#B. Sequential with interim analysis
#C. Sequential with BF 

#A
replication_data <- list()
rep_exp_no <- 0

for(i in select_experiments) {
  
  rep_exp_no <- rep_exp_no+1
  replication_data[[rep_exp_no]] <-
    generate_study(ES_true = current_ES[i],
                   sample_size = rep_sample_size[i])
}

replication_stage_summary <- list()

for(i in 1:rep_exp_no) {
  
  replication_stage_summary[[i]] <- get_summary_study_rep(replication_data[[i]])
}

res_summary_rep <-
  data.frame(rep_no = c(1:rep_exp_no),
             totalN = rep_sample_size[select_experiments],
             p_value = unlist(map(replication_stage_summary, 
                                  "p_value"))[seq(1, 2*rep_exp_no, 2)],
             mean_control = unlist(map(replication_stage_summary, 
                                       "mean_effect"))[seq(1, 2*rep_exp_no, 2)],
             mean_treatment = unlist(map(replication_stage_summary, 
                                         "mean_effect"))[seq(2, 2*rep_exp_no, 2)],
             ES_true = current_ES[select_experiments])


res_summary_rep <-
  res_summary_rep %>% 
  mutate(d_emp = mean_treatment - mean_control)

ggplot(aes(y = d_emp, x = ES_true, col = p_value < .05),
       data = res_summary_rep) +
  geom_point(alpha = 0.4)


write.csv(res_summary_rep, file = "./data/equiv_method1_fixN_onesided")



success <-
  res_summary_rep %>% 
  filter(p_value < .05)

nrow(success)/nrow(res_summary_rep)
sum(success$d_emp > .3)/nrow(success)
hist(success$d_emp, breaks = 50)

no_sig <-
  res_summary_rep %>% 
  filter(p_value > .05)

nrow(no_sig)/nrow(res_summary_rep)
sum(no_sig$d_emp > .3)/nrow(no_sig)
hist(no_sig$d_emp, breaks = 50)



