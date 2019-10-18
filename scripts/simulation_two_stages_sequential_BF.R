
setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

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
library(pwr)
library(gsDesign)
library(BayesFactor)
library(tidyverse)
library(data.table)
library(coda)


#source additional functions
source("./scripts/safeguard_function.R")
source("./scripts/functions_for_simulation_twosided.R")


# Step  1 Generate an effect size distribution
#ES_true <- c(rbeta(1000000, 5, 5),rbeta(1000000, 1, 5))
ES_true <- c(rbeta(1000000, 1, 5))
hist(ES_true)

#how many hypothesis over .3 threshold
prev_pop <- round(sum(ES_true > 0.3)/1000000, 2)
n_exp <- 500
current_ES <- sample(ES_true, n_exp)
hist(current_ES)

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
# aa <- (unlist(map(exp_data_summary, "CI")))
# select_experiments <- which(((apply(cbind(aa[seq(1, length(aa)-1, 2)],
#                                           aa[seq(2, length(aa), 2)]),
#                                     1, function(x){min((x))}) < -.3)))


#alternative: this decision depends on whether exploratory result is significant (p <= .05) or not
bb <- (unlist(map(exp_data_summary, "p_value")))
select_experiments <- which(((apply(cbind(bb[seq(1, length(bb)-1, 2)],
                                          bb[seq(2, length(bb), 2)]),
                                    1, function(x){min((x))}) < .05)))

length(select_experiments)
df$effect[select_experiments]

### remove experiments that have ES < 0
select_experiments <- select_experiments[df$effect[select_experiments] > 0]

rep_attempts <- length(select_experiments)

false_omission_rate <-
  sum(current_ES[-select_experiments] > .3)/
  length(current_ES[-select_experiments])
hist(current_ES[-select_experiments])

true_selection_rate <-
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
#A. fixed sample size >> separate script
#B. Sequential with interim analysis
#C. Sequential with BF >> separate script


### B GROUP SEQUENTIAL DESIGN WITH 3 INTERIM ANALYSES 

final_res <- matrix(NA, nrow = 100000,
                    ncol = 9,
                    dimnames = list(NULL, c("rep_no", "ES_true", "totalN", "nstage", "beta", 
                                            "d_emp", "BF", "stage", "H0")))

final_res_counter <- 1


### determine bounds and critical values of the sequential design >> asymmetric upper and lower bounds
# gs_design <- gsDesign::gsDesign(k = 3, test.type = 5, alpha = 0.025, beta = .2,
#                                 delta = .5, n.fix = 1, timing = 1,
#                                 sfu = sfHSD, sfupar = -4,
#                                 sfl = sfHSD, sflpar = -6)
# 
# plot(gs_design)
# plot(gs_design, plottype = 5)


for(exp_no in select_experiments) {
  replication <- 
    generate_study(ES_true = current_ES[exp_no],
                   sample_size = rep_sample_size[exp_no])
  print(exp_no)
  
  beta <- 0.2
  
  n <- rep_sample_size[exp_no]
  
  
  samp1      <- data.frame(cbind(control = replication$values[replication$treatment == "control"][1:ceiling(n/3)],
                                 treat   = replication$values[replication$treatment == "treat"][1:ceiling(n/3)]))
  N1         <- nrow(samp1)
  stage_1_BF <- ttestBF(x = samp1[ , 1],
                        y = samp1[ , 2],
                        rscale = "medium",
                        posterior = FALSE,
                        paired = FALSE)
  
  BF1        <- extractBF(stage_1_BF, onlybf = TRUE)
  hit_upper1 <- BF1 >= 3
  hit_lower1 <- BF1 < round(1/3, 2)
  samples    <- posterior(stage_1_BF, iterations = 1000)
  output     <- summary(samples) # check for delta
  delta_emp  <- output$statistics[4]
  stage      <- 1
  
  if (hit_upper1 == TRUE) {
    print(paste("stage 1 trial success"))
    
    
    final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                        totalN = n, nstage = N1, beta,
                                        d_emp = delta_emp, BF = BF1, stage, H0 = 2)
    
    final_res_counter <- final_res_counter + 1 
    
    next;
    
  } else if (hit_lower1 == TRUE) {
    
    print(paste("stage 1 trial stopped for futility"))
    
    final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                        totalN = n, nstage = N1, beta,
                                        d_emp = delta_emp, BF = BF1, stage, H0 = 2)
    
    final_res_counter <- final_res_counter + 1
    
    next;
    
  } else {
    
    print(paste("continue to stage 2"))
    
    final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                        totalN = n, nstage = N1, beta,
                                        d_emp = delta_emp, BF = BF1, stage, H0 = 0)
    
    final_res_counter <- final_res_counter + 1
    
    ### sample data points for stage 2
    samp2      <- data.frame(cbind(control = replication$values[replication$treatment == "control"][(nrow(samp1) + 1):(nrow(samp1) * 2)],
                                   treat   = replication$values[replication$treatment == "treat"][(nrow(samp1) + 1):(nrow(samp1) * 2)]))
    samp2      <- rbind(samp2, samp1)
    N2         <- nrow(samp2)
    stage_2_BF <- ttestBF(x = samp2[ , 1],
                          y = samp2[ , 2],
                          rscale = "medium",
                          posterior = FALSE,
                          paired = FALSE)
    
    BF2        <- extractBF(stage_2_BF, onlybf = TRUE)
    hit_upper2 <- BF2 >= 3
    hit_lower2 <- BF2 < round(1/3, 2)
    samples    <- posterior(stage_2_BF, iterations = 1000)
    output     <- summary(samples) # check for delta
    delta_emp  <- output$statistics[4]
    stage      <- 2- (mean(samp2[ , 2]) - mean(samp2[ , 1]))
    
    
    if (hit_upper2 == TRUE) {
      print(paste("stage 2 trial success"))
      
      final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                          totalN = n, nstage = N2, beta,
                                          d_emp = delta_emp, BF = BF2, stage, H0 = 2)
      
      final_res_counter <- final_res_counter + 1
      
      next;
      
    } else if (hit_lower2 == TRUE) {
      
      print(paste("stage 2 trial stopped for futility"))
      
      final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                          totalN = n, nstage = N2, beta,
                                          d_emp = delta_emp, BF = BF2, stage, H0 = 1)
      
      final_res_counter <- final_res_counter + 1
      
      next;
      
    } else {
      
      print(paste("continue to stage 3"))
      
      final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                          totalN = n, nstage = N2, beta,
                                          d_emp = delta_emp, BF = BF2, stage, H0 = 0)
      
      final_res_counter <- final_res_counter + 1
      
      ### sample data points for stage 3
      
      samp3      <- data.frame(cbind(control = replication$values[replication$treatment == "control"][(nrow(samp2) + 1):(nrow(samp1) + nrow(samp2))],
                                     treat   = replication$values[replication$treatment == "treat"][(nrow(samp2) + 1):(nrow(samp1) + nrow(samp2))]))
      samp3      <- rbind(samp3, samp2)
      samp3      <- na.omit(samp3)
      N3         <- nrow(samp3)
      stage_3_BF <- ttestBF(x = samp3[ , 1],
                            y = samp3[ , 2],
                            rscale = "medium",
                            posterior = FALSE,
                            paired = FALSE)
      
      BF3        <- extractBF(stage_3_BF, onlybf = TRUE)
      hit_upper3 <- BF3 >= 3
      hit_lower3 <- BF3 < round(1/3, 2)
      samples    <- posterior(stage_3_BF, iterations = 1000)
      output     <- summary(samples) # check for delta
      delta_emp  <- output$statistics[4]
      stage      <- 3
      
      
      if (hit_upper3 == TRUE) {
        print(paste("stage 3 trial success"))
        
        final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                            totalN = n, nstage = N3, beta,
                                            d_emp = delta_emp, BF = BF3, stage, H0 = 2)
        
        final_res_counter <- final_res_counter + 1
        
        next;
        
      } else if (hit_lower3 == TRUE) {
        
        print(paste("stage 3 trial stopped for futility"))
        
        final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                            totalN = n, nstage = N3, beta,
                                            d_emp = delta_emp, BF = BF3, stage, H0 = 1)
        
        final_res_counter <- final_res_counter + 1
        
        next;
        
      } else {
        
        print(paste("trial terminated without success"))
        
        final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                            totalN = n, nstage = N3, beta,
                                            d_emp = delta_emp, BF = BF3, stage, H0 = 0)
        
        final_res_counter <- final_res_counter + 1
        
      }
      
    }
    
  }
  
}


final <- as.data.frame(final_res)

final <- 
  final %>% 
  filter(totalN != "NA")

write.csv(final, file = "./data/testBF_twosided_BF3")
