
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
source("./scripts/functions_for_simulation_onesided.R")


# Step  1 Generate an effect size distribution
#ES_true <- c(rbeta(1000000, 5, 5),rbeta(1000000, 1, 5))
ES_true <- c(rbeta(1000000, 1, 5))
hist(ES_true)

#how many hypothesis over .3 threshold
prev_pop <- round(sum(ES_true > 0.3)/1000000, 2)
n_exp <- 1000
current_ES <- sample(ES_true, n_exp)
hist(current_ES)
all_positives <- sum(current_ES > .3)
all_negatives <- n_exp - all_positives

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

### decision to go on
### this decision depends on an equivalence test with a bound of .3
### only experiments replicated that include .3 in the CI around the ES measured
aa <- (unlist(map(exp_data_summary, "CI")))
select_experiments <- which(((apply(cbind(aa[seq(1, length(aa)-1, 2)],
                                          aa[seq(2, length(aa), 2)]),
                                    1, function(x){min((x))}) < -.3)))


### alternative: 
### this decision depends on whether exploratory result is significant (p <= .05) or not
# bb <- (unlist(map(exp_data_summary, "p_value")))
# select_experiments <- which(((apply(cbind(bb[seq(1, length(bb)-1, 2)],
#                                               bb[seq(2, length(bb), 2)]),
#                                         1, function(x){min((x))}) < .05)))

length(select_experiments)
df$effect[select_experiments]


### remove experiments that have ES < 0
select_experiments <- select_experiments[df$effect[select_experiments] > 0]

rep_attempts <- length(select_experiments)

SESOI_selected <- sum(current_ES[select_experiments] > .3)

SESOI_not_selected <- sum(current_ES[-select_experiments] > .3)

# false_omission_rate <-
#   sum(current_ES[-select_experiments] > .3)/
#   length(current_ES[-select_experiments])
# hist(current_ES[-select_experiments])
# 
# true_selection_rate <-
#   sum(current_ES[select_experiments] > .3)/
#   length(current_ES[select_experiments])
# hist(current_ES[select_experiments])
# 
# sum(df$effect[select_experiments] > .3)/
#   length(df$effect[select_experiments])
# hist(df$effect[select_experiments])
# 
# sum(df$effect[-select_experiments] > .3)/
#   length(df$effect[-select_experiments])
# hist(df$effect[-select_experiments])

hist(rep_sample_size[select_experiments], breaks = 50)

### conduct replication experiments
### A. fixed sample size >> separate script
### B. Sequential with interim analysis
### C. Sequential with BF >> separate script


### C GROUP SEQUENTIAL DESIGN WITH 3 INTERIM ANALYSES BAYES FACTOR


final_res <- matrix(NA, nrow = 100000,
                    ncol = 15,
                    dimnames = list(NULL, c("rep_no", "ES_true", "totalN", "nstage", "beta", "d_emp",
                                            "t_value", "p_value", "BF", "stage", "H0", "prev_pop",
                                            "rep_attempts", "all_positives", "all_negatives")))


final_res_counter <- 1


### determine bounds and critical values of the sequential design
### one-sided test
### stop for futility if empirical ES < 0
gs_design <- gsDesign::gsDesign(k = 3, test.type = 1, alpha = 0.05, beta = .2,
                                delta = .1, n.fix = 1, timing = 1,
                                sfu = sfHSD, sfupar = -4)

gs_design

plot(gs_design)
plot(gs_design, plottype = 5)


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
  t1 <- t.test(samp1[ , 2], samp1[ , 1], 
               alternative = "greater")
  BF1 <- ttest.tstat(t1$statistic, N1, N1, rscale = "medium",
                     complement = T, simple = T)
  # BF1        <- extractBF(stage_1_BF, onlybf = T)
  hit_upper1 <- t1$p.value <= 0.0026
  samples    <- posterior(stage_1_BF, iterations = 1000)
  output     <- summary(samples) # check for delta
  delta_emp  <- output$statistics[4]
  stage      <- 1
  
  if (hit_upper1 == TRUE) {
    print(paste("stage 1 trial success"))
    
    
    final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                        totalN = n, nstage = N1, beta,
                                        d_emp = delta_emp, t_value = t1$statistic, p_value = t1$p.value, 
                                        BF = BF1, stage, H0 = 2,
                                        prev_pop, rep_attempts, all_positives, all_negatives)
    
    final_res_counter <- final_res_counter + 1 
    
    next;
    
  } else if (hit_upper1 == FALSE & t1$statistic < 0) {
    
    print(paste("stage 1 trial stopped for futility"))
    
    final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                        totalN = n, nstage = N1, beta,
                                        d_emp = delta_emp, t_value = t1$statistic, p_value = t1$p.value, 
                                        BF = BF1, stage, H0 = 1,
                                        prev_pop, rep_attempts, all_positives, all_negatives)
    
    final_res_counter <- final_res_counter + 1 
    
    next;
    
  } else {
    
    print(paste("continue to stage 2"))
    
    final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                        totalN = n, nstage = N1, beta,
                                        d_emp = delta_emp, t_value = t1$statistic, p_value = t1$p.value, 
                                        BF = BF1, stage, H0 = 0,
                                        prev_pop, rep_attempts, all_positives, all_negatives)
    
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
    t2         <- t.test(samp2[ , 2], samp2[ , 1],
                         alternative = "greater")
    BF2 <- ttest.tstat(t2$statistic, N2, N2, rscale = "medium",
                       complement = T, simple = T)
    # BF2        <- extractBF(stage_2_BF, onlybf = TRUE)
    hit_upper2 <- t2$p.value <= 0.0110
    samples    <- posterior(stage_2_BF, iterations = 1000)
    output     <- summary(samples) # check for delta
    delta_emp  <- output$statistics[4]
    stage      <- 2
    
    
    if (hit_upper2 == TRUE) {
      print(paste("stage 2 trial success"))
      
      final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                          totalN = n, nstage = N2, beta,
                                          d_emp = delta_emp, t_value = t2$statistic, p_value = t2$p.value, 
                                          BF = BF2, stage, H0 = 2,
                                          prev_pop, rep_attempts, all_positives, all_negatives)
      
      final_res_counter <- final_res_counter + 1
      
      next;
      
    } else if (hit_upper2 == FALSE & t2$statistic < 0) {
      
      print(paste("stage 2 trial stopped for futility"))
      
      final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                          totalN = n, nstage = N2, beta,
                                          d_emp = delta_emp, t_value = t2$statistic, p_value = t2$p.value, 
                                          BF = BF2, stage, H0 = 1,
                                          prev_pop, rep_attempts, all_positives, all_negatives)
      
      final_res_counter <- final_res_counter + 1
      
      next;
      
    } else {
      
      print(paste("continue to stage 3"))
      
      final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                          totalN = n, nstage = N2, beta,
                                          d_emp = delta_emp, t_value = t2$statistic, p_value = t2$p.value, 
                                          BF = BF2, stage, H0 = 0,
                                          prev_pop, rep_attempts, all_positives, all_negatives)
      
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
      t3         <- t.test(samp3[, 2], samp3[, 1],
                           alternative = "greater")
      BF3 <- ttest.tstat(t3$statistic, N3, N3, rscale = "medium",
                         complement = T, simple = T)
      
      # BF3        <- extractBF(stage_3_BF, onlybf = TRUE)
      hit_upper3 <- t3$p.value <= 0.0465
      samples    <- posterior(stage_3_BF, iterations = 1000)
      output     <- summary(samples) # check for delta
      delta_emp  <- output$statistics[4]
      stage      <- 3
      
      
      if (hit_upper3 == TRUE) {
        print(paste("stage 3 trial success"))
        
        final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                            totalN = n, nstage = N3, beta,
                                            d_emp = delta_emp, t_value = t3$statistic, p_value = t3$p.value, 
                                            BF = BF3, stage, H0 = 2,
                                            prev_pop, rep_attempts, all_positives, all_negatives)
        
        final_res_counter <- final_res_counter + 1
        
        next;
        
      } else if (hit_upper3 == FALSE & t3$statistic < 0) {
        
        print(paste("stage 3 trial stopped for futility"))
        
        final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                            totalN = n, nstage = N3, beta,
                                            d_emp = delta_emp, t_value = t3$statistic, p_value = t3$p.value, 
                                            BF = BF3, stage, H0 = 1,
                                            prev_pop, rep_attempts, all_positives, all_negatives)
        
        final_res_counter <- final_res_counter + 1
        
        next;
        
      } else {
        
        print(paste("trial terminated without success"))
        
        final_res[final_res_counter, ] <- c(rep_no = exp_no, ES_true = current_ES[exp_no], 
                                            totalN = n, nstage = N3, beta,
                                            d_emp = delta_emp, t_value = t3$statistic, p_value = t3$p.value, 
                                            BF = BF3, stage, H0 = 0,
                                            prev_pop, rep_attempts, all_positives, all_negatives)
        
        final_res_counter <- final_res_counter + 1
        
      }
      
    }
    
  }
  
}


final <- as.data.frame(final_res)

final <- 
  final %>% 
  filter(totalN != "NA")

write.csv(final, file = "./data/testBF_onesided_futility_equiv_method1")

max(final$BF)  

final <-
  final %>% 
  filter(BF < 100)

ggplot(data = final, aes(x = factor(stage), y = BF)) +
  geom_point(alpha = .3) +
  facet_wrap( ~ H0) +
  theme_bw()


ggplot(data = final, aes(x = BF)) +
  geom_density(fill = "darkgoldenrod1", alpha = .5) 

ggplot(data = final, aes(x = BF)) +
  geom_histogram(alpha = .5, binwidth = 1) +
  theme_bw()

ggplot(data = final, aes(x = t_value)) +
  geom_density(fill = "darkgoldenrod1", alpha = .5) 

### boxplot of effect size estimates
ggplot(aes(x = factor(nstage), y = d_emp), data = final) +
  geom_boxplot(outlier.alpha = .3) +
  #geom_hline(aes(yintercept = d), color = "red", lty = 2) +
  #facet_wrap(~ d, nrow = 2, ncol = 4) +
  labs(x = "Total sample size", y = "Empirical effect size estimate") +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, colour = "black")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12, colour = "black"))







