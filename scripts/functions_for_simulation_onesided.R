### Simulation for a simplified preclinical research chain
### We assume that we are interested in one primary outcome and only have two groups 
### (control and treatment) to compare

### STAGE 1: EXPLORATORY STUDY
### in this stage a study is conducted with
### 1. a true effect size (mean difference between two groups): ES_true
### 2. a sample size per group: sample_size
### 3. a systematic lab bias when estimating the effect size: l_bias
### 4. a population standard deviation: pop_sd
### This will result in an initial sample and some derived meaures

library(tidyverse)

generate_study <- function(ES_true = 1, sample_size = 10, l_bias = 0, pop_sd = 1) {
  ES_mod <- ES_true + l_bias
  #print(sample_size)
  sample_data <- data.frame(values = c(rnorm(sample_size, 0, pop_sd),
                                       rnorm(sample_size, ES_mod, pop_sd)),
                            treatment = rep(c("control", "treat"),
                                            each = sample_size))
  return(sample_data)
}

### get some info on effect size and p_value from an exploratory study
get_summary_study <- function(study_data, confidence = .8) {
  study_summary <-
    study_data %>% 
    group_by(treatment) %>% 
    summarise(mean_effect = mean(values),
              sd_effect = sd(values)) %>% 
    mutate(p_value = t.test(study_data$values ~ study_data$treatment)$p.value,
           CI = ((t.test(study_data$values ~ study_data$treatment, 
                         conf.level = confidence)$conf.int)),
           CI_95 = ((t.test(study_data$values ~ study_data$treatment,
                            conf.level = .95)$conf.int)))
}


### get some info on effect size and p_value from a replication study
### the replication study employs a one-sided test
get_summary_study_rep <- function(study_data, confidence = .8) {
  study_summary <-
    study_data %>% 
    group_by(treatment) %>% 
    summarise(mean_effect = mean(values),
              sd_effect = sd(values)) %>% 
    mutate(p_value = t.test(study_data$values ~ study_data$treatment,
                            alternative = "less")$p.value,
           CI = ((t.test(study_data$values ~ study_data$treatment,
                         alternative = "less",
                         conf.level = confidence)$conf.int)),
           CI_95 = ((t.test(study_data$values ~ study_data$treatment,
                            alternative = "less",
                            conf.level = .95)$conf.int)))
}

### based on such a study calculate the sample size needed for a confirmatory study
### use different approaches to calculate the replication sample size
### method 1: safeguard power analysis
### method 2: standard power analysis with ES from exploratory study
### method 3: standard power analysis with smallest ES of interest
calc_sample_size <- function(study_summary, study_data, max_sample_size = 200,
                             alpha = .05, power = .8, method = 2) {
  aa<-study_summary
  
  sg_sample <- safeguard.d(d = (aa$mean_effect[2] - aa$mean_effect[1]),
                       n.1 = 10, n.2 = 10,
                       sig.level = .05, power = .8, conf = 0.8) #safeguard
  if (method == 1) {
    if (as.numeric(sg_sample[2])/2 > max_sample_size | as.numeric(sg_sample[1] < 0)) 
      return(max_sample_size) 
    else return(as.numeric(sg_sample[2])/2)
  }
  if (method == 2) es_measured <- abs(aa$mean_effect[2] - aa$mean_effect[1]) #take initial study main effect
  if (method == 3) es_measured <- .3 #fixed effect size for all experiments
  sample_size <- nrow(study_data)/2
  #sd_measured<-sqrt((aa$sd_effect[1]^2+aa$sd_effect[2]^2)/2)
  bb <- power.t.test(delta = es_measured, sd = 1, sig.level = alpha, power = power,
                     type = "two.sample",
                     alternative = "one.sided")
  if (es_measured > 0) {
    if(bb$n > max_sample_size) 
      return(max_sample_size) 
    else return(bb$n)
  }
}



