
# generates study data for exploratory phase
generate_study <- function(ES_true = 1, sample_size = samp_size, l_bias = 0, pop_sd = 1) {
  ES_mod <- ES_true + l_bias
  sample_data <- data.frame(values = c(rnorm(sample_size, 0, pop_sd),
                                       rnorm(sample_size, ES_mod, pop_sd)),
                            treatment = rep(c("control", "treat"),
                                            each = sample_size),
                            sample_size = sample_size)
  
  
  return(sample_data)
}

# performs t-test and generates summary of study data for exploratory phase
get_summary_study <- function(study_data) {
  
  t <- t.test(study_data$values ~ study_data$treatment,
              alternative = "two.sided",
              var.equal = FALSE,
              conf.level = .95)
  
  p_value <- t$p.value
  CI      <- t$conf.int
  effect  <- 2 * t$statistic/sqrt(nrow(study_data)) # two-sided t-test
  
  # d_emp <- (dat_sum$mean_group[1] - dat_sum$mean_group[2]) /
  #           sqrt((dat_sum$sd_group[1]^2 + dat_sum$sd_group[2]^2)/2)
  # 
  # d_emp2 <- (2 * t$statistic/sqrt(20)
  
  study_summary <-
    study_data %>% 
    group_by(sample_size, study_id, treatment) %>% 
    summarize(mean_group = mean(values),
              sd_group = sd(values)) %>% 
    mutate(t_value = round(t$statistic, 4),
           p_value = round(t$p.value, 4),
           CI_lower = round(t$conf.int[1], 4),
           CI_upper = round(t$conf.int[2], 4),
           effect = round(effect, 4))
  
  study_summary <-
    study_summary %>% 
    group_by(sample_size, study_id, t_value, p_value, CI_lower, CI_upper) %>% 
    summarize(effect = mean(effect))
  
}

# performs t-test and generates summary of study data for replication
get_summary_study_rep <- function(study_data) {

  t <- t.test(study_data$values ~ study_data$treatment,
              alternative = "less",
              var.equal = FALSE,
              conf.level = .95)

  p_value <- t$p.value
  CI      <- t$conf.int
  effect  <- t$statistic/sqrt(nrow(study_data)) # one-sided t-test

  study_summary <-
    study_data %>%
    group_by(study_id, treatment) %>%
    summarize(mean_group = mean(values),
              sd_group = sd(values)) %>%
    mutate(t_value = round(t$statistic, 4),
           p_value = round(t$p.value, 4),
           CI_lower = round(t$conf.int[1], 4),
           CI_upper = round(t$conf.int[2], 4),
           effect = round(effect, 4))

  study_summary <-
    study_summary %>%
    group_by(study_id, t_value, p_value, CI_lower, CI_upper) %>%
    summarize(effect = mean(effect))

}

# performs t-test and generates summary of study data for replication with Bayes factors
get_summary_study_rep_BF <- function(study_data) {

  BF <- ttestBF(x = study_data$values[study_data$treatment == "treat"],
                y = study_data$values[study_data$treatment == "control"],
                rscale = "medium",
                pposterior = TRUE)

  #compute Bayesian effect size estimate
  samples     <- posterior(BF, iterations = 10000)
  res_samples <- summary(samples) #check for delta
  # plot(samples)

  #compute 95% HDI (highest posterior density interval)
  HPDint      <- HPDinterval(samples, prob = .95) #check for delta
  
  
  study_summary <-
    study_data %>%
    select(study_id) %>%
    summarize(BF = round(exp(BF@bayesFactor$bf), 4),
              mean_effect = round(res_samples$statistics[4, 1], 4),
              sd_effect = round(res_samples$statistics[4, 2], 4),
              CI_lower = round(HPDint[4, 1], 4),
              CI_upper = round(HPDint[4, 2], 4))
  
}

# decision criterion significance at exploratory stage
get_decision_sig <- function(study_summary, pval_threshold) {
  
  bb <- study_summary$p_value
  bb <- ifelse(bb <= pval_threshold, 1, 0)
  
}

# decision criterion SESOI at exploratory stage
get_decision_equiv <- function(exploratory_data_summary,
                               SESOI) {
  
  helper <-
    cbind(exploratory_data_summary$CI_lower,
          exploratory_data_summary$CI_upper)
  
  select_experiments <- apply(helper, 1, function(x) {min(x)} < - SESOI)
  
  select_experiments <- ifelse(select_experiments == TRUE, 1, 0)
  
}

# sample size calculation for replication
# option to change method
# method 1: standard sample size calculation
# method 2: sample size calculation using SESOI, requires specification of SESOI and power
calc_sample_size <- function(data, sample_size, max_sample_size = 200,
                             alpha = .05, power = .8, SESOI, method = 2) {
  
  aa <- data
  
  if (method == 1) {
    es_measured <- abs(aa$effect) #take initial study main effect
    if(es_measured > 16) es_measured <- 16
    if(es_measured == 0) es_measured <- 0.0001
    
  }  
  
  if (method == 2) 
    es_measured <- SESOI #fixed effect size for all experiments
  
  bb <- power.t.test(delta = es_measured, sd = 1, sig.level = alpha, power = power,
                     type = "two.sample",
                     alternative = "one.sided")
  if (es_measured > 0) {
    if(bb$n > max_sample_size) 
      return(max_sample_size) 
    else return(bb$n)
    
  }
  
}


