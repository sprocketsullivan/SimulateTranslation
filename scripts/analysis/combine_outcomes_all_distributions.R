setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")


### read in Carneiro outcomes data

res_Carneiro_equiv_SESOI_1.0 <- 
  read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_outcomes_equiv_method2_1.0")

res_Carneiro_equiv_SESOI_1.0$distribution <- "Pessimistic"
res_Carneiro_equiv_SESOI_1.0$SESOI        <- 1

res_Carneiro_equiv_SESOI_0.5 <- 
  read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_outcomes_equiv_method2_0.5")

res_Carneiro_equiv_SESOI_0.5$distribution <- "Pessimistic"
res_Carneiro_equiv_SESOI_0.5$SESOI        <- 0.5

res_Carneiro_sig_standard_1.0 <- 
  read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_outcomes_sig_method1_1.0")

res_Carneiro_sig_standard_1.0$distribution <- "Pessimistic"
res_Carneiro_sig_standard_1.0$SESOI        <- 1

res_Carneiro_sig_standard_0.5 <- 
  read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_outcomes_sig_method1_0.5")

res_Carneiro_sig_standard_0.5$distribution <- "Pessimistic"
res_Carneiro_sig_standard_0.5$SESOI        <- 0.5

### read in Szucs outcomes data

res_Szucs_equiv_SESOI_1.0 <- 
  read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_outcomes_equiv_method2_1.0")

res_Szucs_equiv_SESOI_1.0$distribution <- "Optimistic"
res_Szucs_equiv_SESOI_1.0$SESOI        <- 1

res_Szucs_equiv_SESOI_0.5 <- 
  read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_outcomes_equiv_method2_0.5")

res_Szucs_equiv_SESOI_0.5$distribution <- "Optimistic"
res_Szucs_equiv_SESOI_0.5$SESOI        <- 0.5

res_Szucs_sig_standard_1.0 <- 
  read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_outcomes_sig_method1_1.0")

res_Szucs_sig_standard_1.0$distribution <- "Optimistic"
res_Szucs_sig_standard_1.0$SESOI        <- 1

res_Szucs_sig_standard_0.5 <- 
  read.csv(file = "./data/Szucs_distribution/Frequentist_analysis/Szucs_outcomes_sig_method1_0.5")

res_Szucs_sig_standard_0.5$distribution <- "Optimistic"
res_Szucs_sig_standard_0.5$SESOI        <- 0.5

### combine data sets from both distrbutions

outcomes_all_distributions <- 
  bind_rows(res_Carneiro_equiv_SESOI_1.0,
            res_Carneiro_equiv_SESOI_0.5,
            res_Carneiro_sig_standard_1.0,
            res_Carneiro_sig_standard_0.5,
            res_Szucs_equiv_SESOI_1.0,
            res_Szucs_equiv_SESOI_0.5,
            res_Szucs_sig_standard_1.0,
            res_Szucs_sig_standard_0.5)

# outcomes_all_distributions$trajectory <- 
#   interaction(outcomes_all_distributions$decision_crit, outcomes_all_distributions$sampsize_approach)

outcomes_all_distributions$trajectory <- 
  rep(c("T2", "T1"), each = 2, 2)

# save(outcomes_all_distributions, file = "./data/all_outcomes.RData")

outcomes_all_distributions_init_samp_size_10 <-
  outcomes_all_distributions %>% 
  filter(init_sample_size == 10)

# save(outcomes_all_distributions_init_samp_size_10, file = "./data/all_outcomes_init_samp_size_10.RData")
