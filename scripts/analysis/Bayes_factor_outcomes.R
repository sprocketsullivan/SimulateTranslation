setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/")



BF_analysis_data <- 
  read.csv(file = "./data/Carneiro_distribution/Bayes_factor_analysis/Carneiro_distribution_equiv_method1_0.5")

Frequentist_data <- 
  read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_equiv_method1_1.0")

BF_analysis_data$p_value <- Frequentist_data$p_value

BF_analysis_data$effect <- Frequentist_data$effect

BF_analysis_data$BF <- round(BF_analysis_data$BF, 3)

BF_effects <-
  BF_analysis_data %>%
  filter(init_sample_size == 10) %>% 
  filter(rep_sample_size > 20) %>% 
  filter(mean_effect < 10) %>% 
  filter(BF < 30)

ggplot(aes(y = BF, x = p_value),
       data = BF_effects) +
  facet_wrap(~ factor(init_sample_size)) +
  geom_jitter(alpha = 0.5) +
  geom_hline(aes(yintercept = 1), color = "red") +
  geom_vline(aes(xintercept = .05), color = "red") +
  theme_bw()


