library(tidyverse)

setwd("~/Documents/SimulateTranslation/")


extractor <- function(k) {
  
  test_data <- read_csv(paste("./data/Szucs_distribution/Frequentist_analysis/exploratory_stage_equiv_", k, sep = ""))
}

sets_equiv <- c("0.1", "0.3", "0.5", "0.7", "1.0")

exploratory_data <- list()

for (i in sets_equiv) {
  
  exploratory_data[[i]] <- extractor(i)
  print(i)
  
}

sum(exploratory_data$`0.1`$selection_equiv == 1)
sum(exploratory_data$`0.3`$selection_equiv == 1)
sum(exploratory_data$`0.5`$selection_equiv == 1)
sum(exploratory_data$`0.7`$selection_equiv == 1)
sum(exploratory_data$`1.0`$selection_equiv == 1)


exploratory_data <-
  bind_rows(exploratory_data$`0.1`,
            exploratory_data$`0.3`,
            exploratory_data$`0.5`,
            exploratory_data$`0.7`,
            exploratory_data$`1.0`)


exploratory_data$SESOI <-
  rep(sets_equiv, each = 30000)

exploratory_data_Szucs_10 <-
  exploratory_data %>% 
  filter(init_sample_size == 10) %>% 
  filter(SESOI == "1.0") %>% 
  filter(selection_equiv == 1) %>%
  filter(effect <= 0)

# save(exploratory_data_Szucs_10, file = "./data/exploratory_data_SESOI_Szucs_10.RData")


ggplot(data = exploratory_data_Szucs_10,
       aes(x = ES_true, 
           color = factor(selection_equiv), 
           fill = factor(selection_equiv))) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ SESOI, nrow = 1, ncol = 5) +
  theme_bw()
# geom_vline(xintercept = SESOI, color = "red", lty = 2)



extractor2 <- function(k) {
  
  test_data_sig <- read_csv(paste("./data/Szucs_distribution/Frequentist_analysis/exploratory_stage_sig_", k, sep = ""))
}

sets_sig <- c("0.05", "0.1")

exploratory_data_sig <- list()

for (i in sets_sig) {
  
  exploratory_data_sig[[i]] <- extractor2(i)
  print(i)
}

sum(exploratory_data_sig$`0.05`$selection_sig == exploratory_data_sig$`0.1`$selection_sig)

exploratory_data_sig <-
  bind_rows(exploratory_data_sig$`0.05`,
            exploratory_data_sig$`0.1`)

exploratory_data_sig$pval_threshold <-
  rep(sets_sig, each = 30000)

exploratory_data_sig_Szucs_10 <-
  exploratory_data_sig %>% 
  filter(init_sample_size == 10) %>% 
  filter(pval_threshold == 0.05) %>% 
  filter(selection_sig == 1) %>% 
  filter(effect <= 0)

# save(exploratory_data_sig_Szucs_10, file = "./data/exploratory_data_significance_Szucs_10.RData")

ggplot(data = exploratory_data_sig_Szucs_10,
       aes(x = ES_true, 
           color = factor(selection_sig), 
           fill = factor(selection_sig))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ pval_threshold) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "red", lty = 2)

