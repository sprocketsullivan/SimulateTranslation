setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

rm(list = ls())


# source additional scripts

# source("./scripts/simulation/sim_fixN_Carneiro_decision_equivalence_with_SESOI.R")

source("./scripts/simulation/sim_fixN_Carneiro_decision_significance.R")

dat

sum(dat$effect < 0) # empirical effect sizes are negative because t.test function takes control - treat
sum(dat$effect > 0) 
sum(dat$ES_true < 0)

data <-
  dat %>% 
  group_by(init_sample_size, study_id) %>% 
  filter(selection_sig == 1)

sum(data$effect < 0)
sum(data$effect > 0)
sum(data$effect == 0)

selected <-
  data %>% 
  group_by(init_sample_size) %>% 
  summarize(selected = sum(selection_sig == 1))

# now estimate sample size for replication study
# A. Initial effect size (probably the worst solution but good as a floor benchmark, 1 in function)
# B. Set a SESOI. With this all experiments will have the same number of EU (2 as function parameter)

rep_sample_size_std <- NULL

for (i in 1:nrow(data)) {

  rep_sample_size_std[i] <-
    ceiling(calc_sample_size(data = data[i, ], sample_size = data[i, ]$init_sample_size,
                             method = 1))
}

data$rep_samp_size_std <- rep_sample_size_std

max(data$rep_samp_size_std)

hist(data$effect)

data$effect <- ifelse(data$effect < 0, -data$effect, -data$effect)

hist(data$effect, breaks = 200)
hist(data$ES_true, breaks = 200)

sum(data$effect > 0)
sum(data$effect < 0)
sum(data$effect == 0)

negative_ES <-
  data %>% 
  group_by(init_sample_size) %>% 
  summarize(neg_ES = sum(effect < 0))

percent_selected <-
  selected %>% 
  mutate(neg_ES = negative_ES$neg_ES,
         selected_no_negatives = selected-neg_ES,
         per_selected = selected_no_negatives / 10000 * 100)

rep_attempts <- 
  rep(c(percent_selected$selected_no_negatives[1],
        percent_selected$selected_no_negatives[2],
        percent_selected$selected_no_negatives[3]), 
      c(percent_selected$selected_no_negatives[1],
        percent_selected$selected_no_negatives[2],
        percent_selected$selected_no_negatives[3]))

replication_data <- list()

rep_exp_no <- 0

select_experiments <- which(data$selection_sig == 1)

# select_experiments <- which(data$selection_equiv == 1)

select_experiments <- select_experiments[data$effect[select_experiments] >= 0 ]

current_ES_rep <- data$ES_true

for(i in select_experiments) {
  
  rep_exp_no <- rep_exp_no + 1
  
  replication_data[[rep_exp_no]] <-
    generate_study(ES_true = current_ES_rep[i],
                   sample_size = rep_sample_size_std[i])
  
  replication_data[[rep_exp_no]] <-
    replication_data[[rep_exp_no]] %>% 
    mutate(study_id = rep_exp_no)
}

# replication_data[[11138]]

plan(multiprocess)
rep_data_summary <- 
  future_map(replication_data, get_summary_study_rep)

# rep_data_summary[[9559]]

# data$ES_true[select_experiments]

res_summary_rep <-
  data.frame(init_sample_size = data$init_sample_size[select_experiments],
             rep_no = c(1:rep_exp_no),
             rep_sample_size = rep_sample_size_std[select_experiments],
             #rep_sample_size = rep_sample_size_std,
             t_value = unlist(map(rep_data_summary, "t_value")),
             p_value = unlist(map(rep_data_summary, "p_value")), #[seq(1, 2*rep_exp_no, 2)],
             effect = unlist(map(rep_data_summary, "effect")),
             ES_true = data$ES_true[select_experiments])
             # rep_attempts = rep_attempts)

hist(res_summary_rep$effect, breaks = 200)




# res_summary_rep$effect <- ifelse(res_summary_rep$effect < 0,
#                                  -res_summary_rep$effect, -res_summary_rep$effect)
# 
# write.csv(res_summary_rep,
#           file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_distribution_equiv_method2_1.0")
# 
# res_summary_rep <-
#   res_summary_rep %>%
#   filter(effect < 5)
# 
# ggplot(aes(y = effect, x = ES_true, col = p_value < .05),
#        data = res_summary_rep) +
#   facet_wrap(~ factor(init_sample_size)) +
#   geom_point(alpha = 0.2) +
#   geom_hline(aes(yintercept = .5), color = "red") +
#   theme_bw()



res_summary_rep <-
  res_summary_rep %>%
  filter(init_sample_size == 10)

# dat$effect <- ifelse(dat$effect < 0, -dat$effect, -dat$effect)

dat_large_ES <-
  res_summary_rep %>%
  filter(ES_true > 0)

dat_large_ES <-
  dat_large_ES %>% 
  mutate(significant = p_value <= 0.05)


ggplot(data = dat_large_ES, aes(x = dat_large_ES$ES_true, fill = factor(significant))) +
  geom_histogram(bins = 50, color = "black", size = 0.3, alpha = 0.8) +
  # geom_histogram(data = dat_large_ES, aes(x = ),
                 # bins = 50, color = "black", fill = "white")
  labs(x = expression(paste("Sampled effect sizes (Cohen's ", italic("d"), ") ")),
       y = "Frequency",
       fill = "Significant outcome \nat replication") +
  scale_fill_manual(breaks = c("FALSE", "TRUE"),
                    labels = c("no",
                               "yes"),
                    values = c("grey", "steelblue")) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 18, colour = "black")) +
  # theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
  # theme(strip.text.y = element_text(size = 20, colour = "black", face = "bold")) +
  # theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 18, face = "bold")) +
  theme(legend.text = element_text(size = 18)) +
  theme(title = element_text(size = 15)) +
  theme(legend.position = "none")
