setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

rep_sig <- 
  read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_replication_sig_method1")
  
rep_equiv <- 
  read.csv(file = "./data/Carneiro_distribution/Frequentist_analysis/Carneiro_replication_equiv_method2_0.5")

dat <- bind_rows(rep_sig, rep_equiv)


dat_summary <-
  dat %>%
  filter(H0 == 2 & init_sample_size == 10) %>%
  group_by(decision_crit, sampsize_approach) %>% 
  summarize(mean_N = mean(rep_sample_size),
            sd_N = sd(rep_sample_size),
            ymin = mean_N - sd_N,
            ymax = mean_N + sd_N)

dat_summary$trajectory <- interaction(dat_summary$decision_crit, dat_summary$sampsize_approach)

plot_mean_N <-
  ggplot(data = dat_summary,
         aes(x = factor(trajectory), y = mean_N)) +
  geom_bar(stat = "identity", position = "dodge", 
           alpha = 0.7, size = .3,
           color = "black", fill = "steelblue") +
  geom_errorbar(aes(ymax = ymax, ymin = ymin), width = .1) +
  # facet_wrap(~ distribution) +
  # ggtitle("Mean number of animals for each trajectory") +
  labs(x = "Trajectory", y = "Mean # of animals in replication", 
       fill = "SESOI") +
  scale_x_discrete(labels = c("SESOI within CI \nSESOI", "Significance \nStandard")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 18, colour = "black")) +
  # theme(strip.text.x = element_text(size = 20, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(legend.title = element_text(size = 20, face = "bold")) +
  theme(legend.text = element_text(size = 18)) +
  theme(title = element_text(size = 15))

plot_mean_N

ggsave("./plots/Carneiro_mean_N_REC.svg")

# ggplot(data = rep_sig, aes(y = rep_sample_size)) +
#   geom_boxplot()
