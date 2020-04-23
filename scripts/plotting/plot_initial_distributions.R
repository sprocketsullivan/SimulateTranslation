setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

source("./scripts/simulation/load_packages.R")
source("./scripts/simulation/load_data_Szucs.R")
source("./scripts/simulation/load_data_Carneiro.R")



##### CARNEIRO DISTRIBUTION
n_exp <- 10000
ES_true <- ES_data_Carneiro$ES_d
hist(ES_true, breaks = 200)

sum(ES_true < 0)
sum(ES_true > 0)
sum(ES_true == 0)

ES_true <- ifelse(ES_true < 0, -ES_true, -ES_true)
ES_car <- data.frame(ES_true)
sum(ES_true > 0)
sum(ES_true < 0)
sum(ES_true == 0)

hist(ES_true, breaks = 200)
md_car   <- median(ES_true)
mean_car <- mean(ES_true)

# set.seed(4321)
# current_ES <- sample(ES_true, n_exp, replace = TRUE)


ggplot(data = ES_car,
       aes(x = ES_true)) +
  geom_histogram(bins = 50, color = "black", fill = "white", size = 0.3) +
  labs(x = expression(paste("Empirical effect sizes (Cohen's ", italic("d"), ") ")),
       y = "Frequency") +
  ggtitle("Distribution of effect sizes (Carneiro et al., 2018)") +
  # geom_vline(xintercept = 0.5, color = "deeppink3", size = 1.2) +
  # geom_vline(xintercept = md_car, color = "steelblue", size = 1) +
  # geom_vline(xintercept = mean_car, color = "deeppink3", size = 1) +
  # annotate("text", x = 3, y = 19,
  #                   label = "SESOI = 0.5",
  #                   color = "deeppink3", size = 7) +
  # annotate("text", x = 3, y = 19,
  #          label = "paste(italic(Median), \" = 0.38\")", parse = TRUE, 
  #          color = "steelblue", size = 7) +
  # annotate("text", x = 3.13, y = 16.5,
  #          label = "paste(italic(Mean), \" = 0.42\")", parse = TRUE, 
  #          color = "deeppink3", size = 7) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 20, colour = "black")) +
  theme(plot.title = element_text(size = 20, face = "bold"))

ggsave("./plots/Carneiro_distribution_REC2.svg")


##### SZUCS DISTRIBUTION
n_exp <- 10000
ES_true <- ES_data_Szucs$D
hist(ES_true, breaks = 200)

sum(ES_true < 0)
sum(ES_true > 0)
sum(ES_true == 0)

hist(ES_true, breaks = 200)
md_szucs   <- median(ES_true)
mean_szucs <- mean(ES_true)

# set.seed(4321)
# current_ES <- sample(ES_true, n_exp)

ES_szucs <- data.frame(ES_true)

ES_szucs <-
  ES_szucs %>% 
  filter(ES_true < 5)

ggplot(data = ES_szucs,
       aes(x = ES_true)) +
  geom_histogram(bins = 50, color = "black", fill = "white", size = 0.3) +
  labs(x = expression(paste("Empirical effect sizes (Cohen's ", italic("d"), ") ")),
       y = "Frequency") +
  ggtitle("Distribution of effect sizes (Szucs & Ioannidis, 2017)") +
  geom_vline(xintercept = md_car, color = "steelblue", size = 1) +
  geom_vline(xintercept = mean_car, color = "deeppink3", size = 1) +
  annotate("text", x = 3.7, y = 1700,
           label = "paste(italic(Median), \" = 0.65\")", parse = TRUE, 
           color = "steelblue", size = 7) +
  annotate("text", x = 3.8, y = 1450,
           label = "paste(italic(Mean), \" = 0.94\")", parse = TRUE, 
           color = "deeppink3", size = 7) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 18, colour = "black")) +
  theme(axis.text.y = element_text(size = 20, colour = "black")) +
  theme(plot.title = element_text(size = 20, face = "bold"))

ggsave("./plots/Szucs_distribution.svg")
