setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

rm(list = ls())


library(pwr)
library(gsDesign)

#source additional functions
source("./scripts/safeguard_function.R")
#source("./scripts/functions_for_simulation_twosided.R")
source("./scripts/functions_for_simulation_onesided.R")

# Step  1 Generate an effect size distribution
#ES_true <- c(rbeta(1000000, 5, 5),rbeta(1000000, 1, 5))
ES_true <- c(rbeta(1000000, 1, 5))
df_ES <- as.data.frame(ES_true)

hist(ES_true)


ggplot(data = df_ES,
       aes(x = ES_true)) +
  geom_histogram(binwidth = .03,
                 color = "black", fill = "white") +
  labs(x = "True effect size", y = "Frequency") +
  ggtitle("Distribution of true unknown effect sizes") +
  theme_bw()

#how many hypothesis over .3 threshold
prev_pop <- round(sum(ES_true > 0.3)/1000000, 2)
n_exp <- 10000
current_ES <- sample(ES_true, n_exp)
df_current <- as.data.frame(current_ES)
#hist(current_ES)

ggplot(data = df_current,
       aes(x = current_ES)) +
  geom_histogram(binwidth = .03,
                 color = "black", fill = "white") +
  labs(x = "True effect size", y = "Frequency") +
  ggtitle("Distribution of true unknown effect sizes") +
  theme_bw()

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
                           method = 2))



### combine effect from exploratory study with calculated sample sizes  
ff <- (unlist(map(exp_data_summary, "mean_effect")))
ff <- cbind(ff[control = seq(1, length(ff)-1, 2)],
            ff[treatment = seq(2, length(ff), 2)])

df <- data.frame(control = ff[ , 1], treatment = ff[ , 2])
df$effect <- (df$treatment - df$control)
df$rep_sample_size <- rep_sample_size
#hist(df$effect)

ggplot(data = df,
       aes(x = effect)) +
  geom_histogram(binwidth = .05,
                 color = "black", fill = "white") +
  labs(x = "Empirical effect size", y = "Frequency") +
  ggtitle("Distribution empirical effect sizes from exploratory study") +
  theme_bw()


#Decision to go on
#this decision depends on an equivalence test with a bound of .3
#only experiments replicated that include .3 in the CI around the ES measured
# aa <- (unlist(map(exp_data_summary, "CI")))
# select_experiments <- which(((apply(cbind(aa[seq(1, length(aa)-1, 2)],
#                                           aa[seq(2, length(aa), 2)]),
#                                     1, function(x) {min((x))} ) < -.3)))


#alternative: this decision depends on whether exploratory result is significant (p <= .05) or not
bb <- (unlist(map(exp_data_summary, "p_value")))
select_experiments <- which(((apply(cbind(bb[seq(1, length(bb)-1, 2)],
                                          bb[seq(2, length(bb), 2)]),
                                    1, function(x){min((x))}) < .05)))

length(select_experiments)
df$effect[select_experiments]
hist(df$effect[select_experiments], breaks = 30)

### remove experiments that have ES < 0
select_experiments <- select_experiments[df$effect[select_experiments] > 0]

df_selected <- data.frame(sel_ES = df$effect[select_experiments])

hist(df$effect[select_experiments], breaks = 30)

ggplot(data = df_selected,
       aes(x = sel_ES)) +
  geom_histogram(binwidth = .03,
                 color = "black", fill = "white") +
  labs(x = "Empirical effect size", y = "Frequency") +
  ggtitle("Distribution empirical effect sizes from exploratory study") +
  theme_bw()

equiv_met2_fixN <- read.csv(file = "./data/equiv_method2_fixN_onesided")

sig_met2_fixN <- read.csv(file = "./data/sig_method2_fixN_onesided")

hist(equiv_met2_fixN$d_emp)
hist(equiv_met2_fixN$ES_true)

equiv_met2_fixN <-
  equiv_met2_fixN %>% 
  filter(p_value <= .05)

hist(equiv_met2_fixN$d_emp)
hist(equiv_met2_fixN$ES_true)

median(equiv_met2_fixN$d_emp)
median(equiv_met2_fixN$ES_true)


hist(sig_met2_fixN$d_emp)
hist(sig_met2_fixN$ES_true)


sig_met2_fixN <-
  sig_met2_fixN %>% 
  filter(p_value <= .05)

hist(sig_met2_fixN$d_emp)
hist(sig_met2_fixN$ES_true)

median(sig_met2_fixN$d_emp)
median(sig_met2_fixN$ES_true)
