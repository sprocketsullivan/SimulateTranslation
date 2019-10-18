setwd("~/Documents/QUEST/PhD/R/SimulateTranslation/data")

rm(list = ls())

library(tidyverse)
library(ggpubr)
library(ggridges)


### read in data sets from the different trajectories 
### using sequential design in confirmatory study
final_1 <- read.csv(file = "equiv_method1_seq_onesided")
final_2 <- read.csv(file = "equiv_method2_seq_onesided")
final_3 <- read.csv(file = "equiv_method3_seq_onesided")

final_4 <- read.csv(file = "sig_method1_seq_onesided")
final_5 <- read.csv(file = "sig_method2_seq_onesided")
final_6 <- read.csv(file = "sig_method3_seq_onesided")


### create one data file containing all data
final <- rbind(final_1, final_2, final_3, final_4, final_5, final_6)

### add column that codes decision criterion from exploratory stage to confirmatory stage
final$decision_crit <- c(rep("equivalence", nrow(final_1) + nrow(final_2) + nrow(final_3)),
                         rep("significance", nrow(final_4) + nrow(final_5) + nrow(final_6)))

### add column that codes approach to determine sample size for confirmatory study
final$sampsize_approach <- c(rep(1, nrow(final_1)), rep(2, nrow(final_2)), rep(3, nrow(final_3)),
                             rep(1, nrow(final_4)), rep(2, nrow(final_5)), rep(3, nrow(final_6)))

### add column that codes design applied in confirmatory study
final$design <- rep("group_sequential")

### exclude all cases that do not have either a significant or futile outcome
### select columns relevant for plotting
datseq <-
  final %>% 
  filter(H0 != 0) %>% 
  select(rep_no, decision_crit, sampsize_approach, 
         design, totalN, nstage, ES_true, d_emp, H0)

### create subsets to refer to row numbers later
final_1 <-
  datseq %>% 
  filter(decision_crit == "equivalence" & 
         sampsize_approach == 1 &
         design == "group_sequential")

final_2 <-
  datseq %>% 
  filter(decision_crit == "equivalence" & 
           sampsize_approach == 2 &
           design == "group_sequential")

final_3 <-
  datseq %>% 
  filter(decision_crit == "equivalence" & 
           sampsize_approach == 3 &
           design == "group_sequential")

final_4 <-
  datseq %>% 
  filter(decision_crit == "significance" & 
           sampsize_approach == 1 &
           design == "group_sequential")

final_5 <-
  datseq %>% 
  filter(decision_crit == "significance" & 
           sampsize_approach == 2 &
           design == "group_sequential")

final_6 <-
  datseq %>% 
  filter(decision_crit == "significance" & 
           sampsize_approach == 3 &
           design == "group_sequential")

################################################################################################################

### read in data sets from the different trajectories 
### using fixed-N design in confirmatory study
final_fix1 <- read.csv(file = "equiv_method1_fixN_onesided")
final_fix2 <- read.csv(file = "equiv_method2_fixN_onesided")
final_fix3 <- read.csv(file = "equiv_method3_fixN_onesided")

final_fix4 <- read.csv(file = "sig_method1_fixN_onesided")
final_fix5 <- read.csv(file = "sig_method2_fixN_onesided")
final_fix6 <- read.csv(file = "sig_method3_fixN_onesided")

### create one data file containing all data
finalfix <- rbind(final_fix1, final_fix2, final_fix3, final_fix4, final_fix5, final_fix6)

### add column that codes decision criterion from exploratory stage to confirmatory stage
finalfix$decision_crit <- c(rep("equivalence", nrow(final_fix1) + nrow(final_fix2) + nrow(final_fix3)),
                            rep("significance", nrow(final_fix4) + nrow(final_fix5) + nrow(final_fix6)))

### add column that codes approach to determine sample size for confirmatory study
finalfix$sampsize_approach <- c(rep(1, nrow(final_fix1)), rep(2, nrow(final_fix2)), rep(3, nrow(final_fix3)),
                                rep(1, nrow(final_fix4)), rep(2, nrow(final_fix5)), rep(3, nrow(final_fix6)))

### add column that codes design applied in confirmatory study
finalfix$design <- rep("fixN")

### add column for outcome significant / not significant to match outcome column
### of sequential design data set
finalfix$H0 <- ifelse(finalfix$p_value <= .05, 2, 1)

### rename column to match column of sequential design data set
finalfix$nstage <- finalfix$totalN

### select columns relevant for plotting
datfix <-
  finalfix %>% 
  select(rep_no, decision_crit, sampsize_approach, 
         design, totalN, nstage, ES_true, d_emp, H0)

### create subsets to refer to row numbers later
final_fix1 <-
  datfix %>% 
  filter(decision_crit == "equivalence" & 
           sampsize_approach == 1 &
           design == "fixN")

final_fix2 <-
  datfix %>% 
  filter(decision_crit == "equivalence" & 
           sampsize_approach == 2 &
           design == "fixN")

final_fix3<-
  datfix %>% 
  filter(decision_crit == "equivalence" & 
           sampsize_approach == 3 &
           design == "fixN")

final_fix4 <-
  datfix %>% 
  filter(decision_crit == "significance" & 
           sampsize_approach == 1 &
           design == "fixN")

final_fix5 <-
  datfix %>% 
  filter(decision_crit == "significance" & 
           sampsize_approach == 2 &
           design == "fixN")

final_fix6 <-
  datfix %>% 
  filter(decision_crit == "significance" & 
           sampsize_approach == 3 &
           design == "fixN")

################################################################################################################

### combine data sets containing sequential and fixed-N experiments
dat <- rbind(datseq, datfix)


dat$design <- as.factor(dat$design)
levels(dat$design)
levels(dat$design) <- c("Fixed-N \ndesign", "Group sequential \ndesign")

dat$decision_crit <- as.factor(dat$decision_crit)
levels(dat$decision_crit)
levels(dat$decision_crit) <- c("Equivalence", "Significance")

# ggplot(data = dat, aes(x = ES_true, y = d_emp, color = factor(H0))) +
#   geom_point(alpha = .5, size = .7) +
#   facet_grid(design ~ decision_crit) +
#   geom_vline(xintercept = .3, color = "red", lty = 2, size = 0.7) +
#   geom_hline(yintercept = .3, color = "red", lty = 2, size = 0.7) +
#   labs(x = "True effect size", y = "Empirical effect size",
#        color = "Significance") +
#   scale_color_manual(breaks = c(1, 2), 
#                      values = c("darkblue", "darkgoldenrod1"),
#                      labels = c("false", "true")) +
#   theme_bw() +
#   theme(strip.text.x = element_text(size = 12, colour = "black", face = "bold")) +
#   theme(strip.text.y = element_text(size = 12, colour = "black", face = "bold")) +
#   theme(strip.background = element_rect(fill = "white", color = "black")) +
#   theme(axis.title.x = element_text(size = 13)) +
#   theme(axis.title.y = element_text(size = 13)) +
#   theme(axis.text = element_text(size = 12, colour = "black")) +
#   theme(legend.title = element_text(size = 13)) +
#   theme(legend.text = element_text(size = 12))

dat_ES <-
  dat %>% 
  select(d_emp, ES_true, H0, design, decision_crit)


dat_ES_long <-
  dat_ES %>% 
  gather(ES, value = value, -H0, -design, -decision_crit)

# ggplot(data = dat_ES_long, aes(x = ES, y = value, color = factor(H0))) +
#   geom_hline(yintercept = .3, color = "red", lty = 2, size = 0.7) +
#   geom_boxplot(outlier.alpha = .5, outlier.size = .7) +
#   facet_grid(design ~ decision_crit) +
#   labs(x = "", y = "Effect size",
#        color = "Significance") +
#   scale_color_manual(breaks = c(1, 2), 
#                      values = c("darkblue", "darkgoldenrod1"),
#                      labels = c("false", "true")) +
#   scale_x_discrete(labels = c("Empirical \neffect size",
#                               "True \neffect size")) +
#   theme_bw() +
#   theme(strip.text.x = element_text(size = 12, 
#                                     colour = "black", face = "bold")) +
#   theme(strip.text.y = element_text(size = 12, 
#                                     colour = "black", face = "bold")) +
#   theme(strip.background = element_rect(fill = "white", color = "black")) +
#   theme(axis.title.x = element_text(size = 13)) +
#   theme(axis.title.y = element_text(size = 13)) +
#   theme(axis.text = element_text(size = 12, colour = "black")) +
#   theme(legend.title = element_text(size = 13)) +
#   theme(legend.text = element_text(size = 12))
# 
# 
# ggplot(data = dat) +
#   geom_density(aes(x = d_emp, fill = factor(H0)), 
#                position = "dodge", alpha = .4) +
#   geom_density(aes(x = ES_true, color = factor(H0)),
#                position = "dodge", size = 0.7) +
#   facet_grid(design ~ decision_crit) +
#   geom_vline(xintercept = .3, color = "red", lty = 2, size = 0.7) +
#   labs(x = "Effect size", y = "Density",
#        fill = "Significance") +
#   scale_fill_manual(aes(x = d_emp),
#                     breaks = c(1, 2),
#                     values = c("darkblue", "darkgoldenrod1"),
#                     labels = c("false", "true")) +
#   scale_color_manual(aes(x = ES_true),
#                      breaks = c(1, 2),
#                      values = c("darkblue", "darkgoldenrod1"),
#                      labels = c("false", "true")) +
#   xlim(-.5, 1.5) +
#   theme_bw() +
#   theme(strip.text.x = element_text(size = 12, colour = "black", face = "bold")) +
#   theme(strip.text.y = element_text(size = 12, colour = "black", face = "bold")) +
#   theme(strip.background = element_rect(fill = "white", color = "black")) +
#   theme(axis.title.x = element_text(size = 13)) +
#   theme(axis.title.y = element_text(size = 13)) +
#   theme(axis.text = element_text(size = 12, colour = "black")) +
#   theme(legend.title = element_text(size = 13)) +
#   theme(legend.text = element_text(size = 12))
# 
# 
# ggplot(data = dat, aes(x = d_emp, fill = factor(H0))) +
#   geom_density(position = "dodge", alpha = .7) +
#   facet_grid(design ~ decision_crit) +
#   geom_vline(xintercept = .3, color = "red", lty = 2) +
#   labs(x = "Empirical effect size", y = "Density",
#        fill = "Significance") +
#   scale_fill_manual(breaks = c(1, 2),
#                     values = c("darkblue", "darkgoldenrod1"),
#                     labels = c("false", "true")) +
#   theme_bw() +
#   theme(strip.text.x = element_text(size = 12, colour = "black", face = "bold")) +
#   theme(strip.text.y = element_text(size = 12, colour = "black", face = "bold")) +
#   theme(strip.background = element_rect(fill = "white", color = "black")) +
#   theme(axis.title.x = element_text(size = 13)) +
#   theme(axis.title.y = element_text(size = 13)) +
#   theme(axis.text = element_text(size = 12, colour = "black")) +
#   theme(legend.title = element_text(size = 13)) +
#   theme(legend.text = element_text(size = 12))


### create data fraem for plotting mean number of animals for each trajectory
plot_data <-
  dat %>% 
  group_by(decision_crit, sampsize_approach, design) %>% 
  summarize(mean_N = mean(nstage))


not_sig <-
  dat %>% 
  group_by(decision_crit, sampsize_approach, design, H0) %>% 
  filter(H0 == 1) %>% 
  summarize(events = n())

not_sig$N <-   c(nrow(final_fix1), nrow(final_1),
                 nrow(final_fix2), nrow(final_2),
                 nrow(final_fix3), nrow(final_3),
                 nrow(final_fix4), nrow(final_4),
                 nrow(final_fix5), nrow(final_5),
                 nrow(final_fix6), nrow(final_6))

not_sig <-
  not_sig %>% 
  mutate(percent = events/N*100)


sig <-
  dat %>% 
  group_by(decision_crit, sampsize_approach, design, H0) %>% 
  filter(H0 == 2) %>% 
  summarize(events = n())

sig$N <-     c(nrow(final_fix1), nrow(final_1),
                    nrow(final_fix2), nrow(final_2),
                    nrow(final_fix3), nrow(final_3),
                    nrow(final_fix4), nrow(final_4),
                    nrow(final_fix5), nrow(final_5),
                    nrow(final_fix6), nrow(final_6))

sig <-
  sig %>% 
  mutate(percent = events/N*100)

all_positives <- sig$events

plot_data$sig <- sig$percent
plot_data$not_sig <- not_sig$percent


# plot_data2 <-
#   plot_data %>% 
#   gather(key = outcome, value = value, 
#          - decision_crit, - sampsize_approach, - design, - mean_N)

ggplot(aes(x = factor(sampsize_approach), y = sig, fill = (mean_N)), 
       data = plot_data) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  facet_grid(design ~ decision_crit) +
  labs(x = "Approach to determine sample size",
       y = "Percentage") +
  scale_fill_gradient2(low = "navy", high = "darkgoldenrod1", 
                       mid = "white", midpoint = 100) + 
  # scale_fill_manual(values = c("darkblue", "darkgoldenrod1"),
  #                   labels = c("False negative", "True positive")) +
  scale_x_discrete(labels = c("Safeguard", "Standard", "SESOI")) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 12, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11))

ggplot(aes(x = factor(sampsize_approach), y = not_sig, fill = (mean_N)), 
       data = plot_data) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  facet_grid(design ~ decision_crit) +
  labs(x = "Approach to determine sample size",
       y = "Percentage") +
  scale_fill_gradient2(low = "navy", high = "darkgoldenrod1", 
                       mid = "white", midpoint = 100) + 
  # scale_fill_manual(values = c("darkblue", "darkgoldenrod1"),
  #                   labels = c("False negative", "True positive")) +
  scale_x_discrete(labels = c("Safeguard", "Standard", "SESOI")) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 12, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11))


plot_data$decision_crit <- as.factor(plot_data$decision_crit)
levels(plot_data$decision_crit)
levels(plot_data$decision_crit) <- c("Equivalence", "Significance")


plot1 <- 
  ggplot(plot_data , aes(x = factor(design), y = factor(sampsize_approach))) +
  geom_raster(aes(fill = mean_N), interpolate = F) +
  scale_fill_gradient2(low = "navy", high = "darkgoldenrod1", 
                       mid = "white", midpoint = 100) +
  labs(x = "Design", y = "Sample size calculation",
       fill = "Mean # of \nanimals needed") +
  scale_x_discrete(labels = c("Fixed-N \ndesign",
                              "Sequential \ndesign")) +
  scale_y_discrete(labels = c("Safeguard", "Standard", "SESOI")) +
  facet_wrap(~ decision_crit) +
  theme_classic() +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size = 10, colour = "black",
                                   angle = 45)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  guides(fill = guide_colorbar(barwidth = 0.7, barheight = 6))


plot2 <-
  ggplot(plot_data , aes(x = factor(design), y = factor(sampsize_approach))) +
  geom_raster(aes(fill = not_sig), interpolate = F) +
  scale_fill_gradient2(low = "navy", high = "darkgoldenrod1",
                       mid = "white", midpoint = 15) +
  labs(x = "Design", y = "Approach to determine \nsample size",
       fill = "Rate of \nfalse negatives") +
  scale_x_discrete(labels = c("Fixed-N \ndesign",
                              "Sequential \ndesign")) +
  scale_y_discrete(labels = c("Safeguard", "Standard", "SESOI")) +
  facet_wrap(~ decision_crit) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_text(size = 10, colour = "black",
                                   angle = 45)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  guides(fill = guide_colorbar(barwidth = 0.7, barheight = 6))


#sig_all <- as.data.frame(sig_all)

plot3 <- 
  ggplot(plot_data , aes(x = factor(design), y = factor(sampsize_approach))) +
  geom_raster(aes(fill = sig), interpolate = F) +
  scale_fill_gradient2(low = "navy", high = "darkgoldenrod1", 
                       mid = "white", midpoint = 25) +
  labs(x = "Design", y = "Sample size calculation",
       fill = "Rate of \npositive predictive value") +
  scale_x_discrete(labels = c("Fixed-N \ndesign",
                              "Sequential \ndesign")) +
  scale_y_discrete(labels = c("Safeguard", "Standard", "SESOI")) +
  facet_wrap(~ decision_crit) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(strip.background = element_rect(fill = "white", color = "black")) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y =element_blank()) +
  theme(axis.text.x = element_text(size = 10, colour = "black")) +
  theme(axis.text.y = element_text(size = 10, colour = "black",
                                   angle = 45)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 11)) +
  guides(fill = guide_colorbar(barwidth = 0.7, barheight = 6))

#plot3

final_plot <-
  ggarrange(plot1, plot2, plot3, 
            nrow = 3, ncol = 1,
            align = "v",
            heights = c(1, .9, 1.05),
            legend = "right")

final_plot
