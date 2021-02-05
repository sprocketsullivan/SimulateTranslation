
facet_names <- 
  c("equivalence.SESOI" = "SESOI within CI \nSESOI ",
    "significance.SESOI" = "Significance \nSESOI",
    "equivalence.standard" = "SESOI within CI \nStandard",
    "significance.standard" = "Significance \nStandard")
 
plot_FPR <- 
  ggplot(data = outcomes_10EU,
         aes(x = SESOI, y = FPR,
             shape = trajectory,
             color = trajectory)) + 
  geom_point(size = 3) + 
  geom_line() +
  # facet_wrap(~ trajectory, nrow = 2, ncol = 2,  labeller = labeller(.rows = facet_names)) + 
  labs(x = "SESOI", y = "False positive rate") +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 1.0)) +
  scale_color_manual(values = c("#E69F00", "#CC79A7",  "#0072B2", "#009E73")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 11)) +
  theme(axis.title.y = element_text(size = 11)) +
  theme(axis.text.x = element_text(size = 7, colour = "black")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black"))

plot_FPR


colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pie(rep(1, 8), col = colorBlindBlack8)


plot_PPV <- 
  ggplot(data = outcomes_10EU,
         aes(x = SESOI, y = PPV_pop_prev,
             shape = trajectory,
             color = trajectory)) + 
  geom_point(size = 3) + 
  # facet_wrap(~ trajectory, nrow = 2, ncol = 2,  labeller = labeller(.rows = facet_names)) + 
  labs(x = "SESOI", y = "Positive predictive value") +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 1.0)) +
  scale_color_manual(values = c("#E69F00", "#CC79A7",  "#0072B2", "#009E73")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 11)) +
  theme(axis.title.y = element_text(size = 11)) +
  theme(axis.text.x = element_text(size = 7, colour = "black")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black"))

plot_PPV

plot_FNR <- 
  ggplot(data = outcomes_10EU,
         aes(x = SESOI, y = FNR,
             shape = trajectory,
             color = trajectory)) + 
  geom_point(size = 3) + 
  # facet_wrap(~ trajectory, nrow = 2, ncol = 2,  labeller = labeller(.rows = facet_names)) + 
  labs(x = "SESOI", y = "False negative rate") +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 1.0)) +
  scale_color_manual(values = c("#E69F00", "#CC79A7",  "#0072B2", "#009E73")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 11)) +
  theme(axis.title.y = element_text(size = 11)) +
  theme(axis.text.x = element_text(size = 7, colour = "black")) +
  theme(axis.text.y = element_text(size = 10, colour = "black")) +
  theme(strip.text.x = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.text.y = element_text(size = 11, colour = "black", face = "bold")) +
  theme(strip.background = element_rect(fill = "white", color = "black"))

plot_FNR
