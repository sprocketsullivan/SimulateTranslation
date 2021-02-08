setwd("~/Documents/SimulateTranslation/data")

load("exploratory_data_SESOI_Carneiro_10.RData")

exploratory_data_Carneiro_10_0.5 <-
  exploratory_data_Carneiro_10 %>% 
  filter(SESOI == 0.5)

ES_histrogram_Carneiro_selected_SESOI_0.5 <-
  ggplot(data = exploratory_data_Carneiro_10_0.5, 
         aes(x = ES_true, fill = factor(selection_equiv))) +
  geom_histogram(bins = 90, color = "black",
                 size = 0.3, alpha = 0.8) +
  labs(#x = "",
    x = expression(paste("Cohen's ", italic("d"))),
    y = "Frequency",
    fill = "Selected for \nreplication") +
  scale_fill_manual(breaks = c("0", "1"),
                    labels = c("no",
                               "yes"),
                    values = c("white", "#0072B2")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 15, colour = "black")) +
  theme(axis.text.y = element_text(size = 15, colour = "black")) +
  theme(legend.position = "none")

ES_histrogram_Carneiro_selected_SESOI_0.5

exploratory_data_Carneiro_10_1.0 <-
  exploratory_data_Carneiro_10 %>% 
  filter(SESOI == "1.0")

ES_histrogram_Carneiro_selected_SESOI_1.0 <-
  ggplot(data = exploratory_data_Carneiro_10_1.0, 
         aes(x = ES_true, fill = factor(selection_equiv))) +
  geom_histogram(bins = 90, color = "black",
                 size = 0.3, alpha = 0.8) +
  labs(#x = "",
    x = expression(paste("Cohen's ", italic("d"))),
    y = "Frequency",
    fill = "Selected for \nreplication") +
  scale_fill_manual(breaks = c("0", "1"),
                    labels = c("no",
                               "yes"),
                    values = c("white", "#0072B2")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 15, colour = "black")) +
  theme(axis.text.y = element_text(size = 15, colour = "black")) +
  theme(legend.position = "none")

ES_histrogram_Carneiro_selected_SESOI_1.0


exploratory_data_Carneiro_10_1.0 <-
  exploratory_data_Carneiro_10 %>% 
  filter(SESOI == "1.0") %>% 
  filter(selection_equiv == 1)

ES_histrogram_Carneiro_selected_SESOI <-
  ggplot(data = exploratory_data_Carneiro_10_0.5, 
         aes(x = ES_true, fill = factor(selection_equiv))) +
  geom_histogram(bins = 90, color = "black",
                 size = 0.3) +
  geom_histogram(data = exploratory_data_Carneiro_10_1.0,
                 aes(x = ES_true), 
                 bins = 90, size = 0.3, alpha = 0.4,
                 color = "black", fill = "white") +
  labs(#x = "",
    x = expression(paste("Cohen's ", italic("d"))),
    y = "Frequency",
    fill = "Selected for \nreplication") +
  scale_fill_manual(breaks = c("0", "1"),
                    labels = c("no",
                               "yes"),
                    values = c("white", "#0072B2")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 15, colour = "black")) +
  theme(axis.text.y = element_text(size = 15, colour = "black")) +
  theme(legend.position = "none")

ES_histrogram_Carneiro_selected_SESOI


#56B4E9