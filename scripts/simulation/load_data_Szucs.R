
setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")


source("./scripts/simulation/load_packages.R")

data <- readMat("./scripts/simulation/journal.pbio.2000797.s005.mat")

#str(data)

data_of_interest <- data$D

t_value <- unlist(data_of_interest[1])

df <- unlist(data_of_interest[2])

ES_data <- as.data.frame(cbind(t_value, df))


ES_data <-
  ES_data %>% 
  mutate(Dt1 = t_value / (sqrt(df + 1)),
         Dt2 = 2 * t_value / (sqrt(df + 2)))

ES_data <-
  ES_data %>% 
  mutate(D_small_sample = .93 * ES_data$Dt1 + .07 * ES_data$Dt2,
         D_large_sample = .72 * ES_data$Dt1 + .28 * ES_data$Dt2)


ES_small_sample <-
  ES_data %>% 
  filter(df <= 10) %>% 
  select(t_value, df, D_small_sample)

names(ES_small_sample)[3] <- "D"

ES_large_sample <-
  ES_data %>% 
  filter(df > 10) %>% 
  select(t_value, df, D_large_sample)

names(ES_large_sample)[3] <- "D"

ES_data_Szucs <-
  bind_rows(ES_small_sample, ES_large_sample)

# save(ES_data_Szucs, file = "ES_data_Szucs.RData")

min(ES_data_Szucs$D)
max(ES_data_Szucs$D)

sum(ES_data_Szucs$D > 20)

median(ES_data_Szucs$D)
mean(ES_data_Szucs$D)

ES_small <-
  ES_data_Szucs %>%
  filter(D <= 20)



# ggplot(data = ES_data_Szucs, aes(x = df, y = D)) +
#   geom_jitter(alpha = .5) +
#   theme_bw()
# 
# ggplot(data = ES_small, aes(x = D)) +
#   geom_histogram(binwidth = .3, color = "black", fill = "white", size = 0.3) +
#   theme_bw()
# 
# ggplot(data = ES_data_Szucs, aes(y = D)) +
#   geom_boxplot(outlier.alpha = .5) +
#   theme_bw()


#write.csv(ES_data_Szucs, file = "/Users/meggiedanziger/Documents/QUEST/PhD/R/SimulateTranslation/scripts/ES_data_Szucs.csv")

setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")
