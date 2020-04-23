setwd("~/Documents/QUEST/PhD/R/SimulateTranslation")

source("./scripts/simulation/load_packages.R")

ES_data_Carneiro <- read.csv(file = "./scripts/simulation/ES_data_Carneiro.csv", sep = ";", dec = ",")

names(ES_data_Carneiro)[16] <- "ES_d"

ES_data_Carneiro <-
  ES_data_Carneiro %>% 
  select(ES_d) %>% 
  drop_na()

# zeros <- which(ES_data_Carneiro$ES_d == 0)
# sum(ES_data_Carneiro$ES_d == 0)
# mean(ES_data_Carneiro$ES_d)
# median(ES_data_Carneiro$ES_d)
# hist(ES_data_Carneiro$ES_d, breaks = 50)

