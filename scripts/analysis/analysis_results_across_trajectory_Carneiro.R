library(tidyverse)

setwd("~/Documents/SimulateTranslation/")


extractor <- function(k) {
  
  test_data <- read_csv(paste("./data/Carneiro_distribution/Frequentist_analysis/exploratory_stage_equiv_", k, sep = ""))
}

sets_equiv <- c("0.1", "0.3", "0.5", "0.7", "1.0")

exploratory_data <- list()

for (i in sets_equiv) {
  
  exploratory_data[[i]] <- extractor(i)
  print(i)
  
}