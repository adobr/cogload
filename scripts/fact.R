library(psych)
library(dplyr)
library(tidyr)

data <- readRDS('/Users/dobrokhotova/Documents/GitHub/cogload/data/data1.rds')


leppink_vars <- c(
  "IL1",
  "IL2",
  "IL3",
  "EL1",
  "EL2",
  "EL3",
  "GL1",
  "GL2",
  "GL3",
  "GL4"
)


# Выбрать только нужные переменные
leppink_data <- data %>% select(all_of(leppink_vars))

# Запустить FA с Oblimin
fa_result <- fa(leppink_data,
                nfactors = 3,
                rotate = "oblimin",
                fm = "ml",
                scores = "regression")

fa_result


# Factor loadings
fa_result$loadings %>% print(cutoff = 0.3)
