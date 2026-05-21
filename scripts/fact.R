library(lavaan)
library(dplyr)

data <- readRDS('/Users/dobrokhotova/Documents/GitHub/cogload/data/data1.rds')

leppink_vars <- c("IL1","IL2","IL3","EL1","EL2","EL3","GL1","GL2","GL3","GL4")
leppink_data <- data %>% select(all_of(leppink_vars))

# CFA модель по структуре Leppink et al. (2013)
model <- '
  IL =~ IL1 + IL2 + IL3
  EL =~ EL1 + EL2 + EL3
  GL =~ GL1 + GL2 + GL3 + GL4
'

fit <- cfa(model, data = leppink_data, estimator = "ML")

# Основные результаты
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Индексы подгонки отдельно
fitMeasures(fit, c("cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

# Факторные нагрузки
standardizedSolution(fit) %>% filter(op == "=~")
