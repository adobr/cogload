install.packages("lme4")
install.packages("simr")
library(lme4)
library(simr)

# Создаём структуру данных
n_participants <- 20
n_items <- 3  # заданий на условие

# Все комбинации условий
conditions <- expand.grid(
  participant = 1:n_participants,
  complexity = c("easy", "hard"),
  design = c("design1", "design2"),
  item = 1:n_items
)

conditions$participant <- factor(conditions$participant)
conditions$complexity <- factor(conditions$complexity)
conditions$design <- factor(conditions$design)

# Зависимая переменная (пока случайная)
conditions$y <- rnorm(nrow(conditions))

# Строим модель
model <- lmer(y ~ complexity * design + (1 | participant), 
              data = conditions)

# Задаём размер эффекта
# слабый эффект: d = 0.2, что примерно соответствует beta = 0.2
fixef(model)["complexityhard"] <- 0.2
fixef(model)["designdesign2"] <- 0.2
fixef(model)["complexityhard:designdesign2"] <- 0.2

# Power analysis для каждого эффекта
power_complexity <- powerSim(model,
                             test = fixed("complexity"),
                             nsim = 1000)
print(power_complexity)

power_design <- powerSim(model,
                         test = fixed("design"),
                         nsim = 1000)
print(power_design)

power_interaction <- powerSim(model,
                              test = fixed("complexity:design"),
                              nsim = 1000)
print(power_interaction)