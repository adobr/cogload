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

library(psych)

data <- long_df

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


scores_df <- fa_result$scores %>% as.data.frame()

library(plotly)
library(tibble)

data <- data %>%
  mutate(
    GL_factor = fa_result$scores[, "ML1"],
    IL_factor = fa_result$scores[, "ML2"],
    EL_factor = fa_result$scores[, "ML3"]
  )


library(ggplot2)
library(dplyr)

# Кастомный labeller для осей
condition_labeller <- labeller(
  EL = c("low" = "Удобный тест",
         "high" = "Неудобный тест"),
  IL = c("low" = "Простые задания",
         "high" = "Сложные задания")
)

data %>%
  group_by(EL, IL) %>%
  summarise(
    GL_factor = mean(GL_factor, na.rm = TRUE),
    IL_factor = mean(IL_factor, na.rm = TRUE),
    EL_factor = mean(EL_factor, na.rm = TRUE),
    correct_ans = mean(n_correct, na.rm = TRUE) / 10,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(IL_factor, EL_factor, GL_factor, correct_ans),
               names_to = "factor",
               values_to = "mean_value") %>%
  ggplot(aes(x = factor, y = mean_value, fill = factor)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = factor,
                vjust = ifelse(mean_value >= 0, -0.5, 1.5)),
            size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(IL ~ EL, labeller = condition_labeller) +
  labs(x = NULL, y = "Mean Factor Score",
       title = "Factor Scores by EL and IL conditions") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # убираем подписи по оси x
        axis.ticks.x = element_blank()) # убираем тики, т.к. названия уже в легенде


library(patchwork)

# График только по EL
p_EL <- data %>%
  group_by(EL) %>%
  summarise(
    GL_factor = mean(GL_factor, na.rm = TRUE),
    IL_factor = mean(IL_factor, na.rm = TRUE),
    EL_factor = mean(EL_factor, na.rm = TRUE),
    correct_ans = mean(n_correct, na.rm = TRUE) / 10,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(IL_factor, EL_factor, GL_factor, correct_ans),
               names_to = "factor",
               values_to = "mean_value") %>%
  ggplot(aes(x = factor, y = mean_value, fill = factor)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = factor,
                vjust = ifelse(mean_value >= 0, -0.5, 1.5)),
            size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(~ EL, labeller = labeller(
    EL = c("low" = "Удобный тест",
           "high" = "Неудобный тест")
  )) +
  labs(x = NULL, y = "Mean Factor Score",
       title = "Factor Scores by EL condition") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# График только по IL
p_IL <- data %>%
  group_by(IL) %>%
  summarise(
    GL_factor = mean(GL_factor, na.rm = TRUE),
    IL_factor = mean(IL_factor, na.rm = TRUE),
    EL_factor = mean(EL_factor, na.rm = TRUE),
    correct_ans = mean(n_correct, na.rm = TRUE) / 10,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(IL_factor, EL_factor, GL_factor, correct_ans),
               names_to = "factor",
               values_to = "mean_value") %>%
  ggplot(aes(x = factor, y = mean_value, fill = factor)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = factor,
                vjust = ifelse(mean_value >= 0, -0.5, 1.5)),
            size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(~ IL, labeller = labeller(
    IL = c("low" = "Простые задания",
           "high" = "Сложные задания")
  )) +
  labs(x = NULL, y = "Mean Factor Score",
       title = "Factor Scores by IL condition") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Объединяем
p_EL / p_IL