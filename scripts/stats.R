#long_df <- readRDS("/Users/dobrokhotova/Yandex.Disk.localized/курсовая/data.rds")

library(dplyr)
library(lmerTest)

long_df <- long_df %>%
  mutate(
    id = factor(id),
    EL = factor(EL, levels = c("low", "high")),
    IL = factor(IL, levels = c("low", "high"))
  )

survey_vars <- c(
  "IL1",
  "IL2",
  "IL3",
  "EL1",
  "EL2",
  "EL3",
  "GL1",
  "GL2",
  "GL3",
  "GL4",
  "NASA_mental_demand",
  "NASA_performance",
  "NASA_effort",
  "NASA_frustration"
)


models <- lapply(survey_vars, function(v) {
  form <- as.formula(paste0(v, " ~ EL * IL + (1 | id)"))
  model <- lmer(form, data = long_df)
  aov_tab <- anova(model)
  
  out <- as.data.frame(aov_tab)
  out$effect <- rownames(out)
  out$variable <- v
  rownames(out) <- NULL
  out
})

anova_results <- bind_rows(models)

results_table <- anova_results %>%
  select(variable, effect, `NumDF`, `DenDF`, `F value`, `Pr(>F)`) %>%
  filter(effect %in% c("EL", "IL", "EL:IL"))

results_table <- results_table %>%
  group_by(effect) %>%
  mutate(p_adj_BH = p.adjust(`Pr(>F)`, method = "BH")) %>%
  ungroup()

results_table

library(dplyr)
library(knitr)
library(kableExtra)

sig_rows <- which(results_table$p_adj_BH < 0.05)

kbl(results_table, digits = 3, caption = "Результаты тестов") %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(sig_rows, bold = TRUE, background = "#ffd6d6")



library(dplyr)
library(lme4)

long_df <- long_df %>%
  mutate(
    id = factor(id),
    EL = factor(EL, levels = c("low", "high")),
    IL = factor(IL, levels = c("low", "high"))
  )

m_correct <- glmer(
  cbind(n_correct, 7 - n_correct) ~ EL * IL + (1 | id),
  data = long_df,
  family = binomial
)

library(car)

Anova(m_correct, type = 3)

library(ggplot2)

ggplot(long_df, aes(x = EL, y = n_correct, fill = IL)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_y_continuous(breaks = 0:7, limits = c(0, 7)) +
  labs(
    x = "Уровень EL",
    y = "Число правильных ответов",
    fill = "Уровень IL",
    title = "Число правильных ответов по 4 условиям"
  ) +
  theme_minimal()
