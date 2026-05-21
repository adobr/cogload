library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# ── Подготовка датасета ────────────────────────────────────────────────────────

prepare_data <- function(path) {
  df <- readRDS(path)
  df %>%
    mutate(
      EL = factor(EL, levels = c("low", "high"), labels = c("Design Type 1", "Design Type 2")),
      IL = factor(IL, levels = c("low", "high"), labels = c("Easy", "Hard")),
      error_rate = 1 - (n_correct / 7),
      mean_IL = rowMeans(cbind(as.numeric(IL1), as.numeric(IL2), as.numeric(IL3)), na.rm = TRUE),
      mean_EL = rowMeans(cbind(as.numeric(EL1), as.numeric(EL2), as.numeric(EL3)), na.rm = TRUE),
      mean_GL = rowMeans(cbind(as.numeric(GL1), as.numeric(GL2), as.numeric(GL3), as.numeric(GL4)), na.rm = TRUE)
    )
}

data1 <- prepare_data("/Users/dobrokhotova/Documents/GitHub/cogload/data/data1.rds")
data2 <- prepare_data("/Users/dobrokhotova/Documents/GitHub/cogload/data/data2.rds")

# ── Общие настройки ────────────────────────────────────────────────────────────

vars <- c("error_rate", "mean_IL", "mean_EL", "mean_GL")

var_labels <- c(
  error_rate = "Error Rate",
  mean_IL    = "Mean Intrinsic Load",
  mean_EL    = "Mean Extraneous Load",
  mean_GL    = "Mean Germane Load"
)

colors_IL <- c("Easy" = "#4C72B0", "Hard" = "#DD8452")
colors_EL <- c("Design Type 1" = "#2CA02C", "Design Type 2" = "#9467BD")

# ── Функции построения ─────────────────────────────────────────────────────────

# Вариант 1: x = Design Type, линии = Easy/Hard
summarise_by_EL <- function(data, var) {
  data %>%
    group_by(EL, IL) %>%
    summarise(
      mean = mean(.data[[var]], na.rm = TRUE),
      se   = sd(.data[[var]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[var]]))),
      .groups = "drop"
    )
}

plot_by_EL <- function(data, var) {
  df <- summarise_by_EL(data, var)
  df_labels <- df %>% filter(EL == "Design Type 2")
  
  ggplot(df, aes(x = EL, y = mean, color = IL, group = IL)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.1, linewidth = 0.6) +
    geom_text(data = df_labels, aes(label = IL),
              hjust = -0.15, fontface = "bold", size = 4, show.legend = FALSE) +
    scale_color_manual(values = colors_IL) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.25))) +
    labs(title = var_labels[[var]], x = NULL, y = "Mean") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          legend.position = "none")
}

# Вариант 2 (транспонированный): x = Easy/Hard, линии = Design Type
summarise_by_IL <- function(data, var) {
  data %>%
    group_by(IL, EL) %>%
    summarise(
      mean = mean(.data[[var]], na.rm = TRUE),
      se   = sd(.data[[var]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[var]]))),
      .groups = "drop"
    )
}

plot_by_IL <- function(data, var) {
  df <- summarise_by_IL(data, var)
  df_labels <- df %>% filter(IL == "Hard")
  
  ggplot(df, aes(x = IL, y = mean, color = EL, group = EL)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  width = 0.1, linewidth = 0.6) +
    geom_text(data = df_labels, aes(label = EL),
              hjust = -0.15, fontface = "bold", size = 4, show.legend = FALSE) +
    scale_color_manual(values = colors_EL) +
    scale_x_discrete(expand = expansion(mult = c(0.1, 0.35))) +
    labs(title = var_labels[[var]], x = NULL, y = "Mean") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          legend.position = "none")
}

# ── Строим и сохраняем ─────────────────────────────────────────────────────────

make_combined <- function(data, plot_fn) {
  plots <- lapply(vars, function(v) plot_fn(data, v))
  (plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[4]])
}

save_path <- "/Users/dobrokhotova/Documents/GitHub/cogload/results"

# data1, x = Design Type
p1a <- make_combined(data1, plot_by_EL)
ggsave(file.path(save_path, "data1_by_design.png"), p1a, width = 12, height = 9, dpi = 300)

# data1, x = Easy/Hard (транспонированный)
p1b <- make_combined(data1, plot_by_IL)
ggsave(file.path(save_path, "data1_by_complexity.png"), p1b, width = 12, height = 9, dpi = 300)

# data2, x = Design Type
p2a <- make_combined(data2, plot_by_EL)
ggsave(file.path(save_path, "data2_by_design.png"), p2a, width = 12, height = 9, dpi = 300)

# data2, x = Easy/Hard (транспонированный)
p2b <- make_combined(data2, plot_by_IL)
ggsave(file.path(save_path, "data2_by_complexity.png"), p2b, width = 12, height = 9, dpi = 300)
