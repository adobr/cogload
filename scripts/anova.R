library(dplyr)
library(afex)
library(emmeans)

# ── Подготовка датасета ────────────────────────────────────────────────────────

prepare_data <- function(path) {
  df <- readRDS(path)
  df %>%
    mutate(
      id = factor(id),
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

# ── ANOVA функция ──────────────────────────────────────────────────────────────

run_anova <- function(data, var, label) {
  cat("\n\n══════════════════════════════════════════\n")
  cat("Variable:", label, "\n")
  cat("══════════════════════════════════════════\n")

  fit <- aov_ez(
    id        = "id",
    dv        = var,
    data      = data,
    within    = c("EL", "IL"),
    anova_table = list(es = "pes")  # partial eta squared
  )

  print(fit)

  # Post-hoc если взаимодействие значимо
  p_interaction <- fit$anova_table["EL:IL", "Pr(>F)"]
  if (!is.na(p_interaction) && p_interaction < 0.05) {
    cat("\n— Post-hoc (EL × IL interaction significant, p =",
        round(p_interaction, 3), "):\n")
    em <- emmeans(fit, ~ EL * IL)
    print(pairs(em, adjust = "bonferroni"))
  }

  invisible(fit)
}

vars <- c("error_rate", "mean_IL", "mean_EL", "mean_GL")
var_labels <- c(
  error_rate = "Error Rate",
  mean_IL    = "Mean Intrinsic Load",
  mean_EL    = "Mean Extraneous Load",
  mean_GL    = "Mean Germane Load"
)

# ── Study 1 ───────────────────────────────────────────────────────────────────

cat("\n\n##########################################\n")
cat("STUDY 1 (PILOT)\n")
cat("##########################################\n")

results1 <- lapply(vars, function(v) run_anova(data1, v, var_labels[[v]]))
names(results1) <- vars

# ── Study 2 ───────────────────────────────────────────────────────────────────

cat("\n\n##########################################\n")
cat("STUDY 2\n")
cat("##########################################\n")

results2 <- lapply(vars, function(v) run_anova(data2, v, var_labels[[v]]))
names(results2) <- vars
