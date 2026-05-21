library(afex)
library(dplyr)

set.seed(42)
sig.level <- 0.05
k_items   <- 5  # заданий на условие
power_target <- 0.80

simulate_power <- function(N, eta_p, n_sim = 500) {
  f <- sqrt(eta_p / (1 - eta_p))
  sig_count <- 0
  
  for (i in 1:n_sim) {
    rows <- expand.grid(
      id   = 1:N,
      EL   = c("T1", "T2"),
      IL   = c("Easy", "Hard"),
      item = 1:k_items
    )
    
    rows$y <- with(rows, {
      mu_IL  <- ifelse(IL == "Hard",  f * 0.5, 0)
      mu_EL  <- ifelse(EL == "T2",    f * 0.5, 0)
      mu_int <- ifelse(EL == "T2" & IL == "Hard", -f * 0.5, 0)
      mu_IL + mu_EL + mu_int + rnorm(nrow(rows), sd = 1)
    })
    
    # Усредняем по item, чтобы получить одно значение на ячейку на человека
    df_agg <- rows %>%
      group_by(id, EL, IL) %>%
      summarise(y = mean(y), .groups = "drop") %>%
      mutate(id = factor(id), EL = factor(EL), IL = factor(IL))
    
    fit <- tryCatch(
      aov_ez("id", "y", df_agg, within = c("EL", "IL")),
      error = function(e) NULL,
      warning = function(w) suppressWarnings(
        aov_ez("id", "y", df_agg, within = c("EL", "IL"))
      )
    )
    
    if (!is.null(fit)) {
      p <- fit$anova_table["EL:IL", "Pr(>F)"]
      if (!is.na(p) && p < sig.level) sig_count <- sig_count + 1
    }
  }
  
  sig_count / n_sim
}

cat("══════════════════════════════════════════\n")
cat("Required N by η²p (target power = .80)\n")
cat("2x2 within-subjects, 5 items per cell\n")
cat("══════════════════════════════════════════\n\n")

N_seq <- c(5, 8, 10, 12, 15, 20, 25, 30, 40, 50)

for (eta_p in seq(0.2, 0.8, by = 0.1)) {
  found <- FALSE
  for (N in N_seq) {
    pwr <- simulate_power(N, eta_p, n_sim = 500)
    cat(sprintf("  η²p = %.1f, N = %2d → power = %.2f\n", eta_p, N, pwr))
    if (pwr >= power_target && !found) {
      cat(sprintf("  ✓ η²p = %.1f → N = %d\n\n", eta_p, N))
      found <- TRUE
      break
    }
  }
  if (!found) cat(sprintf("  η²p = %.1f → N > 50\n\n", eta_p))
}