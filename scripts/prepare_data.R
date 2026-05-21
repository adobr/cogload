library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)

score_multiselect <- function(x, key) {
  score_one <- function(resp) {
    if (is.na(resp) || trimws(as.character(resp)) == "") {
      return(rep(NA_integer_, length(key)))
    }
    
    # оставляем только цифры 1-7
    resp_clean <- gsub("[^1-7]", "", as.character(resp))
    
    if (resp_clean == "") {
      return(rep(NA_integer_, length(key)))
    }
    
    chosen <- unique(as.integer(strsplit(resp_clean, "")[[1]]))
    
    selected <- seq_along(key) %in% chosen
    true_items <- key == 1
    
    as.integer(selected == true_items)
  }
  
  scored <- t(sapply(x, score_one))
  scored <- as.data.frame(scored)
  names(scored) <- paste0("ans", seq_along(key))
  scored
}


key_low_low <- c(2, 1, 1, 1, 1, 2, 1)
key_low_high <- c(1, 1, 2, 2, 2, 1, 2)
key_high_low  <- c(1, 1, 2, 2, 1, 2, 2)
key_high_high <- c(1, 2, 1, 2, 1, 2, 2)

results <- read_sav("/Users/dobrokhotova/Documents/GitHub/cogload/data/raw2.sav")


long_df <- bind_rows(
  results %>%
    transmute(
      id = row_number(),
      EL = "low",
      IL = "low",
      ans1 = if_else(is.na(q2), NA_integer_, if_else(q2 == key_low_low[1], 1L, 0L)),
      ans2 = if_else(is.na(q3), NA_integer_, if_else(q3 == key_low_low[2], 1L, 0L)),
      ans3 = if_else(is.na(q4), NA_integer_, if_else(q4 == key_low_low[3], 1L, 0L)),
      ans4 = if_else(is.na(q5), NA_integer_, if_else(q5 == key_low_low[4], 1L, 0L)),
      ans5 = if_else(is.na(q6), NA_integer_, if_else(q6 == key_low_low[5], 1L, 0L)),
      ans6 = if_else(is.na(q7), NA_integer_, if_else(q7 == key_low_low[6], 1L, 0L)),
      ans7 = if_else(is.na(q8), NA_integer_, if_else(q8 == key_low_low[7], 1L, 0L))
    ),
  
  results %>%
    transmute(
      id = row_number(),
      EL = "low",
      IL = "high",
      ans1 = if_else(is.na(q14), NA_integer_, if_else(q14 == key_low_high[1], 1L, 0L)),
      ans2 = if_else(is.na(q15), NA_integer_, if_else(q15 == key_low_high[2], 1L, 0L)),
      ans3 = if_else(is.na(q16), NA_integer_, if_else(q16 == key_low_high[3], 1L, 0L)),
      ans4 = if_else(is.na(q17), NA_integer_, if_else(q17 == key_low_high[4], 1L, 0L)),
      ans5 = if_else(is.na(q18), NA_integer_, if_else(q18 == key_low_high[5], 1L, 0L)),
      ans6 = if_else(is.na(q19), NA_integer_, if_else(q19 == key_low_high[6], 1L, 0L)),
      ans7 = if_else(is.na(q20), NA_integer_, if_else(q20 == key_low_high[7], 1L, 0L))
    )
)

high_low_df <- cbind(
  data.frame(
    id = seq_len(nrow(results)),
    EL = "high",
    IL = "low"
  ),
  score_multiselect(results$q26, key_high_low)
)

high_high_df <- cbind(
  data.frame(
    id = seq_len(nrow(results)),
    EL = "high",
    IL = "high"
  ),
  score_multiselect(results$q32, key_high_high)
)

long_df <- dplyr::bind_rows(
  long_df,
  high_low_df,
  high_high_df
)

demo_df <- results %>%
  transmute(
    id = row_number(),
    gender = q38,
    age = q39,
    survey_duration = surveyDuration
  )

long_df <- long_df %>%
  left_join(demo_df, by = "id")


long_df <- long_df %>%
  mutate(
    n_correct = if_else(
      rowSums(!is.na(across(ans1:ans7))) == 0,
      NA_real_,
      rowSums(across(ans1:ans7), na.rm = TRUE)
    )
  )

# Блок с опросниками по всем 4 условиям
survey_df <- bind_rows(
  results %>%
    transmute(
      id = row_number(),
      EL = "low",
      IL = "low",
      IL1 = q9_r1,
      IL2 = q9_r2,
      IL3 = q9_r3,
      EL1 = q9_r4,
      EL2 = q9_r5,
      EL3 = q9_r6,
      GL1 = q9_r7,
      GL2 = q9_r8,
      GL3 = q9_r9,
      GL4 = q9_r10,
      NASA_mental_demand = q10,
      NASA_performance   = q11,
      NASA_effort        = q12,
      NASA_frustration   = q13
    ),
  
  results %>%
    transmute(
      id = row_number(),
      EL = "low",
      IL = "high",
      IL1 = q21_r1,
      IL2 = q21_r2,
      IL3 = q21_r3,
      EL1 = q21_r4,
      EL2 = q21_r5,
      EL3 = q21_r6,
      GL1 = q21_r7,
      GL2 = q21_r8,
      GL3 = q21_r9,
      GL4 = q21_r10,
      NASA_mental_demand = q22,
      NASA_performance   = q23,
      NASA_effort        = q24,
      NASA_frustration   = q25
    ),
  
  results %>%
    transmute(
      id = row_number(),
      EL = "high",
      IL = "low",
      IL1 = q27_r1,
      IL2 = q27_r2,
      IL3 = q27_r3,
      EL1 = q27_r4,
      EL2 = q27_r5,
      EL3 = q27_r6,
      GL1 = q27_r7,
      GL2 = q27_r8,
      GL3 = q27_r9,
      GL4 = q27_r10,
      NASA_mental_demand = q28,
      NASA_performance   = q29,
      NASA_effort        = q30,
      NASA_frustration   = q31
    ),
  
  results %>%
    transmute(
      id = row_number(),
      EL = "high",
      IL = "high",
      IL1 = q33_r1,
      IL2 = q33_r2,
      IL3 = q33_r3,
      EL1 = q33_r4,
      EL2 = q33_r5,
      EL3 = q33_r6,
      GL1 = q33_r7,
      GL2 = q33_r8,
      GL3 = q33_r9,
      GL4 = q33_r10,
      NASA_mental_demand = q34,
      NASA_performance   = q35,
      NASA_effort        = q36,
      NASA_frustration   = q37
    )
)

long_df <- long_df %>%
  left_join(survey_df, by = c("id", "EL", "IL"))

label_map <- list(
  IL1  = "Тема/темы в задании были очень сложными.",
  IL2 = "Материал в задании был очень сложным.",
  IL3 = "Понятия и определения в задании были для меня очень сложными.",
  #EL1 = "Указания и/или объяснения в задании были непонятными.",
  #EL2 = "Указаниях и/или объяснения в задании были бесполезными.",
  #EL3 = "В указаниях и/или объяснениях в задании были непонятные формулировки.",
  EL1 = "Формулировки заданий были непонятными.",
  EL2 = "Формулировки заданий никак не помогали обучению.",
  EL3 = "В формулировках заданий было много незнакомых или непонятных слов.",
  GL1 = "Задание углубило моё понимание темы (или тем).",
  GL2 = "Задание углубило мои знания и моё понимание геометрии.",
  GL3 = "Задание углубило моё понимание материала.",
  GL4 = "Задание углубило моё понимание использованных понятий и определений.",
  NASA_mental_demand = attr(results$q10, "label"),
  NASA_performance   = attr(results$q11, "label"),
  NASA_effort        = attr(results$q12, "label"),
  NASA_frustration   = attr(results$q13, "label")
)

for (nm in names(label_map)) {
  attr(long_df[[nm]], "label") <- label_map[[nm]]
}

head(long_df)
saveRDS(long_df, file = "/Users/dobrokhotova/Documents/GitHub/cogload/data/data2.rds")