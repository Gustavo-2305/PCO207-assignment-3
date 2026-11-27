# ==============================================================================
# PREPARAÇÃO E INTEGRAÇÃO DOS DADOS - DATASET OULAD
# ==============================================================================

library(tidyverse)

prep_data <- function(data_dir = "data") {
  na_padrao <- c("", "NA", "?")
  
  # 1. Informações demográficas e acadêmicas dos estudantes --------------------
  student_info <- read_csv(
    file.path(data_dir, "studentInfo.csv"),
    na = na_padrao,
    show_col_types = FALSE
  ) %>%
    select(-studied_credits, -disability) %>%
    mutate(
      # Corrige o typo conhecido da base OULAD ("10-20" -> "10-20%")
      imd_band = if_else(imd_band == "10-20", "10-20%", imd_band),
      imd_band = factor(
        imd_band,
        levels = c(
          "0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
          "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"
        )
      ),
      highest_education = factor(
        highest_education,
        levels = c(
          "No Formal quals", "Lower Than A Level", "A Level or Equivalent",
          "HE Qualification", "Post Graduate Qualification"
        )
      ),
      age_band = factor(age_band, levels = c("0-35", "35-55", "55<=")),
      final_result = factor(
        final_result,
        levels = c("Withdrawn", "Fail", "Pass", "Distinction")
      ),
      across(c(gender, region), as.factor),
      num_of_prev_attempts = as.numeric(num_of_prev_attempts)
    )
  
  # 2. Engajamento no ambiente virtual (VLE) -----------------------------------
  # Carrega apenas as colunas necessárias para reduzir uso de memória (>10M linhas)
  student_vle <- read_csv(
    file.path(data_dir, "studentVle.csv"),
    col_select = c(id_student, code_module, code_presentation, sum_click),
    na = na_padrao,
    show_col_types = FALSE
  ) %>%
    group_by(id_student, code_module, code_presentation) %>%
    summarise(
      total_clicks = sum(sum_click, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 3. Avaliações e desempenho acadêmico ---------------------------------------
  assessments <- read_csv(
    file.path(data_dir, "assessments.csv"),
    col_select = c(id_assessment, code_module, code_presentation, date, weight),
    na = na_padrao,
    show_col_types = FALSE
  ) %>%
    mutate(
      date = as.numeric(date),
      weight = as.numeric(weight)
    )
  
  student_assessment <- read_csv(
    file.path(data_dir, "studentAssessment.csv"),
    col_select = c(id_assessment, id_student, date_submitted, score),
    na = na_padrao,
    show_col_types = FALSE
  ) %>%
    mutate(
      score = as.numeric(score),
      date_submitted = as.numeric(date_submitted)
    )
  
  student_assessment_summary <- assessments %>%
    inner_join(student_assessment, by = "id_assessment") %>%
    group_by(code_module, code_presentation, id_student) %>%
    summarise(
      n_assessments = sum(!is.na(score)),
      avg_grade = if (n_assessments > 0) mean(score, na.rm = TRUE) else NA_real_,
      weighted_grade = {
        validos <- !is.na(score) & !is.na(weight)
        soma_pesos <- sum(weight[validos])
        if (soma_pesos > 0) {
          sum(score[validos] * weight[validos]) / soma_pesos
        } else {
          avg_grade
        }
      },
      avg_days_late = {
        atraso <- date_submitted - date
        if (all(is.na(atraso))) NA_real_ else mean(atraso, na.rm = TRUE)
      },
      .groups = "drop"
    )
  
  # 4. Base consolidada --------------------------------------------------------
  # Mantém chaves como character durante os joins e converte para factor ao final
  oulad <- student_info %>%
    left_join(student_vle, by = c("code_module", "code_presentation", "id_student")) %>%
    left_join(student_assessment_summary, by = c("code_module", "code_presentation", "id_student")) %>%
    mutate(
      across(c(code_module, code_presentation), as.factor)
    )
  
  oulad
}