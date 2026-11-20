library(tidyverse)

prep_data <- function() {
  student_info <- read_csv("data/studentInfo.csv")
  student_info <- student_info %>% select(c(-10, -11)) %>%
    mutate(
      code_module = as.factor(code_module),
      code_presentation = as.factor(code_presentation),
      gender = as.factor(gender),
      region = as.factor(region),
      highest_education = as.factor(highest_education),
      imd_band = as.character(imd_band),
      imd_band = na_if(imd_band, "?"),
      imd_band = recode(imd_band, "10-20" = "10-20%"),
      imd_band = as.factor(imd_band),
      age_band = as.factor(age_band),
      final_result = as.factor(final_result)
  )
  # glimpse(student_info)
  
  student_vle <- read_csv("data/studentVle.csv")
  student_vle <- student_vle %>% select(c(-4, -5)) %>%
    mutate(
      code_module = as.factor(code_module),
      code_presentation = as.factor(code_presentation)
  )
  student_vle <- student_vle %>%
    group_by(id_student, code_module, code_presentation) %>%
    summarise(total_clicks = sum(sum_click, na.rm = TRUE), .groups = 'drop')
  # glimpse(student_vle)
  
  assessments <- read_csv("data/assessments.csv")
  assessments <- assessments %>% select(c(-4, -5, -6)) %>%
    mutate(
      code_module = as.factor(code_module),
      code_presentation = as.factor(code_presentation)
  )
  # glimpse(assessments)
  
  student_assessment <- read_csv("data/studentAssessment.csv")
  student_assessment <- student_assessment %>% select(c(-3, -4)) %>%
    mutate(
      score = as.numeric(score)
    )
  # glimpse(student_assessment)
  
  student_assessment <- assessments %>% left_join(student_assessment, by = "id_assessment")
  student_assessment <- student_assessment %>% select(-3)
  student_assessment <- student_assessment %>%
    group_by(code_module, code_presentation, id_student) %>%
    summarize(avg_grade = mean(score, na.rm = TRUE), .groups = 'drop')
  # glimpse(student_assessment)
  
  oulad = student_info %>% 
    left_join(student_vle, by = c("code_module", "code_presentation", "id_student")) %>%
    left_join(student_assessment, by = c("code_module", "code_presentation", "id_student"))
  
  return(oulad)
}

# oulad <- prep_data()