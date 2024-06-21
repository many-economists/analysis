# Researcher characteristics and effect sizes
# Organizer B analyses

library(tidyverse)
library(data.table)
library(rio)
library(car)
library(here)
library(nicksshorts)
library(janitor)
library(forcats)
library(broom)
library(knitr)
library(modelsummary)


efdat_rb = import(here("data", "cleaned_survey_post_corrections.parquet"), setclass = 'data.table')
efdat_rb[, Revision_of_Q14 := str_replace_all(Revision_of_Q14, '‚Äì','-')]
efdat_rb[, Revision_of_Q17 := str_replace_all(Revision_of_Q17, '‚Äì','-')]
efdat_rb[, Revision_of_Q20 := str_replace_all(Revision_of_Q20, '‚Äì','-')]
efdat_rb = efdat_rb[Q1 != 972]
efdat_rb = efdat_rb[!is.na(Q2)]
efdat_rb = efdat_rb[Q1 %in% efdat_rb[Q2 == 'The third replication task', Q1]]

qrecode(efdat_rb, 'Q2', 
        c('Revision following the first replication task (such as following peer review)',
          'Revision following the second replication task (such as following peer review)',
          'Revision following the third replication task (such as following peer review)',
          'The first replication task',
          'The second replication task',
          'The third replication task'),
        c('Task 1 Revision',
          'Task 2 Revision',
          'Task 3 Revision',
          'Task 1',
          'Task 2',
          'Task 3'), 'Round', checkfrom = TRUE)
efdat_rb[, Round := factor(Round, levels = c('Task 1',
                                          'Task 1 Revision',
                                          'Task 2',
                                          'Task 2 Revision',
                                          'Task 3',
                                          'Task 3 Revision'))]
efdat_rb[Revision_of_Q6 < 0, Revision_of_Q6 := abs(Revision_of_Q6)]


# Data restriction and load in payment status ----

# Load in payment status -- the order column indicates payment order. Values of
# 200 or less are guaranteed payment. Merges on respondent_id = Q1
payment_status <- import(here("data","email_order.xlsx")) %>% 
  mutate(
    paid_guaranteed = case_when(
      order <= 200 ~ TRUE,
      order > 200 ~ FALSE,
      TRUE ~ NA
    )
  ) %>% 
  rename(
    Q1 = respondent_id
  )


df <- efdat_rb %>% 
  # Look only at main stages
  filter(Round %in% c('Task 1', 'Task 2', 'Task 3')) %>% 
  # Merge in payment status
  left_join(payment_status, by = 'Q1') %>% 
  # Rename variables 
  rename(
    position = Researcher_Q6,
    degree = Researcher_Q10,
    experience = Researcher_Q8
  ) %>% 
  # Recode factors
  mutate(
    position = fct_recode(
      position,
      "Faculty" = "Faculty",
      "Grad student" = "Graduate student",
      "Uni researcher" = "Non-faculty researcher at a university",
      "Other" = "Other (describe)",
      "Private researcher" = "Private-sector researcher",
      "Public researcher" = "Public-sector researcher not at a university"
    ),
    degree = fct_recode(
      degree,
      "Not PhD" = "Master's degree",
      "PhD" = "PhD",
      "Not PhD" = "Some graduate school, but no graduate degree"
    ),
    degree = fct_relevel(degree, "PhD"),
    experience = fct_recode(
      experience,
      "1-5 papers" = "I have 1-5 papers in applied microeconomics published or accepted for publication",
      "6+ papers" = "I have 6+ papers in applied microeconomics published or accepted for publication",
      "0 papers" = "I have never performed academic research in applied microeconomics",
      "0 papers" = "I have written at least one academic paper in applied microeconomics, but none of this work is published or accepted for publication"
    ),
    experience = fct_relevel(experience, "6+ papers", "1-5 papers", "0 papers")
  ) %>% 
  select(Q1, Round, position, degree, experience, Revision_of_Q4, paid_guaranteed)



# Estimate researcher effects ----

# simple version
# lm(Revision_of_Q4 ~ factor(Round) + factor(position) + factor(degree) + factor(experience), data = df) %>% 
#   summary() 

# Calculate change and fit linear model
estimate_change <- function(df, task1, task2) {
  df %>%
    filter(Round %in% c(task1, task2)) %>%
    group_by(Q1) %>%
    mutate(change = abs(Revision_of_Q4 - lag(Revision_of_Q4))) %>%
    ungroup() %>%
    drop_na(change) %>%
    lm(change ~ factor(position) + factor(degree) + factor(experience), data = .)
}

# Calculate absolute range and fit linear model
estimate_range <- function(df) {
  range_df <- df %>%
    group_by(Q1) %>%
    summarise(abs_range = abs(max(Revision_of_Q4) - min(Revision_of_Q4)), 
              position = first(position), 
              degree = first(degree), 
              experience = first(experience)) %>%
    ungroup()
  lm(abs_range ~ factor(position) + factor(degree) + factor(experience), data = range_df)
}

# Model runs
model_task1_vs_task2 <- estimate_change(df, 'Task 1', 'Task 2')
model_task2_vs_task3 <- estimate_change(df, 'Task 2', 'Task 3')
model_task1_vs_task3 <- estimate_change(df, 'Task 1', 'Task 3')
model_range <- estimate_range(df)

researcher_characteristics_models_b <- list(
  "Task 1 vs Task 2" = model_task1_vs_task2,
  "Task 2 vs Task 3" = model_task2_vs_task3,
  "Task 1 vs Task 3" = model_task1_vs_task3,
  "Absolute Range" = model_range
)

# Model table

