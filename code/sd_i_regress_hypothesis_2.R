# Hypothesis 2: SD_i will have a negative linear or quadratic relationship with i

library(tidyverse)
library(broom)
library(data.table)
library(rio)
library(car)
library(nicksshorts)
library(janitor)
library(here)
library(scales)

dat_reg = import(here("data", "cleaned_survey_post_corrections.parquet"), setclass = 'data.table')
dat_reg[, Revision_of_Q14 := str_replace_all(Revision_of_Q14, '‚Äì','-')]
dat_reg[, Revision_of_Q17 := str_replace_all(Revision_of_Q17, '‚Äì','-')]
dat_reg[, Revision_of_Q20 := str_replace_all(Revision_of_Q20, '‚Äì','-')]
dat_reg = dat_reg[Q1 != 972]
dat_reg = dat_reg[Q1 %in% Q1[Q2 == 'The third replication task']]

# Revisions of sample sizes
dat_reg[Q1 == 15 & Q2 == 'The first replication task', Revision_of_Q12 := 5979569]
dat_reg[Q1 == 834 & Q2 == 'The first replication task', Revision_of_Q12 := 2924560]
dat_reg[Q1 == 176 & Q2 == 'The first replication task', Revision_of_Q12 := 284230]
dat_reg[Q1 == 871 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q12 := 20798]
dat_reg[Q1 == 737 & Q2 == 'The second replication task', Revision_of_Q12 := 36762]
dat_reg[Q1 == 737 & Q2 == 'The second replication task', Revision_of_Q18 := 22774]
dat_reg[Q1 == 737 & Q2 == 'The second replication task', Revision_of_Q21 := 13988]
dat_reg[Q1 == 737 & Q2 == 'The first replication task', Revision_of_Q12 := 738057]
dat_reg[Q1 == 737 & Q2 == 'The first replication task', Revision_of_Q18 := 179856]
dat_reg[Q1 == 737 & Q2 == 'The first replication task', Revision_of_Q21 := 558201]
dat_reg[Q1 == 737 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q12 := 248351]
dat_reg[Q1 == 737 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q18 := 110830]
dat_reg[Q1 == 737 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q21 := 137521]
dat_reg[Q1 == 158 & Q2 == "The second replication task", Revision_of_Q18 := 17432]
dat_reg[Q1 == 395 & Q2 == "The second replication task", Revision_of_Q18 := 2834]
dat_reg[Q1 == 591 & Q2 == "The second replication task", Revision_of_Q12 := 28863]
dat_reg[Q1 == 591 & Q2 == "The second replication task", Revision_of_Q18 := 5664]
dat_reg[Q1 == 591 & Q2 == "The second replication task", Revision_of_Q21 := 23199]
dat_reg[Q1 == 923 & Q2 == 'The first replication task', Revision_of_Q12 := 313046]
dat_reg[Q1 == 923 & Q2 == 'The first replication task', Revision_of_Q21 := 221167]
dat_reg[Q1 == 842 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q12 := 3283605]
dat_reg[Q1 == 842 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q21 := 3196015]


dat_reg = dat_reg[!is.na(Q2)]

qrecode(dat_reg, 'Q2', 
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
dat_reg[, Round := factor(Round, levels = c('Task 1',
                                             'Task 1 Revision',
                                             'Task 2',
                                             'Task 2 Revision',
                                             'Task 3',
                                             'Task 3 Revision'))]

# Easier variable numbers

dat_reg <- dat_reg %>% 
  mutate(
    effect_size = Revision_of_Q4,
    sample_size = Revision_of_Q12,
    sample_daca = Revision_of_Q18, # Treatment
    sample_non_daca = Revision_of_Q21, # Control
    round_number = case_when(
      Round == 'Task 1' ~ 1,
      Round == 'Task 1 Revision' ~ 2,
      Round == 'Task 2' ~ 3,
      Round == 'Task 2 Revision' ~ 4,
      Round == 'Task 3' ~ 5,
      Round == 'Task 3 Revision' ~ 6
      )
  )
  
#  Squared difference from round mean for each variable
dat_reg <- dat_reg %>% 
  group_by(round_number) %>% 
  mutate(
    effect_size_diff = (effect_size - mean(effect_size, na.rm = TRUE))^2,
    sample_size_diff = (sample_size - mean(sample_size, na.rm = TRUE))^2,
    sample_daca_diff = (sample_daca - mean(sample_daca, na.rm = TRUE))^2,
    sample_non_daca_diff = (sample_non_daca - mean(sample_non_daca, na.rm = TRUE))^2
  )

# Regress each variable in map_vars on round_number ----

map_vars <- c('effect_size_diff', 'sample_size_diff', 'sample_daca_diff', 'sample_non_daca_diff')

models_linear <- map(map_vars, 
              ~ {
                # Drop two last rounds for sample variables because we prescribe sample
                .df <- dat_reg
                if(.x %in% c('sample_size_diff', 'sample_daca_diff', 'sample_non_daca_diff')) {
                  .df <- dat_reg %>% 
                    filter(round_number < 5)
                }
                # Convert string to symbol and evaluate in lm formula
                formula <- as.formula(paste(.x, "~ round_number"))
                lm(formula, data = .df)
              }) %>% 
  # Extract coefficients and p-values
  map(broom::tidy)

# View the results
models_linear

# The quadratic model show nothing statistically significant or even close to it
models_quadratic <- map(map_vars, 
                     ~ {
                       # Drop two last rounds for sample variables because we prescribe sample
                       .df <- dat_reg
                       if(.x %in% c('sample_size_diff', 'sample_daca_diff', 'sample_non_daca_diff')) {
                         .df <- dat_reg %>% 
                           filter(round_number < 5)
                       }
                       # Convert string to symbol and evaluate in lm formula
                       formula <- as.formula(paste(.x, "~ round_number + I(round_number^2)"))
                       lm(formula, data = .df)
                     }) %>% 
  # Extract coefficients and p-values
  map(broom::tidy)

# View the results
models_quadratic







