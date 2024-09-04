# Levene tests for homogeneity of variances

library(tidyverse)
library(data.table)
library(rio)
library(car)
library(nicksshorts)
library(janitor)
library(here)
library(scales)

dat_vt = import(here("data", "cleaned_survey_post_corrections.parquet"), setclass = 'data.table')
dat_vt[, Revision_of_Q14 := str_replace_all(Revision_of_Q14, '‚Äì','-')]
dat_vt[, Revision_of_Q17 := str_replace_all(Revision_of_Q17, '‚Äì','-')]
dat_vt[, Revision_of_Q20 := str_replace_all(Revision_of_Q20, '‚Äì','-')]
dat_vt = dat_vt[Q1 != 972]
dat_vt = dat_vt[Q1 %in% Q1[Q2 == 'The third replication task']]

efdat_vt = dat_vt[!is.na(Q2)]

qrecode(efdat_vt, 'Q2', 
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
efdat_vt[, Round := factor(Round, levels = c('Task 1',
                                          'Task 1 Revision',
                                          'Task 2',
                                          'Task 2 Revision',
                                          'Task 3',
                                          'Task 3 Revision'))]
efdat_vt[Revision_of_Q6 < 0, Revision_of_Q6 := abs(Revision_of_Q6)]


# Hypotheses 3a-3e (stage comparison) ----

# Function to perform Levene test for each pair of rounds
perform_levene_test <- function(round1, round2) {
  subset_data <- subset(efdat_vt, Round %in% c(round1, round2))
  levene_test_result <- leveneTest(Revision_of_Q4 ~ Round, data = subset_data)
  return(levene_test_result)
}

# List of round pairs
round_pairs <- list(c("Task 1", "Task 1 Revision"),
                    c("Task 1 Revision", "Task 2"),
                    c("Task 2", "Task 2 Revision"),
                    c("Task 2 Revision", "Task 3"),
                    c("Task 3", "Task 3 Revision"),
                    c("Task 1", "Task 2"),
                    c("Task 2", "Task 3"),
                    c("Task 1", "Task 3"))

# Perform Levene test for each pair and print results
levene_test_results = tibble(TaskA = sapply(round_pairs, \(x) x[1]),
                                 TaskB = sapply(round_pairs, \(x) x[2]),
                                 pval = sapply(round_pairs, \(x) perform_levene_test(x[1], x[2])$`Pr(>F)`[1])) %>%
  arrange(pval)


# Hypothesis 4 (Peer review comparisons) ----

# Function to perform the comparison and print results
compare_tasks <- function(task, revision_task, title = task) {
  rev <- efdat_vt %>%
    filter(Round %in% revision_task) %>%
    select(Q1, Revision_of_Q4, Round) %>%
    mutate(Round = str_replace(Round,' Revision',''))
  
  task_data <- efdat_vt %>%
    filter(Round %in% task) %>%
    anti_join(rev, by = c("Q1","Round")) %>%
    select(Q1, Revision_of_Q4, Round) %>%
    rbind(rev) %>%
    mutate(peer_review = FALSE) %>%
    group_by(Round) %>%
    mutate(Revision_of_Q4 = Revision_of_Q4 - mean(Revision_of_Q4, na.rm = TRUE))
  
  rev <- rev %>%
    group_by(Round) %>%
    mutate(Revision_of_Q4 = Revision_of_Q4 - mean(Revision_of_Q4, na.rm = TRUE))
  
  for (r in 1:3) {
    peer_rev = import(here("data", paste0('task_', r, '_peer_review_pairs.csv'))) %>%
      filter(!(dont_send)) %>%
      filter(!is.na(pairID))
    task_data = task_data %>%
      mutate(peer_review = ifelse(Round == paste0('Task ',r) & Q1 %in% peer_rev$id2, TRUE, peer_review))
  }
  
  # Calculate variances
  var_task <- var(task_data %>% filter(!peer_review) %>% pull(Revision_of_Q4))
  var_revision <- var(task_data %>% filter(peer_review) %>% pull(Revision_of_Q4))
  
  # Print variances
  tibble(Task = title,
         `Unreviewed Variance` = number(var_task, .001),
         `Reviewed Variance` = number(var_revision, .001),
         `Levene Test p-value` = number(leveneTest(Revision_of_Q4 ~ factor(peer_review), data = task_data)$`Pr(>F)`[1], .001),
         `Revised Variance` = number(var(rev$Revision_of_Q4), .001))
}

# Perform comparisons for hypothesis 4
peer_review_levene <- compare_tasks("Task 1", "Task 1 Revision") %>%
  bind_rows(compare_tasks("Task 2", "Task 2 Revision")) %>%
  bind_rows(compare_tasks("Task 3", "Task 3 Revision")) %>%
  bind_rows(compare_tasks(c("Task 1","Task 2","Task 3"),
                          c("Task 1 Revision","Task 2 Revision","Task 3 Revision"),
                          "Pooled"))

# # Pooled version (drop within each round)
# # Combine all tasks and revisions into a single dataset
# all_tasks <- efdat_vt %>%
#   filter(Round %in% c("Task 1", "Task 2", "Task 3"))
# 
# # Exclude respondents with revisions from original tasks
# filtered_tasks <- all_tasks %>%
#   filter(!(Q1 %in% (efdat_vt %>% filter(Round == "Task 1 Revision") %>% pull(Q1))) & Round == "Task 1") %>%
#   bind_rows(all_tasks %>% filter(!(Q1 %in% (efdat_vt %>% filter(Round == "Task 2 Revision") %>% pull(Q1))) & Round == "Task 2")) %>%
#   bind_rows(all_tasks %>% filter(!(Q1 %in% (efdat_vt %>% filter(Round == "Task 3 Revision") %>% pull(Q1))) & Round == "Task 3"))
# 
# all_revisions <- efdat_vt %>%
#   filter(Round %in% c("Task 1 Revision", "Task 2 Revision", "Task 3 Revision"))
# 
# combined_data <- bind_rows(filtered_tasks, all_revisions)
# 
# # Update Round to indicate pooled groups
# combined_data <- combined_data %>%
#   mutate(Pooled_Round = ifelse(Round %in% c("Task 1", "Task 2", "Task 3"), "All Tasks", "All Revisions"))
# 
# # Calculate variances for pooled data
# var_all_tasks <- var(combined_data %>% filter(Pooled_Round == "All Tasks") %>% pull(Revision_of_Q4))
# var_all_revisions <- var(combined_data %>% filter(Pooled_Round == "All Revisions") %>% pull(Revision_of_Q4))
# 
# # Print variances
# print(paste("Variance for pooled no revision:", var_all_tasks))
# print(paste("Variance for pooled revisions:", var_all_revisions))

# # Perform two-sided Levene's test on pooled data
# levene_result_pooled <- leveneTest(Revision_of_Q4 ~ factor(Pooled_Round), data = combined_data)
# print("Two-sided Levene's test result for pooled no revision vs revisions")
# print(levene_result_pooled)


# Hypothesis 5 ("future" effects of review) ----

# Get peer review assignment by round
efdat_vt = efdat_vt %>%
  mutate(peer_review = FALSE)

for (r in 1:3) {
  peer_rev = import(here("data", paste0('task_', r, '_peer_review_pairs.csv'))) %>%
    filter(!(dont_send)) %>%
    filter(!is.na(pairID))
  efdat_vt = efdat_vt %>%
    mutate(peer_review = ifelse(Round == paste0('Task ',r) & Q1 %in% peer_rev$id2, TRUE, peer_review))
}


compare_future <- function(task_num) {
  # Review before round
  prior <- efdat_vt %>% 
    filter(Round == paste0("Task ", as.numeric(str_extract(task_num, "\\d")) - 1)) %>%
    select(Q1, peer_review)
  
  # Task data
  task_data <- efdat_vt %>%
    filter(Round == paste0("Task ", as.numeric(str_extract(task_num, "\\d")))) %>%
    select(Q1, Revision_of_Q4) %>% 
    left_join(prior, by = "Q1")
  
  # Calculate variances
  var_no_peer <- var(task_data %>% filter(peer_review == FALSE) %>% pull(Revision_of_Q4))
  var_peer <- var(task_data %>% filter(peer_review == TRUE) %>% pull(Revision_of_Q4))
  
  tibble(Round = task_num,
         var_no_peer = var_no_peer, var_peer = var_peer,
         levene_p = leveneTest(Revision_of_Q4 ~ factor(peer_review), data = task_data)$`Pr(>F)`[1])
}

levene_peer_vs_next_round = compare_future(2) %>%
  bind_rows(compare_future(3)) %>%
  arrange(Round)


# Pooled version (drop within each round)
# Review before round
prior_2 <- efdat_vt %>% 
  filter(Round == "Task 1") %>%
  select(Q1, peer_review)

# Task data
task_2 <- efdat_vt %>%
  filter(Round == "Task 2") %>%
  select(Q1, Revision_of_Q4) %>% 
  left_join(prior_2, by = "Q1")

# Review before round
prior_3 <- efdat_vt %>% 
  filter(Round == "Task 2") %>%
  select(Q1, peer_review)

# Task data
task_3 <- efdat_vt %>%
  filter(Round == "Task 3") %>%
  select(Q1, Revision_of_Q4) %>% 
  left_join(prior_3, by = "Q1")


# Combine all into a single dataset
combined_data <- rbind(task_2, task_3)

# Calculate variances for pooled data
var_all_no_peer <- var(combined_data %>% filter(peer_review == FALSE) %>% pull(Revision_of_Q4))
var_all_peer <- var(combined_data %>% filter(peer_review == TRUE) %>% pull(Revision_of_Q4))

# Perform two-sided Levene's test on pooled data
levene_result_pooled <- leveneTest(Revision_of_Q4 ~ factor(peer_review), data = combined_data)


# Sample size comparisons ----

# Sample size variables:
# sample_size = Revision_of_Q12
# sample_daca = Revision_of_Q18
# sample_control = Revision_of_Q21
# Note some of sample_daca and sample_non_daca are NA or zero; those should
# likely be dropped, although it is weird to have a control group of zero
# individuals


# Hypotheses 3a-3e (stage comparison) ----

# Function to perform Levene test for each pair of rounds and each variable
perform_levene_test_sample <- function(round1, round2, variable) {
  subset_data <- subset(efdat_vt, Round %in% c(round1, round2))
  formula <- as.formula(paste(variable, "~ Round"))
  levene_test_result <- leveneTest(formula, data = subset_data)
  return(levene_test_result)
}

# List of round pairs
round_pairs <- list(c("Task 1", "Task 1 Revision"),
                    c("Task 1 Revision", "Task 2"),
                    c("Task 2", "Task 2 Revision"),
                    c("Task 1", "Task 2")
)

# List of variables
variables <- c("Revision_of_Q12", "Revision_of_Q18", "Revision_of_Q21")

# Perform Levene test for each pair of rounds and each variable, then combine results into a single data frame
levene_test_results_sample <- do.call(rbind, lapply(variables, function(variable) {
  tibble(Variable = variable,
         TaskA = sapply(round_pairs, \(x) x[1]),
         TaskB = sapply(round_pairs, \(x) x[2]),
         pval = sapply(round_pairs, \(x) perform_levene_test_sample(x[1], x[2], variable)$`Pr(>F)`[1]))
}))

# Arrange results by p-value
levene_test_results_sample <- levene_test_results %>% arrange(pval)

# View results
levene_test_results_sample

