# Levene tests for homogeneity of variances

library(tidyverse)
library(data.table)
library(rio)
library(car)
library(here)
library(nicksshorts)
library(janitor)

dat = import(here("data", "cleaned_survey_post_corrections.parquet"), setclass = 'data.table')
dat[, Revision_of_Q14 := str_replace_all(Revision_of_Q14, '‚Äì','-')]
dat[, Revision_of_Q17 := str_replace_all(Revision_of_Q17, '‚Äì','-')]
dat[, Revision_of_Q20 := str_replace_all(Revision_of_Q20, '‚Äì','-')]

efdat = dat[!is.na(Q2)]
efdat = efdat[Q1 %in% efdat[Q2 == 'The third replication task', Q1]]

qrecode(efdat, 'Q2', 
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
efdat[, Round := factor(Round, levels = c('Task 1',
                                          'Task 1 Revision',
                                          'Task 2',
                                          'Task 2 Revision',
                                          'Task 3',
                                          'Task 3 Revision'))]
efdat[Revision_of_Q6 < 0, Revision_of_Q6 := abs(Revision_of_Q6)]

# export(efdat, here("data", "test.csv"))

# Function to perform Levene test for each pair of rounds
perform_levene_test <- function(round1, round2) {
  subset_data <- subset(efdat, Round %in% c(round1, round2))
  levene_test_result <- leveneTest(Revision_of_Q4 ~ Round, data = subset_data)
  return(levene_test_result)
}


# Hypotheses 3a-3e (stage comparison) ----

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
for (pair in round_pairs) {
  cat("Levene test for", pair[1], "vs", pair[2], ":\n")
  print(perform_levene_test(pair[1], pair[2]))
  cat("\n")
}


# Hypothesis 4 (Peer review comparisons) ----

# Function to perform the comparison and print results
compare_tasks <- function(task, revision_task) {
  rev <- efdat %>%
    filter(Round == revision_task) %>%
    select(Q1, Revision_of_Q4, Round)
  
  task_data <- efdat %>%
    filter(Round == task) %>%
    anti_join(rev, by = "Q1") %>%
    select(Q1, Revision_of_Q4, Round) %>%
    rbind(rev)
  
  # Calculate variances
  var_task <- var(task_data %>% filter(Round == task) %>% pull(Revision_of_Q4))
  var_revision <- var(task_data %>% filter(Round == revision_task) %>% pull(Revision_of_Q4))
  
  # Print variances
  print(paste("Variance for", task, ":", var_task))
  print(paste("Variance for", revision_task, ":", var_revision))
  
  print(paste("Comparing", task, "vs", revision_task))
  print(leveneTest(Revision_of_Q4 ~ Round, data = task_data))
}

# Perform comparisons for hypothesis 4
compare_tasks("Task 1", "Task 1 Revision")
compare_tasks("Task 2", "Task 2 Revision")
compare_tasks("Task 3", "Task 3 Revision")

# Pooled version (drop within each round)
# Combine all tasks and revisions into a single dataset
all_tasks <- efdat %>%
  filter(Round %in% c("Task 1", "Task 2", "Task 3"))

# Exclude respondents with revisions from original tasks
filtered_tasks <- all_tasks %>%
  filter(!(Q1 %in% (efdat %>% filter(Round == "Task 1 Revision") %>% pull(Q1))) & Round == "Task 1") %>%
  bind_rows(all_tasks %>% filter(!(Q1 %in% (efdat %>% filter(Round == "Task 2 Revision") %>% pull(Q1))) & Round == "Task 2")) %>%
  bind_rows(all_tasks %>% filter(!(Q1 %in% (efdat %>% filter(Round == "Task 3 Revision") %>% pull(Q1))) & Round == "Task 3"))

all_revisions <- efdat %>%
  filter(Round %in% c("Task 1 Revision", "Task 2 Revision", "Task 3 Revision"))

combined_data <- bind_rows(filtered_tasks, all_revisions)

# Update Round to indicate pooled groups
combined_data <- combined_data %>%
  mutate(Pooled_Round = ifelse(Round %in% c("Task 1", "Task 2", "Task 3"), "All Tasks", "All Revisions"))

# Calculate variances for pooled data
var_all_tasks <- var(combined_data %>% filter(Pooled_Round == "All Tasks") %>% pull(Revision_of_Q4))
var_all_revisions <- var(combined_data %>% filter(Pooled_Round == "All Revisions") %>% pull(Revision_of_Q4))

# Print variances
print(paste("Variance for pooled no revision:", var_all_tasks))
print(paste("Variance for pooled revisions:", var_all_revisions))

# Perform two-sided Levene's test on pooled data
levene_result_pooled <- leveneTest(Revision_of_Q4 ~ factor(Pooled_Round), data = combined_data)
print("Two-sided Levene's test result for pooled no revision vs revisions")
print(levene_result_pooled)


# Hypothesis 5 ("future" effects of review) ----

# Get peer review assignment by round
efdat = efdat %>%
  mutate(peer_review = FALSE)

for (r in 1:3) {
  peer_rev = import(here("data", paste0('task_', r, '_peer_review_pairs.csv'))) %>%
    filter(!(dont_send)) %>%
    filter(!is.na(pairID))
  efdat = efdat %>%
    mutate(peer_review = ifelse(Round == paste0('Task ',r) & Q1 %in% peer_rev$id2, TRUE, peer_review))
}


compare_future <- function(task_num) {
  # Review before round
  prior <- efdat %>% 
    filter(Round == paste0("Task ", as.numeric(str_extract(task_num, "\\d")) - 1)) %>%
    select(Q1, peer_review)
  
  # Task data
  task_data <- efdat %>%
    filter(Round == paste0("Task ", as.numeric(str_extract(task_num, "\\d")))) %>%
    select(Q1, Revision_of_Q4) %>% 
    left_join(prior, by = "Q1")
  
  # Calculate variances
  var_no_peer <- var(task_data %>% filter(peer_review == FALSE) %>% pull(Revision_of_Q4))
  var_peer <- var(task_data %>% filter(peer_review == TRUE) %>% pull(Revision_of_Q4))
  
  # Print variances
  print(paste("Variance in", task_num, "if not in peer reivew:", var_no_peer))
  print(paste("Variance in", task_num, "if in peer review:", var_peer))
  
  print(paste("Comparing", task_num, "with and without peer review"))
  print(leveneTest(Revision_of_Q4 ~ factor(peer_review), data = task_data))
}

compare_future(2)
compare_future(3)


# Pooled version (drop within each round)


# Review before round
prior_2 <- efdat %>% 
  filter(Round == "Task 1") %>%
  select(Q1, peer_review)

# Task data
task_2 <- efdat %>%
  filter(Round == "Task 2") %>%
  select(Q1, Revision_of_Q4) %>% 
  left_join(prior_2, by = "Q1")

# Review before round
prior_3 <- efdat %>% 
  filter(Round == "Task 2") %>%
  select(Q1, peer_review)

# Task data
task_3 <- efdat %>%
  filter(Round == "Task 3") %>%
  select(Q1, Revision_of_Q4) %>% 
  left_join(prior_3, by = "Q1")


# Combine all into a single dataset
combined_data <- rbind(task_2, task_3)

# Calculate variances for pooled data
var_all_no_peer <- var(combined_data %>% filter(peer_review == FALSE) %>% pull(Revision_of_Q4))
var_all_peer <- var(combined_data %>% filter(peer_review == TRUE) %>% pull(Revision_of_Q4))

# Print variances
print(paste("Variance for not in peer review:", var_all_no_peer))
print(paste("Variance for in peer review:", var_all_peer))

# Perform two-sided Levene's test on pooled data
levene_result_pooled <- leveneTest(Revision_of_Q4 ~ factor(peer_review), data = combined_data)
print("Two-sided Levene's test result for pooled no peer review vs peer review")
print(levene_result_pooled)




