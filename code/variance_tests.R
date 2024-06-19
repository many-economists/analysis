# Levene tests for homogeneity of variances

library(tidyverse)
library(data.table)
library(rio)
library(car)
library(here)
library(nicksshorts)

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

export(efdat, here("data", "test.csv"))

# Function to perform Levene test for each pair of rounds
perform_levene_test <- function(round1, round2) {
  subset_data <- subset(efdat, Round %in% c(round1, round2))
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
for (pair in round_pairs) {
  cat("Levene test for", pair[1], "vs", pair[2], ":\n")
  print(perform_levene_test(pair[1], pair[2]))
  cat("\n")
}

