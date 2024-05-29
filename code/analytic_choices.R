library(tidyverse)

# Rename column name function ----
rename_to_lower_snake <- function(df) {
  df %>% 
    rename_with( ~gsub("([a-z])([A-Z])", "\\1_\\2", .x) ) %>%  # Adds _ to camel case var names
    rename_with( ~tolower(gsub("[ ]+", "_", .x)) )  # Converts to lower and substitutes _ for spaces
}


# Load  and clean data ----
researcher_variable_key <- read_csv("../data/researcher_variable_key.csv")
survey_variable_key <- read_csv("../data/survey_variable_key.csv")
survey <- import("../data/cleaned_survey_post_corrections.parquet")


# Clean data
base <- survey %>% 
  rename_to_lower_snake() %>%
  # Remove researchers who did not complete the first replication task
  filter(q1 != 0) %>%
  filter(!is.na(q2)) %>% 
  # Better variable names
  rename(
    researcher_id = q1,
    round = q2,
    method = recoded_q10,
    se_adjustment = recoded_q8,
    effect_size = revision_of_q4,
    sample_size = revision_of_q12
  ) %>% 
  # Keep only researchers who have completed the third replication task
  group_by(researcher_id) %>%
  filter(any(round == "The third replication task")) %>%
  ungroup() %>%
  # General clean-up/reorganize
  dplyr::select(researcher_id, round, method, se_adjustment, effect_size, everything()) 

# Better labels for variables
base <- base %>% 
  mutate(
    round = case_when(
      round == 'Revision following the first replication task (such as following peer review)' ~ 'Task 1 Revision',
      round == 'Revision following the second replication task (such as following peer review)' ~ 'Task 2 Revision',
      round == 'Revision following the third replication task (such as following peer review)' ~ 'Task 3 Revision',
      round == 'The first replication task' ~ 'Task 1',
      round == 'The second replication task' ~ 'Task 2',
      round == 'The third replication task' ~ 'Task 3'
    ),
    se_adjustment = case_when(
      se_adjustment %in% c("Bootstrap", "Other", "New DID Estimator") ~ 'Other/Bootstrap',
      se_adjustment %in% c("Cluster: ID", "Cluster: Other", "Cluster: Strata") ~ "Cluster (ID/Strata/Other)",
      se_adjustment %in% c("Cluster: State") ~ "Cluster (State)",
      se_adjustment %in% c("Cluster: State/Yr") ~ "Cluster (State & Year)",
      TRUE ~ se_adjustment 
    ),
    # Absolute differences
    effect_abs_diff = abs(effect_size - mean(effect_size, na.rm = TRUE)),
    sample_size_abs_diff = abs(sample_size - mean(sample_size, na.rm = TRUE)),
    # Researcher characteristics
    highest_degree = factor(
      case_when(
        researcher_q10 == "PhD" ~ "Ph.D.",
        TRUE ~ "Not Ph.D",
      ), 
      levels = c("Ph.D.", "Not Ph.D")
    ),
    position = factor(
      case_when(
        researcher_q6 == "Faculty" ~ "University faculty",
        researcher_q6 == "Graduate student" ~ "Graduate student",
        researcher_q6 == "Other (describe)" ~ "Other/NA",
        is.na(researcher_q6) ~ "Other/NA",
        TRUE ~ "Other researcher"
      ),
      levels = c("University faculty", "Graduate student", "Other researcher", "Other/NA")
    )
  )





# Analytical choices ----

# Show distribution of e.g. logit/linear, standard error adjustments across stages
# Because of some confusion on where to report that one used sample weights, report weights as being used if the word "weight" appears in any column
charvars = names(base)[sapply(base, is.character)]
setDT(base)
checklist = c()
base[, Weights := 'No Sample Weights']
for (cv in charvars) {
  checklist = c(checklist, base[base[[cv]] %ilike% 'weight'][[cv]])
  base[base[[cv]] %ilike% 'sampling weight' |
         base[[cv]] %ilike% 'sample weight' |
         base[[cv]] %ilike% 'survey weight' |
         base[[cv]] %ilike% 'perwt' |
         base[[cv]] %ilike% 'hhwt' |
         (base[[cv]] %ilike% 'weight' & 
            !(base[[cv]] %ilike% 'inverse probability') &
            !(base[[cv]] %ilike% 'propensity score matching/weighted') &
            !(base[[cv]] %ilike% 'kernel weights') &
            !(base[[cv]] %ilike% 'weighted by the number of individuals')), Weights := 'Sample Weights']
}