# Initial analyses for WWU presentation on 1 November

# Claus:
# Analytic Choices
# Show distribution of e.g. logit/linear, standard error adjustments across stages
# 
# Researcher Characteristics
# Calculate absolute difference from mean of effect size and also sample size. Regress abs difference on different researcher characteristics one at a time


# Attached:
#
# A cleaned_survey.parquet data file (import with rio::import(), you’ll have to
# install the arrow package as well). All identifying variables have been
# removed.
#
# Notes: Q1 is the researcher ID. A Q1 of 0 means that someone filled out the
# sign-up survey and we have researcher variables for them, but they never even
# got as far as being allowed into the study and given a researcher ID (these
# are at the top of the data which is why the data looks like all-NAs when you
# first open it). A Q1 of not-zero but where everything is missing except the
# researcher variables means someone was allowed into the study but never
# submitted Task 1. is.na(Q2) can be used to find these – every Q1 value for
# which Q2 is missing is a replicator who never submitted their Task 1.
#
# All “Researcher_” variables are researcher characteristics from the original
# signup survey. 
#
# All “Revision_” variables are cleaned-up versions of the
# submitted survey questions like “what is your effect size”. Note that the
# variables about the list of controls and the description of sample
# restrictions are not completed, and also remain pretty messy, and are not
# going to be used in this presentation. 
#
# All “Recoded_” variables are simplified recodes of the Revision_ variables,
# for example grouping together estimator responses of “logit” and “probit” into
# “logit/probit”

# Two CSV files that describe the variable labels for the replication-work
# responses, and the researcher-characteristic responses (note the variable names
# all have “Researcher_” appended to the front of them relative to what’s noted
# in this file).


# Load libraries
library(tidyverse)
library(here)
library(rio)
library(arrow)
library(vtable)
library(janitor)
library(nicksshorts)
library(modelsummary)

# Rename column name function ----
rename_to_lower_snake <- function(df) {
  df %>% 
    rename_with( ~gsub("([a-z])([A-Z])", "\\1_\\2", .x) ) %>%  # Adds _ to camel case var names
    rename_with( ~tolower(gsub("[ ]+", "_", .x)) )  # Converts to lower and substitutes _ for spaces
}


# Load  and clean data ----
researcher_variable_key <- read_csv(here("raw_data", "researcher_variable_key.csv"))
survey_variable_key <- read_csv(here("raw_data", "survey_variable_key.csv"))
survey <- import(here("raw_data", "cleaned_survey.parquet")) 


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
  select(researcher_id, round, method, se_adjustment, effect_size, everything()) 

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
    method = case_when(
      is.na(method) ~ 'Other/NA',
      method == "Other" ~ 'Other/NA',
      TRUE ~ method 
    ),
    se_adjustment = case_when(
      se_adjustment %in% c("Bootstrap", "None", "Other", "New DID Estimator") ~ 'Other/Bootstrap/NA',
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

sumtable(base, vars = 'method', group = 'round')
  
sumtable(base, vars = 'se_adjustment', group = 'round')


# Researcher Characteristics ----

# Calculate absolute difference from mean of effect size and also sample size. Regress abs difference on different researcher characteristics one at a time

lm_effect_degree <- lm(effect_abs_diff ~ highest_degree, data = base)
summary(lm_effect_degree)

lm_sample_degree <- lm(sample_size_abs_diff ~ highest_degree, data = base)
summary(lm_sample_degree)

lm_effect_position <- lm(effect_abs_diff ~ position, data = base)
summary(lm_effect_position)

lm_sample_position <- lm(sample_size_abs_diff ~ position, data = base)
summary(lm_sample_position)

models_degree <- list("Effect Size" = lm_effect_degree, 
                      "Sample Size" = lm_sample_degree)

models_position <- list("Effect Size" = lm_effect_position, 
                        "Sample Size" = lm_sample_position)

f <- function(x) formatC(x, digits = 2, big.mark = ",", format = "f")

modelsummary(models_degree, stars = TRUE, statistic = "std.error", 
             title = "Effect of Highest Degree on Absolute Deviation from Mean Effect Size and Mean Sample Size",
             coef_rename = c("highest_degreeNot Ph.D" = "Not Ph.D"),
             fmt = f, gof_map = NA)

modelsummary(models_position, stars = TRUE, statistic = "std.error",
             title = "Effect of Position on Absolute Deviation from Mean Effect Size and Mean Sample Size",
             coef_rename = c("positionUniversity faculty" = "University faculty",
                             "positionGraduate student" = "Graduate student",
                             "positionOther researcher" = "Other researcher",
                             "positionOther/NA" = "Other/NA"),
             fmt = f, gof_map = NA)


