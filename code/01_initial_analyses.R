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
library(data.table)
library(nicksshorts)

# Load  and clean data ----
researcher_variable_key <- read_csv(here("raw_data", "researcher_variable_key.csv"))
survey_variable_key <- read_csv(here("raw_data", "survey_variable_key.csv"))
survey <- import(here("raw_data", "cleaned_survey.parquet")) 


# Clean data
test <- survey %>% 
  filter(Q1 != 0) %>%
  filter(!is.na(Q2))

test <- as.data.table(test)

dat = merge(test, test[Q2 == 'The third replication task', .(Q1)], by = 'Q1')
qrecode(test, 'Q2', 
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
test[, Round := factor(Round, levels = c('Task 1',
                                        'Task 1 Revision',
                                        'Task 2',
                                        'Task 2 Revision',
                                        'Task 3',
                                        'Task 3 Revision'))]


# Analytical choices ----

# Show distribution of e.g. logit/linear, standard error adjustments across stages



sumtable(alldemog, vars = 'Researcher_Q6', group = 'Round')


