# Load necessary libraries
library(rio)
library(data.table)
library(ggplot2)
library(nicksshorts) # remotes::install_github('NickCH-K/nicksshorts')
library(stringr)
library(scales)
library(vtable)
library(fixest)
library(modelsummary)
library(here)
library(patchwork)

colorpal = palette.colors(palette = 'Paired')

# Import data
dat_prss = import(here("data", "cleaned_survey_post_corrections.parquet"), setclass = 'data.table')

# Replace specific characters in the Revision columns
dat_prss[, Revision_of_Q14 := str_replace_all(Revision_of_Q14, '‚Äì','-')]
dat_prss[, Revision_of_Q17 := str_replace_all(Revision_of_Q17, '‚Äì','-')]
dat_prss[, Revision_of_Q20 := str_replace_all(Revision_of_Q20, '‚Äì','-')]

# Revisions of sample sizes
dat_prss[Q1 == 15 & Q2 == 'The first replication task', Revision_of_Q12 := 5979569]
dat_prss[Q1 == 834 & Q2 == 'The first replication task', Revision_of_Q12 := 2924560]
dat_prss[Q1 == 176 & Q2 == 'The first replication task', Revision_of_Q12 := 284230]
dat_prss[Q1 == 871 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q12 := 20798]
dat_prss[Q1 == 737 & Q2 == 'The second replication task', Revision_of_Q12 := 36762]
dat_prss[Q1 == 737 & Q2 == 'The second replication task', Revision_of_Q18 := 22774]
dat_prss[Q1 == 737 & Q2 == 'The second replication task', Revision_of_Q21 := 13988]
dat_prss[Q1 == 737 & Q2 == 'The first replication task', Revision_of_Q12 := 738057]
dat_prss[Q1 == 737 & Q2 == 'The first replication task', Revision_of_Q18 := 179856]
dat_prss[Q1 == 737 & Q2 == 'The first replication task', Revision_of_Q21 := 558201]
dat_prss[Q1 == 737 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q12 := 248351]
dat_prss[Q1 == 737 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q18 := 110830]
dat_prss[Q1 == 737 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q21 := 137521]
dat_prss[Q1 == 158 & Q2 == "The second replication task", Revision_of_Q18 := 17432]
dat_prss[Q1 == 395 & Q2 == "The second replication task", Revision_of_Q18 := 2834]
dat_prss[Q1 == 591 & Q2 == "The second replication task", Revision_of_Q12 := 28863]
dat_prss[Q1 == 591 & Q2 == "The second replication task", Revision_of_Q18 := 5664]
dat_prss[Q1 == 591 & Q2 == "The second replication task", Revision_of_Q21 := 23199]
dat_prss[Q1 == 923 & Q2 == 'The first replication task', Revision_of_Q12 := 313046]
dat_prss[Q1 == 923 & Q2 == 'The first replication task', Revision_of_Q21 := 221167]
dat_prss[Q1 == 842 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q12 := 3283605]
dat_prss[Q1 == 842 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q21 := 3196015]

# Filter out rows with missing Q2 values
dat = dat_prss[!is.na(Q2)]

# Recode the 'Q2' variable
qrecode(dat_prss, 'Q2', 
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

dat_prss <- dat_prss[!Round %in% c("Task 3 Revision", "Task 3")]

# Set the order of factors in 'Round'
dat_prss[, Round := factor(Round, levels = c('Task 1',
                                        'Task 1 Revision',
                                        'Task 2',
                                        'Task 2 Revision'
                                        ))]

# Prepare data for analysis -----

# 90% winsorize the variables

winsorize <- function(x, probs = c(0.05, 0.95)) {
  qnt <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < qnt[1]] <- qnt[1]
  x[x > qnt[2]] <- qnt[2]
  return(x)
}

vars_to_winsorize <- c("Revision_of_Q12", "Revision_of_Q18", "Revision_of_Q21")

dat_prss[, (vars_to_winsorize) := lapply(.SD, function(x) winsorize(x, probs = c(0.10, 0.90))),
    by = Round, .SDcols = vars_to_winsorize]


# Function to compare revisions
compare_revis = function(r) {
  thisr = dat_prss[Round %in% paste0('Task ', r, c('',' Revision'))]
  pairs = fread(here("data", paste0('task_', r, '_peer_review_pairs.csv')))
  pairs = pairs[!(dont_send)]
  thisr = merge(thisr, pairs[, .(Q1 = id2, match = id1, pairID)], all.x = TRUE)
  thisr[, got_reviewed := !is.na(pairID)]
  thisr[, Stage := fifelse(Round == paste0('Task ', r), 0, 1)]
  thisr[, ReviewRound := r]
  return(thisr)
}

# Combine reviews from all tasks
reviews = rbindlist(lapply(1:2, compare_revis))

# Effect Distribution ----

# Function to calculate differences in the specified variables
get_diff = function(i, r, var_name) {
  if (!reviews[i, got_reviewed]) {
    return(NA_real_)
  }
  thispair = reviews[Q1 %in% c(reviews[i, Q1], reviews[i, match]) & Round == r]
  if (nrow(thispair) < 2) {
    return(NA_real_)
  }
  return(abs(thispair[[var_name]][2] - thispair[[var_name]][1]))
}

# List of variables to analyze
variables_to_analyze = c("Revision_of_Q12", "Revision_of_Q18", "Revision_of_Q21")

# Loop through each variable and calculate differences
for (var in variables_to_analyze) {
  var_diff_cols = paste0('Diff_', var, '_', 1:length(unique(reviews$Round)))
  for (rn in 1:length(unique(reviews$Round))) {
    reviews[[var_diff_cols[rn]]] = sapply(1:nrow(reviews), function(x) get_diff(x, unique(reviews$Round)[rn], var))
  }
}

# Update 'more_sim' with new variables
diff_col_names = unlist(lapply(variables_to_analyze, function(var) {
  paste0('Diff_', var, '_', 1:length(unique(reviews$Round)))
}))

more_sim = unique(reviews[Stage == 0 & (got_reviewed), c("Round", "pairID", diff_col_names), with = FALSE])

# Copy reviews for safety
reviews_copy = copy(reviews)

# Update 'Reviewed' flags
reviews_copy[, Reviewed_1 := fifelse(sum(ReviewRound == 1) > 0, any(got_reviewed[ReviewRound == 1]), FALSE), by = Q1]
reviews_copy[, Reviewed_2 := fifelse(sum(ReviewRound == 2) > 0, any(got_reviewed[ReviewRound == 2]), FALSE), by = Q1]

# Prepare data for distribution comparison
dist_compare_list = list()

for (var in variables_to_analyze) {
  dist_compare = rbindlist(list(
    reviews_copy[Round == 'Task 1' & Revision_of_Q6 > 0, .(Variable = var, Round = 'Round 1', Reviewed = Reviewed_1, Observed = 'Pre-Review', Effect = get(var), weight = fifelse(1/Revision_of_Q6 > 200, 200, 1/Revision_of_Q6))],
    reviews_copy[Round == 'Task 2' & Revision_of_Q6 > 0, .(Variable = var, Round = 'Round 1', Reviewed = Reviewed_1, Observed = 'Next Round', Effect = get(var), weight = fifelse(1/Revision_of_Q6 > 200, 200, 1/Revision_of_Q6))],
    reviews_copy[Round == 'Task 2' & Revision_of_Q6 > 0, .(Variable = var, Round = 'Round 2', Reviewed = Reviewed_2, Observed = 'Pre-Review', Effect = get(var), weight = fifelse(1/Revision_of_Q6 > 200, 200, 1/Revision_of_Q6))]
  ))
  
  dist_compare[, Observed := factor(Observed, levels = c('Pre-Review','Next Round'))]
  dist_compare[, Reviewed := fifelse(Reviewed == 1, 'Not Peer-Reviewed','Peer Reviewed')]
  
  dist_compare_list[[var]] = dist_compare
}

# Function to drop leading zeros
dropLeadingZero <- function(l){
  str_replace(l, '0(?=.)', '')
}

# Create a named list to store plots
p_peer_review_distributions = list()


# Map variable names to descriptive titles
variable_descriptions <- list(
  "Revision_of_Q12" = "Analytic Sample Size",
  "Revision_of_Q18" = "Eligible for DACA Sample Size",
  "Revision_of_Q21" = "Not Eligible for DACA Sample Size"
)

# Plot effect distributions for each variable and store them in the list
for (var in variables_to_analyze) {

    # Calculate the range of the Effect variable
  data_range <- range(dist_compare_list[[var]]$Effect, na.rm = TRUE)
  
  # Generate default breaks using pretty(), ensuring zero is included
  default_breaks <- pretty(data_range)
  
  # Exclude the maximum value to avoid label at the right-end (if desired)
  custom_breaks <- default_breaks[default_breaks < max(default_breaks)]
  
  # Include zero in custom_breaks if not already present
  if (!0 %in% custom_breaks) {
    custom_breaks <- sort(c(0, custom_breaks))
  }  
  p <- ggplot(dist_compare_list[[var]], aes(x = Effect, weight = weight, color = Reviewed, fill = Reviewed)) + 
    geom_density(alpha = .4) + 
    scale_color_manual(values = colorpal) +
    scale_fill_manual(values = colorpal) +
    scale_x_continuous(
      breaks = custom_breaks,
      labels = comma_format(),
      limits = c(min(custom_breaks), max(data_range)) 
    ) +
    facet_grid(cols = vars(Observed), rows = vars(Round)) + 
    theme_nick() + 
    labs(
      title = paste("Distributions for", variable_descriptions[[var]]),
      caption = 'All rounds are 80% winsorized', y = 'Density'
    ) +
    guides(
      x = guide_axis(n.dodge = 2)
    )
  
  # Store the plot in the list with the variable name
  p_peer_review_distributions[[var]] = p
}

# Refer to each plot by its variable name, for example:
p_peer_review_distributions[["Revision_of_Q12"]]
p_peer_review_distributions[["Revision_of_Q18"]]
p_peer_review_distributions[["Revision_of_Q21"]]




# Do you become more like your peer reviewer in effect size? ----

# List of variables to analyze
variables_to_analyze = c("Revision_of_Q12", "Revision_of_Q18", "Revision_of_Q21")

# Create a list to store changediff data tables and plots for each variable
changediff_list = list()
plots_more_like_reviewer = list()

# Map variable names to descriptive titles (ensure this is defined)
variable_descriptions <- list(
  "Revision_of_Q12" = "Analytic Sample Size",
  "Revision_of_Q18" = "Eligible for DACA Sample Size",
  "Revision_of_Q21" = "Not Eligible for DACA Sample Size"
)

# Loop over each variable
for (var in variables_to_analyze) {
  
  # Re-initialize 'reviews' for Tasks 1 and 2
  reviews = rbindlist(lapply(1:2, compare_revis))
  
  # Define 'get_diff_across' function for the variable
  get_diff_across = function(i, r, var_name) {
    if (!reviews[i, got_reviewed]) {
      return(NA_real_)
    }
    prev_round_num = as.numeric(str_extract(r, "\\d")) - 1
    prev_round = paste0('Task ', prev_round_num)
    thispair = reviews[(Q1 == reviews[i, Q1] & Round == r) | 
                         (Q1 == reviews[i, match] & Round == prev_round)]
    if (nrow(thispair) < 2) {
      return(NA_real_)
    }
    return(abs(thispair[[var_name]][2] - thispair[[var_name]][1]))
  }
  
  # Compute 'Diff's for the variable
  roundnames = c('Task 1', 'Task 2')
  for (rn in 1:length(roundnames)) {
    r = roundnames[rn]
    reviews[[paste0('Diff_', var, '_', rn)]] = sapply(1:nrow(reviews), function(x) get_diff(x, r, var))
    if (rn > 1) {
      reviews[[paste0('Diff_', var, '_', rn, '_vs_', rn-1)]] = sapply(1:nrow(reviews), function(x) get_diff_across(x, r, var))
    }
  }
  
  # Separate 'unreviewed' and 'reviewed' data
  unreviewed = reviews[!(got_reviewed)]
  reviewed = reviews[(got_reviewed)]
  
  # Assign IDs to unreviewed observations
  unreviewed[, id := .I]
  
  # Create all possible pairs among unreviewed observations
  allreviews = CJ(id1 = unreviewed$id, id2 = unreviewed$id)
  allreviews = allreviews[id1 < id2]  # Avoid duplicates and self-pairing
  
  # Prepare 'ar_round' data tables for each round
  ar_round_list = list()
  for (rn in 1:length(roundnames)) {
    r = roundnames[rn]
    # Merge Effect sizes for each pair in the same round
    ar = merge(allreviews, unreviewed[Round == r, .(id1 = id, E1 = get(var))], by = 'id1')
    ar = merge(ar, unreviewed[Round == r, .(id2 = id, E2 = get(var))], by = 'id2')
    ar[, diff := abs(E1 - E2)]
    ar_round_list[[paste0('ar_round', rn)]] = ar
  }
  
  # Prepare cross-round comparisons (e.g., ar_round2v1)
  # Comparing Task 1 and Task 2 for unreviewed
  ar_round2v1 = merge(allreviews, unreviewed[Round == 'Task 1', .(id1 = id, E1 = get(var))], by = 'id1')
  ar_round2v1 = merge(ar_round2v1, unreviewed[Round == 'Task 2', .(id2 = id, E2 = get(var))], by = 'id2')
  ar_round2v1[, diff := abs(E1 - E2)]
  
  # Build 'changediff_var' data table
  changediff_var = rbindlist(list(
    # Reviewed
    data.table(Type = 'Reviewed', Round = 'Task 1', Comparison = 'Original',
               diff = reviewed[Round == 'Task 1'][[paste0('Diff_', var, '_1')]]),
    data.table(Type = 'Reviewed', Round = 'Task 1', Comparison = 'Next Round', 
               diff = reviewed[Round == 'Task 1'][[paste0('Diff_', var, '_2')]]),
    data.table(Type = 'Reviewed', Round = 'Task 1', Comparison = 'Next vs. This',
               diff = reviewed[Round == 'Task 1'][[paste0('Diff_', var, '_2_vs_1')]]),
    data.table(Type = 'Reviewed', Round = 'Task 2', Comparison = 'Original',
               diff = reviewed[Round == 'Task 2'][[paste0('Diff_', var, '_2')]]),
    # Unreviewed
    data.table(Type = 'Unreviewed', Round = 'Task 1', Comparison = 'Original',
               diff = ar_round_list[['ar_round1']]$diff),
    data.table(Type = 'Unreviewed', Round = 'Task 2', Comparison = 'Original',
               diff = ar_round_list[['ar_round2']]$diff),
    data.table(Type = 'Unreviewed', Round = 'Task 1', Comparison = 'Next Round',
               diff = ar_round_list[['ar_round2']]$diff),
    data.table(Type = 'Unreviewed', Round = 'Task 1', Comparison = 'Next vs. This',
               diff = ar_round2v1$diff)
  ), use.names = TRUE, fill = TRUE)
  
  # Adjust 'Comparison' factor levels
  changediff_var[, Comparison := factor(Comparison, levels = c('Original','Next Round','Next vs. This'))]
  
  # Store 'changediff_var' in the list
  changediff_list[[var]] = changediff_var
  
  # Generate the plot for the variable
  p_var = ggplot(changediff_var, aes(x = diff, color = Type, fill = Type)) + 
    geom_density(alpha = .1) +
    scale_color_manual(values = colorpal) +
    scale_fill_manual(values = colorpal) +
    scale_x_continuous(labels = comma_format()) +
    theme_nick() + 
    facet_grid(rows = vars(Round), cols = vars(Comparison)) + 
    labs(
      title = paste('Absolute Difference for', variable_descriptions[[var]]),
      caption = 'No weights applied.',
      x = 'Absolute Difference',
      y = 'Density'
    )
  
  # Store the plot in the list
  plots_more_like_reviewer[[var]] = p_var
  
  # Optionally, run the regression and store results
  peer_review_reg = feols(diff ~ Comparison * Type, data = changediff_var, split = ~Round)
  
  # Store regression results if needed (e.g., in a list)
  # regression_results[[var]] = peer_review_reg
  
  # If you want to print or save the regression results, you can do so here
}

# Access the plots and changediff data for each variable:

print(plots_more_like_reviewer[["Revision_of_Q12"]])
changediff_Q12 = changediff_list[["Revision_of_Q12"]]

print(plots_more_like_reviewer[["Revision_of_Q18"]])
changediff_Q18 = changediff_list[["Revision_of_Q18"]]

print(plots_more_like_reviewer[["Revision_of_Q21"]])
changediff_Q21 = changediff_list[["Revision_of_Q21"]]


