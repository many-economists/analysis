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
dat = import(here("data", "cleaned_survey_post_corrections.parquet"), setclass = 'data.table')

# Replace specific characters in the Revision columns
dat[, Revision_of_Q14 := str_replace_all(Revision_of_Q14, '‚Äì','-')]
dat[, Revision_of_Q17 := str_replace_all(Revision_of_Q17, '‚Äì','-')]
dat[, Revision_of_Q20 := str_replace_all(Revision_of_Q20, '‚Äì','-')]

# Revisions of sample sizes
dat[Q1 == 15 & Q2 == 'The first replication task', Revision_of_Q12 := 5979569]
dat[Q1 == 834 & Q2 == 'The first replication task', Revision_of_Q12 := 2924560]
dat[Q1 == 176 & Q2 == 'The first replication task', Revision_of_Q12 := 284230]
dat[Q1 == 871 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q12 := 20798]
dat[Q1 == 737 & Q2 == 'The second replication task', Revision_of_Q12 := 36762]
dat[Q1 == 737 & Q2 == 'The second replication task', Revision_of_Q18 := 22774]
dat[Q1 == 737 & Q2 == 'The second replication task', Revision_of_Q21 := 13988]
dat[Q1 == 737 & Q2 == 'The first replication task', Revision_of_Q12 := 738057]
dat[Q1 == 737 & Q2 == 'The first replication task', Revision_of_Q18 := 179856]
dat[Q1 == 737 & Q2 == 'The first replication task', Revision_of_Q21 := 558201]
dat[Q1 == 737 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q12 := 248351]
dat[Q1 == 737 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q18 := 110830]
dat[Q1 == 737 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q21 := 137521]
dat[Q1 == 158 & Q2 == "The second replication task", Revision_of_Q18 := 17432]
dat[Q1 == 395 & Q2 == "The second replication task", Revision_of_Q18 := 2834]
dat[Q1 == 591 & Q2 == "The second replication task", Revision_of_Q12 := 28863]
dat[Q1 == 591 & Q2 == "The second replication task", Revision_of_Q18 := 5664]
dat[Q1 == 591 & Q2 == "The second replication task", Revision_of_Q21 := 23199]
dat[Q1 == 923 & Q2 == 'The first replication task', Revision_of_Q12 := 313046]
dat[Q1 == 923 & Q2 == 'The first replication task', Revision_of_Q21 := 221167]
dat[Q1 == 842 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q12 := 3283605]
dat[Q1 == 842 & Q2 == 'Revision following the first replication task (such as following peer review)', Revision_of_Q21 := 3196015]

# Filter out rows with missing Q2 values
dat = dat[!is.na(Q2)]

# Recode the 'Q2' variable
qrecode(dat, 'Q2', 
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

dat <- dat[!Round %in% c("Task 3 Revision", "Task 3")]

# Set the order of factors in 'Round'
dat[, Round := factor(Round, levels = c('Task 1',
                                        'Task 1 Revision',
                                        'Task 2',
                                        'Task 2 Revision'
                                        ))]

# 90% Winsorize the variables

Winsorize <- function(x, probs = c(0.05, 0.95)) {
  qnt <- quantile(x, probs = probs, na.rm = TRUE)
  x[x < qnt[1]] <- qnt[1]
  x[x > qnt[2]] <- qnt[2]
  return(x)
}

vars_to_winsorize <- c("Revision_of_Q12", "Revision_of_Q18", "Revision_of_Q21")

dat[, (vars_to_winsorize) := lapply(.SD, function(x) Winsorize(x, probs = c(0.10, 0.90))),
    by = Round, .SDcols = vars_to_winsorize]


# Function to compare revisions
compare_revis = function(r) {
  thisr = dat[Round %in% paste0('Task ', r, c('',' Revision'))]
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
plots = list()

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
  plots[[var]] = p
}

# Refer to each plot by its variable name, for example:
# plots[["Revision_of_Q12"]]
# plots[["Revision_of_Q18"]]
# plots[["Revision_of_Q21"]]

# Do you become more like your peer reviewer in effect size? ----

reviews = rbindlist(lapply(1:3, compare_revis))

# Find differences in other rounds
get_diff_across = function(i, r) {
  if (!reviews[i, got_reviewed]) {
    return(NA_real_)
  }
  thispair = reviews[(Q1 == reviews[i, Q1] & Round == r) | (Q1 == reviews[i, match] & Round == paste0('Task ', as.numeric(str_sub(r,-1))-1))]
  if (nrow(thispair) < 2) {
    return(NA_real_)
  }
  return(abs(thispair$Revision_of_Q4[2] - thispair$Revision_of_Q4[1]))
}

roundnames = c('Task 1','Task 2','Task 3')
for (rn in 1:length(roundnames)) {
  reviews[[paste0('Diff_',rn)]] = sapply(1:nrow(reviews), \(x) get_diff(x, roundnames[rn]))
  reviews[[paste0('Diff_',rn,'_vs_',rn-1)]] = sapply(1:nrow(reviews), \(x) get_diff_across(x, roundnames[rn]))
}

unreviewed = reviews[!(got_reviewed)]
reviews = reviews[(got_reviewed)]

unreviewed[, id := 1:.N]
allreviews = CJ(id1 = 1:nrow(unreviewed), id2 = 1:nrow(unreviewed))
allreviews = allreviews[id1 != id2]
ar_round1 = merge(allreviews, unreviewed[Round == 'Task 1', .(id1 = id, E1 = Revision_of_Q4)], by = 'id1')
ar_round1 = merge(ar_round1, unreviewed[Round == 'Task 1', .(id2 = id, E2 = Revision_of_Q4)], by = 'id2')
ar_round1[, diff := abs(E1-E2)]
ar_round2 = merge(allreviews, unreviewed[Round == 'Task 2', .(id1 = id, E1 = Revision_of_Q4)], by = 'id1')
ar_round2 = merge(ar_round2, unreviewed[Round == 'Task 2', .(id2 = id, E2 = Revision_of_Q4)], by = 'id2')
ar_round2[, diff := abs(E1-E2)]
ar_round3 = merge(allreviews, unreviewed[Round == 'Task 3', .(id1 = id, E1 = Revision_of_Q4)], by = 'id1')
ar_round3 = merge(ar_round3, unreviewed[Round == 'Task 3', .(id2 = id, E2 = Revision_of_Q4)], by = 'id2')
ar_round3[, diff := abs(E1-E2)]
ar_round2v1 = merge(allreviews, unreviewed[Round == 'Task 1', .(id1 = id, E1 = Revision_of_Q4)], by = 'id1')
ar_round2v1 = merge(ar_round2v1, unreviewed[Round == 'Task 2', .(id2 = id, E2 = Revision_of_Q4)], by = 'id2')
ar_round2v1[, diff := abs(E1-E2)]
ar_round3v2 = merge(allreviews, unreviewed[Round == 'Task 2', .(id1 = id, E1 = Revision_of_Q4)], by = 'id1')
ar_round3v2 = merge(ar_round3v2, unreviewed[Round == 'Task 3', .(id2 = id, E2 = Revision_of_Q4)], by = 'id2')
ar_round3v2[, diff := abs(E1-E2)]

changediff = rbindlist(list(
  data.table(Type = 'Reviewed',Round = 'Task 1', Comparison = 'Original',
             diff = reviews[Round == 'Task 1', Diff_1]),
  data.table(Type = 'Reviewed',Round = 'Task 1', Comparison = 'Next Round', 
             diff = reviews[Round == 'Task 1', Diff_2]),
  data.table(Type = 'Reviewed',Round = 'Task 1', Comparison = 'Next vs. This',
             diff = reviews[Round == 'Task 1', Diff_2_vs_1]),
  data.table(Type = 'Reviewed',Round = 'Task 2', Comparison = 'Original',
             diff = reviews[Round == 'Task 2', Diff_2]),
  data.table(Type = 'Reviewed',Round = 'Task 2', Comparison = 'Next Round',
             diff = reviews[Round == 'Task 2', Diff_3]),
  data.table(Type = 'Reviewed',Round = 'Task 2', Comparison = 'Next vs. This',
             diff = reviews[Round == 'Task 2', Diff_3_vs_2]),
  data.table(Type = 'Unreviewed', Round = 'Task 1', Comparison = 'Original',
             diff = ar_round1$diff),
  data.table(Type = 'Unreviewed', Round = 'Task 2', Comparison = 'Original',
             diff = ar_round2$diff),
  data.table(Type = 'Unreviewed', Round = 'Task 1', Comparison = 'Next Round',
             diff = ar_round2$diff),
  data.table(Type = 'Unreviewed', Round = 'Task 2', Comparison = 'Next Round',
             diff = ar_round3$diff),
  data.table(Type = 'Unreviewed', Round = 'Task 1', Comparison = 'Next vs. This',
             diff = ar_round2v1$diff),
  data.table(Type = 'Unreviewed', Round = 'Task 2', Comparison = 'Next vs. This',
             diff = ar_round3v2$diff)
))
changediff[, Comparison := factor(Comparison, levels = c('Original','Next Round','Next vs. This'))]

p_more_like_reviewer = ggplot(changediff, aes(x = diff, color = Type,
                       fill = Type)) + 
  geom_density(alpha = .1) +
  scale_color_manual(values = colorpal) +
  scale_fill_manual(values = colorpal) +
  scale_x_continuous(breaks = c(0, .025, .05, .075, .1),
                     labels = c('0', '.025', '.05', '.075', .1)) +
  coord_cartesian(xlim = c(0, .1)) + 
  theme_nick() + 
  theme(axis.text.x = element_text(size = 10)) +
  facet_grid(rows = vars(Round),
             cols = vars(Comparison)) + 
  labs(caption = str_wrap('Original is this round vs. this round. Next round is next round vs. next round. Next vs. This is your next round vs. partner\'s this round. Values beyond .1 omitted for visibility. No weights applied.', 110),

       x = 'Absolute effect difference',
       y = 'Density')


# Regression to show impacts of peer review ----

peer_review_reg = feols(diff ~ Comparison*Type, data = changediff, split = 'Round')