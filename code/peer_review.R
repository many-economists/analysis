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
dat[, Round := factor(Round, levels = c('Task 1',
                                        'Task 1 Revision',
                                        'Task 2',
                                        'Task 2 Revision',
                                        'Task 3',
                                        'Task 3 Revision'))]
compare_revis = function(r) {
  thisr = dat[Round %in% paste0('Task ', r, c('',' Revision'))]
  pairs = fread(paste0('../data/task_', r, '_peer_review_pairs.csv'))
  pairs = pairs[!(dont_send)]
  thisr = merge(thisr, pairs[, .(Q1 = id2, match = id1, pairID)], all.x = TRUE)
  thisr[, got_reviewed := !is.na(pairID)]
  thisr[, Stage := fifelse(Round == paste0('Task ', r), 0, 1)]
  thisr[, ReviewRound := r]
  return(thisr)
}
reviews = rbindlist(lapply(1:3, compare_revis))

# Find differences in other rounds
get_diff = function(i, r) {
  if (!reviews[i, got_reviewed]) {
    return(NA_real_)
  }
  thispair = reviews[Q1 %in% c(reviews[i, Q1], reviews[i, match]) & Round == r]
  if (nrow(thispair) < 2) {
    return(NA_real_)
  }
  return(abs(thispair$Revision_of_Q4[2] - thispair$Revision_of_Q4[1]))
}

roundnames = sort(unique(reviews$Round))
for (rn in 1:length(roundnames)) {
  reviews[[paste0('Diff_',rn)]] = sapply(1:nrow(reviews), \(x) get_diff(x, roundnames[rn]))
}

more_sim = unique(reviews[Stage == 0 & (got_reviewed), .(Round, pairID, Diff_1, Diff_2, Diff_3, Diff_4, Diff_5, Diff_6)])

reviews = copy(reviews)
reviews[, Reviewed_1 := fifelse(sum(ReviewRound == 1) > 0, any(got_reviewed[ReviewRound == 1]), FALSE), by = Q1]
reviews[, Reviewed_2 := fifelse(sum(ReviewRound == 2) > 0, any(got_reviewed[ReviewRound == 2]), FALSE), by = Q1]
reviews[, Reviewed_3 := fifelse(sum(ReviewRound == 3) > 0, any(got_reviewed[ReviewRound == 3]), FALSE), by = Q1]

dist_compare = rbindlist(list(
  reviews[Round == 'Task 1' & Revision_of_Q6 > 0, .(Round = 'Round 1', Reviewed = Reviewed_1, Observed = 'Pre-Review', Effect = Revision_of_Q4, weight = fifelse(1/Revision_of_Q6> 200, 200, 1/Revision_of_Q6))],
  reviews[Round == 'Task 2' & Revision_of_Q6 > 0, .(Round = 'Round 1', Reviewed = Reviewed_1, Observed = 'Next Round', Effect = Revision_of_Q4, weight = fifelse(1/Revision_of_Q6> 200, 200, 1/Revision_of_Q6))],
  reviews[Round == 'Task 2' & Revision_of_Q6 > 0, .(Round = 'Round 2', Reviewed = Reviewed_2, Observed = 'Pre-Review', Effect = Revision_of_Q4, weight = fifelse(1/Revision_of_Q6> 200, 200, 1/Revision_of_Q6))],
  reviews[Round == 'Task 3' & Revision_of_Q6 > 0, .(Round = 'Round 2', Reviewed = Reviewed_2, Observed = 'Next Round', Effect = Revision_of_Q4, weight = fifelse(1/Revision_of_Q6> 200, 200, 1/Revision_of_Q6))],
  reviews[Round == 'Task 3' & Revision_of_Q6 > 0, .(Round = 'Round 3', Reviewed = Reviewed_2, Observed = 'Pre-Review', Effect = Revision_of_Q4, weight = fifelse(1/Revision_of_Q6> 200, 200, 1/Revision_of_Q6))]
))
dist_compare[, Observed := factor(Observed, levels = c('Pre-Review','Next Round'))]
dist_compare[, Reviewed := fifelse(Reviewed == 1, 'Not Peer-Reviewed','Peer Reviewed')]
# Effect distributions for those peer reviewed vs. not
p_peer_review_effect_distributions = ggplot(dist_compare, aes(x = Effect, weight = weight, color = Reviewed, fill = Reviewed)) + 
  geom_density(alpha = .4) + 
  scale_color_manual(values = colorpal) +
  scale_fill_manual(values = colorpal) +
  coord_cartesian(xlim = c(-.05, .15)) + 
  facet_grid(cols = vars(Observed), rows = vars(Round)) + 
  theme_nick() + 
  labs(caption = 'Viewing range limited to -.05 to .15.', y = 'Density')


# Do you become more like your peer reviewer?
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
  labs(caption = str_wrap('Original is this round vs. this round. Next round is next round vs. next round. Next vs. This is your next round vs. partner\'s this round. Values beyond .1 omitted for visibility. No weights applied.', 100),

       x = 'Absolute effect difference',
       y = 'Density')


# Regression to show impacts of peer review
peer_review_reg = feols(diff ~ Comparison*Type, data = changediff, split = 'Round')