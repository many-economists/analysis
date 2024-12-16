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
viol = efdat[Round %in% c('Task 1','Task 2','Task 3') & Revision_of_Q4 != 0, .(Round, Revision_of_Q4, Revision_of_Q6, Revision_of_Q12, Revision_of_Q18, weight = 1, Type = 'Unweighted')]
viol = rbind(viol,
             efdat[Round %in% c('Task 1','Task 2','Task 3'), .(Round, Revision_of_Q4, Revision_of_Q12, Revision_of_Q18, weight = 1/Revision_of_Q6, Revision_of_Q6, Type = 'Weighted')])
viol[!is.infinite(weight), weight := fifelse(weight > quantile(weight[!is.infinite(weight) & !is.na(weight)], .95), quantile(weight[!is.infinite(weight) & !is.na(weight)], .95), weight)]

# Density distributions of the effects
p_effect_distribution = ggplot(viol[(!is.infinite(weight)) & !is.na(weight)], aes(x = Revision_of_Q4, weight = weight)) + 
  geom_density(fill = colorpal[1], alpha = .5) +
  geom_boxplot(width = 5) +
  theme_nick() + 
  coord_cartesian(xlim = c(-.05, .1)) +
  labs(x = 'Effect Size', caption = 'Range limited to [-.05, .1] for viewing.', y = 'Density') +
  facet_grid(rows = vars(Type), cols = vars(Round),
             scales = 'free_x')

p_effect_distribution_task_on_y_axis = ggplot(viol[(!is.infinite(weight)) & !is.na(weight)], aes(x = Revision_of_Q4, weight = weight)) + 
  geom_density(fill = colorpal[1], alpha = .5) +
  geom_boxplot(width = 5) +
  theme_nick() + 
  coord_cartesian(xlim = c(-.05, .1)) +
  labs(x = 'Effect Size (limited to [-.05, .1] for viewing)', y = 'Density') +
  facet_grid(
    rows = vars(Round), 
    cols = vars(Type)
  ) +
  theme(
    strip.placement = "outside",            # Keeps strip labels in the correct orientation.
    axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5) # Rotates and centers the y-axis label.
  )

# summary table
weighted_quantile_rm = function(x, w, m) {
  dat = data.table(x = x, w = w)
  dat = na.omit(dat)
  if (nrow(dat) == 0) {
    return(NA)
  }
  if (nrow(dat) == 1) {
    return(dat[, x])
  }
  setorder(dat, x)
  dat[, `:=`(cumw, cumsum(w)/sum(w))]
  if (dat[1, cumw] >= 0.5) {
    return(dat[1, x])
  }
  return(dat[cumw >= m & shift(cumw) < m, x])
}
make_sumline = function(var, name, round = 'Task 1', weight = FALSE, acc = 1) {
  dsum = viol[Round == round & Type == 'Weighted']
  dsum = subset(dsum, select = c('weight',var))
  if (!weight) {
    dsum[,weight := 1]
  }
  dsum = dsum[!is.na(weight) & weight > 0 & !is.infinite(weight)]
  setnames(dsum, var, 'var')
  dsum = dsum[, .(N = sum(!is.na(var)), 
                  Mean = weighted.mean(var, weight, na.rm = TRUE),
                  SD = weighted.sd(var, weight),
                  Min = min(var, na.rm = TRUE),
                  `Pctl. 25` = weighted_quantile_rm(var, weight, .25),
                  Median = weighted_quantile_rm(var, weight, .5),
                  `Pctl. 75` = weighted_quantile_rm(var, weight, .75),
                  Max = max(var, na.rm = TRUE))]
  nonncolumns = names(dsum)[2:8]
  dsum[, (nonncolumns) := lapply(.SD, label_comma(acc)), .SDcols = nonncolumns]
  dsum[, Variable := name]
  setcolorder(dsum, 'Variable', before = 'N')
  return(dsum)
}
headerrow = function(name) {
  data.table(Variable = paste0(name,'_MULTICOL_c_9'),
             N = 'DELETECELL',
             Mean = 'DELETECELL',
             SD = 'DELETECELL',
             Min = 'DELETECELL',
             `Pctl. 25` = 'DELETECELL',
             Median = 'DELETECELL',
             `Pctl. 75` = 'DELETECELL',
             Max = 'DELETECELL')
}
oneround = function(round) {
  taskround = paste0('Task ',round)
  return(rbindlist(list(
    headerrow(paste0('Round: ',taskround)),
    make_sumline('Revision_of_Q4', 'Effect Size (Unweighted)', round = taskround, weight = FALSE, acc = .001),
    make_sumline('Revision_of_Q4', 'Effect Size (Weighted)', round =  taskround, weight = TRUE, acc = .001),
    make_sumline('Revision_of_Q6', 'Standard Error', round =  taskround, weight = FALSE, acc = .001),
    make_sumline('Revision_of_Q12', 'Sample Size', round =  taskround, weight = FALSE, acc = 1),
    make_sumline('Revision_of_Q18', 'Treated-Group Size', round =  taskround, weight = FALSE, acc = 1)
  )))
}
effect_sum_tab = list(oneround(1),
                       oneround(2),
                      oneround(3)) |>
  rbindlist()

viol[, ci_upper := Revision_of_Q4 + 1.96*Revision_of_Q6]
viol[, ci_lower := Revision_of_Q4 - 1.96*Revision_of_Q6]
viol[, sig := !xor((ci_upper > 0),(ci_lower > 0))]
setorder(viol, Revision_of_Q4)
viol[, Order := 1:.N, by = .(Round, Type)]

# Full arrangement of the effect sizes
p_full_effect_distribution_individual = ggplot(viol[Type == 'Unweighted'], aes(x = Order, y = Revision_of_Q4,
                                       ymax = ci_upper, ymin = ci_lower)) + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_errorbar(mapping = aes(color = sig)) + 
  geom_point()+
  coord_cartesian(ylim = c(-.05, .15)) + 
  scale_color_manual(values = colorpal) +
  facet_wrap(~Round, nrow = 3) + 
  guides(color = 'none') +
  labs(y = 'Effect Size\n(95% CI)',
       x = NULL, caption = '95% CI reconstructed from effect size and SE, even if asymmetric\nCI was reported. Visible range limited to (-.05, .15).') + 
  theme_nick() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Density distributions of sample sizes
p_sample_size_distributions = ggplot(viol[Type == 'Unweighted' & Round %in% c('Task 1','Task 2')], aes(x = Revision_of_Q12)) + 
  geom_density(fill = colorpal[1], alpha = .5) +
  geom_boxplot(width = .1) +
  theme_nick() + 
  coord_cartesian(ylim = c(0, 2)) +
  scale_x_log10(labels = label_rangescale()) +
  labs(x = 'Total Sample Size', y = 'Density') +
  facet_grid(rows = vars(Type), cols = vars(Round))

# Density distributions of the treated-group sample sizes
p_treated_group_sample_size = ggplot(viol[Type == 'Unweighted'], aes(x = Revision_of_Q18)) + 
  geom_density(fill = colorpal[1], alpha = .5) +
  geom_boxplot(width = .1) +
  theme_nick() + 
  coord_cartesian(ylim = c(0, 2),
                  xlim = c(1000,200000)) +
  scale_x_log10(labels = label_rangescale()) +
  labs(x = 'Treated-Group Sample Size', caption = 'Range limited to [1k, 200k] for viewing.', y = 'Density') +
  facet_grid(rows = vars(Type), cols = vars(Round))