p_sample_size_scatter = dat[Round == 'Task 2'] |>
  ggplot(aes(x = Revision_of_Q12, y = Revision_of_Q4)) + 
  geom_smooth( color = 'black', se = FALSE) +
  geom_point() + 
  theme_nick() + 
  labs(x = 'Sample Size (log scale)', y = 'Effect Size',
       caption = 'Analysis range limited to effects from 0 to .1') + 
  scale_x_log10(label = label_rangescale()) + 
  scale_y_continuous(limits = c(0, .1))

p_standard_error_scatter = dat[Round == 'Task 2'] |>
  ggplot(aes(x = Revision_of_Q6, y = Revision_of_Q4)) + 
  geom_smooth(color = 'black', se = FALSE) +
  geom_point() + 
  theme_nick() + 
  labs(x = 'Standard Error', y = 'Effect Size',
       caption = 'Analysis range limited to effects from 0 to .1') + 
  scale_y_continuous(limits = c(0, .1)) + 
  scale_x_log10() + 
  coord_cartesian(xlim = c(0.001, .5))

rvs = dat[Round %in% c('Task 1','Task 2','Task 3') & Q1 %in% dat[Round == 'Task 3', Q1], .(Round, Effect = Revision_of_Q4, Q1)] |>
  dcast(Q1 ~ Round, value.var ='Effect')
get_cor = function(ta,tb) {
  x = rvs[[ta]]
  y = rvs[[tb]]
  correl = cor(x,y)
  pval = cor.test(x,y)$p.value
  nstars = (pval < .1) + (pval < .05) + (pval < .01)
  paste0('Corr. ', number(correl,.001),rep('*',nstars))
}
dropLeadingZero <- function(l){
  str_replace(l, '0(?=.)', '')
}
p_task1_vs_task2 = rvs |>
  ggplot(aes(x = `Task 1`, y = `Task 2`)) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  geom_smooth(color = 'black', se = FALSE) +
  geom_point() + 
  theme_nick() + 
  annotate(geom = 'text', x = .025, y = -0.04,
           label = get_cor('Task 1','Task 2'),
           family = 'serif', size = 13/.pt) +
  labs(x = 'Task 1 Effect', y = 'Task 2 Effect',
       caption = ' ') + 
  # scale_y_continuous(limits = c(0, .1),
  #                    labels = dropLeadingZero) +
  # scale_x_continuous(limits = c(0, .1),
  #                    labels = dropLeadingZero)  +
  coord_cartesian(
    xlim = c(-.05, .1), ylim = c(-.05, .1)
  ) +
  theme(
    axis.title.y = element_text(angle = 90, vjust = .5, hjust = 0.5),
  )

p_task2_vs_task3 = rvs |>
  ggplot(aes(x = `Task 2`, y = `Task 3`)) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  geom_smooth(color = 'black', se = FALSE) +
  geom_point() + 
  theme_nick() + 
  annotate(geom = 'text', x = .025, y = -0.04,
           label = get_cor('Task 2','Task 3'),
           family = 'serif', size = 13/.pt) +
  labs(x = 'Task 2 Effect', y = 'Task 3 Effect',
       caption = 'Visible range limited to effects from 0 to .1.') + 
  # scale_y_continuous(limits = c(0, .1),
  #                    labels = dropLeadingZero) +
  # scale_x_continuous(limits = c(0, .1),
  #                    labels = dropLeadingZero)  +
  coord_cartesian(
    xlim = c(-.05, .1), ylim = c(-.05, .1)
  ) +
  theme(
    axis.title.y = element_text(angle = 90, vjust = .5, hjust = 0.5),
  )

p_task1_vs_task3 = rvs |>
  ggplot(aes(x = `Task 1`, y = `Task 3`)) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  geom_smooth(color = 'black', se = FALSE) +
  geom_point() + 
  theme_nick() + 
  annotate(geom = 'text', x = .025, y = -0.04,
           label = get_cor('Task 1','Task 3'),
           family = 'serif', size = 13/.pt) +
  labs(x = 'Task 1 Effect', y = 'Task 3 Effect',
       caption = '*/**/***: p < .1/.05/.01.') + 
  # scale_y_continuous(limits = c(0, .1),
  #                    labels = dropLeadingZero) +
  # scale_x_continuous(limits = c(0, .1),
  #                    labels = dropLeadingZero) +
  coord_cartesian(
    xlim = c(-.05, .1), ylim = c(-.05, .1)
    ) +
  theme(
    axis.title.y = element_text(angle = 90, vjust = .5, hjust = 0.5),
  )

p_compare_rounds = p_task1_vs_task2+p_task2_vs_task3+p_task1_vs_task3

# Share above .05 by controls
shtask = allcontrols[Q1 %in% Q1[Round == 'Task 3'], .(Q1, Effect, Round, Control = as.character(Control))] |>
  rbind(dat[Round %in% paste0('Task ',1:3) & Q1 %in% Q1[Round == 'Task 3'], .(Q1, Effect = Revision_of_Q4, Round, Control = 'Total')])
shtask = shtask[, .(N = uniqueN(Q1),
                    `Share above .05` = percent(mean(Effect > .05, na.rm = TRUE), .1)),
                by = .(Round,Control)][order(-`Share above .05`)] |>
  dcast(Control ~ Round, value.var = c('N','Share above .05'))
setcolorder(shtask, c('Control','N_Task 1','Share above .05_Task 1',
                      'N_Task 2','Share above .05_Task 2',
                      'N_Task 3','Share above .05_Task 3'))
setnames(shtask, c('Covariate','Task 1: N', 'Above .05',
                   'Task 2: N','Above .05',
                   'Task 3: N','Above .05'))
shtask[, Covariate := c('Age','Age at Migration',
                      'Age in 2012', 'Education','Labor Force Participation Rate',
                      'Marital Status','None','Other', 'Race','Sex','English Speaker',
                      'State','State Policy Variables','Total','Unemployment Rate',
                      'Year','Year of Migration','Continuous Years in USA')]
shtask = rbindlist(list(
  shtask[Covariate == 'Total'],
  shtask[!(Covariate %in% c('None','Other','Total'))],
  shtask[Covariate == 'Other'],
  shtask[Covariate == 'None']
))

# Changes in controls from task 1 to 2
changelevels = dat[Round %in% c('Task 1','Task 2') & Q1 %in% Q1[Round == 'Task 3'],
                   .(Q1, Round, Effect = Revision_of_Q4)] |>
  dcast(Q1 ~ Round)
changelevels = changelevels[, .(Q1, Increase = `Task 2` - `Task 1`)][!is.na(Increase)]

# Check whether changes in controls drove shifts
conchange = allcontrols[Round %in% c('Task 1','Task 2') & Q1 %in% Q1[Round == 'Task 3'], .(Q1,Round, Control, Effect)] |> unique()
conchange = conchange[,.(ControlSwitch = fcase(
  sum(Round == 'Task 1') > 0 & sum(Round == 'Task 2') > 0, 'In Both',
  sum(Round == 'Task 1') > 0 & sum(Round == 'Task 2') == 0, 'Removed',
  sum(Round == 'Task 1') == 0 & sum(Round == 'Task 2') > 0, 'Added'
)), by = .(Q1, Control)] |>
  merge(changelevels, by = 'Q1') |>
  merge(dat[Round == 'Task 2', .(R2Effect = Revision_of_Q4,
                                 Q1)], by = 'Q1')

# Changes in sample limitations from task 1 to task 2
sampconds = names(r12samp_w_q1)
sampconds = sampconds[!(sampconds %in% c('Round','Effect','SE','Sample Size', 'Sample','Round/Sample','Q1'))]
changesamp = r12samp_w_q1[Sample == 'All' & Q1 %in% dat[Round == 'Task 3', Q1],
                          lapply(.SD, \(x) x[1] != x[2]),
                          .SDcols = sampconds,
                          by = Q1] |>
  melt(id.vars = 'Q1',
       variable.name = 'Sample Limitation',
       value.name = 'Changed') |>
  merge(changelevels, by = 'Q1') |>
  merge(dat[Round == 'Task 2', .(R2Effect = Revision_of_Q4,
                                 Q1)], by = 'Q1') |>
  merge(dat[Round == 'Task 2', .(Q1,`Sample Size` = Revision_of_Q12)], by = 'Q1') |>
  merge(dat[Round == 'Task 2', .(Q1,`Standard Error` = Revision_of_Q6)], by = 'Q1')

# matching the description vs not distribution
r12samp_w_q1[, Correct := fifelse(Round == 'Task 2' & Hispanic == 'Hispanic-Mexican' & Birthplace == 'Mexican-Born' & Citizenship == 'Non-Citizen' & `Age at Migration` == '< 16' & `Age in June 2012` == 'Year-Quarter Age' & `Education/Veteran` == 'HS Grad or Veteran','Match','Some Mismatch')]

r12samp_w_q1[, weight := fcase(SE == 0, NA_real_,
                               1/SE > 200, 200,
                               1/SE <= 200, 1/SE)]

p_match_vs_mismatch_distribution = ggplot(unique(r12samp_w_q1[`Round/Sample` == 'Task 2 Treated' & !is.infinite(weight) & !is.na(weight) & Q1 %in% dat[Round == 'Task 3', Q1], .(Q1, Correct, Effect, weight)]), 
       aes(x = Effect, fill = Correct, weight = weight)) + 
  geom_density(alpha = .4) + 
  scale_fill_manual(values = colorpal) + 
  scale_x_continuous(limits = c(-.05, .1)) +
  labs(x = 'Task 2 Effect Estimate',
       y = 'Density',
       caption = 'Visible range restricted to -.05 to .10 for clarity') + 
  theme_nick()

# Matching the instructions
rfield = unique(dat[Q1 %in% dat[Round == 'Task 3', Q1], .(Researcher_Cats, Q1, Researcher_Q13)])
rfield[Researcher_Q13 %like% 'Labor', Researcher_Cats := 'Labor']
rfield[Researcher_Q13 %like% 'Immigration', Researcher_Cats := 'Immigration']
rfield[Researcher_Q13 %like% 'Labor' & Researcher_Q13 %like% 'Immigration', Researcher_Cats := 'Immigration & Labor']
rfield = merge(unique(r12samp_w_q1[`Round/Sample` == 'Task 2 Treated' & Q1 %in% dat[Round == 'Task 3', Q1], .(Q1, Correct)]), rfield, by = 'Q1')
rfield[is.na(Researcher_Cats), Researcher_Cats := 'Neither']
rfield[Researcher_Cats == 'Neither', Researcher_Cats := 'Neither/Other']
rfield = rfield[, .(N = .N), by = .(Correct, Field = Researcher_Cats)]
rfield = rfield[, .(N = N, Share = N/sum(N),
                    Correct), by = Field] |>
  dcast(Field ~ Correct, value.var = c('N','Share'))
rfield[, Share_Match := percent(Share_Match, .1)]
rfield[, `Share_Some Mismatch` := percent(`Share_Some Mismatch`,.1)]
setcolorder(rfield, c('Field', 'Share_Match', 'N_Match', 'Share_Some Mismatch', 'N_Some Mismatch'))
setnames(rfield, c('Field', 'Share Match','Num. Match','Share Some Mismatch','Num Some Mismatch'))
rfield[is.na(`Share Match`), `Share Match` := '0.0%']
rfield[is.na(`Share Some Mismatch`), `Share Some Mismatch` := '0.0%']
rfield[is.na(`Num. Match`), `Num. Match` := '0']
rfield[is.na(`Num Some Mismatch`), `Num Some Mismatch` := 0]