rcats = dat[!is.na(Q2) & !(Q2 %like% 'Revision')]
rcats[, Q8Recode := fcase(
  Researcher_Q8 == 'I have 1-5 papers in applied microeconomics published or accepted for publication', '1-5 Papers in Applied Micro',
  Researcher_Q8 == 'I have 6+ papers in applied microeconomics published or accepted for publication', '6+ Papers',
  Researcher_Q8 == 'I have never performed academic research in applied microeconomics', 'No Academic Papers',
  Researcher_Q8 == 'I have written at least one academic paper in applied microeconomics, but none of this work is published or accepted for publication', 'No Published Academic Papers'
)]
rcats[, RaceRecode := Researcher_Q16]
rcats[str_detect(RaceRecode, ',') | RaceRecode == 'Other', RaceRecode := 'Other or Multiracial']
rcats[, RaceRecode := factor(RaceRecode,
                             levels = c('White','Asian','Black or African American','Hispanic','Other or Multiracial'))]
# Get F stat, p-value, and R2 for every predictor individually
preds = c('Researcher_Q10','Researcher_Q6', 'Q8Recode','Researcher_Q15',
          'RaceRecode',
          'Researcher_Q17',
          'Researcher_Q11',
          'Researcher_Cats',
          'Language')
label = c('Degree','Occupation', 'Research Experience','Gender','Race','LGBTQ+','Recruitment Source','Field',
          'Coding Language')
rcats[, Round := fcase(Q2 == 'The first replication task','Round 1',
                       Q2 == 'The second replication task','Round 2',
                       Q2 == 'The third replication task','Round 3')]
# Only if round 3 completed
rcats[, did3 := max(Round == 'Round 3'), by = Q1]
rcats = rcats[did3 == TRUE]
rcats[, deviation := abs(Revision_of_Q4-mean(Revision_of_Q4)), 
      by = Round]

# Researhcer predictor strength
build_row = function(pred, dv = 'Revision_of_Q4') {
  fmla = as.formula(paste0(dv,' ~ ',preds[pred]))
  # Don't include very tiny categories
  rcats[, N := .N, by = c('Round',preds[pred])]
  m = feols(fmla, data = rcats[N > 5], split = ~Round)
  dt = data.table(Predictor = label[pred])
  for (i in 1:3) {
    newnames = paste(paste0('R',i),c('F','p','R2'))
    dt[, (newnames) := list(
      fitstat(m[[i]], 'f')$f$stat,
      fitstat(m[[i]], 'f')$f$p,
      fitstat(m[[i]],'r2')$r2
    )]
  }
  return(dt)
}

# Researcher predictor tables with different dependent variables
res_tab = 1:length(preds) |>
  lapply(build_row) |>
  rbindlist()
tonom = names(res_tab)[2:10]
res_tab[, (tonom) := lapply(.SD, label_number(.001)), .SDcols = tonom]
setnames(res_tab, c('Predictor','R1: F','p','R2',
                    'R2: F','p','R2',
                    'R3: F','p','R2'))
res_tab_effect = copy(res_tab)

res_tab = 1:length(preds) |>
  lapply(build_row, dv = 'deviation') |>
  rbindlist()
tonom = names(res_tab)[2:10]
res_tab[, (tonom) := lapply(.SD, label_number(.001)), .SDcols = tonom]
setnames(res_tab, c('Predictor','R1: F','p','R2',
                    'R2: F','p','R2',
                    'R3: F','p','R2'))
res_tab_deviation = copy(res_tab)

# By coding language
rcats[, `Abs. Deviation from Sample Mean` := factor(fcase(
  deviation < .05, '< .05',
  deviation < .1, '.05-.1',
  !is.na(deviation), '> .1'
), levels = c('< .05', '.05-.1','> .1'))]
langdev = rcats[, .(N = .N), by = .(Round, Language, `Abs. Deviation from Sample Mean`)]
# no observations
langdev = rbind(langdev,
                data.table(Round = 'Round 3',
                           Language = 'R',
                           N = 0,
                           `Abs. Deviation from Sample Mean` = '.05-.1'))
langdev[, Share := N/sum(N), by = .(Round, Language)]
langdev[, label := paste0(percent(Share, .1), '\n(', N,')')]
p_deviations_by_language = ggplot(langdev[Language %in% c('R','Stata')], aes(x = `Abs. Deviation from Sample Mean`, y = Share, color = Language, fill = Language,
                                                  label = label)) + 
  geom_col(alpha = .4, position = 'dodge') + 
  geom_text(family = 'serif', vjust = -.2, position = position_dodge(.9), show.legend = FALSE) +
  scale_color_manual(values = colorpal) +
  scale_fill_manual(values = colorpal) +
  scale_y_continuous(labels = label_percent(1), limits = c(0,1.1)) +
  facet_wrap(~Round, ncol = 2) + 
  theme_nick() + 
  labs(y = 'Share',
       x = 'Abs. Deviation from Sample Mean')