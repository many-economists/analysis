# Create long data set indicating controls, effects
split_controls = function(i) {
  single = condat[i]
  controls = str_split(single$Revision_of_Q23, ',')[[1]] |>
    str_trim()
  data.table(Q1 = single$Q1,
             Round = single$Round,
             Effect = single$Revision_of_Q4,
             SE = single$Revision_of_Q6,
             Controls = controls)
}

source('../code/control_var_fixdat.R')

condat = copy(dat)

for (i in 1:nrow(condat)) {
  condat[Revision_of_Q23 == fixdat$original[i], Revision_of_Q23 := fixdat$new[i]]
}

condat[, Revision_of_Q23 := str_replace_all(Revision_of_Q23, fixed('‚Äì'),'-')]

# dat[Revision_of_Q23 %like% '4))', Revision_of_Q23]

condat = condat[!is.na(Q2)]
condat[, did3 := max(Q2 == 'The third replication task', na.rm = TRUE), by = Q1]
condat = condat[did3 == TRUE]
condat = condat[Round %in% paste('Task',1:3)]

condat = 1:nrow(condat) |>
  lapply(split_controls) |>
  rbindlist()
condat[, Controls := str_to_upper(Controls)]

condat = condat[!(Controls %like% '\\?\\?\\?')]
condat = condat[!(Controls == '(BIRTHYR > 1981) | (BIRTHYR == 1981 & BIRTHQTR >= 3)')]

# Find the most-common controls 
allcons = condat$Controls
allcons = sapply(allcons, \(x) (x |>
                                  # Get rid of punctuation but not underscores
                                  str_replace_all('_','USCCC') |>
                                  str_replace_all('[\\W0-9]',' ') |>
                                  str_squish() |>
                                  str_split(' '))[[1]]) |>
  unlist() |>
  unname() |>
  str_replace_all('USCCC','_')
# I is just for I() or i.
allcons = allcons[allcons != 'I']
# See the most common
# sort(table(allcons), decreasing = TRUE)
# 50 or more:
# YEAR, SEX, STATE, AGE, AGE_IN_2012,
# EDUC, MARST, UNEMP, RACE, LFPR, State policies
# SPEAKENG, AGE_AT_MIGRATION, YRIMMIG, YRSUSA

#export(data.table(Controls = sort(unique(condat$Controls))), 'controls_recoding.xlsx')
conmap = import('../data/controls_recoding.xlsx', setclass = 'data.table')
conmap[is.na(Broad) & !is.na(Recode), Broad := Recode]


# THese were erroneously grouped on one line
split_and = function(s) {
  tofix = condat[Controls == s]
  newcon = str_split(tofix$Controls, '&')[[1]] |> str_trim()
  tofix = data.table(Q1 = tofix$Q1,
                     Round = tofix$Round,
                     Effect = tofix$Effect,
                     SE = tofix$SE,
                     Controls = newcon)
  return(tofix)
}

for (i in c(
  "I.YEAR & I.STRATA",
  "I.STATE:I.YEAR & I. MARST & I. EDUCD & I.SEX",
  "I.STATE:I.YEAR & I.MARST & I.SEX & I.EDUC"
)) {
  condat = rbind(condat,split_and(i))
  condat = condat[Controls != i]
}

condat = merge(condat, conmap, by = 'Controls', all.x = TRUE)
condat[is.na(Recode), Recode := '']

# This was included in the broadening category for later use
# But wasn't actually a 50+
condat[, Broad := str_replace_all(Broad,'DISABILITY','OTHER')]
# Included on its own by accident
condat[, Broad := str_replace_all(Broad,'HISPAN','OTHER')]
condat[, Broad := str_replace_all(Broad,'HHTYPE','OTHER')]

condat = unique(condat, by = c('Q1','Round','Effect','Broad'))

# Don't care about transformations or interactions
makebroader = function(s) {
  s = str_trim(str_split(s, ' ')[[1]])
  s = s[s != 'INTERACTION']
  s[str_detect(s,'\\(')] = str_extract(s[str_detect(s,'\\(')], "\\((.*?)\\)")
  s = str_replace_all(s,'\\(|\\)','')
  s = s[s != 'NA']
  s[str_sub(s,1,4) == 'RACE'] = 'RACE'
  s = paste(s, collapse = ' ')
  return(s)
}
condat[, Broader := sapply(Broad, makebroader)]
# List of individual controls
conlist = unique(condat$Broader)
conlist = conlist[!(str_detect(conlist,' '))]
conlist = conlist[!(conlist %in% c('NA','')) & !is.na(conlist)]

# Check the relationship between controls and the effect
avg_per_control = function(c) {
  cdat = condat[Broader %like% c,
                .(Q1, Round, Effect, SE)]
  # AGE would pick up AGE_IN_2012 and AGE_AT_MIGRATION
  if (c == 'AGE') {
    cdat = condat[Broader %like% c & !(Broader %like% 'AGE_IN_2012') & !(Broader %like% 'AGE_AT_MIGRATION'),
                  .(Q1, Round, Effect, SE)]
  }
  cdat = unique(cdat)
  cdat[, Control := c]
  return(copy(cdat))
}
allcontrols = conlist |>
  lapply(avg_per_control) |>
  rbindlist()

# Ones with nothing!
nocons = dat[Round %in% c('Task 1','Task 2','Task 3'), .(Q1, Round, Effect = Revision_of_Q4,
                                                         SE = Revision_of_Q6)]
nocons = nocons[!condat[, .(Q1, Round)], on = c('Q1','Round')]
nocons[, Control := 'None']
allcontrols = rbind(allcontrols, nocons)



###


# Variation within versions of a control
# Find which controls have meaningful inclusion-type variation
vars_with_variation = conlist |>
  sapply(\(x) {
    tabl = condat[Broader %like% x, table(Broad)]
    tabl[tabl >= 20]
  })
vars_with_variation = list(
  'STATE/YEAR'= c('INTERACTION STATE YEAR', 'INTERACTION FE(YEAR) STATE', 'YEAR', 'FE(YEAR)', 'STATE'),
  'AGE'= c('AGE', 'FE(AGE)', 'SQ(AGE)'),
  'EDUC'= c('EDUC', 'FE(EDUC)', 'T(EDUC)')
)
# A copy we can manipulate to account for overlaps
trans_con = copy(condat)
trans_con[, keep := FALSE]
trans_con[, category := NA_character_]
for (v in 1:length(vars_with_variation)) {
  for (v2 in vars_with_variation[[v]]) {
    trans_con[Broad == v2, keep := TRUE]
    trans_con[Broad == v2, category := names(vars_with_variation)[v]]
  }
}
trans_con = trans_con[(keep)]
# Combine state/year into one
# Widen to handle overlaps
trans_con = dcast(trans_con[, .(Q1, Round, Effect, SE, Broad, category, keep)], Q1+Round+Effect+SE+category~Broad,
                  value.var = 'keep')
indicators = names(trans_con)[6:16]
idvars = names(trans_con)[1:5]
trans_con[, (indicators) := lapply(.SD, \(x) fifelse(is.na(x),FALSE,x)), .SDcols = indicators]
trans_con[(`SQ(AGE)`), AGE := FALSE]
trans_con[(`FE(AGE)`), AGE := FALSE]
trans_con[(`FE(AGE)`), `SQ(AGE)` := FALSE]
trans_con[(`FE(EDUC)`), EDUC := FALSE]
trans_con[(`T(EDUC)`), EDUC := FALSE]
trans_con[(`FE(EDUC)`),`T(EDUC)` := FALSE]
tozero = c('YEAR','STATE','FE(YEAR)')
trans_con[(`INTERACTION FE(YEAR) STATE` | `INTERACTION STATE YEAR`), (tozero) := lapply(.SD, \(x) FALSE), .SDcols = tozero]
trans_con[(`FE(YEAR)`), YEAR := FALSE]
# Only remaining overlap should be that sometimes you have both STATE and YEAR controls but without interaction
trans_con = melt(trans_con, id.vars = idvars,
                 variable.name = 'Control', value.name = 'Included')[(Included)]
# Only remaining overlap should be that sometimes you have both STATE and YEAR controls but without interaction
# trans_con[, .(N = .N, whichones = paste(unique(Control), collapse = ', ')), by = .(Q1, Round, category)][N>1, table(whichones)]
# FE(YEAR), STATE     STATE, YEAR 
#             63              60 
# Change the names
fixdat = data.table(Control = c(
  "AGE", "EDUC", "FE(AGE)", "FE(EDUC)", "FE(YEAR)" ,"INTERACTION FE(YEAR) STATE","INTERACTION STATE YEAR","SQ(AGE)","STATE","T(EDUC)","YEAR"    
), Relabel = factor(c(
  "Linear Age", "Linear Education", "Age FE", "Education FE", "Year FE" ,"State FE x Year FE","State FE x Linear Year","Age Quadratic","State FE","Education Transform","Linear Year"
),
levels = c(
  "Linear Age", "Age FE","Age Quadratic", "Linear Education",  "Education FE","Education Transform","Linear Year", "Year FE" ,"State FE","State FE x Year FE","State FE x Linear Year"
)))
trans_con = merge(trans_con, fixdat, by = 'Control')


allcontrols[, Total := uniqueN(Q1), by = Round]
controllevs = sort(unique(allcontrols$Control))
controllevs = c(controllevs[controllevs != 'None'],'None')