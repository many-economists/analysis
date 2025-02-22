sampdat = import('../data/sample_restrictions_handcoded.xlsx', setclass = 'data.table')
# columns to correct
corrcolumns = names(sampdat)[names(sampdat) %like% 'CORRECT'] |>
  str_replace('CORRECT_', '')
for (cc in corrcolumns) {
  sampdat[!is.na(eval(pp('CORRECT_',cc))), (cc) := eval(pp('CORRECT_',cc))]
}

sampdat[limAll_HISPAN == 'HIspanic-Mexican', limAll_HISPAN := 'Hispanic-Mexican']
sampdat[limTreat_CITIZEN == 'Non-Cit or Natlzd Post-2012', limTreat_CITIZEN := 'Non-Cit or Natlzd post-2012']
sampdat[limAll_CITIZEN == 'Non-Cit or Natlzd Post-2012', limAll_CITIZEN := 'Non-Cit or Natlzd post-2012']
sampdat[limAll_AGE_AT_MIGRATION == 'Multistep Condition', limAll_AGE_AT_MIGRATION := 'Other']
sampdat[limAll_AGE_IN_2012 %in% c('Multistep Condition','Other'), limAll_AGE_IN_2012 := 'Other']

# REVISION AT END: Get rid of "Multistep Condition" and group it in with "Other"
# We do this because we are not reporting the non-treated group definitions
# and multistep was really only popular for that
# Loop over all character variables in sampdat
for (cc in names(sampdat)[sapply(sampdat, is.character)]) {
  sampdat[sampdat[[cc]] == 'Multistep Condition', (cc) := 'Other']
}

# Note these levels only appear levels that appear in All or Treat
sampdat[, limAll_HISPAN := factor(limAll_HISPAN, levels = c( "Hispanic-Mexican","Hispanic-Any", "Hispanic-Mex or Mex-Born", "None"))]
sampdat[, limTreat_HISPAN := factor(limTreat_HISPAN, levels = c( "Hispanic-Mexican","Hispanic-Any", "Hispanic-Mex or Mex-Born", "None"))]

sampdat[, limAll_BPL := factor(limAll_BPL, levels = c("Mexican-Born", "Hispanic-Mex or Mex-Born", "Non-US Born", "Central America-Born",    "None"))]
sampdat[, limTreat_BPL := factor(limTreat_BPL, levels = c("Mexican-Born", "Hispanic-Mex or Mex-Born", "Non-US Born", "Central America-Born",    "None"))]

sampdat[, limAll_CITIZEN := factor(limAll_CITIZEN, levels = c("Non-Citizen", "Foreign-Born", "Non-Cit or Natlzd post-2012",   "Other" ,  "None"))]
sampdat[, limTreat_CITIZEN := factor(limTreat_CITIZEN, levels =c("Non-Citizen", "Foreign-Born", "Non-Cit or Natlzd post-2012",  "Other" ,  "None"))]

sampdat[, limAll_AGE_AT_MIGRATION := factor(limAll_AGE_AT_MIGRATION, levels = c("< 16", "<= 16", "Other", "None"))]
sampdat[, limTreat_AGE_AT_MIGRATION := factor(limTreat_AGE_AT_MIGRATION, levels = c("< 16", "<= 16",  "Other", "None"))]

sampdat[, limAll_AGE_IN_2012 := factor(limAll_AGE_IN_2012, levels = c("Year-Quarter Age", "Year-Only Age", "Other","None"))]
sampdat[, limTreat_AGE_IN_2012 := factor(limTreat_AGE_IN_2012, levels = c("Year-Quarter Age", "Year-Only Age", "None"))]

sampdat[, limAll_YRIMMIG := factor(limAll_YRIMMIG, levels = c("< 2007", "<= 2007", "< 2012",  "<= 2012",  "Any Year", "Other", "None"))]
sampdat[, limTreat_YRIMMIG := factor(limTreat_YRIMMIG, levels = c("< 2007", "<= 2007", "< 2012",  "<= 2012", "Any Year", "Other", "None"))]

sampdat[, limAll_EDUCVET := factor(limAll_EDUCVET, levels = c("HS Grad or Veteran", "12th Grade or Veteran", "HS Grad", "HS Grad or Non-Veteran",  "Other", "None"))]
sampdat[, limTreat_EDUCVET := factor(limTreat_EDUCVET, levels = c("HS Grad or Veteran", "12th Grade or Veteran", "HS Grad", "HS Grad or Non-Veteran", "Other", "None"))]

sampdat[, limAll_YRSUSA := factor(limAll_YRSUSA, levels = c("Used YRSUSA", "No YRSUSA"))]
sampdat[, limTreat_YRSUSA := factor(limTreat_YRSUSA, levels = c("Used YRSUSA", "No YRSUSA"))]

# This variant for the bimodality section
vars_to_consider = c('HISPAN','BPL','CITIZEN','AGE_AT_MIGRATION','AGE_IN_2012','YRIMMIG','EDUCVET','YRSUSA')
titles = c('Hispanic','Birthplace','Citizenship','Age at Migration',
           'Age in June 2012','Year of Immigration','Education/Veteran','Years Continuous in USA')
r12samp = sampdat[Round %in% c('Task 1','Task 2')]
r12samp = rbind(r12samp[, .SD, .SDcols = c(
  'Round',
  paste0('limAll_',vars_to_consider),
  'Revision_of_Q4',
  'Revision_of_Q6',
  'Revision_of_Q12',
  'Q1'
)][, Sample := 'All'],
r12samp[, .SD, .SDcols = c(
  'Round',
  paste0('limTreat_',vars_to_consider),
  'Revision_of_Q4',
  'Revision_of_Q6',
  'Revision_of_Q12',
  'Q1'
)][, Sample := 'Treated'],
use.names = FALSE)
setnames(r12samp, c('Round',titles,'Effect','SE','Sample Size','Q1','Sample'))
r12samp[, `Round/Sample` := paste(Round,Sample)]
r12samp_w_q1 = copy(r12samp)

basic_samp_limitations = sampdat[!(Round %like% 'Revision') & Round != 'Task 3']

## single-characteristic tables
one_characteristic_tab = function(var,sdat) {
  sstats = sdat[, .(a = quantile(Effect[Sample == 'Treated'], .25),
                    b = quantile(Effect[Sample == 'Treated'], .5, na.rm = TRUE),
                    c = quantile(Effect[Sample == 'Treated'], .75, na.rm = TRUE),
                    d = quantile(Effect[Sample == 'All'], .25),
                    e = quantile(Effect[Sample == 'All'], .5, na.rm = TRUE),
                    f = quantile(Effect[Sample == 'All'], .75, na.rm = TRUE),
                    
                    g = quantile(`Sample Size`[Sample == 'All'], .25, na.rm = TRUE),
                    h = quantile(`Sample Size`[Sample == 'All'], .5, na.rm = TRUE),
                    i = quantile(`Sample Size`[Sample == 'All'], .75, na.rm = TRUE)), by = var]
  setorderv(sstats, var)
  sstats = sstats[!is.na(sstats[[var]])]
  sstats[, (var) := lapply(.SD, \(x) paste('...', x)), .SDcols = var]
  sstats[, (letters[1:6]) := lapply(.SD, label_number(.001)), .SDcols = letters[1:6]]
  sstats[, (letters[7:9]) := lapply(.SD, label_number(1, big.mark = ',')), .SDcols = letters[7:9]]
  setnames(sstats, var,'var')
  sstats = rbind(data.table(var = var),
                 sstats, fill = TRUE)
  cn = names(sstats)
  sstats[, (cn) := lapply(.SD, \(x) fifelse(is.na(x),'',x))]
  return(copy(sstats))
}

make_efftab = function(sdat, title, anchor) {
  efftab = lapply(titles, one_characteristic_tab, sdat = sdat) |>
    rbindlist()
  efftab = rbind(data.table(var = 'HEADERROW',
                            a = '\\multicolumn{3}{c}{Treated-Group Restriction}',
                            b = 'DELETECELL',
                            c = 'DELETECELL',
                            d = '\\multicolumn{6}{c}{All-Sample Restriction}',
                            e = 'DELETECELL',
                            f = 'DELETECELL',
                            g = 'DELETECELL',
                            h = 'DELETECELL',
                            i = 'DELETECELL'),
                 efftab)
  setnames(efftab,
           c('Variable','Effect Pctl. 25','Pctl. 50','Pctl. 75',
             'Effect Pctl. 25','Pctl. 50','Pctl. 75',
             'Samp Size Pctl. 25','Pctl. 50','Pctl. 75'))
  return(efftab)
}