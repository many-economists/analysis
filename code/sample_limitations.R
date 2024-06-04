#source('../code/clean_sample_selection.R')
#save(sampdat, file = 'temp_sampdat.Rdata')
load('../data/temp_sampdat.Rdata')

# Count the number of variables used in sample selection
count_vars = function(s) {
  s = str_replace_all(s, '[^a-zA-Z]',' ')
  s = str_squish(s)
  s = str_split(s,' ')[[1]]
  s = s[!(s %in% c('BETWEEN','AND','MIN','MAX','I','X','FE','NS','OF','N','A','','IN','ABS'))]
  return(uniqueN(s))
}
count_varsV = Vectorize(count_vars, 's')

sampdat[, `Whole Sample` := count_varsV(AllLim)]
sampdat[, `Treated Group` := count_varsV(TreatLim)]
sampdat[, `Untreated Group` := count_varsV(UnTreatLim)]

basic_samp_limitations = sampdat[!(Round %like% 'Revision') & Round != 'Task 3']

## variable selections in sample limitations
vars_to_consider = c('HISPAN','BPL','CITIZEN','AGE_AT_MIGRATION','AGE_IN_2012','YRIMMIG','EDUCVET','YRSUSA')
titles = c('Hispanic','Birthplace','Citizenship','Age at Migration',
           'Age in June 2012','Year of Immigration','Education/Veteran','Years Continuous in USA')
# Simplifying some of the coding
remapper = list(
  CITIZEN = data.table(orig = c("Citizen", "Foreign-Born", "Multistep Condition", "Natural-Born Citizen", "Naturalized Citizen", "Non-Citizen", "Non-Citizen or Naturalized", "Non-Citizen or Naturalized After 2012", "None", "Other",""), new = c("Citizen (various)", "Foreign-Born", "Multistep Condition", "Citizen (various)", "Citizen (various)", "Non-Citizen", "Other", "Non-Cit or Natlzd post-2012", "None", "Other","None")),
  AGE_AT_MIGRATION = data.table(orig = c("< 15", "< 16", "<= 16", "> 16", "Any", "Multistep Condition", "None", "Not 16", "Other","", "> 16","Any Age","Multistep Condition"), new = c("Other", "< 16", "<= 16", "> 16", "Any Age", "Multistep Condition", "None", "Other", "Other","None", "Other", "Other", "Other")),
  YRIMMIG = data.table(orig = c("< 2007", "< 2012", "<= 2007", "<= 2012", "> 2012", ">= 2007", ">= 2012", "Any Year", "Multistep Condition", "None", "Not 2007", "Not 2012", "Other"), new = c("< 2007", "< 2012", "<= 2007", "<= 2012", "Other", ">= 2007", "Other", "Any Year", "Multistep Condition", "None", "Other", "Other", "Other")),
  EDUCVET = data.table(orig = c("", "12th Grade", "12th Grade or Veteran", "HS Grad", "HS Grad and Non-Veteran", "HS Grad and Veteran", "HS Grad or In School", "HS Grad or Non-Veteran", "HS Grad or Veteran", "HS Grad or Veteran or In School", "None", "Other", "Other Education", "Other Education or Non-Veteran", "Other Education or Veteran","Multistep Condition","HS Grad or In School"), new = c("None", "12th Grade or Veteran", "12th Grade or Veteran", "HS Grad", "Other", "Other", "Other", "HS Grad or Non-Veteran", "HS Grad or Veteran", "Other", "None", "Other", "Other", "Other", "Other","Other","Other")),
  HISPAN = data.table(orig = c('Hispanic-Mex or Birthplace-Mex'),
                      new = c('Hispanic-Mex or Mex-Born')),
  BPL = data.table(orig = c('Hispanic-Mexican or Mexican-Born'),
                   new = c('Hispanic-Mex or Mex-Born'))
)

for (var in names(remapper)) {
  for (i in 1:nrow(remapper[[var]])) {
    qrecode(sampdat, paste0('limAll_',var),
            remapper[[var]]$orig,
            remapper[[var]]$new,
            checkfrom = FALSE)
    qrecode(sampdat, paste0('limTreat_',var),
            remapper[[var]]$orig,
            remapper[[var]]$new,
            checkfrom = FALSE)
    qrecode(sampdat, paste0('limUn_',var),
            remapper[[var]]$orig,
            remapper[[var]]$new,
            checkfrom = FALSE)
  }
}

# Note these levels only appear levels that appear in All or Treat
sampdat[, limAll_HISPAN := factor(limAll_HISPAN, levels = c( "Hispanic-Mexican","Hispanic-Any", "Hispanic-Mex or Mex-Born", "Multistep Condition", "None"))]
sampdat[, limTreat_HISPAN := factor(limTreat_HISPAN, levels = c( "Hispanic-Mexican","Hispanic-Any", "Hispanic-Mex or Mex-Born", "Multistep Condition", "None"))]

sampdat[, limAll_BPL := factor(limAll_BPL, levels = c("Mexican-Born", "Hispanic-Mex or Mex-Born", "Non-US Born", "Central America-Born",    "None"))]
sampdat[, limTreat_BPL := factor(limTreat_BPL, levels = c("Mexican-Born", "Hispanic-Mex or Mex-Born", "Non-US Born", "Central America-Born",    "None"))]

sampdat[, limAll_CITIZEN := factor(limAll_CITIZEN, levels = c("Non-Citizen", "Foreign-Born", "Non-Cit or Natlzd post-2012", "Citizen (various)",  "Multistep Condition", "Other" ,  "None"))]
sampdat[, limTreat_CITIZEN := factor(limTreat_CITIZEN, levels =c("Non-Citizen", "Foreign-Born", "Non-Cit or Natlzd post-2012", "Citizen (various)",  "Multistep Condition", "Other" ,  "None"))]

sampdat[, limAll_AGE_AT_MIGRATION := factor(limAll_AGE_AT_MIGRATION, levels = c("< 16", "<= 16", "Other", "None"))]
sampdat[, limTreat_AGE_AT_MIGRATION := factor(limTreat_AGE_AT_MIGRATION, levels = c("< 16", "<= 16", "> 16", "Any Age", "Multistep Condition", "Other", "None"))]

sampdat[, limAll_AGE_IN_2012 := factor(limAll_AGE_IN_2012, levels = c("Year-Quarter Age", "Year-Only Age", "None"))]
sampdat[, limTreat_AGE_IN_2012 := factor(limTreat_AGE_IN_2012, levels = c("Year-Quarter Age", "Year-Only Age", "None"))]

sampdat[, limAll_YRIMMIG := factor(limAll_YRIMMIG, levels = c("< 2007", "<= 2007", "< 2012",  "<= 2012", ">= 2007", "Any Year", "Multistep Condition", "Other", "None"))]
sampdat[, limTreat_YRIMMIG := factor(limTreat_YRIMMIG, levels = c("< 2007", "<= 2007", "< 2012",  "<= 2012", ">= 2007", "Any Year", "Multistep Condition", "Other", "None"))]

sampdat[, limAll_EDUCVET := factor(limAll_EDUCVET, levels = c("HS Grad or Veteran", "12th Grade or Veteran", "HS Grad", "HS Grad or Non-Veteran",  "Other", "None"))]
sampdat[, limTreat_EDUCVET := factor(limTreat_EDUCVET, levels = c("HS Grad or Veteran", "12th Grade or Veteran", "HS Grad", "HS Grad or In School", "HS Grad or Non-Veteran", "Other", "None"))]

sampdat[, limAll_YRSUSA := factor(limAll_YRSUSA, levels = c("Used YRSUSA", "No YRSUSA"))]
sampdat[, limTreat_YRSUSA := factor(limTreat_YRSUSA, levels = c("Used YRSUSA", "No YRSUSA"))]

# Since sample restrictions are mostly null for Task 3, do this for only task 2, and only for All and Treat
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
r12samp[, Q1 := NULL]


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