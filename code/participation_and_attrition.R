# Attrition Counts
orig_num = dat[is.na(Q2) & Q1 == 0, .N] + dat[is.na(Q2) & Q1 != 0, uniqueN(Q1)] + uniqueN(dat[!is.na(Q2),Q1])
neverfin = dat[Q1 != 0,uniqueN(Q1)]
justcount = dat[!(str_detect(Q2, '\\('))]
justcount = justcount[, .(Participants = uniqueN(Q1)), by = .(Round = Q2)]
justcount = rbind(data.table(Round = c('Original Signup','Assigned Task 1'),
                             Participants = c(orig_num, neverfin)),
                  justcount)
justcount[, Attrition := percent(1-shift(Participants,-1)/Participants, .01)]
justcount[.N, Attrition := '']

# Payment ordering RDD
eo = import('../raw_data/email_order.xlsx')
moneyatt = dat[Q1 != 0]
moneyatt = moneyatt[, .(Finished = max(!is.na(Q2))), by = Q1]
setnames(moneyatt, "Q1",'respondent_id')
moneyatt = merge(moneyatt, eo, by = 'respondent_id')
moneyatt[, Stage := factor(fcase(
  order <= 200, 'Assured',
  order <= moneyatt[respondent_id > 10000, min(order)], 'Not Assured',
  !is.na(order), 'Late'
), levels = c('Assured','Not Assured','Late'))]
library(rdrobust)
m = feols(Finished ~ Stage, data = moneyatt)
m2 = feols(Finished ~ I(order-200)*Stage, data = moneyatt)
#etable(m, m2)

# Researcher data prep
dat[, Researcher_Q10 := as.character(Researcher_Q10)]
dat[Researcher_Q10 == "Professional or graduate degree other than master's or PhD", Researcher_Q10 := 'Prof. Degree']
dat[Researcher_Q10 == "Some graduate school, but no graduate degree", Researcher_Q10 := 'Some Grad School']

# Create research field categories
dat[, Researcher_Cats := factor(fcase(Researcher_Q13 %like% 'Immigration' & Researcher_Q13 %like% 'Labor', 'Immigation & Labor',
                                      Researcher_Q13 %like% 'Immigration', 'Immigration',
                                      Researcher_Q13 %like% 'Labor', 'Labor',
                                      !is.na(Researcher_Q13),'Neither'),
                                levels = c('Immigration & Labor','Immigration','Labor','Neither'))]

fullset = copy(dat)
fullset[Q1 == 0, Q1 := (1:.N) + 100000]
fullset = fullset[, lapply(.SD, last), by = Q1]

demogdat = dat[Q2 %in% c('The first replication task', 'The third replication task')]
tokeep = c('Q1', names(dat)[names(dat) %like% 'Researcher_'], 'Revision_of_Q4')
tokeep2 = c('Q1', 'Q2', names(dat)[names(dat) %like% 'Researcher_'], 'Revision_of_Q4')
alldemog = rbindlist(list(
  unique(subset(fullset, select = tokeep))[, Q2 := 'Original Signup'],
  unique(subset(dat[Q1 != 0], select = tokeep))[, lapply(.SD, last), by = Q1][, Q2 := 'Assigned Task 1'],
  unique(subset(demogdat, select = tokeep2))
), use.names = TRUE)
qrecode(alldemog, 'Q2',c('Assigned Task 1','Original Signup',
                         'The first replication task',
                         'The third replication task'),
        c('Assigned task 1',
          'Original signup',
          'Finished task 1',
          'Finished task 3'))
alldemog[, Round := factor(Q2, levels = c('Original signup',
                                          'Assigned task 1',
                                          'Finished task 1',
                                          'Finished task 3'))]
alldemog[, Researcher_Q10 := factor(Researcher_Q10,
                                    levels = c('No graduate school',
                                               'Some Grad School',
                                               'Master\'s degree',
                                               'Prof. Degree',
                                               'PhD'))]
qrecode(alldemog, 'Researcher_Q6',
        c('Faculty','Graduate student',
          'Non-faculty researcher at a university', 
          'Other (describe)',
          'Private-sector researcher',
          'Public-sector researcher not at a university'),
        c('Faculty','Grad. Student',
          'Other Researcher','Other','Other Researcher','Other Researcher'))

alldemog[, Q8Recode := fcase(
  Researcher_Q8 == 'I have 1-5 papers in applied microeconomics published or accepted for publication', '1-5 Papers in Applied Micro',
  Researcher_Q8 == 'I have 6+ papers in applied microeconomics published or accepted for publication', '6+ Papers',
  Researcher_Q8 == 'I have never performed academic research in applied microeconomics', 'No Academic Papers',
  Researcher_Q8 == 'I have written at least one academic paper in applied microeconomics, but none of this work is published or accepted for publication', 'No Published Academic Papers'
)]
alldemog[, RaceRecode := Researcher_Q16]
alldemog[str_detect(RaceRecode, ',') | RaceRecode == 'Other', RaceRecode := 'Other or Multiracial']
alldemog[, RaceRecode := factor(RaceRecode,
                                levels = c('White','Asian','Black or African American','Hispanic','Other or Multiracial'))]

alldemog[, Researcher_Q11 := factor(Researcher_Q11,
                                    levels = c('Social media', 'Department email','Email of a professional organization','Other'))]
alldemog[, Researcher_Q17 := factor(Researcher_Q17, 
                                    levels = c('Yes','No','Prefer not to say'))]
# Coding languages
lang = import('../raw_data/replication-coding-language.xlsx', setclass = 'data.table')
setnames(lang, c('Researcher ID','Language Used'),c('Q1','Language'))
lang = lang[Q1 != 322]
lang[, `Replication Task` := NULL]
lang = rbind(lang, data.table(Q1 = 322, Language = 'R/Stata'))
dat = merge(dat, lang, by = 'Q1', all.x = TRUE)
alldemog = merge(alldemog, lang, by = 'Q1', all.x = TRUE)

# This person's submissions judged to not satisfactorily complete the tasks
dat = dat[Q1 != 972]