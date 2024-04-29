{
  library(data.table)
  library(stringr)
  library(nicksshorts)
  library(rio)
  library(vtable)
}

dat = import('../Research Survey as of 102623.xlsx')

# Variable key
vtab = data.table(varname = names(dat),
                desc = as.vector(dat[1,]))
export(vtab, 'survey_variable_key.csv')

dat = import('../Research Survey as of 102623.xlsx', skip = 2, setclass = 'data.table')
setnames(dat, vtab$varname)

# Direct fixes requested via email
dat[Q1 == 98264 & Q2 == 'The first replication task', `Revision_of_Q4` := 0.061913]
dat[Q1 == 870, Q1 := 807]
dat[Q1 == 805, Q1 := 815]
dat[Q1 == 770 & Q2 == 'Revision following the third replication task (such as following peer review)' & `Duration (in seconds)` == 176, Q2 := 'The third replication task']

dat[,Revision_of_Q18 := as.character(Revision_of_Q18)]

# Fixes from the survey-corrections survey
correct = import('../SurveyCorrections.xlsx', setclass = 'data.table')
setnames(correct, c('Timestamp',
                    'QID','Fixed',
                    'Survey_Response',
                    'Explanation',
                    'NeedsRevision','notes'))
# Did not give QID information
correct[, num_under := str_count(QID,'_')]
correct = correct[num_under == 4]
# If there's nothing in the fixed version, that means their direct response was sufficient
correct[is.na(Fixed), Fixed := Survey_Response]
# For each correction, apply it
for (i in 1:nrow(correct)) {
  QID = str_split(correct[i, QID],'_')[[1]]
  QID = lapply(QID, str_trim)
  QID[[1]] = paste0(QID[[1]],'_',QID[[2]])
  QID[[4]] = fcase(
    QID[[4]] == 'T1','The first replication task',
    QID[[4]] == 'T2','The second replication task',
    QID[[4]] == 'T3','The third replication task',
    QID[[4]] == 'T1R','Revision following the first replication task (such as following peer review)',
    QID[[4]] == 'T2R','Revision following the second replication task (such as following peer review)',
    QID[[4]] == 'T3R','Revision following the third replication task (such as following peer review)'
  )
  # Check if we have a match
  rowmatch = dat[, which(ResponseId == QID[[1]] & Q1 == QID[[3]] & Q2 == QID[[4]])]
  if (length(rowmatch) != 1) {
    stop(paste0('Found ',length(rowmatch),' matches for ',i))
  }
  if (!(paste0('Revision_of_',QID[[5]]) %in% names(dat))) {
    stop(paste0('Revision_of_',QID[[5]],' not in names'))
  }
  set(dat, rowmatch, paste0('Revision_of_',QID[[5]]),
      correct[i, Fixed])
}

dat[, Revision_of_Q6 := str_trim(Revision_of_Q6)]


# Destringing
dat[, Revision_of_Q4 := as.numeric(Revision_of_Q4)]
dat[, Revision_of_Q6 := as.numeric(Revision_of_Q6)]
dat[, Revision_of_Q12 := as.numeric(Revision_of_Q12)]
dat[, Revision_of_Q18 := as.numeric(Revision_of_Q18)]
dat[Revision_of_Q21 == '10 222\r\n', Revision_of_Q21 := 10222]
dat[, Revision_of_Q21 := as.numeric(Revision_of_Q21)]

table(dat$Revision_of_Q10)
dat[, Recoded_Q10 := NA_character_]
dat[Revision_of_Q10 == "linear regression"                                                    , Recoded_Q10 := 'Linear Regression']
dat[Revision_of_Q10 == "Linear Regression"                                                    , Recoded_Q10 := 'Linear Regression']
dat[Revision_of_Q10 == "linear regression, logit regression"                                  , Recoded_Q10 := 'Linear Regression']
dat[Revision_of_Q10 == "logit regression"                                                     , Recoded_Q10 := 'Logit/Probit']
dat[Revision_of_Q10 == "logit"                                                     , Recoded_Q10 := 'Logit/Probit']
dat[Revision_of_Q10 == "DID:logit regression"                                                 , Recoded_Q10 := 'Logit/Probit']
dat[Revision_of_Q10 == "DID:nonparametric"                                                    , Recoded_Q10 := 'Other']
dat[Revision_of_Q10 == "DID:logit"                                                            , Recoded_Q10 := 'Logit/Probit']
dat[Revision_of_Q10 == "DID:inverse probability weighting"                                    , Recoded_Q10 := 'IPW']
dat[Revision_of_Q10 == "RDD"                                                                  , Recoded_Q10 := 'Linear Regression']
dat[Revision_of_Q10 == "linear regression, DID:LOM"                                           , Recoded_Q10 := 'Linear Regression']
dat[Revision_of_Q10 == "probit"                                                               , Recoded_Q10 := 'Logit/Probit']
dat[Revision_of_Q10 == "DID:Borusyak et al. (2021)'s"                                         , Recoded_Q10 := 'New DID Estimator']
dat[Revision_of_Q10 == "DID:LPM"                                                              , Recoded_Q10 := 'Linear Regression']
dat[Revision_of_Q10 == "DID:probit"                                                           , Recoded_Q10 := 'Logit/Probit']
dat[Revision_of_Q10 == "precision weighted average"                                           , Recoded_Q10 := 'Other']
dat[Revision_of_Q10 == "DID: inverse probability weighting;linear regression"                 , Recoded_Q10 := 'Matching+Regression']
dat[Revision_of_Q10 == "matching, logit"                 , Recoded_Q10 := 'Matching+Regression']
dat[Revision_of_Q10 == "DID: Chaisemartin and D'Haultfoeuille (2020)"                         , Recoded_Q10 := 'New DID Estimator']
dat[Revision_of_Q10 == "Locally efficient doubly robust DiD estimators"                         , Recoded_Q10 := 'New DID Estimator']
dat[Revision_of_Q10 == "inverse probability weighting"                                        , Recoded_Q10 := 'IPW']
dat[Revision_of_Q10 == "LASSO"                                                                , Recoded_Q10 := 'Other']
dat[Revision_of_Q10 == "DID: inverse probability weighting"                                   , Recoded_Q10 := 'IPW']
dat[Revision_of_Q10 == "probit;linear regression"                                             , Recoded_Q10 := 'Logit/Probit']
dat[Revision_of_Q10 == "PSM"                                                                  , Recoded_Q10 := 'PSM']
dat[Revision_of_Q10 == "DID:Callaway and Sant'Anna"                                           , Recoded_Q10 := 'New DID Estimator']
dat[Revision_of_Q10 == "logit; inverse probability weighting"                                 , Recoded_Q10 := 'Matching+Regression']
dat[Revision_of_Q10 == "IPW"                                                                  , Recoded_Q10 := 'IPW']
dat[Revision_of_Q10 == "DID:Borusyak et al. (2023)"                                           , Recoded_Q10 := 'New DID Estimator']
dat[Revision_of_Q10 == "nonparametric regression"                                             , Recoded_Q10 := 'Other']
dat[Revision_of_Q10 == "DID: Woolridge"                                                       , Recoded_Q10 := 'New DID Estimator']
dat[Revision_of_Q10 == "DID:Borusyak et al. (2023), Sun Abraham (2020), linear regression"    , Recoded_Q10 := 'New DID Estimator']
dat[Revision_of_Q10 == "DID: Borusyak Jaravel Spiess (2021)"                                  , Recoded_Q10 := 'New DID Estimator']
dat[Revision_of_Q10 == "probit regression"                                                    , Recoded_Q10 := 'Logit/Probit']
dat[Recoded_Q10 %in% c('IPW','PSM','Matching+Regression'),Recoded_Q10 := 'Matching']

table(dat$Revision_of_Q8)
dat[, Recoded_Q8 := "None"]
dat[Revision_of_Q8 == "cluster:birthyr,yrimmig "                                                , Recoded_Q8 := 'Cluster: Multiple']
dat[Revision_of_Q8 == "cluster:yrimmig,birthyr"                                                 , Recoded_Q8 := 'Cluster: Multiple']
dat[Revision_of_Q8 == "robust"                                                                  , Recoded_Q8 := 'Het-Robust']
dat[Revision_of_Q8 == "cluster:state"                                                           , Recoded_Q8 := 'Cluster: State']
dat[Revision_of_Q8 == "cluster:strata"                                                          , Recoded_Q8 := 'Cluster: Strata']
dat[Revision_of_Q8 == "bootstrap"                                                               , Recoded_Q8 := 'Bootstrap']
dat[Revision_of_Q8 == "cluster:state,year"                                                      , Recoded_Q8 := 'Cluster: State/Yr']
dat[Revision_of_Q8 == "cluster: cluster"                                                        , Recoded_Q8 := 'Cluster: Cluster']
dat[Revision_of_Q8 == "cluster:household"                                                       , Recoded_Q8 := 'Cluster: ID']
dat[Revision_of_Q8 == "robust; cluster:state"                                                   , Recoded_Q8 := 'Cluster: State']
dat[Revision_of_Q8 == "cluster:state, year"                                                     , Recoded_Q8 := 'Cluster: State/Yr']
dat[Revision_of_Q8 == "cluster:county"                                                          , Recoded_Q8 := 'Cluster: Other']
dat[Revision_of_Q8 == "other:sdr"                                                               , Recoded_Q8 := 'Other']
dat[Revision_of_Q8 == "robust; cluster:state, year"                                             , Recoded_Q8 := 'Cluster: State/Yr']
dat[Revision_of_Q8 == "other:Callaway & Sant'Anna (2021) JoE"                                   , Recoded_Q8 := 'New DID Estimator']
dat[Revision_of_Q8 == "cluster: matching_id"                                                    , Recoded_Q8 := 'Cluster: ID']
dat[Revision_of_Q8 == "cluster:treatment"                                                       , Recoded_Q8 := 'Cluster: Treatment']
dat[Revision_of_Q8 == "cluster:?"                                                               , Recoded_Q8 := 'Cluster: Other']
dat[Revision_of_Q8 == "other:\"leave out\" estimate described by Borusyak et al. (2021)."       , Recoded_Q8 := 'New DID Estimator']
dat[Revision_of_Q8 == "other: Donald and Lang (2007) aggregation"                               , Recoded_Q8 := 'Other']
dat[Revision_of_Q8 == "cluster:year"                                                            , Recoded_Q8 := 'Cluster: Year']
dat[Revision_of_Q8 == "cluster:state_of_residence"                                              , Recoded_Q8 := 'Cluster: State']
dat[Revision_of_Q8 == "cluster:cluster"                                                         , Recoded_Q8 := 'Cluster: Cluster']
dat[Revision_of_Q8 == "cluster:X"                                                               , Recoded_Q8 := 'Cluster: Other']
dat[Revision_of_Q8 == "robust; delta method"                                                    , Recoded_Q8 := 'Het-Robust']
dat[Revision_of_Q8 == "robust;cluster:state,year"                                               , Recoded_Q8 := 'Cluster: State/Yr']
dat[Revision_of_Q8 == "other: honest DID"                                                       , Recoded_Q8 := 'New DID Estimator']
dat[Revision_of_Q8 == "cluster:state,year,birth_qtr"                                       , Recoded_Q8 := 'Cluster: Multiple']
dat[Revision_of_Q8 == "cluster:Äòcluster‚Äô"                                                    , Recoded_Q8 := 'Cluster: Cluster']
dat[Revision_of_Q8 == "cluster: IPUMS-survey"                                                   , Recoded_Q8 := 'Cluster: Other']
dat[Revision_of_Q8 == "cluster:strata, state,year"                                              , Recoded_Q8 := 'Cluster: Multiple']
dat[Revision_of_Q8 == "cluster:state,Sant‚ÄôAnna and Zhao (2020)"                               , Recoded_Q8 := 'New DID Estimator']
dat[Revision_of_Q8 == "other:Borusyak et al. (2021)"                                            , Recoded_Q8 := 'New DID Estimator']
dat[Revision_of_Q8 == "cluster:individual"                                                      , Recoded_Q8 := 'Cluster: ID']
dat[Revision_of_Q8 == "cluster:CB serial number"                                                , Recoded_Q8 := 'Cluster: ID']
dat[Revision_of_Q8 == "cluster:treatment group"                                                 , Recoded_Q8 := 'Cluster: Treatment']
dat[Revision_of_Q8 == "other:Donald andLang(2007)"                                              , Recoded_Q8 := 'Other']
dat[Revision_of_Q8 == "cluster:serial number"                                                   , Recoded_Q8 := 'Cluster: ID']
dat[Revision_of_Q8 == "cluster:serial"                                                          , Recoded_Q8 := 'Cluster: ID']
dat[Revision_of_Q8 == "cluster:birthyr"                                                         , Recoded_Q8 := 'Cluster: Age']
dat[Revision_of_Q8 == "cluster:state,year,birth_qtr"                                            , Recoded_Q8 := 'Cluster: Multiple']
dat[Revision_of_Q8 == "BRR"                                                                     , Recoded_Q8 := 'Other']
dat[Revision_of_Q8 == "robust; cluster:state  ; cluster:serialnumber"                           , Recoded_Q8 := 'Cluster: Multiple']
dat[Revision_of_Q8 == "robust; cluster"                                                         , Recoded_Q8 := 'Cluster: Other']
dat[Revision_of_Q8 == "cluster: birth_qtr"                                                      , Recoded_Q8 := 'Cluster: Age']
dat[Revision_of_Q8 == "honestDID"                                                               , Recoded_Q8 := 'New DID Estimator']
dat[Revision_of_Q8 == "wild_bootstrap"                                                          , Recoded_Q8 := 'Bootstrap']
dat[Revision_of_Q8 == "cluster: strata"                                                         , Recoded_Q8 := 'Cluster: Strata']
dat[Revision_of_Q8 == "cluster:state,birthyr"                                                   , Recoded_Q8 := 'Cluster: Multiple']
dat[Revision_of_Q8 == "robust; cluster:treatmentgrp"                                            , Recoded_Q8 := 'Cluster: Treatment']
dat[Revision_of_Q8 == "robust; cluster:year"                                                    , Recoded_Q8 := 'Cluster: Year']
dat[Revision_of_Q8 == "cluster"                                                                 , Recoded_Q8 := 'Cluster: Other']
dat[Revision_of_Q8 == "robust; bootstrap"                                                       , Recoded_Q8 := 'Bootstrap']
dat[Revision_of_Q8 == "cluster:statefip"                                                        , Recoded_Q8 := 'Cluster: State']
dat[Revision_of_Q8 == "Calloway Santananna 2019"                                                , Recoded_Q8 := 'New DID Estimator']
dat[Revision_of_Q8 == "robust; cluster:CB serial number"                                        , Recoded_Q8 := 'Cluster: ID']
dat[Recoded_Q8 %in% c('Cluster: Age','Cluster: Cluster','Cluster: Multiple',
                      'Cluster: Treatment','Cluster: Year'), Recoded_Q8 := 'Cluster: Other']

# Handle multiple submissions
dat[, subnum := 1:.N, by = .(Q1, Q2)]
dat[is.na(Q1), Q1 := '']
dat = dat[!(Q1 == 'adf')]
dat = dat[!(Q1 == '')]
dat[Q2 == 'The first replication task' & Q4 == '1.04', Q1 := '446']
dat[Q2 == 'The first replication task' & Q4 == '0.11' & Q1 == '', Q1 := '649']
dat[, Q1 := as.numeric(Q1)]

# Get the last non-missing value in each row
last_non_missing = function(x) {
  x = x[!is.na(x)]
  if (is.character(x)) {
    x = x[x != '']
  }
  if (length(x) == 0) {
    if (is.integer(x)) {
      return(NA_integer_)
    } else if (is.character(x)) {
      return(NA_character_) 
    } else {
      return(NA_real_)
    }
  }
  x = tail(x, 1)
  return(x)
}
dat = dat[, lapply(.SD, last_non_missing), by = .(Q1, Q2)]


# Researcher characteristics
rch = import('../Signup Info/Econ Sign Up Sheet 10.6.csv', setclass = 'data.table', skip = 1)
rch[, FLAG := NULL]
rch[, NOT_QUALIFIED := NULL]
rch[, email := tolower(Q5_3)]
rch[, Q5_3 := NULL]
# Correct for later email updates
rch[email == 'dnlang86@stanford.edu', email := 'dnlang.ucla@gmail.com']
rch[email == 'stephen.weinberg2@health.my.gov', email := 'stephen.weinberg2@health.ny.gov']
rch[email == 'emunozsaavedra@worldbank.org', email := 'emunozsaavedra@gradcenter.cuny.edu']
rch[email == 'chwatson@fdic.gov', email := 'lukexwatson@gmail.com']
rch[email == 'ilayda.isabetli@istinye.edu.tr', email := 'ilaydaisabetli@hotmail.com']
rch[email == 'imaan.enterprised@gmail.com', email := 'imaan.enterprises@gmail.com']
rch[email == 'ferhatcitak@hitit.edu.tr', email := 'fccitak@gmail.com']
rch[email == 'economist243@gmail.com', email := 'zandlib@yahoo.com']
rch[email == 'gondar03@gmail.com', email := 'hey.china@yahoo.com']
rch = rch[, lapply(.SD, last_non_missing), by = email]

emails = import('../code/email_data.csv', setclass = 'data.table')
emails[, email := tolower(email)]
emails = emails[, .(email, Q1 = respondent_id)]
emails = emails[!(email %like% '\\(duplicate\\)')]
emails[!(email %in% rch$email)]

rch = merge(rch, emails, by = 'email', all.x = TRUE)
rch[is.na(Q1), Q1 := 0]
todrop = c('email','StartDate','EndDate','Status','IPAddress','Progress',
           'Duration (in seconds)','Finished','RecordedDate',
           'ResponseId','RecipientLastName','RecipientFirstName',
           'RecipientEmail','ExternalReference','LocationLatitude',
           'LocationLongitude','DistributionChannel','UserLanguage')
rch[, (todrop) := NULL]
rch[, Q5_1 := NULL]
rch[, Q5_2 := NULL]
rch[, Q5_4 := NULL]
setnames(rch, \(x) paste0('Researcher_',x))
setnames(rch, 'Researcher_Q1','Q1')


rchnames = import('../Signup Info/Econ Sign Up Sheet 10.6.csv', setclass = 'data.table')
rchnames = data.table(varname = as.vector(rchnames[2,]),
                      desc = as.vector(rchnames[1, ]))
rchnames = rchnames[varname %like% 'Q']
rchnames = rchnames[!(varname %like% 'Q5')]
export(rchnames, 'researcher_variable_key.csv')

dat = merge(dat, rch, all = TRUE, by = 'Q1')
todrop = c('StartDate','EndDate','Status','IPAddress',
           'Progress','Duration (in seconds)','Finished',
           'RecordedDate','ResponseId','RecipientLastName',
           'RecipientFirstName','RecipientEmail','ExternalReference',
           'LocationLatitude','LocationLongitude','DistributionChannel',
           'UserLanguage')
dat[, (todrop) := NULL]
export(dat, 'cleaned_survey_post_corrections.parquet')
