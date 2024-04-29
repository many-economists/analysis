pastenulls = function(a, b, c) {
  s = fifelse(is.na(a) | a == 'BLANK','',paste0(a, ' & '))
  s = fifelse(is.na(b) | b == 'BLANK',s,paste0(s, b, ' & '))
  s = fifelse(is.na(c) | c == 'BLANK',s,paste0(s, c))
  if (str_sub(s, -1) == '&') {
    s = str_sub(s, 1, -2)
  }
  return(s)
}
pastenullsV = Vectorize(pastenulls, c('a','b','c'))

sampdat = copy(dat)
sampdat = sampdat[!is.na(Q2)]
sampdat[, did3 := max(Q2 == 'The third replication task', na.rm = TRUE), by = Q1]
sampdat = sampdat[did3 == TRUE]
# Put together the full group definitions
sampdat[, AllLim := str_to_upper(pastenullsV(Revision_of_Q13,
                                             Revision_of_Q14,
                                             NA_character_))]
sampdat[, TreatLim := str_to_upper(pastenullsV(Revision_of_Q13,
                                               Revision_of_Q14,
                                               Revision_of_Q17))]
sampdat[, UnTreatLim := str_to_upper(pastenullsV(Revision_of_Q13,
                                                 Revision_of_Q14,
                                                 Revision_of_Q20))]

sampdat[, AllLim := str_replace_all(AllLim,'BIRHTYR','BIRTHYR')]
sampdat[, TreatLim := str_replace_all(TreatLim,'BIRHTYR','BIRTHYR')]
sampdat[, UnTreatLim := str_replace_all(UnTreatLim,'BIRHTYR','BIRTHYR')]

# GPT-4 drafted, human-edited and expanded, human-tested
# Slow as heck but seems necessary
handling_processing <- function(s, var) {
  origs = s
  if (is.na(s)) {
    return(NA_character_)
  }
  if (nchar(s) == 0) {
    return('')
  }
  logical_statements <- c() # Initialize an empty vector to store logical statements
  
  while (nchar(s) > 0) { # Repeat until the string is blank
    # Check the positions of "&" and "(" to decide how to split the string
    amp_pos <- str_locate(s, "&")[1]
    open_paren_pos <- str_locate(s, "\\(")[1]
    
    if (is.na(amp_pos)) amp_pos <- nchar(s) + 1 # Set amp_pos beyond end of string if "&" is not found
    if (is.na(open_paren_pos)) open_paren_pos <- nchar(s) + 1 # Set open_paren_pos beyond end of string if "(" is not found
    
    # Case 1: "&" occurs before "(" or at the end of the string
    if (amp_pos < open_paren_pos | amp_pos == nchar(s) + 1) {
      logical_statements <- c(logical_statements, substr(s, 1, amp_pos - 1))
      s <- substring(s, amp_pos + 1)
    } else { # Case 2: "(" occurs before "&"
      # Find the matching ")" for the "("
      depth <- 0
      pos <- open_paren_pos
      while (pos <= nchar(s)) {
        if (substr(s, pos, pos) == "(") depth <- depth + 1
        if (substr(s, pos, pos) == ")") depth <- depth - 1
        if (depth == 0) break
        pos <- pos + 1
      }
      # Find the next & or EOS after this position
      amploc = str_locate(substr(s, pos, nchar(s)), "&")[1] + pos - 2
      if (is.na(amploc)) {
        pos = nchar(s)
      } else {
        pos = amploc
      }
      segment <- str_squish(substr(s, open_paren_pos, pos))
      
      if (str_sub(segment,1,1) == '(' & str_sub(segment, -1) == ')' & !str_detect(segment, '\\|')) {
        # Remove the leading "(" and ending ")" and treat it as a regular segment
        s = str_replace(str_squish(s), fixed(segment), str_sub(segment, 2,-2))
        next # Return to the beginning of the loop without removing anything from s
      } else {
        logical_statements <- c(logical_statements, segment)
      }
      
      s <- substring(s, pos + 1)
    }
  }
  
  # Filter the logical statements to only those containing the specified variable
  var_statements <- logical_statements[str_detect(logical_statements, sprintf("\\b%s\\b", var))]
  var_statements = unique(str_squish(var_statements))
  if (length(var_statements) > 1) {
    var_statements = paste0('(',var_statements,')')
    var_statements <- str_squish(paste(var_statements, collapse = ' & '))
  } else if (length(var_statements) == 1) {
    if (str_sub(var_statements,1,1) == '(' & str_sub(var_statements,-1) == ')') {
      var_statements = str_squish(str_sub(var_statements, 2, -2))
    }
  } else {
    return('')
  }
  if (length(var_statements) > 1 | class(var_statements) == 'list') {
    stop(origs)
  }
  
  return(var_statements)
}
handlingV = Vectorize(handling_processing, 's')

# Typo fixes
fixdat = data.table(original = c('(2012 - BIRTHYR - (BIRTHQTR - 2.5)/4) BETWEEN 26 AND 35)',
                                 'I(HISPAN == 1), I(BPL >= 100)',
                                 'CITIZEN == 3 AND BPL > 120',
                                 'HISPAND == 100, CITIZEN == 3',
                                 'HISPAN IN( 1, 2, 3, 4)',
                                 'AGE >= 16, AGE <= 60',
                                 '(HISPAN-MEXICAN), (CITIZEN == 3) & (RACHSING-HISPANIC/LATIN)',
                                 '(HISPAN-MEXICAN ), (CITIZEN == 3) & (RACHSING-HISPANIC/LATIN)',
                                 'CITIZEN == 3, EDUCD > 61, AGE BETWEEN 18 AND 45, YRSUSA1 > 4',
                                 'HISPAN == 1, (SCHOOL BETWEEN  1 AND 2)',
                                 'AGE BETWEEN 18 AND 35, I(HISPAN == 1), I(BPL == 200), I(CITIZEN == 3), I(EDUCD >= 062)',
                                 'AGE_IN_2012 BETWEEN 26 AND 35,  I(AGE_AT_MIGRATION >= 12), I(AGE_AT_MIGRATION <= 19)',
                                 ' I(',
                                 'BPL < 999, AGE_IN_2012 BETWEEN 26 AND 35',
                                 'AGE BETWEEN 12 AND 100 AND HISPAN BETWEEN 1 AND 9',
                                 '(HISPAN == 1 & BPL == 200 & CITIZEN == 3 & YEAR BETWEEN 2006 AND 2016 & YEAR != 2012 & EMPSTAT IN (1,2) & (AGE_AT_MIGRATION < 16)',
                                 '((AGE_AT_MIGRATION <= 15 & (YEAR - YRIMMIG) >= 6 & BPL == 200 & CITIZEN == 3) | (HISPAN == 1 & BPL < 100))',
                                 '==','>=','>','<','<=',
                                 '< =', '> =', '= =',
                                 '(HISPAN != 1 | BPL != 200)',
                                 ';'),
                    new = c('(2012 - BIRTHYR - (BIRTHQTR - 2.5)/4) BETWEEN 26 AND 35',
                            'I(HISPAN == 1) & I(BPL >= 100)',
                            'CITIZEN == 3 & BPL > 120',
                            'HISPAND == 100 & CITIZEN == 3',
                            'HISPAN IN (1, 2, 3, 4)',
                            'AGE BETWEEN 16 AND 60',
                            'HISPAN == 1 & CITIZEN == 3 & & RACHSING == 5',
                            'HISPAN == 1 & CITIZEN == 3 & & RACHSING == 5',
                            'CITIZEN == 3 & EDUCD > 61 & AGE BETWEEN 18 AND 45 & YRSUSA1 > 4',
                            'HISPAN == 1 & (SCHOOL BETWEEN  1 AND 2)',
                            'AGE BETWEEN 18 AND 35 & I(HISPAN == 1) & I(BPL == 200) & I(CITIZEN == 3) & I(EDUCD >= 062)',
                            'AGE_IN_2012 BETWEEN 26 AND 35 &  I(AGE_AT_MIGRATION >= 12) & I(AGE_AT_MIGRATION <= 19)',
                            ' (',
                            'BPL < 999 & AGE_IN_2012 BETWEEN 26 AND 35',
                            'AGE BETWEEN 12 AND 100 & HISPAN BETWEEN 1 AND 9',
                            'HISPAN == 1 & BPL == 200 & CITIZEN == 3 & YEAR BETWEEN 2006 AND 2016 & YEAR != 2012 & EMPSTAT IN (1,2) & AGE_AT_MIGRATION < 16',
                            '((AGE_AT_MIGRATION <= 15 & (YEAR - YRIMMIG) >= 6 & BPL == 200 & CITIZEN == 3) | (HISPAN == 1 & BPL < 100))',
                            ' == ',' >= ',' > ',' < ',' <= ',
                            '<=', '>=', '==',
                            'HISPAN == 1 & BPL == 200',
                            ' & ')
)

for (i in 1:nrow(fixdat)) {
  sampdat[, AllLim := str_replace_all(AllLim, fixed(fixdat$original[i]), fixdat$new[i])]
  sampdat[, TreatLim := str_replace_all(TreatLim, fixed(fixdat$original[i]), fixdat$new[i])]
  sampdat[, UnTreatLim := str_replace_all(UnTreatLim, fixed(fixdat$original[i]), fixdat$new[i])]
}

# and regex ones
fixdat = data.table(original = c('^I\\(',
                                 '\r','\n'),
                    new = c('(',
                            '',''))
for (i in 1:nrow(fixdat)) {
  sampdat[, AllLim := str_replace_all(AllLim, fixdat$original[i], fixdat$new[i])]
  sampdat[, TreatLim := str_replace_all(TreatLim, fixdat$original[i], fixdat$new[i])]
  sampdat[, UnTreatLim := str_replace_all(UnTreatLim, fixdat$original[i], fixdat$new[i])]
}

# Final fixes for ???s, do these by hand evaluation of code
questions = sampdat[AllLim %like% '\\?\\?\\?' | TreatLim %like% '\\?\\?\\?' | UnTreatLim %like% '\\?\\?\\?']
questions = questions[, .(Q1, Round, Q13, Revision_of_Q13, 
                          Q14, Revision_of_Q14,
                          Q17, Revision_of_Q17,
                          Q20, Revision_of_Q20,
                          AllLim, TreatLim, UnTreatLim)]
export(questions, '../data/final_questions.xlsx')
questions = import('../data/final_questions.xlsx')
for (i in 1:nrow(questions)) {
  sampdat[Q1 == questions$Q1[i] & Round == questions$Round[i], AllLim := questions$AllLim[i]]
  sampdat[Q1 == questions$Q1[i] & Round == questions$Round[i], TreatLim := questions$TreatLim[i]]
  sampdat[Q1 == questions$Q1[i] & Round == questions$Round[i], UnTreatLim := questions$UnTreatLim[i]]
}
rm(questions)

# Remove remaining ???s
sampdat[AllLim %like% '\\?\\?\\?', AllLim := NA_character_]
sampdat[TreatLim %like% '\\?\\?\\?', TreatLim := NA_character_]
sampdat[UnTreatLim %like% '\\?\\?\\?', UnTreatLim := NA_character_]

sampdat[, limAll_HISPAN := handlingV(AllLim, 'HISPAN')]
sampdat[, limTreat_HISPAN := handlingV(TreatLim, 'HISPAN')]
sampdat[, limUn_HISPAN := handlingV(UnTreatLim, 'HISPAN')]
sampdat[, limAll_HISPAND := handlingV(AllLim, 'HISPAND')]
sampdat[, limTreat_HISPAND := handlingV(TreatLim, 'HISPAND')]
sampdat[, limUn_HISPAND := handlingV(UnTreatLim, 'HISPAND')]

source('../code/sample_selection_fixdat.R')

for (i in 1:nrow(fixdat)) {
  sampdat[limAll_HISPAN == fixdat$original[i], limAll_HISPAN := fixdat$new[i]]
  sampdat[limTreat_HISPAN == fixdat$original[i], limTreat_HISPAN := fixdat$new[i]]
  sampdat[limUn_HISPAN == fixdat$original[i], limUn_HISPAN := fixdat$new[i]]
}

fixdat = data.table(original = c("HISPAND BETWEEN 100 AND 107", "HISPAND IN 100, 103", "HISPAND > 0", "HISPAND == 100", "HISPAND IN 100, 102, 103, 106, 107", "HISPAND != 0", "(HISPAND BETWEEN 100 AND 107) & (HISPAND == 100)", "(HISPAND IN 100, 102, 103, 106, 107) & ((HISPAND == 100 | HISPAND == 102 | HISPAND == 103 | HISPAND == 106 | HISPAND == 107))", "(HISPAND != 0) & (HISPAND > 0)"),
                    new = c("Hispanic-Mexican", "Hispanic-Mexican", "Hispanic-Any", "Hispanic-Mexican", "Hispanic-Mexican", "Hispanic-Any", "Hispanic-Mexican", "Hispanic-Mexican", "Hispanic-Any"))

for (i in 1:nrow(fixdat)) {
  sampdat[limAll_HISPAND == fixdat$original[i] & limAll_HISPAN == 'None', limAll_HISPAN := fixdat$new[i]]
  sampdat[limTreat_HISPAND == fixdat$original[i] & limTreat_HISPAN == 'None', limTreat_HISPAN := fixdat$new[i]]
  sampdat[limUn_HISPAND == fixdat$original[i] & limUn_HISPAN == 'None', limUn_HISPAN := fixdat$new[i]]
}

# MEXICAN BORN
sampdat[, limAll_BPL := handlingV(AllLim, 'BPL')]
sampdat[, limTreat_BPL := handlingV(TreatLim, 'BPL')]
sampdat[, limUn_BPL := handlingV(UnTreatLim, 'BPL')]
sampdat[, limAll_BPLD := handlingV(AllLim, 'BPLD')]
sampdat[, limTreat_BPLD := handlingV(TreatLim, 'BPLD')]
sampdat[, limUn_BPLD := handlingV(UnTreatLim, 'BPLD')]

for (i in 1:nrow(fixdat_bpl)) {
  sampdat[limAll_BPL == fixdat_bpl$original[i], limAll_BPL := fixdat_bpl$new[i]]
  sampdat[limTreat_BPL == fixdat_bpl$original[i], limTreat_BPL := fixdat_bpl$new[i]]
  sampdat[limUn_BPL == fixdat_bpl$original[i], limUn_BPL := fixdat_bpl$new[i]]
  sampdat[limAll_BPLD == fixdat_bpl$original[i] & limAll_BPL %in% c('None',''), limAll_BPL := fixdat_bpl$new[i]]
  sampdat[limTreat_BPLD == fixdat_bpl$original[i] & limAll_BPL %in% c('None',''), limTreat_BPL := fixdat_bpl$new[i]]
  sampdat[limUn_BPLD == fixdat_bpl$original[i] & limAll_BPL %in% c('None',''), limUn_BPL := fixdat_bpl$new[i]]
}

# NON-CITIZEN
sampdat[, limAll_CITIZEN := handlingV(AllLim, 'CITIZEN')]
sampdat[, limTreat_CITIZEN := handlingV(TreatLim, 'CITIZEN')]
sampdat[, limUn_CITIZEN := handlingV(UnTreatLim, 'CITIZEN')]

for (i in 1:nrow(fixdat_citizen)) {
  sampdat[limAll_CITIZEN == fixdat_citizen$original[i], limAll_CITIZEN := fixdat_citizen$new[i]]
  sampdat[limTreat_CITIZEN == fixdat_citizen$original[i], limTreat_CITIZEN := fixdat_citizen$new[i]]
  sampdat[limUn_CITIZEN == fixdat_citizen$original[i], limUn_CITIZEN := fixdat_citizen$new[i]]
}
# All typos of this kind do non-citizen
sampdat[AllLim %like% 'CITITZEN', limAll_CITIZEN := 'Non-Citizen']
sampdat[TreatLim %like% 'CITITZEN', limTreat_CITIZEN := 'Non-Citizen']
sampdat[UnTreatLim %like% 'CITITZEN', limUn_CITIZEN := 'Non-Citizen']

sampdat[, AllLim := str_replace_all(AllLim, 'YRIMMIG - BIRTHYR', 'AGE_AT_MIGRATION')]
sampdat[, limAll_AGE_AT_MIGRATION := handlingV(AllLim, 'AGE_AT_MIGRATION')]
sampdat[, limTreat_AGE_AT_MIGRATION := handlingV(TreatLim, 'AGE_AT_MIGRATION')]
sampdat[, limUn_AGE_AT_MIGRATION := handlingV(UnTreatLim, 'AGE_AT_MIGRATION')]
sampdat[limAll_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION <= 16' & str_count(limAll_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limAll_AGE_AT_MIGRATION,'!'), limAll_AGE_AT_MIGRATION := '<= 16']
sampdat[limAll_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION < 16' & str_count(limAll_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limAll_AGE_AT_MIGRATION,'!'), limAll_AGE_AT_MIGRATION := '< 16']
sampdat[limAll_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION <= 15' & str_count(limAll_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limAll_AGE_AT_MIGRATION,'!'), limAll_AGE_AT_MIGRATION := '< 15']
sampdat[limTreat_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION <= 16' & str_count(limTreat_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limTreat_AGE_AT_MIGRATION,'!'), limTreat_AGE_AT_MIGRATION := '<= 16']
sampdat[limTreat_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION < 16' & str_count(limTreat_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limTreat_AGE_AT_MIGRATION,'!'), limTreat_AGE_AT_MIGRATION := '< 16']
sampdat[limTreat_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION <= 15' & str_count(limTreat_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limTreat_AGE_AT_MIGRATION,'!'), limTreat_AGE_AT_MIGRATION := '< 15']
sampdat[limUn_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION <= 16' & str_count(limUn_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limUn_AGE_AT_MIGRATION,'!'), limUn_AGE_AT_MIGRATION := '<= 16']
sampdat[limUn_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION < 16' & str_count(limUn_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limUn_AGE_AT_MIGRATION,'!'), limUn_AGE_AT_MIGRATION := '< 16']
sampdat[limUn_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION <= 15' & str_count(limUn_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limUn_AGE_AT_MIGRATION,'!'), limUn_AGE_AT_MIGRATION := '< 15']
sampdat[limAll_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION >= 16 \\|' & str_count(limAll_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limAll_AGE_AT_MIGRATION,'!'), limAll_AGE_AT_MIGRATION := 'Multistep Condition']
sampdat[limAll_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION > 16 \\|' & str_count(limAll_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limAll_AGE_AT_MIGRATION,'!'), limAll_AGE_AT_MIGRATION := 'Multistep Condition']
sampdat[limAll_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION >= 15 \\|' & str_count(limAll_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limAll_AGE_AT_MIGRATION,'!'), limAll_AGE_AT_MIGRATION := 'Multistep Condition']
sampdat[limTreat_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION >= 16 \\|' & str_count(limTreat_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limTreat_AGE_AT_MIGRATION,'!'), limTreat_AGE_AT_MIGRATION := 'Multistep Condition']
sampdat[limTreat_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION > 16 \\|' & str_count(limTreat_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limTreat_AGE_AT_MIGRATION,'!'), limTreat_AGE_AT_MIGRATION := 'Multistep Condition']
sampdat[limTreat_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION >= 15 \\|' & str_count(limTreat_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limTreat_AGE_AT_MIGRATION,'!'), limTreat_AGE_AT_MIGRATION := 'Multistep Condition']
sampdat[limUn_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION >= 16 \\|' & str_count(limUn_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limUn_AGE_AT_MIGRATION,'!'), limUn_AGE_AT_MIGRATION := 'Multistep Condition']
sampdat[limUn_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION > 16 \\|' & str_count(limUn_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limUn_AGE_AT_MIGRATION,'!'), limUn_AGE_AT_MIGRATION := 'Multistep Condition']
sampdat[limUn_AGE_AT_MIGRATION %like% 'AGE_AT_MIGRATION >= 15 \\|' & str_count(limUn_AGE_AT_MIGRATION,'AGE_AT_MIGRATION') == 1 & !str_detect(limUn_AGE_AT_MIGRATION,'!'), limUn_AGE_AT_MIGRATION := 'Multistep Condition']

for (i in 1:nrow(fixdat_aam)) {
  sampdat[limAll_AGE_AT_MIGRATION == fixdat_aam$original[i], limAll_AGE_AT_MIGRATION := fixdat_aam$new[i]]
  sampdat[limTreat_AGE_AT_MIGRATION == fixdat_aam$original[i], limTreat_AGE_AT_MIGRATION := fixdat_aam$new[i]]
  sampdat[limUn_AGE_AT_MIGRATION == fixdat_aam$original[i], limUn_AGE_AT_MIGRATION := fixdat_aam$new[i]]
}
# Wrote AGE_AT_MIGRATION in a weird way
sampdat[95, limTreat_AGE_AT_MIGRATION := '> 16']

# AGE_IN_2012 / BIRTHYR + BIRTHQTR / BIRTHYR
# Here we are simplifying to whether the quarter is used
sampdat[, limAll_AGE_IN_2012 := fcase(AllLim %like% 'AGE_IN_2012' | AllLim %like% 'BIRTHQTR', 'Year-Quarter Age',
                                      AllLim %like% 'BIRTHYR', 'Year-Only Age',
                                      !(AllLim %like% 'BIRTHYR'), 'None')]
sampdat[, limTreat_AGE_IN_2012 := fcase(TreatLim %like% 'AGE_IN_2012' | TreatLim %like% 'BIRTHQTR', 'Year-Quarter Age',
                                      TreatLim %like% 'BIRTHYR', 'Year-Only Age',
                                      !(TreatLim %like% 'BIRTHYR'), 'None')]
sampdat[, limUn_AGE_IN_2012 := fcase(UnTreatLim %like% 'AGE_IN_2012' | UnTreatLim %like% 'BIRTHQTR', 'Year-Quarter Age',
                                        UnTreatLim %like% 'BIRTHYR', 'Year-Only Age',
                                        !(UnTreatLim %like% 'BIRTHYR'), 'None')]

# IMMIGRATED BEFORE 2007
sampdat[, limAll_YRIMMIG := handlingV(AllLim, 'YRIMMIG')]
sampdat[, limTreat_YRIMMIG := handlingV(TreatLim, 'YRIMMIG')]
sampdat[, limUn_YRIMMIG := handlingV(UnTreatLim, 'YRIMMIG')]

# Clean out the multistep conditions
sampdat[str_count(limAll_YRIMMIG,'\\|') >= 3, limAll_YRIMMIG := 'Multistep Condition']
sampdat[str_count(limTreat_YRIMMIG,'\\|') >= 3, limTreat_YRIMMIG := 'Multistep Condition']
sampdat[str_count(limUn_YRIMMIG,'\\|') >= 3, limUn_YRIMMIG := 'Multistep Condition']

# Prune out common cases
def_yrimmig = function(s) {
  s_no_paren = str_squish(str_replace_all(s,'\\(|\\)',''))
  if (s_no_paren %like% '(^|\\& )YRIMMIG < 2007($| \\&)') {
    return('< 2007')
  }
  if (s_no_paren %like% '(^|\\& )YRIMMIG <= 2006($| \\&)' | 
      s_no_paren %like% '(^|\\& )YRIMMIG BETWEEN \\d+ AND 2006($| \\&)') {
    return('< 2007')
  }
  if (s_no_paren %like% '(^|\\& )YRIMMIG <= 2007($| \\&)' | s_no_paren %like% '(^|\\& )YRIMMIG <=2007($| \\&)') {
    return('<= 2007')
  }
  if (s_no_paren %like% '(^|\\& )YRIMMIG > 2007($| \\&)') {
    return('> 2007')
  }
  if (s_no_paren %like% '(^|\\& )!YRIMMIG < 2007($| \\&)') {
    return('> 2007')
  }
  if (s_no_paren %like% '(^|\\& )YRIMMIG >= 2007($| \\&)') {
    return('>= 2007')
  }
  if (s_no_paren %like% '(^|\\| )YRIMMIG < 2007($| \\|)') {
    return('Multistep Condition')
  }
  if (s_no_paren %like% '(^|\\| )YRIMMIG <= 2006($| \\|)') {
    return('Multistep Condition')
  }
  if ((s %like% 'YRIMMIG > 0' | s %like% 'YRIMMIG >= BIRTHYR') & str_count(s, 'YRIMMIG') == 1) {
    return('Any Year')
  }
  return(s)
}
def_yrimmigV = Vectorize(def_yrimmig, 's')

sampdat[, limAll_YRIMMIG := def_yrimmigV(limAll_YRIMMIG)]
sampdat[, limTreat_YRIMMIG := def_yrimmigV(limTreat_YRIMMIG)]
sampdat[, limUn_YRIMMIG := def_yrimmigV(limUn_YRIMMIG)]

for (i in 1:nrow(fixdat_educvet)) {
  sampdat[limAll_YRIMMIG == fixdat_yrimmig$original[i], limAll_YRIMMIG := fixdat_yrimmig$new[i]]
  sampdat[limTreat_YRIMMIG == fixdat_yrimmig$original[i], limTreat_YRIMMIG := fixdat_yrimmig$new[i]]
  sampdat[limUn_YRIMMIG == fixdat_yrimmig$original[i], limUn_YRIMMIG := fixdat_yrimmig$new[i]]
}

# HS GRAD OR VETERAN
for (vrs in c('EDUC','EDUCD','VETSTAT')) {
  sampdat[[paste0('limAll_',vrs)]] = handlingV(sampdat$AllLim, vrs)
  sampdat[[paste0('limTreat_',vrs)]] = handlingV(sampdat$TreatLim, vrs)
  sampdat[[paste0('limUn_',vrs)]] = handlingV(sampdat$UnTreatLim, vrs)
}
# A lot of the above probably copied the same part multiple times
sampdat[['limAll_EDUCVET']] = sapply(1:nrow(sampdat), \(x) {
  paste(unique(c(sampdat$limAll_EDUC[x], sampdat$limAll_EDUCD[x], sampdat$limAll_VETSTAT[x])), collapse = ' & ')
})
sampdat[['limTreat_EDUCVET']] = sapply(1:nrow(sampdat), \(x) {
  paste(unique(c(sampdat$limTreat_EDUC[x], sampdat$limTreat_EDUCD[x], sampdat$limTreat_VETSTAT[x])), collapse = ' & ')
})
sampdat[['limUn_EDUCVET']] = sapply(1:nrow(sampdat), \(x) {
  paste(unique(c(sampdat$limUn_EDUC[x], sampdat$limUn_EDUCD[x], sampdat$limUn_VETSTAT[x])), collapse = ' & ')
})

# Prune out common cases
def_educorvet = function(s) {
  if (s %like% 'VETSTAT == 1' & s %like% 'VETSTAT == 2') {
    return(s)
  }
  ssub = copy(s)
  ssub = str_replace_all(ssub, 'VETSTATD IN (20, 21, 22)','VETSTAT == 2')
  ssub = str_replace_all(ssub, 'EDUC BETWEEN 6 AND 11','EDUC >= 6')
  if (ssub %like% 'EDUC >= 6 \\| VETSTAT == 2' | 
      ssub %like% 'EDUC > 5 \\| VETSTAT == 2' |
      ssub %like% 'EDUCD >= 62 \\| VETSTAT == 2' | 
        ssub %like% 'EDUCD > 61 \\| VETSTAT == 2') { 
    return('HS Grad or Veteran')
  }
  return(s)
}
def_educorvetV = Vectorize(def_educorvet, 's')

sampdat[, limAll_EDUCVET := def_educorvetV(limAll_EDUCVET)]
sampdat[, limTreat_EDUCVET := def_educorvetV(limTreat_EDUCVET)]
sampdat[, limUn_EDUCVET := def_educorvetV(limUn_EDUCVET)]

for (i in 1:nrow(fixdat_educvet)) {
  sampdat[limAll_EDUCVET == fixdat_educvet$original[i], limAll_EDUCVET := fixdat_educvet$new[i]]
  sampdat[limTreat_EDUCVET == fixdat_educvet$original[i], limTreat_EDUCVET := fixdat_educvet$new[i]]
  sampdat[limUn_EDUCVET == fixdat_educvet$original[i], limUn_EDUCVET := fixdat_educvet$new[i]]
}


# LIVED CONTINUOUSLY
# Just look for presence of YRSUSA
sampdat = copy(sampdat)
sampdat[, limAll_YRSUSA := fifelse(AllLim %like% 'YRSUSA', 'Used YRSUSA', 'No YRSUSA')]
sampdat[, limTreat_YRSUSA := fifelse(TreatLim %like% 'YRSUSA', 'Used YRSUSA', 'No YRSUSA')]
sampdat[, limUn_YRSUSA := fifelse(UnTreatLim %like% 'YRSUSA', 'Used YRSUSA', 'No YRSUSA')]

### REVISIONS
# At this point an error in the original handlingV was discovered which blanked
# any limitation in which the relevant variable was after the last & in AllLim/etc.
# Thankfully, due to the construction of the code, which looks for exact matches in relevant cases
# This will lead to omitted codings reflecting the original values in any case where this error changed the output,
# which can be easily spotted and corrected at this point.
source('../code/sample_selection_fixdat_revisions.R')

for (i in 1:nrow(fixdat_aam)) {
  sampdat[limAll_AGE_AT_MIGRATION == fixdat_aam$original[i], limAll_AGE_AT_MIGRATION := fixdat_aam$new[i]]
  sampdat[limTreat_AGE_AT_MIGRATION == fixdat_aam$original[i], limTreat_AGE_AT_MIGRATION := fixdat_aam$new[i]]
  sampdat[limUn_AGE_AT_MIGRATION == fixdat_aam$original[i], limUn_AGE_AT_MIGRATION := fixdat_aam$new[i]]
}

for (i in 1:nrow(fixdat_bpl)) {
  sampdat[limAll_BPL == fixdat_bpl$original[i], limAll_BPL := fixdat_bpl$new[i]]
  sampdat[limTreat_BPL == fixdat_bpl$original[i], limTreat_BPL := fixdat_bpl$new[i]]
  sampdat[limUn_BPL == fixdat_bpl$original[i], limUn_BPL := fixdat_bpl$new[i]]
}

for (i in 1:nrow(fixdat_citizen)) {
  sampdat[limAll_CITIZEN == fixdat_citizen$original[i], limAll_CITIZEN := fixdat_citizen$new[i]]
  sampdat[limTreat_CITIZEN == fixdat_citizen$original[i], limTreat_CITIZEN := fixdat_citizen$new[i]]
  sampdat[limUn_CITIZEN == fixdat_citizen$original[i], limUn_CITIZEN := fixdat_citizen$new[i]]
}

for (i in 1:nrow(fixdat_educvet)) {
  sampdat[limAll_EDUCVET == fixdat_educvet$original[i], limAll_EDUCVET := fixdat_educvet$new[i]]
  sampdat[limTreat_EDUCVET == fixdat_educvet$original[i], limTreat_EDUCVET := fixdat_educvet$new[i]]
  sampdat[limUn_EDUCVET == fixdat_educvet$original[i], limUn_EDUCVET := fixdat_educvet$new[i]]
}

sampdat[limAll_HISPAN == '(HISPAN == 1) & (HISPAN == 1))', limAll_HISPAN := 'Hispanic-Mexican']
sampdat[limTreat_HISPAN == '(HISPAN == 1) & (HISPAN == 1))', limTreat_HISPAN := 'Hispanic-Mexican']
sampdat[limUn_HISPAN == '(HISPAN == 1) & (HISPAN == 1))', limUn_HISPAN := 'Hispanic-Mexican']

for (i in 1:nrow(fixdat_yrimmig)) {
  sampdat[limAll_YRIMMIG == fixdat_yrimmig$original[i], limAll_YRIMMIG := fixdat_yrimmig$new[i]]
  sampdat[limTreat_YRIMMIG == fixdat_yrimmig$original[i], limTreat_YRIMMIG := fixdat_yrimmig$new[i]]
  sampdat[limUn_YRIMMIG == fixdat_yrimmig$original[i], limUn_YRIMMIG := fixdat_yrimmig$new[i]]
}

