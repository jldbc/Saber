# proposed hof procedure if this works out well: 
#   - hof eligibility determined solely by an ensemble of models (one for career stats, one for advanced and per-pa stats, a third with everything)
#   - of those elected by the model, a panel of three non-baseball "good people" make the call as to whether they're morally fit for the hall (oprah, bono, etc.)

df = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/predict_hof_bat.csv')
currents = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/predict_hof_bat_currents.csv')

# drop columns that shouldn't be in the model 
ids = df$playerID
ids_current = currents$playerID
cols = c('votes_pct','Name','bbrefID', 'retroID','debut', 'finalGame','nameFirst',
         'nameLast', 'deathCity', 'deathState', 'deathCountry', 'deathDay',
         'deathMonth', 'deathYear', 'birthCity', 'birthState', 'birthCountry',
         'birthDay','birthMonth','time_on_ballot','total_times_on_ballot','needed_vote',
         'category', 'votes', 'needed', 'ballots','votedBy','yearid','playerID','X',
         'needed_note','Name_master','nameGiven')

df = df[,!(names(df) %in% cols)]
df$throws = ifelse(df$throws=='L',1,0)
df$bats = ifelse(df$bats=='L',1,0)
df$inducted = ifelse(df$inducted=='Y',1,0)

currents = currents[,!(names(currents) %in% cols)]
currents$throws = ifelse(currents$throws=='L',1,0)
currents$bats = ifelse(currents$bats=='L',1,0)
currents$inducted = ifelse(currents$inducted=='Y',1,0)

# mean impute NAs
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
  currents[is.na(currents[,i]), i] <- mean(df[,i], na.rm = TRUE)
}

# first stage: explanatory modeling, staying in sample  
# findings: 
#   major multicollinearity problem. advanced stats highly correlated w/ simple ones and one-another 
#   similar multiollinearity problem with including career-total and per-PA stats within same model 
#   manually pruning insignificants shows an end-model including career total stats that collectively try to 
#       compensate for the advanced stats I've dropped (pos coeffs for hits and walks but negative for PA approximates high OBP/woba)

# shuffle rows to ensure splitting is random
df$id = ids 
df = df[sample(nrow(df)),]
ids_shuffled = df$id
df_display = df
# normalize to 0 - 1 scale for regularized models
normalize <- function(x) {
  (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -  min(x, na.rm=TRUE))
}

# normalize everything except the ids
ids = df$id
df$id = NULL
df = as.data.frame(lapply(df, normalize))
df$id = ids

# create a copy of currents since we'll want to save the unnormalized data to display to users later on 
currents$id = ids_current
currents_norm = currents 
cur_ids = currents_norm$id
currents_norm$id = NULL
currents_norm = as.data.frame(lapply(currents_norm, normalize))
currents_norm$id = cur_ids


write.csv(df, '/Users/jledoux/Documents/projects/Saber/hof/data/all_past.csv')
write.csv(df_display, '/Users/jledoux/Documents/projects/Saber/hof/data/all_past_orig.csv')
write.csv(currents_norm, '/Users/jledoux/Documents/projects/Saber/hof/data/all_currents_model.csv')
write.csv(currents, '/Users/jledoux/Documents/projects/Saber/hof/data/all_currents_display.csv')
