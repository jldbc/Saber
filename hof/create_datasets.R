# proposed hof procedure if this works out well: 
#   - hof eligibility determined solely by an ensemble of models (one for career stats, one for advanced and per-pa stats, a third with everything)
#   - of those elected by the model, a panel of three non-baseball "good people" make the call as to whether they're morally fit for the hall (oprah, bono, etc.)
library(dplyr)

df = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/predict_hof_bat2.csv')
currents = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/predict_hof_bat_currents.csv')
awards = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/lahman/AwardsPlayers.csv')
playoffs = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/lahman/SeriesPost.csv')

# remove shoeless joe, pete rose, and who's the negro league guy who only played one yr? 
#    rosepe01, 
# create decade dummies (group by player, mean year, round down to 10s place)
# create dummy for playing before an award was possible (eg ruth played before there was an mvp award)
# find 1880 - 1900 data or drop those with start date before 1900

awards$lgID = NULL
awards$notes = NULL
awards$tie = NULL
awards$yearID = NULL

awards = awards %>% group_by(playerID, awardID) %>% mutate(count = n())
awards = dcast(awards, playerID~awardID)

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
df = df %>% left_join(awards, by=c('id'='playerID'))
df[is.na(df)] = 0 # take care of the nas created by this join
df$id = NULL
df = as.data.frame(lapply(df, normalize))
df[is.na(df)] = 0 
df$id = ids

# create a copy of currents since we'll want to save the unnormalized data to display to users later on 
currents$id = ids_current
currents = currents %>% left_join(awards, by=c('id'='playerID'))
currents[is.na(currents)] = 0 # take care of the nas created by this join
currents_norm = currents 
cur_ids = currents_norm$id
currents_norm$id = NULL
currents_norm = as.data.frame(lapply(currents_norm, normalize))
currents_norm[is.na(currents_norm)] = 0 
currents_norm$id = cur_ids

# a quick sanity check to see if the data looks correct for those who were inducted 
# initially realized i was missing data on pre-1900 players. added those in, which left only pre-modern-era players,
# managers, and players who spent most of their career in the negro leagues 
temp = df_display[df_display$inducted==1,c('id','H_tot')]
temp[order(temp$H_tot),]

# anomalies = c('orourji01', 'duffyhu01', 'kellejo01', 'thompsa01', 'jennihu01', 'hamilbi01', 'broutda01', 'brownwi02', 'delahed01', 'mcgrajo01', 'irvinmo01')
# master[master$playerID %in% anomalies,]
#anomalies2 = c('brownwi02', 'irvinmo01', 'mackco01', 'wrighge01')
#master[master$playerID %in% anomalies2,]
# Willard Brown: negro league legend, only one mlb season 
# Monte Irvin: half of career in negro league
# Connie Mack: manager
# George Wright: most of career played pre 1876 when modern NL was founded. exclude. 36? see what hes at w 76 - 79 added
temp = df_display[df_display$inducted==1,c('id','years_in_league')]
temp[order(temp$years_in_league),]

#players who played < 13 yrs, to see if I'm missing any data. everything looks fine
anomalies3 = c('kinerra01','robinja02','youngro01','camparo01','gordojo01','mackco01','puckeki01','wilsoha01','combsea01')
master[master$playerID %in% anomalies2,]

# drop remaining anomalies (inc. rose and bonds bc of unique circumstances)
drops = c('mackco01', 'irvinmo01', 'brownwi02', 'rosepe01','bondsba01')

df = df[!(df$id %in% drops),]
df_display = df_display[!(df_display$id %in% drops),]

write.csv(df, '/Users/jledoux/Documents/projects/Saber/hof/data/all_past.csv')
write.csv(df_display, '/Users/jledoux/Documents/projects/Saber/hof/data/all_past_orig.csv')
write.csv(currents_norm, '/Users/jledoux/Documents/projects/Saber/hof/data/all_currents_model.csv')
write.csv(currents, '/Users/jledoux/Documents/projects/Saber/hof/data/all_currents_display.csv')
