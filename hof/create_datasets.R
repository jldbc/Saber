# TODO
# add playoff series wins 

# imports 
library(dplyr)
library(ggplot2)
library(cowplot)
# load data
hof = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/lahman/HallOfFame.csv')
lahman_bat = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/lahman/batting.csv')
lahman_pitch = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/lahman/pitching.csv')
fg_bat_old = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/fg_1876_1899_bat.csv')
fg_bat = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/fg_1900_2017_bat.csv')
fg_pitch = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/fg_1900_2017_pitch.csv')
master = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/lahman/master.csv')
fielding = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/lahman/fielding.csv')

fg_bat = rbind(fg_bat,fg_bat_old)

# the fangraphs data is far more complete with advanced stats than the lahman data, but doesn't include a player id variable 
# joining on name alone isn't enough, but we can create a birthyear variable given age and season 
# joining on age and birthyear should work in most/all cases 
fg_bat$birthYear = fg_bat$Season - fg_bat$Age
fg_pitch$birthYear = fg_pitch$Season - fg_pitch$Age

# manually fixing a few players with inaccurate birth years. Need this for matching btwn fg and lahamn data 
fg_bat[fg_bat$Name=='Wilbert Robinson','birthYear'] = 1864
fg_bat[fg_bat$Name=='George Davis','birthYear'] = 1871
fg_bat[fg_bat$Name=='Frankie Frisch','birthYear'] = 1898
fg_bat[fg_bat$Name=='Billy Williams','birthYear'] = 1938
fg_bat[fg_bat$Name=='Phil Rizzuto','birthYear'] = 1918
fg_bat[fg_bat$Name=='Ben Taylor','birthYear'] = 1889
fg_bat[fg_bat$Name=='Roger Connor','birthYear']  = 1858
fg_bat[fg_bat$Name=='Ad Gumbert','birthYear']  = 1867
fg_bat[fg_bat$Name=='Adam Comorosky','birthYear']  = 1905
fg_bat[fg_bat$Name=='Alex McCarthy','birthYear']  = 1889
fg_bat[fg_bat$Name=='Allan Russell','birthYear']  = 1893
fg_bat[fg_bat$Name=='Adam Comorosky','birthYear']  = 1905
fg_bat[fg_bat$Name=='Bill Gleason','birthYear']  = 1859
fg_bat[fg_bat$Name=='Bill McCarthy','birthYear'] = 1886  
fg_bat[fg_bat$Name=='Billy Rohr','birthYear'] = 1946 

# names with a period in them have spaces after the period in the lahman db. fix to be consistent w fangraphs
master$nameFirst = as.character(master$nameFirst)
master[(master$nameFirst == 'A. J.') & (!is.na(master$nameFirst)),'nameFirst'] = "A.J."
master[(master$nameFirst == "D. T.") & (!is.na(master$nameFirst)),'nameFirst'] = "D.T."
master[(master$nameFirst == 'D. J.') & (!is.na(master$nameFirst)),'nameFirst'] = "D.J."
master[(master$nameFirst == 'C. J.') & (!is.na(master$nameFirst)),'nameFirst'] = "C.J."
master[(master$nameFirst == 'C. B.') & (!is.na(master$nameFirst)),'nameFirst'] = "C.B."
master[(master$nameFirst == 'C. V.') & (!is.na(master$nameFirst)),'nameFirst'] = "C.V."
master[(master$nameFirst == 'B. J.') & (!is.na(master$nameFirst)),'nameFirst'] = "B.J."
master[(master$nameFirst == 'R. J.') & (!is.na(master$nameFirst)),'nameFirst'] = "R.J."
master[(master$nameFirst == 'J. A.') & (!is.na(master$nameFirst)),'nameFirst'] = "J.A."
master[(master$nameFirst == 'J. B.') & (!is.na(master$nameFirst)),'nameFirst'] = "J.B."
master[(master$nameFirst == 'J. C.') & (!is.na(master$nameFirst)),'nameFirst'] = "J.C."
master[(master$nameFirst == 'J. D.') & (!is.na(master$nameFirst)),'nameFirst'] = "J.D."
master[(master$nameFirst == 'J. J.') & (!is.na(master$nameFirst)),'nameFirst'] = "J.J."
master[(master$nameFirst == 'J. P.') & (!is.na(master$nameFirst)),'nameFirst'] = "J.P."
master[(master$nameFirst == 'J. R.') & (!is.na(master$nameFirst)),'nameFirst'] = "J.R."
master[(master$nameFirst == 'J. T.') & (!is.na(master$nameFirst)),'nameFirst'] = "J.T."
master[(master$nameFirst == 'L. J.') & (!is.na(master$nameFirst)),'nameFirst'] = "L.J."
master[(master$nameFirst == 'R. A.') & (!is.na(master$nameFirst)),'nameFirst'] = "R.A."
master[(master$nameFirst == 'R. C.') & (!is.na(master$nameFirst)),'nameFirst'] = "R.C."
master[(master$nameFirst == 'T. E.') & (!is.na(master$nameFirst)),'nameFirst'] = "T.E."
master[(master$nameFirst == 'T. J.') & (!is.na(master$nameFirst)),'nameFirst'] = "T.J."
master[(master$nameFirst == 'U. L.') & (!is.na(master$nameFirst)),'nameFirst'] = "U.L."


# can't modify where null values
master = master[complete.cases(master$birthMonth),]
master[master$birthMonth>=7,'birthYear'] = master[master$birthMonth>=7,'birthYear'] + 1  # from analyzing baseball data with R book
master$Name = paste(master$nameFirst, master$nameLast,sep=" ")

fg_bat = left_join(fg_bat, master, by = c("Name","birthYear"))

# remove players who started their careers before the modern NL (1876). Their data is incomplete. 
# keep ones with nas since most of them seem to be post-1876 from a qualitative scan of their seasons 
fg_bat$debutYear = substring(fg_bat$debut, 1,4)
fg_bat$debutYear = as.numeric(fg_bat$debutYear)
fg_bat = fg_bat[!((fg_bat$debutYear<1876) & (!is.na(fg_bat$debutYear))),]

# the age column from fangraphs isn't always reliable and causes us to miss a few player-season pairs 
# find not-matched rows, join these with fg-bat on name, fix bat birth years 
#not_matched = anti_join(fg_bat, master, by=c("Name","birthYear"))
#dim(unique(not_matched[,c('Name','birthYear')]))
#dim(unique(fg_bat[,c('Name','birthYear')]))
#nm_names = unique(not_matched$Name)
#nm_all = fg_bat[fg_bat$Name %in% nm_names,c('Name','playerID','Age','Season','birthYear')]
#nm_all = nm_all[order(nm_all[,'Name'], nm_all[,'Age']),]
#View(nm_all)
#length(unique(nm_all$Name))
#length(unique(fg_bat$Name))

# drop current players (players whose max season is this past year)
# actually, since it takes 5 tries to get in on average, lets drop players since 2011
fg_bat = fg_bat %>% group_by(playerID) %>% mutate(last_season = max(Season)) %>% 
  mutate(years_in_league = n()) %>% 
  mutate(first_season = min(Season))# %>%
#filter(last_season < 2011)

#not_hof = hof[!(hof$playerID %in% in_hof$playerID),] %>% group_by(playerID) %>% filter(votes_pct == max(votes_pct))

# modify fielding to have only a player's main position 
fielding$not_p = ifelse(fielding$POS=='P',0,1)
fielding = fielding[,c('playerID','G','not_p')]
fielding = fielding %>% group_by(playerID) %>% summarise(
  gp_not_p = sum(G[not_p==1]),
  gp_p = sum(G[not_p==0]))

fielding$not_p = ifelse(fielding$gp_not_p>fielding$gp_p,1,0)

fg_bat = left_join(fg_bat, fielding, by='playerID')
fg_bat = fg_bat[fg_bat$not_p==1,]

# evaluate unmatched records with anti_join. 0.3% didn't match. Not ideal but it will work. 
not_matched = anti_join(fg_bat, master, by=c("Name","birthYear"))
dim(fg_bat)
dim(not_matched)
dim(not_matched)[1]/dim(fg_bat)[1]

# create variable for times voted on so far 
hof = hof %>% group_by(playerID) %>%
  mutate(total_times_on_ballot = n()) %>%
  mutate(time_on_ballot = seq(1,n(),1))

# merge master data onto hof table 
hof = left_join(hof, master[,c('playerID','Name')], by='playerID')

# biggest landslides in HOF voting history 
# note that not all inductees are players who were voted on by BWAA (eg centennial commission, old timers committee)
hof$votes_pct = hof$votes / hof$needed
print(hof[order(-hof$votes_pct),c('Name','votes_pct','yearid','inducted')],n=30)

in_hof = hof[hof$inducted=='Y',]

not_hof = hof[!(hof$playerID %in% in_hof$playerID),] %>% group_by(playerID) %>% filter(votes_pct == max(votes_pct))
not_hof[order(-not_hof$votes_pct),c('Name','votes_pct','yearid')]

# how many voting cycles does it take on average for those who get in? 
mean(hof[hof$inducted=='Y',]$time_on_ballot)

# to be joined to player performance df
# one row per player, including their top % of needed votes and whether they're in the hall 
hof = rbind(in_hof,not_hof)

# we can see how many of these non-matched players are in the hall and use the lahman data if it's a large enough number
# 6 out of 317, or 1.9%, weren't matched. Fixed this manually (see above where I hardcode in BirthYear for these players)

#not_matched_players = not_matched$Name
#n_not_matched_in_hall = dim(in_hof[in_hof$Name %in% not_matched_players,])
#n_not_matched_in_hall / dim(in_hof)[1]

#not_matched_in_hall = in_hof[in_hof$Name %in% not_matched_players,'Name']
#print(not_matched_in_hall) # looks good now 

# collapse rows on player idto get career-level statistics 
cols_to_keep =  c('playerID','wOBA','OBP','H','X1B','X2B','X3B','HR','BB','IBB','SO','HBP','PA','AVG','SLG','AB','WAR',
                  'RBI','R','BB','RAR','OPS','BABIP','G')

# weighting by PA
pa_weight_cols = c('playerID','wOBA','OBP','H','X1B','X2B','X3B','HR','BB','IBB','SO','HBP','PA')
ab_weight_cols = c('playerID','AVG','SLG','AB')
g_weight_cols = c('playerID','WAR','RAR','OPS','BABIP','G')
sum_cols = c('playerID','AB','H','X1B','X2B','X3B','HR','RBI','R','BB','IBB','WAR','RAR','G','PA')

fgb = fg_bat[,cols_to_keep]

fg_bat_career_pa = fg_bat[,pa_weight_cols] %>% group_by(playerID) %>%
  summarise_all(
    funs(weighted.mean(., PA))
  )

fg_bat_career_ab = fg_bat[,ab_weight_cols] %>% group_by(playerID) %>%
  summarise_all(
    funs(weighted.mean(., AB))
  )

fg_bat_career_g = fg_bat[,g_weight_cols] %>% group_by(playerID) %>%
  summarise_all(
    funs(weighted.mean(., G))
  )

fg_bat_career_tot = fg_bat[,sum_cols] %>% group_by(playerID) %>%
  summarise_all(
    funs(sum(.))
  )

fg_bat_career = fg_bat_career_tot %>% left_join(fg_bat_career_g,by='playerID',suffix=c(x='_tot',y='_g')) %>%
  left_join(.,fg_bat_career_ab,by='playerID',suffix=c(x='_tot',y='_ab')) %>%  
  left_join(.,fg_bat_career_pa,by='playerID',suffix=c(x='_tot',y='_pa')) 

# connect with HOF data 
fg_bat_career = fg_bat_career %>% left_join(hof, by='playerID',suffix=c(x='',y='_hof'))
fg_bat_career = fg_bat_career %>% left_join(master, by='playerID',suffix=c(x='',y='_master'))
fg_bat_career[is.na(fg_bat_career$inducted),'inducted'] = 'N'

# connect with years in league data created earlier 
fg_bat_career = fg_bat_career %>% left_join(distinct(fg_bat[c('playerID','first_season','last_season','years_in_league')]), by='playerID',suffix=c(x='',y='_hof'))


# drop players who are in HOF for non-playing reasons (management, umpires, etc.)
mgr_hof = c('Walter Alston', 'Sparky Anderson', 'Jocko Conlan','Bobby Cox', 'Leo Durocher', 
            'Bucky Harris', 'Whitey Herzog', 'Tony LaRussa', 'Al Lopez', 'Bill McKechnie',
            'Branch Rickey', 'Wilbert Robinson', 'Dick Williams', 'Billy Southworth', 'Casey Stengel')

mgr_ids = in_hof[in_hof$Name %in% mgr_hof,'playerID']
fg_bat_career = fg_bat_career[!(fg_bat_career$playerID %in% as.list(mgr_ids)$playerID),]

fg_bat_career$primary_decade = (((fg_bat_career$last_season + fg_bat_career$first_season) / 2) %/% 10) * 10

# create flags for players being ineligible for major awards due to them not being created for most/all of their career
fg_bat_career$pre_ws_mvp = ifelse(fg_bat_career$primary_decade<1960,1,0)
fg_bat_career$pre_tsn_poy = ifelse(fg_bat_career$primary_decade<1940,1,0)
fg_bat_career$pre_tsn_ml_poy = ifelse(fg_bat_career$primary_decade<1930,1,0)
fg_bat_career$pre_roy = ifelse(fg_bat_career$first_season<1947,1,0)
fg_bat_career$pre_lcs_mvp = ifelse(fg_bat_career$primary_decade<1980,1,0)
fg_bat_career$pre_mvp = ifelse(fg_bat_career$primary_decade<1910,1,0)

#create decade dummies 
for(level in unique(fg_bat_career$primary_decade)){
  fg_bat_career[paste("decade", level, sep = "_")] <- ifelse(fg_bat_career$primary_decade == level, 1, 0)
}

ggplot(fg_bat_career[(fg_bat_career$AB_tot>100),], aes(x=WAR_g, y=wOBA,color=inducted)) + geom_point()

write.csv(fg_bat_career[fg_bat_career$last_season<2010,],'/users/jledoux/Documents/projects/Saber/baseball-data/predict_hof_bat2.csv')
#write.csv(fg_bat_career,'/users/jledoux/Documents/projects/Saber/baseball-data/predict_hof_bat.csv')
write.csv(fg_bat_career[fg_bat_career$last_season==2017,], '/users/jledoux/Documents/projects/Saber/baseball-data/predict_hof_bat_currents.csv')
#low_war_hof = fg_bat_career[(fg_bat_career$WAR_g<2.5)& (fg_bat_career$AVG<2.7) &(fg_bat_career$inducted=='Y'),'playerID']
#master[master$playerID %in% low_war_hof$playerID,c('nameFirst','nameLast')]

#high_war_nothall = fg_bat_career[(fg_bat_career$WAR_g>5)&(fg_bat_career$inducted=='N'),'playerID']
#master[master$playerID %in% high_war_nothall$playerID,c('nameFirst','nameLast')]

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
