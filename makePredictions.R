library(dplyr)
library(ggplot2)
library(plyr)
library(rvest)

setwd("~/Desktop/Other/Basketball/NCAAB/Teams/NCAAB_team_predictions")




########### LOAD TRAINING DATA ###########

### GET 2020-2023 data and append to training
twenties = read.csv("../teamCSVs/teamStats2020-2023.csv")
colnames(twenties)=gsub("\\.", "%", colnames(twenties))
#   # Two data sets, one to train and one to present that includes 2020?

train = read.csv('../teamCSVs/team_predictions.csv')
colnames(train)=gsub("\\.", "%", colnames(train))
colnames(train)[9] = 'TotalS%'
colnames(train)[4] = 'Exit-Round'

allData = rbind.fill(twenties, train)
allData %>% 
  group_by(Year) %>% 
  dplyr::summarize(count = n())

### Get current season data
curr = read.csv('../teamCSVs/currentSzn.csv')
colnames(curr)=gsub("\\.", "%", colnames(curr))
colnames(curr)[6] = 'TotalS%'

# Add new columns
train = train %>% 
  
         # 3P attempt rate
  mutate(`3PA%` = `X3PA` / FGA,
         `3PA%_Opp` = `X3PA_Opp` / FGA_Opp,
         
         # free throw attemp rate
         FTR = FTA / FGA,
         FTR_Opp = FTA_Opp / FGA_Opp,
         FTR_diff = FTR - FTR_Opp,
         
         # effective FG% difference
         eFG_diff = `eFG%` - `eFG%_Opp`,
         
         # extra possessions per court trip
         extra_poss = 100*(ORB/FGA) + `TOV%_Opp`,
         Opp_extra_poss = 100*(ORB_Opp/FGA_Opp) + `TOV%`,
         adj_poss_diff = extra_poss - Opp_extra_poss,
         
         # numeric tournament result
         y=Result,

         court_trips = DRB + FGM_Opp + FTM_Opp,
         court_trips_opp = DRB_Opp + FGM + FTM,
         
         orb_100_court_trips = (ORB/(FGA_Opp - FGM_Opp))*100,
         tov_100_court_trips = TOV_Opp*(100/court_trips_opp),
         extra_poss_100_court_trips = orb_100_court_trips+tov_100_court_trips,
         
         orb_100_court_trips_opp = (ORB_Opp/(FGA - FGM))*100,
         tov_100_court_trips_opp = TOV*(100/court_trips),
         extra_poss_100_court_trips_opp = orb_100_court_trips_opp+tov_100_court_trips_opp,
         
         orb_diff = orb_100_court_trips - orb_100_court_trips_opp,
         tov_diff = tov_100_court_trips - tov_100_court_trips_opp,
         
         exPoss = extra_poss_100_court_trips - extra_poss_100_court_trips_opp,
         
         ts_diff = `TS%` - `TS%_Opp`)

train$SOS[train$SOS > max(curr$SOS)] = max(curr$SOS)



# train model

mod.fit = glm(y ~ I(AdjEM^4) + 
                  ts_diff + 
                  orb_diff + 
                  tov_diff + 
                  SOS,
              data=train)

summary(mod.fit)


# predictions = data.frame(team = curr['Team'],
#                         team_score = as.numeric(predict(object=mod.fit,
#                                                         newdata=curr,
#                                                          type="response")))


# add new columns
curr = curr %>% 
  
         # extra possessions per court trip
  mutate(extra_poss = 100*(ORB/FGA) + `TOV%_Opp`,
         Opp_extra_poss = 100*(ORB_Opp/FGA_Opp) + `TOV%`,
         adj_poss_diff = as.numeric(extra_poss - Opp_extra_poss),
         
         # 3P attempt rate
         `3PA%` = `X3PA` / FGA,
         `3PA%_Opp` = `X3PA_Opp` / FGA_Opp,
         
         # free throw rate
         FTR = FTA / FGA,
         FTR_Opp = FTA_Opp / FGA_Opp,
         FTR_diff = FTR - FTR_Opp,
         
         # effective FG% difference
         eFG_diff = `eFG%` - `eFG%_Opp`,
         
         # EXTRA
         AdjEM_rate = AdjEM / max(AdjEM),
         SOS_rate = SOS / max(SOS),
         adj_poss_diff_rate = adj_poss_diff / max(adj_poss_diff),
         `Opp_OR%` = 100*(ORB_Opp/FGA_Opp),
         `OR%` = 100*(ORB/FGA),
         #AdjEM_sqrt = case_when(AdjEM < 0 ~ (-1)*sqrt(-1*AdjEM),
         #                       AdjEM >= 0 ~ sqrt(AdjEM)),
         AdjEM_pos = AdjEM + min(AdjEM),
         
         court_trips = DRB + FGM_Opp + FTM_Opp,
         court_trips_opp = DRB_Opp + FGM + FTM,
         
         orb_100_court_trips = (ORB/(FGA_Opp - FGM_Opp))*100,
         tov_100_court_trips = TOV_Opp*(100/court_trips_opp),
         extra_poss_100_court_trips = orb_100_court_trips+tov_100_court_trips,
         
         orb_100_court_trips_opp = (ORB_Opp/(FGA - FGM))*100,
         tov_100_court_trips_opp = TOV*(100/court_trips),
         extra_poss_100_court_trips_opp = orb_100_court_trips_opp+tov_100_court_trips_opp,
         
         orb_diff = orb_100_court_trips - orb_100_court_trips_opp,
         tov_diff = tov_100_court_trips - tov_100_court_trips_opp,
         
         exPoss = extra_poss_100_court_trips - extra_poss_100_court_trips_opp,
         
         ts_diff = `TS%` - `TS%_Opp`)

mod.coef.adjem = mod.fit$coefficients[['I(AdjEM^4)']]-((2.757e-07)*7.5)
mod.coef.int = mod.fit$coefficients[['(Intercept)']]-(0.008493*100)
mod.coef.ts = mod.fit$coefficients[['ts_diff']]+(1.137*4.075)
mod.coef.orb = mod.fit$coefficients[['orb_diff']]+(0.004774*1.5)
mod.coef.tov = mod.fit$coefficients[['tov_diff']]+(0.01153*4.375)
mod.coef.sos = mod.fit$coefficients[['SOS']]+(0.008493*8)





# get offense and defense scores

OnD_scores = function(od, net, rev=F){
  if (rev){od=(-1)*od}
  
  p = pnorm(od, mean(od,na.rm = T), sd(od,na.rm=T))
  #return(qnorm(p, mean(net,na.rm = T), sd(net,na.rm=T)))
  q = qnorm(p, mean(net,na.rm = T), sd(net,na.rm=T))
  return(q)
}

curr$ts_off = OnD_scores(curr$`TS%`, curr$ts_diff)
curr$ts_def = OnD_scores(curr$`TS%_Opp`, curr$ts_diff, rev=T)

curr$orb_off = OnD_scores(curr$orb_100_court_trips, curr$orb_diff)
curr$orb_def = OnD_scores(curr$orb_100_court_trips_opp, curr$orb_diff, rev=T)

curr$tov_off = OnD_scores(curr$tov_100_court_trips_opp, curr$tov_diff, rev=T)
curr$tov_def = OnD_scores(curr$tov_100_court_trips, curr$tov_diff)

curr$eff_off = OnD_scores(curr$AdjOE, curr$AdjEM)
curr$eff_def = OnD_scores(curr$AdjDE, curr$AdjEM, rev=T)




# add scores to current season stats
curr = curr %>% 
  
  mutate(
    
  team_score = case_when(
    AdjEM<0 ~ mod.coef.adjem*(-1)*(AdjEM^4) + mod.coef.ts*ts_diff + mod.coef.orb*orb_diff  + mod.coef.tov*tov_diff + mod.coef.sos*SOS + mod.coef.int - 1,
    AdjEM>=0 ~ mod.coef.adjem*(AdjEM^4) + mod.coef.ts*ts_diff + mod.coef.orb*orb_diff  + mod.coef.tov*tov_diff + mod.coef.sos*SOS + mod.coef.int) - 1,
    
  TOPS.Offense = case_when(
    eff_off<0 ~ mod.coef.adjem*(-1)*(eff_off^4) + mod.coef.ts*ts_off  + mod.coef.orb*orb_off  + mod.coef.tov*tov_off + (mod.coef.sos*SOS) + mod.coef.int - 0.5,
    eff_off>=0 ~ mod.coef.adjem*(eff_off^4) + mod.coef.ts*ts_off  + mod.coef.orb*orb_off  + mod.coef.tov*tov_off + (mod.coef.sos*SOS) + mod.coef.int - 0.5),
    
  TOPS.Defense = case_when(
    eff_def<0 ~ mod.coef.adjem*(-1)*(eff_def^4) + mod.coef.ts*ts_def  + mod.coef.orb*orb_def  + mod.coef.tov*tov_def + (mod.coef.sos*SOS) + mod.coef.int - 0.5,
    eff_def>=0 ~ mod.coef.adjem*(eff_def^4) + mod.coef.ts*ts_def  + mod.coef.orb*orb_def  + mod.coef.tov*tov_def + (mod.coef.sos*SOS) + mod.coef.int - 0.5),
  
  #ETW.Rank = rank(-ETW),
  #ETW.Off.Rank = rank(-ETW.Offense),
  #ETW.Def.Rank = rank(-ETW.Defense),
  ) 




# get a table of just current season etw ratings with inputs values
etwCurrTable = curr %>% 
  
  mutate(
    
    ETW = case_when(
      AdjEM<0 ~ mod.coef.adjem*(-1)*(AdjEM^4) + mod.coef.ts*ts_diff + mod.coef.orb*orb_diff  + mod.coef.tov*tov_diff + mod.coef.sos*SOS + mod.coef.int,
      AdjEM>=0 ~ mod.coef.adjem*(AdjEM^4) + mod.coef.ts*ts_diff + mod.coef.orb*orb_diff  + mod.coef.tov*tov_diff + mod.coef.sos*SOS + mod.coef.int),
    
    ETW.Offense = case_when(
      eff_off<0 ~ mod.coef.adjem*(-1)*(eff_off^4) + mod.coef.ts*ts_off  + mod.coef.orb*orb_off  + mod.coef.tov*tov_off + mod.coef.sos*SOS + mod.coef.int,
      eff_off>=0 ~ mod.coef.adjem*(eff_off^4) + mod.coef.ts*ts_off  + mod.coef.orb*orb_off  + mod.coef.tov*tov_off + mod.coef.sos*SOS + mod.coef.int),
    
    ETW.Defense = case_when(
      eff_def<0 ~ mod.coef.adjem*(-1)*(eff_def^4) + mod.coef.ts*ts_def  + mod.coef.orb*orb_def  + mod.coef.tov*tov_def + mod.coef.sos*SOS + mod.coef.int,
      eff_def>=0 ~ mod.coef.adjem*(eff_def^4) + mod.coef.ts*ts_def  + mod.coef.orb*orb_def  + mod.coef.tov*tov_def + mod.coef.sos*SOS + mod.coef.int),
    
    ETW.Rank = rank(-ETW),
    ETW.Off.Rank = rank(-ETW.Offense),
    ETW.Def.Rank = rank(-ETW.Defense),
  ) %>% 
  
  arrange(desc(ETW)) %>% 
  select(Team,Year, Conference, ETW, ETW.Rank, AdjEM, ts_diff, orb_diff, tov_diff, SOS, 
         ETW.Offense, ETW.Off.Rank, AdjOE, `TS%`, orb_100_court_trips, tov_100_court_trips_opp,
         ETW.Defense, ETW.Def.Rank, AdjDE, `TS%_Opp`, orb_100_court_trips_opp, tov_100_court_trips) 






# get a table of all time etw ratings with inputs values and number of tournament wins

train$ts_off = OnD_scores(train$`TS%`, train$ts_diff)
train$ts_def = OnD_scores(train$`TS%_Opp`, train$ts_diff, rev=T)

train$orb_off = OnD_scores(train$orb_100_court_trips, train$orb_diff)
train$orb_def = OnD_scores(train$orb_100_court_trips_opp, train$orb_diff, rev=T)

train$tov_off = OnD_scores(train$tov_100_court_trips_opp, train$tov_diff, rev=T)
train$tov_def = OnD_scores(train$tov_100_court_trips, train$tov_diff)

train$eff_off = OnD_scores(train$AdjOE, train$AdjEM)
train$eff_def = OnD_scores(train$AdjDE, train$AdjEM, rev=T)

etwAllTimeTable = train %>% 
  
  mutate(
    
    ETW = case_when(
      AdjEM<0 ~ mod.coef.adjem*(-1)*(AdjEM^4) + mod.coef.ts*ts_diff + mod.coef.orb*orb_diff  + mod.coef.tov*tov_diff + mod.coef.sos*SOS + mod.coef.int,
      AdjEM>=0 ~ mod.coef.adjem*(AdjEM^4) + mod.coef.ts*ts_diff + mod.coef.orb*orb_diff  + mod.coef.tov*tov_diff + mod.coef.sos*SOS + mod.coef.int),
    
    ETW.Offense = case_when(
      eff_off<0 ~ mod.coef.adjem*(-1)*(eff_off^4) + mod.coef.ts*ts_off  + mod.coef.orb*orb_off  + mod.coef.tov*tov_off + mod.coef.sos*SOS* + mod.coef.int,
      eff_off>=0 ~ mod.coef.adjem*(eff_off^4) + mod.coef.ts*ts_off  + mod.coef.orb*orb_off  + mod.coef.tov*tov_off + mod.coef.sos*SOS + mod.coef.int),
    
    ETW.Defense = case_when(
      eff_def<0 ~ mod.coef.adjem*(-1)*(eff_def^4) + mod.coef.ts*ts_def  + mod.coef.orb*orb_def  + mod.coef.tov*tov_def + mod.coef.sos*SOS + mod.coef.int,
      eff_def>=0 ~ mod.coef.adjem*(eff_def^4) + mod.coef.ts*ts_def  + mod.coef.orb*orb_def  + mod.coef.tov*tov_def + mod.coef.sos*SOS + mod.coef.int),
    
    ETW.Rank = rank(-ETW),
    ETW.Off.Rank = rank(-ETW.Offense),
    ETW.Def.Rank = rank(-ETW.Defense),
  ) %>% 
  
  arrange(desc(ETW)) %>% 
  select(Team,Year, ETW, ETW.Rank, AdjEM, ts_diff, exPoss, SOS, 
         ETW.Offense, ETW.Off.Rank, AdjOE, `TS%`, extra_poss_100_court_trips_opp, 
         ETW.Defense, ETW.Def.Rank, AdjDE, `TS%_Opp`, extra_poss_100_court_trips) 

#curr$TOPS.Defense[curr$TOPS.Defense>6] = 5.831

#etw.off.weight = abs(curr$TOPS.Offense) / (abs(curr$TOPS.Offense) + abs(curr$TOPS.Defense))
#curr$TOPS.Offense = etw.off.weight*curr$team_score
#curr$TOPS.Defense = (1-etw.off.weight)*curr$team_score

# curr = curr %>% arrange(desc(TOPS.Offense))
# curr$ETW.Off.Rank = seq(1, length(curr$Team))

# curr = curr %>% arrange(desc(TOPS.Defense))
# curr$ETW.Def.Rank = seq(1, length(curr$Team))


### OUTPUT 3 Tables:
  # 1) curr szn with etw ratings and all stats
  # 2) curr szn etw ratings, inputs
  # 3) all time etw ratings, inputs, outputs (n tourney wins)


chk = curr %>% select(Team, team_score)

curr = curr  %>%  arrange(desc(team_score))
curr$Rank = seq(1, length(curr$team_score))
curr = curr  %>%  arrange(desc(SOS))
curr$SOS.Rank = seq(1, length(curr$team_score))
curr = curr  %>%  arrange(desc(team_score))
# WRITE OUT CURRENT SEASON AND SIMPLE PREDICTIONS DFS
#write.csv(predictions, "../teamCSVs/myPredictions.csv", row.names=FALSE)
#write.csv(mod.results, "../teamCSVs/model-inputs.csv", row.names=FALSE)


curr$team_score = (curr$TOPS.Offense + curr$TOPS.Defense) - abs(curr$TOPS.Offense - curr$TOPS.Defense) / 8
etwAllTimeTable$ETW = (etwAllTimeTable$ETW.Offense + etwAllTimeTable$ETW.Defense) - abs(etwAllTimeTable$ETW.Offense - etwAllTimeTable$ETW.Defense) / 8
etwCurrTable$ETW = (etwCurrTable$ETW.Offense + etwCurrTable$ETW.Defense) - abs(etwCurrTable$ETW.Offense - etwCurrTable$ETW.Defense) / 8

curr$Rank = rank(-curr$team_score)
etwAllTimeTable$ETW.Rank = rank(-etwAllTimeTable$ETW)
etwCurrTable$ETW.Rank = rank(-etwCurrTable$ETW)


#chk = curr %>% select(Team, ts_diff, ts_off, ts_def, orb_diff, orb_off, orb_def, tov_diff, tov_off, tov_def, AdjEM, eff_off, eff_def) %>% filter(Team %in% c("North Carolina", "Marquette"))
chk = curr %>% select(Team, 
                      team_score, TOPS.Offense, TOPS.Defense) %>% filter(Team %in% c("North Carolina", "Marquette"))



################################
################################
### MOVE ALL TEAMS DATA CLEANING HERE 
### AND WRITE OUT CLEAN DF WITH CBB DATA INCLUDED
################################
################################


Teams = curr


cbbdata::cbd_login()
library(cbbdata)
#cbbdata::cbd_torvik_player_game
tvResume = cbbdata::cbd_torvik_current_resume()
#tvResults = cbbdata::cbd_torvik_ncaa_results(2003, 2023)
#tvSheets = cbbdata::cbd_torvik_ncaa_sheets(2025)
tvSheets = read_html('https://barttorvik.com/teamsheets.php?year=2025') %>% html_table()
tvSheets = data.frame(tvSheets)
colnames(tvSheets) <- tvSheets[1,]
tvSheets <- tvSheets[-1, ] 
colnames(tvSheets) = tolower(colnames(tvSheets))
tvSheets$team = gsub(' 10', '', tvSheets$team)
tvSheets$team = gsub(' 11', '', tvSheets$team)
tvSheets$team = gsub(' 12', '', tvSheets$team)
tvSheets$team = gsub(' 13', '', tvSheets$team)
tvSheets$team = gsub(' 14', '', tvSheets$team)
tvSheets$team = gsub(' 15', '', tvSheets$team)
tvSheets$team = gsub(' 16', '', tvSheets$team)
tvSheets$team = gsub(' 1', '', tvSheets$team)
tvSheets$team = gsub(' 2', '', tvSheets$team)
tvSheets$team = gsub(' 3', '', tvSheets$team)
tvSheets$team = gsub(' 4', '', tvSheets$team)
tvSheets$team = gsub(' 5', '', tvSheets$team)
tvSheets$team = gsub(' 6', '', tvSheets$team)
tvSheets$team = gsub(' 7', '', tvSheets$team)
tvSheets$team = gsub(' 8', '', tvSheets$team)
tvSheets$team = gsub(' 9', '', tvSheets$team)
tvSheets$team = gsub(' F4O', '', tvSheets$team)
tvSheets$team = gsub(' N4O', '', tvSheets$team)



# char_array = c("foo_bar","bar_foo","apple","beer")
# a = data.frame("data"=char_array,"data2"=1:4)
#tvSheets$team = substr(tvSheets$team,1,nchar(tvSheets$team)-2)
#tvResume = cbbdata::cbd_torvik_similar_resumes()
tvMetrics = cbbdata::cbd_all_metrics()


tv = join(tvMetrics, tvResume, by='team')
tv = join(tv, tvSheets, by='team')


#Teams = read.csv('model-inputs.csv')

# drop duplicate rows
Teams = Teams %>% distinct(Team, .keep_all = TRUE)
colnames(Teams)=gsub("\\.", "%", colnames(Teams))
colnames(Teams)=gsub("SOS%Rank", "SOS.Rank", colnames(Teams))
colnames(Teams)=gsub("TOPS%Offense", "TOPS.Offense", colnames(Teams))
colnames(Teams)=gsub("TOPS%Defense", "TOPS.Defense", colnames(Teams))
colnames(Teams)=gsub("X3PM", "3PM", colnames(Teams))
colnames(Teams)=gsub("X3PA", "3PA", colnames(Teams))
colnames(Teams)=gsub("X3P%", "3P%", colnames(Teams))
colnames(Teams)=gsub("X3PM_Opp", "3PM_Opp", colnames(Teams))
colnames(Teams)=gsub("X3PA_Opp", "3PA_Opp", colnames(Teams))
colnames(Teams)=gsub("X3P%_Opp", "3P%_Opp", colnames(Teams))

Teams = Teams %>% 
  mutate(`2PA` = FGA - `3PA`,
         `2PA_Opp` =  FGA_Opp - `3PA_Opp`,
         `2P%` =  (FGM-`3PM`)/(FGA-`3PA`),
         `2P%_Opp` =  (FGM_Opp-`3PM_Opp`)/(FGA_Opp-`3PA_Opp`),
         `2PM` = FGM - `3PM` )

Teams$`2PA` = round(Teams$`2PA`, 1)
Teams$`2P%`= round(Teams$`2P%`*100, 1)
Teams$`3PA` = round(Teams$`3PA`, 1)
Teams$`3P%`= round(Teams$`3P%`*100, 1)
Teams$`FT%`= round(Teams$`FT%`*100, 1)

Teams = Teams %>% 
  mutate(`3PAp100` = round((`3PA`/Pace)*100, 1),
         `3PAp100_Opp` = round((`3PA_Opp`/Pace_Opp)*100, 1),
         #`3PAp100_pctl` = ntile(`3PAp100`, 100),
         `2PAp100` = round((`2PA`/Pace)*100, 1),
         `2PAp100_Opp` = round((`2PA_Opp`/Pace_Opp)*100, 1),
         #`2PAp100_pctl` = ntile(`2PAp100`, 100),
         `FTAp100` = round((`FTA`/Pace)*100, 1),
         `FTAp100_Opp` = round((`FTA_Opp`/Pace_Opp)*100, 1))
#`FTAp100_pctl` = ntile(`FTAp100`, 100))

Teams = Teams %>% 
  mutate(#`3PAp100` = (`3PA`/AdjTempo)*100,
    `3PAp100_pctl` = ntile(`3PAp100`, 100),
    `3PAp100_Opp_pctl` = ntile(`3PAp100_Opp`, 100),
    #`2PAp100` = (`2PA`/AdjTempo)*100,
    `2PAp100_pctl` = ntile(`2PAp100`, 100),
    `2PAp100_Opp_pctl` = ntile(`2PAp100_Opp`, 100),
    #`FTAp100` = (`FTA`/AdjTempo)*100,
    `FTAp100_pctl` = ntile(`FTAp100`, 100),
    `FTAp100_Opp_pctl` = ntile(`FTAp100_Opp`, 100),
    #`FTR_pctl` = ntile(`FTR`, 100),
    `FTR_Opp_pctl` = ntile(-`FTR_Opp`, 100))

# format numeric values
#Teams$team_score = round(Teams$team_score*100, 2)
Teams$SOS.Rank = round(as.numeric(Teams$SOS.Rank), 0)
Teams = Teams %>% 
  mutate(`Kenpom.AdjEM` = round(AdjEM, 2)) %>% 
  arrange(desc(Kenpom.AdjEM))
Teams$`Kenpom.Rank` = seq(1, length(Teams$`Kenpom.AdjEM`))
# select desired columns and clean column names
#Teams = Teams %>% distinct(Team, .keep_all = TRUE) %>% 
#  select(Team, Conference, team_score,
#         SOS, SOS.Rank, `Kenpom.AdjEM`, `Kenpom.Rank`) %>% 
#  arrange(desc(team_score))
Teams = Teams %>% dplyr::select(-X)
colnames(Teams)[135] = 'TOPS'
colnames(Teams)[2] = 'School'

# add ranks
#Teams['Natl.Rank'] = seq(1, length(Teams$School))install
Teams=Teams %>% 
  group_by(Conference) %>% 
  mutate(Conf.Rank=rank(-TOPS))
Teams = Teams %>% 
  ##  select(School, Conference, TOPS, 
  #         Rank, Conf.Rank, 
  #         SOS, SOS.Rank, `Kenpom.AdjEM`, `Kenpom.Rank`,
  #         eFG_diff, `FT%`, FTR_diff, adj_poss_diff) %>% 
  mutate(PPP = PPG/Pace,
         PPP_Opp = round(PPG_Opp/Pace_Opp,3),
         TOPS = round(TOPS, 3)) %>%
  arrange(desc(TOPS))

Teams$PPP_pctl = ntile(Teams$PPP, 100)
Teams$FTR_pctl = ntile(Teams$FTR, 100)
#Teams$Logo <- paste("..//teamCSVs//", Teams$Logo, sep="")
Teams$Logo[Teams$School == "West Virginia"] = 'logos//West-Virginia-Mountaineers.png'
Teams$Logo[Teams$School == "Lehigh"] = 'logos//Lehigh-Mountain-Hawks.png'
Teams$Logo[Teams$School == "Appalachian State"] = 'logos//Appalachian-State-Mountaineers.png'
Teams$Logo[Teams$School == "Le Moyne"] = 'logos//Le-Moyne.png'

Teams$ETW = Teams$TOPS
Teams$`ETW Offense` = Teams$TOPS.Offense
Teams$`ETW Defense` = Teams$TOPS.Defense
teamsSelect1 = Teams %>% 
  dplyr::select(-School, -`X%_x`, -`TotalS%`, -`X%_Opp_x`, -`Total%S%_Opp`, -PPS, -PPS_Opp, -FIC40, -FIC40_Opp,
                -ORtg_Opp, -DRtg_Opp, -eDiff_Opp, -Poss_Opp, -Pace_Opp, -`X%_y`, -`X%_Opp_y`, -GP_Opp, -MPG_Opp, -TOPS, -TOPS.Offense, -TOPS.Defense,
                -Year, -Year_x, -Year_y, -RankTempo, -RankAdjTempo, -RankOE, -RankAdjOE, -RankDE, -RankAdjDE, -RankAdjEM,
                -Conference, -Logo, -extra_poss, -Opp_extra_poss, -`adj_poss_diff`, -`3PA%`, -`3PA%_Opp`, -FTR_diff, -AdjEM_rate,
                -SOS_rate, -adj_poss_diff_rate, `Opp_OR%`, -`OR%`, -Rank, -SOS.Rank, -`3PAp100_pctl`, -`2PAp100_pctl`,-`FTAp100_pctl`,-PPP_pctl,
                -`Kenpom.AdjEM`, -`Kenpom.Rank`, 
  )
teamsSelect = colnames(teamsSelect1[,2:length(colnames(teamsSelect1))])




p6 = c("Big 12", 'Big East', 'Big Ten', 'SEC', 'ACC', 'Pac-12')
nonP6 = subset(Teams, !(Conference %in% p6))$Conference

p6zags = Teams$School[(Teams$Conference %in% p6) | Teams$School == "Gonzaga"]
nonP6zags = subset(Teams, !(School %in% p6zags))$School

p6zagsDF = Teams[(Teams$Conference %in% p6) | Teams$School == "Gonzaga",]
nonP6zagsDF = subset(Teams, !(School %in% p6zags))



colnames(Teams) = gsub("_Opp", " Allowed", colnames(Teams))
#colnames(Teams) = gsub(".", " ", colnames(Teams))
colnames(Teams) = gsub("Kenpom", "KenPom", colnames(Teams))

colnames(Teams) = gsub("AdjOE", "KenPom AdjOE", colnames(Teams))
colnames(Teams) = gsub("AdjDE", "KenPom AdjDE", colnames(Teams))
colnames(Teams) = gsub("AdjEM", "KenPom AdjEM", colnames(Teams))


Teams = Teams %>% 
  select(-`X%_x`, -`X% Allowed_x`,-`X%_y`, -`X% Allowed_y`, -`TotalS%`, -`Total%S% Allowed`, -FIC40, -`FIC40 Allowed`,
         -`GP Allowed`, -MPG, -`MPG Allowed`, -Year_x, -Year_y, -extra_poss, -Opp_extra_poss, -adj_poss_diff, -extra_poss_100_court_trips,
         -extra_poss_100_court_trips_opp, -FTR_diff, -eFG_diff, -`KenPom AdjEM_rate`, -SOS_rate, -adj_poss_diff_rate, -`OR%`, -`Opp_OR%`,
         -`KenPom AdjEM_pos`, -court_trips, -court_trips_opp, -orb_100_court_trips, -orb_100_court_trips_opp, -tov_100_court_trips, -tov_100_court_trips_opp,
         -orb_diff, -tov_diff, -exPoss, -ts_def, -ts_off, -tov_off, -tov_def, -orb_off, -orb_def, -eff_off, -eff_def, -TOPS, -TOPS.Offense, -TOPS.Defense,
         -Rank, -SOS.Rank, -`3PAp100_pctl`, -`2PAp100_pctl`, -`FTAp100_pctl`, -`3PAp100 Allowed_pctl`, -`2PAp100 Allowed_pctl`, -`FTAp100 Allowed_pctl`,
         -`KenPom.KenPom AdjEM`, -PPP_pctl, -FTR_pctl, -ETW, -`ETW Offense`, -`ETW Defense`, -`ORtg Allowed`, -`DRtg Allowed`, -`eDiff Allowed`,
         -ts_diff, -RankOE, -RankDE, -`RankAdjTempo`,-`Conf.Rank`,-PPS, -`PPS Allowed`, -`Poss Allowed`, -`Pace Allowed`,-Pace,-`RankTempo`,
         -`RankKenPom AdjOE`,-`RankKenPom AdjDE`, -`RankKenPom AdjEM`, `KenPom.Rank`, -`FTR Allowed_pctl`, -`ORtg`, -`DRtg`)

Teams = Teams %>% 
  dplyr::rename(#"Tempo Rank" = "RankTempo",
    "KenPom Adj. Tempo" = "AdjTempo",
    #"Rank KenPom AdjTempo" = "RankAdjTempo",
    "Offensive Rating" = "OE",
    "Defensive Rating" = "DE",
    #"Offensive Rating Rank" = "RankOE",
    #"Defensive Rating Rank" = "RankDE",
    "KenPom Rank" = "KenPom.Rank",
    "3PA/100 Poss." = "3PAp100",
    "2PA/100 Poss." = "2PAp100",
    "FTA/100 Poss." = "FTAp100",
    "3PA/100 Poss. Allowed" = "3PAp100 Allowed",
    "2PA/100 Poss. Allowed" = "2PAp100 Allowed",
    "FTA/100 Poss. Allowed" = "FTAp100 Allowed",
    "Net Rating" = "eDiff",
    "3PAr" = "3PA%",
    "3PAr Allowed" = "3PA% Allowed",
    "Forced TOV/G" = "TOV Allowed",
    "Forced TOV%" = "TOV% Allowed",
    "AST/G" = "APG",
    "ORB/G" = "ORB",
    "DRB/G" = "DRB",
    "STL/G" = "SPG",
    "BLK/G" = "BPG",
    "FGA/G" = "FGA",
    "FGM/G" = "FGM",
    "3PA/G" = "3PA",
    "2PA/G" = "2PA",
    "FTA/G" = "FTA",
    "3PM/G" = "3PM",
    "2PM/G" = "2PM",
    "FTM/G" = "FTM",
    "TRB/G" = "RPG",
    "PF/G" = "PF",
    "AST/G Allowed" = "APG Allowed",
    "ORB/G Allowed" = "ORB Allowed",
    "DRB/G Allowed" = "DRB Allowed",
    "STL/G Allowed" = "SPG Allowed",
    "BLK/G Allowed" = "BPG Allowed",
    "FGA/G Allowed" = "FGA Allowed",
    "FGM/G Allowed" = "FGM Allowed",
    "3PA/G Allowed" = "3PA Allowed",
    "2PA/G Allowed" = "2PA Allowed",
    "FTA/G Allowed" = "FTA Allowed",
    "3PM/G Allowed" = "3PM Allowed",
    #"2PM/G Allowed" = "2PM Allowed",
    "FTM/G Allowed" = "FTM Allowed",
    "TRB/G Allowed" = "RPG Allowed",
    "PF/G Allowed" = "PF Allowed",
    "Total Possessions" = "Poss",
    "Possessions/G" = "Tempo",
    "Strength of Schedule" = "SOS",
    "TOV/G" = "TOV",
    "Games Played" = "GP")

Teams$`TS%` = Teams$`TS%`*100
Teams$`eFG%` = Teams$`eFG%`*100
Teams$`FG%` = Teams$`FG%`*100

Teams$`3PAr` = Teams$`3PAr`*100
Teams$`3PAr Allowed` = Teams$`3PAr Allowed`*100
Teams$`2P% Allowed` = Teams$`2P% Allowed`*100
Teams$`FT% Allowed` = Teams$`FT% Allowed`*100

Teams$`TS% Allowed` = Teams$`TS% Allowed`*100
Teams$`eFG% Allowed` = Teams$`eFG% Allowed`*100
Teams$`FG% Allowed` = Teams$`FG% Allowed`*100
Teams$`FTR Allowed` = Teams$`FTR Allowed`*100
Teams$`FTR` = Teams$`FTR`*100



tv$team = gsub("St\\.$", "State", tv$team)
#tv$team = gsub("^St\\.", "Saint", tv$team)
#tv$team = gsub("St\\.", "State", tv$team)
tv$team = gsub("College of Charleston", "Charleston", tv$team)
tv$team = gsub("^Connecticut", "UConn", tv$team)
tv$team = gsub("Central Uconn", "UConn", tv$team)
tv$team = gsub("Boston University", "Boston U", tv$team)
tv$team = gsub("FIU", "Florida International", tv$team)
tv$team = gsub("Central Connecticut", "Central Connecticut State", tv$team)
tv$team = gsub("Louisiana Lafayette", "Louisiana", tv$team)
tv$team = gsub("Middle Tennessee", "Middle Tennessee State", tv$team)
tv$team = gsub("VMI", "Virginia Military", tv$team)
#tv$team = gsub("Detroit", "Detroit Mercy", tv$team)
tv$team = gsub("LIU Brooklyn", "Long Island", tv$team)
tv$team = gsub("Massachusetts", "UMass", tv$team)
tv$team = gsub("^Mississippi$", "Ole Miss", tv$team)
tv$team = gsub("Mount State Mary's", "Mount Saint Mary's", tv$team)
tv$team = gsub("North Carolina State", "NC State", tv$team)
tv$team = gsub("Nebraska Omaha", "Omaha", tv$team)
tv$team = gsub("Saint Bonaventure", "St. Bonaventure", tv$team)
tv$team = gsub("Cal St\\.", "Cal State", tv$team)
tv$team = gsub("Mount St\\.", "Mount Saint", tv$team)
tv$team = gsub("F\\.", "F", tv$team)
tv$team = gsub("UTSA", "UT San Antonio", tv$team)
tv$team = gsub("Texas A&M Corpus Chris", "Texas A&M CC", tv$team)
tv$team = gsub("McNeese State", "McNeese", tv$team)
tv$team = gsub("Nicholls State", "Nicholls", tv$team)
tv$team = gsub("Southeast Missouri State", "Southeast Missouri", tv$team)
tv$team = gsub("East Texas A&M", "Texas A&M Commerce", tv$team)
tv$team = gsub("SIU Edwardsville", "SIUE", tv$team)
tv$team = gsub("Cal State Northridge", "CSUN", tv$team)

tv$team = gsub("Purdue Fort Wayne", "IU Indy", tv$team)
tv$team = gsub("N.C. State", "NC State", tv$team)
tv$team = gsub("UMKC", "Kansas City", tv$team)
tv$team = gsub("LIU", "Long Island", tv$team)
tv$team = gsub("Saint Francis", "Saint Francis", tv$team)
#tv$team = gsub("Detroit Mercy Mercy", "Detriot Mercy", tv$team)
Teams$School = gsub("East Texas A&M", "Texas A&M Commerce", Teams$School)

# one = sort(tv$team)
# two = sort(Teams$School)

# for (i in 1:length(one)){
#   #print(i)
#   if (one[i] != two[i]) {
#     print(paste(one[i],"|", two[i]))
#   }
# }
colnames(tv) = make.unique(colnames(tv))
tv = tv %>% dplyr::filter(team != "Team")

for (i in tv$team){
  if (!(i %in% Teams$School)){
    print(i)
  }
}

#newRow = data.frame(t(c("Texas A&M Commerce", rep(NA, 86))))
#colnames(newRow) = colnames(Teams)
#Teams = rbind(Teams, newRow)


Teams = merge(Teams, tv, by.x = 'School', by.y="team")

colnames(Teams) = gsub("_", " ", colnames(Teams))
colnames(Teams) = gsub("trank", "T-Rank", colnames(Teams))
colnames(Teams) = gsub("def$", "AdjDE", colnames(Teams))
colnames(Teams) = gsub("off$", "AdjOE", colnames(Teams))
colnames(Teams) = gsub("def ", "AdjDE ", colnames(Teams))
colnames(Teams) = gsub("off ", "AdjOE ", colnames(Teams))
colnames(Teams) = gsub("tempo", "Tempo", colnames(Teams))
colnames(Teams) = gsub("rank", "Rank", colnames(Teams))
colnames(Teams) = gsub("bpi", "ESPN BPI", colnames(Teams))
colnames(Teams) = gsub("kpi", "KPI", colnames(Teams))
colnames(Teams) = gsub("barthag", "T-Rank BARTHAG", colnames(Teams))
colnames(Teams) = gsub("t Rank", "T-Rank", colnames(Teams))
colnames(Teams) = gsub("wab", "Wins Above Bubble", colnames(Teams))
colnames(Teams) = gsub("net", "NET", colnames(Teams))
colnames(Teams) = gsub("neT-Rank", "NET Rank", colnames(Teams))
colnames(Teams) = gsub("NET Rank AdjEM", "NET Rank", colnames(Teams))
colnames(Teams) = gsub("ESPN BPI AdjOE", "ESPN BPI Offense", colnames(Teams))
colnames(Teams) = gsub("ESPN BPI AdjDE", "ESPN BPI Defense", colnames(Teams))
colnames(Teams) = gsub("BPI sos", "SOS", colnames(Teams))
colnames(Teams) = gsub("BPI sor", "SOR", colnames(Teams))
colnames(Teams) = gsub("ESPN BPI rating", "ESPN BPI", colnames(Teams))
Teams$`T-Rank AdjEM` = Teams$`T-Rank AdjOE` - Teams$`T-Rank AdjDE`

Teams = Teams[ , !(names(Teams) %in% c('L'))]

q1 = Teams %>%
  tidyr::separate(quad1, c("W", "L"), "-") %>% 
  dplyr::select("W", "L") %>% 
  dplyr::mutate(W = as.numeric(W),
                L = as.numeric(L),
                G = W + L,
                pct = case_when(
                                G == 0 ~ 0,
                                TRUE ~ W / G))

Teams$`Q1 Games Played` = q1$`G`
Teams$`Q1 Wins` = q1$`W`
Teams$`Q1 Win %` = q1$`pct`


q2 = Teams %>%
  tidyr::separate(quad2, c("W", "L"), "-") %>% 
  select("W", "L") %>% 
  mutate(W = as.numeric(W),
         L = as.numeric(L),
         G = W + L,
         pct = case_when(
           G == 0 ~ 0,
           TRUE ~ W / G))

Teams$`Q2 Games Played` = q2$`G`
Teams$`Q2 Wins` = q2$`W`
Teams$`Q2 Win %` = q2$`pct`

q3 = Teams %>%
  tidyr::separate(quad3, c("W", "L"), "-") %>% 
  select("W", "L") %>% 
  mutate(W = as.numeric(W),
         L = as.numeric(L),
         G = W + L,
         pct = case_when(
           G == 0 ~ 0,
           TRUE ~ W / G))

Teams$`Q3 Games Played` = q3$`G`
Teams$`Q3 Wins` = q3$`W`
Teams$`Q3 Win %` = q3$`pct`

q4 = Teams %>%
  tidyr::separate(quad4, c("W", "L"), "-") %>% 
  select("W", "L") %>% 
  mutate(W = as.numeric(W),
         L = as.numeric(L),
         G = W + L,
         pct = case_when(
           G == 0 ~ 0,
           TRUE ~ W / G))

Teams$`Q4 Games Played` = q4$`G`
Teams$`Q4 Wins` = q4$`W`
Teams$`Q4 Win %` = q4$`pct`

Teams$`Q1+Q2 Games Played` =  Teams$`Q1 Games Played` + Teams$`Q2 Games Played`
Teams$`Q1+Q2 Wins` =  Teams$`Q1 Wins` + Teams$`Q2 Wins`
Teams$`Q1+Q2 Win %` =  (q1$`W` +  q2$`W`) / (q1$`G` +  q2$`G`)
Teams$`Q1+Q2 Win %`[is.na(Teams$`Q1+Q2 Win %`)] = 0

Teams$`Q3+Q4 Games Played` =  Teams$`Q3 Games Played` + Teams$`Q4 Games Played`
Teams$`Q3+Q4 Wins` =  Teams$`Q3 Wins` + Teams$`Q4 Wins`
Teams$`Q3+Q4 Win %` =  (q3$`W` +  q4$`W`) / (q3$`G` +  q4$`G`)

Teams$`Q3+Q4 Win %`[is.na(Teams$`Q3+Q4 Win %`)] = 0

Teams <- Teams %>% dplyr::select(-contains('quad'))
#Teams <- Teams %>% dplyr::select(-contains('Rank'))
Teams <- Teams %>% dplyr::select(-contains('resume'))
Teams <- Teams %>% dplyr::select(-contains('kp'))
Teams <- Teams %>% dplyr::select(-contains('qual'))

Teams$`Win %` = Teams$wins / (Teams$wins + Teams$losses)
Teams <- Teams %>% dplyr::select(-`adj o`, -`adj d`, -`adj o rk`, -`adj d rk`,
                                 -`adj t`,-`adj t rk`,-conf, -conf.1, -seed, -seed,
                                 -`Wins Above Bubble.1`, -losses, -wins, -elo, -NET.1,
                                 -`T-Rank AdjOE Rank`, -`T-Rank AdjDE Rank`,
                                 -`T-Rank Tempo Rank`, -`ESPN BPI Rank`,
                                 -`T-Rank rating`, -`T-Rank Rank`,
                                 -`Wins Above Bubble Rank`, -`Wins Above Bubble rk`,
                                 -`ESPN BPI Defense Rank`,-`ESPN BPI Offense Rank`,
                                 -rk)


Teams = Teams %>% distinct(School, .keep_all = TRUE)
curr = curr %>% distinct(Team, .keep_all = TRUE)




write.csv(curr, "../teamCSVs/model-inputs.csv", row.names=FALSE)
write.csv(Teams, "../teamCSVs/model-inputs-2.csv", row.names=FALSE)
write.csv(Teams, "../NCAAB_team_predictions/model-inputs.csv", row.names=FALSE)


write.csv(etwCurrTable, "../teamCSVs/etwCurrSzn.csv", row.names=FALSE)
write.csv(etwAllTimeTable, "../teamCSVs/etwAllTime.csv", row.names=FALSE)










####################################
############## OLD #################
####################################





########### train model #############
# mod.fit = glm(y ~ exp(eFG_diff) +
#                 `FT%` +
#                 exp(FTR_diff) +
#                 adj_poss_diff +
#                 SOS +
#                 I(sqrt(AdjEM)),
#               data=train)
# 
# summary(mod.fit)



# ################## MAKE PREDICTIONS ##################
# predictions = data.frame(team = curr['Team'],
#                          team_score = as.numeric(predict(object=mod.fit,
#                                                          newdata=curr,
#                                                          type="response")))
# 
# # add rank column
# predictions = predictions %>% arrange(desc(team_score))
# predictions['Rank'] = seq(1, length(predictions$team_score))
# #print("AHHHHHHHHHHHHHH HELP!!!!!!!!!!!!!!")
# # top 25 DF
# topPred = predictions %>% top_n(-25)
# 
# # merge predictions on full data
# curr = merge(curr, predictions, by = "Team")
# 
# # adjust TOPS scale
# #curr$team_score = curr$team_score / max(predict(mod.fit, train))
# 
# # results with inputs DF
# mod.results = curr %>% 
#   select(Team, Conference, Rank, team_score, eFG_diff,
#          `FT%`, FTR_diff, adj_poss_diff,
#          SOS, AdjEM) %>% 
#   arrange(Rank)
# 
# OnD_scores = function(od, net, rev=F){
#   if (rev){od=(-1)*od}
#   p = pnorm(od, mean(od,na.rm = T), sd(od,na.rm=T))
#   q = qnorm(p, mean(net,na.rm = T), sd(net,na.rm=T))
#   return(q)
# }
# t = curr$Team
# v = curr$AdjOE
# v2 = curr$AdjEM
# OnD_scores(curr$AdjOE, curr$AdjEM)
# OnD_scores(curr$AdjDE, curr$AdjEM, rev=T)
# chk=data.frame(t,v,v2,p,q)
# 
# mod.fit$coefficients
# 
# manual.prediction = function(data){
#   
#   data = data %>% 
#     mutate(
#       # adjEM_pctl = ntile(AdjEM, length(data$AdjEM)),
#       #      eFG_diff_pctl = ntile(eFG_diff, length(data$AdjEM)),
#       #      FT_pctl = ntile(`FT%`, length(data$AdjEM)),
#       #      FTR_diff_pctl = ntile(FTR_diff, length(data$AdjEM)),
#       #      adj_poss_diff_pctl = ntile(adj_poss_diff, length(data$AdjEM)),
#       #      sos_pctl = ntile(SOS, length(data$AdjEM)),
#       #      
#       #      adjOE_pctl = ntile(AdjOE, length(data$AdjEM)),
#       #      eFG_pctl = ntile(`eFG%`, length(data$AdjEM)),
#       #      FTR_pctl = ntile(FTR, length(data$AdjEM)),
#       #      extra_poss_pctl = ntile(extra_poss, length(data$AdjEM)),
#       #     
#       #      adjDE_pctl = ntile(-AdjDE, length(data$AdjEM)),
#       #      eFG_opp_pctl = ntile(-`eFG%_Opp`, length(data$AdjEM)),
#       #      FTR_opp_pctl = ntile(-`FTR_Opp`, length(data$AdjEM)),
#       #      opp_extra_poss_pctl = ntile(-Opp_extra_poss, length(data$AdjEM)),
#       #      
#       #      # get offensive input values
#       #      adjEM_off = quantile(AdjEM, adjOE_pctl/length(data$AdjEM)),
#       #      eFG_off = quantile(`eFG_diff`, eFG_pctl/length(data$AdjEM)),
#       #      FTR_off = quantile(FTR_diff, FTR_pctl/length(data$AdjEM)),
#       #      extra_poss_off = quantile(adj_poss_diff, extra_poss_pctl/length(data$AdjEM)),
#       #      
#       #      sos_off = quantile(SOS, sos_pctl/length(data$AdjEM)),
#       #      ft_off = quantile(`FT%`, FT_pctl/length(data$AdjEM)),
#       #      
#       #      # get defensive input values
#       #      adjEM_def = quantile(AdjEM, adjDE_pctl/length(data$AdjEM)),
#       #      eFG_def = quantile(`eFG_diff`, eFG_opp_pctl/length(data$AdjEM)),
#       #      FTR_def = quantile(FTR_diff, FTR_opp_pctl/length(data$AdjEM)),
#       #      extra_poss_def = quantile(adj_poss_diff, opp_extra_poss_pctl/length(data$AdjEM))
#       
#       #   get offensive input values
#            adjEM_off = OnD_scores(data$AdjOE, data$AdjEM),
#            eFG_off = OnD_scores(data$`eFG%`, data$`eFG_diff`),
#            FTR_off = OnD_scores(data$FTR, data$FTR_diff),
#            
#            extra_poss_off = OnD_scores(data$Opp_extra_poss, data$adj_poss_diff, rev=T),
#            sos_off = data$SOS,
#            ft_off = data$`FT%`,
# 
#            # get defensive input values
#            adjEM_def = OnD_scores(data$AdjDE, data$AdjEM, rev=T),
#            eFG_def = OnD_scores(data$`eFG%_Opp`, data$`eFG_diff`, rev=T),
#            FTR_def = OnD_scores(data$FTR_Opp, data$FTR_diff, rev=T),
#            extra_poss_def = OnD_scores(data$extra_poss, data$adj_poss_diff),
#            
#       )
#   
#   #data.pctls.pos = data.pctls %>% filter(AdjEM>=0)
#   #data.pctls.neg = data.pctls %>% filter(AdjEM0)
#   
#   
#   
#   
#   # overall scores
#   
#   data.neg = data %>% 
#     filter(AdjEM<0) %>% 
#     mutate(team_score = (-1)*sqrt((-1)*AdjEM)*(mod.fit$coefficients[['I(sqrt(AdjEM))']]-(0)) + 
#                         exp(eFG_diff)*(mod.fit$coefficients[['exp(eFG_diff)']]+1.92004/4) + 
#                         `FT%`*mod.fit$coefficients[['`FT%`']] + 
#                         exp(FTR_diff)*(mod.fit$coefficients[['exp(FTR_diff)']]+0.56140) + 
#                         adj_poss_diff*mod.fit$coefficients[['adj_poss_diff']] + 
#                         SOS*(mod.fit$coefficients[['SOS']]+(0.01165*6))+
#              (mod.fit$coefficients[['(Intercept)']])-1,
#            
#            TOPS.Offense = (-1)*sqrt((-1)*adjEM_off)*(mod.fit$coefficients[['I(sqrt(AdjEM))']]-(0)) + 
#              exp(eFG_off)*(mod.fit$coefficients[['exp(eFG_diff)']]+1.92004/4) + 
#              ft_off*mod.fit$coefficients[['`FT%`']] + 
#              exp(FTR_off)*(mod.fit$coefficients[['exp(FTR_diff)']]+0.56140) + 
#              extra_poss_off*mod.fit$coefficients[['adj_poss_diff']] + 
#              sos_off*(mod.fit$coefficients[['SOS']]+(0.01165*6))+
#              (mod.fit$coefficients[['(Intercept)']])-1,
#            
#            TOPS.Defense = (-1)*sqrt((-1)*adjEM_def)*(mod.fit$coefficients[['I(sqrt(AdjEM))']]-(0)) + 
#              exp(eFG_def)*(mod.fit$coefficients[['exp(eFG_diff)']]+1.92004/4) + 
#              ft_off*mod.fit$coefficients[['`FT%`']] + 
#              exp(FTR_def)*(mod.fit$coefficients[['exp(FTR_diff)']]+0.56140) + 
#              extra_poss_def*mod.fit$coefficients[['adj_poss_diff']] + 
#              sos_off*(mod.fit$coefficients[['SOS']]+(0.01165*6))+
#              (mod.fit$coefficients[['(Intercept)']])-1)
#   
#   data.pos = data %>% 
#     filter(AdjEM>=0) %>% 
#     mutate(team_score = sqrt(AdjEM)*(mod.fit$coefficients[['I(sqrt(AdjEM))']]-(0)) + 
#              exp(eFG_diff)*(mod.fit$coefficients[['exp(eFG_diff)']]+1.92004/4) + 
#              `FT%`*mod.fit$coefficients[['`FT%`']] + 
#              exp(FTR_diff)*(mod.fit$coefficients[['exp(FTR_diff)']]+0.56140) + 
#              adj_poss_diff*mod.fit$coefficients[['adj_poss_diff']] + 
#              SOS*(mod.fit$coefficients[['SOS']]+(0.01165*6))+
#              (mod.fit$coefficients[['(Intercept)']])-1, 
#            
#            TOPS.Offense = sqrt(adjEM_off)*(mod.fit$coefficients[['I(sqrt(AdjEM))']]-(0)) + 
#              exp(eFG_off)*(mod.fit$coefficients[['exp(eFG_diff)']]+1.92004/4) + 
#              ft_off*mod.fit$coefficients[['`FT%`']] + 
#              exp(FTR_off)*(mod.fit$coefficients[['exp(FTR_diff)']]+0.56140) + 
#              
#              extra_poss_off*mod.fit$coefficients[['adj_poss_diff']] + 
#              sos_off*(mod.fit$coefficients[['SOS']]+(0.01165*6))+
#              (mod.fit$coefficients[['(Intercept)']])-1,
#            
#            TOPS.Defense = sqrt(adjEM_def)*(mod.fit$coefficients[['I(sqrt(AdjEM))']]-(0)) + 
#              exp(eFG_def)*(mod.fit$coefficients[['exp(eFG_diff)']]+1.92004/4) + 
#              ft_off*mod.fit$coefficients[['`FT%`']] + 
#              exp(FTR_def)*(mod.fit$coefficients[['exp(FTR_diff)']]+0.56140) + 
#              extra_poss_def*mod.fit$coefficients[['adj_poss_diff']] + 
#              sos_off*(mod.fit$coefficients[['SOS']]+(0.01165*6))+
#              (mod.fit$coefficients[['(Intercept)']])-1)
#   
#   
#   data = rbind(data.pos, data.neg) %>% arrange(desc(team_score))
#   
#   data = data %>% 
#     mutate(TOPS.Offense = case_when(
#       (adjEM_off >= 0 & is.na(TOPS.Offense)) ~ sqrt(adjEM_off)*(mod.fit$coefficients[['I(sqrt(AdjEM))']]-(0)) + 
#         exp(eFG_off)*(mod.fit$coefficients[['exp(eFG_diff)']]+1.92004/4) + 
#         ft_off*mod.fit$coefficients[['`FT%`']] + 
#         exp(FTR_off)*(mod.fit$coefficients[['exp(FTR_diff)']]+0.56140) + 
#         extra_poss_off*mod.fit$coefficients[['adj_poss_diff']] + 
#         sos_off*(mod.fit$coefficients[['SOS']]+(0.01165*6))+
#         (mod.fit$coefficients[['(Intercept)']])-1,
#       
#       (adjEM_off < 0 & is.na(TOPS.Offense)) ~ (-1)*sqrt((-1)*adjEM_off)*(mod.fit$coefficients[['I(sqrt(AdjEM))']]-(0)) + 
#         exp(eFG_off)*(mod.fit$coefficients[['exp(eFG_diff)']]+1.92004/4) + 
#         ft_off*mod.fit$coefficients[['`FT%`']] + 
#         exp(FTR_off)*(mod.fit$coefficients[['exp(FTR_diff)']]+0.56140) + 
#         extra_poss_off*mod.fit$coefficients[['adj_poss_diff']] + 
#         sos_off*(mod.fit$coefficients[['SOS']]+(0.01165*6))+
#         (mod.fit$coefficients[['(Intercept)']])-1,
#       
#       TRUE ~ TOPS.Offense)) %>% 
#     
#     mutate(TOPS.Defense = case_when(  
#       (adjEM_def >= 0 & is.na(TOPS.Defense)) ~ sqrt(adjEM_def)*(mod.fit$coefficients[['I(sqrt(AdjEM))']]-(0)) + 
#         exp(eFG_def)*(mod.fit$coefficients[['exp(eFG_diff)']]+1.92004/4) + 
#         ft_off*mod.fit$coefficients[['`FT%`']] + 
#         exp(FTR_def)*(mod.fit$coefficients[['exp(FTR_diff)']]+0.56140) + 
#         extra_poss_def*mod.fit$coefficients[['adj_poss_diff']] + 
#         sos_off*(mod.fit$coefficients[['SOS']]+(0.01165*6))+
#         (mod.fit$coefficients[['(Intercept)']])-1,
#       
#       (adjEM_def < 0 & is.na(TOPS.Defense)) ~ (-1)*sqrt((-1)*adjEM_def)*(mod.fit$coefficients[['I(sqrt(AdjEM))']]-(0)) + 
#         exp(eFG_def)*(mod.fit$coefficients[['exp(eFG_diff)']]+1.92004/4) + 
#         ft_off*mod.fit$coefficients[['`FT%`']] + 
#         exp(FTR_def)*(mod.fit$coefficients[['exp(FTR_diff)']]+0.56140) + 
#         extra_poss_def*mod.fit$coefficients[['adj_poss_diff']] + 
#         sos_off*(mod.fit$coefficients[['SOS']]+(0.01165*6))+
#         (mod.fit$coefficients[['(Intercept)']])-1,
#       
#       TRUE ~ TOPS.Defense
#     ))
#   
#   return(data)
# 
# }
# 
# curr = manual.prediction(curr)
# 
# curr$
# 
# test = manual.prediction(curr)
# test = test  %>%  arrange(desc(team_score))
# test['Rank'] = seq(1, length(test$team_score))
# test = test %>%
#   select(Team, Conference, Rank, team_score, TOPS.Offense, TOPS.Defense, eFG_diff,
#          `FT%`, FTR_diff, adj_poss_diff,
#          SOS, AdjEM, adjEM_off, eFG_off, FTR_off, ft_off,
#          extra_poss_off, sos_off, adjEM_def, eFG_def, FTR_def, extra_poss_def)
# 
# test.train = manual.prediction(train)
# test.train = test.train %>%
#   select(Team, team_score, eFG_diff,
#          `FT%`, FTR_diff, adj_poss_diff,
#          SOS, AdjEM, `Exit-Round`, y)
# 
# # # manually apply model formula to negative Adj EM DFs
# # mod.results.neg = curr %>% 
# #   filter(AdjEM<0) %>% 
# #   mutate(team_score = (-1)*sqrt((-1)*AdjEM)*mod.fit$coefficients[['I(sqrt(AdjEM))']] + exp(eFG_diff)*mod.fit$coefficients[['exp(eFG_diff)']] + `FT%`*mod.fit$coefficients[['`FT%`']] + exp(FTR_diff)*mod.fit$coefficients[['exp(FTR_diff)']] + adj_poss_diff*mod.fit$coefficients[['adj_poss_diff']] + SOS*mod.fit$coefficients[['SOS']]+(-2.30584446))
# # 
# # mod.results.neg.check = mod.results.neg%>% 
# #   select(Team, Conference, Rank, team_score, eFG_diff,
# #          `FT%`, FTR_diff, adj_poss_diff,
# #          SOS, AdjEM)
# # # put together full DF with all ratings
# # mod.results.pos = curr %>% filter(AdjEM>00)
# # 
# # mod.results.pos.check = mod.results.pos%>% 
# #   select(Team, Conference, Rank, team_score, eFG_diff,
# #          `FT%`, FTR_diff, adj_poss_diff,
# #          SOS, AdjEM)
# # 
# # 
# # 
# # curr = rbind(mod.results.pos, mod.results.neg)
# # t = curr %>% select(Team, team_score)
# # add rank columns for TOPS and SOS
# 
# 
# # curr = curr %>%
# #   mutate(Off.Ratio = abs(TOPS.Offense)/(abs(TOPS.Offense)+abs(TOPS.Defense)),
# #          Def.Ratio = abs(TOPS.Defense)/(abs(TOPS.Offense)+abs(TOPS.Defense)),
# #          TOPS.Offense = team_score*Off.Ratio,
# #          TOPS.Defense = team_score*Def.Ratio,
# #          #team_score=TOPS.Offense+TOPS.Defense
# #          )
# 
# curr = curr %>% mutate(team_score = (TOPS.Offense + TOPS.Defense)/2,
#                        TOPS.Offense = TOPS.Offense/2,
#                        TOPS.Defense = TOPS.Defense/2)
# 
# curr = curr  %>%  arrange(desc(team_score))
# curr$Rank = seq(1, length(curr$team_score))
# curr = curr  %>%  arrange(desc(SOS))
# curr$SOS.Rank = seq(1, length(curr$team_score))
# 
# # WRITE OUT CURRENT SEASON AND SIMPLE PREDICTIONS DFS
# write.csv(predictions, "../teamCSVs/myPredictions.csv", row.names=FALSE)
# # #write.csv(mod.results, "../teamCSVs/model-inputs.csv", row.names=FALSE)
# write.csv(curr, "../teamCSVs/model-inputs.csv", row.names=FALSE)