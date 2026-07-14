### 0.1) Imports ###
library(shiny)
library(dplyr)
library(shinydashboard)
#library(shinydashboardPlus)
library(DT)
library(ggplot2)
library(plotly)
library(ggthemes)
library(english)
library(toOrdinal)
library(shinydashboardPlus)
library(thematic)
library(bslib)
library(ggbeeswarm)
library(ggrepel)
library(tidyr)
#library(html)
library(ggimage)
thematic::thematic_on()

update = read.csv("update.csv")$A[1]
new=as.Date(update, "%B %d, %Y")
update = format(new, "%B %e, %Y")
#new=update
ap = read.csv("currAP.csv")
pcsDF = read.csv("pcsDF.csv")
colnames(pcsDF)=gsub("\\.", "%", colnames(pcsDF))
#colnames(pcsDF)=gsub("DRtg%1", "DRtg", colnames(pcsDF))
colnames(pcsDF)=gsub("Net%Rating", "Net Rating", colnames(pcsDF))

historicPCS = read.csv("historicPCS_Dec2023.csv")
colnames(historicPCS)=gsub("\\.", "%", colnames(historicPCS))
colnames(historicPCS)=gsub("Net%Rating", "Net Rating", colnames(historicPCS))

etwCurr = read.csv("etwCurrSzn.csv")
colnames(etwCurr)=gsub("\\.", "%", colnames(etwCurr))
etwCurr <- etwCurr %>% 
  dplyr::rename("ETW Rank" = "ETW%Rank",
         "ETW Offense Rank" = "ETW%Off%Rank",
         "ETW Defense Rank" = "ETW%Def%Rank",
         "ETW Offense" = "ETW%Offense",
         "ETW Defense" = "ETW%Defense",
         "TS% Margin" = "ts_diff",
         "OR% Margin" = "orb_diff",
         "TOV Margin/100 Trips" = "tov_diff",
         "OR%" = "orb_100_court_trips",
         "OR% Allowed" = "orb_100_court_trips_opp",
         "TOV/100 Trips" = "tov_100_court_trips_opp",
         "Forced TOV/100 Trips" = "tov_100_court_trips",
         "TS% Allowed" = "TS%_Opp")
etwCurr$`TS% Margin` = etwCurr$`TS% Margin`*100
etwCurr$`TS%` = etwCurr$`TS%`*100
etwCurr$`TS% Allowed` = etwCurr$`TS% Allowed`*100
etwCurr$`TOV Margin/100 Trips` = etwCurr$`TOV Margin/100 Trips`*(-1)
etwCurr = etwCurr %>% arrange(desc(`ETW Offense`))
etwCurr$`ETW Offense Rank` = seq(1, length(etwCurr$Team))
etwCurr = etwCurr %>% arrange(desc(`ETW Defense`))
etwCurr$`ETW Defense Rank` = seq(1, length(etwCurr$Team))

etwAllTime = read.csv("etwAllTime.csv") %>% distinct()
colnames(etwAllTime)=gsub("\\.", "%", colnames(etwAllTime))
colnames(etwAllTime)=gsub("", "%", colnames(etwAllTime))

# https://stackoverflow.com/questions/2181902/how-to-use-an-image-as-a-point-in-ggplot




############################################ 1) IMPORT DATA ######################################################





################# 1.1) Load Team Stats ###########################




#setwd("~/Desktop/Other/Basketball/NCAAB/Players/playerCSVs/pastV2")
# get data
#setwd("~/Desktop/Other/Basketball/NCAAB/Teams/teamCSVs")
TeamStats = read.csv('currentSzn.csv')
TeamStats = TeamStats[!duplicated(TeamStats$Team), ]
# remove extra indices
TeamStats=TeamStats[,3:length(TeamStats)]

# clean column names
colnames(TeamStats)=gsub("\\.", "%", colnames(TeamStats))
colnames(TeamStats)=gsub("X", "", colnames(TeamStats))
TeamStats=TeamStats %>% 
  mutate(`2PA` = round(FGA - `3PA`, 4),
         `2PA_Opp` =  round(FGA_Opp - `3PA_Opp`, 4),
         `2P%` =  round((FGM-`3PM`)/(FGA-`3PA`), 3),
         `2P%_Opp` =  round((FGM_Opp-`3PM_Opp`)/(FGA_Opp-`3PA_Opp`), 3),
         `2PM` = round(FGM - `3PM`, 4),
         AdjOE=round(AdjOE, 2),
         AdjDE=round(AdjDE, 2),
         AdjTempo=round(AdjTempo, 2),
         `2P%`=`2P%`*100,
         `3P%`=`3P%`*100,
         `FT%`=`FT%`*100,
         `TS%`=`TS%`*100,
         `eFG%`=`eFG%`*100,
         `FG%`=`FG%`*100)

# add percentiles for important stats
TeamStats=TeamStats %>% 
  mutate(`TS%_pctl` = ntile(`TS%`, 100),
         `eFG%_pctl` = ntile(`eFG%`, 100),
         `eFG%_Opp_pctl` = ntile(-`eFG%_Opp`, 100),
         `3P%_pctl` = ntile(`3P%`, 100),
         `ORB%_pctl` = ntile(`ORB%`, 100),
         `DRB%_pctl` = ntile(`DRB%`, 100),
         `DRB%_Opp_pctl` = ntile(-`DRB%_Opp`, 100),
         `TOV%_pctl` = ntile(-`TOV%`, 100),
         `TOV%_Opp_pctl` = ntile(`TOV%_Opp`, 100),
         `3PA_pctl` = ntile(`3PA`, 100),
         `FT%_pctl` = ntile(`FT%`, 100),
         `FTA_pctl` = ntile(`FTA`, 100),
         `TS%_Opp_pctl` = ntile(-`TS%_Opp`, 100),
         `AdjOE_pctl` = ntile(AdjOE, 100),
         `AdjDE_pctl` = ntile(-AdjDE, 100),
         `AdjTempo_pctl` = ntile(`AdjTempo`, 100),
         SOS_pctl = ntile(SOS, 100),
         `2P%_pctl` = ntile(`2P%`, 100),
         `2P%_opp_pctl` = ntile(-`2P%_Opp`, 100),
         `3P%_opp_pctl` = ntile(-`3P%_Opp`, 100),
         `FT%_opp_pctl` = ntile(-`FT%_Opp`, 100),
         `2PA_pctl` = ntile(`2PA`, 100),
         `3PM_pctl` = ntile(`3PM`, 100),
         `2PM_pctl` = ntile(`2PM`, 100),
         `FTM_pctl` = ntile(`FTM`, 100),
         `BLK%_pctl` = ntile(`BLK%`, 100),
         `AST%_pctl` = ntile(`AST%`, 100),
         `STL%_pctl` = ntile(`STL%`, 100),
         
         `TS%_rank` = floor(rank(-`TS%`, 100)),
         `eFG%_rank` = floor(rank(-`eFG%`, 100)),
         `eFG%_Opp_rank` = floor(rank(`eFG%_Opp`, 100)),
         `3P%_rank` = floor(rank(-`3P%`, 100)),
         `ORB%_rank` = floor(rank(-`ORB%`, 100)),
         `DRB%_rank` = floor(rank(-`DRB%`, 100)),
         `DRB%_Opp_rank` = floor(rank(`DRB%_Opp`, 100)),
         `TOV%_rank` = floor(rank(`TOV%`, 100)),
         `TOV%_Opp_rank` = floor(rank(-`TOV%_Opp`, 100)),
         `3PA_rank` = floor(rank(-`3PA`, 100)),
         `FT%_rank` = floor(rank(-`FT%`, 100)),
         `FTA_rank` = floor(rank(-`FTA`, 100)),
         `TS%_Opp_rank` = floor(rank(`TS%_Opp`, 100)),
         `AdjOE_rank` = floor(rank(-AdjOE, 100)),
         `AdjDE_rank` = floor(rank(AdjDE, 100)),
         `AdjTempo_rank` = floor(rank(-`AdjTempo`, 100)),
         SOS_rank = floor(rank(-SOS, 100)),
         `2P%_rank` = floor(rank(-`2P%`, 100)),
         `2P%_opp_rank` = floor(rank(`2P%_Opp`, 100)),
         `3P%_opp_rank` = floor(rank(`3P%_Opp`, 100)),
         `FT%_opp_rank` = floor(rank(`FT%_Opp`, 100)),
         `2PA_rank` = floor(rank(-`2PA`, 100)),
         `3PM_rank` = floor(rank(-`3PM`, 100)),
         `2PM_rank` = floor(rank(-`2PM`, 100)),
         `FTM_rank` = floor(rank(-`FTM`, 100)),
         `BLK%_rank` = floor(rank(-`BLK%`, 100)),
         `AST%_rank` = floor(rank(-`AST%`, 100)),
         `STL%_rank` = floor(rank(-`STL%`, 100)))

TeamStats = TeamStats %>% arrange(desc(AdjEM))

t100 = TeamStats[1:150,]

TeamStats$Logo[TeamStats$Team == "West Virginia"] = 'logos//West-Virginia-Mountaineers.png'
TeamStats$Logo[TeamStats$Team == "Lehigh"] = 'logos//Lehigh-Mountain-Hawks.png'
TeamStats$Logo[TeamStats$Team == "Appalachian State"] = 'logos//Appalachian-State-Mountaineers.png'
TeamStats$Logo[TeamStats$Team == "North Carolina"] = 'logos//North-Carolina-Tar-Heels.png'

TeamStats$Logo[TeamStats$Team == "North Carolina"] = 'logos//North-Carolina-Tar-Heels.png'
TeamStats$Logo[TeamStats$Team == "McNeese"] = 'logos//McNeese-State-Cowboys.png'
TeamStats$Logo[TeamStats$Team == "CSUN"] = 'logos//Cal-State-Northridge-Matadors.png'
TeamStats$Logo[TeamStats$Team == "IU Indy"] = 'logos//IUPUI-Jaguars.png'
TeamStats$Logo[TeamStats$Team == "Kansas City"] = 'logos//UMKC-Kangaroos.png'
TeamStats$Logo[TeamStats$Team == "Southeast Missouri"] = 'logos//Southeast-Missouri-State-Redhawks.png'
TeamStats$Logo[TeamStats$Team == "SIUE"] = 'logos//SIU-Edwardsville-Cougars.png'
TeamStats$Logo[TeamStats$Team == "West Georgia"] = 'logos//west-georgia-wolves.png'
TeamStats$Logo[TeamStats$Team == "Nicholls"] = 'logos//Nicholls-State-Colonels.png'


###################### 1.2) Load Teams ###########################


tv = read.csv("shot_quality.csv")
tv$team = tv$Team_Name
tv$team = gsub("St\\.$", "State", tv$team)
tv$team = gsub("^St\\.", "Saint", tv$team)
tv$team = gsub("St\\.", "State", tv$team)
tv$team = gsub("College of Charleston", "Charleston", tv$team)
tv$team = gsub("^Connecticut", "UConn", tv$team)
tv$team = gsub("Central Uconn", "UConn", tv$team)
tv$team = gsub("Boston University", "Boston U", tv$team)
tv$team = gsub("FIU", "Florida International", tv$team)
tv$team = gsub("Central Connecticut", "Central Connecticut State", tv$team)
tv$team = gsub("Louisiana Lafayette", "Louisiana", tv$team)
tv$team = gsub("Middle Tennessee", "Middle Tennessee State", tv$team)
tv$team = gsub("VMI", "Virginia Military", tv$team)
tv$team = gsub("Detroit", "Detroit Mercy", tv$team)
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

tv$team = gsub("N.C. State", "NC State", tv$team)
tv$team = gsub("Saint John's", "St. John's", tv$team)
tv$team = gsub("Saint Thomas", "St. Thomas", tv$team)
tv$team = gsub("Purdue Fort Wayne", "Fort Wayne", tv$team)
tv$team = gsub("Queens of Charlotte", "Charlotte", tv$team)
tv$team = gsub("LIU", "Long Island", tv$team)
tv$team = gsub("Saint Francis PA", "St. Francis PA", tv$team)
tv$team = gsub("Stonehill College", "Stonehill", tv$team)

tv$team = gsub("McNeese State", "McNeese", tv$team)
tv$team = gsub("Nicholls State", "Nicholls", tv$team)
tv$team = gsub("Southeast Missouri State", "Southeast Missouri", tv$team)
tv$team = gsub("East Texas A&M", "Texas A&M Commerce", tv$team)
tv$team = gsub("SIU Edwardsville", "SIUE", tv$team)
tv$team = gsub("Cal State Northridge", "CSUN", tv$team)


tv = tv %>% select(-Conference, -Offensive_Shot_Quality_Rank_Visual, -Defensive_Shot_Quality_Rank_Visual,
                   -Actual_Record, -SQ_Record)

colnames(tv) = gsub("_", " ", colnames(tv))
colnames(tv) = gsub("Adjusted", "Adj.", colnames(tv))
colnames(tv) = gsub("Percentage", "%", colnames(tv))
colnames(tv) = gsub("Record Luck", "SQ Record Luck", colnames(tv))
colnames(tv) = gsub("Offensive Shot Quality", "Shot Quality Adj. Offense", colnames(tv))
colnames(tv) = gsub("Defensive Shot Quality", "Shot Quality Adj. Defense", colnames(tv))
colnames(tv) = gsub("Adj. Shot Quality", "Shot Quality Adj.", colnames(tv))
colnames(tv) = gsub("rate", "Rate", colnames(tv))

TeamsFull = read.csv('model-inputs-2.csv', check.names=FALSE)

#TeamsFull = merge(TeamsFull, tv, by.x = 'School', by.y = 'team')



# get data
Teams = read.csv('model-inputs.csv')


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
         `3PAp100_Opp_pctl` = ntile(-`3PAp100_Opp`, 100),
         `3PAp100_rank` = floor(ntile(-`3PAp100`, 100)),
         `3PAp100_Opp_rank` = floor(ntile(`3PAp100_Opp`, 100)),
         #`2PAp100` = (`2PA`/AdjTempo)*100,
         `2PAp100_pctl` = ntile(`2PAp100`, 100),
         `2PAp100_Opp_pctl` = ntile(-`2PAp100_Opp`, 100),
         `2PAp100_rank` = floor(rank(-`2PAp100`, 100)),
         `2PAp100_Opp_rank` = floor(rank(`2PAp100_Opp`, 100)),
         #`FTAp100` = (`FTA`/AdjTempo)*100,
         `FTAp100_pctl` = ntile(`FTAp100`, 100),
         `FTAp100_Opp_pctl` = ntile(-`FTAp100_Opp`, 100),
         `FTAp100_rank` = floor(rank(-`FTAp100`, 100)),
         `FTAp100_Opp_rank` = floor(rank(`FTAp100_Opp`, 100)),
         #`FTR_pctl` = ntile(`FTR`, 100),
         `FTR_Opp_pctl` = ntile(-`FTR_Opp`, 100),
         `FTR_Opp_rank` = floor(rank(`FTR_Opp`, 100)))

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
Teams$TOPS = Teams$TOPS + 1
colnames(Teams)[2] = 'School'

Teams = Teams %>% arrange(desc(TOPS))
top.team = Teams$School[1]
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
         TOPS = round(TOPS, 3),) %>%
  arrange(desc(TOPS))

Teams$PPP_pctl = ntile(Teams$PPP, 100)
Teams$PPP_opp_pctl = ntile(-Teams$PPP_Opp, 100)
Teams$PPP_rank = floor(rank(-Teams$PPP, 100))
Teams$PPP_opp_rank = floor(rank(Teams$PPP_Opp, 100))
Teams$FTR_pctl = ntile(Teams$FTR, 100)
Teams$FTR_rank = floor(rank(-Teams$FTR, 100))

Teams$Logo[Teams$School == "West Virginia"] = 'logos//West-Virginia-Mountaineers.png'
Teams$Logo[Teams$School == "Lehigh"] = 'logos//Lehigh-Mountain-Hawks.png'
Teams$Logo[Teams$School == "Appalachian State"] = 'logos//Appalachian-State-Mountaineers.png'
Teams$Logo[Teams$School == "North Carolina"] = 'logos//North-Carolina-Tar-Heels.png'

Teams$Logo[Teams$School == "North Carolina"] = 'logos//North-Carolina-Tar-Heels.png'
Teams$Logo[Teams$School == "McNeese"] = 'logos//McNeese-State-Cowboys.png'
Teams$Logo[Teams$School == "CSUN"] = 'logos//Cal-State-Northridge-Matadors.png'
Teams$Logo[Teams$School == "IU Indy"] = 'logos//IUPUI-Jaguars.png'
Teams$Logo[Teams$School == "Kansas City"] = 'logos//UMKC-Kangaroos.png'
Teams$Logo[Teams$School == "Southeast Missouri"] = 'logos//Southeast-Missouri-State-Redhawks.png'
Teams$Logo[Teams$School == "SIUE"] = 'logos//SIU-Edwardsville-Cougars.png'
Teams$Logo[Teams$School == "West Georgia"] = 'logos//west-georgia-wolves.png'
Teams$Logo[Teams$School == "Nicholls"] = 'logos//Nicholls-State-Colonels.png'



Teams$Logo[Teams$School == "Le Moyne"] = 'logos//Le-Moyne.png'

a = Teams %>% filter(Logo=="")

TeamStats$Logo[TeamStats$Logo=="" & TeamStats$Team == "West Virginia"] = 'logos//West-Virginia-Mountaineers.png'
TeamStats$Logo[TeamStats$Logo=="" & TeamStats$Team == "Lehigh"] = 'logos//Lehigh-Mountain-Hawks.png'
TeamStats$Logo[TeamStats$Logo=="" & TeamStats$Team == "Appalachian State"] = 'logos//Appalachian-State-Mountaineers.png'
# create parallel DF with added columns of paths to logo image and team abbr
logoPath = "../../Players/pastV2/logos/"
#rightDF=TeamStats %>% unique() %>% select(Team, Logo_str)
temp=Teams %>% select(School, Kenpom.AdjEM, TOPS)
TeamStats = merge(TeamStats, temp, by.x='Team', by.y='School')
TeamStats = TeamStats %>% dplyr::rename(BASIC = TOPS)

Teams$ETW = Teams$TOPS
Teams$`ETW Offense` = Teams$TOPS.Offense
Teams$`ETW Defense` = Teams$TOPS.Defense
teamsSelect1 = Teams %>% 
  dplyr::select(-School, -`X%_x`, -`TotalS%`, -`X%_Opp_x`, -`Total%S%_Opp`, -PPS, -PPS_Opp, -FIC40, -FIC40_Opp,
         -ORtg_Opp, -DRtg_Opp, -eDiff_Opp, -Poss_Opp, -Pace_Opp, -`X%_y`, -`X%_Opp_y`, -GP_Opp, -MPG_Opp, -TOPS, -TOPS.Offense, -TOPS.Defense,
         -Year, -Year_x, -Year_y, -RankTempo, -RankAdjTempo, -RankOE, -RankAdjOE, -RankDE, -RankAdjDE, -RankAdjEM,
         -Conference, -Logo, -extra_poss, -Opp_extra_poss, -`adj_poss_diff`, -`3PA%`, -`3PA%_Opp`, -FTR_diff, -AdjEM_rate,
         -SOS_rate, -adj_poss_diff_rate, `Opp_OR%`, -`OR%`, -Rank, -SOS.Rank, -`3PAp100_pctl`, -`2PAp100_pctl`,-`FTAp100_pctl`,-PPP_pctl,
         -`Kenpom.AdjEM`, -`Kenpom.Rank`, -`Conf.Rank`,#-adjEM_off, `adjEM_def`, -extra_poss_off, -extra_poss_def,
         #-FTR_off, -FTR_def, -ft_off, -sos_off, -eFG_off, -eFG_def)
  )
teamsSelect = colnames(teamsSelect1[,2:length(colnames(teamsSelect1))])
#teamLogo = merge(Teams, rightDF,by.x = "School", by.y = "Team")
#teamLogo=teamLogo %>% 
#  mutate(Logo_str=paste0(logoPath, Logo_str))

#Teams$Conference = gsub('Missouri Valley', 'MVC', Teams$Conference)
#Teams$Conference = gsub('Mountain West', 'MWC', Teams$Conference)
#Teams$Conference = gsub('Patriot League', 'Patriot', Teams$Conference)
#Teams$Conference = gsub('Summit League', 'Summit', Teams$Conference)
#Teams$Conference = gsub('Horizon League', 'Horizon', Teams$Conference)
Teams$Conference = gsub('CAA', 'Colonial', Teams$Conference)

################# 1.3) Load Bracketology #########################



# get data
Bracketology = read.csv("bracketPredictions2.csv")
Bracketology$Conference = Bracketology$conf
Bracketology$Team = Bracketology$team
Bracketology$`SOR Rank` = Bracketology$sor_resume
Bracketology$`Projected.Seed` = Bracketology$seed
Bracketology$team = gsub("St\\.$", "State", Bracketology$team)
#tv$team = gsub("^St\\.", "Saint", tv$team)
#tv$team = gsub("St\\.", "State", tv$team)
Bracketology$team = gsub("College of Charleston", "Charleston", Bracketology$team)
Bracketology$team = gsub("^Connecticut", "UConn", Bracketology$team)
Bracketology$team = gsub("Central Uconn", "UConn", Bracketology$team)
Bracketology$team = gsub("Boston University", "Boston U", Bracketology$team)
Bracketology$team = gsub("FIU", "Florida International", Bracketology$team)
Bracketology$team = gsub("Central Connecticut", "Central Connecticut State", Bracketology$team)
Bracketology$team = gsub("Louisiana Lafayette", "Louisiana", Bracketology$team)
Bracketology$team = gsub("Middle Tennessee", "Middle Tennessee State", Bracketology$team)
Bracketology$team = gsub("VMI", "Virginia Military", Bracketology$team)
Bracketology$team = gsub("Detroit", "Detroit Mercy", Bracketology$team)
Bracketology$team = gsub("LIU Brooklyn", "Long Island", Bracketology$team)
Bracketology$team = gsub("Massachusetts", "UMass", Bracketology$team)
Bracketology$team = gsub("^Mississippi$", "Ole Miss", Bracketology$team)
Bracketology$team = gsub("Mount State Mary's", "Mount Saint Mary's", Bracketology$team)
Bracketology$team = gsub("North Carolina State", "NC State", Bracketology$team)
Bracketology$team = gsub("Nebraska Omaha", "Omaha", Bracketology$team)
Bracketology$team = gsub("Saint Bonaventure", "St. Bonaventure", Bracketology$team)
Bracketology$team = gsub("Cal St\\.", "Cal State", Bracketology$team)
Bracketology$team = gsub("Mount St\\.", "Mount Saint", Bracketology$team)
Bracketology$team = gsub("F\\.", "F", Bracketology$team)
Bracketology$team = gsub("UTSA", "UT San Antonio", Bracketology$team)
Bracketology$team = gsub("Texas A&M Corpus Chris", "Texas A&M CC", Bracketology$team)
# Bracketology$`SOR Rank` = Bracketology$SOR.RK
# Bracketology = Bracketology %>% 
#   select(-`SOR.RK`)
# # # add column with projected seed
seeds = c(rep(1, 4),rep(2, 4),rep(3, 4),rep(4, 4),
          rep(5, 4),rep(6, 4),rep(7, 4),rep(8, 4),
          rep(9, 4),rep(10, 4),rep(11, 6),rep(12, 4),
          rep(13, 4),rep(14, 4),rep(15, 4),
          rep(16, 6))
# #           "Next Next 4 out","Next Next 4 out")
while (length(seeds) < length(Bracketology$Team)){
  seeds = append(seeds, "MISSED")
}
# Bracketology$Bracket.Rank = seq(1, length(Bracketology$Team), 1)
# Bracketology = Bracketology %>% arrange(desc(SOS))
# Bracketology$SOS.Rank = seq(1, length(Bracketology$Team), 1)
# Bracketology = Bracketology %>% arrange(Bracket.Rank)

#Bracketology = Bracketology %>% arrange(desc(SOS))

################# 1.4) Load Game Slate ###########################

# get data
This.Week = read.csv('week-game-slate.csv')
Today = read.csv('today-game-slate.csv')

#This.Week$Date = as.Date("2024/04/08")


confMergeDF = Teams %>% dplyr::select(`School`, Conference)

away.today.merge = merge(confMergeDF, Today, by.x="School", by.y="Away.Team") %>% arrange(Home.Team)
Today = Today %>% arrange(Home.Team)
Today$Away.Conference = away.today.merge$Conference

home.today.merge = merge(confMergeDF, Today, by.x="School", by.y="Home.Team") %>% arrange(Away.Team)
Today = Today %>% arrange(Away.Team)
Today$Home.Conference = home.today.merge$Conference

away.week.merge = merge(confMergeDF, This.Week, by.x="School", by.y="Away.Team") %>% arrange(Home.Team)
This.Week = This.Week %>% arrange(Home.Team)
This.Week$Away.Conference = away.week.merge$Conference

home.week.merge = merge(confMergeDF, This.Week, by.x="School", by.y="Home.Team") %>% arrange(Away.Team)
This.Week = This.Week %>% arrange(Away.Team)
This.Week$Home.Conference = home.week.merge$Conference

# format column names; add column with matchup & rankings
Today=Today %>% 
  mutate(Home = paste(Home.Rank, Home.Team),
         Away = paste(Away.Rank, Away.Team),
         `Time (ET)` = Time,
         at = '@',
         Home.Team.Score=round((Home.Score), 3),
         Away.Team.Score=round((Away.Score), 3),
         Home.Team.Rank=(Home.Rank),
         Away.Team.Rank=(Away.Rank),
         Game.Score = round(((((Home.Team.Score + Away.Team.Score) - abs((Home.Team.Score+0.15) - Away.Team.Score)/1.25)/116)*100), 2),
         Matchup = paste(Away.Rank, Away.Team, 'at', Home.Rank, Home.Team))
Today=Today[,2:length(Today)]
Today = Today %>% 
  dplyr::select(Away,at,Home, Game.Score,
         Home.Team, Home.Team.Score, Away.Team, Away.Team.Score,
         Date, Week,Location, `Time (ET)`,Away.Conference,Home.Conference) %>% 
  arrange(desc(Game.Score))

# same for games this week
This.Week=This.Week %>% 
  mutate(Home = paste(Home.Rank, Home.Team),
         Away = paste(Away.Rank, Away.Team),
         `Time (ET)` = Time,
         at = '@',
         Home.Team.Score=round((Home.Score), 3),
         Away.Team.Score=round((Away.Score), 3),
         Home.Team.Rank=(Home.Rank),
         Away.Team.Rank=(Away.Rank),
         Game.Score = round(((((Home.Team.Score + Away.Team.Score) - abs((Home.Team.Score+0.15) - Away.Team.Score)/1.25)/116)*100), 2),
         Matchup = paste(Away.Rank, Away.Team, 'at', Home.Rank, Home.Team))
This.Week=This.Week[,2:length(This.Week)]
This.Week = This.Week %>% 
  dplyr::select(Away,at,Home, Game.Score,
         Home.Team, Home.Team.Score, Away.Team, Away.Team.Score,
         Date,Week, Location, `Time (ET)`,Away.Conference,Home.Conference) %>% 
  arrange(desc(Game.Score))

Sys.setenv(TZ='America/Chicago')

#Today = Today %>% filter(Date=="Poopy Butt")
#This.Week = This.Week %>% filter(Date=="Poopy Butt")



###################### 1.5) Load Players #########################







# get data

Players = read.csv('player-ratings.csv')
#Players = Players %>% filter(Player != "Patrick Ngongba, Jr.")
Players = Players %>% filter(Player != "Kasean Pryor")
Players = Players %>% filter(GP > 9)
# clean column names
colnames(Players)[3] = 'Team'
Players=Players[,2:length(Players)]
colnames(Players)=gsub("\\.", "%", colnames(Players))
colnames(Players)=gsub("X", "", colnames(Players))
colnames(Players)[32] = 'Net Rating'


# verify no duplicates
Players=Players %>% distinct(Player,Team, .keep_all=TRUE)

# clean column names
colnames(Players)[3]="PCS"
colnames(Players)[2]="School"
Players = Players %>% 
  mutate(`PCS` = round(`PCS`, 3),
         `2P%` = (((`3PA` + `2PA`) * `FG%`) - (`3P%` * `3PA`)) / `2PA`,
         `TS%` = 100*`TS%`,`FG%` = 100*`FG%`,`eFG%` = 100*`eFG%`,
         `3P%` = 100*`3P%`,`2P%` = 100*`2P%`,`FT%` = 100*`FT%`)
PlayersSelect = c('PCS', colnames(Players[,9:length(Players)-3]))

#Players = Players %>% 
#  mutate(`3P%` = case_when(
#    `3PA`==0 ~ 0,
#    `3PA`!=0 ~ `3P%`))


# clean mismatch school names
#levels(Players$School) <- c(levels(Players$School), "Connecticut")
#Players[Players$School=='UConn',]$School = as.character("Connecticut")
Players = Players %>% mutate(`2PM` = `FG%`*(`3PA` + `2PA`) -(`3PA`*`3P%`),
                             `2P%` = `2PM`/`2PA`)
Players = Players %>% mutate(`3PM` = `FG%`*(`3PA` + `2PA`) -(`2PA`*`2P%`))
Players = Players %>% 
  mutate(`3par_pctl` = ntile((`3PA`/(`3PA`+`2PA`))*100, 100),
         `3p%_pctl` = ntile(`3P%`, 100))


#Players = Players %>% mutate(`BASIC` = `Value Rating`) %>% select(-`Value Rating`)
round(((Players$PCS + 5) / 8.1)*100,0)
round(((historicPCS$PCS + 5) / 8.1)*100,0)

Players$PCS = ((Players$PCS + 5) / 8.0775)*100
historicPCS$PCS = ((historicPCS$PCS + 5) / 8.0775)*100
pcsDF$PCS = ((pcsDF$PCS + 5) / 8.0775)*100
  
Players = Players %>% arrange(desc(PCS))
Players['Natl.Rank'] = seq(1, length(Players$Player))

### add ranks

# conference
Players=Players %>% 
  dplyr::group_by(Conference) %>% 
  dplyr::mutate(Conf.Rank=rank(-PCS))# %>% ungroup()

# team
Players=Players %>% 
  dplyr::group_by(School) %>% 
  dplyr::mutate(Team.Rank=rank(-PCS))# %>% ungroup()

pcsDF = pcsDF %>% arrange(desc(PCS))
################ 1.6) Load Season Awards #########################



pcsDF = pcsDF %>% 
  dplyr::rename(BASIC = PCS,
         Playmaking = Passing)
historicPCS = historicPCS %>% 
  dplyr::rename(BASIC = PCS,
         Playmaking = Passing)
Players = Players %>% 
  dplyr::rename(BASIC = PCS)

# pcsDF %>% 
#   relocate(Defense, .after = Scoring)
# historicPCS = historicPCS %>% 
#   relocate(Defense, .after = Scoring)

# get data
ACC = read.csv("ACC-currentSZN25.csv")%>% dplyr::rename(`Conf Seed` = `Conf..Rank`)
big12 = read.csv("Big 12-currentSZN25.csv")%>% dplyr::rename(`Conf Seed` = `Conf..Rank`)
SEC = read.csv("SEC-currentSZN25.csv") %>% dplyr::rename(`Conf Seed` = `Conf..Rank`)
bigTen = read.csv("Big Ten-currentSZN25.csv")  %>% dplyr::rename(`Conf Seed` = `Conf..Rank`)
#pac12 = read.csv("Pac-12-currentSZN25.csv") %>% dplyr::rename(`Conf Seed` = `Conf..Rank`)
bigEast = read.csv("Big East-currentSZN25.csv") %>% dplyr::rename(`Conf Seed` = `Conf..Rank`)
ncaa = read.csv("ncaa-currentSZN25.csv") %>% arrange(desc(Prediction)) %>% mutate(`X2P.` = (((`X3PA` + `X2PA`) * `FG.`) - (`X3P.` * `X3PA`)) / `X2PA`)#  %>% dplyr::rename(`Conf. Rank` = `Conf..Rank`)
ACC = ACC %>% arrange(desc(Prediction))
SEC = SEC %>% arrange(desc(Prediction))
big12 = big12 %>% arrange(desc(Prediction))
bigTen = bigTen %>% arrange(desc(Prediction))
bigEast = bigEast %>% arrange(desc(Prediction))



# pac12 = pac12 %>% arrange(desc(Prediction))
confpoy = rbind(big12[1:1,], bigTen[1:1,], bigEast[1:1,], SEC[1:1,], ACC[1:1,])

# pac12$Player = pac12$player
bigEast$Player = bigEast$player
bigTen$Player = bigTen$player
big12$Player = big12$player
SEC$Player = SEC$player
ACC$Player = ACC$player

#pac12$Conf = pac12$conf
bigEast$Conf = bigEast$conf
bigTen$Conf = bigTen$conf
big12$Conf = big12$conf
SEC$Conf = SEC$conf
ACC$Conf = ACC$conf

confpoy = rbind(big12[1:1,], bigTen[1:1,], bigEast[1:1,], SEC[1:1,], ACC[1:1,])

# team logo data frame
rightDF2 = ncaa %>% unique() %>% dplyr::select(Full.School.Name, Team_x)
#teamLogo = merge(teamLogo, rightDF2,by.x = "School", by.y = "Full.School.Name")
#teamLogo=teamLogo %>% unique() %>% arrange(Natl.Rank)
#write.csv(teamLogo, '../teamLogo.csv')

p6 = c("Big Ten", "Big 12", "Big East", 'SEC', 'ACC')

# Teams = Teams %>% 
#   mutate(`eFG% Diff` = eFG_diff,
#          `Adj Extra Poss Diff` = adj_poss_diff,
#          `FTR Diff` = FTR_diff,
#          `Kenpom AdjEM` = Kenpom.AdjEM)

#tops.colors = styleInterval(seq(-20,14,0.09), colorRampPalette(c('#FF0000','#FF6B5E', '#FFC2A6','#fff3d4', '#C8FFB3','#95FF66','#00FF00'))(length(seq(-20,14,0.09))+1))
#ft.colors = styleInterval(seq(min(Teams$`FT%`),max(Teams$`FT%`),0.05), colorRampPalette(c('#FF0000','#FF6B5E', '#FFC2A6','#fff3d4','#C8FFB3','#95FF66','#00FF00'))(length(seq(min(Teams$`FT%`),max(Teams$`FT%`),0.05))+1))
#eFG.colors = styleInterval(seq(min(Teams$eFG_diff)+0.02,max(Teams$`eFG_diff`),0.0005), colorRampPalette(c('#FF0000','#FF6B5E', '#FFC2A6','#fff3d4','#C8FFB3','#95FF66','#00FF00'))(length(seq(min(Teams$eFG_diff)+0.02,max(Teams$eFG_diff),0.0005))+1))
#poss.colors = styleInterval(seq(min(Teams$adj_poss_diff),max(Teams$adj_poss_diff),0.05), colorRampPalette(c('#FF0000','#FF6B5E', '#FFC2A6','#fff3d4','#C8FFB3','#95FF66','#00FF00'))(length(seq(min(Teams$adj_poss_diff),max(Teams$adj_poss_diff),0.05))+1))
#ftr.colors = styleInterval(seq(min(Teams$FTR_diff),max(Teams$FTR_diff),0.001), colorRampPalette(c('#FF0000','#FF6B5E', '#FFC2A6','#fff3d4','#C8FFB3','#95FF66','#00FF00'))(length(seq(min(Teams$FTR_diff),max(Teams$FTR_diff),0.001))+1))
#adjem.colors = styleInterval(seq(min(Teams$`Kenpom.AdjEM`),max(Teams$`Kenpom.AdjEM`),0.15), colorRampPalette(c('#FF0000','#FF6B5E', '#FFC2A6','#fff3d4','#C8FFB3','#95FF66','#00FF00'))(length(seq(min(Teams$`Kenpom.AdjEM`),max(Teams$`Kenpom.AdjEM`),0.15))+1))
  

### MERCEYHURST DATA MISSING
Teams = Teams %>% filter(!is.na(ORtg))
etwCurr = etwCurr %>% filter(!is.na(ETW))


get.color.vector = function(vec, rev=F, data=Teams){
  
  print(vec)
  
  # adjust column names for readablility
  data$`Adj Extra Poss Diff` = data$adj_poss_diff
  data$`eFG% Diff` = data$eFG_diff
  data$`FTR Diff` = data$FTR_diff
  data$`Kenpom AdjEM` = round(data$Kenpom.AdjEM, 1)
  data$`Kenpom Rank` = data$Kenpom.Rank
  data$`Kenpom Off Eff` = round(data$`AdjOE`, 1)
  data$`Kenpom Def Eff` = round(data$`AdjDE`, 1)
  data$`SOS Rank` = as.numeric(data$SOS.Rank)
  data$`Net Rating` = data$eDiff
  data$`ETW Rank` = data$Rank
  
  data$`2PA/G_Opp` = data$`2PA_Opp`
  data$`3PA/G_Opp` = data$`3PA_Opp`
  data$`FTA/G_Opp` = data$`FTA_Opp`
  data$`2PA/100 Poss_Opp` = data$`2PAp100_Opp`
  data$`3PA/100 Poss_Opp` = data$`3PAp100_Opp`
  data$`FTA/100 Poss_Opp` = data$`FTAp100_Opp`
  
  #Teams = Teams %>% arrange(desc(`Kenpom Off Eff`))
  #Teams['Kenpom Off Rank'] = seq(1, length(Teams$Team))
  #Teams = Teams %>% arrange(`Kenpom Def Eff`)
  #Teams['Kenpom Def Rank'] = seq(1, length(Teams$Team))
  
  data=data %>% 
    dplyr::select(School, Conference, `Kenpom Rank`, `Kenpom AdjEM`, `Kenpom Off Eff`, `Kenpom Def Eff`, `ETW Rank`,ETW,
           `eFG% Diff`, `Adj Extra Poss Diff`, `FTR Diff`, `ETW Offense`, `ETW Defense`,
           PPG, PPP, `TS%`, `eFG%`, `FG%`,
           `2PA`, `2PAp100`, `2P%`, `3PA`, `3PAp100`, `3P%`, `FTA`, `FTAp100`, `FT%`,
           APG, TOV, `AST%`, `TOV%`,
           DRB, `DRB%`, ORB, `ORB%`,
           SPG, `STL%`, BPG, `BLK%`,
           `Net Rating`, ORtg, DRtg,
           SOS, `SOS Rank`,
           PPG_Opp, PPP_Opp, `TS%_Opp`, `eFG%_Opp`, `FG%_Opp`,
           `2PA_Opp`, `2PAp100_Opp`, `2P%_Opp`, `3PA_Opp`, `3PAp100_Opp`, `3P%_Opp`, `FTA_Opp`, `FTAp100_Opp`, `FT%_Opp`,
           APG_Opp, TOV_Opp, `AST%_Opp`, `TOV%_Opp`,
           DRB_Opp, `DRB%_Opp`, ORB_Opp, `ORB%_Opp`,
           SPG_Opp, `STL%_Opp`, BPG_Opp, `BLK%_Opp`,
           Conf.Rank)
  
  data$`eFG% Diff` = round(data$`eFG% Diff`, 4)
  data$`FT%` = round(data$`FT%`, 2)
  data$`FTR Diff` = round(data$`FTR Diff`, 4)
  data$`Adj Extra Poss Diff` = round(data$`Adj Extra Poss Diff`, 2)
  data$`2PA_Opp` = round(data$`2PA_Opp`, 1)
  data$`2P%_Opp` = round(data$`2P%_Opp`*100, 1)
  data$`PPP` = round(data$`PPP`, 1)
  
  data$`TS%_Opp` = round(data$`TS%_Opp`*100, 1)
  data$`eFG%_Opp` = round(data$`eFG%_Opp`*100, 1)
  data$`FG%_Opp` = round(data$`FG%_Opp`*100, 1)
  data$`3P%_Opp` = round(data$`3P%_Opp`*100, 1)
  data$`FT%_Opp` = round(data$`FT%_Opp`*100, 1)
  
  data$`TS%` = round(data$`TS%`*100, 1)
  data$`eFG%` = round(data$`eFG%`*100, 1)
  data$`FG%` = round(data$`FG%`*100, 1)
  
  #colnames(Teams)=gsub("_Opp", paste("Opponent ", gsub("_Opp", "", colnames(Teams))), colnames(Teams))
  #colnames(Teams)=gsub(" School", "", colnames(Teams))
  colnames(data)=gsub("_Opp", " Opponent", colnames(data))
  
  brk = (max(data[[vec]]) - min(data[[vec]]))/length(data[[vec]])
  brk = brk * 7.165186
  fr = min(data[[vec]], na.rm=TRUE)
  if (fr==0 | is.na(fr)){
    fr = 0.000001
  }
  sq = seq(fr,max(data[[vec]]), brk)
  
  if (rev){
    return(styleInterval(sq,colorRampPalette(c('#02bf02','#95FF66','#ddfccf',
                                               '#e1bbd9','#cd8ec0','#b862a8'))(length(sq)+1)))
  } else {
    return(styleInterval(sq,colorRampPalette(c('#b862a8','#cd8ec0','#e1bbd9',
                                               '#ddfccf','#95FF66','#02bf02'))(length(sq)+1)))
  }
  
}

grades = c("A+", "A", "A-",
           "B+", "B", "B-",
           "C+", "C", "C-",
           "D+", "D", "D-",
           "F")
cls = colorRampPalette(c('#02bf02','#95FF66','#ddfccf','#e1bbd9','#cd8ec0','#b862a8'))(length(grades))

grade.colors = styleEqual(grades, cls)

grades2 = c("A++")
cls2 = colorRampPalette(c('#02bf02'))(length(grades2))

grade.colors2 = styleEqual(grades2, cls2)
#tm.cl.df = read.csv("team_colors.csv")
#pl.cl.df = read.csv("player_color.csv")

tmcl.tops = get.color.vector('ETW')
tmcl.tops.off = get.color.vector('ETW Offense')
tmcl.tops.def = get.color.vector('ETW Defense')
tmcl.ftpct = get.color.vector('FT%')
tmcl.efg_diff = get.color.vector('eFG% Diff')
tmcl.axpd = get.color.vector('Adj Extra Poss Diff')
tmcl.ftrdiff = get.color.vector('FTR Diff')
tmcl.kp = get.color.vector('Kenpom AdjEM')
tmcl.kpo = get.color.vector('Kenpom Off Eff')
tmcl.kpd = get.color.vector('Kenpom Def Eff', rev=T)
tmcl.sos = get.color.vector('SOS')
tmcl.ortg = get.color.vector('ORtg')
tmcl.drtg = get.color.vector('DRtg', rev=T)
tmcl.netrating = get.color.vector('Net Rating')

tmcl.ppg = get.color.vector("PPG")
tmcl.ts = get.color.vector("TS%")
tmcl.efg = get.color.vector("eFG%")
tmcl.fg = get.color.vector("FG%")
tmcl.3pa = get.color.vector("3PA")
tmcl.3p = get.color.vector("3P%")
tmcl.2pa = get.color.vector("2PA")
tmcl.2p = get.color.vector("2P%")
tmcl.fta = get.color.vector("FTA")
tmcl.ft = get.color.vector("FT%")
tmcl.apg = get.color.vector("APG")
tmcl.astpct = get.color.vector("AST%")
tmcl.tov = get.color.vector("TOV", rev=T)
tmcl.tovpct = get.color.vector("TOV%", rev=T)
tmcl.orb = get.color.vector("ORB")
tmcl.orbpct = get.color.vector("ORB%")
tmcl.drb = get.color.vector("DRB")
tmcl.drbpct = get.color.vector("DRB%")
tmcl.spg = get.color.vector("SPG")
tmcl.stlpct = get.color.vector("STL%")
tmcl.bpg = get.color.vector("BPG")
tmcl.blkpct = get.color.vector("BLK%")
tmcl.ppp = get.color.vector("PPP")
tmcl.3pa100 = get.color.vector("3PAp100")
tmcl.2pa100 = get.color.vector("2PAp100")
tmcl.fta100 = get.color.vector("FTAp100")

tmcl.ppg.opp = get.color.vector("PPG Opponent", rev=T)
tmcl.ppp.opp = get.color.vector("PPP Opponent", rev=T)
tmcl.ts.opp = get.color.vector("TS% Opponent", rev=T)
tmcl.efg.opp = get.color.vector("eFG% Opponent", rev=T)
tmcl.fg.opp = get.color.vector("FG% Opponent", rev=T)
tmcl.3pa.opp = get.color.vector("3PA Opponent", rev=T)
tmcl.3p.opp = get.color.vector("3P% Opponent", rev=T)
tmcl.2pa.opp = get.color.vector("2PA Opponent", rev=T)
tmcl.2p.opp = get.color.vector("2P% Opponent", rev=T)
tmcl.fta.opp = get.color.vector("FTA Opponent", rev=T)
tmcl.ft.opp = get.color.vector("FT% Opponent", rev=T)
tmcl.3pa100.opp = get.color.vector("3PAp100 Opponent", rev=T)
tmcl.2pa100.opp = get.color.vector("2PAp100 Opponent", rev=T)
tmcl.fta100.opp = get.color.vector("FTAp100 Opponent", rev=T)

tmcl.apg.opp = get.color.vector("APG Opponent", rev=T)
tmcl.astpct.opp = get.color.vector("AST% Opponent", rev=T)
tmcl.tov.opp = get.color.vector("TOV Opponent", rev=F)
tmcl.tovpct.opp = get.color.vector("TOV% Opponent", rev=F)
tmcl.orb.opp = get.color.vector("ORB Opponent", rev=T)
tmcl.orbpct.opp = get.color.vector("ORB% Opponent", rev=T)
tmcl.drb.opp = get.color.vector("DRB Opponent", rev=T)
tmcl.drbpct.opp = get.color.vector("DRB% Opponent", rev=T)
tmcl.spg.opp = get.color.vector("SPG Opponent", rev=T)
tmcl.stlpct.opp = get.color.vector("STL% Opponent", rev=T)
tmcl.bpg.opp = get.color.vector("BPG Opponent", rev=T)
tmcl.blkpct.opp = get.color.vector("BLK% Opponent", rev=T)



######### GET PLAYERS
get.color.vector.players = function(vec, rev=F, data=Players, full=F, spec=F, outliers=F, spec2=F, outliers2=F){
  
  print(vec)
  
  if (full){
    brk = (as.numeric(quantile(data[[vec]],0.999)) - as.numeric(quantile(data[[vec]],0.001)) ) / length(data[[vec]])
    brk = brk * 16.2418
    sq = seq(as.numeric(quantile(data[[vec]],0.001)),as.numeric(quantile(data[[vec]],0.999)), brk)
  }else{
    if (spec){
      brk = (as.numeric(quantile(data[[vec]],0.9999)) - as.numeric(quantile(data[[vec]],0.0001)) ) / length(data[[vec]])
      brk = brk * 7.165186
      sq = seq(as.numeric(quantile(data[[vec]],0.0001)),as.numeric(quantile(data[[vec]],0.9999)), brk)
    } else if (spec2){
      brk = (as.numeric(quantile(data[[vec]],0.9999)) - as.numeric(quantile(data[[vec]],0.0001)) ) / length(data[[vec]])
      brk = brk / 7.165186
      sq = seq(as.numeric(quantile(data[[vec]],0.0001)),as.numeric(quantile(data[[vec]],0.9999)), brk)
    } else {
      brk = (as.numeric(quantile(data[[vec]],0.999)) - as.numeric(quantile(data[[vec]],0.001)) ) / length(data[[vec]])
      brk = brk * 16.2418
      sq = seq(as.numeric(quantile(data[[vec]],0.001)),as.numeric(quantile(data[[vec]],0.999)), brk)
    }
    
    if (outliers){
      brk = (as.numeric(quantile(data[[vec]],0.98)) - as.numeric(quantile(data[[vec]],0.02)) ) / length(data[[vec]])
      brk = brk * 13.2418
      sq = seq(as.numeric(quantile(data[[vec]],0.02)),as.numeric(quantile(data[[vec]],0.98)), brk)
    }
    
    if (outliers2){
      brk = (as.numeric(quantile(data[[vec]],0.96)) - as.numeric(quantile(data[[vec]],0.04)) ) / length(data[[vec]])
      brk = brk * 13.2418
      sq = seq(as.numeric(quantile(data[[vec]],0.04)),as.numeric(quantile(data[[vec]],0.96)), brk)
    }
    
  }
  
  if (rev){
    return(styleInterval(sq,colorRampPalette(c('#02bf02','#95FF66','#ddfccf',
                                               '#e1bbd9','#cd8ec0','#b862a8'))(length(sq)+1)))
  } else {
    return(styleInterval(sq,colorRampPalette(c('#b862a8','#cd8ec0','#e1bbd9',
                                               '#ddfccf','#95FF66','#02bf02'))(length(sq)+1)))
  }
  
}

Players = Players %>% mutate(tAst = round(APG * GP, 0))
Players = Players %>% mutate(tTov = round(TOV * GP, 0))
Players=Players %>% mutate(`3PA/40 Min` = round((`3PA`/MPG)*40, 1),
                           `AST:TOV` = case_when(
                             TOV == 0 ~ 0,
                             #TOV < 0.1 ~ 0,
                             APG == 0 ~ 0,
                             TRUE ~ tAst/tTov))
Players = Players %>% mutate(`AST:TOV2` = case_when(
                            `AST:TOV` > 3.5 ~ 3.5,
                             TRUE ~ `AST:TOV`))

#Teams$`AST:TOV` = Teams$APG/Teams$TOV
Players$REB = Players$ORB + Players$DRB
Players$REB = round(Players$REB, 1)
Players$STOCKS = Players$BPG + Players$SPG
Players$STOCKS = round(Players$STOCKS, 1)
Players$`3PAr` = (Players$`3PA` / (Players$`3PA` + Players$`2PA`)) * 100

Players = Players %>% mutate(`3PAr2` = case_when(
  `3PAr` > 65 ~ 65,
  TRUE ~ `3PAr`))
Players[is.na(Players)] <- 0
plcl.stocks = get.color.vector.players("STOCKS")
plcl.reb = get.color.vector.players("REB")   
plcl.threemin = get.color.vector.players("3PAr2", full=F, outliers=T)
Players = Players %>% dplyr::select(-`3PA/40 Min`)
Players = Players %>% dplyr::select(-`3PAr`, -`3PAr2`)
plcl.ppr = get.color.vector.players("PPR", outliers=T)
plcl.ratio = get.color.vector.players("AST:TOV2", outliers=T)
plcl.mpg = get.color.vector.players("MPG", outliers=T)
plcl.pcs = get.color.vector.players("BASIC",spec=T)
plcl.usg = get.color.vector.players("USG%", outliers=T)
plcl.sos = get.color.vector.players("SOS", outliers=T)   
plcl.ppg = get.color.vector.players("PPG", full=F)
plcl.ts = get.color.vector.players("TS%", full=F, outliers=T)
plcl.efg = get.color.vector.players("eFG%", full=F, outliers=T)
plcl.fg = get.color.vector.players("FG%", full=F, outliers=F)
plcl.3pa = get.color.vector.players("3PA", outliers=T)
#plcl.3p = get.color.vector.players("3P%")
plcl.2pa = get.color.vector.players("2PA", outliers=T)
#plcl.2p = get.color.vector.players("2P%")
plcl.fta = get.color.vector.players("FTA", outliers=T)
plcl.ft = get.color.vector.players("FT%", outliers=T)
plcl.apg = get.color.vector.players("APG", outliers=T)
plcl.astpct = get.color.vector.players("AST%", outliers=T)
plcl.tov = get.color.vector.players("TOV", rev=T, outliers2=T)
plcl.tovpct = get.color.vector.players("TOV%", rev=T, full=F, outliers2=T)
plcl.orb = get.color.vector.players("ORB", outliers=T)
plcl.orbpct = get.color.vector.players("ORB%", outliers=T)
plcl.drb = get.color.vector.players("DRB")
plcl.drbpct = get.color.vector.players("DRB%", outliers=T)
plcl.spg = get.color.vector.players("SPG", outliers=T)
plcl.stlpct = get.color.vector.players("STL%", outliers=T)
plcl.bpg = get.color.vector.players("BPG", outliers=T)
plcl.blkpct = get.color.vector.players("BLK%", outliers=T)
plcl.netrating = get.color.vector.players("Net Rating", outliers=T)
plcl.ortg = get.color.vector.players("ORtg", outliers=T)
plcl.drtg = get.color.vector.players("DRtg", rev=T, outliers=T)
plcl.per = get.color.vector.players("PER", outliers=T)
#Players=Players %>% select(-`AST:TOV`)
plcl.scoring = get.color.vector.players("Scoring", data=pcsDF, full=T)
plcl.passing = get.color.vector.players("Playmaking", data=pcsDF, full=T)
plcl.rebounding = get.color.vector.players("Rebounding", data=pcsDF, full=T)
plcl.def = get.color.vector.players("Defense", data=pcsDF, full=T)
#plcl.interiorD = get.color.vector.players("Interior.Defense", data=pcsDF, full=F)
plcl.impact = get.color.vector.players("Impact", data=pcsDF, full=T)
plcl.load = get.color.vector.players("Load", data=pcsDF, full=T)
plcl.opp = get.color.vector.players("Opponents", data=pcsDF, full=T)

etwcl.ts = get.color.vector.players("TS% Margin", data=etwCurr, full=T)
etwcl.orb = get.color.vector.players("OR% Margin", data=etwCurr, full=T)
etwcl.tov = get.color.vector.players("TOV Margin/100 Trips", data=etwCurr, full=T, rev=T)
etwcl.ts.off = get.color.vector.players("TS%", data=etwCurr, full=T)
etwcl.orb.off = get.color.vector.players("OR%", data=etwCurr, full=T)
etwcl.tov.off = get.color.vector.players("TOV/100 Trips", data=etwCurr, full=T, rev=T)
etwcl.ts.def = get.color.vector.players("TS% Allowed", data=etwCurr, full=T, rev=T)
etwcl.orb.def = get.color.vector.players("OR% Allowed", data=etwCurr, full=T, rev=T)
etwcl.tov.def = get.color.vector.players("Forced TOV/100 Trips", data=etwCurr, full=T)

t = Bracketology %>% arrange(net)
t = t[1:75,]

brk = (as.numeric(quantile(t[['net']],1)) - as.numeric(quantile(t[['net']],0)) ) / length(t[['net']])
brk = brk * 1
sq = seq(as.numeric(quantile(t[['net']],0)),as.numeric(quantile(t[['net']],1)), brk)


rank.colors = (styleInterval(sq,colorRampPalette(c('#02bf02','#95FF66','#ddfccf',
                                             '#e1bbd9','#cd8ec0','#b862a8'))(length(sq)+1)))
  
t = Bracketology %>% arrange(desc(wab))
t = t[1:125,]

brk = (as.numeric(quantile(t[['wab']],1)) - as.numeric(quantile(t[['wab']],0)) ) / length(t[['wab']])
brk = brk * 1
sq = seq(as.numeric(quantile(t[['wab']],0)),as.numeric(quantile(t[['wab']],1)), brk)


wab.colors = (styleInterval(sq,colorRampPalette(c('#b862a8','#cd8ec0','#e1bbd9',
                                                  '#ddfccf','#95FF66','#02bf02'))(length(sq)+1)))

#rank.colors = get.color.vector.players("net", data=t, full=T, rev=T)

Players = Players %>% 
  mutate(TRB = DRB + ORB)

plcl.trb = get.color.vector.players("TRB", full=T)

last10 = read.csv("Last10_kpFinishesPostTourney_15-25.csv")
last10 = last10 %>% filter(Season != 2014)
last10 = last10 %>% filter(Season != 2015)
last10$Season <- as.character(last10$Season)


last10$Team = gsub("Brigham Young", "BYU", last10$Team)

# last10.kp = last10 %>%
#   #dplyr::mutate(Id= row_number()) %>% 
#   dplyr::group_by(Team, Season) %>%
#   dplyr::summarise(AdjEM = AdjEM) %>%
#   spread(Season, AdjEM)
# 
# # last10.kp = last10.kp %>%
# #   dplyr::group_by(player) %>%
# #   dplyr::filter(row_number()==1)
# 
# last10.kpo = last10 %>%
#   #dplyr::mutate(Id= row_number()) %>% 
#   dplyr::group_by(Team, Season) %>%
#   dplyr::summarize(AdjOE = AdjOE) %>%
#   spread(Season, AdjOE)
# 
# last10.kpd = last10 %>%
#   #dplyr::mutate(Id= row_number()) %>% 
#   dplyr::group_by(Team, Season) %>%
#   dplyr::summarize(AdjDE = AdjDE) %>%
#   spread(Season, AdjDE)
# 
# last10.kpt = last10 %>%
#   #dplyr::mutate(Id= row_number()) %>% 
#   dplyr::group_by(Team, Season) %>%
#   dplyr::summarize(AdjTempo = AdjTempo) %>%
#   spread(Season, AdjTempo)


#https://www.geeksforgeeks.org/draw-multiple-time-series-in-same-plot-in-r/#
#https://stackoverflow.com/questions/22389553/how-to-make-a-timeseries-boxplot-in-r
#https://stackoverflow.com/questions/27268542/plot-boxplots-and-line-of-time-series-data-in-r

# ggplot(aes(Season, Deaths)) +
#   geom_line() +
#   facet_wrap(~ series, ncol = 1,
#              scales = "free_y")




# sq1 = seq(1, 100, 1)
# #pctl.colors = styleInterval(sq1,colorRampPalette(c('#02bf02','#95FF66','#E8F5E9','#e1bbd9','#cd8ec0','#b862a8'))(length(sq1)+1))
# pctl.colors = data.frame("pctl"=sq1)
# pctl.colors$colors = colorRampPalette(c('#b862a8','#cd8ec0','#e1bbd9',
#                                         '#c6fcae','#95FF66','#02bf02'))(length(sq1))
# #pctl.colors$pctl = sq1

# tops.colors = styleInterval(seq(-20,14,0.05), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(-20,14,0.05))+1))
# ft.colors = styleInterval(seq(min(Teams$`FT%`),max(Teams$`FT%`),0.0005), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$`FT%`),max(Teams$`FT%`),0.0005))+1))
# eFG.colors = styleInterval(seq(min(Teams$eFG_diff)+0.02,max(Teams$`eFG_diff`),0.00008), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$eFG_diff)+0.02,max(Teams$eFG_diff),0.00008))+1))
# poss.colors = styleInterval(seq(min(Teams$adj_poss_diff)+4,max(Teams$adj_poss_diff),0.05), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$adj_poss_diff)+4,max(Teams$adj_poss_diff),0.05))+1))
# ftr.colors = styleInterval(seq(min(Teams$FTR_diff),max(Teams$FTR_diff),0.0001), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$FTR_diff),max(Teams$FTR_diff),0.0001))+1))
# adjem.colors = styleInterval(seq(min(Teams$`Kenpom AdjEM`),max(Teams$`Kenpom AdjEM`),0.15), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$`Kenpom AdjEM`),max(Teams$`Kenpom AdjEM`),0.15))+1))

rn = Sys.time()



################ 1.7) Conference Breakdown #########################

topNavg = function(newCol, col, N, conf){
  
  means = c()
  for (c in conf){
    this2 = TeamsFull %>% filter(Conference == c)
    this2 <- this2[order(this2[[col]], decreasing = TRUE),]
    #return(this2[[col]])
    means = c(means, mean(this2[[col]][1:N]))
  }
  
  colName = paste0("Top ", N, " - ", newCol)
  out = data.frame("Conference" = conf,
             colName = means)
  names(out)[names(out) == "colName"] <- colName
  return(out)
}


TeamsFull$Conference = gsub("^Southern$", "SoCon", TeamsFull$Conference)
TeamsFull$Conference = gsub("^CAA$", "Colonial", TeamsFull$Conference)
TeamsFull$Conference = gsub("^A-10$", "Atlantic 10", TeamsFull$Conference)
TeamsFull$Conference = gsub("^Northeast$", "NEC", TeamsFull$Conference)


TeamsFull2=TeamsFull %>% 
  filter(Conference != "Independents" &
           !(School %in% c('West Georgia', 'Le Moyne', 'Mercyhurst', 'Long Island') )) %>% 
  dplyr::group_by(Conference) %>% 
  dplyr::mutate(conf.rank = rank(desc(`KenPom AdjEM`)),
                #metric.rank = `KenPom Rank`,
                conf.tot = max(conf.rank),
                #conf.last = (conf.rank)
                conf.topHalf = conf.rank <= ceiling(conf.tot/2),
                conf.topQuarter = conf.rank <= ceiling(conf.tot/4),
                conf.topThreeQuarter = conf.rank <= ceiling(((conf.tot/4)*3)),
                conf.bottomQuarter = conf.rank >= ceiling(conf.tot/4))


TeamsFull2$conf.last = TeamsFull2$conf.rank == TeamsFull2$conf.tot

conf.kp = TeamsFull2 %>% 
  dplyr::group_by(Conference) %>% 
  dplyr::summarize(`Avg. KP AdjEM` = mean(`KenPom AdjEM`,na.rm=T),
                   
                   
            `Top 50% - Avg. KP AdjEM` = mean(`KenPom AdjEM`[conf.topHalf == TRUE],na.rm=T),
            `Top 25% - Avg. KP AdjEM` = mean(`KenPom AdjEM`[conf.topQuarter == TRUE],na.rm=T),
            `Top 75% - Avg. KP AdjEM` = mean(`KenPom AdjEM`[conf.topThreeQuarter == TRUE],na.rm=T),
            `Bottom 25% - Avg. KP AdjEM` = mean(`KenPom AdjEM`[conf.bottomQuarter == TRUE],na.rm=T),

            `Avg. KP Offense` = mean(`KenPom AdjOE`,na.rm=T),
            `Avg. KP Defense` = mean(`KenPom AdjDE`,na.rm=T),
            #`Avg. KP Tempo` = mean(`KenPom Adj. Tempo`),
            
            `Best Team (Rk)` = paste0(`School`[conf.rank == 1], " (",toOrdinal(`KenPom Rank`[conf.rank == 1]), ")"),
            `Worst Team (Rk)` = paste0(`School`[conf.rank == conf.tot], " (",toOrdinal(`KenPom Rank`[conf.last]), ")"),
            `Middle Team (Rk)` = paste0(`School`[conf.rank == ceiling(conf.tot/2)], " (",toOrdinal(`KenPom Rank`[conf.rank == ceiling(conf.tot/2)]), ")"),
            
            `Standard Deviation KP AdjEM` = sd(`KenPom AdjEM`,na.rm=T),
            
            ) %>% 
  arrange(desc(`Avg. KP AdjEM`))

conf.kp = conf.kp %>%
  group_by(Conference) %>%
  filter(row_number()==1)

conf.kp.all = get.color.vector.players("Avg. KP AdjEM", data=conf.kp, spec2=T)
conf.kp.50 = get.color.vector.players("Top 50% - Avg. KP AdjEM", data=conf.kp, spec2=T)
conf.kp.25 = get.color.vector.players("Top 25% - Avg. KP AdjEM", data=conf.kp, spec2=T)
conf.kp.75 = get.color.vector.players("Top 75% - Avg. KP AdjEM", data=conf.kp, spec2=T)
conf.kp.std = get.color.vector.players("Standard Deviation KP AdjEM", data=conf.kp, spec2=T)
conf.kp.b25 = get.color.vector.players("Bottom 25% - Avg. KP AdjEM", data=conf.kp, spec2=T)
conf.kp.off = get.color.vector.players("Avg. KP Offense", data=conf.kp, spec2=T)
conf.kp.def = get.color.vector.players("Avg. KP Defense", data=conf.kp, spec2=T, rev=T)
#conf.kp.tempo = get.color.vector.players("Avg. KP Tempo", data=conf.kp, spec2=T)


# df %>%
#   group_by(year, area) %>%
#   mutate(mean_left = mean(pp[left == 1])) %>%
#   ungroup()

# conf.kp = merge(conf.kp, topNavg("Avg. KenPom AdjEM", "KenPom AdjEM", 3, unique(TeamsFull$Conference)), by='Conference')
# conf.kp = merge(conf.kp, topNavg("Avg. KenPom Offense", "KenPom AdjOE", 3, unique(TeamsFull$Conference)), by='Conference')
# conf.kp = merge(conf.kp, topNavg("Avg. KenPom Defense", "KenPom AdjDE", 3, unique(TeamsFull$Conference)), by='Conference')
# conf.kp = merge(conf.kp, topNavg("Avg. KenPom AdjEM", "KenPom AdjEM", 6, unique(TeamsFull$Conference)), by='Conference')
# conf.kp = merge(conf.kp, topNavg("Avg. KenPom Offense", "KenPom AdjOE", 6, unique(TeamsFull$Conference)), by='Conference')
# conf.kp = merge(conf.kp, topNavg("Avg. KenPom Defense", "KenPom AdjDE", 6, unique(TeamsFull$Conference)), by='Conference')
# conf.kp = merge(conf.kp, topNavg("Avg. KenPom AdjEM", "KenPom AdjEM", 9, unique(TeamsFull$Conference)), by='Conference')
# conf.kp = merge(conf.kp, topNavg("Avg. KenPom Offense", "KenPom AdjOE", 9, unique(TeamsFull$Conference)), by='Conference')
# conf.kp = merge(conf.kp, topNavg("Avg. KenPom Defense", "KenPom AdjDE", 9, unique(TeamsFull$Conference)), by='Conference')


### Top Half, Top 1/4, Bottom 1/4, Top 3/4, best team, worst team, 

meanProp = function(t, p){
  
  t %>% 
    group_by(Conference) %>% 
    
  
  
  v = sort(v, decreasing = TRUE)
  n = round(length(v)*p,0)
  return(mean(v[1:n]))
  
}











TeamsFull2=TeamsFull %>% 
  filter(Conference != "Independents" &
           !(School %in% c('West Georgia', 'Le Moyne', 'Mercyhurst', 'Long Island') )) %>% 
  dplyr::group_by(Conference) %>% 
  dplyr::mutate(conf.rank = rank(desc(`T-Rank BARTHAG`)),
                conf.tot = max(conf.rank),
                conf.topHalf = conf.rank <= ceiling(conf.tot/2),
                conf.topQuarter = conf.rank <= ceiling(conf.tot/4),
                conf.topThreeQuarter = conf.rank <= ceiling(((conf.tot/4)*3)),
                conf.bottomQuarter = conf.rank >= ceiling(conf.tot/4))

TeamsFull2$conf.last = TeamsFull2$conf.rank == TeamsFull2$conf.tot

conf.bt = TeamsFull2 %>% 
  dplyr::group_by(Conference) %>% 
  dplyr::summarize(`Avg. Barthag` = mean(`T-Rank BARTHAG`,na.rm=T),
                   
                   `Top 50% - Avg. Barthag` = mean(`T-Rank BARTHAG`[conf.topHalf == TRUE],na.rm=T),
                   `Top 25% - Avg. Barthag` = mean(`T-Rank BARTHAG`[conf.topQuarter == TRUE],na.rm=T),
                   `Top 75% - Avg. Barthag` = mean(`T-Rank BARTHAG`[conf.topThreeQuarter == TRUE],na.rm=T),
                   `Bottom 25% - Avg. Barthag` = mean(`T-Rank BARTHAG`[conf.bottomQuarter == TRUE],na.rm=T),

                   `Avg. Torvik Offense` = mean(`T-Rank AdjOE`,na.rm=T),
                   `Avg. Torvik Defense` = mean(`T-Rank AdjDE`,na.rm=T),
                   #`Avg. Torvik Tempo` = mean(`T-Rank Tempo`),

                   `Best Team (Rk)` = paste0(`School`[conf.rank == 1], " (",toOrdinal(`T-Rank`[conf.rank == 1]), ")"),
                   `Worst Team (Rk)` = paste0(`School`[conf.rank == conf.tot], " (",toOrdinal(`T-Rank`[conf.last]), ")"),
                   `Middle Team (Rk)` = paste0(`School`[conf.rank == ceiling(conf.tot/2)], " (",toOrdinal(`T-Rank`[conf.rank == ceiling(conf.tot/2)]), ")"),
                   
                   `Standard Deviation Barthag` = sd(`T-Rank BARTHAG`,na.rm=T),
                   ) %>% 
  arrange(desc(`Avg. Barthag`))

conf.bt.all = get.color.vector.players("Avg. Barthag", data=conf.bt, spec2=T)
conf.bt.50 = get.color.vector.players("Top 50% - Avg. Barthag", data=conf.bt, spec2=T)
conf.bt.25 = get.color.vector.players("Top 25% - Avg. Barthag", data=conf.bt, spec2=T)
conf.bt.75 = get.color.vector.players("Top 75% - Avg. Barthag", data=conf.bt, spec2=T)
conf.bt.std = get.color.vector.players("Standard Deviation Barthag", data=conf.bt, spec2=T)
conf.bt.b25 = get.color.vector.players("Bottom 25% - Avg. Barthag", data=conf.bt, spec2=T)
conf.bt.off = get.color.vector.players("Avg. Torvik Offense", data=conf.bt, spec2=T)
conf.bt.def = get.color.vector.players("Avg. Torvik Defense", data=conf.bt, spec2=T, rev=T)
#conf.bt.tempo = get.color.vector.players("Avg. Torvik Tempo", data=conf.bt, spec2=T)






Teams2 = Teams %>% 
  ungroup %>% 
  arrange(desc(ETW))

Teams2$ETW.rank = seq(1, length(Teams$School))

TeamsFull2=Teams2 %>% 
  filter(Conference != "Independents" &
           !(School %in% c('West Georgia', 'Le Moyne', 'Mercyhurst', 'Long Island') ))%>% 
  dplyr::group_by(Conference) %>% 
  dplyr::mutate(conf.rank = rank(desc(`ETW`)),
                conf.tot = max(conf.rank),
                conf.topHalf = conf.rank <= ceiling(conf.tot/2),
                conf.topQuarter = conf.rank <= ceiling(conf.tot/4),
                conf.topThreeQuarter = conf.rank <= ceiling(((conf.tot/4)*3)),
                conf.bottomQuarter = conf.rank >= ceiling(conf.tot/4))

TeamsFull2$conf.last = TeamsFull2$conf.rank == TeamsFull2$conf.tot

conf.etw = TeamsFull2 %>% 
  dplyr::group_by(Conference) %>% 
  dplyr::summarize(`Avg. ETW` = mean(`ETW`,na.rm=T),
                   
                   `Top 50% - Avg. ETW` = mean(`ETW`[conf.topHalf == TRUE],na.rm=T),
                   `Top 25% - Avg. ETW` = mean(`ETW`[conf.topQuarter == TRUE],na.rm=T),
                   `Top 75% - Avg. ETW` = mean(`ETW`[conf.topThreeQuarter == TRUE],na.rm=T),
                   `Bottom 25% - Avg. ETW` = mean(`ETW`[conf.bottomQuarter == TRUE],na.rm=T),

                   `Avg. ETW Offense` = mean(`ETW Offense`,na.rm=T),
                   `Avg. ETW Defense` = mean(`ETW Defense`,na.rm=T),
                   #`Avg. Tempo` = mean(`Pace`),
                   

                   `Best Team (Rk)` = paste0(`School`[conf.rank == 1], " (",toOrdinal(`ETW.rank`[conf.rank == 1]), ")"),
                   `Worst Team (Rk)` = paste0(`School`[conf.rank == conf.tot], " (",toOrdinal(`ETW.rank`[conf.last]), ")"),
                   `Middle Team (Rk)` = paste0(`School`[conf.rank == ceiling(conf.tot/2)], " (",toOrdinal(`ETW.rank`[conf.rank == ceiling(conf.tot/2)]), ")"),
                                      
                   `Standard Deviation ETW` = sd(`ETW`,na.rm=T),
                   
                   ) %>% 
  arrange(desc(`Avg. ETW`))

conf.etw.all = get.color.vector.players("Avg. ETW", data=conf.etw, spec2=T)
conf.etw.50 = get.color.vector.players("Top 50% - Avg. ETW", data=conf.etw, spec2=T)
conf.etw.25 = get.color.vector.players("Top 25% - Avg. ETW", data=conf.etw, spec2=T)
conf.etw.75 = get.color.vector.players("Top 75% - Avg. ETW", data=conf.etw, spec2=T)
conf.etw.std = get.color.vector.players("Standard Deviation ETW", data=conf.etw, spec2=T)
conf.etw.b25 = get.color.vector.players("Bottom 25% - Avg. ETW", data=conf.etw, spec2=T)
conf.etw.off = get.color.vector.players("Avg. ETW Offense", data=conf.etw, spec2=T)
conf.etw.def = get.color.vector.players("Avg. ETW Defense", data=conf.etw, spec2=T, rev=F)
#conf.etw.tempo = get.color.vector.players("Avg. Tempo", data=conf.etw, spec2=T)



#TeamsFull2 = TeamsFull %>% arrange(desc(`Shot Quality Adj.`))
#TeamsFull2$`Shot Quality Rank` = seq(1, length(TeamsFull2$`Shot Quality Adj.`))

# TeamsFull2=TeamsFull2 %>% 
#   
#   filter(Conference != "Independents" &
#            !(School %in% c('West Georgia', 'Le Moyne', 'Mercyhurst', 'Long Island') )) %>% 
#   dplyr::group_by(Conference) %>% 
#   dplyr::mutate(conf.rank = rank((`Shot Quality Rank`)),
#                 conf.tot = max(conf.rank),
#                 conf.topHalf = conf.rank <= ceiling(conf.tot/2),
#                 conf.topQuarter = conf.rank <= ceiling(conf.tot/4),
#                 conf.topThreeQuarter = conf.rank <= ceiling(((conf.tot/4)*3)),
#                 conf.bottomQuarter = conf.rank >= ceiling(conf.tot/4))


# conf.sq = TeamsFull2 %>% 
#   dplyr::group_by(Conference) %>% 
#   dplyr::summarize(`Avg. Shot Quality` = mean(`Shot Quality Adj.`,na.rm=T),
#                    
#                    `Top 50% - Avg. SQ` = mean(`Shot Quality Adj.`[conf.topHalf == TRUE],na.rm=T),
#                    `Top 25% - Avg. SQ` = mean(`Shot Quality Adj.`[conf.topQuarter == TRUE],na.rm=T),
#                    `Top 75% - Avg. SQ` = mean(`Shot Quality Adj.`[conf.topThreeQuarter == TRUE],na.rm=T),
#                    `Bottom 25% - Avg. SQ` = mean(`Shot Quality Adj.`[conf.bottomQuarter == TRUE],na.rm=T),
# 
#                    `Avg. SQ Offense` = mean(`Shot Quality Adj. Offense`,na.rm=T),
#                    `Avg. SQ Defense` = mean(`Shot Quality Adj. Defense`,na.rm=T),
#                    #`Avg. Tempo` = mean(`Possessions/G`),
#                    
#                    `Best Team (Rk)` = paste0(`School`[conf.rank == 1], " (",toOrdinal(`Shot Quality Rank`[conf.rank == 1]), ")"),
#                    `Worst Team (Rk)` = paste0(`School`[conf.rank == conf.tot], " (",toOrdinal(`Shot Quality Rank`[conf.rank == conf.tot]), ")"),
#                    `Middle Team (Rk)` = paste0(`School`[conf.rank == ceiling(conf.tot/2)], " (",toOrdinal(`Shot Quality Rank`[conf.rank == ceiling(conf.tot/2)]), ")"),
# 
#                    `Standard Deviation SQ` = sd(`Shot Quality Adj.`,na.rm=T),
#                    ) %>% 
#   arrange(desc(`Avg. Shot Quality`))
# 
# conf.sq.all = get.color.vector.players("Avg. Shot Quality", data=conf.sq, spec2=T)
# conf.sq.50 = get.color.vector.players("Top 50% - Avg. SQ", data=conf.sq, spec2=T)
# conf.sq.25 = get.color.vector.players("Top 25% - Avg. SQ", data=conf.sq, spec2=T)
# conf.sq.75 = get.color.vector.players("Top 75% - Avg. SQ", data=conf.sq, spec2=T)
# conf.sq.std = get.color.vector.players("Standard Deviation SQ", data=conf.sq, spec2=T)
# conf.sq.b25 = get.color.vector.players("Bottom 25% - Avg. SQ", data=conf.sq, spec2=T)
# conf.sq.off = get.color.vector.players("Avg. SQ Offense", data=conf.sq, spec2=T)
# conf.sq.def = get.color.vector.players("Avg. SQ Defense", data=conf.sq, spec2=T, rev=T)
# #conf.sq.tempo = get.color.vector.players("Avg. Tempo", data=conf.sq, spec2=T)
# 







TeamsFull2 = TeamsFull %>% arrange(desc(`ESPN BPI`))
TeamsFull2$`ESPN BPI Rank` = seq(1, length(TeamsFull2$`ESPN BPI`))

TeamsFull2=TeamsFull2 %>% 
  
  filter(Conference != "Independents") %>% 
  dplyr::group_by(Conference) %>% 
  dplyr::mutate(conf.rank = rank(`ESPN BPI Rank`),
                conf.tot = max(conf.rank),
                conf.topHalf = conf.rank <= ceiling(conf.tot/2),
                conf.topQuarter = conf.rank <= ceiling(conf.tot/4),
                conf.topThreeQuarter = conf.rank <= ceiling(((conf.tot/4)*3)),
                conf.bottomQuarter = conf.rank >= ceiling(conf.tot/4))

TeamsFull2$conf.last = TeamsFull2$conf.rank == TeamsFull2$conf.tot

conf.bpi = TeamsFull2 %>% 
  dplyr::group_by(Conference) %>% 
  dplyr::summarize(`Avg. ESPN BPI` = mean(`ESPN BPI`,na.rm=T),
                   
                   `Top 50% - Avg. BPI` = mean(`ESPN BPI`[conf.topHalf == TRUE],na.rm=T),
                   `Top 25% - Avg. BPI` = mean(`ESPN BPI`[conf.topQuarter == TRUE],na.rm=T),
                   `Top 75% - Avg. BPI` = mean(`ESPN BPI`[conf.topThreeQuarter == TRUE],na.rm=T),
                   `Bottom 25% - Avg. BPI` = mean(`ESPN BPI`[conf.bottomQuarter == TRUE],na.rm=T),

                   `Avg. BPI Offense` = mean(`ESPN BPI Offense`,na.rm=T),
                   `Avg. BPI Defense` = mean(`ESPN BPI Defense`,na.rm=T),
                   #`Avg. Tempo` = mean(`Possessions/G`),
                   
                   `Best Team (Rk)` = paste0(`School`[conf.rank == 1], " (",toOrdinal(`ESPN BPI Rank`[conf.rank == 1]), ")"),
                   `Worst Team (Rk)` = paste0(`School`[conf.rank == conf.tot], " (",toOrdinal(`ESPN BPI Rank`[conf.last]), ")"),
                   `Middle Team (Rk)` = paste0(`School`[conf.rank == ceiling(conf.tot/2)], " (",toOrdinal(`ESPN BPI Rank`[conf.rank == ceiling(conf.tot/2)]), ")"),
                   
                   `Standard Deviation BPI` = sd(`ESPN BPI`,na.rm=T)
  ) %>% 
  arrange(desc(`Avg. ESPN BPI`))

conf.bpi.all = get.color.vector.players("Avg. ESPN BPI", data=conf.bpi, spec2=T)
conf.bpi.50 = get.color.vector.players("Top 50% - Avg. BPI", data=conf.bpi, spec2=T)
conf.bpi.25 = get.color.vector.players("Top 25% - Avg. BPI", data=conf.bpi, spec2=T)
conf.bpi.75 = get.color.vector.players("Top 75% - Avg. BPI", data=conf.bpi, spec2=T)
conf.bpi.std = get.color.vector.players("Standard Deviation BPI", data=conf.bpi, spec2=T)
conf.bpi.b25 = get.color.vector.players("Bottom 25% - Avg. BPI", data=conf.bpi, spec2=T)
conf.bpi.off = get.color.vector.players("Avg. BPI Offense", data=conf.bpi, spec2=T)
conf.bpi.def = get.color.vector.players("Avg. BPI Defense", data=conf.bpi, spec2=T, rev=F)
#conf.bpi.tempo = get.color.vector.players("Avg. Tempo", data=conf.bpi, spec2=T)














high.major = c("Big 12", "Big East", "Big Ten", "ACC", "SEC")

mid.major = c("Mountain West", "American", "Missouri Valley", "Atlantic 10", "Sun Belt", "WAC",
              "Ivy League", "Big West", "Horizon League", "Colonial", "MAC", "C-USA", "WCC", 'SoCon')

low.major = c('America East',
              'Atlantic Sun',
              'Big Sky',
              'Big South',
              'MAAC',
              'MEAC', 
              'NEC',
              'Ohio Valley',
              'Patriot League',
              'Southland',
              'Summit League',
              'SWAC')


#conf.bt.all = get.color.vector.players("Avg. Barthag", data=conf.bt, spec2=T)

######  ________________________ ######







############################################# 2) UI LAYOUT #######################################################

ui <- dashboardPage(
  

  ################ 2.1) Dashboard Setup #######################

  
  skin = "purple",
  dashboardHeader(title = "CBB DASH",titleWidth = 270),
  dashboardSidebar(
    width = 270,
    sidebarMenuOutput("menu")
  ),
  
  ###### 2.1.1) CSS Formatting ######
  # CSS formatting
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Trebuchet MS", monospace;
        font-size: 28px;
      }
      .main-sidebar { font-family: "Trebuchet MS", serif;
                      font-size: 15px;}
      .info-box {height: 120px; } 
      .info-box-icon {height: 120px; line-height: 120px;} 
      .info-box-content {padding-top: 0px; padding-bottom: 0px; font-size=15px; font-family: "Trebuchet MS"}
      .body {font-family: "Trebuchet MS"}
      * {font-family: "Trebuchet MS"};
      .select-input { font-size: 24px;}
      .selectInput { font-size: 24px;}
      #boxSmallScatter{height:600px !important;}
      #homePageSmallScatter{height:550px !important;}
      #boxSmallScatter .box-header{ display: none}
      .nav-tabs-custom .nav-tabs li.active {
        border-top-color: light-blue;}"
      
    '))),
    #.small-box {height: 255px}
    
    
    tabItems(
      
      
      ###### ____ ######

      ################ 2.2) Home Page Layout ######################

      
      
      tabItem(tabName = "home",
              
              # header
              fluidRow(
                
                column(11,
                       tags$h2(strong("CBB Dash"), style = "font-size: 350%;"),
                       #tags$p(strong('College Basketball Dashboards'), style = 'font-size: 175%; font-family: "Trebuchet MS";'),
                       tags$p(em(strong("Last Updated", update)),
                              style = 'font-size: 108%; font-family: "Trebuchet MS";'),
                ),
                
                column(1,imageOutput("homePage-logo",
                                     width='5%',
                                     height='5%'))
              ),
              #
              #img(src='..//..//Logos//logo-textRight.png', align = "right"),
              
              br(),
              
              
              ### teams preview
              fluidRow(
                
                ###### 2.2.1) Games this week ######
                # top teams DT
                column(2, 
                       #box(id="boxSmallScatter",
                       #    width=2, 
                       h2(strong(paste("Natl. Champ"))),
                       #h2(strong(paste("Week",This.Week$Week[1]))),
                       
                       #h2(strong(paste("Champ Week"))),
                           ### top games preview
                           
                           br(),
                           # best game this week
                           valueBox(#title = format(as.Date(This.Week$Date[1]), "%a, %b %y"),
                                   tags$p(HTML(paste0(This.Week$Away[1],""," v ","<br/>", This.Week$Home[1])), style = "font-size: 45%;"),
                                   tags$p(paste(format(as.Date(This.Week$Date[1]), "%a, %b %e"), '@', This.Week$Time[1], "ET", '|', This.Week$Location[1]), style = "font-size: 105%;"),
                                   width=14,
                                   icon=icon('basketball'),
                                   color='purple'),
                           # 2nd game
                           valueBox(#title = format(as.Date(This.Week$Date[4]), "%a, %b %y"),
                                   value = tags$p(HTML(paste(This.Week$Away[2],"v<br/>",This.Week$Home[2])), style = "font-size: 45%;"),
                                   tags$p(paste(format(as.Date(This.Week$Date[2]), "%a, %b %e"),'@', This.Week$Time[2], "ET",'|', This.Week$Location[2]), style = "font-size: 105%;"),
                                   width=14,
                                   icon=icon('basketball'),
                                   color='light-blue'),
                           # 3rd game
                           valueBox(#title = format(as.Date(This.Week$Date[7]), "%a, %b %y"),
                                   value = tags$p(HTML(paste(This.Week$Away[3],"v<br/>",This.Week$Home[3])), style = "font-size: 45%;"),
                                   tags$p(paste(format(as.Date(This.Week$Date[3]), "%a, %b %e"),'@', This.Week$Time[3], "ET",'|',This.Week$Location[3]), style = "font-size: 105%;"),
                                   width=14,
                                   icon=icon('basketball'),
                                   color='purple'),
                           # 4th game
                           valueBox(#title = format(as.Date(This.Week$Date[2]), "%a, %b %y"),
                                   value = tags$p(HTML(paste(This.Week$Away[4],"v<br/>",This.Week$Home[4])), style = "font-size: 45%;"),
                                   tags$p(paste(format(as.Date(This.Week$Date[4]), "%a, %b %e"),'@', This.Week$Time[4], "ET", "|", This.Week$Location[4]), style = "font-size: 105%;"),
                                   width=14,
                                   icon=icon('basketball'),
                                   color='light-blue'),
                           # 5th game
                           valueBox(#title = format(as.Date(This.Week$Date[5]), "%a, %b %y"),
                                   value = tags$p(HTML(paste(This.Week$Away[5],"v<br/>",This.Week$Home[5])), style = "font-size: 45%;"),
                                   tags$p(paste(format(as.Date(This.Week$Date[5]), "%a, %b %e"), '@', This.Week$Time[5], "ET", "|", This.Week$Location[5]), style = "font-size: 105%;"),
                                   width=14,
                                   icon=icon('basketball'),
                                   color='purple'),
                           # 6th game
                           valueBox(#title = format(as.Date(This.Week$Date[8]), "%a, %b %y"),
                                   value = tags$p(HTML(paste(This.Week$Away[6],"v<br/>",This.Week$Home[6])), style = "font-size: 45%;"),
                                   tags$p(paste(format(as.Date(This.Week$Date[6]), "%a, %b %e"),'@', This.Week$Time[6], "ET", '|',This.Week$Location[6]), style = "font-size: 105%;"),
                                   width=14,
                                   icon=icon('basketball'),
                                   color='light-blue'),
                           # 7th game
                           valueBox(#title = format(as.Date(This.Week$Date[3]), "%a, %b %y"),
                                   value = tags$p(HTML(paste(This.Week$Away[7],"v<br/>",This.Week$Home[7])), style = "font-size: 45%;"),
                                   tags$p(paste(format(as.Date(This.Week$Date[7]), "%a, %b %e"),'@', This.Week$Time[7], "ET",'|',This.Week$Location[7]), style = "font-size: 105%;"),
                                   width=14,
                                   icon=icon('basketball'),
                                   color='purple'),
                           # 8th game
                           valueBox(#title = format(as.Date(This.Week$Date[6]), "%a, %b %y"),
                                   value = tags$p(HTML(paste(This.Week$Away[8],"v<br/>",This.Week$Home[8])), style = "font-size: 45%;"),
                                   tags$p(paste(format(as.Date(This.Week$Date[8]), "%a, %b %e"),'@', This.Week$Time[8], "ET",'|',This.Week$Location[6]), style = "font-size: 105%;"),
                                   width=14,
                                   icon=icon('basketball'),
                                   color='light-blue'),
                           # 9th game
                           valueBox(#title = format(as.Date(This.Week$Date[9]), "%a, %b %y"),
                                   value = tags$p(HTML(paste(This.Week$Away[9],"v<br/>",This.Week$Home[9])), style = "font-size: 45%;"),
                                   tags$p(paste(format(as.Date(This.Week$Date[9]), "%a, %b %e"),'@', This.Week$Time[9], "ET",'|', This.Week$Location[9]), style = "font-size: 105%;"),
                                   width=14,
                                   icon=icon('basketball'),
                                   color='purple')) ,
                       
                column(6,
                       
                       ###### 2.2.2) Top ETW 25 Scatter ######
                       h2(strong("Dash Top 25 in ETW")),
                       plotOutput('homePage-AdjDEvsAdjOE'),
                       
                       ###### 2.2.2) All Americans, Conf POY, Stat Leaders ######
                       h2(strong("All-American First Team Predictive Model"), style = "font-size: 150%;"),# (Stats from Reg. Szn)"), style = "font-size: 150%;"),
                       DT::dataTableOutput('AAfirst-homePage'),
                       
                       h2(strong("Power-5 Conf. POY Predictive Model"), style = "font-size: 150%;"),
                       DT::dataTableOutput('confPOY-homePage'),
                       
                       h2(strong("Box-Score Stat Leaders"), style = "font-size: 175%;"),
                       checkboxInput("homePage-statsD1", label=p(strong("Show Power-5 Leaders"), style = "font-size: 118%;")),
                       
                       
                       tabBox(width=14,
                              tabPanel(strong("PPG"),
                                       DT::dataTableOutput('homePage-top5scorers')),
                              tabPanel(strong("AST"),
                                       DT::dataTableOutput('homePage-top5passers')),
                              tabPanel(strong("REB"),
                                       DT::dataTableOutput('homePage-top5rebounders')),
                              tabPanel(strong("TOV"),
                                       DT::dataTableOutput('homePage-top5tov')),
                              tabPanel(strong("AST:TOV"),
                                       p(strong("*min 2.5 AST/G"), style = "font-size: 100%;"),
                                       DT::dataTableOutput('homePage-top5ratio')),
                              tabPanel(strong("BLK"),
                                       DT::dataTableOutput('homePage-top5blocks')),
                              tabPanel(strong("STL"),
                                       DT::dataTableOutput('homePage-top5steals')),
                              tabPanel(strong("O-Rtg"),
                                       p(strong("* min 18 MPG & 18% Usage"), style = "font-size: 100%;"),
                                       DT::dataTableOutput('homePage-top5ortg'),
                                       #br(),
                                       p(strong("* Offensive rating is team's points per 100 possessions when
                                                player is on the court"), style = "font-size: 85%;")),
                              tabPanel(strong("D-Rtg"),
                                       p(strong("* min 18 MPG & 12% Usage"), style = "font-size: 100%;"),
                                       DT::dataTableOutput('homePage-top5drtg'),
                                       #br(),
                                       p(strong("* Defensive rating is team's points allowed per 100 possessions when
                                                player is on the court"), style = "font-size: 85%;"))
                       ),
                       

                       ),
                column(4,
                       ###### 2.2.3) Top 32 BASIC ######
                       # top offenses
                       #box(width=12,
                           h2(strong("Top 32 Players per BASIC")),
                           DT::dataTableOutput('homePageTop25Player'))
                       # # top defenses
                       # box(width=2,
                       #     title=h2(strong("Top 10 KP Def")),
                       #     DT::dataTableOutput('homePage-top10TeamDef'))),
              ),
              
              ###### 2.2.4) Bracketology One Seeds ######
              ### bracketology preview
              # h2(strong("Dash Bracketology 1 Seeds")),
              # fluidRow(
              #   # top overall seed
              #   column(3,infoBox(title = tags$p(Bracketology$Conference[1],style='font-size:125%'),
              #                    value = tags$p(Bracketology$Team[1],style='font-size:125%'),
              #                    paste('NET:',toOrdinal(Bracketology$net[1]),'|','SOR:',toOrdinal(Bracketology$`SOR Rank`[1]),"| Q1 Wins:", Bracketology$`quad_1_W`[1]),
              #                    width=14,
              #                    icon=icon('1'),
              #                    color='light-blue')),
              #   # 2nd one seed
              #   column(3,infoBox(title = tags$p(Bracketology$Conference[2],style='font-size:125%'),
              #                    value = tags$p(Bracketology$Team[2],style='font-size:125%'),
              #                    paste('NET:',toOrdinal(Bracketology$net[2]),'|','SOR:',toOrdinal(Bracketology$`SOR Rank`[2]),"| Q1 Wins:", Bracketology$`quad_1_W`[2]),
              #                    width=14,
              #                    icon=icon('1'),
              #                    color='light-blue')),
              #   # 3rd one seed
              #   column(3,infoBox(title = tags$p(Bracketology$Conference[3],style='font-size:125%'),
              #                    value = tags$p(Bracketology$Team[3],style='font-size:125%'),
              #                    paste('NET:',toOrdinal(Bracketology$net[3]),'|','SOR:',toOrdinal(Bracketology$`SOR Rank`[3]),"| Q1 Wins:", Bracketology$`quad_1_W`[3]),
              #                    width=14,
              #                    icon=icon('1'),
              #                    color='light-blue')),
              #   # 4th one seed
              #   column(3,infoBox(title = tags$p(Bracketology$Conference[4],style='font-size:125%'),
              #                    value = tags$p(Bracketology$Team[4],style='font-size:125%'),
              #                    paste('NET:',toOrdinal(Bracketology$net[4]),'|','SOR:',toOrdinal(Bracketology$`SOR Rank`[4]),"| Q1 Wins:", Bracketology$`quad_1_W`[4]),
              #                    width=14,
              #                    icon=icon('1'),
              #                    color='light-blue'))),
              
              fluidRow(
                
                column(6,
                       
                       ###### 2.2.5) KenPom Top 25 Scatter ###### 
                       h2(strong("KenPom Top 25 Adj. Efficiency")),
                       plotOutput("homePage-KP-AdjDEvsAdjOE")),
                
                column(6,
                       
                       ###### 2.2.6) AP Poll Top 25 Scatter ######
                       h2(strong("AP Poll Top 25 Raw Efficiency")),
                       plotOutput("homePage-AP-ORtgvsDRtg")
                       )
                
              ),
              
              # br(),
              # fluidRow(width=3,
              #          
              #          ### players preview
              #          column(9,
              #                 fluidRow(column(5,
              #                                 
              #                                 # top 25 players in nation
              #                                 box(width=12,
              #                                     title=h2(strong("Top 25 in Player Contribution Score")),
              #                                     DT::dataTableOutput('homePage-top10TeamDef'))),
              #                          
              #                          # power 6 leaders
              #                          column(7,
              #                                 box(width=12,title=h2(strong("Scoring Leaders in Power 6")),
              #                                     DT::dataTableOutput('homePage-top5scorers')),
              #                                 box(width=12, title=h2(strong("Passing Leaders in Power 6")),
              #                                     DT::dataTableOutput('homePage-top5passers')),
              #                                 box(width=12,title=h2(strong("Rebounding Leaders in Power 6")),
              #                                     DT::dataTableOutput('homePage-top5rebounders'))
              #                          )
              #                 )
              #          ),
              #          
              #          br(),
              #          column(3, 
              #                 
              #          ),
              # )
      ),
      ###### ____ ######

      ############# 2.2) Stats & Ratings Layout ###################

      
      tabItem(tabName = "ratings-teams", 
              
              # header
              fluidRow(
                
                column(11,h2(strong("Full Stats & Ratings - Teams"))),
                
                column(1,
                       imageOutput("homePage-logo3",
                                            width='5%',
                                            height='5%'))
                
              ),
              
              #strong(p('CBB dash has created two custom college hoops rating systems, one for teams and one for players')),
              br(),
              
              ### filters
              fluidRow(
                # conference filter
                column(4,
                       
                       selectInput(
                         inputId = "Conference", 
                         label = strong("Conference:", style='font-size:130%'),
                         multiple = FALSE,
                         #placeholder = "School",
                         choices = c('All', 'Big 12', 'Big Ten', 'Big East', 'SEC', 'ACC', 'Mountain West', 
                                     sort(unique(as.character(Teams$Conference[!Teams$Conference %in% c('Big 12', 'Big Ten', 'Big East', 'SEC', 'ACC','Mountain West')])))),
                         selectize = TRUE,
                         width = NULL,
                         size = NULL,
                         selected = "All"),
                       
                       # checkboxInput("fullStats-finalFourCheckbox", 
                       #               label=strong("Show Final Four",
                       #                            style = "font-size: 120%; color:red;"))
                       
                       # selectizeInput("Conference", "Conference: (Default 'NCAA')",
                       #                  c('NCAA', sort(unique(as.character(Teams$Conference)))),
                       #                  options = list(
                       #                    create = FALSE,
                       #                    placeholder = "Conference",
                       #                    maxItems = '1',
                       #                    onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                       #                    onType = I("function (str) {if (str === \"\") {this.close();}}")
                       #                  ))
                       
                       ),
                # school filter
                column(4,
                       
                       selectInput(
                         inputId = "School", 
                         label = strong("School:", style='font-size:130%'),
                         multiple = FALSE,
                         #placeholder = "School",
                         choices = c("All",sort(unique(as.character(Teams$School)))), 
                         selectize = TRUE,
                         width = NULL,
                         size = NULL,
                         selected = "All")
                       
                      
                       
                       # selectizeInput("School",,
                       #                  c("All",sort(unique(as.character(Teams$School)))), 
                       #                  options = list(
                       #                    create = FALSE,
                       #                    placeholder = "School",
                       #                    maxItems = '1',
                       #                    onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                       #                    onType = I("function (str) {if (str === \"\") {this.close();}}")
                       #                  ))
                       ),
                # age filter
                # column(4,selectInput("Class",
                #                      strong("Class:", style='font-size:130%'),
                #                      c("All","Fr","So","Jr","Sr")))),
              # Team DT
              box(width=12, DT::dataTableOutput('fullTeamDT')),
      )
      
      ),
      
      
      
      
      
      
      
      
      tabItem(tabName = "ratings-players", 
              
              # header
              fluidRow(
                
                column(11,h2(strong("Full Stats & Ratings - Players"))),
                
                column(1,
                       imageOutput("homePage-logo15",
                                   width='5%',
                                   height='5%'))
                
              ),
              
              #strong(p('CBB dash has created two custom college hoops rating systems, one for teams and one for players')),
              br(),
              
              ### filters
              fluidRow(
                # conference filter
                column(4,
                       
                       selectInput(
                         inputId = "Conference-player", 
                         label = strong("Conference:", style='font-size:130%'),
                         multiple = FALSE,
                         #placeholder = "School",
                         choices = c('All', 'Big 12', 'Big Ten', 'Big East', 'SEC', 'ACC', sort(unique(as.character(Teams$Conference[!Teams$Conference %in% c('Big 12', 'Big Ten', 'Big East', 'SEC', 'ACC')])))),
                         selectize = TRUE,
                         width = NULL,
                         size = NULL,
                         selected = "All"),
                       
                       # checkboxInput("fullStats-finalFourCheckbox-player", 
                       #               label=strong("Show Final Four",
                       #                            style = "font-size: 120%; color:red;"))
                       
                ),
                # if (as.character(textOutput("player-conf-text")) == "All"){
                #   choice = c("All","A",sort(unique(as.character(Teams$School))))
                # }else{
                #   choice = c("All",sort(unique(as.character(Teams$School[Teams$Conference == as.character(textOutput("player-conf-text"))]))))
                # },
                # school filter
                column(4,
                       
                       #if (as.character(textOutput("player-conf-text")) == "All"){
                       selectInput(
                         inputId = "School-player", 
                         label = strong("School:", style='font-size:130%'),
                         multiple = FALSE,
                         #placeholder = "School",
                         #if (as.character(textOutput("player-conf-text")) == "All"){choices = c("All","A",sort(unique(as.character(Teams$School))))}
                         choices = c("All",sort(unique(as.character(Teams$School)))),
                         selectize = TRUE,
                         width = NULL,
                         size = NULL,
                         selected = "All")
                       # } else {
                       #   selectInput(
                       #     inputId = "School-player",
                       #     label = strong("School:", style='font-size:130%'),
                       #     multiple = FALSE,
                       #     #placeholder = "School",
                       #     choices = c("All",sort(unique(as.character(Teams$School[Teams$Conference == as.character(textOutput("player-conf-text"))])))),
                       #     selectize = TRUE,
                       #     width = NULL,
                       #     size = NULL,
                       #     selected = "All")
                       # }
                       
                       
     
                ),
                # age filter
                column(4,selectInput("Class",
                                     strong("Class:", style='font-size:130%'),
                                     c("All","Fr","So","Jr","Sr"))),
              
              
              
              
              
              # Player DT
              box(width=12,DT::dataTableOutput('fullPlayerDT')),
              br(),
              p(strong("* players with at least 8 MPG and 16 GP"))#,
              #p(choice)
              )
                  
      ),
      
      
      
      
      
      
      
      
      
      
      tabItem(tabName = "ratings-conf", 
              # header
              fluidRow(
                
                column(11,h2(strong("Full Stats & Ratings - Conferences"))),
                
                column(1,
                       imageOutput("homePage-logo16",
                                   width='5%',
                                   height='5%'))
                
              ),
              br(),
              
              ### filters
              fluidRow(
                # conference filter
                column(4,
                       
                       selectInput(
                         inputId = "Conference-group", 
                         label = strong("Conference Group:", style='font-size:130%'),
                         multiple = FALSE,
                         #placeholder = "School",
                         choices = c('All', 'Power-5', 'Mid-Major', 'Low-Major'), 
                                     #sort(unique(as.character(Teams$Conference[!Teams$Conference %in% c('Big 12', 'Big Ten', 'Big East', 'SEC', 'ACC', 'Pac-12','Mountain West')])))),
                         selectize = TRUE,
                         width = NULL,
                         size = NULL,
                         selected = "All")
                       
                       ),
                column(4,
                       
                       selectInput(
                         inputId = "Conference-metric", 
                         label = strong("Metric Source:", style='font-size:130%'),
                         multiple = FALSE,
                         #placeholder = "School",
                         choices = c('KenPom', 
                                     #'BartTorvik', 
                                     'Estimated Tournament Wins', 
                                     #'Shot Quality', 
                                     'ESPN BPI'), 
                         #sort(unique(as.character(Teams$Conference[!Teams$Conference %in% c('Big 12', 'Big Ten', 'Big East', 'SEC', 'ACC', 'Pac-12','Mountain West')])))),
                         selectize = TRUE,
                         width = NULL,
                         size = NULL,
                         selected = "KenPom")
                       
                ),
              ),
              fluidRow(
                column(12,
                  box(DT::dataTableOutput('fullConferenceDT'), width=12)
                )
                ),
              
              
      ),
      
      ###### ____ ######
      
      ############## 2.3) Team Identity Layout ####################

      
      
      
      
      
      tabItem(tabName = "team-identities", 
              
              # header 
              fluidRow(
                
                column(11,
                       h2(strong("Team Identity",style='font-size:150%'))),

                column(1,imageOutput("homePage-logo2",
                                     width='5%',
                                     height='5%'))
              ),
              
              br(),
              fluidRow(
              #   selectInput(
              #     inputId="cusTeamVis-topNinStat",
              #     label = "",
              #     choices = sort(unique(colnames(Teams %>% select(-Logo, -Conference, -School, -Year, -`KenPom Rank`)))), 
              #     selected="KenPom AdjEM",
              #     multiple = FALSE,
              #     selectize = TRUE,
              #     width = NULL,
              #     size = NULL
              #   ),
                # school search bar
                column(3, 
                       selectInput(
                         inputId = "teamIdentity-teamSearchBox", 
                         label = strong("Enter School:",style='font-size:160%'),
                         multiple = FALSE,
                         choices = sort(Teams$School),
                         selectize = TRUE,
                         width = NULL,
                         size = NULL,
                         selected = top.team
                         # options = list(
                         #   create = FALSE,
                         #   placeholder = "School",
                         #   maxItems = '1',
                         #   onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                         #   onType = I("function (str) {if (str === \"\") {this.close();}}")
                         # )
                         ),
                       
                       selectInput("mainPage-teamEff2", 
                                   label = strong("Select Team Metric Source:", style='font-size:160%'),
                                   choices = c("Estimated Tournament Wins",
                                               #"KenPom","BartTorvik",
                                               "ESPN BPI"
                                               #"Shot Quality"
                                               ))),
                
                column(2,imageOutput("homePage-logoTest",
                                     width='5%',
                                     height='5%')),
                
                column(7,
                       
                       uiOutput("teamIdentity-overallRanks")
                       
                       )
                  
                ),
              br(),
              ###### 2.3.1) Main Page - Team Identity ######
              # 5 top infoboxes - TOPS, KP Off, KP Def, KP Tempo, SoS
              uiOutput("teamIdentity-mainPage",
                       width='5%',
                       height='5%'),
              
              fluidRow(
                
                ###### 2.3.2) Roster Stats ######
                #team stats in 3 tabs - 'in possession' & 'on defense' show 4 factors, 'scoring efficiency' shows 3P, 2P, FT
                column(7,
                       h2(strong("Individual Player Stats")),
                       br(),
                       box(title = "* players with at least 8 MPG and 16 GP",
                           DT::dataTableOutput('teamIdentity-rosterStats'), 
                           width=12)
                ),
                
                # 
                # column(7, 
                #        
                #        h2(strong("8 Most Utilized Players")),
                #        br(),
                #        plotOutput('teamIdentity-usagePie')#, width=12)
                # ),
                
                ###### 2.3.3) Roster Characteristics ######
                # roster grades in 3 tabs - 'floor spacing', 'ball control', 'interior presence'
                column(5, 
                       h2(strong("Roster Characteristics")),
                       br(),
                       tabBox(width = 12,
                              # pie chart of players 
                              tabPanel(strong("Player Utilization"), 
                                       plotOutput('teamIdentity-usagePie'),
                                       width=12),
                              # 'floor spacing' shows shooting gravity, 
                              tabPanel(strong("Floor Spacing"), 
                                       DT::dataTableOutput('teamIdentity-rosterSpacing'), 
                                       width=12),
                              # 'ball control' shows ability to create opportunities & value possessions
                              tabPanel(strong("Ball Control"), 
                                       DT::dataTableOutput('teamIdentity-rosterBallControl'), 
                                       width=12),
                              # 'interior presence' shows a players ability to hold their own in the paint
                              tabPanel(strong("Interior Presence"), 
                                       DT::dataTableOutput('teamIdentity-rosterInteriorPresence'), 
                                       width=12)
                             
                       )))
      ),
      
      
      ###### ____ #####
      
      
      
      
      ###### 2.4) Original Metrics Layout ######
      tabItem(
        
        ###### 2.4.1) ETW Page ######
        tabName = "etw-page",
        
        fluidRow(
          
          column(11, h2(strong("Estimated Tournament Wins (ETW)")),
                       p("are determined using a generalized linear model that is trained on advanced team statistics from 2006 to now.
                    Some of the advanced team statistics used in the model are 
                    KenPom adjusted efficiency margin, extra possessions per 100 court trips, 
                    FT Rate Margin, and others. ETW is best interpreted as a team rating relative to other teams in the same year.
                    ",style = "font-size: 125%;")),
          
          column(1,imageOutput("homePage-logo13",
                               width='5%',
                               height='5%'))
        ),
              
        
        
              
              p("",
                style = "font-size: 125%;"),
              
              br(),
              p(strong("Current Season 2024-25"),
                style = "font-size: 175%;"),
              
              fluidRow(column(4,checkboxInput("etwPage-statsCheckbox", label=strong("Show Offense & Defense Stats",
                                                                                    style = "font-size: 110%;")),
                              # checkboxInput("etwPage-finalFourCheckbox", label=strong("Show Final Four",
                              #                                                     style = "font-size: 120%; color:red;"))
                              ),
                       column(4,selectizeInput("etwPage-filterConf", label = strong("Conference:",
                                                                                    style = "font-size: 125%;"),
                                               choices = c("All", unique(pcsDF$Conference)),
                                               options = list(create = FALSE,placeholder = "Conference",maxItems = '1',
                                                              onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                              onType = I("function (str) {if (str === \"\") {this.close();}}")))),
                       column(4,selectizeInput("etwPage-filterSchool", label = strong("School:",
                                                                                      style = "font-size: 125%;"),
                                               choices = c("All",unique(etwCurr$Team)),
                                               options = list(create = FALSE,placeholder = "School",maxItems = '1',
                                                              onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                              onType = I("function (str) {if (str === \"\") {this.close();}}")))),
              ),
              
              
              box(width=12,DT::dataTableOutput('originalMetrics-etw')), 
              
              # br(),
              # p(strong("All Tome 2007-2023"),
              #   style = "font-size: 175%;"),
              # 
              # fluidRow(column(2,checkboxInput("etwPage-statsCheckboxAllTime", label=p(strong("Show Contributing Stats")))),
              #          column(5,selectizeInput("etwPage-filterConfAllTIme", label = p("Conference:"),
              #                                  choices = c("All", unique(pcsDF$Conference)),
              #                                  options = list(create = FALSE,placeholder = "Conference",maxItems = '1',
              #                                                 onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
              #                                                 onType = I("function (str) {if (str === \"\") {this.close();}}")))),
              #          column(5,selectizeInput("etwPage-filterSchoolAllTime", label = p("School:"),
              #                                  choices = c("All",unique(pcsDF$School)),
              #                                  options = list(create = FALSE,placeholder = "School",maxItems = '1',
              #                                                 onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
              #                                                 onType = I("function (str) {if (str === \"\") {this.close();}}")))),
              # ),
              # 
              # 
              # box(width=12,DT::dataTableOutput('originalMetrics-etwAllTime')), 
              
              ),
      
      
      tabItem(
        ###### 2.4.2) BASIC Page ######
        tabName = "pcs-page",
        
        fluidRow(
          
          column(11, h2(strong("Box Aggregate Score using Impact, Counting and efficiency stats (BASIC)"))),
          
          column(1,imageOutput("homePage-logo14",
                               width='5%',
                               height='5%'))
        ),
              

              p(HTML(paste0("is a simple <u>player</u> rating system that grades players using box-score & impact stats. BASIC is different than other all-in-one player metrics
                 because it accounts for box efficiency and counting stats, as well impact stats. BASIC grades players on BOTH if they do good things on the court 
                 at an efficient rate, as well as impact winning without necessarily 'doing things' (accruing box-score stats). 
                 Most advanced metrics measure either the former or latter. The idea is to be able to look at a collection of players, like a team or conference,
                 and get a <i>basic</i> understanding of how important each player is. Other metrics that only measure counting, effiency, or impact stats will always leave behind certain
                 players that have are effective in some way. BASIC will not. <br>
                 As stated, BASIC is meant to give a basic overview rating. Understanding what exactly makes a player effective, or what their role on a 
                 team is, is not something BASIC can tell you.
                 Incorporating counting stats makes the size of a player's role matter more than other metrics tend to be based on just efficiency.
                <br><br>
                <b>Detials on the calculation:</b> First, BASIC combines box-score counting and efficiency stats to get subscores in
                    5 categories: scoring, defense, playmaking, rebounding, and load. 
                    For example, to evaluate a players rebounding, the counting and effiency stats considered are ORB/G, ORB%, DRB/G, and DRB%.
                    They are weighted and combined to get a raw rebounding subscore that is then normalized. The normalized scores for each of 
                    the 5 categories are weighted and combined into the composite box-score totals. Then, impact is determined using Net-Rating
                    (team's calculated point differential per 100 poss when player is on the court). It is also normalized, and the final box-score composite
                     and impact composite are weighted and combine. For full details and weighting, view the about tab.")),
                style = "font-size: 125%;"),
              
              br(),
              p(strong("Current Season 2024-25"),
                style = "font-size: 175%;"),
              
              fluidRow(column(3,checkboxInput("pcsPage-statsCheckbox", label=strong("Show Contributing Stats",
                                                                                      style = "font-size: 110%;"))),
                        column(3,selectizeInput("pcsPage-filterConf", label = strong("Conference:",
                                                                                     style = "font-size: 125%;"),
                                               choices = c("All", unique(pcsDF$Conference)),
                                               options = list(create = FALSE,placeholder = "Conference",maxItems = '1',
                                                              onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                              onType = I("function (str) {if (str === \"\") {this.close();}}")))),
                       column(3,selectizeInput("pcsPage-filterSchool", label = strong("School:",
                                                                                      style = "font-size: 125%;"),
                                               choices = c("All",unique(pcsDF$School)),
                                               options = list(create = FALSE,placeholder = "School",maxItems = '1',
                                                              onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                              onType = I("function (str) {if (str === \"\") {this.close();}}")))),
                       column(3,selectizeInput("pcsPage-filterPlayer", label = strong("Player:",
                                                                                      style = "font-size: 125%;"),
                                               choices = c("All",unique(pcsDF$Player)),
                                               options = list(create = FALSE,placeholder = "Player",maxItems = '1',
                                                              onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                              onType = I("function (str) {if (str === \"\") {this.close();}}"))))
              ),
              
              
              box(width=12,DT::dataTableOutput('originalMetrics-pcs')), 
              
              br(),
              p(strong("All Time BASIC - 2007-2023"),
                style = "font-size: 175%;"),
              
              fluidRow(column(3,checkboxInput("pcsPage-statsAllTimeCheckbox", label=strong("Show Contributing Stats",
                                                                                           style = "font-size: 110%;"))),
                       column(2,selectizeInput("pcsPage-filterConfAllTime", label = strong("Conference:",
                                                                                           style = "font-size: 125%;"),
                                               choices = c("All","Big 12", "Big Ten", "Big East", "SEC", "ACC", "Pac-12"),
                                               options = list(create = FALSE,placeholder = "Conference",maxItems = '1',
                                                              onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                              onType = I("function (str) {if (str === \"\") {this.close();}}")))),
                       column(2,selectizeInput("pcsPage-filterSchoolAllTime", label = strong("School:",
                                                                                             style = "font-size: 125%;"),
                                               choices = c("All",unique(historicPCS$School)),
                                               options = list(create = FALSE,placeholder = "School",maxItems = '1',
                                                              onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                              onType = I("function (str) {if (str === \"\") {this.close();}}")))),
                       column(2,selectizeInput("pcsPage-filterPlayerAllTime", label = strong("Player:",
                                                                                             style = "font-size: 125%;"),
                                               choices = c("All",unique(historicPCS$Player)),
                                               options = list(create = FALSE,placeholder = "Player",maxItems = '1',
                                                              onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                              onType = I("function (str) {if (str === \"\") {this.close();}}")))),
                       column(2,selectizeInput("pcsPage-filterYearAllTime", label = strong("Year:",
                                                                                           style = "font-size: 125%;"),
                                               choices = c("All",unique(historicPCS$Year)),
                                               options = list(create = FALSE,placeholder = "Year",maxItems = '1',
                                                              onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                              onType = I("function (str) {if (str === \"\") {this.close();}}"))))
              ),
              
             box(width=12, DT::dataTableOutput('originalMetrics-historicPCS')), 
              
              ),
      
      ###### ____ #####
      
      
      ################## 2.5) Visuals Layout ######################
      

      ###### 2.5.1) Pre Built Team Visuals ######
      
      tabItem(tabName = "teams-vis", 
              
              # header
              fluidRow(
                
                column(11,h2(strong("Team Visuals"))),
                
                column(1,
                       imageOutput("homePage-logo4",
                                            width='5%',
                                            height='5%'))
                
              ),
              br(),
              
              ### create tab box with the following tabs:
              ###   efficiency, scoring, defense, playmaking, reboudning
              tabBox(id = "tabset2", width='300px',
                     
                     # efficiency tab
                     tabPanel(strong("Overall", style = "font-size: 130%;"), 
                              
                              ### filters
                              fluidRow(column(3, checkboxInput("vis-teams-overallFilterP6", label=p(strong("Show High-Major Schools Only")))),
                                       column(3, sliderInput("vis-teams-overallFilterN", label=p("Maximum Number of Schools"),min=5, max=length(TeamStats$Team), value=40)),
                                       column(3, p(strong("Dashed Lines are Averages of Top 150 KenPom Teams")))),
                              
                              fluidRow(
                                
                                # left column
                                column(6,

                                       box(width=12,plotOutput("vis-teams-AdjDEvsAdjOE", height = 500)),
                                       box(width=12,plotOutput("vis-teams-AdjEMvsTOPS", height = 500))
                                ),
                                
                                # right column
                                column(6,
                                       
                                       box(width=12,plotOutput("vis-teams-DRtgvsORtg", height = 500)),
                                       box(width=12,plotOutput("vis-teams-NetRtgvsSOS", height = 500))
                                )
                              ),
                     ),
                     
                     # Offense tab
                     tabPanel(strong("Offense", style = "font-size: 130%;"), 
                              
                              ### filters
                              fluidRow(column(3, checkboxInput("vis-teams-offenseFilterP6", label=p(strong("Show High-Major Schools Only")))),
                                       column(3, sliderInput("vis-teams-offenseFilterN", label=p("Maximum Number of Schools"),min=5, max=length(TeamStats$Team), value=40)),
                                       column(3, p(strong("Dashed Lines are Averages of Top 150 KenPom Teams")))),
                              
                              # create two columns of plots
                              fluidRow(
                                
                                # left column
                                column(6, 
                                       box(width=12,plotOutput("vis-teams-AdjOEvsRankAdjOE", height = 500)),
                                       box(width=12,plotOutput("vis-teams-TS%vsPPG", height = 500))
                                ),
                                
                                # right column
                                column(6, 
                                       box(width=12,plotOutput("vis-teams-SOSvsORtg", height = 500)),
                                       box(width=12,plotOutput("vis-teams-eFG%vsPPG", height = 500))
                                ),
                              )
                              
                              # PPG vs TS%
                              # PPG vs eFG%
                              # 2PA vs 2P%
                              # 3PA vs 3P%
                              # FTA vs FT%
                              # PPG vs FG%?
                     ),
                     
                     # defense tab
                     tabPanel(strong("Defense", style = "font-size: 130%;"), 
                              
                              ### filters
                              fluidRow(column(3, checkboxInput("vis-teams-defenseFilterP6", label=p(strong("Show High-Major Schools Only")))),
                                       column(3, sliderInput("vis-teams-defenseFilterN", label=p("Maximum Number of Schools"),min=5, max=length(TeamStats$Team), value=40)),
                                       column(3, p(strong("Dashed Lines are Averages of Top 150 KenPom Teams")))),
                              
                              # create two columns of plots
                              fluidRow(
                                
                                # left column
                                column(6, 
                                       box(width=12,plotOutput("vis-teams-AdjDEvsRankAdjDE", height = 500)),
                                       box(width=12,plotOutput("vis-teams-BLK%vsDRtg", height = 500))
                                ),
                                
                                # right column
                                column(6, 
                                       box(width=12,plotOutput("vis-teams-eFG%_OppvsPPG_Opp", height = 500)),
                                       box(width=12,plotOutput("vis-teams-TOV%_OppvsDRtg", height = 500))
                                ),
                              )
                              
                              
                              # MPG vs DRtg
                              # SPG vs DRtg
                              # BPG vs DRtg
                              # STL% vs BPG
                              # BLK% vs SPG 
                     ),
                     
                     # shooting tab
                     tabPanel(strong("Shooting", style = "font-size: 130%;"), 
                              
                              ### filters
                              fluidRow(column(3, checkboxInput("vis-teams-shootingFilterP6", label=p(strong("Show High-Major Schools Only")))),
                                       column(3, sliderInput("vis-teams-shootingFilterN", label=p("Maximum Number of Schools"),min=5, max=length(TeamStats$Team), value=40)),
                                       column(3, p(strong("Dashed Lines are Averages of Top 150 KenPom Teams")))),
                              
                              fluidRow(
                                
                                
                                
                                # left column
                                column(6,
                                       
                                       box(width=12,plotOutput("vis-teams-2P%vs2PA", height = 500)),
                                       box(width=12,plotOutput("vis-teams-FT%vsFTA", height = 500))
                                       
                                ),
                                
                                # right column
                                column(6,
                                       box(width=12,plotOutput("vis-teams-3P%vs3PA", height = 500)),
                                       box(width=12,plotOutput("vis-teams-TS%vsPPG2", height = 500))
                                       
                                )
                              ),
                     ),
                     
                     # playmaking tab
                     tabPanel(strong("Playmaking", style = "font-size: 130%;"), 
                              
                              ### filters
                              fluidRow(column(3, checkboxInput("vis-teams-playmakingFilterP6", label=p(strong("Show High-Major Schools Only")))),
                                       column(3, sliderInput("vis-teams-playmakingFilterN", label=p("Maximum Number of Schools"),min=5, max=length(TeamStats$Team), value=40)),
                                       column(3, p(strong("Dashed Lines are Averages of Top 150 KenPom Teams")))),
                              
                              # create two columns of plots
                              fluidRow(
                                
                                # left column
                                column(6, 
                                       box(width=12,plotOutput("vis-teams-TOV%vsAST%", height = 500)),
                                       box(width=12,plotOutput("vis-teams-AdjTempovsTOV%", height = 500))
                                ),
                                
                                # right column
                                column(6, 
                                       box(width=12,plotOutput("vis-teams-TOVvsAPG", height = 500)),
                                       box(width=12,plotOutput("vis-teams-AdjTempovsAST%", height = 500))
                                ),
                              )
                              
                              # APG vs TOV
                              # AST% vs TOV%
                              # APG vs AST%
                              # TOV vs TOV%
                     ),
                     
                     # rebounding tab
                     tabPanel(strong("Rebounding", style = "font-size: 130%;"), 
                              
                              ### filters
                              fluidRow(column(3, checkboxInput("vis-teams-reboundingFilterP6", label=p(strong("Show High-Major Schools Only")))),
                                       column(3, sliderInput("vis-teams-reboundingFilterN", label=p("Maximum Number of Schools"),min=5, max=length(TeamStats$Team), value=40)),
                                       column(3, p(strong("Dashed Lines are Averages of Top 150 KenPom Teams")))),
                              
                              # create two columns of plots
                              fluidRow(
                                
                                # left column
                                column(6, 
                                       box(width=12,plotOutput("vis-teams-ORB%vsDRB%", height = 500)),
                                       box(width=12,plotOutput("vis-teams-DRB%vsRPG", height = 500))
                                ),
                                
                                # right column
                                column(6, 
                                       box(width=12,plotOutput("vis-teams-ORB%vsRPG", height = 500))
                                ),
                              )
                              
                              # DRB vs ORB
                              # DRB% vs ORB%
                              # DRB vs DRB%
                              # ORB vs ORB%
                     )
                     
              )
      ),

      ###### 2.5.2) Pre Built Player Visuals ######

      
      tabItem(tabName = "player-vis", 
              
              
              # header 
              fluidRow(
                
                column(11,
                       h2(strong("Player Visuals")),
                ),
                column(1,imageOutput("homePage-logo11",
                                     width='5%',
                                     height='5%'))
              ),
              
              br(),
              tabBox(
                
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", width='300px',
                tabPanel(strong("Efficiency", style = "font-size: 130%;"), 
                         
                         ### filters
                         fluidRow(column(3, numericInput("vis-players-impactFilterGP", label=p("Min. Games Played:"), min = 1, max = max(Players$GP), value=3),
                                         checkboxInput("vis-players-impactFilterP6", label=p("Show Power-5 Players Only"))),
                                  column(3, numericInput("vis-players-impactFilterMPG", label=p("Min. Minutes per Game:"), min = 1, max = 39, value=10)),
                                  column(3, numericInput("vis-players-impactFilterUSG", label=p("Min. Usage %:"), min = 1, max = max(Players$`USG%`)-1, value=10)),
                                  column(3, sliderInput("vis-players-impactFilterN", label=p("Max. Number of Players"),min=10, max=length(Players$GP), value=200))),
                         #column(1, checkboxInput("vis-players-impactFilterP6", label=p("Show Power-5 Players Only")))),
                         ### plots
                         fluidRow(
                           
                           # left column
                           column(6, 
                                  box(plotlyOutput("vis-players-PERvsPCS", height = 525), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-players-DRtgvsORtg", height = 525), height=575, width=12),
                                   br(),
                                   box(plotlyOutput("vis-players-NetRtgvsPER", height = 525), height=575, width=12)),
                           # right column
                           column(6, 
                                  box(plotlyOutput("vis-players-NetRtgvsMPG", height = 525), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-players-NetRtgvsPCS", height = 525), height=575, width=12))
                         )
                         
                ),
                tabPanel(strong("Scoring", style = "font-size: 130%;"), 
                         
                         ### filters
                         fluidRow(column(3, numericInput("vis-players-scoringFilterGP", label=p("Min. Games Played:"), min = 1, max = max(Players$GP), value=3),
                                         checkboxInput("vis-players-scoringFilterP6", label=p("Show Power-5 Players Only"))),
                                  column(3, numericInput("vis-players-scoringFilterMPG", label=p("Min. Minutes per Game:"), min = 1, max = 39, value=10)),
                                  column(3, numericInput("vis-players-scoringFilterUSG", label=p("Min. Usage %:"), min = 1, max = max(Players$`USG%`)-1, value=10)),
                                  column(3, sliderInput("vis-players-scoringFilterN", label=p("Max. Number of Players"),min=10, max=length(Players$GP), value=200))),
                         
                         ### plots
                         fluidRow(
                           
                           # left column
                           column(6, 
                                  box(plotlyOutput("vis-player-ORtgvsMPG", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-eFG%vsPPG", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-2P%vs2PA", height = 575), height=575, width=12)
                           ),
                           
                           # right column
                           column(6, 
                                  box(plotlyOutput("vis-player-TS%vsPPG", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-3P%vs3PA", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-FT%vsFTA", height = 575), height=575, width=12)
                           )
                         )
                         
                ),
                tabPanel(strong("Defense", style = "font-size: 130%;"), 
                         
                         ### filters
                         fluidRow(column(3, numericInput("vis-players-defenseFilterGP", label=p("Min. Games Played:"), min = 1, max = max(Players$GP), value=3),
                                         checkboxInput("vis-players-defenseFilterP6", label=p("Show Power-5 Players Only"))),
                                  column(3, numericInput("vis-players-defenseFilterMPG", label=p("Min. Minutes per Game:"), min = 1, max = 39, value=10)),
                                  column(3, numericInput("vis-players-defenseFilterUSG", label=p("Min. Usage %:"), min = 1, max = max(Players$`USG%`)-1, value=10)),
                                  column(3, sliderInput("vis-players-defenseFilterN", label=p("Max. Number of Players"),min=10, max=length(Players$GP), value=200))),
                         
                         ### plots
                         fluidRow(
                           
                           # left column
                           column(6,
                                  box(plotlyOutput("vis-player-DRtgvsMPG", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-SPGvsBPG", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-BLK%vsBPG", height = 575), height=575, width=12)),
                           
                           # right column
                           column(6,
                                  box(plotlyOutput("vis-player-DRtgvsBPG", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-DRtgvsSPG", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-STL%vsSPG", height = 575), height=575, width=12))
                         )
                ),
                tabPanel(strong("Playmaking", style = "font-size: 130%;"), 
                         
                         ### filters
                         fluidRow(column(3, numericInput("vis-players-playmakingFilterGP", label=p("Min. Games Played:"), min = 1, max = max(Players$GP), value=3),
                                         checkboxInput("vis-players-playmakingFilterP6", label=p("Show Power-5 Players Only"))),
                                  column(3, numericInput("vis-players-playmakingFilterMPG", label=p("Min. Minutes per Game:"), min = 1, max = 39, value=10)),
                                  column(3, numericInput("vis-players-playmakingFilterUSG", label=p("Min. Usage %:"), min = 1, max = max(Players$`USG%`)-1, value=10)),
                                  column(3, sliderInput("vis-players-playmakingFilterN", label=p("Max. Number of Players"),min=10, max=length(Players$GP), value=200))),
                         
                         ### plots
                         fluidRow(
                           
                           # left column
                           column(6,
                                  box(plotlyOutput("vis-player-TOVvsAPG", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-TOV%vsAPG", height = 575), height=575, width=12),
                           ),
                           
                           # right column
                           column(6,
                                  box(plotlyOutput("vis-player-AST%vsAPG", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-TOV%vsAST%", height = 575), height=575, width=12),
                           )
                           
                         )
                ),
                tabPanel(strong("Rebounding", style = "font-size: 130%;"), 
                         
                         ### filters
                         fluidRow(column(3, numericInput("vis-players-reboundingFilterGP", label=p("Min. Games Played:"), min = 1, max = max(Players$GP), value=3),
                                         checkboxInput("vis-players-reboundingFilterP6", label=p("Show Power-5 Players Only"))),
                                  column(3, numericInput("vis-players-reboundingFilterMPG", label=p("Min. Minutes per Game:"), min = 1, max = 39, value=10)),
                                  column(3, numericInput("vis-players-reboundingFilterUSG", label=p("Min. Usage %:"), min = 1, max = max(Players$`USG%`)-1, value=10)),
                                  column(3, sliderInput("vis-players-reboundingFilterN", label=p("Max. Number of Players"),min=10, max=length(Players$GP), value=200))),
                         
                         
                         ### plots
                         fluidRow(
                           
                           # left column
                           column(6,
                                  box(plotlyOutput("vis-player-ORBvsDRB", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-DRB%vsDRB", height = 575), height=575, width=12),
                           ),
                           
                           # right column
                           column(6,
                                  box(plotlyOutput("vis-player-ORB%vsORB", height = 575), height=575, width=12),
                                  br(),
                                  box(plotlyOutput("vis-player-ORB%vsDRB%", height = 575), height=575, width=12),
                           )
                           
                         )
                         
                         # DRB vs ORB
                         # DRB% vs ORB%
                         # DRB vs DRB%
                         # ORB vs ORB%
                )
              )),
      
      
      
      ###### 2.5.3) Custom Player Visuals ######
      tabItem(tabName = "custom-player-vis", 
              
              
              # header 
              fluidRow(
                
                column(11,
                       h2(strong("Custom Player Visuals")),
                ),
                       column(1,imageOutput("homePage-logo12",
                                            width='5%',
                                            height='5%'))
                ),
              
              
              
              
              
              #strong(p('Team Ratings are based on past team stats and tournament outcomes.')),
              br(),
              fluidRow(
                
                column(4, 
                       
                       # select x and y axis stats
                       box(height=180, width=12,
                           title=h2(strong("Axes")),
                           br(),
                           fluidRow(column(6,selectizeInput("cusPlaVis-xAxis", label = p(strong("X-axis")),choices = PlayersSelect, selected="BASIC",
                                                            options = list(create = FALSE,placeholder = "X axis",maxItems = '1',
                                                                           onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                                           onType = I("function (str) {if (str === \"\") {this.close();}}"))),
                                           checkboxInput("cusPlaVis-invertXcheck", strong("Invert X-axis"), FALSE)
                                           ),
                                    column(6,selectizeInput("cusPlaVis-yAxis", label = p(strong("Y-axis")),choices = PlayersSelect, selected="PER",
                                                            options = list(create = FALSE,placeholder = "Y axis",maxItems = '1',
                                                                           onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                                           onType = I("function (str) {if (str === \"\") {this.close();}}"))),
                                           checkboxInput("cusPlaVis-invertYcheck", strong("Invert Y-axis"), FALSE)
                                    )
                           )
                       ),
                       box(height=600, width=12,
                           
                           ### filters
                           
                           title=h2(strong("Filters")),
                           br(),
                           
                           # show top N in selected stat
                           p(strong("Show only top N players in Stat for NCAA")),
                           
                           fluidRow(column(6,selectizeInput("cusPlaVis-topNinStat", label = p("Stat:"),choices = PlayersSelect,
                                                            options = list(create = FALSE,placeholder = "PPG, 3P%, etc",maxItems = '1',
                                                                           onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                                           onType = I("function (str) {if (str === \"\") {this.close();}}")))),
                                    column(6,numericInput("cusPlaVis-topNinStatNumVal", p(paste0("N: (max=", length(Players$Player), ")")), length(Players$Player), min = 1, max = length(Players$Player)))),
                           br(),
                           checkboxInput("cusPlaVis-power6check", strong("Show Power-5 Players only"), FALSE),
                           br(),
                           # filter by conference or school
                           fluidRow(column(6,selectizeInput("cusPlaVis-filterConf", label = strong("Conference:",
                                                                                                   style = "font-size: 125%;"),choices = (c("NCAA", "Big 12", "Big Ten", "Big East", "SEC", "ACC", 'Pac-12')),
                                                            options = list(create = FALSE,placeholder = "Conference",maxItems = '1',
                                                                           onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                                           onType = I("function (str) {if (str === \"\") {this.close();}}")))),
                                    column(6,selectizeInput("cusPlaVis-filterSchool", label = p("School:",
                                                                                                style = "font-size: 125%;"),choices = (c("All", levels(Players$School))),
                                                            options = list(create = FALSE,placeholder = "School",maxItems = '1',
                                                                           onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                                           onType = I("function (str) {if (str === \"\") {this.close();}}"))))
                           ),
                           br(),
                           
                           # general filters, show players with at least or at most a given amount in given stat
                           p(strong("General Filters:")),
                           fluidRow(column(4,selectInput("cusPlaVis-filterStat1", label = p(),choices = c("Disabled","More than","Less than"))),
                                    column(3,numericInput("cusPlaVis-filterVal1", p(), min = 1, max = 100, step=0.05, value=0)),      
                                    column(5,selectInput("cusPlaVis-filterCol1", label = p(),selected="MPG",choices = PlayersSelect))),
                           fluidRow(column(4,selectInput("cusPlaVis-filterStat2", label = p(),choices = c("Disabled","More than","Less than"))),
                                    column(3,numericInput("cusPlaVis-filterVal2", p(), min = 1, max = 100, step=0.05, value=0)),      
                                    column(5,selectInput("cusPlaVis-filterCol2", label = p(),choices = PlayersSelect, selected="GP"))),
                           fluidRow(column(4,selectInput("cusPlaVis-filterStat3", label = p(),choices = c("Disabled","More than","Less than"))),
                                    column(3,numericInput("cusPlaVis-filterVal3", p(), min = 1, max = 1000, step=0.2, value=0)),      
                                    column(5,selectInput("cusPlaVis-filterCol3", label = p(),choices = PlayersSelect, selected="USG%")))
                       )),
                
                ### custom player scatter plot
                column(8, box(title = "CBB DASH CUSTOM PLOT", plotlyOutput("vis-custom-playerScatter", height = 780), height = 800, width=12)))
      ),
      
      
      
      ###### 2.5.4) Custom Team Visuals ######
      tabItem(tabName = "custom-team-vis", 
              
              
              # header 
              fluidRow(
                
                column(11,
                       h2(strong("Custom Team Visuals")),
                       tags$a(href="https://spmur15.shinyapps.io/cbb-scatter/", 
                              strong("***Click here for full custom scatter plot app***",
                                     style = "font-size: 167%;"), target="_blank"),
                ),
                       column(1,imageOutput("homePage-logo10",
                                            width='5%',
                                            height='5%'))
                ),
              
              #strong(p('Team Ratings are based on past team stats and tournament outcomes.')),
              br(),
              fluidRow(
                
                column(4, 
                       
                       # select x and y axis stats
                       box(height=180, width=12,
                           title=h2(strong("Axes")),
                           br(),
                           fluidRow(column(6,selectizeInput("cusTeamVis-xAxis", label = p(strong("X-axis")),choices = teamsSelect, selected="AdjOE",
                                                            options = list(create = FALSE,placeholder = "X axis",maxItems = '1',
                                                                           onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                                           onType = I("function (str) {if (str === \"\") {this.close();}}"))),
                                           checkboxInput("cusTeamVis-invertXcheck", strong("Invert X-axis"), FALSE)
                           ),
                           column(6,selectizeInput("cusTeamVis-yAxis", label = p(strong("Y-axis")),choices = teamsSelect, selected="AdjDE",
                                                   options = list(create = FALSE,placeholder = "Y axis",maxItems = '1',
                                                                  onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                                  onType = I("function (str) {if (str === \"\") {this.close();}}"))),
                                  checkboxInput("cusTeamVis-invertYcheck", strong("Invert Y-axis"), TRUE)
                           )
                           )
                       ),
                       box(height=600, width=12,
                           
                           ### filters
                           
                           title=h2(strong("Filters")),
                           br(),
                           
                           # show top N in selected stat
                           p(strong("Show only top N Teams in STAT")),
                           
                           fluidRow(column(6,selectizeInput("cusTeamVis-topNinStat", label = p("STAT:"),choices = c("Kenpom.AdjEM", teamsSelect), selected="Kenpom.AdjEM",
                                                            options = list(create = FALSE,placeholder = "PPG, 3P%, etc",maxItems = '1',
                                                                           onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                                           onType = I("function (str) {if (str === \"\") {this.close();}}")))),
                                    column(6,numericInput("cusTeamVis-topNinStatNumVal", p(paste0("N: (max=", length(Teams$School), ")")), 50, min = 1, max = length(Teams$School)))),
                           br(),
                           checkboxInput("cusTeamVis-power6check", strong("Show Power-5 Teams only"), FALSE),
                           br(),
                           # filter by conference or school
                           fluidRow(column(6,selectizeInput("cusTeamVis-filterConf", label = p("Conf: (Default=\"NCAA\")"),choices = c("NCAA", unique(Teams$Conference)),
                                                            options = list(create = FALSE,placeholder = "Conference",maxItems = '1',
                                                                           onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                                           onType = I("function (str) {if (str === \"\") {this.close();}}")))),
                                    column(6,selectizeInput("cusTeamVis-filterSchool", label = p("School: (Default=\"All\")"),choices = c("All", unique(Teams$School)),
                                                            options = list(create = FALSE,placeholder = "School",maxItems = '1',
                                                                           onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                                                           onType = I("function (str) {if (str === \"\") {this.close();}}"))))
                           ),
                           br(),

                           # general filters, show players with at least or at most a given amount in given stat
                           p(strong("General Filters:")),
                           fluidRow(column(4,selectInput("cusTeamVis-filterStat1", label = p(),choices = c("Disabled","More than","Less than"))),
                                    column(3,numericInput("cusTeamVis-filterVal1", p(), min = 1, max = 100, step=0.05, value=0)),
                                    column(5,selectInput("cusTeamVis-filterCol1", label = p(),selected="Kenpom.Rank",choices = c("Kenpom.Rank", teamsSelect)))),
                           fluidRow(column(4,selectInput("cusTeamVis-filterStat2", label = p(),choices = c("Disabled","More than","Less than"))),
                                    column(3,numericInput("cusTeamVis-filterVal2", p(), min = 1, max = 100, step=0.05, value=0)),
                                    column(5,selectInput("cusTeamVis-filterCol2", label = p(),choices = teamsSelect, selected="AdjTempo"))),
                           fluidRow(column(4,selectInput("cusTeamVis-filterStat3", label = p(),choices = c("Disabled","More than","Less than"))),
                                    column(3,numericInput("cusTeamVis-filterVal3", p(), min = 1, max = 1000, step=0.2, value=0)),
                                    column(5,selectInput("cusTeamVis-filterCol3", label = p(),choices = teamsSelect, selected="3PAp100")))
                       )),
                
                ### custom player scatter plot
                column(8, box(title = "CBB DASH CUSTOM PLOT", plotOutput("vis-custom-teamScatter", height = 780), height = 800, width=12)))
      ),
      
      #### ____ ####
      
      
      
      
      
      
      
      
      

      ################ 2.6) Game Slate Layout #####################

      
      
      
      
      ###### 2.6.1) Today ######
      tabItem(tabName = "today", 
              
              # header
              fluidRow(
                
                column(11,
                       h2(strong(paste("Today's Games"),style = "font-size: 120%;")), 
                       h2(strong(update)),style = "font-size: 90%;"),
                
                column(1,
                       imageOutput("homePage-logo5",
                                            width='5%',
                                            height='5%'))
                
              ),
              # header
              
              
              br(),
              
              fluidRow(
                
                # conference filter
                column(5,selectInput("Conference-today", strong("Conference:",
                                                                style = "font-size: 140%;"),
                                     c('All', 'Big 12', 'Big Ten', 'Big East', 'SEC', 'ACC', 'Mountain West', 
                                       sort(unique(as.character(Teams$Conference[!Teams$Conference %in% c('Big 12', 'Big Ten', 'Big East', 'SEC', 'ACC','Mountain West')]))))))
                ),
              
              # today's games
              fluidRow(uiOutput("ibox"))),
      
      
      
      ###### 2.6.2) This Week ######
      tabItem(tabName = "thisWeek", 
              
              # header
              fluidRow(
                
                column(11,
                       # header
                       #h2(strong(paste('Games for Feast Week')))),
                       h2(strong(paste('Games for Week', as.factor(This.Week$Week[1])),style = "font-size: 120%;"))),
                       
                column(1,
                       imageOutput("homePage-logo9",
                                   width='5%',
                                   height='5%'))
                
              ),
              
              br(),
              
              fluidRow(
                # conference filter
                column(4,selectInput("Conference-thisWeek", strong("Conference:",
                                                                   style = "font-size: 125%;"),
                                     c('All', 'Big 12', 'Big Ten', 'Big East', 'SEC', 'ACC', 'Mountain West', 
                                       sort(unique(as.character(Teams$Conference[!Teams$Conference %in% c('Big 12', 'Big Ten', 'Big East', 'SEC', 'ACC','Mountain West')]))))))),
              
              # this week's games
              fluidRow(uiOutput("ibox2"))),
      

      #### ____ ####
      
      
      
      ################ 2.7) Bracketology Layout ###################

      
      tabItem(tabName = "bracketology", 
              
              
              
              # header
              fluidRow(
                
                column(11,
                       h2(strong("Dash Bracketology")),
                       # p(strong("...will be updated in 2025"),
                       #   style = "font-size: 125%;"),
                       p(strong("Dash Bracketology"),"uses two predicitve models to determine the field of 68 and their seeds. The first is a resume based model
                       to determine the at-large bids. Then a model trained on team quality metrics determines the likely autobids, as well as the seeding of the resulting 68 teams.",
                       #This is meant to mimic the committee's process, where they have said resume metrics get you in the tournament
                       #and performance based metrics determine seed.",
                         style = "font-size: 125%;"),
                       p(strong("Dash Bracketology Model is trained with a binary outcome: making or missing the tournemnt. Unlike Estimated Tournament Wins, which is trained numeric outcomes based on tournament round reached.")),
                ),
                column(1,
                       imageOutput("homePage-logo6",
                                            width='5%',
                                            height='5%'))
                
              ),
              
              br(),
              
              fluidRow(
                # conference filter
                column(4,selectInput("Conference-bracketology", strong("Conference:",
                                                                       style = "font-size: 135%;"),
                                     c('NCAA', 'B12', 'B10', 'BE', 'SEC', 'ACC', 
                                       sort(unique(as.character(Bracketology$Conference[!Bracketology$Conference %in% c('B12', 'B10', 'BE', 'SEC', 'ACC')]))))),

                       # checkbox - switches between teams as infoboxes and in DT
                       checkboxInput("bracketology-checkbox", strong("Show Full Resumes",
                                                                     style = "font-size: 110%;"), FALSE))),

              # bracketology teams
              fluidRow(column(12,uiOutput("bracketologySwitch")))
              ),
      

      ###### ____ #####
      
      
      ################ 2.8) Season Awards Layout ##################

      
      tabItem(tabName = "seasonAwards", 
              
              # header
              
              
              # header
              fluidRow(
                
                column(11,
                       h2(strong("Season Awards Models")),
                       p(strong("Season Award Predictions"),"are based on predictive models that incorporate player stats
                                       and team success. There are seven models, one for All-American predictions and six for All-Conference
                                       predictions among Power-5 conferences. They are trained on data going back to 2006.",
                         style = "font-size: 125%;"),
                       h2(strong("All-American Predictions"))),
                       column(1,
                              imageOutput("homePage-logo7",
                                                   width='5%',
                                                   height='5%'))),
              br(),
              
              
              ###### 2.8.1) All Americans ######
              # all americans
              #     all teams have checkbox to add full player stats to DT
              tabBox(id = "tabset1", width='300px',
                     tabPanel(strong("First Team", style = "font-size: 130%;"), 
                              checkboxInput("aa-1-checkbox", strong("Show Regular Season Stats", style = "font-size: 112%;"), FALSE),
                              DT::dataTableOutput("AAfirst")),
                     tabPanel(strong("Second Team", style = "font-size: 130%;"),  
                              checkboxInput("aa-2-checkbox", strong("Show Regular Season Stats", style = "font-size: 112%;"), FALSE),
                              DT::dataTableOutput("AAsecond")),
                     tabPanel(strong("Third Team", style = "font-size: 130%;"),  
                              checkboxInput("aa-3-checkbox", strong("Show Regular Season Stats", style = "font-size: 112%;"), FALSE),
                              DT::dataTableOutput("AAthird")),
                     tabPanel(strong("Honorable Mentions", style = "font-size: 130%;"),  
                              checkboxInput("aa-hm-checkbox", strong("Show Regular Season Stats", style = "font-size: 112%;"), FALSE),
                              DT::dataTableOutput("AAhm"))),
              
              
              ###### 2.8.2) All Conference ######
              # header
              h2(strong('All-Conference Predictions')),
              
              # power 6 all conference
              #     all teams have checkbox to add full player stats to DT  
              tabBox(id = "tabset1", width='300px',
                     tabPanel(strong("Big 12", style = "font-size: 130%;"),  
                              checkboxInput("big12-checkbox", strong("Show Stats in Conference Games", style = "font-size: 112%;"), FALSE),
                              #strong("First Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("big12"),
                              br(),
                              #strong("Second Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("big12-2"),
                              br(),
                              #strong("Third Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("big12-3"),
                              br(),
                              #strong("Honorable Mention", style = "font-size: 125%;"),
                              DT::dataTableOutput("big12-4")),
                     
                     tabPanel(strong("Big East", style = "font-size: 130%;"), 
                              checkboxInput("bigEast-checkbox", strong("Show Stats in Conference Games", style = "font-size: 112%;"), FALSE),
                              #strong("First Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("bigEast"),
                              br(),
                              #strong("Second Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("bigEast-2"),
                              br(),
                              #strong("Third Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("bigEast-3"),
                              br(),
                              #strong("Honorable Mention", style = "font-size: 125%;"),
                              DT::dataTableOutput("bigEast-4")),
                     
                     tabPanel(strong("Big Ten", style = "font-size: 130%;"),  
                              checkboxInput("bigTen-checkbox", strong("Show Stats in Conference Games", style = "font-size: 112%;"), FALSE),
                              #strong("First Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("bigTen"),
                              br(),
                              #strong("Second Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("bigTen-2"),
                              br(),
                              #strong("Third Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("bigTen-3"),
                              br(),
                              #strong("Honorable Mention", style = "font-size: 125%;"),
                              DT::dataTableOutput("bigTen-4")),
                     
                     tabPanel(strong("SEC", style = "font-size: 130%;"),  
                              checkboxInput("sec-checkbox", strong("Show Stats in Conference Games", style = "font-size: 112%;"), FALSE),
                              #strong("First Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("SEC"),
                              br(),
                              #strong("Second Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("SEC-2"),
                              br(),
                              #strong("Third Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("SEC-3"),
                              br(),
                              #strong("Honorable Mention", style = "font-size: 125%;"),
                              DT::dataTableOutput("SEC-4")),
                     
                     tabPanel(strong("ACC", style = "font-size: 130%;"),  
                              checkboxInput("acc-checkbox", strong("Show Stats in Conference Games", style = "font-size: 112%;"), FALSE),
                              #strong("First Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("ACC"),
                              br(),
                              #strong("Second Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("ACC-2"),
                              br(),
                              #strong("Third Team", style = "font-size: 125%;"),
                              DT::dataTableOutput("ACC-3"),
                              br(),
                              #strong("Honorable Mention", style = "font-size: 125%;"),
                              DT::dataTableOutput("ACC-4")),
                     
                     br(),
                     #tags$p("* Predictions are made from conference games only, predictions are likely to change as number of games increases", style = "font-size: 120%;")
                     
                     # tabPanel(strong("Pac-12", style = "font-size: 130%;"),  
                     #          checkboxInput("pac12-checkbox", strong("Show Stats in Conference Games", style = "font-size: 112%;"), FALSE),
                     #          #strong("First Team", style = "font-size: 125%;"),
                     #          DT::dataTableOutput("pac12"),
                     #          br(),
                     #          #strong("Second Team", style = "font-size: 125%;"),
                     #          DT::dataTableOutput("pac12-2"),
                     #          br(),
                     #          #strong("Third Team", style = "font-size: 125%;"),
                     #          DT::dataTableOutput("pac12-3"),
                     #          br(),
                     #          #strong("Honorable Mention", style = "font-size: 125%;"),
                     #          DT::dataTableOutput("pac12-4"))
                     
                     )),
      
      
      
      #### ____ ####
      
      
      ################### 2.9) About Layout #######################

      
      
      #print(13/16)
      tabItem(tabName = "about", 
              
              # header
              fluidRow(
                
                column(11,
                       h2(strong("About"), style = "font-size: 350%;")),
                       column(1,
                              imageOutput("homePage-logo8",
                                                   width='5%',
                                                   height='5%'))
                       
                ),
              
              br(),br(),
              box(width=12,
                  
              ###### 2.9.1) Bio ######
              title=h2(strong("Bio")),

              br(),
              strong(HTML(paste0("My name is Sam Murray, I am a data analyst and college basketball enthusiast. I studied
                 data science and computer science at the University of Wisconsin and graduated in 2022. You can contact me
                 on ", tags$a(href="https://twitter.com/CBB_Players", "Twitter", target="_blank"),  
                            " or ", 
                            tags$a(href="https://www.linkedin.com/in/sam-murray-8252541a2/", "Linkedin", target="_blank"),
                            " with any questions or comments.")),
                style = "font-size: 135%;"),
              
              br(),br(),
              
              p(HTML(paste0("If you're interested in college basketball advanced stats, 
                            check out these great sites that inspired much of this app: ", 
                            #strong("Advanced Stats:"), 
                            tags$a(href="https://kenpom.com/index.php", "KenPom", target="_blank"), ", ",
                            tags$a(href="https://barttorvik.com/#", "BartTorvik", target="_blank"), ", ",
                            tags$a(href="https://haslametrics.com/", "Haslametrics", target="_blank"), ", ",
                            tags$a(href="https://evanmiya.com/", "EvanMiya", target="_blank"), ", ",
                            tags$a(href="https://cbbanalytics.com/", "CBB Analytics", target="_blank"), ", ",
                            tags$a(href="https://github.com/andreweatherman/cbbdata", "CBBData (via Andrew Weatherman)", target="_blank"), ", ",
                            tags$a(href="https://hoop-explorer.com/OnOffAnalyzer?", "Hoop-Explorer (On/Off)", target="_blank"), ", ",
                            tags$a(href="https://twitter.com/3MW_CBB", "Three Man Weave", target="_blank"), ", ",
                            tags$a(href="https://www.teamrankings.com/ncb/", "Team Rankings", target="_blank"), ", ",
                            tags$a(href="http://www.bracketmatrix.com/", "Bracket Matrix", target="_blank"), ", ",
                            tags$a(href="https://synergysports.com/sport/basketball/", "Synergy Sports", target="_blank"), ", ",
                            tags$a(href="https://www.sports-reference.com/cbb/", "College Basketball Reference", target="_blank"),# ", ",
                            #tags$a(href="https://cbbalmanac.com/", "The Almanac", target="_blank"),
                            
                            "."
                            ),
                            
                            ),
                style = "font-size: 135%;"),
              
              br(),
              
              
              p(HTML(paste0("If you're looking for an explanation of any stats go here: ", 
                            #strong("Advanced Stats:"), 
                            tags$a(href="https://www.basketball-reference.com/about/glossary.html", "Basketball Reference Glossary", target="_blank"),
                            "."
              )
              ),
              style = "font-size: 135%;")
              
              # br(),
              
              # p(HTML(paste0("If you're interested in year round CBB discussion, check out the Three Man Weave
              #               podcast (which I am in no way associated with): ", 
              #               #strong("Advanced Stats:"), 
              #               tags$a(href="https://twitter.com/3MW_CBB", "Three Man Weave"),
              #               "."
              # )
              # ),
              # style = "font-size: 135%;")
              
              
              
              ### Add links to inspiration sites
              
              
              ),
              
              
              br(),br(),
              box(width=12, 
                  title=h2(strong("Original Metrics")),
                  #br(),br(),
                  #p(strong("CBB Dash has created two original college basketball metrics."), 
                    #style = "font-size: 150%;"),
                  br(),
                  p(strong("Estimated Tournament Wins (ETW)"),
                    "are determined using a generalized linear model that is trained on advanced team statistics from 2006 to now.
                    Some of the advanced team statistics used in the model are 
                    KenPom adjusted efficiency margin, extra possessions per 100 court trips, 
                    FT Rate Margin, and others. ETW is best interpreted as a team rating relative to other teams in the same year.
                    ", 
                    style = "font-size: 125%;")
              ),
              box(width=12,
                  br(),
                  p(strong("Box Aggregate Score using Impact, Counting, and efficiency stats (BASIC)"),
                    HTML(paste0("is a simple <u>player</u> rating system that grades players using box-score & impact stats. BASIC is different than other all-in-one player metrics
                 because it accounts for box efficiency and counting stats, as well impact stats. BASIC grades players on BOTH if they do good things on the court 
                 at an efficient rate, as well as impact winning without necessarily 'doing things' (accruing box-score stats). 
                 Most advanced metrics measure either the former or latter. The idea is to be able to look at a collection of players, like a team or conference,
                 and get a <i>basic</i> understanding of how important each player is. Other metrics that only measure counting, effiency, or impact stats will always leave behind certain
                 players that have are effective in some way. BASIC will not. <br>
                 As stated, BASIC is meant to give a basic overview rating. Understanding what exactly makes a player effective, or what their role on a 
                 team is, is not something BASIC can tell you.
                 Incorporating counting stats makes the size of a player's role matter more than other metrics tend to be based on just efficiency.
                <br><br>
                <b>Detials on the calculation:</b> First, BASIC combines box-score counting and efficiency stats to get subscores in
                    5 categories: scoring, defense, playmaking, rebounding, and load. 
                    For example, to evaluate a players rebounding, the counting and effiency stats considered are ORB/G, ORB%, DRB/G, and DRB%.
                    They are weighted and combined to get a raw rebounding subscore that is then normalized. The normalized scores for each of 
                    the 5 categories are weighted and combined into the composite box-score totals. Then, impact is determined using Net-Rating
                    (team's calculated point differential per 100 poss when player is on the court). It is also normalized, and the final box-score composite
                     and impact composite are weighted and combine. For full details and weighting, view the about tab.")),
                    style = "font-size: 125%;")
                  ),
              br(),br(),br(),
              
              h2(strong("Explanation by Sidebar Tabs")),
              br(),
              tabBox(width=12,
                     #title=h2(strong("Explanation by Tabs")),
                  
                     # tabPanel(strong("Full Stats & Ratings"), 
                     #          h2(strong("Full Stats & Ratings")),
                     #          br(),
                     #          p("Shows all D1 teams and players with full box score and efficency statistics.",
                     #          style = "font-size: 135%;"),
                     #          p("Filter by conference, team, or year in school.",
                     #            style = "font-size: 135%;")
                     #          ),
                    
                    ###### 2.9.2) Team Identity Explaination ######
                    tabPanel(strong("Team Identity"),
                             h2(strong("Team Identity")),
                             br(),
                             p(strong("Presents a dashboard on on all statistics for a chosen team."),
                             style = "font-size: 180%;"),
                             br(),
                             
                             p(strong("Team Stats"),
                               style = "font-size: 180%;"),

                             #p("Overall team stats are from KenPom.com, the standard in the college basketball community 
                             #  to measure offensive and defensive efficiency.", 
                                #When you hear someone on ESPN say \"Gonzaga is the top offense this season\" they are referencing Kenpom's Adjusted Offensive Efficiency rating.",
                              #  style = "font-size: 135%;"),
                             #p("Specific team stats show major elements of team with rate stats. 
                             #  If you want to learn more about these stats I recommend basketball-reference.com/about/glossary.html",
                             #  style = "font-size: 135%;"),
                             
                             p("Letter grades are assigned based on percentile among all 362 D1 teams.",
                               style = "font-size: 135%;"),
                             p(strong("—  A:"),">= 85th %ile",
                               style = "font-size: 115%;"),
                             p(strong("—  B:"),"84th - 70th %ile",
                               style = "font-size: 115%;"),
                             p(strong("—  C:"), "69th - 55th %ile",
                               style = "font-size: 115%;"),
                             p(strong("—  D:"), "54th - 30th %ile",
                               style = "font-size: 115%;"),
                             p(strong("—  F:"), "<= 30th %ile",
                               style = "font-size: 115%;"),
                             
                             br(),
                             p(strong("Roster Stats"),
                               style = "font-size: 180%;"),
                             br(),
                             
                             p(strong("Key Roster Characterisitics"), "is meant to show capabilities of a team's roster 
                               that have been proven to be vital to an NCAA tournament run. Letter grades are assigned to each player and the landscape of the roster can be analyzed",
                               style = "font-size: 135%;"),
                             # br(),
                             # p(strong("— Floor Spacing"), "is meant to show the gravity a player imposes on the defense with their 3 point shooitng.
                             #   Letter grades are calculated using a mix of 3P attempts per minute and 3P%. 
                             #   There is a high favor for players that are high frequency three point shooters, as these players apply constant pressure on the defense to stay attached.
                             #   Teams that have few players with good floor spacing grades may be easier to guard with defenses able to help more and pack the paint. ",
                             #   style = "font-size: 115%;"),
                             # 
                             # p(strong("— Ball Control"), "is meant to show the stability a player brings to the offense through their ability control the ball and make plays. Typically,
                             #   this grade identifies floor generals that run offenses and value the ball. It can also recognize great playmaking bigs that facilitate open opportunities,
                             #   again while valuing the ball. Teams that have few good ball control grades can be susceptible to inconsistency and inadaptablitly as they often need 1 or 2 players
                             #   to always be on the court and playing well to stabalize the offense.",
                             #   style = "font-size: 115%;"),
                             # 
                             # p(strong("— Interior Presence"), "is meant to show how well a player can hold their own in the paint as a defender and rebounder. Simply put, this is a measure 
                             #   of how much of a \"big\" a player is. Teams with few good interior presence grades are susceptible to be out-rebounded, exploited at the rim on defense,
                             #   and can have common foul trouble issues.",
                             #   style = "font-size: 115%;"),
                             
                             ),
                    
                    # tabPanel(strong("Visuals"), 
                    #   
                    #   p("",style = "font-size: 135%;")),
                    # 
                    
                    ###### 2.9.3) Upcoming Games Explaination ######
                    #tabPanel(strong("Upcoming Games"),

                    #  p("Upcoming games are sorted by how good the games should be based on ETW. The factors considered
                    #    are how good both teams are with an advantage for home teams, and how closely matched the teams are.",style = "font-size: 135%;")),
                    
                    tabPanel(strong("Dash Bracketology"), 
                             h2(strong("Dash Bracketology")),
                             br(),
                             p(strong("Dash Bracketology"),"uses two predicitve models to determine the field of 68 and their seeds. The first is a resume based model
                       to determine the at-large bids. Then a model trained on team quality metrics determines the likely autobids, as well as the seeding of the resulting 68 teams.
                       This is meant to mimic the committee's process, where they have said resume metrics get you in the tournament
                       and performance based metrics determine seed.",
                               style = "font-size: 135%;"),
                             
                             # br(),
                             # p("",
                             #   style = "font-size: 135%;")
                             ),
                     
                    
                    ###### 2.9.4) Season Awards Explaination ######
                    tabPanel(strong("Season Awards"),
                             h2(strong("Season Award Predictions")),
                             br(),
                             p(strong("Season Award Predictions"),"are based on several predictive models that incorporate player stats
                                       and team success. There are seven models, one for All-American predictions and six for All-Conference
                                       predictions among Power-5 conferences. They are trained on data going back to 2006.",
                               style = "font-size: 135%;"),
                             br(),
                             p("The models are meant to understand the tendencies of the voters and make selections 
                                on the current season that ideally would mimic the voters' choices.
                                It is best described as a prediction of what will happen, while BASIC ratings
                                can be considered what CBB Dash thinks should happen.",
                               style = "font-size: 135%;")),
                    
                    ),
              
              #h2(strong("Bio")),
              br(),
              #box(width=12, 
              
              #),
              )
      
    )
  )
)
###### _______________________ #######

##################################### 3) SERVER ELEMENTS  #################################################


server <- function(input, output) {
  

  #################### 3.1) Sidebar Setup #####################

  
  output$menu <- renderMenu({
    sidebarMenu(
      
      # home
      menuItem(strong("Home",style = 'font-size: 108%; font-family: "Trebuchet MS";'), 
               tabName='home', 
               icon = icon("home")),
      
      # stats & ratings
      menuItem(strong("All Stats",style = 'font-size: 108%; font-family: "Trebuchet MS";'),  
               tabName = "ratings", 
               icon = icon("ranking-star"),
               menuSubItem(text = "Teams", 
                           tabName = "ratings-teams", 
                           icon=icon('people-group')),
               menuSubItem(text = "Players", 
                           tabName = "ratings-players", 
                           icon=icon('person')),
               menuSubItem(text = "Conferences", 
                           tabName = "ratings-conf", 
                           icon=icon('building'))),

      # team identity
      menuItem(strong("Team Identity",style = 'font-size: 108%; font-family: "Trebuchet MS";'), 
               tabName = "team-identities", 
               icon=icon('magnifying-glass-chart')),
      
      
      # season awards
      menuItem(strong("All-League Predictive Models",style = 'font-size: 108%; font-family: "Trebuchet MS";'), 
               tabName="seasonAwards", 
               icon = icon("medal")),
      
      # bracketolgy
      menuItem(strong("Dash Bracketology Model",style = 'font-size: 108%; font-family: "Trebuchet MS";'), 
               tabName = "bracketology",
               icon = icon("trophy")),
      
      # visuals
      menuItem(strong("Visuals",style = 'font-size: 108%; font-family: "Trebuchet MS";'), 
               tabName='Team-Visuals',
               icon = icon("chart-line"),
                 
               menuSubItem(text = "Pre-Built Team Visuals", 
                           tabName = "teams-vis", 
                           icon=icon('chart-line')),
               menuSubItem(text = "Pre-Built Player Visuals",
                           tabName = "player-vis", 
                           icon=icon('chart-line')),
               menuSubItem(text = "Custom Team Visuals", 
                           tabName = "custom-team-vis", 
                           icon=icon('chart-line')),
               menuSubItem(text = "Custom Player Visuals",
                           tabName = "custom-player-vis",
                           icon=icon('chart-line'))
               
               
               ),
      
      # stats & ratings
      menuItem(strong("Experimental Metrics",style = 'font-size: 108%; font-family: "Trebuchet MS";'),  
               tabName='original-metrics',
               icon = icon("python"),
               menuSubItem(text = HTML("<i>Estimated Tournament Wins (ETW)</i>"), 
                           tabName = "etw-page", 
                           icon=icon('python')),
               menuSubItem(text = HTML("<i>Box Aggregate Score using Impact,<br>Counting & efficiency stats (BASIC)</i>"), 
                           tabName = "pcs-page", 
                           icon=icon('python'))),
      
      
      # # visuals
      # menuItem("Player Visuals", 
      #          tabName='Player-Visuals',
      #          icon = icon("chart-line"),
      #          menuSubItem(text = "Custom Player Visuals",
      #                      tabName = "custom-player-vis",
      #                      icon=icon('chart-line')),
      #          
      #          menuSubItem(text = "Pre-Built Player Visuals",
      #                      tabName = "player-vis", 
      #                      icon=icon('chart-line'))
      # ),
      # upcoming games
      menuItem(strong("Upcoming Games",style = 'font-size: 108%; font-family: "Trebuchet MS";'), 
               icon = icon("calendar"),
               menuSubItem(text = "Today",
                           tabName = "today", 
                           icon=icon('calendar')),
               menuSubItem(text = "This Week", 
                           tabName = "thisWeek",
                           icon=icon('calendar'))),
      
      
      
      
      
      # about
      menuItem(strong("About",style = 'font-size: 108%; font-family: "Trebuchet MS";'),
               tabName = "about", 
               icon = icon("circle-info"))
    )
  })
  

  #################### 3.2) Home Page Tab #####################

  
  ###### 3.2.1) Logo Images #######
  output$`homePage-logo` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")
  })
  
  output$`homePage-logo2` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo3` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo4` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo5` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo6` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo7` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo8` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo9` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo10` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo11` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo12` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo13` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo14` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo15` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logo16` <- renderImage({
    list(src = 'logo-circle-modified.png',
         width = "75", height = "75")  })
  
  output$`homePage-logoTest` <- renderImage({
    this = Teams %>% filter(School == input$`teamIdentity-teamSearchBox`)
    list(src = this$Logo,
         width = "200")
  })
  
  
  ###### Top 25 players in BASIC - only show player, BASIC, and team
  output$homePageTop25Player <- DT::renderDataTable({
    
    # grab top 25 players, only Player, BASIC and School columns
    this=Players[1:32,1:3]
    this=this %>% dplyr::select(Player,`BASIC`,School)
    
    ### DT
    DT::datatable(this,
                  
                  # DT settings - remove search bar, set scroll, etc.
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                 scrollX = '400px', autoWidth = T,dom = 't',ordering=F,pageLength = 32,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('body').css({'font-family': 'Trebuchet MS'});",
                                   "}")))%>%
      
      ### styling
      
      # color  - green yellow red scale
      formatStyle(c('BASIC'),backgroundColor = plcl.pcs) %>% 
      
      # add thick black borders
      formatStyle(c(2,3),`border-left` = '4px solid black')%>% 
      
      # bold BASIC column
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('BASIC', 'Player', 'School')) %>% 
      formatRound(columns=c("BASIC"), 1) 
  })
  
  ###### Top 10 Players DT with full stats
  output$homePageTop10Players <- DT::renderDataTable({
    
    # DT
    DT::datatable(Players[1:10,],
                  
                  # extension to keep player name column from scoll
                  extensions = "FixedColumns",
                  
                  # settings - column widths, scroll, remove search, etc.
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),  
                                                   list(width = '125px', targets = c(2)),
                                                   list(width = '100px', targets = c(3))),
                                 scrollX = '400px', autoWidth = T,dom = 't',ordering=F,
                                 
                                 # keep player name column from scoll
                                 fixedColumns = list(leftColumns = 4), 
                                 
                                 # font
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('body').css({'font-family': 'Trebuchet MS'});",
                                   "}")))%>%
      
      ### styling
      
      # color BASIC - green yellow red scale
      formatStyle(c('BASIC'),backgroundColor = styleInterval(seq(-7,10,0.1), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b', '#c5fc8b', '#a3fb45','#6fd404'))(length(seq(-7,10,0.1))+1))) %>% 
      
      # add black borders to group similar columns
      formatStyle(c(1,3,6,10, 14, 20, 24, 28, 32, 36),`border-left` = '4px solid black')%>% 
      formatStyle(c(3),`border-right` = '4px solid black')%>% 
      
      # change column with of player and team
      formatStyle(columns = c(1,2), width='500px') %>% 
      
      # bold BASIC columns
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('BASIC'))
    
    ###### Top 12 teams in tournament outcome prediction score
    output$`home-top12TeamsScatter` <- renderPlotly({
      
      # get data
      teamLogo=Teams %>% arrange(desc(ETW))
      this=teamLogo %>% unique()
      this=this[1:12,]
      rownames(this)=c(1:length(this$School))
      
      ### scatter
      p <- ggplot(this, 
                  aes_string(x=(12:1), 
                             y=this$ETW,
                             label=this$School)) + 
        geom_point(aes(text=paste(School,"\n",Conference,sep=""))) +
        
        ### formatting
        theme_gdocs() +
        ggtitle("Top 12 Schools in ETW")+
        xlab("Rank")+
        ylab("Estimated Tournamnet Wins")+
        theme_minimal()+
        theme(text = element_text(family = "Trebuchet MS"),
              plot.title = element_text(size=18, face='bold'),
              axis.title.y = element_text(size=12, hjust=0, face="italic", color="black"),
              axis.title.x = element_text(size=12, hjust=0, face="italic", color="black"))+
        geom_text(angle=90, vjust = 0, nudge_y = 0.14)
      
      ggplotly(p)
    })
    
    
  })
  
  ###### home page top 12 teams DT
  output$homePageTopTeamsDT <- DT::renderDataTable({
    
    # grab 3 columns
    this=Teams %>% dplyr::select(School, ETW, Conference)
    
    ### DT
    DT::datatable(this,
                  
                  # settings to set page length to 12, remove search bar, etc.
                  options = list(
                    columnDefs = list(list(className = 'dt-center', targets = "_all")), dom = 't',ordering=F, pageLength=12)) %>% 
      
      # add color to TOPS column - green yellow red scale
      formatStyle(c('ETW'), backgroundColor = styleInterval(seq(-4,10,0.1), colorRampPalette(c('#e1685f','#e7847d','#F0BB32',
                                                                                                 '#fee08b','#ffffbf','#d9ef8b',
                                                                                                 '#c5fc8b','#c5fc8b','#a3fb45','#6fd404'))(length(seq(-4,10,0.1))+1)))%>% 
      # bold TOPS column
      formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('ETW'))
  })
  
  
  
  ###### Home Page Top 5 Players
  
  
  ###### Top 5 Scorers
  
  output$`homePage-top5scorers` <- DT::renderDataTable({
    
    fullD1 = input$`homePage-statsD1`
    
    if (!fullD1){
      this=Players %>% 
        arrange(desc(PPG)) %>% 
        dplyr::select(Player, School, GP, PPG, `TS%`, `eFG%`, `FG%`)
    } else{
      this=Players %>% 
        filter(Conference %in% c("Big Ten", "Big 12", "Big East", "SEC", 'ACC')) %>% 
        arrange(desc(PPG)) %>% 
        dplyr::select(Player, School, GP, PPG, `TS%`, `eFG%`, `FG%`)
    }
    ### get power 6 players only & grab relevant stats
    
    
    this$`eFG%` = round(this$`eFG%`, 1)
    this$`FG%` = round(this$`FG%`, 1)
    this$`TS%` = round(this$`TS%`, 1)
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 5,lengthChange = FALSE, dom = 't',ordering=F)) %>% 
      
      # DT formatting
      formatStyle(c(4,5),`border-left` = '4px solid black') %>% 
      formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>% 
      formatStyle(c('TS%'),backgroundColor = plcl.ts) %>% 
      formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>% 
      formatStyle(c('FG%'),backgroundColor = plcl.fg)%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1)  %>% 
      formatRound(c("GP"), digits = 0)  %>% 
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = colnames(this))
  })  
  
  
  ###### Top 5 Passers
  
  output$`homePage-top5passers` <- DT::renderDataTable({
    
    fullD1 = input$`homePage-statsD1`
    
    if (!fullD1){
      this=Players %>% 
        #filter(Conference %in% c("Big Ten", "Big 12", 'Pac-12', "Big East", "SEC", 'ACC')) %>% 
        arrange(desc(APG)) %>% 
        mutate(`AST/G` = APG,
               `TOV/G` = TOV) %>% 
        dplyr::select(Player, School, GP, `AST/G`,`AST:TOV`,`AST%`, `TOV/G`, `TOV%`)
    } else{
      this=Players %>% 
        filter(Conference %in% c("Big Ten", "Big 12", "Big East", "SEC", 'ACC')) %>% 
        arrange(desc(APG)) %>% 
        mutate(`AST/G` = APG,
               `TOV/G` = TOV) %>% 
        dplyr::select(Player, School, GP, `AST/G`,`AST:TOV`,`AST%`, `TOV/G`, `TOV%`)
    }
    
    ### get power 6 players only & grab relevant stats
    
    
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 5,lengthChange = FALSE, dom = 't',ordering=F)) %>%
      
      # DT formatting
      formatStyle(c(4,5),`border-left` = '4px solid black') %>% 
      formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>% 
      formatStyle(c('AST/G'),backgroundColor = plcl.apg) %>% 
      formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>% 
      formatStyle(c('TOV/G'),backgroundColor = plcl.tov) %>% 
      formatStyle(c('TOV%'),backgroundColor = plcl.tovpct)%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1) %>% 
      formatRound(c("GP"), digits = 0)  %>% 
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = colnames(this))
  })    
  
  
  output$`homePage-top5ratio` <- DT::renderDataTable({
    
    fullD1 = input$`homePage-statsD1`
    
    if (!fullD1){
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        filter(#Conference %in% c("Big Ten", "Big 12", 'Pac-12', "Big East", "SEC", 'ACC'),
               APG>2.5, TOV>0) %>% 
        #mutate(`AST:TOV` = tAst/tTov) %>% 
        arrange(desc(`AST:TOV`)) %>% 
        mutate(`AST/G` = APG,
               `TOV/G` = TOV) %>% 
        dplyr::select(Player, School, GP, `AST:TOV`, `AST/G`, `AST%`, `TOV/G`, `TOV%`)
    } else{
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        filter(Conference %in% c("Big Ten", "Big 12", "Big East", "SEC", 'ACC'),
               APG>2.5, TOV>0) %>% 
        #mutate(`AST:TOV` = APG/TOV) %>% 
        arrange(desc(`AST:TOV`)) %>% 
        mutate(`AST/G` = APG,
               `TOV/G` = TOV) %>% 
        dplyr::select(Player, School, GP, `AST:TOV`, `AST/G`, `AST%`, `TOV/G`, `TOV%`)
    }
    
    
    
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 5,lengthChange = FALSE, dom = 't',ordering=F)) %>%
      
      # DT formatting
      formatStyle(c(4,5),`border-left` = '4px solid black') %>% 
      formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>% 
      formatStyle(c('AST/G'),backgroundColor = plcl.apg) %>% 
      formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>% 
      formatStyle(c('TOV/G'),backgroundColor = plcl.tov) %>% 
      formatStyle(c('TOV%'),backgroundColor = plcl.tovpct)%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1) %>% 
      formatRound(c("GP"), digits = 0)  %>% 
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = colnames(this))
  })    
  
  ###### Top 5 Rebounders
  
  output$`homePage-top5rebounders` <- DT::renderDataTable({
    
    fullD1 = input$`homePage-statsD1`
    
    if (!fullD1){
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        #filter(Conference %in% c("Big Ten", "Big 12", 'Pac-12', "Big East", "SEC", 'ACC')) %>% 
        mutate(`REB/G` = ORB+DRB,
               `ORB/G` = ORB,
               `DRB/G` = DRB) %>% 
        arrange(desc(TRB)) %>% 
        dplyr::select(Player,School,GP,`REB/G`,`ORB/G`,`ORB%`,`DRB/G`,`DRB%`)
    } else{
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        filter(Conference %in% c("Big Ten", "Big 12", "Big East", "SEC", 'ACC')) %>% 
        mutate(`REB/G` = ORB+DRB,
               `ORB/G` = ORB,
               `DRB/G` = DRB) %>% 
        arrange(desc(TRB)) %>% 
        dplyr::select(Player,School,GP,`REB/G`,`ORB/G`,`ORB%`,`DRB/G`,`DRB%`)
    }
    
    
    
    
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 5,lengthChange = FALSE, dom = 't',ordering=F)) %>% 
      
      # DT formatting
      formatStyle(c(4,5),`border-left` = '4px solid black') %>% 
      formatStyle(c('REB/G'),backgroundColor = plcl.trb) %>% 
      formatStyle(c('ORB/G'),backgroundColor = plcl.orb) %>% 
      formatStyle(c('ORB%'),backgroundColor = plcl.orbpct) %>% 
      formatStyle(c('DRB/G'),backgroundColor = plcl.drb) %>% 
      formatStyle(c('DRB%'),backgroundColor = plcl.drbpct)%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1)  %>% 
      formatRound(c("GP"), digits = 0)  %>% 
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = colnames(this))
  })   
  
  ###### Top 5 Steals
  
  output$`homePage-top5steals` <- DT::renderDataTable({
    
    fullD1 = input$`homePage-statsD1`
    
    if (!fullD1){
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        #filter(Conference %in% c("Big Ten", "Big 12", 'Pac-12', "Big East", "SEC", 'ACC')) %>% 
        arrange(desc(SPG)) %>% 
        mutate(`STL/G` = SPG) %>% 
        dplyr::select(Player, School, GP, `STL/G`, `STL%`, DRtg)
    } else{
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        filter(Conference %in% c("Big Ten", "Big 12", "Big East", "SEC", 'ACC')) %>% 
        arrange(desc(SPG)) %>% 
        mutate(`STL/G` = SPG) %>% 
        dplyr::select(Player, School, GP, `STL/G`, `STL%`, DRtg)
    }
    
    
    
    #this$`eFG%` = round(this$`eFG%`, 1)
    #this$`FG%` = round(this$`FG%`, 1)
    #this$`TS%` = round(this$`TS%`, 1)
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 5,lengthChange = FALSE, dom = 't',ordering=F)) %>% 
      
      # DT formatting
      formatStyle(c(4,5),`border-left` = '4px solid black') %>% 
      formatStyle(c('STL/G'),backgroundColor = plcl.spg) %>% 
      formatStyle(c('STL%'),backgroundColor = plcl.stlpct) %>% 
      formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>% 
      #formatStyle(c('FG%'),backgroundColor = plcl.fg)%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1)  %>% 
      formatRound(c("GP"), digits = 0)  %>% 
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = colnames(this))
  })  
  
  ###### Top 5 Blocks
  
  output$`homePage-top5blocks` <- DT::renderDataTable({
    
    fullD1 = input$`homePage-statsD1`
    
    if (!fullD1){
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        #filter(Conference %in% c("Big Ten", "Big 12", 'Pac-12', "Big East", "SEC", 'ACC')) %>% 
        arrange(desc(BPG)) %>% 
        mutate(`BLK/G` = BPG) %>% 
        dplyr::select(Player, School, GP, `BLK/G`, `BLK%`, DRtg)
    } else{
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        filter(Conference %in% c("Big Ten", "Big 12", "Big East", "SEC", 'ACC')) %>% 
        arrange(desc(BPG)) %>% 
        mutate(`BLK/G` = BPG) %>% 
        dplyr::select(Player, School, GP, `BLK/G`, `BLK%`, DRtg)
    }
    
    
    
    #this$`eFG%` = round(this$`eFG%`, 1)
    #this$`FG%` = round(this$`FG%`, 1)
    #this$`TS%` = round(this$`TS%`, 1)
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 5,lengthChange = FALSE, dom = 't',ordering=F)) %>% 
      
      # DT formatting
      formatStyle(c(4,5),`border-left` = '4px solid black') %>% 
      formatStyle(c('BLK/G'),backgroundColor = plcl.bpg) %>% 
      formatStyle(c('BLK%'),backgroundColor = plcl.blkpct) %>% 
      formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>% 
      #formatStyle(c('FG%'),backgroundColor = plcl.fg)%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1)  %>% 
      formatRound(c("GP"), digits = 0)  %>% 
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = colnames(this))
  })  
  
  ###### Top 5 Turnovers
  
  output$`homePage-top5tov` <- DT::renderDataTable({
    
    fullD1 = input$`homePage-statsD1`
    
    if (!fullD1){
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        #filter(Conference %in% c("Big Ten", "Big 12", 'Pac-12', "Big East", "SEC", 'ACC')) %>% 
        arrange(desc(TOV)) %>% 
        mutate(`TOV/G` = TOV,
               `AST/G` = APG) %>% 
        dplyr::select(Player, School, GP, `TOV/G`, `AST:TOV`, `TOV%`, `AST/G`, `USG%`)
    } else{
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        filter(Conference %in% c("Big Ten", "Big 12", "Big East", "SEC", 'ACC')) %>% 
        arrange(desc(TOV)) %>% 
        mutate(`TOV/G` = TOV,
               `AST/G` = APG) %>% 
        dplyr::select(Player, School, GP, `TOV/G`, `AST:TOV`, `TOV%`, `AST/G`, `USG%`)
    }
    
    
    
    #this$`eFG%` = round(this$`eFG%`, 1)
    #this$`FG%` = round(this$`FG%`, 1)
    #this$`TS%` = round(this$`TS%`, 1)
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 5,lengthChange = FALSE, dom = 't',ordering=F)) %>% 
      
      # DT formatting
      formatStyle(c(4,5),`border-left` = '4px solid black') %>% 
      formatStyle(c('TOV/G'),backgroundColor = plcl.tov) %>% 
      formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>% 
      formatStyle(c('TOV%'),backgroundColor = plcl.tovpct) %>% 
      formatStyle(c('USG%'),backgroundColor = plcl.usg) %>% 
      formatStyle(c('AST/G'),backgroundColor = plcl.apg) %>% 
      #formatStyle(c('FG%'),backgroundColor = plcl.fg)%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1)  %>% 
      formatRound(c("GP"), digits = 0)  %>% 
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = colnames(this))
  })  
  
  
  ###### Top 5 ORtg
  
  output$`homePage-top5ortg` <- DT::renderDataTable({
    
    fullD1 = input$`homePage-statsD1`
    
    if (!fullD1){
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        filter(#Conference %in% c("Big Ten", "Big 12", 'Pac-12', "Big East", "SEC", 'ACC'),
               MPG>=18, `USG%`>=18) %>% 
        arrange(desc(ORtg)) %>% 
        mutate(`Offensive Rating` = ORtg) %>% 
        dplyr::select(Player, School, GP, `MPG`, `USG%`, `Offensive Rating`, `Net Rating`)
    } else{
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        filter(Conference %in% c("Big Ten", "Big 12", "Big East", "SEC", 'ACC'),
               MPG>=18, `USG%`>=18) %>% 
        arrange(desc(ORtg)) %>% 
        mutate(`Offensive Rating` = ORtg) %>% 
        dplyr::select(Player, School, GP, `MPG`, `USG%`, `Offensive Rating`, `Net Rating`)
    }
    
    
    
    #this$`eFG%` = round(this$`eFG%`, 1)
    #this$`FG%` = round(this$`FG%`, 1)
    #this$`TS%` = round(this$`TS%`, 1)
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 5,lengthChange = FALSE, dom = 't',ordering=F)) %>% 
      
      # DT formatting
      formatStyle(c(4,6,7),`border-left` = '4px solid black') %>% 
      formatStyle(c('Offensive Rating'),backgroundColor = plcl.ortg) %>% 
      formatStyle(c('USG%'),backgroundColor = plcl.usg) %>% 
      formatStyle(c('Net Rating'),backgroundColor = plcl.netrating) %>% 
      #formatStyle(c('FG%'),backgroundColor = plcl.fg)%>% 
      formatStyle(c('MPG'),backgroundColor = plcl.mpg)%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1)  %>% 
      formatRound(c("GP"), digits = 0)  %>% 
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = colnames(this))
  })  
  
  
  ###### Top 5 DRtg
  
  output$`homePage-top5drtg` <- DT::renderDataTable({
    
    fullD1 = input$`homePage-statsD1`
    
    if (!fullD1){
      ### get power 6 players only & grab relevant stats
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        filter(#Conference %in% c("Big Ten", "Big 12", 'Pac-12', "Big East", "SEC", 'ACC'),
               MPG>=18, `USG%`>=12) %>% 
        arrange(DRtg) %>% 
        mutate(`Defensive Rating` = DRtg) %>% 
        dplyr::select(Player, School, GP, `MPG`, `USG%`, `Defensive Rating`, `Net Rating`)
    } else{
      ### get power 6 players only & grab relevant stats
      this=Players %>% 
        filter(Conference %in% c("Big Ten", "Big 12", "Big East", "SEC", 'ACC'),
               MPG>=18, `USG%`>=12) %>% 
        arrange(DRtg) %>% 
        mutate(`Defensive Rating` = DRtg) %>% 
        dplyr::select(Player, School, GP, `MPG`, `USG%`, `Defensive Rating`, `Net Rating`)
    }
    
    
    
    #this$`eFG%` = round(this$`eFG%`, 1)
    #this$`FG%` = round(this$`FG%`, 1)
    #this$`TS%` = round(this$`TS%`, 1)
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 5,lengthChange = FALSE, dom = 't',ordering=F)) %>% 
      
      # DT formatting
      formatStyle(c(4,6,7),`border-left` = '4px solid black') %>% 
      formatStyle(c('Defensive Rating'),backgroundColor = plcl.drtg) %>% 
      formatStyle(c('Net Rating'),backgroundColor = plcl.netrating) %>% 
      formatStyle(c('USG%'),backgroundColor = plcl.usg) %>% 
      formatStyle(c('MPG'),backgroundColor = plcl.mpg)%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1)  %>% 
      formatRound(c("GP"), digits = 0)  %>% 
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = colnames(this))
  })  
  
  
  
  ###### Small Scatter of all teams TOPS
  
  output$`homePage-smallTeamScatter` <- renderPlotly({
    
    # prepare data
    this = Teams
    this$Rank = rep(0, length(Teams$School))
    this$ETW[is.na(this$ETW)] = -10
    this=this %>% 
      mutate(labelStr = paste(School, as.factor(ETW), '\n', Conference))
    
    ### DT
    p <- ggplot(this, 
                aes_string(x=this$Rank, 
                           y=this$ETW,
                           label=this$School,
                           ETW=this$ETW)) + 
      
      ### format vector to not overlap points - beeswarm
      geom_beeswarm(dodge.width=0.2, cex=2)+
      
      ### DT formatting
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    style(p, text = row.names(this))
    ggplotly(p, tooltip = c("label", "ETW")) %>% config(displayModeBar = F)
  })
  
  
  ###### Top 10 Kenpom Offenses
  
  output$`homePage-top10TeamOff` <- DT::renderDataTable({
    
    # prepare data
    TeamStats = TeamStats %>% arrange(desc(AdjOE))
    this = TeamStats[1:10,]
    this=this %>% dplyr::select(Team, AdjOE)
    
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 10,lengthChange = FALSE, dom = 't',ordering=F)) %>% 
      
      # DT options
      formatStyle(c(2),`border-left` = '4px solid black') %>% 
      formatStyle(c(2),`border-right` = '4px solid black')
  })
  
  output$`homePage-top10TeamDef` <- DT::renderDataTable({
    
    # prepare data
    TeamStats = TeamStats %>% arrange(AdjDE)
    this = TeamStats[1:10,]
    this=this %>% dplyr::select(Team, AdjDE)
    
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 10,lengthChange = FALSE, dom = 't',ordering=F)) %>% 
      
      # DT formatting
      formatStyle(c(2),`border-left` = '4px solid black') %>% 
      formatStyle(c(2),`border-right` = '4px solid black')
  })
  
  
  ### AdjOE vs AdjDE
  output$`homePage-AdjDEvsAdjOE` <- renderPlot({
    
    # get data
    this = Teams %>% arrange(desc(ETW))
    this = this[1:25,]
    this$Rank = paste0("#", seq(1,25,1))
    this50 = Teams %>% arrange(desc(ETW))
    this50 = this50[1:50,]
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`ETW Offense`, 
                           y=this$`ETW Defense`)) + 
      geom_image(size = 0.14,aes(image=Logo)) +
      geom_vline(xintercept=mean(this50$`ETW Offense`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(this50$`ETW Defense`), linetype="dashed", color = "purple", size=1.1) +
      geom_text(aes(label=paste("", this$Rank, "", sep = "")),
                size = 4.75,vjust = 1,nudge_y = -0.25,
                fontface = "bold",family = "Trebuchet MS")+
      
      # formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab("ETW Offense Rating")+
      ylab("ETW Defense Rating")+
      theme_minimal()+
      theme(text = element_text(size=18, family="Trebuchet MS"),
            plot.title=element_blank()) + 
      
      annotate(
        "text", label = paste("Good\nOffense"),
        x = max(this$`ETW Offense`)+0.2,
        y = mean(this50$`ETW Defense`)-0.3,
        size = 5, colour = "darkgreen",
        fontface = "bold",family = "Trebuchet MS") + 
      
      annotate(
        "text", label = paste("Good\nDefense"),
        x = mean(this50$`ETW Offense`)-0.25, y = max(this$`ETW Defense`)+0.2, size = 5, colour = "darkgreen",
        fontface = "bold",family = "Trebuchet MS")+
      
      scale_x_continuous(breaks = seq(0, 6, 1))+
      scale_y_continuous(breaks = seq(0, 6, 1))+
      
      theme(
        axis.text.x = element_text(size=14, face="bold", colour = "black"), 
        axis.text.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=18, face="bold", colour = "black"),    
        axis.title.y = element_text(size=18, face="bold", colour = "black")
      )
     
    
    p
    
  })
  
  ### AdjOE vs AdjDE
  output$`homePage-KP-AdjDEvsAdjOE` <- renderPlot({
    
    # get data
    this = Teams %>% arrange(desc(Kenpom.AdjEM))
    this = this[1:25,]
    this$Rank = paste0("#", seq(1,25,1))
    this50 = Teams %>% arrange(desc(ETW))
    this50 = this50[1:50,]
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`AdjOE`, 
                           y=this$`AdjDE`)) + 
      geom_image(size = 0.14,aes(image=Logo)) +
      geom_vline(xintercept=mean(this50$`AdjOE`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(this50$`AdjDE`), linetype="dashed", color = "purple", size=1.1) +
      geom_text(aes(label=paste("", this$Rank, "", sep = "")),
                size = 4.75,vjust = 1,nudge_y = -1.25, 
                fontface = "bold",family = "Trebuchet MS")+
      scale_y_reverse()+
      
      # formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab("KenPom Adj. Offensive Efficiency")+
      ylab("KenPom Adj. Defensive Efficiency")+theme_minimal()+
      theme(text = element_text(size=18, family="Trebuchet MS"),
            plot.title=element_blank()) + 
      annotate(
        "text", label = paste("Good\nOffense"),
        x = max(this$`AdjOE`)+1.5, y = mean(this50$`AdjDE`)-2, size = 5, colour = "darkgreen",
        fontface = "bold",family = "Trebuchet MS") + 
      annotate(
        "text", label = paste("Good Defense"),
        x = mean(this50$`AdjOE`)+1.5, y = min(this$`AdjDE`)-3, size = 5, colour = "darkgreen",
        fontface = "bold",family = "Trebuchet MS")+
      scale_x_continuous(breaks = seq(100, 125, 5))+
      scale_y_continuous(breaks = seq(110, 85, -5))+
      scale_y_reverse()
    
    p
    
  })
  
  ### AdjOE vs AdjDE
  output$`homePage-AP-ORtgvsDRtg` <- renderPlot({
    
    # get data
    this = Teams# %>% arrange(desc(KenPom AdjEM))
    include = c()
    for (tm in this$School){
      if (tm %in% ap$TEAM){
        include = c(include, TRUE)
      } else{
        include = c(include, FALSE)
      }
    }
    
    this = this[include,]
    ap$RANK = ap$RANKING
    this = merge(this, ap %>% dplyr::select(TEAM, RANK), by.x = "School", by.y = "TEAM")
    this$Rank = paste0("#", this$RANK)
    this = this %>% arrange(RANK)
    this50 = Teams %>% arrange(desc(ETW))
    this50 = this50[1:50,]
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`ORtg`, 
                           y=this$`DRtg`)) + 
      geom_image(size = 0.14,aes(image=Logo)) +
      geom_vline(xintercept=mean(this50$`ORtg`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(this50$`DRtg`), linetype="dashed", color = "purple", size=1.1) +
      geom_text(aes(label=paste("", this$Rank, "", sep = "")),
                size = 4.75,vjust = 1,nudge_y = -1.5, 
                fontface = "bold",family = "Trebuchet MS")+
      scale_y_reverse()+
      
      # formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab("Offensive Rating")+
      ylab("Defensive Rating")+theme_minimal()+
      theme(text = element_text(size=18, family="Trebuchet MS"),
            plot.title=element_blank()) + 
      annotate(
        "text", label = paste("Good\nOffense"),
        x = max(this$`ORtg`)+2, y = mean(this50$`DRtg`)-2, size = 5, colour = "darkgreen",
        fontface = "bold",family = "Trebuchet MS") + 
      annotate(
        "text", label = paste("Good Defense"),
        x = mean(this50$`ORtg`)+2, y = min(this50$`DRtg`)-3, size = 5, colour = "darkgreen",
        fontface = "bold",family = "Trebuchet MS")+
      scale_x_continuous(breaks = seq(100, 125, 5))+
      scale_y_continuous(breaks = seq(105, 80, -5))+
      scale_y_reverse()
    
    p
    
  })
  
  
  
  ################## 3.3) Stats & Ratings Tab #################

  output$`player-conf-text` = reactive({input$`Conference-player`})
  
  ###### Players DT with full stats
  output$fullPlayerDT <- DT::renderDataTable({
    
    ### filters
    
    # conference
    if (input$`Conference-player` != "All") {Players <- Players[Players$Conference == input$`Conference-player`,]}
    
    # team
    if (input$`School-player` != "All") {Players <- Players[Players$School == input$`School-player`,]}
    
    # age
    if (input$Class != "All") {Players <- Players[Players$Class == input$Class,]}
    
    this = Players %>% dplyr::select(-`2PM`, -`3PM`, -PPR, -TRB, -`AST:TOV2`)
    
    this$`TS%` = round(this$`TS%`, 1)
    this$`eFG%` = round(this$`eFG%`, 1)
    this$`FG%` = round(this$`FG%`, 1)
    this$`3P%` = round(this$`3P%`, 1)
    this$`2P%` = round(this$`2P%`, 1)
    this$`FT%` = round(this$`FT%`, 1)
    this$`3PA` = round(this$`3PA`, 1)
    this$`2PA` = round(this$`2PA`, 1)
    this$`FTA` = round(this$`FTA`, 1)
    this$`AST:TOV` = round(this$`AST:TOV`, 2)
    
    this = this %>% relocate(BASIC, .after=Player)
    this = this %>% relocate(STOCKS, .after=`DRB%`)
    this = this %>% relocate(REB, .after=`TOV%`)
    this = this %>% relocate(`AST:TOV`, .after=`FT%`)
    
    
    this$Conference = gsub('Missouri Valley', 'MVC', this$Conference)
    this$Conference = gsub('Mountain West', 'MWC', this$Conference)
    this$Conference = gsub('Patriot League', 'Patriot', this$Conference)
    this$Conference = gsub('Summit League', 'Summit', this$Conference)
    this$Conference = gsub('Horizon League', 'Horizon', this$Conference)
    this$Conference = gsub('CAA', 'Colonial', this$Conference)
    
    #if (input$`fullStats-finalFourCheckbox-player`) {this <- this[this$School %in% c("UConn", 'Purdue', 'NC State', 'Alabama'),]}
    
    ### DT
    DT::datatable(this,
                  
                  # remove index column
                  rownames = FALSE,
                  
                  # extension to keep team name locked from scroll
                  extensions = "FixedColumns",
                  
                  # adjust columns width, settings for scroll, etc 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"), 
                                                   list(width = '150px', targets = c(3)),
                                                   list(width = '100px', targets = c(1))
                                                   ),
                                 scrollX = '400px', autoWidth = T, pageLength=15,
                                 
                                 # keep team name locked from scroll
                                 fixedColumns = list(leftColumns = 3), 
                                 
                                 # font
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('body').css({'font-family': 'Trebuchet MS'});",
                                   "}")))%>%
      ### styling
      
      # color BASIC column by rating - green yellow red scale
      formatStyle(c('BASIC'),backgroundColor = plcl.pcs) %>% #styleInterval(seq(-7,10,0.1), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b', '#c5fc8b', '#a3fb45','#6fd404'))(length(seq(-7,10,0.1))+1))) %>% 
      formatStyle(c('USG%'),backgroundColor = plcl.usg) %>%
      formatStyle(c('SOS'),backgroundColor = plcl.sos) %>%
      
      formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
      formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
      formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
      formatStyle(c('FG%'),backgroundColor = plcl.fg) %>%
      
      formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
      formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
      formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
      formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
      formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
      formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
      
      formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
      formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
      formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>%
      formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
      formatStyle(c('TOV%'),backgroundColor = plcl.tovpct) %>%
      
      formatStyle(c('REB'),backgroundColor = plcl.reb) %>%
      formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
      formatStyle(c('ORB%'),backgroundColor = plcl.orbpct) %>%
      formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
      formatStyle(c('DRB%'),backgroundColor = plcl.drbpct) %>%
      
      formatStyle(c('STOCKS'),backgroundColor = plcl.stocks) %>%
      formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
      formatStyle(c('STL%'),backgroundColor = plcl.stlpct) %>%
      formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
      formatStyle(c('BLK%'),backgroundColor = plcl.blkpct) %>%
      
      formatStyle(c('Net Rating'),backgroundColor = plcl.netrating) %>%
      formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
      formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
      formatStyle(c('PER'),backgroundColor = plcl.per) %>%
      
      
      # add solid black borders to group related stats
      formatStyle(c(1,2,3,8,10, 14, 20, 25, 30, 35, 39),`border-left` = '4px solid black')%>% 
      formatStyle(c(3),`border-right` = '4px solid black')%>% 
      
      # adjust width of player name and team column
      formatStyle(columns = c(1), width='600px') %>% 
      formatStyle(columns = c(3), width='400px') %>% 
      
      # bold BASIC colmn
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('BASIC', 'Player', 'School'))%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1) %>%
      formatRound(columns=c("AST:TOV"), digits = 2) %>% 
      formatRound(columns=c("Natl.Rank", "Conf.Rank", 'Team.Rank', 'GP', 'MPG'), digits = 0)
      #formatRound(columns=c("BASIC"), digits = 1)
  })
  
  ###### Teams DT with full stats
  output$fullTeamDT <- DT::renderDataTable({
    
    
    
    # adjust column names for readablility
    Teams$`Adj Extra Poss Diff` = Teams$adj_poss_diff
    Teams$`eFG% Diff` = Teams$eFG_diff
    Teams$`FTR Diff` = Teams$FTR_diff
    Teams$`Kenpom AdjEM` = round(Teams$Kenpom.AdjEM, 1)
    Teams$`Kenpom Rank` = Teams$Kenpom.Rank
    Teams$`Kenpom Off Eff` = round(Teams$`AdjOE`, 1)
    Teams$`Kenpom Def Eff` = round(Teams$`AdjDE`, 1)
    Teams$`SOS Rank` = as.numeric(Teams$SOS.Rank)
    Teams$`Net Rating` = Teams$eDiff
    Teams$`ETW Rank` = Teams$Rank
    Teams$`2PA/G` = Teams$`2PA`
    Teams$`3PA/G` = Teams$`3PA`
    Teams$`FTA/G` = Teams$`FTA`
    Teams$`2PA/100 Poss` = Teams$`2PAp100`
    Teams$`3PA/100 Poss` = Teams$`3PAp100`
    Teams$`FTA/100 Poss` = Teams$`FTAp100`
    
    Teams$`2PA/G_Opp` = Teams$`2PA_Opp`
    Teams$`3PA/G_Opp` = Teams$`3PA_Opp`
    Teams$`FTA/G_Opp` = Teams$`FTA_Opp`
    Teams$`2PA/100 Poss_Opp` = Teams$`2PAp100_Opp`
    Teams$`3PA/100 Poss_Opp` = Teams$`3PAp100_Opp`
    Teams$`FTA/100 Poss_Opp` = Teams$`FTAp100_Opp`
    #Teams$`ETW Offense` = Teams$`ETW Offense`
    #Teams$`ETW Defense` = Teams$` Defense`
    
    Teams = Teams %>% arrange(desc(`ETW Offense`))
    Teams$`ETW Offense Rank` = seq(1, length(Teams$eDiff))
    Teams = Teams %>% arrange(desc(`ETW Defense`))
    Teams$`ETW Defense Rank` = seq(1, length(Teams$eDiff))
    
    Teams=Teams %>% 
      dplyr::select(School, Conference, 
             `ETW Rank`,ETW,`ETW Offense Rank`,`ETW Offense`,`ETW Defense Rank`,`ETW Defense`,
             #`eFG% Diff`, `Adj Extra Poss Diff`, `FTR Diff`,
             PPG, PPP, `TS%`, `eFG%`, `FG%`,
             `2PA/G`, `2PA/100 Poss`, `2P%`, `3PA/G`, `3PA/100 Poss`, `3P%`, `FTA/G`, `FTA/100 Poss`, `FT%`,
             APG, `AST%`, TOV, `TOV%`,
             DRB, `DRB%`, ORB, `ORB%`,
             SPG, `STL%`, BPG, `BLK%`,
             `Net Rating`, ORtg, DRtg,
             SOS, `SOS Rank`,
             `Kenpom Rank`, `Kenpom AdjEM`, `Kenpom Off Eff`, `Kenpom Def Eff`, 
             PPG_Opp, PPP_Opp, `TS%_Opp`, `eFG%_Opp`, `FG%_Opp`,
             `2PA/G_Opp`, `2PA/100 Poss_Opp`, `2P%_Opp`, `3PA/G_Opp`, `3PA/100 Poss_Opp`, `3P%_Opp`, `FTA/G_Opp`, `FTA/100 Poss_Opp`, `FT%_Opp`,
             APG_Opp, TOV_Opp, `AST%_Opp`, `TOV%_Opp`,
             DRB_Opp, `DRB%_Opp`, ORB_Opp, `ORB%_Opp`,
             SPG_Opp, `STL%_Opp`, BPG_Opp, `BLK%_Opp`,
             Conf.Rank)
    
    #Teams$`eFG% Diff` = round(Teams$`eFG% Diff`, 4)
    Teams$`FT%` = round(Teams$`FT%`, 2)
    #Teams$`FTR Diff` = round(Teams$`FTR Diff`, 4)
    #Teams$`Adj Extra Poss Diff` = round(Teams$`Adj Extra Poss Diff`, 3)
    Teams$`2PA/G_Opp` = round(Teams$`2PA/G_Opp`, 1)
    Teams$`2P%_Opp` = round(Teams$`2P%_Opp`*100, 1)
    Teams$`TS%` = round(Teams$`TS%`*100, 1)
    Teams$`eFG%` = round(Teams$`eFG%`*100, 1)
    Teams$`FG%` = round(Teams$`FG%`*100, 1)
    Teams$`PPP` = round(Teams$`PPP`, 3)
    
    Teams$`TS%_Opp` = round(Teams$`TS%_Opp`*100, 1)
    Teams$`eFG%_Opp` = round(Teams$`eFG%_Opp`*100, 1)
    Teams$`FG%_Opp` = round(Teams$`FG%_Opp`*100, 1)
    Teams$`3P%_Opp` = round(Teams$`3P%_Opp`*100, 1)
    Teams$`FT%_Opp` = round(Teams$`FT%_Opp`*100, 1)
    Teams$`ETW Offense` = round(Teams$`ETW Offense`, 3)
    Teams$`ETW Defense` = round(Teams$`ETW Defense`, 3)
    
    #colnames(Teams)=gsub("_Opp", paste("Opponent ", gsub("_Opp", "", colnames(Teams))), colnames(Teams))
    #colnames(Teams)=gsub(" School", "", colnames(Teams))
    colnames(Teams)=gsub("_Opp", " Allowed", colnames(Teams))
    Teams = Teams %>% arrange(`ETW Rank`)
    ### filters
    # conference
    if (input$Conference != "All") {Teams <- Teams[Teams$Conference == input$Conference,]}
    
    # school
    if (input$School != "All") {Teams <- Teams[Teams$School == input$School,]}
    
    #print(input$`fullStats-finalFourCheckbox`)
    #if (input$`fullStats-finalFourCheckbox`) {Teams <- Teams[Teams$School %in% c("UConn", 'Purdue', 'NC State', 'Alabama'),]}
    
    Teams$Conference = gsub('Missouri Valley', 'MVC', Teams$Conference)
    Teams$Conference = gsub('Mountain West', 'MWC', Teams$Conference)
    Teams$Conference = gsub('Patriot League', 'Patriot', Teams$Conference)
    Teams$Conference = gsub('Summit League', 'Summit', Teams$Conference)
    Teams$Conference = gsub('Horizon League', 'Horizon', Teams$Conference)
    
    ### DT
    DT::datatable(Teams,rownames = FALSE,
                  extensions = "FixedColumns",
                  # list(list(className = 'dt-center', targets = "_all"), 
                  #      list(width = '150px', targets = c(3)),
                  #      list(width = '100px', targets = c(1))
                  # ),
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                   list(width = '100px', targets = c(1))),
                                 scrollX = '400px',  autoWidth = T, fixedColumns = list(leftColumns = 4), pageLength=15)) %>% 
      
      ### EFFICIENCY
      formatStyle(columns = c("School"), width='300px') %>% 
      formatStyle(columns = c("School"), width='250px') %>% 
      formatStyle(c('ETW'), backgroundColor = tmcl.tops) %>%
      formatStyle(c('ETW Offense'), backgroundColor = tmcl.tops.off) %>%
      formatStyle(c('ETW Defense'), backgroundColor = tmcl.tops.def) %>%#styleInterval(seq(-20,14,0.05), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(-20,14,0.05))+1)))%>% 
      #formatStyle(c('FT%'), backgroundColor = ft.colors) %>% #styleInterval(seq(min(Teams$`FT%`),max(Teams$`FT%`),0.001), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$`FT%`),max(Teams$`FT%`),0.001))+1)))%>% 
      #formatStyle(c('eFG% Diff'), backgroundColor = tmcl.efg_diff) %>% #styleInterval(seq(min(Teams$`eFG% Diff`)+0.05,max(Teams$`eFG% Diff`),0.001), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$`eFG% Diff`)+0.05,max(Teams$`eFG% Diff`),0.001))+1)))%>% 
      #formatStyle(c('Adj Extra Poss Diff'), backgroundColor = tmcl.axpd) %>% #styleInterval(seq(min(Teams$`Adj Extra Poss Diff`)+4,max(Teams$`Adj Extra Poss Diff`),0.001), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$`Adj Extra Poss Diff`)+4,max(Teams$`Adj Extra Poss Diff`),0.001))+1)))%>% 
      #formatStyle(c('FTR Diff'), backgroundColor = tmcl.ftrdiff) %>% #styleInterval(seq(min(Teams$`FTR Diff`),max(Teams$`FTR Diff`),0.001), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$`FTR Diff`),max(Teams$`FTR Diff`),0.001))+1)))%>% 
      formatStyle(c('Kenpom AdjEM'), backgroundColor = tmcl.kp) %>%
      formatStyle(c('Kenpom Off Eff'), backgroundColor = tmcl.kpo) %>%
      formatStyle(c('Kenpom Def Eff'), backgroundColor = tmcl.kpd) %>%#styleInterval(seq(min(Teams$`Kenpom AdjEM`),max(Teams$`Kenpom AdjEM`),0.01), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$`Kenpom AdjEM`),max(Teams$`Kenpom AdjEM`),0.01))+1)))%>% 
      formatStyle(c('SOS'), backgroundColor = tmcl.sos) %>% #styleInterval(seq(min(Teams$`Kenpom AdjEM`),max(Teams$`Kenpom AdjEM`),0.01), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$`Kenpom AdjEM`),max(Teams$`Kenpom AdjEM`),0.01))+1)))%>% 
      
      formatStyle(c('ORtg'), backgroundColor = tmcl.ortg) %>% #styleInterval(seq(min(Teams$`Kenpom AdjEM`),max(Teams$`Kenpom AdjEM`),0.01), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b','#c5fc8b', '#a3fb45','#6fd404'))(length(seq(min(Teams$`Kenpom AdjEM`),max(Teams$`Kenpom AdjEM`),0.01))+1)))%>% 
      formatStyle(c('DRtg'), backgroundColor = tmcl.drtg) %>%
      formatStyle(c('Net Rating'), backgroundColor = tmcl.netrating) %>%
      
      ### OFFENSE
      
      formatStyle(c('PPG'), backgroundColor = tmcl.ppg) %>%
      formatStyle(c('PPP'), backgroundColor = tmcl.ppp) %>%
      formatStyle(c('TS%'), backgroundColor = tmcl.ts) %>%
      formatStyle(c('eFG%'), backgroundColor = tmcl.efg) %>%
      formatStyle(c('FG%'), backgroundColor = tmcl.fg) %>%
      
      formatStyle(c('2PA/G'), backgroundColor = tmcl.2pa) %>%
      formatStyle(c('2PA/100 Poss'), backgroundColor = tmcl.2pa100) %>%
      formatStyle(c('2P%'), backgroundColor = tmcl.2p) %>%
      formatStyle(c('3PA/G'), backgroundColor = tmcl.3pa) %>%
      formatStyle(c('3PA/100 Poss'), backgroundColor = tmcl.3pa100) %>%
      formatStyle(c('3P%'), backgroundColor = tmcl.3p) %>%
      formatStyle(c('FTA/G'), backgroundColor = tmcl.fta) %>%
      formatStyle(c('FTA/100 Poss'), backgroundColor = tmcl.fta100) %>%
      formatStyle(c('FT%'), backgroundColor = tmcl.ft) %>%
      
      formatStyle(c('APG'), backgroundColor = tmcl.apg) %>%
      formatStyle(c('TOV'), backgroundColor = tmcl.tov) %>%
      formatStyle(c('AST%'), backgroundColor = tmcl.astpct) %>%
      formatStyle(c('TOV%'), backgroundColor = tmcl.tovpct) %>%
      
      formatStyle(c('DRB'), backgroundColor = tmcl.drb) %>%
      formatStyle(c('DRB%'), backgroundColor = tmcl.drbpct) %>%
      formatStyle(c('ORB'), backgroundColor = tmcl.orb) %>%
      formatStyle(c('ORB%'), backgroundColor = tmcl.orbpct) %>%
      
      formatStyle(c('SPG'), backgroundColor = tmcl.spg) %>%
      formatStyle(c('STL%'), backgroundColor = tmcl.stlpct) %>%
      formatStyle(c('BPG'), backgroundColor = tmcl.bpg) %>%
      formatStyle(c('BLK%'), backgroundColor = tmcl.blkpct) %>%
      
      ### DEFENSE
      
      formatStyle(c('PPG Allowed'), backgroundColor = tmcl.ppg.opp) %>%
      formatStyle(c('PPP Allowed'), backgroundColor = tmcl.ppp.opp) %>%
      formatStyle(c('TS% Allowed'), backgroundColor = tmcl.ts.opp) %>%
      formatStyle(c('eFG% Allowed'), backgroundColor = tmcl.efg.opp) %>%
      formatStyle(c('FG% Allowed'), backgroundColor = tmcl.fg.opp) %>%
      
      formatStyle(c('2PA/G Allowed'), backgroundColor = tmcl.2pa.opp) %>%
      formatStyle(c('2P% Allowed'), backgroundColor = tmcl.2p.opp) %>%
      formatStyle(c('3PA/G Allowed'), backgroundColor = tmcl.3pa.opp) %>%
      formatStyle(c('3P% Allowed'), backgroundColor = tmcl.3p.opp) %>%
      formatStyle(c('FTA/G Allowed'), backgroundColor = tmcl.fta.opp) %>%
      formatStyle(c('FT% Allowed'), backgroundColor = tmcl.ft.opp) %>%
      formatStyle(c('2PA/100 Poss Allowed'), backgroundColor = tmcl.2pa100.opp) %>%
      formatStyle(c('3PA/100 Poss Allowed'), backgroundColor = tmcl.3pa100.opp) %>%
      formatStyle(c('FTA/100 Poss Allowed'), backgroundColor = tmcl.fta100.opp) %>%
      
      formatStyle(c('APG Allowed'), backgroundColor = tmcl.apg.opp) %>%
      formatStyle(c('TOV Allowed'), backgroundColor = tmcl.tov.opp) %>%
      formatStyle(c('AST% Allowed'), backgroundColor = tmcl.astpct.opp) %>%
      formatStyle(c('TOV% Allowed'), backgroundColor = tmcl.tovpct.opp) %>%
      
      formatStyle(c('DRB Allowed'), backgroundColor = tmcl.drb.opp) %>%
      formatStyle(c('DRB% Allowed'), backgroundColor = tmcl.drbpct.opp) %>%
      formatStyle(c('ORB Allowed'), backgroundColor = tmcl.orb.opp) %>%
      formatStyle(c('ORB% Allowed'), backgroundColor = tmcl.orbpct.opp) %>%
      
      formatStyle(c('SPG Allowed'), backgroundColor = tmcl.spg.opp) %>%
      formatStyle(c('STL% Allowed'), backgroundColor = tmcl.stlpct.opp) %>%
      formatStyle(c('BPG Allowed'), backgroundColor = tmcl.bpg.opp) %>%
      formatStyle(c('BLK% Allowed'), backgroundColor = tmcl.blkpct.opp) %>%
      
      formatStyle(c(1,9,14,17,20,23,27,31,35,39,40,44,49,52,55,58,62,66,70,74),`border-left` = '4px solid black')%>% 
      formatStyle(c(4),`border-right` = '4px solid black')%>% 
      formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('School','Conference', 'Kenpom Rank',
                                                                'Kenpom AdjEM', 'ETW', 'ETW Rank',
                                                                'ETW Offense Rank', 'ETW Defense Rank'))%>% 
      formatRound(which(sapply(Teams,is.numeric)), digits = 1) %>%
      formatRound(columns=c("ETW","ETW Offense", "ETW Defense","PPP", "PPP Allowed"), digits = 3) %>% 
      formatRound(columns=c("ETW Rank", "ETW Offense Rank", 'ETW Defense Rank', 'Conf.Rank', 'SOS Rank', 'Kenpom Rank'), digits = 0)
      
      
  })
  
  output$fullConferenceDT <- DT::renderDataTable({
    
    metricInput = input$`Conference-metric`
    confGroup = input$`Conference-group`
    
    
    if (metricInput == "KenPom"){
      
      if (confGroup != "All"){
        
        if (confGroup == "Power-5"){
          conf.kp = conf.kp %>% 
            filter(Conference %in% high.major)
        } else if (confGroup == "Mid-Major"){
          conf.kp = conf.kp %>% 
            filter(Conference %in% mid.major)
        }else if (confGroup == "Low-Major"){
          conf.kp = conf.kp %>% 
            filter(Conference %in% low.major)
        }
        
      }
      
      DT::datatable(conf.kp,rownames = FALSE,
                    extensions = "FixedColumns",
                    # list(list(className = 'dt-center', targets = "_all"), 
                    #      list(width = '150px', targets = c(3)),
                    #      list(width = '100px', targets = c(1))
                    # ),
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(width = '100px', targets = c(1))),
                                   scrollX = T,  autoWidth = T, fixedColumns = list(leftColumns = 1), pageLength=16)) %>%
        
        formatStyle(c(1,3,7,9,12,13),`border-left` = '4px solid black')%>% 
        formatStyle(c(1),`border-right` = '4px solid black')%>% 
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Conference', 'Avg. KP AdjEM'))%>% 
        formatRound(which(sapply(conf.kp, is.numeric)), digits = 1) %>% 
        
        formatStyle(c('Avg. KP AdjEM'), backgroundColor = conf.kp.all) %>% 
        formatStyle(c('Top 50% - Avg. KP AdjEM'), backgroundColor = conf.kp.50) %>% 
        formatStyle(c('Top 25% - Avg. KP AdjEM'), backgroundColor = conf.kp.25) %>% 
        formatStyle(c('Top 75% - Avg. KP AdjEM'), backgroundColor = conf.kp.75) %>% 
        formatStyle(c('Bottom 25% - Avg. KP AdjEM'), backgroundColor = conf.kp.b25) %>% 
        formatStyle(c('Standard Deviation KP AdjEM'), backgroundColor = conf.kp.std) %>% 
        formatStyle(c('Avg. KP Offense'), backgroundColor = conf.kp.off) %>% 
        formatStyle(c('Avg. KP Defense'), backgroundColor = conf.kp.def) %>% 
        #formatStyle(c('Avg. KP Tempo'), backgroundColor = conf.kp.tempo) %>% 
        
        formatStyle(columns = c("Conference"), width='275px') %>% 
        formatStyle(columns = c("Best Team (Rk)", "Worst Team (Rk)", "Middle Team (Rk)"), width='450px') %>% 
        formatStyle(columns = c("Top 50% - Avg. KP AdjEM", 'Top 25% - Avg. KP AdjEM',
                                'Top 75% - Avg. KP AdjEM', 'Bottom 25% - Avg. KP AdjEM'), width='85px')
      
      
    } 
    # else if (metricInput == "BartTorvik"){
    #   
    #   if (confGroup != "All"){
    #     
    #     if (confGroup == "Power-5"){
    #       conf.bt = conf.bt %>% 
    #         filter(Conference %in% high.major)
    #     } else if (confGroup == "Mid-Major"){
    #       conf.bt = conf.bt %>% 
    #         filter(Conference %in% mid.major)
    #     }else if (confGroup == "Low-Major"){
    #       conf.bt = conf.bt %>% 
    #         filter(Conference %in% low.major)
    #     }
    #     
    #   }
    #   
    #   DT::datatable(conf.bt,rownames = FALSE,
    #                 extensions = "FixedColumns",
    #                 # list(list(className = 'dt-center', targets = "_all"), 
    #                 #      list(width = '150px', targets = c(3)),
    #                 #      list(width = '100px', targets = c(1))
    #                 # ),
    #                 options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
    #                                                  list(width = '100px', targets = c(1))),
    #                                scrollX = T,  autoWidth = F, fixedColumns = list(leftColumns = 1), pageLength=16)) %>%
    #     
    #     formatStyle(c(1,3,7,9,12,13),`border-left` = '4px solid black')%>% 
    #     formatStyle(c(1),`border-right` = '4px solid black')%>% 
    #     formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Conference', 'Avg. Barthag'))%>% 
    #     formatRound(which(sapply(conf.kp, is.numeric)), digits = 3) %>% 
    #     formatRound(c("Avg. Torvik Offense", 'Avg. Torvik Defense'), digits = 1) %>% 
    #     
    #     formatStyle(c('Avg. Barthag'), backgroundColor = conf.bt.all) %>% 
    #     formatStyle(c('Top 50% - Avg. Barthag'), backgroundColor = conf.bt.50) %>% 
    #     formatStyle(c('Top 25% - Avg. Barthag'), backgroundColor = conf.bt.25) %>% 
    #     formatStyle(c('Top 75% - Avg. Barthag'), backgroundColor = conf.bt.75) %>% 
    #     formatStyle(c('Bottom 25% - Avg. Barthag'), backgroundColor = conf.bt.b25) %>% 
    #     formatStyle(c('Standard Deviation Barthag'), backgroundColor = conf.bt.std) %>% 
    #     formatStyle(c('Avg. Torvik Offense'), backgroundColor = conf.bt.off) %>% 
    #     formatStyle(c('Avg. Torvik Defense'), backgroundColor = conf.bt.def) %>% 
    #     #formatStyle(c('Avg. Torvik Tempo'), backgroundColor = conf.bt.tempo) %>% 
    #     
    #     formatStyle(columns = c("Conference"), width='275px') %>% 
    #     formatStyle(columns = c("Best Team (Rk)", "Worst Team (Rk)", "Middle Team (Rk)"), width='450px') %>% 
    #     formatStyle(columns = c("Top 50% - Avg. Barthag", 'Top 75% - Avg. Barthag',
    #                             'Top 25% - Avg. Barthag', 'Bottom 25% - Avg. Barthag'), width='85px')
    #   
    # }
    else if (metricInput == "Estimated Tournament Wins"){
      
      if (confGroup != "All"){
        
        if (confGroup == "Power-5"){
          conf.etw = conf.etw %>% 
            filter(Conference %in% high.major)
        } else if (confGroup == "Mid-Major"){
          conf.etw = conf.etw %>% 
            filter(Conference %in% mid.major)
        }else if (confGroup == "Low-Major"){
          conf.etw = conf.etw %>% 
            filter(Conference %in% low.major)
        }
        
      }
      
      DT::datatable(conf.etw,rownames = FALSE,
                    extensions = "FixedColumns",
                    # list(list(className = 'dt-center', targets = "_all"), 
                    #      list(width = '150px', targets = c(3)),
                    #      list(width = '100px', targets = c(1))
                    # ),
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(width = '100px', targets = c(1))),
                                   scrollX = T,  autoWidth = F, fixedColumns = list(leftColumns = 1), pageLength=16)) %>%
        
        formatStyle(c(1,3,7,9,12,13),`border-left` = '4px solid black')%>% 
        formatStyle(c(1),`border-right` = '4px solid black')%>% 
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Conference', 'Avg. ETW'))%>% 
        formatRound(which(sapply(conf.etw, is.numeric)), digits = 3) %>% 
        #formatRound(c("Avg. Tempo"), digits = 1) %>% 
        
        formatStyle(c('Avg. ETW'), backgroundColor = conf.etw.all) %>% 
        formatStyle(c('Top 50% - Avg. ETW'), backgroundColor = conf.etw.50) %>% 
        formatStyle(c('Top 25% - Avg. ETW'), backgroundColor = conf.etw.25) %>% 
        formatStyle(c('Top 75% - Avg. ETW'), backgroundColor = conf.etw.75) %>% 
        formatStyle(c('Bottom 25% - Avg. ETW'), backgroundColor = conf.etw.b25) %>% 
        formatStyle(c('Standard Deviation ETW'), backgroundColor = conf.etw.std) %>% 
        formatStyle(c('Avg. ETW Offense'), backgroundColor = conf.etw.off) %>% 
        formatStyle(c('Avg. ETW Defense'), backgroundColor = conf.etw.def) %>% 
        #formatStyle(c('Avg. Tempo'), backgroundColor = conf.etw.tempo) %>% 
        
        formatStyle(columns = c("Conference"), width='275px') %>% 
        formatStyle(columns = c("Best Team (Rk)", "Worst Team (Rk)", "Middle Team (Rk)"), width='450px') %>% 
        formatStyle(columns = c("Top 50% - Avg. ETW", 'Top 25% - Avg. ETW',
                                 'Top 75% - Avg. ETW', 'Bottom 25% - Avg. ETW'), width='85px')
      
    }else if (metricInput == "ESPN BPI"){
      
      
      if (confGroup != "All"){
        
        if (confGroup == "Power-5"){
          conf.bpi = conf.bpi %>% 
            filter(Conference %in% high.major)
        } else if (confGroup == "Mid-Major"){
          conf.bpi = conf.bpi %>% 
            filter(Conference %in% mid.major)
        }else if (confGroup == "Low-Major"){
          conf.bpi = conf.bpi %>% 
            filter(Conference %in% low.major)
        }
        
      }
      
      DT::datatable(conf.bpi,rownames = FALSE,
                    extensions = "FixedColumns",
                    # list(list(className = 'dt-center', targets = "_all"), 
                    #      list(width = '150px', targets = c(3)),
                    #      list(width = '100px', targets = c(1))
                    # ),
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(width = '100px', targets = c(1))),
                                   scrollX = T,  autoWidth = F, fixedColumns = list(leftColumns = 1), pageLength=16)) %>%
        
        formatStyle(c(1,3,7,9,12,13),`border-left` = '4px solid black')%>% 
        formatStyle(c(1),`border-right` = '4px solid black')%>% 
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Conference', 'Avg. ESPN BPI'))%>% 
        formatRound(which(sapply(conf.etw, is.numeric)), digits = 1) %>% 
        #formatRound(c("Avg. Tempo"), digits = 1) %>% 
        
        formatStyle(c('Avg. ESPN BPI'), backgroundColor = conf.bpi.all) %>% 
        formatStyle(c('Top 50% - Avg. BPI'), backgroundColor = conf.bpi.50) %>% 
        formatStyle(c('Top 25% - Avg. BPI'), backgroundColor = conf.bpi.25) %>% 
        formatStyle(c('Top 75% - Avg. BPI'), backgroundColor = conf.bpi.75) %>% 
        formatStyle(c('Bottom 25% - Avg. BPI'), backgroundColor = conf.bpi.b25) %>% 
        formatStyle(c('Standard Deviation BPI'), backgroundColor = conf.bpi.std) %>% 
        formatStyle(c('Avg. BPI Offense'), backgroundColor = conf.bpi.off) %>% 
        formatStyle(c('Avg. BPI Defense'), backgroundColor = conf.bpi.def) %>% 
        #formatStyle(c('Avg. Tempo'), backgroundColor = conf.bpi.tempo) %>% 
        
        formatStyle(columns = c("Conference"), width='275px') %>% 
        formatStyle(columns = c("Best Team (Rk)", "Worst Team (Rk)", "Middle Team (Rk)"), width='450px') %>% 
        formatStyle(columns = c("Top 50% - Avg. BPI", 'Top 25% - Avg. BPI',
                                'Top 75% - Avg. BPI', 'Bottom 25% - Avg. BPI'), width='85px')
      
      
      
    }
    # else if (metricInput == "Shot Quality"){
    #   
    #   
    #   if (confGroup != "All"){
    #     
    #     if (confGroup == "Power-5"){
    #       conf.sq = conf.sq %>% 
    #         filter(Conference %in% high.major)
    #     } else if (confGroup == "Mid-Major"){
    #       conf.sq = conf.sq %>% 
    #         filter(Conference %in% mid.major)
    #     }else if (confGroup == "Low-Major"){
    #       conf.sq = conf.sq %>% 
    #         filter(Conference %in% low.major)
    #     }
    #     
    #   }
    #   
    #   DT::datatable(conf.sq,rownames = FALSE,
    #                 extensions = "FixedColumns",
    #                 # list(list(className = 'dt-center', targets = "_all"), 
    #                 #      list(width = '150px', targets = c(3)),
    #                 #      list(width = '100px', targets = c(1))
    #                 # ),
    #                 options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
    #                                                  list(width = '100px', targets = c(1))),
    #                                scrollX = T,  autoWidth = F, fixedColumns = list(leftColumns = 1), pageLength=16)) %>%
    #     
    #     formatStyle(c(1,3,7,9,12,13),`border-left` = '4px solid black')%>% 
    #     formatStyle(c(1),`border-right` = '4px solid black')%>% 
    #     formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Conference', 'Avg. Shot Quality'))%>% 
    #     formatRound(which(sapply(conf.etw, is.numeric)), digits = 3) %>% 
    #     #formatRound(c("Avg. Tempo"), digits = 1) %>% 
    #     
    #     formatStyle(c('Avg. Shot Quality'), backgroundColor = conf.sq.all) %>% 
    #     formatStyle(c('Top 50% - Avg. SQ'), backgroundColor = conf.sq.50) %>% 
    #     formatStyle(c('Top 25% - Avg. SQ'), backgroundColor = conf.sq.25) %>% 
    #     formatStyle(c('Top 75% - Avg. SQ'), backgroundColor = conf.sq.75) %>% 
    #     formatStyle(c('Bottom 25% - Avg. SQ'), backgroundColor = conf.sq.b25) %>% 
    #     formatStyle(c('Standard Deviation SQ'), backgroundColor = conf.sq.std) %>% 
    #     formatStyle(c('Avg. SQ Offense'), backgroundColor = conf.sq.off) %>% 
    #     formatStyle(c('Avg. SQ Defense'), backgroundColor = conf.sq.def) %>% 
    #     #formatStyle(c('Avg. Tempo'), backgroundColor = conf.sq.tempo) %>% 
    #     
    #     formatStyle(columns = c("Conference"), width='275px') %>% 
    #     formatStyle(columns = c("Best Team (Rk)", "Worst Team (Rk)", "Middle Team (Rk)"), width='450px') %>% 
    #     formatStyle(columns = c("Top 50% - Avg. SQ", 'Top 25% - Avg. SQ',
    #                             'Top 75% - Avg. SQ', 'Bottom 25% - Avg. SQ'), width='85px')
    #   
    # }
    
    
  
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  ################### 3.4) Team Identity Tab ##################
  
  ## main team identity infoboxes
  ## adjOE, adjDE, adjTempo, SoS, TOPS

  output$`teamIdentity-mainPage` <- renderUI({
    
    ### get team to focus on from top search box
    TeamStats = TeamStats %>% arrange(desc(SOS))
    TeamStats$`SOS.Rank` = seq(1, length(TeamStats$PPG), 1)
    
    
    Teams = Teams %>% arrange(desc(`ETW Offense`))
    Teams$`ETW Offense Rank` = seq(1, length(Teams$PPG), 1)
    Teams = Teams %>% arrange(desc(`ETW Defense`))
    Teams$`ETW Defense Rank` = seq(1, length(Teams$PPG), 1)
    TeamsFull = TeamsFull %>% arrange(desc(`Possessions/G`))
    TeamsFull$`Possessions Rank` = seq(1, length(TeamsFull$PPG), 1)
    Teams$`ETW.Offense.pctl` = ntile(Teams$`ETW Offense`, 100)
    Teams$`ETW.Defense.pctl` = ntile(Teams$`ETW Defense`, 100)
    TeamsFull$`Possessions.pctl` = ntile(TeamsFull$`Possessions/G`, 100)
    
    
    TeamsFull = TeamsFull %>% arrange(desc(`ESPN BPI Offense`))
    TeamsFull$`ESPN BPI Offense Rank` = seq(1, length(TeamsFull$PPG), 1)
    TeamsFull = TeamsFull %>% arrange(desc(`ESPN BPI Defense`))
    TeamsFull$`ESPN BPI Defense Rank` = seq(1, length(TeamsFull$PPG), 1)
    TeamsFull$`ESPN BPI.Offense.pctl` = ntile(TeamsFull$`ESPN BPI Offense`, 100)
    TeamsFull$`ESPN BPI.Defense.pctl` = ntile(TeamsFull$`ESPN BPI Defense`, 100)
    
    Teams$`kp.off.pctl` = ntile(Teams$`AdjOE`, 100)
    Teams$`kp.def.pctl` = ntile(-Teams$`AdjDE`, 100)
    Teams$`kp.tempo.pctl` = ntile(Teams$`AdjTempo`, 100)
    
    TeamsFull = TeamsFull %>% arrange(desc(`T-Rank AdjOE`))
    TeamsFull$`T-Rank AdjOE Rank` = seq(1, length(TeamsFull$PPG), 1)
    TeamsFull = TeamsFull %>% arrange(`T-Rank AdjDE`)
    TeamsFull$`T-Rank AdjDE Rank` = seq(1, length(TeamsFull$PPG), 1)
    TeamsFull = TeamsFull %>% arrange(desc(`T-Rank Tempo`))
    TeamsFull$`T-Rank Tempo Rank` = seq(1, length(TeamsFull$PPG), 1)
    TeamsFull$`T-Rank.Offense.pctl` = ntile(TeamsFull$`T-Rank AdjOE`, 100)
    TeamsFull$`T-Rank.Defense.pctl` = ntile(-TeamsFull$`T-Rank AdjDE`, 100)
    TeamsFull$`T-Rank.Tempo.pctl` = ntile(TeamsFull$`T-Rank Tempo`, 100)
    
    # TeamsFull = TeamsFull %>% arrange(desc(`Shot Quality Adj. Offense`))
    # TeamsFull$`Shot Quality Offense Rank` = seq(1, length(TeamsFull$PPG), 1)
    # TeamsFull = TeamsFull %>% arrange(`Shot Quality Adj. Defense`)
    # TeamsFull$`Shot Quality Defense Rank` = seq(1, length(TeamsFull$PPG), 1)
    # 
    # TeamsFull$`Shot Quality.Offense.pctl` = ntile(TeamsFull$`Shot Quality Adj. Offense`, 100)
    # TeamsFull$`Shot Quality.Defense.pctl` = ntile(-TeamsFull$`Shot Quality Adj. Defense`, 100)
    # 
    #comehere
    
    this = Teams %>% filter(School == input$`teamIdentity-teamSearchBox`)
    thisStats = TeamStats %>% filter(Team == input$`teamIdentity-teamSearchBox`)
    thisBracket = Bracketology %>% filter(Team == input$`teamIdentity-teamSearchBox`)
    thisFull = TeamsFull %>% filter(School == input$`teamIdentity-teamSearchBox`)
    
    print(length(this))
    print(length(thisStats))
    print(length(thisBracket))
    print(length(thisFull))
    
    
    ratingSystem = input$`mainPage-teamEff2`
    #print(ratingSystem)
    if (ratingSystem == "Estimated Tournament Wins"){
      offName = "Est. Tournament Wins - Offense"
      defName = "Est. Tournament Wins - Defense"
      tempoName = "Pace"
      offStat = this$`ETW Offense`
      defStat = this$`ETW Defense`
      tempoStat = thisFull$`Possessions/G`
      offRank = this$`ETW Offense Rank`
      defRank = this$`ETW Defense Rank`
      tempoRank = thisFull$`Possessions Rank`
      offPctl = this$`ETW.Offense.pctl`
      defPctl = this$`ETW.Defense.pctl`
      tempoPctl = thisFull$`Possessions.pctl`
    } else if (ratingSystem == "ESPN BPI"){
      offName = "ESPN BPI Offense"
      defName = "ESPN BPI Defense"
      tempoName = "Pace"
      offStat = thisFull$`ESPN BPI Offense`
      defStat = thisFull$`ESPN BPI Defense`
      tempoStat = thisFull$`Possessions/G`
      offRank = thisFull$`ESPN BPI Offense Rank`
      defRank = thisFull$`ESPN BPI Defense Rank`
      tempoRank = thisFull$`Possessions Rank`
      offPctl = thisFull$`ESPN BPI.Offense.pctl`
      defPctl = thisFull$`ESPN BPI.Defense.pctl`
      tempoPctl = thisFull$`Possessions.pctl`
    } else if (ratingSystem == "KenPom"){
      offName = "KenPom Offensive Efficiency"
      defName = "KenPom Defensive Efficiency"
      tempoName = "KenPom Adj. Tempo"
      offStat = this$`AdjOE`
      defStat = this$`AdjDE`
      tempoStat = this$`AdjTempo`
      offRank = this$`RankAdjOE`
      defRank = this$`RankAdjDE`
      tempoRank = this$`RankAdjTempo`
      offPctl = this$`kp.off.pctl`
      defPctl = this$`kp.def.pctl`
      tempoPctl = this$`kp.tempo.pctl`
    } else if (ratingSystem == "BartTorvik"){
      offName = "T-Rank Offensive Efficiency"
      defName = "T-Rank Defensive Efficiency"
      tempoName = "T-Rank Adj. Tempo"
      offStat = thisFull$`T-Rank AdjOE`
      defStat = thisFull$`T-Rank AdjDE`
      tempoStat = thisFull$`T-Rank Tempo`
      offRank = thisFull$`T-Rank AdjOE Rank`
      defRank = thisFull$`T-Rank AdjDE Rank`
      tempoRank = thisFull$`T-Rank Tempo Rank`
      offPctl = thisFull$`T-Rank.Offense.pctl`
      defPctl = thisFull$`T-Rank.Defense.pctl`
      tempoPctl = thisFull$`T-Rank.Tempo.pctl`
    } 
    # else if (ratingSystem == "Shot Quality"){
    #   offName = "Shot Quality Adj. Offense"
    #   defName = "Shot Quality Adj. Defense"
    #   tempoName = "Pace"
    #   offStat = thisFull$`Shot Quality Adj. Offense`
    #   defStat = thisFull$`Shot Quality Adj. Defense`
    #   tempoStat = thisFull$`Possessions/G`
    #   offRank = thisFull$`Shot Quality Offense Rank`
    #   defRank = thisFull$`Shot Quality Defense Rank`
    #   tempoRank = thisFull$`Possessions Rank`
    #   offPctl = thisFull$`Shot Quality.Offense.pctl`
    #   defPctl = thisFull$`Shot Quality.Defense.pctl`
    #   tempoPctl = thisFull$`Possessions.pctl`
    # }
    
    if (offPctl == 100 & offRank != 1){offPctl=99}
    if (defPctl == 100 & defRank != 1){defPctl=99}
    if (tempoPctl == 100 & tempoRank != 1){tempoPctl=99}
    if (thisStats$SOS_pctl == 100 & thisStats$SOS.Rank != 1){thisStats$SOS_pctl=99}
    if (thisStats$AdjOE_pctl == 100 & thisStats$RankAdjOE != 1){thisStats$AdjOE_pctl=99}
    if (thisStats$AdjDE_pctl == 100 & thisStats$RankAdjDE != 1){thisStats$AdjDE_pctl=99}
    
    
    # this = this %>% 
    #   mutate(`3PAp100` = round((`3PA`/AdjTempo)*100, 1),
    #          #`3PAp100_pctl` = ntile(`3PAp100`, 100),
    #          `2PAp100` = round((`2PA`/AdjTempo)*100, 1),
    #          #`2PAp100_pctl` = ntile(`2PAp100`, 100),
    #          `FTAp100` = round((`FTA`/AdjTempo)*100, 1))
    # #`FTAp100_pctl` = ntile(`FTAp100`, 100))
    # 
    # this = this %>% 
    #   mutate(#`3PAp100` = (`3PA`/AdjTempo)*100,
    #     `3PAp100_pctl` = ntile(`3PAp100`, 100),
    #     #`2PAp100` = (`2PA`/AdjTempo)*100,
    #     `2PAp100_pctl` = ntile(`2PAp100`, 100),
    #     #`FTAp100` = (`FTA`/AdjTempo)*100,
    #     `FTAp100_pctl` = ntile(`FTAp100`, 100))
    
    
    fluidRow(column(12,
                    fluidRow(
                      
                      column(8,h2(strong("Team Efficiency"))),
                      
                      # column(4,selectInput("mainPage-teamEff", 
                      #                      label = strong("Select Team Rating System:",style='font-size:125%'),
                      #                      choices = c("ETW","KenPom","BartTorvik","ESPN BPI","Shot Quality"))),
                      
                      column(4,h2(strong("Team Stats")))
                      
                    ),
                    
                    br(),
                    fluidRow(
                      column(8,
                      column(6,
                             #p(strong("Offense"),style='font-size:115%'),
                             ### create infoboxes with reactive color and letter grade based on percentiles
                             
                            # tabBox(width=12,
                               #tabPanel(strong("Dash TOPS"),
                                        ### Adj Off Eff
                                        infoBox(width = 14,
                                                
                                                # header
                                                tags$p(strong(offName),style='font-size:105%'),
                                                
                                                # stat
                                                tags$h2(strong(paste(format(round(offStat, 3), nsmall=3))),style='font-size:125%'),
                                                
                                                # %tile
                                                tags$p(strong(paste(toOrdinal(offRank), "in D1 |", toOrdinal(offPctl), " %ile")), style='font-size:130%'),
                                                
                                                # reactive letter grade
                                                if (this$ETW.Offense.pctl >= 85){icon = icon("a")}
                                                else if (this$ETW.Offense.pctl >= 70){icon = icon("b")}
                                                else if (this$ETW.Offense.pctl >= 55){icon = icon("c")}
                                                else if (this$ETW.Offense.pctl >= 30){icon = icon("d")}
                                                else {icon = icon("f")},
                                                
                                                # reactive box color
                                                if (this$ETW.Offense.pctl >= 85){color='green'}
                                                else if (this$ETW.Offense.pctl >= 70){color='teal'}
                                                else if (this$ETW.Offense.pctl >= 55){color='blue'}
                                                else if (this$ETW.Offense.pctl >= 30){color='orange'}
                                                else {color='red'}
                                                
                                        ),
                                        
                                        ### Adj Def Eff
                                        infoBox(width = 14,
                                                
                                                # header
                                                tags$p(strong(defName),style='font-size:105%'),
                                                
                                                # stat
                                                tags$h2(strong(paste(format(round(defStat,3), nsmall=3))), style='font-size:125%'),
                                                
                                                # %ile
                                                tags$p(strong(paste(toOrdinal(defRank), "in D1 |", toOrdinal(defPctl), " %ile")), style='font-size:130%'),
                                                
                                                # reactive letter grade
                                                if (this$ETW.Defense.pctl >= 85){icon = icon("a")}
                                                else if (this$ETW.Defense.pctl >= 70){icon = icon("b")}
                                                else if (this$ETW.Defense.pctl >= 55){icon = icon("c")}
                                                else if (this$ETW.Defense.pctl >= 30){icon = icon("d")}
                                                else {icon = icon("f")},
                                                
                                                # reactive box color
                                                if (this$ETW.Defense.pctl >= 85){color='green'}
                                                else if (this$ETW.Defense.pctl >= 70){color='teal'}
                                                else if (this$ETW.Defense.pctl >= 55){color='blue'}
                                                else if (this$ETW.Defense.pctl >= 30){color='orange'}
                                                else {color='red'}
                                        )
                               #),
                                        
                               
                               # #tabPanel(strong("KenPom"),
                               #          
                               #          ### Adj Off Eff
                               #          infoBox(width = 14,
                               #                  
                               #                  # header
                               #                  tags$p(strong("KP Adj. Offensive Efficiency"),style='font-size:105%'),
                               #                  
                               #                  # stat
                               #                  tags$h2(strong(paste(format(round(thisStats$AdjOE, 1), nsmall=1), "Pts/100 Poss")),style='font-size:95%'),
                               #                  
                               #                  # %tile
                               #                  tags$p(strong(paste(toOrdinal(thisStats$RankAdjOE), "in D1 |", toOrdinal(thisStats$AdjOE_pctl), " %ile")), style='font-size:130%'),
                               #                  
                               #                  # reactive letter grade
                               #                  if (thisStats$`AdjOE_pctl` >= 85){icon = icon("a")}
                               #                  else if (thisStats$AdjOE_pctl >= 70){icon = icon("b")}
                               #                  else if (thisStats$AdjOE_pctl >= 55){icon = icon("c")}
                               #                  else if (thisStats$AdjOE_pctl >= 30){icon = icon("d")}
                               #                  else {icon = icon("f")},
                               #                  
                               #                  # reactive box color
                               #                  if (thisStats$`AdjOE_pctl` >= 85){color='green'}
                               #                  else if (thisStats$AdjOE_pctl >= 70){color='teal'}
                               #                  else if (thisStats$AdjOE_pctl >= 55){color='blue'}
                               #                  else if (thisStats$AdjOE_pctl >= 30){color='orange'}
                               #                  else {color='red'}
                               #                  
                               #          ),
                               #          
                                        
                               ),

                             
                             
                             
                      
                      column(6,
                             
                             ### Adj tempo
                             infoBox(width = 14,
                                     
                                     # header
                                     tags$p(strong(tempoName),style='font-size:105%'),
                                     
                                     # stat
                                     tags$h2(strong(paste(format(round(tempoStat,1), nsmall=1), "Poss/G")), style='font-size:105%'),
                                     
                                     # %ile
                                     tags$p(strong(paste(toOrdinal(tempoRank), "in D1 |", toOrdinal(tempoPctl), " %ile")), style='font-size:130%'),
                                     
                                     # reactive letter grade
                                     if (thisStats$`AdjTempo_pctl` >= 90){icon = icon("jet-fighter")}
                                     else if (thisStats$AdjTempo_pctl >= 70){icon = icon("person-running")}
                                     else if (thisStats$AdjTempo_pctl >= 50){icon = icon("person-walking")}
                                     else if (thisStats$AdjTempo_pctl >= 35){icon = icon("person-hiking")}
                                     else if (thisStats$AdjTempo_pctl >= 15){icon = icon("baby")}
                                     else {icon = icon("person-cane")},
                                     
                                     # reactive box color
                                     if (thisStats$`AdjTempo_pctl` >= 90){color='red'}
                                     else if (thisStats$AdjTempo_pctl >= 70){color='orange'}
                                     else if (thisStats$AdjTempo_pctl >= 50){color='yellow'}
                                     else if (thisStats$AdjTempo_pctl >= 30){color='aqua'}
                                     else if (thisStats$AdjTempo_pctl >= 15){color='teal'}
                                     else {color='blue'}
                             ),
                             
                             ### SoS
                             infoBox(width = 14,
                                     
                                     # header
                                     tags$p(strong("Strength of Schedule"),style='font-size:105%'),
                                     
                                     # stat
                                     tags$h2(strong(paste(format(thisStats$SOS, nsmall=1), "SoS")), style='font-size:105%'),
                                     
                                     # %ile
                                     tags$p(strong(HTML(paste(toOrdinal(thisStats$`SOS.Rank`),  "in D1 |", toOrdinal(thisStats$SOS_pctl), " %ile"))), style='font-size:130%'),
                                     
                                     # reactive letter grades
                                     if (thisStats$`SOS_pctl` >= 85){icon = icon("a")}
                                     else if (thisStats$SOS_pctl >= 70){icon = icon("b")}
                                     else if (thisStats$SOS_pctl >= 55){icon = icon("c")}
                                     else if (thisStats$SOS_pctl >= 30){icon = icon("d")}
                                     else {icon = icon("f")},
                                     
                                     # reactive %ile
                                     if (thisStats$`SOS_pctl` >= 85){color='green'}
                                     else if (thisStats$SOS_pctl >= 70){color='teal'}
                                     else if (thisStats$SOS_pctl >= 55){color='blue'}
                                     else if (thisStats$SOS_pctl >= 30){color='orange'}
                                     else {color='red'}
                             )  
                      ),
                      
                      h2(strong("Previous 10 Years per KenPom")),
                      
                      tabBox(width=12,
                             tabPanel(strong("Efficiency Margin"),
                                      #p(strong("Previous 10 Kenpom finishes")),
                                      plotlyOutput("teamIdentity-kpAdjEM-timeSeries", height = 350)),
                             tabPanel(strong("Offensive Efficiency"),
                                      plotlyOutput("teamIdentity-kpAdjOff-timeSeries", height = 350)),
                             tabPanel(strong("Defensive Efficiency"),
                                      plotlyOutput("teamIdentity-kpAdjDef-timeSeries", height = 350)),
                             tabPanel(strong("Tempo"),
                                      plotlyOutput("teamIdentity-kpAdjTempo-timeSeries", height = 350))
                      )
                      

                      ),

                             
                             ### specific team stats
                             column(4,
                                    
                                    tabBox(width=12,
                                           #tags$head(tags$style(HTML(".small-box {height: 50px}"))),
                                           ###### On Offense
                                           tabPanel(strong("In Possession"),
                                                    fluidRow(
                                                      
                                                      column(12,

                                                             ### Adj Def Eff
                                                             valueBox(width = 14,

                                                                     # header
                                                                     value=tags$p(strong("KenPom Adj. Offensive Eff."),style='font-size:55%'),

                                                                     # stat
                                                                     #tags$h2(strong(paste(format(round(thisStats$AdjDE,1), nsmall=1), "Pts Allowed/100 Poss")), style='font-size:95%'),

                                                                     # %ile
                                                                     subtitle=tags$p(strong(HTML(paste(format(round(thisStats$AdjOE,1), nsmall=1), "Pts/100 Poss<br/>",
                                                                                                 toOrdinal(thisStats$RankAdjOE), "in D1 |", toOrdinal(thisStats$AdjOE_pctl), " %ile"))),
                                                                                     style='font-size:120%'),

                                                                     # # reactive letter grade
                                                                     # if (thisStats$`AdjDE_pctl` >= 85){icon = icon("a")}
                                                                     # else if (thisStats$AdjDE_pctl >= 70){icon = icon("b")}
                                                                     # else if (thisStats$AdjDE_pctl >= 55){icon = icon("c")}
                                                                     # else if (thisStats$AdjDE_pctl >= 30){icon = icon("d")}
                                                                     # else {icon = icon("f")},
                                                                     icon = NULL,

                                                                     # reactive box color
                                                                     if (thisStats$`AdjOE_pctl` >= 85){color='green'}
                                                                     else if (thisStats$AdjOE_pctl >= 70){color='teal'}
                                                                     else if (thisStats$AdjOE_pctl >= 55){color='blue'}
                                                                     else if (thisStats$AdjOE_pctl >= 30){color='orange'}
                                                                     else {color='red'}
                                                             )

                                                             ),
                                                      
                                                      
                                                      column(6,
                                                             
                                                             ### TS%
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      value=tags$p(strong(paste0(format(thisStats$`eFG%`, nsmall = 1), "% Effective FG"),style='font-size:115%')),
                                                                      
                                                                      # stat
                                                                      #h2(strong(paste0(thisStats$`TS%`,"%"))),
                                                                      
                                                                      # %ile
                                                                      subtitle=tags$p(strong(paste(toOrdinal(thisStats$`eFG%_rank`), " | ",toOrdinal(thisStats$`eFG%_pctl`), " %ile")),style='font-size:120%'),
                                                                      
                                                                      icon = NULL,
                                                                      # reactive letter grade
                                                                      # if (thisStats$`TS%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`TS%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`TS%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`TS%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`eFG%_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`eFG%_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`eFG%_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`eFG%_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                                      
                                                                      #color = pctl.colors$colors[pctl.colors$pctl==round(thisStats$AdjOE_pctl,0)]
                                                             )),#),
                                                    
                                                    #fluidRow(
                                                      column(6,
                                                             
                                                             ### TS%
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      tags$p(strong(paste0(format(thisStats$`TS%`, nsmall=1),"% True Shooting"),style='font-size:115%')),
                                                                      
                                                                      # stat
                                                                      #tags$h2(strong(paste0(thisStats$`AST%`,"%")),style='font-size:85%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(thisStats$`TS%_rank`), " | ", toOrdinal(thisStats$`TS%_pctl`), " %ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # reactive letter grade    
                                                                      # if (thisStats$`AST%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`AST%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`AST%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`AST%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`TS%_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`TS%_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`TS%_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`TS%_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ))),
                                                    
                                                    
                                                    fluidRow(   
                                                      column(12,
                                                             
                                                             ### ORB%
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      tags$p(strong(paste0(format(thisStats$`ORB%`, nsmall=1), "% Offensive Rebound Rate"),style='font-size:125%')),
                                                                      
                                                                      # stat
                                                                      #tags$h2(strong(paste0(thisStats$`ORB%`,"%")),style='font-size:85%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(thisStats$`ORB%_rank`), " | ", toOrdinal(thisStats$`ORB%_pctl`), " %ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade       
                                                                      # if (thisStats$`DRB%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`DRB%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`DRB%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`DRB%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`ORB%_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`ORB%_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`ORB%_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`ORB%_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ))),
                                                    fluidRow(   
                                                      column(12,
                                                             
                                                             ### TOV %
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      tags$p(strong(paste0(format(thisStats$`TOV%`, nsmall=1),"% Turnover Rate"),style='font-size:125%')),
                                                                      
                                                                      # stat
                                                                      #tags$h2(strong(paste0(thisStats$`TOV%`,"%")),style='font-size:85%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(thisStats$`TOV%_rank`), " | ", toOrdinal(thisStats$`TOV%_pctl`), " %ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade        
                                                                      # if (thisStats$`TOV%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`TOV%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`TOV%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`TOV%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`TOV%_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`TOV%_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`TOV%_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`TOV%_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ))),
                                                    fluidRow(   
                                                      column(12,
                                                             
                                                             ### TOV %
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      tags$p(strong(paste0(format(round(this$`FTR`*100,1), nsmall=1),"% Free Throw Rate"),style='font-size:125%')),
                                                                      
                                                                      # stat
                                                                      #tags$h2(strong(paste0(thisStats$`TOV%`,"%")),style='font-size:85%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(this$`FTR_rank`), " | ", toOrdinal(this$`FTR_pctl`), " %ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade        
                                                                      # if (thisStats$`TOV%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`TOV%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`TOV%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`TOV%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (this$`FTR_pctl` >= 85){color='green'}
                                                                      else if (this$`FTR_pctl` >= 70){color='teal'}
                                                                      else if (this$`FTR_pctl` >= 55){color='blue'}
                                                                      else if (this$`FTR_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ))),
                                                    fluidRow(   
                                                      column(12,
                                                             
                                                             ### AST%
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      tags$p(strong(paste0(format(thisStats$`AST%`, nsmall=1),"% Assist Rate"),style='font-size:125%')),
                                                                      
                                                                      # stat
                                                                      #tags$h2(strong(paste0(thisStats$`AST%`,"%")),style='font-size:85%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(thisStats$`AST%_rank`), " | ", toOrdinal(thisStats$`AST%_pctl`), " %ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # reactive letter grade    
                                                                      # if (thisStats$`AST%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`AST%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`AST%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`AST%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`AST%_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`AST%_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`AST%_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`AST%_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ))
                                                      
                                                    )),
                                           
                                           ###### On Defense
                                           tabPanel(strong("On Defense"),
                                                    fluidRow(
                                                      
                                                      column(12,

                                                             ### Adj Def Eff
                                                             valueBox(width = 14,

                                                                      # header
                                                                      value=tags$p(strong("KenPom Adj. Defensive Eff."),style='font-size:55%'),

                                                                      # stat
                                                                      #tags$h2(strong(paste(format(round(thisStats$AdjDE,1), nsmall=1), "Pts Allowed/100 Poss")), style='font-size:95%'),

                                                                      # %ile
                                                                      subtitle=tags$p(strong(HTML(paste(format(round(thisStats$AdjDE,1), nsmall=1), "Pts Allowed/100 Poss<br/>", 
                                                                                                        toOrdinal(thisStats$RankAdjDE), "in D1 |", toOrdinal(thisStats$AdjDE_pctl), " %ile"))), 
                                                                                             style='font-size:120%'),

                                                                      # # reactive letter grade
                                                                      # if (thisStats$`AdjDE_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$AdjDE_pctl >= 70){icon = icon("b")}
                                                                      # else if (thisStats$AdjDE_pctl >= 55){icon = icon("c")}
                                                                      # else if (thisStats$AdjDE_pctl >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      icon = NULL,

                                                                      # reactive box color
                                                                      if (thisStats$`AdjDE_pctl` >= 85){color='green'}
                                                                      else if (thisStats$AdjDE_pctl >= 70){color='teal'}
                                                                      else if (thisStats$AdjDE_pctl >= 55){color='blue'}
                                                                      else if (thisStats$AdjDE_pctl >= 30){color='orange'}
                                                                      else {color='red'}
                                                             )

                                                      ),
                                                      
                                                      
                                                      column(12,
                                                             
                                                             ### Opponenet TS%
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      tags$p(strong(paste0(format(thisStats$`eFG%_Opp`*100, nsmall=1), "% Opp. Effective Field Goal %"),style='font-size:125%')),
                                                                      
                                                                      # stat      
                                                                      #tags$h2(strong(paste0(thisStats$`TS%_Opp`*100, "%")),style='font-size:85%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(thisStats$`eFG%_Opp_rank`), " | ", toOrdinal(thisStats$`eFG%_Opp_pctl`), " %ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # reactive letter grade
                                                                      # if (thisStats$`TS%_Opp_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`TS%_Opp_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`TS%_Opp_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`TS%_Opp_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`eFG%_Opp_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`eFG%_Opp_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`eFG%_Opp_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`eFG%_Opp_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ))),
                                                    fluidRow(
                                                      column(12,
                                                             
                                                             ### DRB%
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      tags$p(strong(paste0(format(thisStats$`DRB%`, nsmall=1),"% Defensive Rebound Rate"),style='font-size:125%')),
                                                                      
                                                                      # stat
                                                                      #tags$h2(strong(paste0(thisStats$`DRB%`, "%")),style='font-size:85%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(thisStats$`DRB%_rank`), " | ", toOrdinal(thisStats$`DRB%_pctl`), " %ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # reactive letter grade        
                                                                      # if (thisStats$`DRB%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`DRB%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`DRB%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`DRB%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`DRB%_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`DRB%_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`DRB%_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`DRB%_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ))),
                                                  fluidRow(    
                                                      column(12,
                                                             
                                                             ### Forced TOV%
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      tags$p(strong(paste0(format(thisStats$`TOV%_Opp`, nsmall=1),"% Forced Turnover Rate"),style='font-size:125%')),
                                                                      
                                                                      # stat
                                                                      #tags$h2(strong(paste0(thisStats$`TOV%_Opp`, "%")),style='font-size:85%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(thisStats$`TOV%_Opp_rank`), " | ", toOrdinal(thisStats$`TOV%_Opp_pctl`), " %ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade
                                                                      # if (thisStats$`TOV%_Opp_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`TOV%_Opp_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`TOV%_Opp_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`TOV%_Opp_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`TOV%_Opp_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`TOV%_Opp_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`TOV%_Opp_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`TOV%_Opp_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ))),
                                                  
                                                  fluidRow(    
                                                    column(12,
                                                           
                                                           ### Forced TOV%
                                                           valueBox(width=14,
                                                                    
                                                                    # header
                                                                    tags$p(strong(paste0(format(round(this$`FTR_Opp`*100,1), nsmall=1),"% Opp. Free Throw Rate"),style='font-size:125%')),
                                                                    
                                                                    # stat
                                                                    #tags$h2(strong(paste0(thisStats$`TOV%_Opp`, "%")),style='font-size:85%'),
                                                                    
                                                                    # %ile
                                                                    tags$p(strong(paste(toOrdinal(this$`FTR_Opp_rank`), " | ", toOrdinal(this$`FTR_Opp_pctl`), " %ile")),style='font-size:120%'),
                                                                    
                                                                    icon=NULL,
                                                                    # # reactive letter grade
                                                                    # if (thisStats$`TOV%_Opp_pctl` >= 85){icon = icon("a")}
                                                                    # else if (thisStats$`TOV%_Opp_pctl` >= 70){icon = icon("b")}
                                                                    # else if (thisStats$`TOV%_Opp_pctl` >= 55){icon = icon("c")}
                                                                    # else if (thisStats$`TOV%_Opp_pctl` >= 30){icon = icon("d")}
                                                                    # else {icon = icon("f")},
                                                                    
                                                                    # reactive color
                                                                    if (this$`FTR_Opp_pctl` >= 85){color='green'}
                                                                    else if (this$`FTR_Opp_pctl` >= 70){color='teal'}
                                                                    else if (this$`FTR_Opp_pctl` >= 55){color='blue'}
                                                                    else if (this$`FTR_Opp_pctl` >= 30){color='orange'}
                                                                    else {color='red'}
                                                           ))),
                                                  
                                                  fluidRow(    
                                                      column(6,
                                                             
                                                             ### BLK%
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      tags$p(strong(paste0(format(thisStats$`BLK%`, nsmall=1), "% Block Rate"),style='font-size:125%')),
                                                                      
                                                                      # stat
                                                                      #tags$h2(strong(paste0(thisStats$`BLK%`, "%")),style='font-size:85%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(thisStats$`BLK%_rank`), " | ", toOrdinal(thisStats$`BLK%_pctl`), " %ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade
                                                                      # if (thisStats$`BLK%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`BLK%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`BLK%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`BLK%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`BLK%_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`BLK%_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`BLK%_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`BLK%_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             )),
                                                    #),
                                                  #fluidRow(
                                                    column(6,
                                                           
                                                           ### STL%
                                                           valueBox(width=14,
                                                                    
                                                                    # header
                                                                    tags$p(strong(paste0(format(thisStats$`STL%`, nsmall=1),"% Steal Rate"),style='font-size:125%')),
                                                                    
                                                                    # stat
                                                                    #tags$h2(strong(paste0(thisStats$`DRB%`, "%")),style='font-size:85%'),
                                                                    
                                                                    # %ile
                                                                    tags$p(strong(paste(toOrdinal(thisStats$`STL%_rank`), " | ", toOrdinal(thisStats$`STL%_pctl`), " %ile")),style='font-size:120%'),
                                                                    
                                                                    icon=NULL,
                                                                    # reactive letter grade        
                                                                    # if (thisStats$`DRB%_pctl` >= 85){icon = icon("a")}
                                                                    # else if (thisStats$`DRB%_pctl` >= 70){icon = icon("b")}
                                                                    # else if (thisStats$`DRB%_pctl` >= 55){icon = icon("c")}
                                                                    # else if (thisStats$`DRB%_pctl` >= 30){icon = icon("d")}
                                                                    # else {icon = icon("f")},
                                                                    
                                                                    # reactive color
                                                                    if (thisStats$`STL%_pctl` >= 85){color='green'}
                                                                    else if (thisStats$`STL%_pctl` >= 70){color='teal'}
                                                                    else if (thisStats$`STL%_pctl` >= 55){color='blue'}
                                                                    else if (thisStats$`STL%_pctl` >= 30){color='orange'}
                                                                    else {color='red'}
                                                           ))),
                                                  ),
                                           
                                           ###### SCORING EFFICIENCY BY DISTANCE
                                           tabPanel(strong("Shooting Efficiency"),
                                                    fluidRow(
                                                      
                                                      column(6,
                                                             strong("Points per Possession",style='font-size:120%'),
                                                             br(),
                                                             ### three point efficiency
                                                             valueBox(width=14,
                                                                      
                                                                      # header      
                                                                      #tags$p(strong("Twos",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      tags$p(strong(paste0(format(round(this$`PPP`,3), nsmall=1) , " Pts/Poss")), style='font-size:52%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(this$`PPP_rank`), " | ", toOrdinal(this$`PPP_pctl`), "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade
                                                                      # if (thisStats$`2P%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`2P%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`2P%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`2P%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (this$`PPP_pctl` >= 85){color='green'}
                                                                      else if (this$`PPP_pctl` >= 70){color='teal'}
                                                                      else if (this$`PPP_pctl` >= 55){color='blue'}
                                                                      else if (this$`PPP_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             )),
                                                      
                                                      column(6,
                                                             #strong("Defense",style='font-size:167%'),
                                                             br(),
                                                             ### three point efficiency
                                                             valueBox(width=14,
                                                                      
                                                                      # header      
                                                                      #tags$p(strong("Twos",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      tags$p(strong(HTML(paste0(format(round(this$`PPP_Opp`,3), nsmall=1) , " Pts/Poss<br/>Allowed"))), style='font-size:45%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(this$`PPP_opp_rank`), " | ", toOrdinal(this$`PPP_opp_pctl`), "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade
                                                                      # if (thisStats$`2P%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`2P%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`2P%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`2P%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (this$`PPP_opp_pctl` >= 85){color='green'}
                                                                      else if (this$`PPP_opp_pctl` >= 70){color='teal'}
                                                                      else if (this$`PPP_opp_pctl` >= 55){color='blue'}
                                                                      else if (this$`PPP_opp_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             )),
                                                      
                                                      
                                                      column(12,p(strong("Twos"), style = "font-size: 125%;")),
                                                      
                                                      column(6,
                                                             
                                                             ### two point efficiency
                                                             valueBox(width=14,
                                                                      
                                                                      # header      
                                                                      #tags$p(strong("Twos",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      tags$p(strong(paste0(format(thisStats$`2P%`, nsmall=1) , "% 2P")), style='font-size:52%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste( toOrdinal(thisStats$`2P%_pctl`), "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`2P%_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`2P%_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`2P%_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`2P%_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ),
                                                             
                                                             ### two point opp efficiency
                                                             valueBox(width=14,
                                                                      
                                                                      # header      
                                                                      #tags$p(strong("Twos",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      tags$p(strong(HTML(paste0(format(thisStats$`2P%_Opp`*100, nsmall=1) , "% 2P<br>Allowed"))), style='font-size:45%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste( toOrdinal(thisStats$`2P%_opp_pctl`), "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`2P%_opp_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`2P%_opp_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`2P%_opp_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`2P%_opp_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             )
                                                             
                                                             
                                                             
                                                             ),
                                                             
                                                             ### three point efficiency
                                                      column(6,
                                                             
                                                             valueBox(width=14,
                                                                      
                                                                      # header      
                                                                      #tags$p(strong("Twos",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      tags$p(strong(paste0(format(this$`2PAp100`, nsmall=1), " 2PA/100 Poss")), style='font-size:45%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(this$`2PAp100_pctl`), "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade
                                                                      # if (thisStats$`2P%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`2P%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`2P%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`2P%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (this$`2PAp100_pctl` >= 85){color='green'}
                                                                      else if (this$`2PAp100_pctl` >= 70){color='teal'}
                                                                      else if (this$`2PAp100_pctl` >= 55){color='blue'}
                                                                      else if (this$`2PAp100_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ),
                                                             
                                                             valueBox(width=14,
                                                                      
                                                                      # header      
                                                                      #tags$p(strong("Twos",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      tags$p(strong(HTML(paste0(format(this$`2PAp100_Opp`, nsmall=1), " 2PA/100 Poss<br/>Allowed"))), style='font-size:45%'),
                                                                      
                                                                      # %ile
                                                                      tags$p(strong(paste(toOrdinal(this$`2PAp100_Opp_pctl`), "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade
                                                                      # if (thisStats$`2P%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`2P%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`2P%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`2P%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (this$`2PAp100_Opp_pctl` >= 85){color='green'}
                                                                      else if (this$`2PAp100_Opp_pctl` >= 70){color='teal'}
                                                                      else if (this$`2PAp100_Opp_pctl` >= 55){color='blue'}
                                                                      else if (this$`2PAp100_Opp_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ),
                                                      )),
                                                    fluidRow(  
                                                      column(12,p(strong("Threes"), style = "font-size: 125%;")),
                                                      column(6,
                                                             #p(strong("Threes"), style = "font-size: 125%;"),
                                                             # two point efficiency
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      #tags$p(strong("Threes",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      value=tags$p(strong(paste( paste0(format(thisStats$`3P%`, nsmall=1) , "% 3P"))), style='font-size:52%'),
                                                                      
                                                                      # %ile
                                                                      subtitle=tags$p(strong(paste( toOrdinal(thisStats$`3P%_pctl`) , "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,

                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`3P%_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`3P%_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`3P%_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`3P%_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ),
                                                             
                                                             
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      #tags$p(strong("Threes",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      value=tags$p(strong( HTML(paste0(format(thisStats$`3P%_Opp`*100, nsmall=1) , "% 3P<br>Allowed"))), style='font-size:45%'),
                                                                      
                                                                      # %ile
                                                                      subtitle=tags$p(strong(paste( toOrdinal(thisStats$`3P%_opp_pctl`) , "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      
                                                                      # reactive color
                                                                      if (thisStats$`3P%_opp_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`3P%_opp_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`3P%_opp_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`3P%_opp_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ),
                                                             
                                                             
                                                             
                                                             ),
                                                    column(6,         
                                                             # two point efficiency
                                                             valueBox(width=14,
                                                                      
                                                                      # header
                                                                      #tags$p(strong("Threes",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      value=tags$p(strong(paste( format(this$`3PAp100`, nsmall=1), "3PA/100 Poss")), style='font-size:45%'),
                                                                      
                                                                      # %ile
                                                                      subtitle=tags$p(strong(paste(toOrdinal(this$`3PAp100_pctl`), "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade        
                                                                      # if (thisStats$`3P%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`3P%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`3P%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`3P%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # reactive color
                                                                      if (this$`3PAp100_pctl` >= 85){color='green'}
                                                                      else if (this$`3PAp100_pctl` >= 70){color='teal'}
                                                                      else if (this$`3PAp100_pctl` >= 55){color='blue'}
                                                                      else if (this$`3PAp100_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ),
                                                           
                                                           valueBox(width=14,
                                                                    
                                                                    # header
                                                                    #tags$p(strong("Threes",style='font-size:110%')),
                                                                    
                                                                    # stat HTML(paste0(This.Week$Away[1],""," vs ","<br/>", This.Week$Home[1]))
                                                                    value=tags$p(strong(HTML(paste( format(this$`3PAp100_Opp`, nsmall=1), "3PA/100 Poss<br/>Allowed"))), style='font-size:45%'),
                                                                    
                                                                    # %ile
                                                                    subtitle=tags$p(strong(paste(toOrdinal(this$`3PAp100_Opp_pctl`), "%ile")),style='font-size:120%'),
                                                                    
                                                                    icon=NULL,
                                                                    # # reactive letter grade        
                                                                    # if (thisStats$`3P%_pctl` >= 85){icon = icon("a")}
                                                                    # else if (thisStats$`3P%_pctl` >= 70){icon = icon("b")}
                                                                    # else if (thisStats$`3P%_pctl` >= 55){icon = icon("c")}
                                                                    # else if (thisStats$`3P%_pctl` >= 30){icon = icon("d")}
                                                                    # else {icon = icon("f")},
                                                                    
                                                                    # reactive color
                                                                    if (this$`3PAp100_Opp_pctl` >= 85){color='green'}
                                                                    else if (this$`3PAp100_Opp_pctl` >= 70){color='teal'}
                                                                    else if (this$`3PAp100_Opp_pctl` >= 55){color='blue'}
                                                                    else if (this$`3PAp100_Opp_pctl` >= 30){color='orange'}
                                                                    else {color='red'}
                                                           )
                                                           
                                                           )),
                                                  fluidRow(   
                                                    column(12, p(strong("Free Throws"), style = "font-size: 125%;")),
                                                      column(6,
                                                             
                                                             valueBox(width=14,
                                                                      
                                                                      # header      
                                                                      #tags$p(strong("Free Throws",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      value=tags$p(strong(paste0(format(thisStats$`FT%`, nsmall=1) , "% FT")), style='font-size:52%'),
                                                                      
                                                                      # %ile
                                                                      subtitle=tags$p(strong(paste(toOrdinal(thisStats$`FT%_pctl`) , "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade
                                                                      # if (thisStats$`FT%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`FT%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`FT%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`FT%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # rective color
                                                                      if (thisStats$`FT%_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`FT%_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`FT%_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`FT%_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ),
                                                             
                                                             valueBox(width=14,
                                                                      
                                                                      # header      
                                                                      #tags$p(strong("Free Throws",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      value=tags$p(strong(HTML(paste0(format(thisStats$`FT%_Opp`*100, nsmall=1) , "% FT<br>Allowed"))), style='font-size:45%'),
                                                                      
                                                                      # %ile
                                                                      subtitle=tags$p(strong(paste(toOrdinal(thisStats$`FT%_opp_pctl`) , "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade
                                                                      # if (thisStats$`FT%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`FT%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`FT%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`FT%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # rective color
                                                                      if (thisStats$`FT%_opp_pctl` >= 85){color='green'}
                                                                      else if (thisStats$`FT%_opp_pctl` >= 70){color='teal'}
                                                                      else if (thisStats$`FT%_opp_pctl` >= 55){color='blue'}
                                                                      else if (thisStats$`FT%_opp_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             )
                                                             
                                                             ),
                                                        column(6,     
                                                             valueBox(width=14,
                                                                      
                                                                      # header      
                                                                      #tags$p(strong("Free Throws",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      value=tags$p(strong(paste(format(this$`FTAp100`, nsmall=1), "FTA/100 Poss")), style='font-size:45%'),
                                                                      
                                                                      # %ile
                                                                      subtitle=tags$p(strong(paste(toOrdinal(this$`FTAp100_pctl`) , "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade
                                                                      # if (thisStats$`FT%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`FT%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`FT%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`FT%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # rective color
                                                                      if (this$`FTAp100_pctl` >= 85){color='green'}
                                                                      else if (this$`FTAp100_pctl` >= 70){color='teal'}
                                                                      else if (this$`FTAp100_pctl` >= 55){color='blue'}
                                                                      else if (this$`FTAp100_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             ),
                                                             
                                                             valueBox(width=14,
                                                                      
                                                                      # header      
                                                                      #tags$p(strong("Free Throws",style='font-size:110%')),
                                                                      
                                                                      # stat
                                                                      value=tags$p(strong(HTML(paste(format(this$`FTAp100_Opp`, nsmall=1), "FTA/100 Poss<br/>Allowed"))), style='font-size:45%'),
                                                                      
                                                                      # %ile
                                                                      subtitle=tags$p(strong(paste(toOrdinal(this$`FTAp100_Opp_pctl`) , "%ile")),style='font-size:120%'),
                                                                      
                                                                      icon=NULL,
                                                                      # # reactive letter grade
                                                                      # if (thisStats$`FT%_pctl` >= 85){icon = icon("a")}
                                                                      # else if (thisStats$`FT%_pctl` >= 70){icon = icon("b")}
                                                                      # else if (thisStats$`FT%_pctl` >= 55){icon = icon("c")}
                                                                      # else if (thisStats$`FT%_pctl` >= 30){icon = icon("d")}
                                                                      # else {icon = icon("f")},
                                                                      
                                                                      # rective color
                                                                      if (this$`FTAp100_Opp_pctl` >= 85){color='green'}
                                                                      else if (this$`FTAp100_Opp_pctl` >= 70){color='teal'}
                                                                      else if (this$`FTAp100_Opp_pctl` >= 55){color='blue'}
                                                                      else if (this$`FTAp100_Opp_pctl` >= 30){color='orange'}
                                                                      else {color='red'}
                                                             )
                                                             
                                                             
                                                             
                                                             ))
                                                    ))
                                    ))
                             
                             #),
                    ),
                    
                    # ### bracketology box
                    # valueBox(width=12, color='light-blue',
                    #          tags$h2(strong("Dash Bracketology"), style="font-size:225%"), 
                    #          
                    #          # format projected seed
                    #          if (nchar(as.vector(thisBracket$Projected.Seed))>2){
                    #            tags$h2(strong(paste(thisBracket$Projected.Seed)), style='font-size:200%')
                    #          } else {
                    #            tags$h2(strong(paste0(paste(thisBracket$Projected.Seed, "Seed"), thisBracket$Qual.Games)), style='font-size:200%')
                    #          })),
                    # 
                    # ### TOPS box
                    # valueBox(width=12,
                    #          color='purple',
                    #          
                    #          # header
                    #          tags$h2(strong(paste(paste("Tournament","Outcome","Pred","Score:", sep='\n'), this$TOPS)), style='font-size:225%'),
                    #          
                    #          # rank
                    #          subtitle=tags$h2(strong(toOrdinal(as.numeric(this$Rank)), style='font-size:200%'))
                    # )
                    # 
                    # )),
                    
                    
                    # fluidRow(
                    #   column(8, 
                    #          
                    #          ### bracketology box
                    #          valueBox(width=12, color='light-blue',
                    #                   tags$h2(strong("Dash Bracketology"), style="font-size:225%"), 
                    #                   
                    #                   # format projected seed
                    #                   if (nchar(as.vector(thisBracket$Projected.Seed))>2){
                    #                     tags$h2(strong(paste(thisBracket$Projected.Seed)), style='font-size:200%')
                    #                   } else {
                    #                     tags$h2(strong(paste0(paste(thisBracket$Projected.Seed, "Seed"), thisBracket$Qual.Games)), style='font-size:200%')
                    #                   }))
                    #   
                    #   
                    # ),
                    # fluidRow(
                    #   column(8, 
                    #          ### TOPS box
                    #          valueBox(width=12,
                    #                   color='purple',
                    # 
                    #                   # header
                    #                   tags$h2(strong(paste(paste("Tournament","Outcome","Pred","Score:", sep='\n'), this$TOPS)), style='font-size:225%'),
                    # 
                    #                   # rank
                    #                   subtitle=tags$h2(strong(toOrdinal(as.numeric(this$Rank)), style='font-size:200%'))
                    #          )
                    # )
                    # ) 
                    # ### TOPS box
                    # valueBox(width=12,
                    #          color='purple',
                    #          
                    #          # header
                    #          tags$h2(strong(paste(paste("Tournament","Outcome","Pred","Score", sep='\n'), this$TOPS)), style='font-size:200%'),
                    #          
                    #          # rank
                    #          subtitle=tags$h2(strong(toOrdinal(as.numeric(this$Rank)), style='font-size:380%'))
                    # )
    )#)
  })
  
  output$`teamIdentity-overallRanks` = renderUI({
    
    TeamsFull=TeamsFull %>% arrange(desc(`ESPN BPI`))
    TeamsFull$`ESPN BPI Rank` = seq(1, length(TeamsFull$PPG), 1)
    
    #TeamsFull=TeamsFull %>% arrange(desc(`Shot Quality Adj.`))
    TeamsFull$`Shot Quality Rank` = seq(1, length(TeamsFull$PPG), 1)
    
    TeamsFull=TeamsFull %>% arrange(desc(`T-Rank BARTHAG`))
    TeamsFull$`BartTorvik BARTHAG Rank` = seq(1, length(TeamsFull$PPG), 1)
    
    Teams = Teams %>% 
      group_by(Conference) %>% 
      mutate(Conf.Rank = rank(-ETW))
    
    TeamsFull = TeamsFull %>% 
      group_by(Conference) %>% 
      mutate(Conf.BPI.Rank = rank(-`ESPN BPI`),
             #Conf.SQ.Rank = rank(-`Shot Quality Adj.`),
             Conf.KP.Rank = rank(-`KenPom AdjEM`),
             Conf.BT.Rank = rank(-`T-Rank BARTHAG`))
    
    this = Teams %>% filter(School == input$`teamIdentity-teamSearchBox`)
    thisFull = TeamsFull %>% filter(School == input$`teamIdentity-teamSearchBox`)
    thisStats = TeamStats %>% filter(Team == input$`teamIdentity-teamSearchBox`)
    thisBracket = Bracketology %>% filter(gsub("\\*", "", team) == input$`teamIdentity-teamSearchBox`)
    thisBracket$Projected.Seed = thisBracket$seed
    
    ratingSystem = input$`mainPage-teamEff2`
    
    if (ratingSystem=="Estimated Tournament Wins"){
      name = paste("Estimated","Tournament","Wins:", sep='\n')
      stat = this$ETW
      rank = this$Rank
      confRank = this$Conf.Rank
      conf = this$Conference
    } else if (ratingSystem=="ESPN BPI"){
      name = paste("ESPN","BPI:", sep='\n')
      stat = thisFull$`ESPN BPI`
      rank = thisFull$`ESPN BPI Rank`
      confRank = thisFull$Conf.BPI.Rank
      conf = thisFull$Conference
    } 
    # else if (ratingSystem=="Shot Quality"){
    #   name = paste("Shot","Quality:", sep='\n')
    #   stat = thisFull$`Shot Quality Adj.`
    #   rank = thisFull$`Shot Quality Rank`
    #   confRank = thisFull$Conf.SQ.Rank
    #   conf = thisFull$Conference
    #} 
    else if (ratingSystem=="KenPom"){
      name = paste("KenPom AdjEM:", sep='\n')
      stat = thisFull$`KenPom AdjEM`
      rank = thisFull$`KenPom Rank`
      confRank = thisFull$Conf.KP.Rank
      conf = thisFull$Conference
    } else if (ratingSystem=="BartTorvik"){
      name = paste("BartTorvik Barthag:", sep='\n')
      stat = thisFull$`T-Rank BARTHAG`
      rank = thisFull$`BartTorvik BARTHAG Rank`
      confRank = thisFull$Conf.BT.Rank
      conf = thisFull$Conference
    }
    
    
    
    fluidRow(
      
      column(7,
             ### TOPS box
             valueBox(width=14,
                      color='purple',
                      
                      # header
                      tags$h2(strong(paste(name, round(stat, 3))), style='font-size:145%'),
                      
                      # rank
                      subtitle=tags$h2(strong(paste(toOrdinal(as.numeric(rank)), 
                                                    "in D1 |",
                                                    toOrdinal(as.numeric(confRank)),
                                                    "in",
                                                    conf), style='font-size:85%'))
                      )
             ),
      
      column(4,
             ### bracketology box
             valueBox(width=14, color='light-blue',
                      tags$h2(strong("Dash Bracketology"), style="font-size:133%"),

                      # format projected seed
                      if (nchar(as.vector(thisBracket$Projected.Seed))>2){
                        subtitle=tags$h2(strong(paste(thisBracket$Projected.Seed), style='font-size:80%'))
                      } else {
                        subtitle=tags$h2(strong(paste0(paste(thisBracket$Projected.Seed, "Seed")), style='font-size:80%'))
                      })
             )
    )
    
  })
  
  
  
  
  

  ### Key Roster Characteristics
  
  
  ###### floor spacing
  
  output$`teamIdentity-rosterSpacing` <- DT::renderDataTable({
    
    
    # get team and relevant stats
    this = Players %>% filter(School == input$`teamIdentity-teamSearchBox`)
    this=this %>% dplyr::select(Player, MPG,`3PA`, `3P%`, `2PA`, `3par_pctl`, `3p%_pctl`)
    this=this[,2:length(colnames(this))]
    
    # # calculate formula for ability score
    # this=this %>% mutate(#`3PA/40 Min` = round((`3PA`/MPG)*40, 1),
    #                      `3PA/40 Min` = round((`3PA`/(`3PA`+`2PA`))*100, 4),
    #                      `3PA/min` = round((`3PA`/MPG), 5),
    #                      `3PM` = `3PA`*(`3P%`/100),
    #                      `3PM/min` = (`3PM`/MPG)*40,
    #                      `3pm.adj` = `3PA`/1.5 * (`3P%`),
    #                      `3PM/min.adj` = (`3pm.adj`/MPG),
    #                      Grade1 = round(((`3PA/min`/.29) + (`3P%`/10-.34)/1.75) / 2.13, 4),
    #                      `3P%` = round(`3P%`, 1),
    #                      Grade2 = ((`3PA/40 Min`) + (`3P%`*1.25))/2)#comeback
    
    this = this %>% 
      mutate(`3PAr` = round((`3PA`/(`3PA`+`2PA`))*100, 4),
             grade1 = round(((`3par_pctl`*1.667) + (`3p%_pctl`))/2.667),1) %>% 
      select(Player, grade1, `3PAr`, `3par_pctl`, `3P%`, `3p%_pctl`) %>% 
      dplyr::rename(`3P Att. Rate` = `3PAr`)
    
    this = this %>% 
      mutate(Grade = case_when(
        `3P Att. Rate` < 10 ~ 'F',
        `3P Att. Rate` < 15 & `3P%` < 37 ~ 'F',
        `3P Att. Rate` < 15 ~ 'D-',
        
        grade1 >= 90 ~ 'A++',
        grade1 >= 83 ~ 'A+',
        grade1 >= 76 ~ 'A',
        grade1 >= 68 ~ 'A-',
        grade1 >= 64 ~ 'B+',
        grade1 >= 60 ~ 'B',
        grade1 >= 55 ~ 'B-',
        grade1 >= 50 ~ 'C+',
        grade1 >= 45 ~ 'C',
        grade1 >= 40 ~ 'C-',
        grade1 >= 35 ~ 'D+',
        grade1 >= 30 ~ 'D',
        grade1 >= 25~ 'D-',
        
        
        TRUE ~ 'F')) %>% 
      select(Player, Grade, `3P Att. Rate`, `3P%`)


    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 25,lengthChange = FALSE, dom = 't',ordering=F)) %>% 
      
      # DT formatting
      formatStyle(fontsize = 40,fontWeight = 'bold',columns = c('Player', 'Grade')) %>% 
      formatStyle(c('3P Att. Rate'),backgroundColor = plcl.threemin) %>%
      formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
      formatStyle(c('Grade'),backgroundColor = grade.colors) %>% 
      formatStyle(c('Grade'),backgroundColor = grade.colors2) %>% 
      formatStyle(c(1,2),`border-right` = '4px solid black')%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1)# %>% 
      #formatRound("3PM/min", digits = 4)
  })
  
  
  ###### ball control ######
  output$`teamIdentity-rosterBallControl` <- DT::renderDataTable({
    
    
    # get team and relevant stats
    this = Players %>% filter(School == input$`teamIdentity-teamSearchBox`)
    this=this %>% dplyr::select(Player,PPR,`AST%`,`TOV%`)
    this=this[,2:length(colnames(this))]
    
    # calculate formula for ability score
    this=this %>% mutate(Grade1 = ((PPR + (`AST%`)/4-(`TOV%`)/4)/5),
                         `Pure Point Rtg`=PPR,
                         `AST%` = round(`AST%`, 1),
                         `TOV%` = round(`TOV%`, 1))
    
    # assign letter grade
    this=this %>% mutate(Grade = case_when(
      #Grade1 >3  ~ "A+",
      Grade1 >= 1.4 ~ "A+",
      Grade1 >= 1.05 ~ "A",
      Grade1 >= 0.87 ~ "A-",
      Grade1 >= 0.675 ~ "B+",
      Grade1 >= 0.5 ~ "B",
      Grade1 >= 0.25 ~ "B-",
      Grade1 >= 0 ~ "C+",
      Grade1 >= -0.325 ~ "C",
      Grade1 >= -0.55 ~ "C-",
      Grade1 >= -0.75 ~ "D",
      Grade1 >= -1 ~ "D",
      Grade1 >= -1.25 ~ "D-",
      TRUE ~ "F")) %>% 
      
      dplyr::select(Player,Grade,`Pure Point Rtg`,`AST%`,`TOV%`)
    
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 25,lengthChange = FALSE, dom = 't',ordering=F))%>% 
      
      # DT formatting
      formatStyle(fontsize = 40,fontWeight = 'bold',columns = c('Player', 'Grade'))%>% 
      formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>%
      formatStyle(c('TOV%'),backgroundColor = plcl.tovpct) %>%
      formatStyle(c('Pure Point Rtg'),backgroundColor = plcl.ppr) %>%
      formatStyle(c('Grade'),backgroundColor = grade.colors)%>% 
      formatStyle(c(1,2),`border-right` = '4px solid black')#%>% 
      #formatRound(which(sapply(this,is.numeric)), digits = 1)
  })
  
  
  ###### interior presence ######
  
  output$`teamIdentity-rosterInteriorPresence` <- DT::renderDataTable({
    
    # get team and relevant stats
    this = Players %>% filter(School == input$`teamIdentity-teamSearchBox`)
    this=this %>% dplyr::select(Player,`DRB%`, `ORB%`, `BLK%`)
    this=this[,2:length(colnames(this))]
    
    # calculate formula for ability score
    this=this %>% mutate(Grade1 = `DRB%`/1.75 + `ORB%`*4 + `BLK%`*6,
                         `ORB%` = round(`ORB%`, 1),
                         `DRB%` = round(`DRB%`, 1),
                         `BLK%` = round(`BLK%`, 1))
    
    # assign letter grade
    this=this %>% mutate(Grade = case_when(
      #Grade1 >3  ~ "A+",
      Grade1 >= 75 ~ "A+",
      Grade1 >= 65~ "A",
      Grade1 >= 58 ~ "A-",
      Grade1 >= 50 ~ "B+",
      #Grade1 >= 58 ~ "B",
      Grade1 >= 46 ~ "B-",
      Grade1 >= 42 ~ "C+",
      #Grade1 >= 48~ "C-",
      Grade1 >= 38 ~ "C-",
      Grade1 >= 34 ~ "D",
      #Grade1 >= -3 ~ "D",
      #Grade1 >= -3.5 ~ "D-",
      TRUE ~ "F")) %>% 
      dplyr::select(Player,Grade,`DRB%`, `ORB%`, `BLK%`)
      
    
    ### DT
    DT::datatable(this, 
                  
                  # DT options
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                 scrollX = '400px',pageLength = 25,lengthChange = FALSE, dom = 't',ordering=F))%>% 
      
      # DT formatting
      formatStyle(fontsize = 40,fontWeight = 'bold',columns = c('Player', 'Grade')) %>% 
      formatStyle(c('ORB%'),backgroundColor = plcl.orbpct) %>%
      formatStyle(c('BLK%'),backgroundColor = plcl.blkpct) %>%
      formatStyle(c('DRB%'),backgroundColor = plcl.drbpct) %>%
      formatStyle(c('Grade'),backgroundColor = grade.colors)%>% 
      formatStyle(c(1,2),`border-right` = '4px solid black')%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1)
  })
  
  
  ###### Roster stats ######
  
  output$`teamIdentity-rosterStats` <- DT::renderDataTable({
    
    # get school
    this = Players %>% filter(School == input$`teamIdentity-teamSearchBox`)
    this=this %>% dplyr::select(-`2PM`, -`3PM`, -PPR)
    
    this$`TS%` = round(this$`TS%`,1)
    this$`eFG%` = round(this$`eFG%`,1)
    this$`FG%` = round(this$`FG%`,1)
    this$`2P%` = round(this$`2P%`,1)
    this$`3P%` = round(this$`3P%`,1)
    this$`FT%` = round(this$`FT%`,1)
    this$`2PA` = round(this$`2PA`,1)
    this$`3PA` = round(this$`3PA`,1)
    
    this = this %>% 
      dplyr::select(Player, `BASIC`, Class,
             GP, MPG, `USG%`, SOS,
             PPG, `TS%`, `eFG%`, `FG%`,
             `3PA`, `3P%`, `2PA`, `2P%`, `FTA`, `FT%`,
             APG, `AST%`, `TOV`, `TOV%`,
             ORB, `ORB%`, DRB, `DRB%`,
             SPG, `STL%`, BPG, `BLK%`,
             `Net Rating`, ORtg, DRtg, PER,
             Natl.Rank, Conf.Rank, Team.Rank, School)
    
    ### DT
    DT::datatable(this[1:10,],#[, input$show_vars, drop = FALSE], 
                  
                  # DT options
                  extensions = "FixedColumns",
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),  
                                                   list(width = '125px', targets = c(2)),
                                                   list(width = '125px', targets = c(length(colnames(this))))
                                                   #list(width = '100px', targets = c(3))
                                                   ),
                                 scrollX = '400px', autoWidth = T,dom = 't',ordering=F,
                                 fixedColumns = list(leftColumns = 3), 
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('body').css({'font-family': 'Trebuchet MS'});",
                                   "}")))%>%
      
      # DT formatting
      
      formatStyle(c('BASIC'),backgroundColor = plcl.pcs) %>% #styleInterval(seq(-7,10,0.1), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b', '#c5fc8b', '#a3fb45','#6fd404'))(length(seq(-7,10,0.1))+1))) %>% 
      formatStyle(c('USG%'),backgroundColor = plcl.usg) %>%
      formatStyle(c('SOS'),backgroundColor = plcl.sos) %>%
      
      formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
      formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
      formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
      formatStyle(c('FG%'),backgroundColor = plcl.fg) %>%
      
      formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
      formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
      formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
      formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
      formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
      formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
      
      formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
      formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>%
      formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
      formatStyle(c('TOV%'),backgroundColor = plcl.tovpct) %>%
      
      formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
      formatStyle(c('ORB%'),backgroundColor = plcl.orbpct) %>%
      formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
      formatStyle(c('DRB%'),backgroundColor = plcl.drbpct) %>%
      
      formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
      formatStyle(c('STL%'),backgroundColor = plcl.stlpct) %>%
      formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
      formatStyle(c('BLK%'),backgroundColor = plcl.blkpct) %>%
      
      formatStyle(c('Net Rating'),backgroundColor = plcl.netrating) %>%
      formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
      formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
      formatStyle(c('PER'),backgroundColor = plcl.per) %>%
    
      formatStyle(c(1,2,6,8,12,18,22,26,30,34),`border-left` = '4px solid black')%>% 
      formatStyle(c(2),`border-right` = '4px solid black')%>% 
      formatStyle(columns = c(1, length(colnames(this))), width='250px') %>% 
      formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('Player', 'BASIC'))%>% 
      formatRound(which(sapply(this,is.numeric)), digits = 1)%>% 
      formatRound(columns=c("BASIC"), digits = 1) %>% 
      formatRound(columns=c("GP", 'Natl.Rank', 'Conf.Rank', 'Team.Rank'), digits = 0)
    
  })
  
  ### AdjEM vs TOPS
  output$`teamIdentity-usagePie` <- renderPlot({
    
    # get data
    this = Players %>% filter(School == input$`teamIdentity-teamSearchBox`)
    this=this %>% dplyr::select(-`2PM`, -`3PM`, -PPR)
    
    this = this %>% 
      mutate(`Relative USG%` = `USG%`/sum(this$`USG%`),
             `USG% * MPG` = `USG%`*MPG) %>% 
      arrange(desc(`USG% * MPG`))
    
    this = this[1:8,]
    this = this %>% 
      arrange(desc(`USG%` * `MPG`)) %>% 
      mutate(util=rank(-(`USG%` * `MPG`)))
    #this$util = seq(1, length(this), 1)
    # apply filters
    
    
    # Get the positions
    this <- this %>% 
      mutate(csum = rev(cumsum(rev(-(`USG% * MPG`)))), 
             pos = -(`USG% * MPG`)/2 + lead(csum, 1),
             pos = if_else(is.na(pos), -(`USG% * MPG`)/2, pos))
    
    # scatter
    p <- ggplot(this, aes(x = "", y=-`USG% * MPG`, fill = reorder(Player, -`USG% * MPG`))) +
      geom_bar(stat = "identity", color = "black") +
      coord_polar(theta = "y")+
      scale_fill_brewer(palette = "Pastel1") +
      geom_label_repel(data = this,
                       aes(y = pos, label = paste0(util, ": ", Player)),
                       size = 4.5, nudge_x = 0.6667, show.legend = FALSE) +
      theme_void() +
      ggtitle("Load Among 8 Most Utilized Players")+
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank(),
            legend.position = "none",
            plot.title = element_text(face = "bold", size=24, hjust = 0.5))
    
    p

  })
  
  last10$RankAdjEM.ordinal = toOrdinal(last10$RankAdjEM)
  last10$RankAdjOE.ordinal = toOrdinal(last10$RankAdjOE)
  last10$RankAdjDE.ordinal = toOrdinal(last10$RankAdjDE)
  
  ### AdjEM Historic Time Series
  output$`teamIdentity-kpAdjEM-timeSeries` <- renderPlotly({
    
    this.kp = last10 %>% filter(Team == input$`teamIdentity-teamSearchBox`)
    
    p <- ggplot() + 
      
      # distribution of all D1 teams underlayed
      geom_boxplot(data = last10, aes(x=Season, y=AdjEM)) + 
      
      # create line connecting KP AdjEM year by year
      geom_line(data=this.kp, 
                aes(x = Season, y = AdjEM, group=1)) + 
      
      # add points to each year adjEM value
      geom_point(data=this.kp, 
                 aes(x = Season, y = AdjEM, group=1, text = paste("Rank:", RankAdjEM)),
                 shape = 21, fill="Purple", size=7)  +
      
      geom_text(data=this.kp, 
                aes(x=Season, y=AdjEM, label=RankAdjEM), vjust=1.2, fontface='bold', size=3.67, family='Aptos Narrow', color='white',
                ) + 
      
      
    
    
    # formatting
    #theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+
      theme(axis.text=element_text(size=12, family = 'Trebuchet MS'),
            text=element_text(family = 'Trebuchet MS'))
    
    ggplotly(p)%>%
      
      ### extra plotly formatting
      layout(#title = list(text = paste0('Boxplots are of all D1 Teams')),
             xaxis = list(title=paste0("<sup><span style='font-size:16px;'>", "Tournament Year", "</span></sup>")),
             yaxis = list(title=paste0("<sup><span style='font-size:16px;'>", "KenPom Adj. EM", "</span></sup>")))
    
  })
  
  ### Adj Off Eff Historic Time Series
  output$`teamIdentity-kpAdjOff-timeSeries` <- renderPlotly({
    
    this.kpo = last10 %>% filter(Team == input$`teamIdentity-teamSearchBox`)
    
    p <- ggplot() + 
      
      # distribution of all D1 teams underlayed
      geom_boxplot(data = last10, aes(x=Season, y=AdjOE, label=toOrdinal(RankAdjOE))) + 
      
      # create line connecting KP AdjEM year by year
      geom_line(data=this.kpo, 
                aes(x = Season, y = AdjOE, group=1)) + 
      
      # add points to each year adjEM value
      geom_point(data=this.kpo,
                 aes(x = Season, y = AdjOE, group=1, text = paste("Rank:", RankAdjOE)),
                 shape = 21, fill="Green", size=7) +
      
      geom_text(data=this.kpo, 
                 aes(x=Season, y=AdjOE, label=RankAdjOE), vjust=1.2, fontface='bold', size=3.67, family='Aptos Narrow', angle=45) + 
    
    # formatting
    #theme_gdocs() +
    ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+
      theme(axis.text=element_text(size=12, family = 'Trebuchet MS'),
            text=element_text(family = 'Trebuchet MS'))
    
    ggplotly(p)%>%
      
      ### extra plotly formatting
      layout(#title = list(text = paste0('Boxplots are of all D1 Teams')),
             xaxis = list(title=paste0("<sup><span style='font-size:16px;'>", "Tournament Year", "</span></sup>")),
             yaxis = list(title=paste0("<sup><span style='font-size:16px;'>", "KenPom Adj. Offensive Efficiency", "</span></sup>")))
  })
  
  ### Adj Def Eff Historic Time Series
  output$`teamIdentity-kpAdjDef-timeSeries` <- renderPlotly({
    
    this.kpd = last10 %>% filter(Team == input$`teamIdentity-teamSearchBox`)
    
    p <- ggplot() + 
      
      # distribution of all D1 teams underlayed
      geom_boxplot(data = last10, aes(x=Season, y=-AdjDE)) + 
      
      # create line connecting KP AdjEM year by year
      geom_line(data=this.kpd, 
                aes(x = Season, y = -AdjDE, group=1)) + 
      
      # add points to each year adjEM value
      geom_point(data=this.kpd, 
                 aes(x = Season, y = -AdjDE, group=1, text = paste("Rank:", RankAdjDE)),
                 shape = 21, fill="Red", size=8) +
      
      geom_text(data=this.kpd, 
                aes(x=Season, y= -AdjDE, label=RankAdjDE), vjust=1.2, fontface='bold', size=3.67, family='Aptos Narrow', angle=45, color='white') + 
      
      # formatting
      #theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+
      theme(axis.text=element_text(size=12, family = 'Trebuchet MS'),
            text=element_text(family = 'Trebuchet MS'))
    
    ggplotly(p)%>%
      
      ### extra plotly formatting
      layout(#title = list(text = paste0('Boxplots are of all D1 Teams')),
             xaxis = list(title=paste0("<sup><span style='font-size:16px;'>", "Tournament Year", "</span></sup>")),
             yaxis = list(title=paste0("<sup><span style='font-size:16px;'>", "KenPom Adj. Defensive Efficiency", "</span></sup>")))
  })
  
  ### Adj Tempo Historic Time Series
  output$`teamIdentity-kpAdjTempo-timeSeries` <- renderPlotly({
    
    this.kpt = last10 %>% filter(Team == input$`teamIdentity-teamSearchBox`)
    
    p <- ggplot() + 
      
      # distribution of all D1 teams underlayed
      geom_boxplot(data = last10, aes(x=Season, y=AdjTempo)) + 
      
      # create line connecting KP AdjEM year by yearf
      geom_line(data=this.kpt, 
                aes(x = Season, y = AdjTempo, group=1)) + 
      
      # add points to each year adjEM value
      geom_point(data=this.kpt, 
                 aes(x = Season, y = AdjTempo, group=1, text = paste("Rank:", RankAdjTempo)),
                 shape = 21, fill="Orange", size=7)  +
      
      geom_text(data=this.kpt, 
                aes(x=Season, y= AdjTempo, label=RankAdjTempo), vjust=1.2, fontface='bold', size=3.67, family='Aptos Narrow', angle=45, color='white') + 
      
      # formatting
      #theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+
      theme(axis.text=element_text(size=12, family = 'Trebuchet MS'),
            text=element_text(family = 'Trebuchet MS'))
    
    ggplotly(p)%>%
      
      ### extra plotly formatting
      layout(#title = list(text = paste0('Boxplots are of all D1 Teams')),
             xaxis = list(title=paste0("<sup><span style='font-size:16px;'>", "Tournament Year", "</span></sup>")),
             yaxis = list(title=paste0("<sup><span style='font-size:16px;'>", "KenPom Adj. Tempo", "</span></sup>")))
  })
  
  output$`originalMetrics-etw` <- DT::renderDataTable({
    
    
    
    if (input$`etwPage-statsCheckbox`){
      
      etwCurr = etwCurr %>% 
        dplyr::select(Team, Conference, `ETW Rank`, ETW,
               SOS, AdjEM, `TS% Margin`, `OR% Margin`, `TOV Margin/100 Trips`,
               `ETW Offense Rank`, `ETW Offense`, AdjOE, `TS%`, `OR%`, `TOV/100 Trips`,
               `ETW Defense Rank`, `ETW Defense`, AdjDE, `TS% Allowed`, `OR% Allowed`, `Forced TOV/100 Trips`)
      
      # apply filters
      c = input$`etwPage-filterConf`
      t = input$`etwPage-filterSchool`
      #p = input$`pcsPage-filterPlayer`
      
      if (c != "All") {etwCurr = etwCurr %>% filter(Conference == c)}
      if (t != "All") {etwCurr = etwCurr %>% filter(Team == t)}
      
      #etwCurr$Year = as.character(etwCurr$Year)
      
      # rename variables
      #etwCurr = etwCurr %>% 
      #  mutate()
      
      # order / select variables
      
      etwCurr = etwCurr %>% arrange(desc(ETW))
      
      # if (input$`etwPage-finalFourCheckbox`){
      #   etwCurr = etwCurr %>% filter(Team %in% c("UConn", 'Alabama', 'Purdue', 'NC State'))
      # }
      
      DT::datatable(etwCurr,
                    rownames = FALSE,
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(width = '18%', targets = c(0,1))),
                                   scrollX = '400px', fixedColumns = list(leftColumns = 4), pageLength=10))%>%
        
        formatStyle(c(4, 10, 16),`border-left` = '4px solid black')%>% 
        formatStyle(c(4),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(etwCurr,is.numeric)), digits = 2) %>% 
        formatRound(c("TS%", "TS% Allowed", "TS% Margin"), digits = 1) %>% 
        formatRound(c('ETW', 'ETW Offense', 'ETW Defense'), digits = 3) %>% 
        formatRound(c('ETW Rank', 'ETW Offense Rank', 'ETW Defense Rank'), digits = 0) %>% 
        formatRound(c('SOS'), digits = 1) %>% 
        
        formatStyle(c('ETW'),backgroundColor = tmcl.tops) %>% 
        formatStyle(c('ETW Offense'),backgroundColor = tmcl.tops.off) %>%
        formatStyle(c('ETW Defense'),backgroundColor = tmcl.tops.off) %>%
        
        formatStyle(c('AdjEM'),backgroundColor = tmcl.kp) %>% 
        formatStyle(c('AdjOE'),backgroundColor = tmcl.kpo) %>%
        formatStyle(c('AdjDE'),backgroundColor = tmcl.kpd) %>%
        
        formatStyle(c('SOS'),backgroundColor = tmcl.sos) %>%
        
        formatStyle(c('TS% Margin'),backgroundColor = etwcl.ts) %>% 
        formatStyle(c('TS%'),backgroundColor = etwcl.ts.off) %>%
        formatStyle(c('TS% Allowed'),backgroundColor = etwcl.ts.def) %>%
        
        formatStyle(c('OR% Margin'),backgroundColor = etwcl.orb) %>% 
        formatStyle(c('OR%'),backgroundColor = etwcl.orb.off) %>%
        formatStyle(c('OR% Allowed'),backgroundColor = etwcl.orb.def) %>%
        
        formatStyle(c('TOV Margin/100 Trips'),backgroundColor = etwcl.tov) %>% 
        formatStyle(c('TOV/100 Trips'),backgroundColor = etwcl.tov.off) %>%
        formatStyle(c('Forced TOV/100 Trips'),backgroundColor = etwcl.tov.def) %>% 
        
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('ETW', 'ETW Offense', 'ETW Defense',
                                                                  'ETW Rank', 'ETW Offense Rank', 'ETW Defense Rank',
                                                                  'Team', 'Conference'))
      
    }else{
      etwCurr = etwCurr %>% 
        dplyr::select(Team, Conference, `ETW Rank`, ETW,
               SOS, AdjEM, `TS% Margin`, `OR% Margin`, `TOV Margin/100 Trips`)
      
      # apply filters
      c = input$`etwPage-filterConf`
      t = input$`etwPage-filterSchool`
      #p = input$`pcsPage-filterPlayer`
      
      if (c != "All") {etwCurr = etwCurr %>% filter(Conference == c)}
      if (t != "All") {etwCurr = etwCurr %>% filter(Team == t)}
      
      #etwCurr$Year = as.character(etwCurr$Year)
      
      # if (input$`etwPage-finalFourCheckbox`){
      #   etwCurr = etwCurr %>% filter(Team %in% c("UConn", 'Alabama', 'Purdue', 'NC State'))
      # }
      
      etwCurr = etwCurr %>% arrange(desc(ETW))
      DT::datatable(etwCurr,
                    rownames = FALSE,
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(width = '18%', targets = c(0,1))),
                                   scrollX = '400px', fixedColumns = list(leftColumns = 4), pageLength=10))%>%
        
        formatStyle(c(4, 10, 16),`border-left` = '4px solid black')%>% 
        formatStyle(c(4),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(etwCurr,is.numeric)), digits = 2) %>% 
        formatRound(c('ETW'), digits = 3) %>% 
        formatRound(c('ETW Rank'), digits = 0) %>% 
        formatRound(c('SOS'), digits = 1) %>% 
        formatRound(c("TS% Margin"), digits = 1) %>% 
        
        formatStyle(c('ETW'),backgroundColor = tmcl.tops) %>% 
        formatStyle(c('AdjEM'),backgroundColor = tmcl.kp) %>% 
        formatStyle(c('SOS'),backgroundColor = tmcl.sos) %>%
        formatStyle(c('TS% Margin'),backgroundColor = etwcl.ts) %>% 
        formatStyle(c('OR% Margin'),backgroundColor = etwcl.orb) %>% 
        formatStyle(c('TOV Margin/100 Trips'),backgroundColor = etwcl.tov) %>% 
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('ETW', #'ETW Offense', 'ETW Defense',
                                                                  'ETW Rank', #'ETW Offense Rank', 'ETW Defense Rank',
                                                                  'Team', 'Conference'))
      
    }
    
  })
  
  # https://stackoverflow.com/questions/68108168/shiny-dt-multiple-levels-header-with-dynamic-difference-quantity-columns-using-j
  
  output$`originalMetrics-etwAllTime` <- DT::renderDataTable({
    
    # apply filters
    
    #if (p != "All") {pcsDF = pcsDF %>% filter(Player == p)}
    
    # rename variables
    
    # order / select variables
    
    DT::datatable(etwAllTime,
                  rownames = FALSE,
                  extensions = "FixedColumns",
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                   list(width = '18%', targets = c(0,1))),
                                 scrollX = '400px', fixedColumns = list(leftColumns = 5), pageLength=10))%>%
      
      formatStyle(c(9),`border-left` = '4px solid black')%>% 
      formatStyle(c(5),`border-right` = '4px solid black') %>% 
      formatRound(which(sapply(pcsDF,is.numeric)), digits = 2) #%>% 
      
      # formatStyle(c('ETW'),backgroundColor = tmcl.tops) %>% 
      # formatStyle(c('ETW Offense'),backgroundColor = tmcl.tops.off) %>%
      # formatStyle(c('ETW Defense'),backgroundColor = tmcl.tops.off) %>%
      # 
      # formatStyle(c('TS% Margin'),backgroundColor = etwcl.ts) %>% 
      # formatStyle(c('TS%'),backgroundColor = etwcl.ts.off) %>%
      # formatStyle(c('TS% Allowed'),backgroundColor = etwcl.ts.def) %>%
      # 
      # formatStyle(c('ORB Margin/100 Trips'),backgroundColor = etwcl.orb) %>% 
      # formatStyle(c('ORB/100 Trips'),backgroundColor = etwcl.orb.off) %>%
      # formatStyle(c('ORB Allowed/100 Trips'),backgroundColor = etwcl.orb.def) %>%
      # 
      # formatStyle(c('TOV Margin/100 Trips'),backgroundColor = etwcl.tov) %>% 
      # formatStyle(c('TOV/100 Trips'),backgroundColor = etwcl.tov.off) %>%
      # formatStyle(c('TOV Allowed/100 Trips'),backgroundColor = etwcl.tov.def)
      
    
    # add color vectors
    
    # round values
    
  })
  
  output$`originalMetrics-pcs` <- DT::renderDataTable({
    
    pcsDF = pcsDF %>% arrange(desc(BASIC))
    pcsDF$`Rk` = seq(1, length(pcsDF$Player))
    
    c = input$`pcsPage-filterConf`
    t = input$`pcsPage-filterSchool`
    p = input$`pcsPage-filterPlayer`
    
    if (c != "All") {pcsDF = pcsDF %>% filter(Conference == c)}
    if (t != "All") {pcsDF = pcsDF %>% filter(School == t)}
    if (p != "All") {pcsDF = pcsDF %>% filter(Player == p)}

    #pcsDF = pcsDF %>% 
    #  mutate(`Perimeter Defense` = Perimeter.Defense,
    #         `Interior Defense` = Interior.Defense)
    
    pcsDF = pcsDF %>% dplyr::select(-X)#, -Perimeter.Defense, -Interior.Defense)
    
    if (input$`pcsPage-statsCheckbox`){
      
      pcsDF$`TS%` = pcsDF$`TS%`*100
      pcsDF$`TS%` = round(pcsDF$`TS%`,1)
      pcsDF = pcsDF %>% dplyr::select(Player, School, Conference,`Rk`, BASIC,
                               Scoring, PPG, `TS%`, 
                               Playmaking, `APG`, TOV, `AST%`, `TOV%`, 
                               Rebounding, ORB, `ORB%`, `DRB`, `DRB%`,
                               `Defense`, DRtg, BPG, `BLK%`, SPG, `STL%`,
                               Impact, `Net Rating`,
                               Load, MPG, `USG%`,
                               Opponents, SOS) %>% 
        arrange(desc(BASIC))
      
      dt = DT::datatable(pcsDF,
                    rownames = FALSE,
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(width = '18%', targets = c(0,1))),
                                   scrollX = '400px', fixedColumns = list(leftColumns = 4), pageLength=10))%>%
        
        formatStyle(c(6, 9, 14, 19, 25, 27, 30),`border-left` = '4px solid black')%>% 
        formatStyle(c(4),`border-right` = '4px solid black') %>% 
        #formatStyle(c(3),`border-right` = '4px solid black')%>% 
        
        # bold BASIC colmn
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('BASIC', 'Player', 'School',
                                                                  'Scoring', 'Playmaking', 'Rebounding',
                                                                  'Defense', 'Impact', 'Load', 'Opponents',
                                                                  'Conference', 'Rk'))%>% 
        formatRound(which(sapply(pcsDF,is.numeric)), digits = 1)%>% 
        formatRound(columns=c('Scoring', 'Playmaking', 'Rebounding',
                              'Defense', 'Impact', "Load", 'Opponents'), digits = 3) %>% 
        formatRound(columns=c("Rk"), digits = 0) %>% 
        formatRound(columns=c("BASIC"), digits = 1) %>% 
        
        # DT formatting
        #formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('Player', 'BASIC', 'School', 'Conference')) %>% 
        
        formatStyle(c('BASIC'),backgroundColor = plcl.pcs) %>% 
        formatStyle(c('Scoring'),backgroundColor = plcl.scoring) %>%
        formatStyle(c('Playmaking'),backgroundColor = plcl.passing) %>%
        formatStyle(c('Rebounding'),backgroundColor = plcl.rebounding) %>%
        formatStyle(c('Defense'),backgroundColor = plcl.def) %>%
        #formatStyle(c('Interior Defense'),backgroundColor = plcl.interiorD) %>%
        formatStyle(c('Impact'),backgroundColor = plcl.impact) %>%
        formatStyle(c('Load'),backgroundColor = plcl.load) %>%
        formatStyle(c('Opponents'),backgroundColor = plcl.opp) %>%
        # color BASIC column by rating - green yellow red scale
        #formatStyle(c('BASIC'),backgroundColor = plcl.pcs) %>% #styleInterval(seq(-7,10,0.1), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b', '#c5fc8b', '#a3fb45','#6fd404'))(length(seq(-7,10,0.1))+1))) %>%
        formatStyle(c('USG%'),backgroundColor = plcl.usg) %>%
        formatStyle(c('SOS'),backgroundColor = plcl.sos) %>%

        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%

        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('TOV%'),backgroundColor = plcl.tovpct) %>%

        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('ORB%'),backgroundColor = plcl.orbpct) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('DRB%'),backgroundColor = plcl.drbpct) %>%

        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('STL%'),backgroundColor = plcl.stlpct) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('BLK%'),backgroundColor = plcl.blkpct) %>%
        formatStyle(c('MPG'),backgroundColor = plcl.mpg) %>%
        
        formatStyle(c('Net Rating'),backgroundColor = plcl.netrating) %>%
        formatStyle(c('DRtg'),backgroundColor = plcl.drtg)
        
    }else{
      
      
      
      pcsDF = pcsDF %>% dplyr::select(Player, School, Conference,`Rk`, BASIC,
                               Scoring, Playmaking, Rebounding,
                               `Defense`,# `Interior Defense`,
                               Impact, Load, Opponents) %>% 
        arrange(desc(BASIC))
      
      dt = DT::datatable(pcsDF,
                    rownames = FALSE,
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(width = '18%', targets = c(0,1))),
                                   scrollX = '400px', fixedColumns = list(leftColumns = 4), pageLength=10))%>%
        
        # DT formatting
        formatStyle(c(6),`border-left` = '4px solid black') %>% 
        formatStyle(c(4),`border-right` = '4px solid black') %>% 
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('Player', 'BASIC', 'School', 'Conference', 'Rk')) %>% 
        formatRound(which(sapply(pcsDF,is.numeric)), digits = 1)%>% 
        formatRound(columns=c('Scoring', 'Playmaking', 'Rebounding',
                              'Defense', 'Impact', "Load", 'Opponents'), digits = 3) %>% 
        formatRound(columns=c("Rk"), digits = 0) %>% 
        formatRound(columns=c("BASIC"), digits = 1) %>% 
        
        formatStyle(c('BASIC'),backgroundColor = plcl.pcs) %>%
        formatStyle(c('Scoring'),backgroundColor = plcl.scoring) %>%
        formatStyle(c('Playmaking'),backgroundColor = plcl.passing) %>%
        formatStyle(c('Rebounding'),backgroundColor = plcl.rebounding) %>%
        formatStyle(c('Defense'),backgroundColor = plcl.def) %>%
        #formatStyle(c('Interior Defense'),backgroundColor = plcl.interiorD) %>%
        formatStyle(c('Impact'),backgroundColor = plcl.impact) %>%
        formatStyle(c('Load'),backgroundColor = plcl.load) %>%
        formatStyle(c('Opponents'),backgroundColor = plcl.opp)
    }
    
    return(dt)
  })
  
  
  output$`originalMetrics-historicPCS` <- DT::renderDataTable({
    
    historicPCS$`TS%` = historicPCS$`TS%`*100
    historicPCS$`TS%` = round(historicPCS$`TS%`,1)
    historicPCS = historicPCS %>% arrange(desc(BASIC))
    historicPCS$`All Time Rank` = seq(1, length(historicPCS$Player))
    
    c = input$`pcsPage-filterConfAllTime`
    t = input$`pcsPage-filterSchoolAllTime`
    p = input$`pcsPage-filterPlayerAllTime`
    y = input$`pcsPage-filterYearAllTime`
    
    #print(c)
    #print(t)
    #print(p)
    if (c != "All") {historicPCS = historicPCS %>% filter(Conference == c)}
    if (t != "All") {historicPCS = historicPCS %>% filter(School == t)}
    if (p != "All") {historicPCS = historicPCS %>% filter(Player == p)}
    if (y != "All") {historicPCS = historicPCS %>% filter(Year == y)}
    
   # historicPCS = historicPCS %>% 
    #  mutate(`Perimeter Defense` = Perimeter.Defense,
    #         `Interior Defense` = Interior.Defense)
    
    #historicPCS = historicPCS %>% select(-X, -Perimeter.Defense, -Interior.Defense)
    
    if (input$`pcsPage-statsAllTimeCheckbox`){
      
      #historicPCS$`TS%` = historicPCS$`TS%`*100
      historicPCS = historicPCS %>% dplyr::select(Player, Year, School, Conference,`All Time Rank`, BASIC,
                               Scoring, PPG, `TS%`, 
                               Playmaking, `APG`, TOV, `AST%`, `TOV%`, 
                               Rebounding, ORB, `ORB%`, `DRB`, `DRB%`,
                               `Defense`, DRtg, BPG, `BLK%`, SPG, `STL%`,
                               Impact, `Net Rating`,
                               Load, MPG, `USG%`,
                               Opponents, SOS) %>% 
        arrange(desc(BASIC))
      historicPCS$Year = as.character(historicPCS$`Year`)
      #historicPCS = historicPCS %>% arrange(desc(BASIC))
      dt = DT::datatable(historicPCS,
                    rownames = FALSE,
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(width = '15%', targets = c(0,2)),
                                                     list(width = '10%', targets = c(1,3))),
                                   scrollX = '400px', fixedColumns = list(leftColumns = 5), pageLength=10))%>%
        
        formatStyle(c(7, 10, 15, 20, 26, 28, 31),`border-left` = '4px solid black')%>% 
        formatStyle(c(5),`border-right` = '4px solid black') %>% 
        #formatStyle(c(3),`border-right` = '4px solid black')%>% 
        
        # bold BASIC colmn
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('BASIC', 'Player', 'School',
                                                                  'Scoring', 'Playmaking', 'Rebounding',
                                                                  'Defense', 'Impact', 'Load', 'Opponents',
                                                                  'Conference', 'All Time Rank'))%>% 
        formatRound(which(sapply(historicPCS,is.numeric)), digits = 1)%>% 
        formatRound(columns=c('Scoring', 'Playmaking', 'Rebounding',
                              'Defense', 'Impact', "Load", 'Opponents'), digits = 3) %>% 
        formatRound(columns=c("All Time Rank"), digits = 0) %>% 
        formatRound(columns=c("BASIC"), digits = 1) %>% 
        
        # DT formatting
        #formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('Player', 'BASIC', 'School', 'Conference')) %>% 
        
        #formatStyle(c('BASIC'),backgroundColor = plcl.pcs) %>%
        formatStyle(c('Scoring'),backgroundColor = plcl.scoring) %>%
        formatStyle(c('Playmaking'),backgroundColor = plcl.passing) %>%
        formatStyle(c('Rebounding'),backgroundColor = plcl.rebounding) %>%
        formatStyle(c('Defense'),backgroundColor = plcl.def) %>%
        #formatStyle(c('Interior Defense'),backgroundColor = plcl.interiorD) %>%
        formatStyle(c('Impact'),backgroundColor = plcl.impact) %>%
        formatStyle(c('Load'),backgroundColor = plcl.load) %>%
        formatStyle(c('Opponents'),backgroundColor = plcl.opp) %>%
        # # color BASIC column by rating - green yellow red scale
        formatStyle(c('BASIC'),backgroundColor = plcl.pcs) %>% #styleInterval(seq(-7,10,0.1), colorRampPalette(c('#e1685f','#e7847d', '#F0BB32','#fee08b','#ffffbf','#d9ef8b','#c5fc8b', '#c5fc8b', '#a3fb45','#6fd404'))(length(seq(-7,10,0.1))+1))) %>%
        formatStyle(c('USG%'),backgroundColor = plcl.usg) %>%
        formatStyle(c('SOS'),backgroundColor = plcl.sos) %>%

        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%

        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('TOV%'),backgroundColor = plcl.tovpct) %>%

        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('ORB%'),backgroundColor = plcl.orbpct) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('DRB%'),backgroundColor = plcl.drbpct) %>%

        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('STL%'),backgroundColor = plcl.stlpct) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('BLK%'),backgroundColor = plcl.blkpct) %>%
        formatStyle(c('MPG'),backgroundColor = plcl.mpg) %>%

        formatStyle(c('Net Rating'),backgroundColor = plcl.netrating) %>%
        formatStyle(c('DRtg'),backgroundColor = plcl.drtg)
        
        # add solid black borders to group related stats
        
      
      
      
      
      
    }else{
      
      
      
      
      historicPCS = historicPCS %>% dplyr::select(Player, `Year`, School, Conference,`All Time Rank`, BASIC,
                                           Scoring, Playmaking, Rebounding,
                                           `Defense`,# `Interior Defense`,
                                           Impact, Load, Opponents) %>% 
        arrange(desc(BASIC))
      historicPCS$Year = as.character(historicPCS$`Year`)
      dt = DT::datatable(historicPCS,
                    rownames = FALSE,
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(width = '15%', targets = c(0,2)),
                                                     list(width = '10%', targets = c(1,3))),
                                   scrollX = '400px', fixedColumns = list(leftColumns = 5), pageLength=10))%>%
        
        # DT formatting
        formatStyle(c(7),`border-left` = '4px solid black') %>% 
        formatStyle(c(5),`border-right` = '4px solid black') %>% 
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('Player', 'Year','BASIC', 'School', 'Conference', 'All Time Rank')) %>% 
        formatRound(which(sapply(historicPCS,is.numeric)), digits = 1)%>% 
        formatRound(columns=c('Scoring', 'Playmaking', 'Rebounding',
                              'Defense', 'Impact', "Load", 'Opponents'), digits = 3) %>% 
        formatRound(columns=c("All Time Rank"), digits = 0) %>% 
        formatRound(columns=c("BASIC"), digits = 1) %>% 
        #formatRound(columns=c('Year'), digits = 0, mark = "",) %>% 
        
        formatStyle(c('BASIC'),backgroundColor = plcl.pcs) %>% 
        formatStyle(c('Scoring'),backgroundColor = plcl.scoring) %>%
        formatStyle(c('Playmaking'),backgroundColor = plcl.passing) %>%
        formatStyle(c('Rebounding'),backgroundColor = plcl.rebounding) %>%
        formatStyle(c('Defense'),backgroundColor = plcl.def) %>%
        #formatStyle(c('Interior Defense'),backgroundColor = plcl.interiorD) %>%
        formatStyle(c('Impact'),backgroundColor = plcl.impact) %>%
        formatStyle(c('Load'),backgroundColor = plcl.load) %>%
        formatStyle(c('Opponents'),backgroundColor = plcl.opp)
      
      
      
      
    }
    
    return(dt)
    
  })
  
  
  #############################################################
  ################### 3.5) Visuals Tab ########################
  #############################################################
  
  #############################
  ###### Pre-Built Teams ######
  #############################
  
  ###### RANKINGS ######
  
  
  ### AdjEM vs TOPS
  output$`vis-teams-AdjEMvsTOPS` <- renderPlot({
    
    # get data
    this = Teams %>% arrange(desc(AdjEM))
    t1002 = this[1:150,]
    # apply filters

    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-overallFilterN`
    if (input$`vis-teams-overallFilterP6`){
      this=this %>% filter(Conference %in% p6 | School %in% c("Gonzaga", "Memphis"))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    print(this$Logo)
    this=this %>% filter(!is.na(School))
    showing=length(this$School)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$ETW, 
                           y=this$AdjEM)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t1002$`ETW`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`AdjEM`), linetype="dashed", color = "purple", size=1.1) +
      
      
      # formatting
      theme_gdocs() +
      ggtitle("AdjEM vs ETW", 
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Estimated Tournament Wins")+
      ylab("KenPom Adjusted Efficiency Margin")+theme_minimal()+
      theme(text = element_text(size=20,  family="Trebuchet MS"))
    
    p

  })
  
  
  ### DRtg vs ORtg
  output$`vis-teams-DRtgvsORtg` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(AdjEM))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-overallFilterN`
    if (input$`vis-teams-overallFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$ORtg, 
                           y=this$DRtg)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`ORtg`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`DRtg`), linetype="dashed", color = "purple", size=1.1) +
      scale_y_reverse()+
      
      # formatting
      theme_gdocs() +
      ggtitle("Defensive Rating vs Offensive Rating", 
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Offensive Rating")+
      ylab("Defensive Rating")+theme_minimal()+
      theme(text = element_text(size=20,  family="Trebuchet MS")) 
    
    p
  })
  
  ### AdjOE vs AdjDE
  output$`vis-teams-AdjDEvsAdjOE` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(AdjEM))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-overallFilterN`
    if (input$`vis-teams-overallFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$AdjOE, 
                           y=this$AdjDE)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`AdjOE`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`AdjDE`), linetype="dashed", color = "purple", size=1.1) +
      scale_y_reverse()+
      
      # formatting
      theme_gdocs() +
      ggtitle("KP AdjDE vs KP AdjOE", 
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("KenPom Adj. Off Efficiency")+
      ylab("KenPom Adj. Def Efficiecny")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS")) 
    
    p
    
  })
  

  
  ### Net Rating vs SOS
  output$`vis-teams-NetRtgvsSOS` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(AdjEM))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-overallFilterN`
    if (input$`vis-teams-overallFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$SOS, 
                           y=this$eDiff)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`SOS`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`eDiff`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("Net Rating vs Strength of Schedule",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Strength of Schedule")+
      ylab("Net Rating")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS")) 
    
    p
  })
  
  ###### SCORING ######
  
  # PPG vs TS% 
  # PPG vs eFG%
  # ORtg vs SOS
  # AdjOE vs RankAdjOE
  
  ### PPG vs TS%
  output$`vis-teams-TS%vsPPG` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(AdjEM))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-offenseFilterN`
    if (input$`vis-teams-offenseFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$PPG, 
                           y=this$`TS%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`PPG`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`TS%`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("TS% vs PPG",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Points per Game")+
      ylab("True Shooting %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  
  ### PPG vs eFG%
  output$`vis-teams-eFG%vsPPG` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(AdjEM))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    p6Title = ""
    #print(colnames(this))
    N = input$`vis-teams-offenseFilterN`
    if (input$`vis-teams-offenseFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$PPG, 
                           y=this$`eFG%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`PPG`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`eFG%`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("eFG% vs PPG",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Points per Game")+
      ylab("Effective Field Goal %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  ### ORtg vs SOS
  output$`vis-teams-SOSvsORtg` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(AdjEM))
    
    # apply filters
    p6Title = ""
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    N = input$`vis-teams-offenseFilterN`
    if (input$`vis-teams-offenseFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$SOS, 
                           y=this$ORtg)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`SOS`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`ORtg`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("Offensive Rating vs Strength of Schedule",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Strength of Schedule")+
      ylab("Offensive Rating")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  
  
  ### AdjOE Ranks
  output$`vis-teams-AdjOEvsRankAdjOE` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(AdjEM))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-offenseFilterN`
    if (input$`vis-teams-offenseFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$RankAdjOE, 
                           y=this$AdjOE)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`RankAdjOE`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`AdjOE`), linetype="dashed", color = "purple", size=1.1) +
      scale_x_reverse()+
      
      # formatting
      theme_gdocs() +
      ggtitle("Adjusted Offensive Effiency Rankings",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("KenPom Offense Rank")+
      ylab("KenPom Adj. Offenive Effiency")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  
  ###### SHOOTING ######
  
  ### 2P% vs 2PA
  output$`vis-teams-2P%vs2PA` <- renderPlot({
    
    # get data
    #TeamStats = TeamStats %>% mutate(`2PM` = `2PA`*`2P%`)
    this = Teams %>% arrange(desc(`AdjEM`))
    #this = this %>% 
    #  mutate(`2PAp100` = round((`2PA`/AdjTempo)*100, 1))
    t1002 = this[1:150,]
    # apply filters
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    
    p6Title = ""
    N = input$`vis-teams-shootingFilterN`
    if (input$`vis-teams-shootingFilterP6`){
      this=this %>% filter(Conference %in% p6 | School %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(School))
    showing=length(this$School)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`2PAp100`, 
                           y=this$`2P%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t1002$`2PAp100`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t1002$`2P%`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("2P% vs 2PA/100 Poss",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Twos Attempted per 100 Possessions")+
      ylab("Two Point %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  
  ### 3P% vs 3PA
  output$`vis-teams-3P%vs3PA` <- renderPlot({
    
    # get data
    #TeamStats = TeamStats %>% mutate(`3PM` = `3PA`*`3P%`)
    this = Teams %>% arrange(desc(`AdjEM`))
    #this = this %>% 
    #  mutate(`3PAp100` = round((`3PA`/AdjTempo)*100, 1))
    t1002 = this[1:150,]
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-shootingFilterN`
    if (input$`vis-teams-shootingFilterP6`){
      this=this %>% filter(Conference %in% p6 | School %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(School))
    showing=length(this$School)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`3PAp100`, 
                           y=this$`3P%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t1002$`3PAp100`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t1002$`3P%`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("3P% vs 3PA/100 Poss",
              subtitle = paste0('Showing Top ', as.factor(showing), ' KenPom Teams'))+
      xlab("Threes Attempted per 100 Possessions")+
      ylab("Three Point %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
    
    # ggplotly(p)%>%
    #   
    #   ### extra plotly formatting
    #   layout(title = list(text = paste0('3P% vs 3PA','<br>',
    #                                     '<sup>','Showing Top ', as.factor(showing), ' Teams in 3PM','</sup>')),
    #          xaxis = list(title=paste0("<sup>", "Threes Attempted per Game", "</sup>")),
    #          yaxis = list(title=paste0("<sup>", "Three Point %", "</sup>")))
  })
  
  ### FT% vs FTA
  output$`vis-teams-FT%vsFTA` <- renderPlot({
    
    # get data
    TeamStats = TeamStats %>% mutate(`FTM` = `FTA`*`FT%`)
    this = Teams %>% arrange(desc(AdjEM))
    #this = this %>% 
    #  mutate(`FTAp100` = round((`FTA`/AdjTempo)*100, 1))
    t1002 = this[1:150,]
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-shootingFilterN`
    if (input$`vis-teams-shootingFilterP6`){
      this=this %>% filter(Conference %in% p6 | School %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(School))
    showing=length(this$School)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$FTAp100, 
                           y=this$`FT%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t1002$`FTAp100`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t1002$`FT%`), linetype="dashed", color = "purple", size=1.1) +
      # formatting
      theme_gdocs() +
      ggtitle("FT% vs FTA/100 Poss",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Free Throws Attempts per 100 Possessions")+
      ylab("Free Throw %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
    
    # ggplotly(p)%>%
    #   
    #   ### extra plotly formatting
    #   layout(title = list(text = paste0('FT% vs FTA','<br>',
    #                                     '<sup>','Showing Top ', as.factor(showing), ' Teams in FTM/G','</sup>')),
    #          xaxis = list(title=paste0("<sup>", "Free Throw Attempts per Game", "</sup>")),
    #          yaxis = list(title=paste0("<sup>", "Free Throw %", "</sup>")))
  })
  
  
  
  ### TS% vs PPG
  output$`vis-teams-TS%vsPPG2` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(AdjEM))
    this = this %>% 
      mutate(`FGAp100` = round(((`3PA`+`2PA`)/AdjTempo)*100, 1))
    t1002 = this[1:150,]
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-shootingFilterN`
    if (input$`vis-teams-shootingFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$FGAp100, 
                           y=this$`TS%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t1002$`FGAp100`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t1002$`TS%`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("TS% vs FGA/100 Poss",
              paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Field Goals Attempted per 100 Possessions")+
      ylab("True Shooting %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
    
    # ggplotly(p)%>%
    #   
    #   ### extra plotly formatting
    #   layout(title = list(text = paste0('TS% vs PPG','<br>',
    #                                     '<sup>','Showing Top ', as.factor(showing), ' Teams in PPG','</sup>')),
    #          xaxis = list(title=paste0("<sup>", "Points per Game", "</sup>")),
    #          yaxis = list(title=paste0("<sup>", "True Shooting %", "</sup>")))
  })
  
  
  ###### DEFENSE ######
  
  # PPG_Opp vs eFG%_Opp
  # DRtg vs SOS
  # AdjDE vs RankAdjDE
  # AdjDE vs BLK%
  # AdjDE vs TOV%_Opp
  
  ### eFG%_Opp vs PPG_Opp
  output$`vis-teams-eFG%_OppvsPPG_Opp` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(`AdjEM`))
    
    # apply filters
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    p6Title = ""
    N = input$`vis-teams-defenseFilterN`
    if (input$`vis-teams-defenseFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`PPG_Opp`,
                           y=this$`eFG%_Opp`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`PPG_Opp`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`eFG%_Opp`), linetype="dashed", color = "purple", size=1.1) +
      scale_x_reverse()+scale_y_reverse()+
      
      # formatting
      theme_gdocs() +
      ggtitle("eFG% Allowed vs PPG Allowed",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("PPG Allowed")+
      ylab("Effective Field Goal % Allowed")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  
  ### Adj Ranks
  output$`vis-teams-AdjDEvsRankAdjDE` <- renderPlot({
    
    # get data
    #TeamStats = TeamStats %>% mutate(`3PM` = `3PA`*`3P%`)
    this = TeamStats %>% arrange(desc(`AdjEM`))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-defenseFilterN`
    if (input$`vis-teams-defenseFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`RankAdjDE`, 
                           y=this$`AdjDE`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`RankAdjDE`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`AdjDE`), linetype="dashed", color = "purple", size=1.1) +
      scale_y_reverse()+
      scale_x_reverse()+
      
      # formatting
      theme_gdocs() +
      ggtitle("Adjusted Defensive Effiency Rankings",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("KenPom Defense Rank")+
      ylab("KenPom Adj. Defensive Efficiency")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  ### BLK% vs AdjDE
  output$`vis-teams-BLK%vsDRtg` <- renderPlot({
    
    # get data
    #TeamStats = TeamStats %>% mutate(`FTM` = `FTA`*`FT%`)
    this = TeamStats %>% arrange(desc(`AdjEM`))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    p6Title = ""
    #print(colnames(this))
    N = input$`vis-teams-defenseFilterN`
    if (input$`vis-teams-defenseFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`TOV%_Opp`, 
                           y=this$`BLK%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`TOV%_Opp`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`BLK%`), linetype="dashed", color = "purple", size=1.1) +  
      #scale_x_reverse()+
      
      # formatting
      theme_gdocs() +
      ggtitle("BLK% vs Forced TOV%",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Forced Turnover %")+
      ylab("Block %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))# + 
      #labs(caption = "(Pauloo, et al. 2017)") FOOTNOTE
    
    p
  })
  
  
  
  ### TOV%_Opp vs DRtg
  output$`vis-teams-TOV%_OppvsDRtg` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(AdjEM))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    p6Title = ""
    #print(colnames(this))
    N = input$`vis-teams-defenseFilterN`
    if (input$`vis-teams-defenseFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$DRtg, 
                           y=this$`TOV%_Opp`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`DRtg`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`TOV%_Opp`), linetype="dashed", color = "purple", size=1.1) +
      scale_x_reverse()+
      
      # formatting
      theme_gdocs() +
      ggtitle("Forced TOV% vs DRtg",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Defensive Rating")+
      ylab("Forced Turnover %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  
  ###### PLAYMAKING ######
  
  ### AST% vs TOV%
  output$`vis-teams-TOV%vsAST%` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(`AdjEM`))
    
    # apply filters
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    
    p6Title = ""
    N = input$`vis-teams-playmakingFilterN`
    if (input$`vis-teams-playmakingFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`AST%`,
                           y=this$`TOV%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_hline(yintercept=mean(t100$`TOV%`), linetype="dashed", color = "purple", size=1.1) +
      geom_vline(xintercept=mean(t100$`AST%`), linetype="dashed", color = "purple", size=1.1) +
      scale_y_reverse()+
      # formatting
      theme_gdocs() +
      ggtitle("TOV% vs AST%",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams')) +
      xlab("Assist %")+
      ylab("Turnover %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS")) 
      
      p

  })
  
  
  ### TOV% vs AdjTempo
  output$`vis-teams-AdjTempovsTOV%` <- renderPlot({
    
    # get data
    #TeamStats = TeamStats %>% mutate(`3PM` = `3PA`*`3P%`)
    this = TeamStats %>% arrange(desc(`AdjEM`))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    
    p6Title = ""
    N = input$`vis-teams-playmakingFilterN`
    if (input$`vis-teams-playmakingFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`TOV%`, 
                           y=this$`AdjTempo`)) + 
      #geom_point(aes(text=paste(Team,"\n",Conference,sep=""))) +
      geom_image(size = 0.095,aes(image=Logo)) +
      scale_x_reverse()+
      geom_hline(yintercept=mean(t100$`AdjTempo`), linetype="dashed", color = "purple", size=1.1) +
      geom_vline(xintercept=mean(t100$`TOV%`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("Adj. Tempo vs TOV%",
              subtitle = paste0('Showing ', as.factor(showing), p6Title, ' KenPom Teams'))+
      #ggsubtitle(paste0('Showing Top ', as.factor(showing), ' Teams with HIGHEST TOV%'))+
      xlab("Turnover %")+
      ylab("KenPom Adj. Tempo")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS")) 
    
      p
    # ggplotly(p)%>%
    #   
    #   ### extra plotly formatting
    #   layout(title = list(text = paste0('Adj. Tempo vs TOV%','<br>',
    #                                     '<sup>','Showing Top ', as.factor(showing), ' Teams with HIGHEST TOV%','</sup>')),
    #          xaxis = list(title=paste0("<sup>", "Turnover %", "</sup>")),
    #          yaxis = list(title=paste0("<sup>", "Kenpom Adj. Tempo", "</sup>")))
  })
  
  ### APG vs TOV
  output$`vis-teams-TOVvsAPG` <- renderPlot({
    
    # get data
    #TeamStats = TeamStats %>% mutate(`FTM` = `FTA`*`FT%`)
    this = TeamStats %>% arrange(desc(`AdjEM`))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6title = ""
    N = input$`vis-teams-playmakingFilterN`
    if (input$`vis-teams-playmakingFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$APG, 
                           y=this$`TOV`)) + 
      #geom_point(aes(text=paste(Team,"\n",Conference,sep=""))) +  
      #background_image(img) +
      geom_image(size = 0.095,aes(image=Logo)) +
      scale_y_reverse()+
      geom_hline(yintercept=mean(t100$`TOV`), linetype="dashed", color = "purple", size=1.1) +
      geom_vline(xintercept=mean(t100$`APG`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("TOV vs APG",
              subtitle = paste0('Showing Top ', as.factor(showing),  p6title, ' KenPom Teams'))+
      xlab("Assists per Game")+
      ylab("Turnovers per Game")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    # ggplotly(p)%>%
    #   
    #   ### extra plotly formatting
    #   layout(title = list(text = paste0('TOV vs APG','<br>',
    #                                     '<sup>','Showing Top ', as.factor(showing), ' Teams in APG','</sup>')),
    #          xaxis = list(title=paste0("<sup>", "Assists per Game", "</sup>")),
    #          yaxis = list(title=paste0("<sup>", "Turnovers per Game", "</sup>")))
      
      p
  })
  
  ### AST% vs AdjTempo
  output$`vis-teams-AdjTempovsAST%` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(`AdjEM`))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    
    p6Title = ""
    N = input$`vis-teams-playmakingFilterN`
    if (input$`vis-teams-playmakingFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`AST%`, 
                           y=this$`AdjTempo`)) + 
      #geom_point(aes(text=paste(Team,"\n",Conference,sep=""))) +  
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_hline(yintercept=mean(t100$`AdjTempo`), linetype="dashed", color = "purple", size=1.1) +
      geom_vline(xintercept=mean(t100$`AST%`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("Adj. Tempo vs AST%",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Assist %")+
      ylab("KenPom Adj. Tempo")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    
      p

  })
  

  ###### REBOUNDING ######
  
  
  ### ORB% vs DRB%
  output$`vis-teams-ORB%vsDRB%` <- renderPlot({
    
    # get data
    #TeamStats = TeamStats %>% mutate(`3PM` = `3PA`*`3P%`)
    this = TeamStats %>% arrange(desc(`AdjEM`))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-reboundingFilterN`
    if (input$`vis-teams-reboundingFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`DRB%`, 
                           y=this$`ORB%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`DRB%`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`ORB%`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("ORB% vs DRB%",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Defensive Rebound %")+
      ylab("Offensive Rebound %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  ### ORB% vs RPG
  output$`vis-teams-ORB%vsRPG` <- renderPlot({
    
    # get data
    #TeamStats = TeamStats %>% mutate(`FTM` = `FTA`*`FT%`)
    this = TeamStats %>% arrange(desc(`AdjEM`))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-reboundingFilterN`
    if (input$`vis-teams-reboundingFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$RPG, 
                           y=this$`ORB%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`RPG`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`ORB%`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("ORB% vs TRB",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Total Rebounds per Game")+
      ylab("Offensive Rebound %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  ### DRB% vs RPG
  output$`vis-teams-DRB%vsRPG` <- renderPlot({
    
    # get data
    this = TeamStats %>% arrange(desc(`AdjEM`))
    
    # apply filters
    
    colnames(this)[colnames(this) == "Conference.y"] = "Conference"
    #print(colnames(this))
    p6Title = ""
    N = input$`vis-teams-reboundingFilterN`
    if (input$`vis-teams-reboundingFilterP6`){
      this=this %>% filter(Conference %in% p6 | Team %in% c("Gonzaga", 'Memphis'))
      p6Title = " High-Major"
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Team))
    showing=length(this$Team)
    
    # scatter
    p <- ggplot(this, 
                aes_string(x=this$`RPG`, 
                           y=this$`DRB%`)) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      geom_vline(xintercept=mean(t100$`RPG`), linetype="dashed", color = "purple", size=1.1) +
      geom_hline(yintercept=mean(t100$`DRB%`), linetype="dashed", color = "purple", size=1.1) +
      
      # formatting
      theme_gdocs() +
      ggtitle("DRB% vs TRB",
              subtitle = paste0('Showing Top ', as.factor(showing), p6Title, ' KenPom Teams'))+
      xlab("Total Rebounds per Game")+
      ylab("Defensive Rebound %")+theme_minimal()+
      theme(text = element_text(size=20, family="Trebuchet MS"))
    
    p
  })
  
  
  
  
  ###############################
  ###### Pre-Built Players ######
  ###############################
  
  ###### IMPACT ######
  
  ### 1) PER vs BASIC
  output$`vis-players-PERvsPCS` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`BASIC`)) 
    
    # apply filter
    minGP = input$`vis-players-impactFilterGP`
    minMPG = input$`vis-players-impactFilterMPG`
    minUSG= input$`vis-players-impactFilterUSG`
    N = input$`vis-players-impactFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    
    if (input$`vis-players-impactFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`BASIC`, 
                           y=this$PER)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### formatting - set up dummy labels to be changed with plotly settings
      theme_gdocs() + ggtitle(" ")+ xlab(" ")+ ylab(" ")+ theme_minimal() +
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### extra plotly formatting
      layout(title = list(text = paste0('PER vs BASIC','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in BASIC','</sup>')),
             xaxis = list(title=paste0("<sup><i>", "<b>B</b>ox <b>A</b>ggregate <b>S</b>core using<br><b>I</b>mpact, <b>C</b>ounting, and efficiency stats", "</i></sup>")),
             yaxis = list(title=paste0("<sup><i>", "Player Efficiency Rating", "</i></sup>")))
  })
  
  ### 2) Net Rating vs BASIC
  output$`vis-players-NetRtgvsPCS` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`BASIC`)) 
    
    # apply filter
    minGP = input$`vis-players-impactFilterGP`
    minMPG = input$`vis-players-impactFilterMPG`
    minUSG= input$`vis-players-impactFilterUSG`
    N = input$`vis-players-impactFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-impactFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`BASIC`, 
                           y=this$`Net Rating`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) +
      
      ### formatting - set up dummy labels to be changed with plotly settings
      theme_gdocs() + ggtitle(" ")+ xlab(" ")+ ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p) %>%
      
      ### extra plotly formatting
      layout(title = list(text = paste0('Net Rtg vs BASIC','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in BASIC','</sup>')),
             xaxis = list(title=paste0("<sup>", "Player Contribution Score", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Net Rating", "</sup>")))
  })
  
  ### 3) D-Rating vs O-Rating
  output$`vis-players-DRtgvsORtg` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`Net Rating`)) 
    
    # apply filter
    minGP = input$`vis-players-impactFilterGP`
    minMPG = input$`vis-players-impactFilterMPG`
    minUSG= input$`vis-players-impactFilterUSG`
    N = input$`vis-players-impactFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    
    if (input$`vis-players-impactFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$ORtg, 
                           y=this$DRtg)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### formatting - set up dummy labels to be changed with plotly settings
      theme_gdocs() + ggtitle(" ")+ xlab(" ")+ ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))+
      scale_y_reverse()
    
    ggplotly(p) %>%
      
      ### extra plotly formatting
      layout(title = list(text = paste0('Defensive Rating vs Offensive Rating','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in Net Rating','</sup>')),
             xaxis = list(title=paste0("<sup>", "Offensive Rating", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Defensive Rating", "</sup>")))
  })
  
  ### 4) Net Rating vs PER
  output$`vis-players-NetRtgvsPER` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`PER`)) 
    
    # apply filter
    minGP = input$`vis-players-impactFilterGP`
    minMPG = input$`vis-players-impactFilterMPG`
    minUSG= input$`vis-players-impactFilterUSG`
    N = input$`vis-players-impactFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-impactFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    showing=length(this$Player)
    this=this %>% filter(!is.na(Player))
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$PER, 
                           y=this$`Net Rating`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### formatting - set up dummy labels to be changed with plotly settings
      theme_gdocs() +ggtitle(" ")+xlab(" ")+ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### extra plotly formatting
      layout(title = list(text = paste0('Net Rtg vs PER','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in PER','</sup>')),
             xaxis = list(title=paste0("<sup>", "Player Efficency Rating", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Net Rating", "</sup>")))
  })
  
  ### 5) Net Rating vs MPG
  output$`vis-players-NetRtgvsMPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`Net Rating`)) 
    
    # apply filter
    minGP = input$`vis-players-impactFilterGP`
    minMPG = input$`vis-players-impactFilterMPG`
    minUSG= input$`vis-players-impactFilterUSG`
    N = input$`vis-players-impactFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-impactFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    showing=length(this$Player)
    this=this %>% filter(!is.na(Player))
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$MPG, 
                           y=this$`Net Rating`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### formatting - set up dummy labels to be changed with plotly settings
      theme_gdocs() +ggtitle(" ")+xlab(" ")+ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### extra plotly formatting
      layout(title = list(text = paste0('Net Rtg vs MPG','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in Net Rating','</sup>')),
             xaxis = list(title=paste0("<sup>", "Minutes per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Net Rating", "</sup>")))
  })  
  
  ###### SCORING ######
  
  output$`vis-player-ORtgvsMPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`ORtg`)) 
    
    # apply filters
    minGP = input$`vis-players-scoringFilterGP`
    minMPG = input$`vis-players-scoringFilterMPG`
    minUSG= input$`vis-players-scoringFilterUSG`
    N = input$`vis-players-scoringFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-scoringFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$MPG, 
                           y=this$`ORtg`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('Offensive Rating vs MPG', '<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in ORtg','</sup>')),
             xaxis = list(title=paste0("<sup>", "MPG", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Offensive Rating", "</sup>")))
  })
  
  
  output$`vis-player-TS%vsPPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`PPG`)) 
    
    # apply filters
    minGP = input$`vis-players-scoringFilterGP`
    minMPG = input$`vis-players-scoringFilterMPG`
    minUSG= input$`vis-players-scoringFilterUSG`
    N = input$`vis-players-scoringFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-scoringFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$PPG, 
                           y=this$`TS%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('TS% vs PPG', '<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in PPG','</sup>')),
             xaxis = list(title=paste0("<sup>", "Points per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "True Shooting %", "</sup>")))
  })
  
  
  output$`vis-player-eFG%vsPPG` <- renderPlotly({
    
    
    # get data
    this = Players %>% arrange(desc(`PPG`)) 
    
    # apply filters
    minGP = input$`vis-players-scoringFilterGP`
    minMPG = input$`vis-players-scoringFilterMPG`
    minUSG= input$`vis-players-scoringFilterUSG`
    N = input$`vis-players-scoringFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-scoringFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$PPG, 
                           y=this$`eFG%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ### format with plotly settings
    ggplotly(p)%>%
      layout(title = list(text = paste0('eFG% vs PPG','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in PPG','</sup>')),
             xaxis = list(title=paste0("<sup>", "Points per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Effective Field Goal %", "</sup>")))
  })
  
  
  output$`vis-player-2P%vs2PA` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`2PM`)) 
    
    # apply filters
    minGP = input$`vis-players-scoringFilterGP`
    minMPG = input$`vis-players-scoringFilterMPG`
    minUSG= input$`vis-players-scoringFilterUSG`
    N = input$`vis-players-scoringFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-scoringFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`2PA`, 
                           y=this$`2P%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('2P% vs 2PA','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in 2PM/G','</sup>')),
             xaxis = list(title=paste0("<sup>", "Twos Attempted per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Two Point Shooting %", "</sup>")))
  })
  
  
  output$`vis-player-3P%vs3PA` <- renderPlotly({
    Players = Players %>% mutate(`2PM` = `2P%`*`2PA`)
    Players = Players %>% mutate(`3PM` = `3P%`*`3PA`)
    # get data
    this = Players %>% arrange(desc(`3PM`)) 
    
    # apply filters
    minGP = input$`vis-players-scoringFilterGP`
    minMPG = input$`vis-players-scoringFilterMPG`
    minUSG= input$`vis-players-scoringFilterUSG`
    N = input$`vis-players-scoringFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-scoringFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    p <- ggplot(this, 
                aes_string(x=this$`3PA`, 
                           y=this$`3P%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      layout(title = list(text = paste0('3P% vs 3PA','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in 3PM/G','</sup>')),
             xaxis = list(title=paste0("<sup>", "Threes Attempted per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Three Point Shooting %", "</sup>")))
  })
  
  
  output$`vis-player-FT%vsFTA` <- renderPlotly({
    
    Players = Players %>% mutate(`2PM` = `2P%`*`2PA`)
    Players = Players %>% mutate(`3PM` = `3P%`*`3PA`)
    Players = Players %>% mutate(`FTM` = `FT%`*FTA)
    # get data
    this = Players %>% arrange(desc(`FTM`)) 
    
    # apply filters
    minGP = input$`vis-players-scoringFilterGP`
    minMPG = input$`vis-players-scoringFilterMPG`
    minUSG= input$`vis-players-scoringFilterUSG`
    N = input$`vis-players-scoringFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-scoringFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    p <- ggplot(this, 
                aes_string(x=this$`FTA`, 
                           y=this$`FT%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      layout(title = list(text = paste0('FT% vs FTA','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in FTM','</sup>')),
             xaxis = list(title=paste0("<sup>", "Free Throws Attempted per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Free Throw %", "</sup>")))
  })
  
  
  
  ###### DEFENSE ######
  
  output$`vis-player-DRtgvsMPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(`DRtg`)
    
    # apply filters
    minGP = input$`vis-players-defenseFilterGP`
    minMPG = input$`vis-players-defenseFilterMPG`
    minUSG= input$`vis-players-defenseFilterUSG`
    N = input$`vis-players-defenseFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-defenseFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$MPG, 
                           y=this$`DRtg`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))+
      scale_y_reverse()
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('Defensive Rating vs MPG', '<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in Defensive Rating','</sup>')),
             xaxis = list(title=paste0("<sup>", "Minutes per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Defensive Rating", "</sup>")))
  })
  
  
  output$`vis-player-DRtgvsBPG` <- renderPlotly({
    
    
    # get data
    this = Players %>% arrange(desc(`BPG`)) 
    
    # apply filters
    minGP = input$`vis-players-defenseFilterGP`
    minMPG = input$`vis-players-defenseFilterMPG`
    minUSG= input$`vis-players-defenseFilterUSG`
    N = input$`vis-players-defenseFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-defenseFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$BPG, 
                           y=this$`DRtg`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))+
      scale_y_reverse()
    
    ### format with plotly settings
    ggplotly(p)%>%
      layout(title = list(text = paste0('Defensive Rating vs BPG','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in BPG','</sup>')),
             xaxis = list(title=paste0("<sup>", "Blocks per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Defensive Rating", "</sup>")))
  })
  
  
  output$`vis-player-DRtgvsSPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`SPG`)) 
    
    # apply filters
    minGP = input$`vis-players-defenseFilterGP`
    minMPG = input$`vis-players-defenseFilterMPG`
    minUSG= input$`vis-players-defenseFilterUSG`
    N = input$`vis-players-defenseFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-defenseFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`SPG`, 
                           y=this$`DRtg`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))+
      scale_y_reverse()
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('Defensive Rating vs SPG','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in SPG','</sup>')),
             xaxis = list(title=paste0("<sup>", "Steals per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Defensive Rating", "</sup>")))
  })
  
  
  output$`vis-player-SPGvsBPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(`DRtg`)
    
    # apply filters
    minGP = input$`vis-players-defenseFilterGP`
    minMPG = input$`vis-players-defenseFilterMPG`
    minUSG= input$`vis-players-defenseFilterUSG`
    N = input$`vis-players-defenseFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-defenseFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`BPG`, 
                           y=this$`SPG`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('SPG vs BPG','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in Defensive Rating','</sup>')),
             xaxis = list(title=paste0("<sup>", "Steals per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Blocks per Game", "</sup>")))
  })
  
  
  output$`vis-player-BLK%vsBPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`BPG`)) 
    
    # apply filters
    minGP = input$`vis-players-defenseFilterGP`
    minMPG = input$`vis-players-defenseFilterMPG`
    minUSG= input$`vis-players-defenseFilterUSG`
    N = input$`vis-players-defenseFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-defenseFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`BPG`, 
                           y=this$`BLK%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('BLK% vs BPG','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in BPG','</sup>')),
             xaxis = list(title=paste0("<sup>", "Blocks per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Block %", "</sup>")))
  })
  
  
  output$`vis-player-STL%vsSPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`SPG`)) 
    
    # apply filters
    minGP = input$`vis-players-defenseFilterGP`
    minMPG = input$`vis-players-defenseFilterMPG`
    minUSG= input$`vis-players-defenseFilterUSG`
    N = input$`vis-players-defenseFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-defenseFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`SPG`, 
                           y=this$`STL%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('STL% vs SPG','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in SPG','</sup>')),
             xaxis = list(title=paste0("<sup>", "Steals per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Steal %", "</sup>")))
  })
  
  
  ###### PLAYMAKING ######
  
  output$`vis-player-TOVvsAPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`APG`)) 
    
    # apply filters
    minGP = input$`vis-players-playmakingFilterGP`
    minMPG = input$`vis-players-playmakingFilterMPG`
    minUSG= input$`vis-players-playmakingFilterUSG`
    N = input$`vis-players-playmakingFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-playmakingFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`APG`, 
                           y=this$`TOV`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))+
      scale_y_reverse()
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('TOV vs APG','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in APG','</sup>')),
             xaxis = list(title=paste0("<sup>", "Assists per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Turnovers per Game", "</sup>")))
  })
  
  output$`vis-player-TOV%vsAST%` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`AST%`)) 
    
    # apply filters
    minGP = input$`vis-players-playmakingFilterGP`
    minMPG = input$`vis-players-playmakingFilterMPG`
    minUSG= input$`vis-players-playmakingFilterUSG`
    N = input$`vis-players-playmakingFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-playmakingFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`AST%`, 
                           y=this$`TOV%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))+
      scale_y_reverse()
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('TOV% vs AST%','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in AST%','</sup>')),
             xaxis = list(title=paste0("<sup>", "Assist %", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Turnover %", "</sup>")))
  })
  
  
  output$`vis-player-AST%vsAPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`APG`)) 
    
    # apply filters
    minGP = input$`vis-players-playmakingFilterGP`
    minMPG = input$`vis-players-playmakingFilterMPG`
    minUSG= input$`vis-players-playmakingFilterUSG`
    N = input$`vis-players-playmakingFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-playmakingFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`APG`, 
                           y=this$`AST%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('APG vs AST%','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in APG','</sup>')),
             xaxis = list(title=paste0("<sup>", "Assists per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Assist %", "</sup>")))
  })
  
  
  output$`vis-player-TOV%vsAPG` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`APG`)) 
    
    # apply filters
    minGP = input$`vis-players-playmakingFilterGP`
    minMPG = input$`vis-players-playmakingFilterMPG`
    minUSG= input$`vis-players-playmakingFilterUSG`
    N = input$`vis-players-playmakingFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-playmakingFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`APG`, 
                           y=this$`AST%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))+
      scale_y_reverse()
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('TOV% vs APG','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in APG','</sup>')),
             xaxis = list(title=paste0("<sup>", "Assists per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Turnover %", "</sup>")))
  })
  
  
  
  ###### REBOUNDING ######
  
  output$`vis-player-ORBvsDRB` <- renderPlotly({
    
    # get data
    Players = Players %>% mutate(TRB = ORB+DRB)
    this = Players %>% arrange(desc(`TRB`)) 
    
    # apply filters
    minGP = input$`vis-players-reboundingFilterGP`
    minMPG = input$`vis-players-reboundingFilterMPG`
    minUSG= input$`vis-players-reboundingFilterUSG`
    N = input$`vis-players-reboundingFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-reboundingFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`DRB`, 
                           y=this$`ORB`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('ORB vs DRB','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in TRB','</sup>')),
             xaxis = list(title=paste0("<sup>", "Defensive Rebounds per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Offensive Rebounds per Game", "</sup>")))
  })
  
  output$`vis-player-ORB%vsORB` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`ORB`)) 
    
    # apply filters
    minGP = input$`vis-players-reboundingFilterGP`
    minMPG = input$`vis-players-reboundingFilterMPG`
    minUSG= input$`vis-players-reboundingFilterUSG`
    N = input$`vis-players-reboundingFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-reboundingFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`ORB`, 
                           y=this$`ORB%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('ORB% vs ORB','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in ORB','</sup>')),
             xaxis = list(title=paste0("<sup>", "Offensive Rebounds per Game %", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Offensive Rebound %", "</sup>")))
  })
  
  
  output$`vis-player-DRB%vsDRB` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`DRB`)) 
    
    # apply filters
    minGP = input$`vis-players-reboundingFilterGP`
    minMPG = input$`vis-players-reboundingFilterMPG`
    minUSG= input$`vis-players-reboundingFilterUSG`
    N = input$`vis-players-reboundingFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-reboundingFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`DRB`, 
                           y=this$`DRB%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('DRB% vs DRB','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in DRB','</sup>')),
             xaxis = list(title=paste0("<sup>", "Defensive Rebounds per Game", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Defensive Rebound %", "</sup>")))
  })
  
  
  output$`vis-player-ORB%vsDRB%` <- renderPlotly({
    
    # get data
    this = Players %>% arrange(desc(`ORB%`)) 
    
    # apply filters
    minGP = input$`vis-players-reboundingFilterGP`
    minMPG = input$`vis-players-reboundingFilterMPG`
    minUSG= input$`vis-players-reboundingFilterUSG`
    N = input$`vis-players-reboundingFilterN`
    this = this %>% 
      filter(GP > minGP,
             MPG > minMPG,
             `USG%`> minUSG)
    if (input$`vis-players-reboundingFilterP6`){
      this=this %>% filter(Conference %in% p6)
    }
    this=this[1:N,]
    this=this %>% filter(!is.na(Player))
    showing=length(this$Player)
    
    ### scatter
    p <- ggplot(this, 
                aes_string(x=this$`DRB%`, 
                           y=this$`ORB%`)) + 
      geom_point(aes(text=paste(Player,"\n",School,sep=""))) + 
      
      ### dummy formatting
      theme_gdocs() +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+theme_minimal()+
      theme(text=element_text(size=14,  family="Trebuchet MS"))
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0('ORB% vs DRB%','<br>',
                                        '<sup>','Showing Top ', as.factor(showing), ' Players in ORB%','</sup>')),
             xaxis = list(title=paste0("<sup>", "Defensive Rebound %", "</sup>")),
             yaxis = list(title=paste0("<sup>", "Offensive Rebound %", "</sup>")))
  })
  
  
  
  
  
  #############################
  ###### Custom Visuals #######
  #############################
  
  ###### scatter plot of player with customization settings such as sample size, stats on axes, and more ###### 
  output$`vis-custom-playerScatter` <- renderPlotly({
    
    ### filters
    
    # top N in stat
    filterCol = input$`cusPlaVis-topNinStat`
    N0 = input$`cusPlaVis-topNinStatNumVal`
    
    # specific stat filters
    N1 = input$`cusPlaVis-filterVal1`
    N2 = input$`cusPlaVis-filterVal2`
    N3 = input$`cusPlaVis-filterVal3`
    
    filterOne = input$`cusPlaVis-filterStat1`
    filterTwo = input$`cusPlaVis-filterStat2`
    filterThree = input$`cusPlaVis-filterStat3`
    
    filterStatOne = input$`cusPlaVis-filterCol1`
    filterStatTwo = input$`cusPlaVis-filterCol2`
    filterStatThree = input$`cusPlaVis-filterCol3`
    
    # team filter
    filterConf = input$`cusPlaVis-filterConf`
    
    # conference filter
    filterSchool = input$`cusPlaVis-filterSchool`
    
    titleSize = 18
    subtitleAdd = ""
    
    # get top N in stat
    this = Players[order(Players[[filterCol]], decreasing = TRUE), ]
    this=this[1:N0,]
    
    if (filterConf != "NCAA"){
        this = this[this[,"Conference"]==filterConf,]
        subtitleAdd = paste0(subtitleAdd, " in the ", filterConf)
        titleSize = titleSize-1
      } else if (input$`cusPlaVis-power6check` & filterSchool == "All"){
        this = this[this$Conference %in% p6,]
        subtitleAdd = paste0(subtitleAdd, " in Power-5")
        titleSize = titleSize-1}
    
    if (filterSchool != "All"){
        this = this[this[,"School"]==filterSchool,]
        subtitleAdd = paste0(subtitleAdd, " on ", filterSchool)
        titleSize = titleSize-1
    }
    
    #this = this %>% arrange(desc(filterCol))
    
    #if (N0 > length(this$School)){this = this[1:N0,]}

    #titleSize = 18
    if (filterOne == "Less than") {
      this=this[this[,filterStatOne]<N1,]
      subtitleAdd=paste(subtitleAdd, "with less than", N1, filterStatOne)
      titleSize = titleSize-2}
    else if (filterOne == "More than") {
      this=this[this[,filterStatOne]>N1,]
      subtitleAdd=paste(subtitleAdd, "with more than", N1, filterStatOne)
      titleSize = titleSize-2}
    if (filterTwo == "Less than") {
      this=this[this[,filterStatTwo]<N2,]
      if (filterThree != "Disabled"){subtitleAdd=paste0(subtitleAdd, ", less than ", N2, " ",filterStatTwo)} 
      else {subtitleAdd=paste0(subtitleAdd, ", and less than ", N2, " ",filterStatTwo)}
      titleSize = titleSize-1}
    else if (filterTwo == "More than") {
      this=this[this[,filterStatTwo]>N2,]
      if (filterThree != "Disabled"){subtitleAdd=paste0(subtitleAdd, ", more than ", N2, " ",filterStatTwo)} 
      else {subtitleAdd=paste0(subtitleAdd, ", and more than ", N2, " ",filterStatTwo)}
      titleSize = titleSize-1}
    if (filterThree == "Less than") {
      this=this[this[,filterStatThree]<N3,]
      subtitleAdd=paste0(subtitleAdd, ", and less than ", N3, " ",filterStatThree)
    } else if (filterThree == "More than") {
      this=this[this[,filterStatThree]>N3,]
      subtitleAdd=paste0(subtitleAdd, ", and more than ", N3, " ",filterStatThree)}
    
    showing=length(this$School)
    ### scatter
    p <- ggplot(this,
                aes_string(x=paste("`", input$`cusPlaVis-xAxis`, "`", sep=""), 
                           y=paste("`", input$`cusPlaVis-yAxis`, "`", sep=""))) + 
      geom_point(aes(text=paste(this$Player,"\n",this$School,sep=""))) +
      ggtitle(" ")+
      xlab(" ")+
      ylab(" ")+
      theme_gdocs()+theme_minimal()+
      theme(text=element_text(size=titleSize,  family="Trebuchet MS"))+
      {if(input$`cusPlaVis-invertYcheck`) scale_y_reverse()}+
      {if(input$`cusPlaVis-invertXcheck`) scale_x_reverse()}
    
    
    ggplotly(p)%>%
      
      ### format with plotly settings
      layout(title = list(text = paste0(paste(input$`cusPlaVis-yAxis`, "vs", input$`cusPlaVis-xAxis`),'<br>',
                                        '<sup>','Showing ', as.factor(showing), ' Players ', subtitleAdd, '</sup>')),
             xaxis = list(title=paste0("<sup>", input$`cusPlaVis-xAxis`, "</sup>")),
             yaxis = list(title=paste0("<sup>",input$`cusPlaVis-yAxis`, "</sup>")))
  })
  
  
  
  
  
  
  
  
  
  
  ###### scatter plot of teams with customization settings such as sample size, stats on axes, and more ###### 
  output$`vis-custom-teamScatter` <- renderPlot({
    
    # # specific stat filters
    N1 = input$`cusTeamVis-filterVal1`
    N2 = input$`cusTeamVis-filterVal2`
    N3 = input$`cusTeamVis-filterVal3`

    filterOne = input$`cusTeamVis-filterStat1`
    filterTwo = input$`cusTeamVis-filterStat2`
    filterThree = input$`cusTeamVis-filterStat3`

    filterStatOne = input$`cusTeamVis-filterCol1`
    filterStatTwo = input$`cusTeamVis-filterCol2`
    filterStatThree = input$`cusTeamVis-filterCol3`
    
    # team filter
    filterConf = input$`cusTeamVis-filterConf`
    
    # conference filter
    filterSchool = input$`cusTeamVis-filterSchool`
    
    titleSize = 18
    subtitleAdd = paste("in", input$`cusTeamVis-topNinStat`)
    
    this = Teams %>% arrange(desc(AdjEM))
    
    if (filterConf != "NCAA"){
      this = this[this[,"Conference"]==filterConf,]
      subtitleAdd = paste0(subtitleAdd, "in the ", filterConf)
      titleSize = titleSize-1
      
    } else if (input$`cusTeamVis-power6check` & filterSchool == "All"){
      this = this[this$Conference %in% p6,]
      subtitleAdd = paste0(subtitleAdd, "in Power-5")
      titleSize = titleSize-1}
    
    if (filterSchool != "All"){
      this = this[this[,"School"]==filterSchool,]
      subtitleAdd = paste0(subtitleAdd, "on ", filterSchool)
      titleSize = titleSize-1
    }
    
    # get top N in stat
    # top N in stat
    filterCol = input$`cusTeamVis-topNinStat`
    N0 = input$`cusTeamVis-topNinStatNumVal`
    
    this = this[order(this[[filterCol]], decreasing = TRUE), ]
    this = this[1:N0,]
    #this <- top_n(this,N0,filterCol)
    # this = this %>% arrange(desc(filterCol))
    # 
    # if (N0 > length(this$School)){this = this[1:N0,]}
    
    #titleSize = 18
    if (filterOne == "Less than") {
      this=this[this[,filterStatOne]<N1,]
      subtitleAdd=paste(subtitleAdd, "with less than", N1, filterStatOne)
      titleSize = titleSize-2}

    else if (filterOne == "More than") {
      this=this[this[,filterStatOne]>N1,]
      subtitleAdd=paste(subtitleAdd, "with more than", N1, filterStatOne)
      titleSize = titleSize-2}

    if (filterTwo == "Less than") {
      this=this[this[,filterStatTwo]<N2,]
      if (filterThree != "Disabled"){subtitleAdd=paste0(subtitleAdd, ", less than ", N2, " ",filterStatTwo)}
      else {subtitleAdd=paste0(subtitleAdd, ", and less than ", N2, " ",filterStatTwo)}
      titleSize = titleSize-1}

    else if (filterTwo == "More than") {
      this=this[this[,filterStatTwo]>N2,]
      if (filterThree != "Disabled"){subtitleAdd=paste0(subtitleAdd, ", more than ", N2, " ",filterStatTwo)}
      else {subtitleAdd=paste0(subtitleAdd, ", and more than ", N2, " ",filterStatTwo)}
      titleSize = titleSize-1}

    if (filterThree == "Less than") {
      this=this[this[,filterStatThree]<N3,]
      subtitleAdd=paste0(subtitleAdd, ", and less than ", N3, " ",filterStatThree)

    } else if (filterThree == "More than") {
      this=this[this[,filterStatThree]>N3,]
      subtitleAdd=paste0(subtitleAdd, ", and more than ", N3, " ",filterStatThree)}
    
    showing=length(this$School)
    ### scatter
    p <- ggplot(this,
                aes_string(x=paste("`", input$`cusTeamVis-xAxis`, "`", sep=""), 
                           y=paste("`", input$`cusTeamVis-yAxis`, "`", sep=""))) + 
      geom_image(size = 0.095,aes(image=Logo)) +
      #geom_vline(xintercept=mean(t1002$`TOPS`), linetype="dashed", color = "purple", size=1.1) +
      #geom_hline(yintercept=mean(t100$`AdjEM`), linetype="dashed", color = "purple", size=1.1) +
      ggtitle(paste(input$`cusTeamVis-yAxis`, "vs", input$`cusTeamVis-xAxis`),
              subtitle = paste0('Showing ', as.factor(showing), ' Teams ', subtitleAdd))+
      xlab(paste0(input$`cusTeamVis-xAxis`))+
      ylab(paste0(input$`cusTeamVis-yAxis`))+
      theme_gdocs()+theme_minimal()+
      theme(text=element_text(size=titleSize,  family="Trebuchet MS"))+
      {if(input$`cusTeamVis-invertYcheck`) scale_y_reverse()}+
      {if(input$`cusTeamVis-invertXcheck`) scale_x_reverse()}
    
    p

  })
  
  
  

  #############################################################
  ################### 3.6) Upcoming Games Tab #################
  #############################################################
  
  ###### Today ######
  
  # reactive infoboxes - changes color and logo
  todayReactiveInfoboxes = reactive({
    
    # conference filter
    if (input$`Conference-today` != "All") {
      this = Today %>%
        filter(Home.Conference == input$`Conference-today` | Away.Conference == input$`Conference-today`)
    }else{this=Today}
    #this=Today
    
    # applying infobox
    info <- lapply(rownames(this), function(row){
      infoBox(width=4,
              
              ### text
              
              # date of game
              title = tags$p(format(as.Date(this[row, 9]), "%a, %b %e"),style='font-size:105%'),
              
              # matchup with ranks
              value = tags$p(paste(this[row,1]," v ",this[row,3]),style='font-size:112%'),
              
              # time of game
              tags$p(paste(this[row,11],"|",this[row,12], "ET"),style='font-size:105%'),
              
              #tags$p(paste(format(as.Date(This.Week$Date[7]), "%a, %b %d"),'@', This.Week$Time[7], "ET",'|',This.Week$Location[7]), style = "font-size: 105%;"),
              ### apply icon based on game score
              if (this[row,4] > 5){icon = icon("gem")} 
              else if (this[row,4] > 3.3){icon = icon("star")}
              else if (this[row,4] > 2.45){icon = icon("thumbs-up")}
              else if (this[row,4] > 1){icon = icon("check")}
              #else if (this[row,4] > -68){icon = icon("minus")}
              #else if (this[row,4] > -120){icon = icon("thumbs-down")}
              else {icon = icon("minus")},
              
              ### apply color based on game score
              if (this[row,4] > 5){color="light-blue"}
              else if (this[row,4] > 3.3){color = "yellow"}
              else if (this[row,4] > 2.45){color="green"}
              else if (this[row,4] > 1){color="teal"}
              #else if (this[row,4] > -68){color="navy"}
              #else if (this[row,4] > -120){color="maroon"}
              else {color="navy"})
      
    })
    return(info)
  })
  output$ibox <- renderUI({
    todayReactiveInfoboxes()
  })
  
  ###### This Week ###### 
  
  # reactive infoboxes - changes color and logo
  thisWeekReactiveInfoboxes = reactive({
    
    # conference filter
    if (input$`Conference-thisWeek` != "All") {
      this = This.Week %>%
        filter(Home.Conference == input$`Conference-thisWeek` | Away.Conference == input$`Conference-thisWeek`)
      }else{this=This.Week}
    #this=This.Week
    
    # apply infoboxes
    info2 <- lapply(rownames(this),function(row){
      infoBox(width=4,
              
              ### text
              
              # date of game
              title = tags$p(format(as.Date(this[row, 9]), "%a, %b %e"),style='font-size:105%'),
              
              # matchup with ranks
              value = tags$p(paste(this[row,1]," v ",this[row,3]),style='font-size:112%'),
              
              # time of game
              tags$p(paste(this[row,11],"|",this[row,12], "ET"),style='font-size:105%'),
              
              ### apply icon based on game score
              if (this[row,4] > 5){icon = icon("gem")} 
              else if (this[row,4] > 3.3){icon = icon("star")}
              else if (this[row,4] > 2.45){icon = icon("thumbs-up")}
              else if (this[row,4] > 1){icon = icon("check")}
              #else if (this[row,4] > -68){icon = icon("minus")}
              #else if (this[row,4] > -120){icon = icon("thumbs-down")}
              else {icon = icon("minus")},
              
              ### apply color based on game score
              if (this[row,4] > 5){color="light-blue"}
              else if (this[row,4] > 3.3){color = "yellow"}
              else if (this[row,4] > 2.45){color="green"}
              else if (this[row,4] > 1){color="teal"}
              #else if (this[row,4] > -68){color="navy"}
              #else if (this[row,4] > -120){color="maroon"}
              else {color="navy"})
    })
    return(info2)
  })
  output$ibox2 <- renderUI({
    thisWeekReactiveInfoboxes()
  })
  
  #############################################################
  ################## 3.7) Bracketology Tab ######################
  #############################################################
  
  ### reactive infoboxes
  bracketologyReactiveInfoboxes = reactive({
    
    # conference filter
    if (input$`Conference-bracketology` != "NCAA") {
      this <- Bracketology[Bracketology$Conference == input$`Conference-bracketology`,]
      this = this %>% filter(`q1` != '' | !is.na(`q1`))
    }else{
      this=Bracketology
    }
    
    # apply infoboxes
    info3 <- lapply(rownames(this),function(row){
      infoBox(
        
        ### text
        
        # conference
        title = tags$p(this[row, 41],style='font-size:110%'),
        
        # team
        value = tags$p(this[row, 2],style='font-size:120%'),
        
        
        # brackeotlogy stats
        tags$p(paste("NET:",toOrdinal(this[row, 3]),"| Q1 Wins:", 
                     this[row, 26]),"| SOR:", 
                     toOrdinal(this[row, 5]),style='font-size:110%'),
        #comehere
        ### reactive icon based on predicted seed
        
        if (as.factor(this[row,40]) == '1'){icon = icon("1")} 
        else if (this[row,40] == '2'){icon = icon("2")}
        else if (this[row,40] == '3'){icon = icon("3")}
        else if (this[row,40] == '4'){icon = icon("4")}
        else if (this[row,40] == '5'){icon = icon("5")}
        else if (this[row,40] == '6'){icon = icon("6")}
        else if (this[row,40] == '7'){icon = icon("7")}
        else if (this[row,40] == '8'){icon = icon("8")}
        else if (this[row,40] == '9'){icon = icon("9")}
        else if (this[row,40] == '10'){icon = icon("arrow-down-1-9")}
        else if (this[row,40] == '11'){icon = icon("arrow-down-1-9")}
        else if (this[row,40] == '12'){icon = icon("arrow-down-1-9")}
        else if (this[row,40] == '13'){icon = icon("arrow-down-1-9")}
        else if (this[row,40] == '14'){icon = icon("arrow-down-1-9")}
        else if (this[row,40] == '15'){icon = icon("arrow-down-1-9")}
        else if (this[row,40] == '16'){icon = icon("arrow-down-1-9")}
        else if (this[row,40] == 'First 4 out'){icon = icon("circle-question")}#here
        else if (this[row,40] == 'Next 4 out'){icon = icon("circle-question")}
        #else if (this[row,3] == 'Next Next 4 out'){icon = icon("face-sad-cry")}
        else {icon = icon("x")},
        
        ### color alternate rows
        if (this[row,40] == '1'){color="light-blue"}
        else if (this[row,40] == '2'){color = "purple"}
        else if (this[row,40] == '3'){color="light-blue"}
        else if (this[row,40] == '4'){color="purple"}
        else if (this[row,40] == '5'){color="light-blue"}
        else if (this[row,40] == '6'){color="purple"}
        else if (this[row,40] == '7'){color="light-blue"}
        else if (this[row,40] == '8'){color="purple"}
        else if (this[row,40] == '9'){color="light-blue"}
        else if (this[row,40] == '10'){color="purple"}
        else if (this[row,40] == '11'){color="light-blue"}
        else if (this[row,40] == '12'){color="purple"}
        else if (this[row,40] == '13'){color="light-blue"}
        else if (this[row,40] == '14'){color="purple"}
        else if (this[row,40] == '15'){color="light-blue"}
        else if (this[row,40] == '16'){color="purple"}
        else if (this[row,40] == 'First 4 out'){color=("red")}
        else if (this[row,40] == 'Next 4 out'){color=("red")}
        #else if (this[row,3] == 'Next Next 4 out'){color=("red")}
        else {color=("black")},
        width=3
      )
    })
    return(info3)
  })
  output$bracketologyInfoBoxes <- renderUI({
    bracketologyReactiveInfoboxes()
  })
  
  ###### Bracketology DT - shown on checkbox instead of infoboxes ######
  output$bracketologyDT <- DT::renderDataTable({
    
    
    # conference filter
    if (input$`Conference-bracketology` != "NCAA") {
      
      this <- Bracketology[Bracketology$Conference == input$`Conference-bracketology`,]
      #this = na.omit(this)
    }else{
      this=Bracketology
    }
    
    # df length to use in coloring seed column
    extra=length(Bracketology$Team)-68
    
    this$NET = this$net
    this$`Resume KPI` = this$`kpi_resume`
    this$`Resume SOR` = this$`sor_resume`
    this$`Quality BPI` = this$`bpi_quality`
    this$`Quality KP` = this$`kp_quality`
    
    this$`Quad 1A` = this$`q1a`
    this$`Quad 1` = this$`q1`
    this$`Quad 2` = this$`q2`
    this$`Quad 3` = this$`q3`
    this$`Quad 4` = this$`q4`
    this$`School` = this$`team`
    this$`Conf.` = this$`conf`
    this$`Seed` = this$`seed`
    this$`Quads 1&2` = paste0((this$`quad_1_W`+this$`quad_2_W`), 
                              '-', 
                              (this$`quad_1_L`+this$`quad_2_L`))
    this$`Quads 3&4` = paste0((this$`quad_3_W`+this$`quad_4_W`), 
                             '-', 
                             (this$`quad_3_L`+this$`quad_4_L`))
    this$`Quads 2-4` = paste0(this$`quad_234_W`, '-', this$`quad_234_L`)
    this$`Wins Above Bubble` = round(this$wab,2)
    
    this = this %>% select(Team, `Conf.`, Seed,
                           NET, `Resume KPI`, `Resume SOR`, `Quality BPI`, `Quality KP`,
                           `Wins Above Bubble`,
                           `Quad 1A`, `Quad 1`,`Quad 2`,`Quad 3`,`Quad 4`,
                           `Quads 1&2`,`Quads 3&4`,`Quads 2-4`)
    
    this = this %>% filter(`Quads 3&4` != 'NA-NA')
    ### DT
    DT::datatable(this, 
                  
                  # settings to set page length to 16, set scroll, etc.
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                 scrollX = '400px', 
                                 pageLength=16#,
                                 #fixedColumns = list(leftColumns = 3)
                                 )
                  )%>% 
      
      ### styling
      
      # color seeds alternating purple and grey
      formatStyle(c('Seed'),backgroundColor = styleInterval(sort(seeds), 
                                                            c(rep('#b08ef5',4), rep('lightgrey',4), rep('#b08ef5',4), rep('lightgrey',4), rep('#b08ef5',4),
                                                              rep('lightgrey',4), rep('#b08ef5',4), rep('lightgrey',6), rep('lightgrey',4), rep('#b08ef5',4),
                                                              rep('lightgrey',4), rep('#b08ef5',6), rep('lightgrey',4), rep('#b08ef5',4), rep('lightgrey',4),
                                                              rep('#b08ef5',4),
                                                              rep('darkgrey',(364-67))))) %>% 
      # add black border around seed column
      formatStyle(c(3,4,5,7,9,10,15),`border-left` = '4px solid black')%>% 
      #formatStyle(c(2),`border-right` = '4px solid black')%>% 
      formatStyle(c('Resume KPI', "Resume SOR", 'Quality BPI', 'Quality KP', 'NET'),backgroundColor = rank.colors) %>% 
      formatStyle(c('Wins Above Bubble'),backgroundColor = wab.colors) %>% 
      #formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
      # bold seed column
      formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Seed', 'Team', 'Conf.', 'NET'))
  })
  
  ### checkbox element - switch between infoboxes and DT
  output$bracketologySwitch <- renderUI({
    
    if (input$`bracketology-checkbox`){
      DT::dataTableOutput("bracketologyDT")
    }else{
      uiOutput("bracketologyInfoBoxes")
    }
  })
  

  ################## 3.8) Season Awards Tab ###################

  ### function to clean award prediction df
  applyCheckbox2 = function(df, check, abbre){
    
    # if checkbox true - show all stats
    if (check){
      
      # clean column names
      df = df %>% 
        mutate(Team = Full.School.Name)
      colnames(df)=gsub("\\.", "%", colnames(df))
      colnames(df)=gsub("Full%School%Name", "Full School Name", colnames(df))
      colnames(df)=gsub("Full.School.Name", "Full School Name", colnames(df))
      colnames(df)=gsub("X", "", colnames(df))
      colnames(df)=gsub("NETRtg", "Net Rating", colnames(df))
      df = df[,2:length(df)]
      #df=df[,3:48]
      df=df[,!names(df) %in% c("Total%S%%")]
      df=df[,!names(df) %in% c("Position")]

      df=df %>% dplyr::mutate(#`2PM` = FGM -`3PM`,
                       `3P%` = round(`3P%`, 1),
                       `TS%` = round(`TS%`, 1),
                       `eFG%` = round(`eFG%`, 1),
                       `AST:TOV` = `AST%TOV`) %>% 
        dplyr::select(`All-Conference Team`, player, #Pos, hgt, 
                      team, 
                      conf, `Conf Seed`, PPG,`TS%`,`eFG%`, #`FG%`,
                      `2PA`, `2P%`, `3PA`, `3P%`, `FTA`, `FT%`,
               `APG`, `TOV`, `AST:TOV`,
               ORB, DRB,
               SPG, BPG,
               ORtg, #DRtg
               )

    } 
    # checkbox false - show only player and school
    else{
      if(abbre){
        df = df %>% 
          mutate(Team = Full.School.Name) %>%
          dplyr::select(Player, Team, Conference)
      }else{
        df = df %>% 
          mutate(Team = Full.School.Name) %>% 
          dplyr::select(`All-Conference Team`, Player, Team, `Conf Seed`)
      }
    }
    return(df)
  }
  
  ### function to clean award prediction df
  applyCheckbox = function(df, check, abbre){
    
    # if checkbox true - show all stats
    if (check){
      
      # clean column names
      df = df %>% 
        mutate(Team = Full.School.Name)
      colnames(df)=gsub("\\.", "%", colnames(df))
      colnames(df)=gsub("Full%School%Name", "Full School Name", colnames(df))
      colnames(df)=gsub("Full.School.Name", "Full School Name", colnames(df))
      colnames(df)=gsub("X", "", colnames(df))
      colnames(df)=gsub("NETRtg", "Net Rating", colnames(df))
      df=df[,3:48]
      df=df[,!names(df) %in% c("Total%S%%")]
      df=df[,!names(df) %in% c("Position")]
      if (abbre){colnames(df)[2] = "School Abbr"}
      else{df$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))}
      colnames(df)[3] = "School"
      df=df %>% relocate(`2PA`, .before=`3PM`)
      df=df %>% relocate(`2P%`, .before=`3PM`)
      df=df %>% mutate(`2PM` = FGM -`3PM`,
                       `3P%` = round(`3P%` * 100, 1),
                       `2P%` = round(`2P%` * 100, 1),
                       `FG%` = round(`FG%` * 100, 1),
                       `TS%` = round(`TS%` * 100, 1),
                       `eFG%` = round(`eFG%` * 100, 1),
                       `FT%` = round(`FT%` * 100, 1),
                       `2PA` = round(`2PA`, 1))
      # change column order
      df=df %>% relocate(`2PM`, .before=`2PA`)
      df=df %>% relocate(`TS%`, .before=`FGM`)
      if (!abbre){df=df %>% relocate(`All-Conference Team`, .before=`Player`)}
      df=df %>% relocate(`eFG%`, .before=`FGM`)
      df=df %>% relocate(`TOV`, .before=`SPG`)
      
      df=df %>% dplyr::select(-`2PM`, -`3PM`, -`FTM`, -`FGM`, -`FGA`, 
                       -RPG, -PF, -PPR, -PPS, -FIC, -`TRB%`)
    } 
    # checkbox false - show only player and school
    else{
      if(abbre){
        df = df %>% 
          mutate(Team = Full.School.Name) %>%
          dplyr::select(Player, Team, Conference)
      }else{
        df = df %>% 
          mutate(Team = Full.School.Name) %>% 
          dplyr::select(`All-Conference Team`, Player, Team)
      }
    }
    return(df)
  }
  
  output$`confPOY-homePage` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = confpoy
    rownames(aaFirstTeam) <- NULL
    
    #aaFirstTeam=applyCheckbox(aaFirstTeam, T, T)
    #aaFirstTeam$Prediction = rep("All-American First Team", 5)
    aaFirstTeam$Team = aaFirstTeam$Full.School.Name
    aaFirstTeam$REB = aaFirstTeam$ORB + aaFirstTeam$DRB
    aaFirstTeam$REB = round(aaFirstTeam$REB, 1)
    aaFirstTeam$AST = round(aaFirstTeam$APG, 1)
    #XaaFirstTeam$Conf = aaFirstTeam$Conference
    #aaFirstTeam$Team = aaFirstTeam$Team_x
    #aaFirstTeam$`Net Rtg` = aaFirstTeam$NETRtg
    aaFirstTeam$`TS%` = round(aaFirstTeam$`TS.`,1)
    aaFirstTeam = aaFirstTeam %>% select(Conf, Player, Team,`Conf Seed`, PPG, `TS%`, REB, AST, TOV)
    
    ### DT
    DT::datatable(aaFirstTeam, rownames = FALSE,
                  
                  # extension to keep team name locked from scroll
                  extensions = "FixedColumns",
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"), 
                                                   list(width = '200px', targets = c(1)),
                                                   list(width = '500px', targets = c(2))),
                                 scrollX = '400px', lengthChange = FALSE, dom = 't',ordering=F
                  )) %>% 
      # bold player name column
      formatStyle(fontsize = 14,fontWeight = 'bold',columns = colnames(aaFirstTeam)) %>% 
      #formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
      #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
      formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
      formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
      formatStyle(c('AST'),backgroundColor = plcl.apg) %>%
      formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
      formatStyle(c('REB'),backgroundColor = plcl.trb) %>% 
      
      formatStyle(c(1,4),`border-right` = '4px solid black')%>% 
      formatRound(which(sapply(aaFirstTeam,is.numeric)), digits = 1) %>% 
      formatStyle(columns = c(1), width='200px') %>% 
      formatStyle(columns = c(2), width='500px')
    
  })
  
  
  ###### All americans ######
  
  ### first team
  output$`AAfirst-homePage` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = ncaa[1:5,]
    rownames(aaFirstTeam) <- NULL
    
    #aaFirstTeam=applyCheckbox(aaFirstTeam, T, T)
    aaFirstTeam$Prediction = rep("All-American First Team", 5)
    aaFirstTeam$Team = aaFirstTeam$Full.School.Name
    aaFirstTeam$REB = aaFirstTeam$ORB + aaFirstTeam$DRB
    aaFirstTeam$REB = round(aaFirstTeam$REB, 1)
    aaFirstTeam$AST = round(aaFirstTeam$APG, 1)
    aaFirstTeam$`TS%` = round(aaFirstTeam$`TS.`*100,1)
    aaFirstTeam = aaFirstTeam %>% select(Player, Team, ORtg, DRtg, PPG, `TS%`, REB, AST, TOV)
    
    ### DT
    DT::datatable(aaFirstTeam, rownames = FALSE,
                  
                  # extension to keep team name locked from scroll
                  extensions = "FixedColumns",
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                 scrollX = '400px', lengthChange = FALSE, dom = 't',ordering=F
                  )) %>% 
      # bold player name column
      formatStyle(fontsize = 14,fontWeight = 'bold',columns = colnames(aaFirstTeam)) %>% 
      formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
      formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
      formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
      formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
      formatStyle(c('AST'),backgroundColor = plcl.apg) %>%
      formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
      formatStyle(c('REB'),backgroundColor = plcl.trb) %>% 
      
      formatStyle(c(2,4),`border-right` = '4px solid black')%>% 
      formatRound(which(sapply(aaFirstTeam,is.numeric)), digits = 1)
  })
  
  ### first team
  output$AAfirst <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = ncaa[1:5,]
    rownames(aaFirstTeam) <- NULL
    
    # get checkbox status
    checkFlag = input$`aa-1-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox(aaFirstTeam, checkFlag, T)
    
    if (checkFlag){aaFirstTeam = aaFirstTeam %>% select(-School,
                                                        -Class,
                                                        -`Full School Name`,
                                                        -`Conference_x`)}
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam, rownames = FALSE,
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                   scrollX = '400px', lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 1)
                    )) %>% 
        # bold player name column
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Player')) %>% 
      
       formatStyle(c('USG%'),backgroundColor = plcl.usg) %>%

       formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
       formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
       formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
       formatStyle(c('FG%'),backgroundColor = plcl.fg) %>%

       formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
       formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
       formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
       formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
       formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
       formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%

       formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
       formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>%
       formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
       formatStyle(c('TOV%'),backgroundColor = plcl.tovpct) %>%

       formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
       formatStyle(c('ORB%'),backgroundColor = plcl.orbpct) %>%
       formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
       formatStyle(c('DRB%'),backgroundColor = plcl.drbpct) %>%

       formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
       formatStyle(c('STL%'),backgroundColor = plcl.stlpct) %>%
       formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
       formatStyle(c('BLK%'),backgroundColor = plcl.blkpct) %>%

       formatStyle(c('Net Rating'),backgroundColor = plcl.netrating) %>%
       formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
       formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
       formatStyle(c('PER'),backgroundColor = plcl.per) %>% 
        
        # add solid black borders to group related stats
        formatStyle(c(1,4,8,14,16,18,20,22,24,26,30),`border-left` = '4px solid black')%>% 
        formatStyle(c(1),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam,is.numeric)), digits = 1)
        
        
    } else {
      ### DT
      DT::datatable(aaFirstTeam, rownames = FALSE,
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                   scrollX = '400px', lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 1)
                    )) %>% 
        # bold player name column
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Player'))
    }
     
      
  })
  
  ### second team
  output$AAsecond <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = ncaa[6:10,]
    rownames(aaFirstTeam) <- NULL
    
    # get checkbox status
    checkFlag = input$`aa-2-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox(aaFirstTeam, checkFlag, T)
    if (checkFlag){aaFirstTeam = aaFirstTeam %>% select(-School,
                                                        -Class,
                                                        -`Full School Name`,
                                                        -`Conference_x`)}
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam, rownames = FALSE,
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                   scrollX = '400px', lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 1)
                    )) %>% 
        # bold player name column
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Player')) %>% 
        
        formatStyle(c('USG%'),backgroundColor = plcl.usg) %>%
        
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('FG%'),backgroundColor = plcl.fg) %>%
        
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('TOV%'),backgroundColor = plcl.tovpct) %>%
        
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('ORB%'),backgroundColor = plcl.orbpct) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('DRB%'),backgroundColor = plcl.drbpct) %>%
        
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('STL%'),backgroundColor = plcl.stlpct) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('BLK%'),backgroundColor = plcl.blkpct) %>%
        
        formatStyle(c('Net Rating'),backgroundColor = plcl.netrating) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c('PER'),backgroundColor = plcl.per)%>% 
        
        # add solid black borders to group related stats
        formatStyle(c(1,4,8,14,16,18,20,22,24,26,30),`border-left` = '4px solid black')%>% 
        formatStyle(c(1),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam,is.numeric)), digits = 1)
    } else {
      ### DT
      DT::datatable(aaFirstTeam, rownames = FALSE,
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                   scrollX = '400px', lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 1)
                    )) %>% 
        # bold player name column
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Player'))
    }
    
  })
  
  # third team
  output$AAthird <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = ncaa[11:15,]
    rownames(aaFirstTeam) <- NULL
    
    # get checkbox status
    checkFlag = input$`aa-3-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox(aaFirstTeam, checkFlag, T)
    if (checkFlag){aaFirstTeam = aaFirstTeam %>% select(-School,
                                                        -Class,
                                                        -`Full School Name`,
                                                        -`Conference_x`)}
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam, rownames = FALSE,
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                   scrollX = '400px', lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 1)
                    )) %>% 
        # bold player name column
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Player')) %>% 
        
        formatStyle(c('USG%'),backgroundColor = plcl.usg) %>%
        
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('FG%'),backgroundColor = plcl.fg) %>%
        
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('TOV%'),backgroundColor = plcl.tovpct) %>%
        
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('ORB%'),backgroundColor = plcl.orbpct) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('DRB%'),backgroundColor = plcl.drbpct) %>%
        
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('STL%'),backgroundColor = plcl.stlpct) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('BLK%'),backgroundColor = plcl.blkpct) %>%
        
        formatStyle(c('Net Rating'),backgroundColor = plcl.netrating) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c('PER'),backgroundColor = plcl.per)%>% 
        
        # add solid black borders to group related stats
        formatStyle(c(1,4,8,14,16,18,20,22,24,26,30),`border-left` = '4px solid black')%>% 
        formatStyle(c(1),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam,is.numeric)), digits = 1)
    } else {
      ### DT
      DT::datatable(aaFirstTeam, rownames = FALSE,
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                   scrollX = '400px', lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 1)
                    )) %>% 
        # bold player name column
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Player'))
    }
    
  })
  
  # honorable mentions
  output$AAhm <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = ncaa[16:30,]
    rownames(aaFirstTeam) <- NULL
    
    # get checkbox status
    checkFlag = input$`aa-hm-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox(aaFirstTeam, checkFlag, T)
    if (checkFlag){aaFirstTeam = aaFirstTeam %>% select(-School,
                                                        -Class,
                                                        -`Full School Name`,
                                                        -`Conference_x`)}
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam, rownames = FALSE,
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                   scrollX = '400px', lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 1)
                    )) %>% 
        # bold player name column
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Player')) %>% 
        
        formatStyle(c('USG%'),backgroundColor = plcl.usg) %>%
        
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('FG%'),backgroundColor = plcl.fg) %>%
        
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('AST%'),backgroundColor = plcl.astpct) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('TOV%'),backgroundColor = plcl.tovpct) %>%
        
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('ORB%'),backgroundColor = plcl.orbpct) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('DRB%'),backgroundColor = plcl.drbpct) %>%
        
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('STL%'),backgroundColor = plcl.stlpct) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('BLK%'),backgroundColor = plcl.blkpct) %>%
        
        formatStyle(c('Net Rating'),backgroundColor = plcl.netrating) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c('PER'),backgroundColor = plcl.per)%>% 
        
        # add solid black borders to group related stats
        formatStyle(c(1,4,8,14,16,18,20,22,24,26,30),`border-left` = '4px solid black')%>% 
        formatStyle(c(1),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam,is.numeric)), digits = 1)
    } else {
      ### DT
      DT::datatable(aaFirstTeam, rownames = FALSE,
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                   scrollX = '400px', lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 1)
                    )) %>% 
        # bold player name column
        formatStyle(fontsize = 14,fontWeight = 'bold',columns = c('Player'))
    }
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###### Power 6 All-Conference ######
  
  
  
  
  
  
  
  
  
  
  ### Big 12
  output$big12 <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = big12[1:25,]
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    aaFirstTeam = aaFirstTeam %>% arrange(desc(Prediction))
    aaFirstTeam1 = big12[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = big12[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = big12[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = big12[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`big12-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam1, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(c("All-Conference Team"), backgroundColor = styleEqual(c(rep("1st Team", 5), 
                                                                             rep("2nd Team", 5),
                                                                             rep("3rd Team", 5),
                                                                             rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam1,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam1, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam1), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
    
  })
  
  
  ### Big 12
  output$`big12-2` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = big12[1:25,]
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    aaFirstTeam = aaFirstTeam %>% arrange(desc(Prediction))
    aaFirstTeam1 = big12[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = big12[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = big12[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = big12[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`big12-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam2, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                     list(width = '200px', targets = c(0)
                                                          )
                                                     ),
                                   scrollX = '400px',
                                   pageLength = 25,
                                   lengthChange = FALSE,
                                   dom = 't',
                                   ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))
                    ) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(c("All-Conference Team"), backgroundColor = styleEqual(c(rep("1st Team", 5), 
                                                                             rep("2nd Team", 5),
                                                                             rep("3rd Team", 5),
                                                                             rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam2,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam2, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam2), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
    
  })
  
  ### Big 12
  output$`big12-3` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = big12[1:25,]
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    aaFirstTeam = aaFirstTeam %>% arrange(desc(Prediction))
    aaFirstTeam1 = big12[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = big12[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = big12[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = big12[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`big12-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam3, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(c("All-Conference Team"), backgroundColor = styleEqual(c(rep("1st Team", 5), 
                                                                             rep("2nd Team", 5),
                                                                             rep("3rd Team", 5),
                                                                             rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam3,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam3, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam3), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
    
  })
  
  ### Big 12
  output$`big12-4` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = big12[1:25,]
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    aaFirstTeam = aaFirstTeam %>% arrange(desc(Prediction))
    aaFirstTeam1 = big12[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = big12[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = big12[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = big12[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`big12-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam4, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(c("All-Conference Team"), backgroundColor = styleEqual(c(rep("1st Team", 5), 
                                                                             rep("2nd Team", 5),
                                                                             rep("3rd Team", 5),
                                                                             rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam4,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam4, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam4), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
    
  })
  
  
  
  
  
  
  
  ### Big East
  output$bigEast <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = bigEast[1:25,]
    aaFirstTeam = aaFirstTeam %>% arrange(desc(Prediction))
    aaFirstTeam1 = bigEast[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = bigEast[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = bigEast[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = bigEast[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`bigEast-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam1, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(c("All-Conference Team"), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 

        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam1,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam1, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam1), backgroundColor = styleEqual(c(rep("1st Team", 5), 
                                                                          rep("2nd Team", 5),
                                                                          rep("3rd Team", 5), 
                                                                          rep("Honorable Mention", 10)), 
                                                                        colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  ### Big East
  output$`bigEast-2` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = bigEast[1:25,]
    aaFirstTeam = aaFirstTeam %>% arrange(desc(Prediction))
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    aaFirstTeam1 = bigEast[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = bigEast[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = bigEast[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = bigEast[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`bigEast-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam2, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(c("All-Conference Team"), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam2,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam2, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam2), backgroundColor = styleEqual(c(rep("1st Team", 5), 
                                                                          rep("2nd Team", 5),
                                                                          rep("3rd Team", 5), 
                                                                          rep("Honorable Mention", 10)), 
                                                                        colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  ### Big East
  output$`bigEast-3` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = bigEast[1:25,]
    aaFirstTeam = aaFirstTeam %>% arrange(desc(Prediction))
    aaFirstTeam1 = bigEast[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = bigEast[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = bigEast[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = bigEast[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`bigEast-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam3, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(c("All-Conference Team"), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam3,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam3, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam3), backgroundColor = styleEqual(c(rep("1st Team", 5), 
                                                                           rep("2nd Team", 5),
                                                                           rep("3rd Team", 5), 
                                                                           rep("Honorable Mention", 10)), 
                                                                         colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  ### Big East
  output$`bigEast-4` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = bigEast[1:25,]
    aaFirstTeam = aaFirstTeam %>% arrange(desc(Prediction))
    aaFirstTeam1 = bigEast[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = bigEast[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = bigEast[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = bigEast[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`bigEast-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam4, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(c("All-Conference Team"), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam4,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam4, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam4), backgroundColor = styleEqual(c(rep("1st Team", 5), 
                                                                           rep("2nd Team", 5),
                                                                           rep("3rd Team", 5), 
                                                                           rep("Honorable Mention", 10)), 
                                                                         colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  
  
  
  
  
  ### Big Ten
  output$bigTen <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = bigTen[1:25,]
    aaFirstTeam1 = bigTen[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = bigTen[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = bigTen[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = bigTen[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`bigTen-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam1, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam1), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam1,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam1, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam1), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  ### Big Ten
  output$`bigTen-2` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = bigTen[1:25,]
    aaFirstTeam1 = bigTen[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = bigTen[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = bigTen[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = bigTen[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`bigTen-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam2, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam2), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam2,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam2, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam2), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  ### Big Ten
  output$`bigTen-3` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = bigTen[1:25,]
    aaFirstTeam1 = bigTen[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = bigTen[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = bigTen[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = bigTen[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`bigTen-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam3, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam3), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam3,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam3, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam3), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  ### Big Ten
  output$`bigTen-4` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = bigTen[1:25,]
    aaFirstTeam1 = bigTen[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = bigTen[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = bigTen[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = bigTen[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`bigTen-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam4, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam4), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam4,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam4, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam4), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  
  
  ### SEC
  output$SEC <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = SEC[1:25,]
    aaFirstTeam1 = SEC[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = SEC[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = SEC[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = SEC[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`sec-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam1, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam1), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam1,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam1, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam1), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  ### SEC
  output$`SEC-2` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = SEC[1:25,]
    aaFirstTeam1 = SEC[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = SEC[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = SEC[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = SEC[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`sec-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam2, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam2), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam2,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam2, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam2), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  ### SEC
  output$`SEC-3` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = SEC[1:25,]
    aaFirstTeam1 = SEC[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = SEC[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = SEC[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = SEC[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`sec-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam3, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam3), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam3,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam3, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam3), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  ### SEC
  output$`SEC-4` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = SEC[1:25,]
    aaFirstTeam1 = SEC[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = SEC[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = SEC[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = SEC[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`sec-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam4, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam4), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam4,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam4, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam4), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  
  
  ### ACC
  output$ACC <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = ACC[1:25,]
    aaFirstTeam1 = ACC[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = ACC[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = ACC[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = ACC[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team`=c(rep("1st Team", 5),rep("2nd Team", 5),rep("3rd Team", 5),rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5),rep("#b08ef5", 5),rep("#b08ef5", 5),rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`acc-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam1, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam1), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam1,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam1, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam1), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  
  ### ACC
  output$`ACC-2` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = ACC[1:25,]
    aaFirstTeam1 = ACC[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = ACC[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
    aaFirstTeam3 = ACC[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = ACC[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team`=c(rep("1st Team", 5),rep("2nd Team", 5),rep("3rd Team", 5),rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5),rep("#b08ef5", 5),rep("#b08ef5", 5),rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`acc-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam2, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam2), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam2,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam2, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam2), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  ### ACC
  output$`ACC-3` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = ACC[1:25,]
    aaFirstTeam1 = ACC[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = ACC[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2ns Team", 5)
    aaFirstTeam3 = ACC[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = ACC[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team`=c(rep("1st Team", 5),rep("2nd Team", 5),rep("3rd Team", 5),rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5),rep("#b08ef5", 5),rep("#b08ef5", 5),rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`acc-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam3, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam3), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam3,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam3, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam3), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  ### ACC
  output$`ACC-4` <- DT::renderDataTable({
    
    # get data
    aaFirstTeam = ACC[1:25,]
    aaFirstTeam1 = ACC[1:5,]
    aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
    aaFirstTeam2 = ACC[6:10,]
    aaFirstTeam2$`All-Conference Team` = rep("2ns Team", 5)
    aaFirstTeam3 = ACC[11:15,]
    aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
    aaFirstTeam4 = ACC[16:25,]
    aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
    #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
    
    
    # add column for all-conference team and alternate colors
    aaFirstTeam$`All-Conference Team`=c(rep("1st Team", 5),rep("2nd Team", 5),rep("3rd Team", 5),rep("Honorable Mention", 10))
    colors = c(rep("#b08ef5", 5),rep("#b08ef5", 5),rep("#b08ef5", 5),rep("lightgrey", 10))
    
    # get checkbox status
    checkFlag = input$`acc-checkbox`
    
    # prepare data based on checkbox
    aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
    
    aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
    aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
    aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
    aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
    
    colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
    colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
    #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
    
    if (checkFlag){
      ### DT
      DT::datatable(aaFirstTeam4, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam4), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
        formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
        formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
        formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
        formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
        formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
        formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
        formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
        formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
        formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
        formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
        formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
        formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
        formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
        formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
        formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
        formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
        formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
        #formatStyle(c('DRtg'),backgroundColor = plcl.drtg) %>%
        formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
        formatRound(which(sapply(aaFirstTeam4,is.numeric)), digits = 1) %>% 
        formatStyle(c(1,9,15,18,20,24),`border-left` = '4px solid black')
        #formatStyle(c(1,11,17,20,22,26,28),`border-left` = '4px solid black')
    } else {
      ### DT
      DT::datatable(aaFirstTeam4, rownames = FALSE, 
                    
                    # extension to keep team name locked from scroll
                    extensions = "FixedColumns",
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
                                   scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
                                   
                                   # keep player name locked from scroll
                                   fixedColumns = list(leftColumns = 2))) %>% 
        ### styling
        
        # color all-conference team alternating between purple and grey
        formatStyle(colnames(aaFirstTeam4), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
        
        # bold player and all-conference team columns
        formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
    }
  })
  
  
  
  
  ######### Pac-12 All Conference Teams
  
  
  
  
  
  
  # ### Pac-12
  # output$pac12 <- DT::renderDataTable({
  #   
  #   # get data
  #   aaFirstTeam = pac12[1:25,]
  #   #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
  #   
  #   aaFirstTeam1 = pac12[1:5,]
  #   aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
  #   aaFirstTeam2 = pac12[6:10,]
  #   aaFirstTeam2$`All-Conference Team` = rep("2ns Team", 5)
  #   aaFirstTeam3 = pac12[11:15,]
  #   aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
  #   aaFirstTeam4 = pac12[16:25,]
  #   aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
  #   # add column for all-conference team and alternate colors
  #   aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5),rep("2nd Team", 5),rep("3rd Team", 5),rep("Honorable Mention", 10))
  #   colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
  #   
  #   # get checkbox status
  #   checkFlag = input$`pac12-checkbox`
  #   
  #   # prepare data based on checkbox
  #   aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
  #   
  #   aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
  #   aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
  #   aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
  #   aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
  #   
  #   
  #   colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
  #   colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
  #   #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
  #   
  #   if (checkFlag){
  #     ### DT
  #     DT::datatable(aaFirstTeam1, rownames = FALSE, 
  #                   
  #                   # extension to keep team name locked from scroll
  #                   extensions = "FixedColumns",
  #                   options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
  #                                  scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
  #                                  
  #                                  # keep player name locked from scroll
  #                                  fixedColumns = list(leftColumns = 2))) %>% 
  #       ### styling
  #       
  #       # color all-conference team alternating between purple and grey
  #       formatStyle(colnames(aaFirstTeam1), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
  #       
  #       # bold player and all-conference team columns
  #       formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
  #       formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
  #       formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
  #       formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
  #       formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
  #       formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
  #       formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
  #       formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
  #       formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
  #       formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
  #       formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
  #       formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
  #       formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
  #       formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
  #       formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
  #       formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
  #       formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
  #       formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
  #       formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
  #       formatRound(which(sapply(aaFirstTeam1,is.numeric)), digits = 1) %>% 
  #       formatStyle(c(1,9,15,18,20,22),`border-left` = '4px solid black')
  #   } else {
  #     ### DT
  #     DT::datatable(aaFirstTeam1, rownames = FALSE, 
  #                   
  #                   # extension to keep team name locked from scroll
  #                   extensions = "FixedColumns",
  #                   options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
  #                                  scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
  #                                  
  #                                  # keep player name locked from scroll
  #                                  fixedColumns = list(leftColumns = 2))) %>% 
  #       ### styling
  #       
  #       # color all-conference team alternating between purple and grey
  #       formatStyle(colnames(aaFirstTeam1), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
  #       
  #       # bold player and all-conference team columns
  #       formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
  #     
  #     
  #   }
  # })
  
  
  
  # ### Pac-12
  # output$`pac12-2` <- DT::renderDataTable({
  #   
  #   # get data
  #   aaFirstTeam = pac12[1:25,]
  #   #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
  #   
  #   aaFirstTeam1 = pac12[1:5,]
  #   aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
  #   aaFirstTeam2 = pac12[6:10,]
  #   aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
  #   aaFirstTeam3 = pac12[11:15,]
  #   aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
  #   aaFirstTeam4 = pac12[16:25,]
  #   aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
  #   # add column for all-conference team and alternate colors
  #   aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5),rep("2nd Team", 5),rep("3rd Team", 5),rep("Honorable Mention", 10))
  #   colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
  #   
  #   # get checkbox status
  #   checkFlag = input$`pac12-checkbox`
  #   
  #   # prepare data based on checkbox
  #   aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
  #   
  #   aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
  #   aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
  #   aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
  #   aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
  #   
  #   
  #   colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
  #   colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
  #   #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
  #   
  #   if (checkFlag){
  #     ### DT
  #     DT::datatable(aaFirstTeam2, rownames = FALSE, 
  #                   
  #                   # extension to keep team name locked from scroll
  #                   extensions = "FixedColumns",
  #                   options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
  #                                  scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
  #                                  
  #                                  # keep player name locked from scroll
  #                                  fixedColumns = list(leftColumns = 2))) %>% 
  #       ### styling
  #       
  #       # color all-conference team alternating between purple and grey
  #       formatStyle(colnames(aaFirstTeam2), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
  #       
  #       # bold player and all-conference team columns
  #       formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
  #       formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
  #       formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
  #       formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
  #       formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
  #       formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
  #       formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
  #       formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
  #       formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
  #       formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
  #       formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
  #       formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
  #       formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
  #       formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
  #       formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
  #       formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
  #       formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
  #       formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
  #       
  #       # add solid black borders to group related stats
  #       formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
  #       formatRound(which(sapply(aaFirstTeam2,is.numeric)), digits = 1) %>% 
  #       formatStyle(c(1,9,15,18,20,22),`border-left` = '4px solid black')
  #   } else {
  #     ### DT
  #     DT::datatable(aaFirstTeam2, rownames = FALSE, 
  #                   
  #                   # extension to keep team name locked from scroll
  #                   extensions = "FixedColumns",
  #                   options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
  #                                  scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
  #                                  
  #                                  # keep player name locked from scroll
  #                                  fixedColumns = list(leftColumns = 2))) %>% 
  #       ### styling
  #       
  #       # color all-conference team alternating between purple and grey
  #       formatStyle(colnames(aaFirstTeam2), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
  #       
  #       # bold player and all-conference team columns
  #       formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
  #     
  #     
  #   }
  # })
  
  # ### Pac-12
  # output$`pac12-3` <- DT::renderDataTable({
  #   
  #   # get data
  #   aaFirstTeam = pac12[1:25,]
  #   #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
  #   
  #   aaFirstTeam1 = pac12[1:5,]
  #   aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
  #   aaFirstTeam2 = pac12[6:10,]
  #   aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
  #   aaFirstTeam3 = pac12[11:15,]
  #   aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
  #   aaFirstTeam4 = pac12[16:25,]
  #   aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
  #   # add column for all-conference team and alternate colors
  #   aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5),rep("2nd Team", 5),rep("3rd Team", 5),rep("Honorable Mention", 10))
  #   colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
  #   
  #   # get checkbox status
  #   checkFlag = input$`pac12-checkbox`
  #   
  #   # prepare data based on checkbox
  #   aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
  #   
  #   aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
  #   aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
  #   aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
  #   aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
  #   
  #   
  #   colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
  #   colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
  #   #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
  #   
  #   if (checkFlag){
  #     ### DT
  #     DT::datatable(aaFirstTeam3, rownames = FALSE, 
  #                   
  #                   # extension to keep team name locked from scroll
  #                   extensions = "FixedColumns",
  #                   options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
  #                                  scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
  #                                  
  #                                  # keep player name locked from scroll
  #                                  fixedColumns = list(leftColumns = 2))) %>% 
  #       ### styling
  #       
  #       # color all-conference team alternating between purple and grey
  #       formatStyle(colnames(aaFirstTeam3), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
  #       
  #       # bold player and all-conference team columns
  #       formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
  #       formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
  #       formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
  #       formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
  #       formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
  #       formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
  #       formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
  #       formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
  #       formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
  #       formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
  #       formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
  #       formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
  #       formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
  #       formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
  #       formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
  #       formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
  #       formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
  #       formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
  #       formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
  #       formatRound(which(sapply(aaFirstTeam3,is.numeric)), digits = 1) %>% 
  #       formatStyle(c(1,9,15,18,20,22),`border-left` = '4px solid black')
  #   } else {
  #     ### DT
  #     DT::datatable(aaFirstTeam3, rownames = FALSE, 
  #                   
  #                   # extension to keep team name locked from scroll
  #                   extensions = "FixedColumns",
  #                   options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
  #                                  scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
  #                                  
  #                                  # keep player name locked from scroll
  #                                  fixedColumns = list(leftColumns = 2))) %>% 
  #       ### styling
  #       
  #       # color all-conference team alternating between purple and grey
  #       formatStyle(colnames(aaFirstTeam3), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
  #       
  #       # bold player and all-conference team columns
  #       formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
  #     
  #     
  #   }
  # })
  
  # ## Pac-12
  # output$`pac12-4` <- DT::renderDataTable({
  #   
  #   # get data
  #   aaFirstTeam = pac12[1:25,]
  #   #aaFirstTeam = aaFirstTeam %>% dplyr::select(-Team)
  #   
  #   aaFirstTeam1 = pac12[1:5,]
  #   aaFirstTeam1$`All-Conference Team` = rep("1st Team", 5)
  #   aaFirstTeam2 = pac12[6:10,]
  #   aaFirstTeam2$`All-Conference Team` = rep("2nd Team", 5)
  #   aaFirstTeam3 = pac12[11:15,]
  #   aaFirstTeam3$`All-Conference Team` = rep("3rd Team", 5)
  #   aaFirstTeam4 = pac12[16:25,]
  #   aaFirstTeam4$`All-Conference Team` = rep("Honorable Mention", 10)
  #   # add column for all-conference team and alternate colors
  #   aaFirstTeam$`All-Conference Team` = c(rep("1st Team", 5),rep("2nd Team", 5),rep("3rd Team", 5),rep("Honorable Mention", 10))
  #   colors = c(rep("#b08ef5", 5), rep("#b08ef5", 5), rep("#b08ef5", 5), rep("lightgrey", 10))
  #   
  #   # get checkbox status
  #   checkFlag = input$`pac12-checkbox`
  #   
  #   # prepare data based on checkbox
  #   aaFirstTeam=applyCheckbox2(aaFirstTeam, checkFlag, F)
  #   
  #   aaFirstTeam1=applyCheckbox2(aaFirstTeam1, checkFlag, F)
  #   aaFirstTeam2=applyCheckbox2(aaFirstTeam2, checkFlag, F)
  #   aaFirstTeam3=applyCheckbox2(aaFirstTeam3, checkFlag, F)
  #   aaFirstTeam4=applyCheckbox2(aaFirstTeam4, checkFlag, F)
  #   
  #   
  #   colnames(aaFirstTeam)=gsub("School", "Conference", colnames(aaFirstTeam))
  #   colnames(aaFirstTeam)=gsub("Full Conference Name", "School", colnames(aaFirstTeam))
  #   #aaFirstTeam = aaFirstTeam %>% select(-Conference, -`KenPom%Rank`)
  #   
  #   if (checkFlag){
  #     ### DT
  #     DT::datatable(aaFirstTeam4, rownames = FALSE, 
  #                   
  #                   # extension to keep team name locked from scroll
  #                   extensions = "FixedColumns",
  #                   options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
  #                                  scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
  #                                  
  #                                  # keep player name locked from scroll
  #                                  fixedColumns = list(leftColumns = 2))) %>% 
  #       ### styling
  #       
  #       # color all-conference team alternating between purple and grey
  #       formatStyle(colnames(aaFirstTeam4), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
  #       
  #       # bold player and all-conference team columns
  #       formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'player')) %>% 
  #       formatStyle(c('PPG'),backgroundColor = plcl.ppg) %>%
  #       formatStyle(c('TS%'),backgroundColor = plcl.ts) %>%
  #       formatStyle(c('eFG%'),backgroundColor = plcl.efg) %>%
  #       formatStyle(c('3P%'),backgroundColor = tmcl.3p) %>%
  #       formatStyle(c('TOV'),backgroundColor = plcl.tov) %>%
  #       formatStyle(c('ORB'),backgroundColor = plcl.orb) %>%
  #       formatStyle(c('APG'),backgroundColor = plcl.apg) %>%
  #       formatStyle(c('3PA'),backgroundColor = plcl.3pa) %>%
  #       formatStyle(c('DRB'),backgroundColor = plcl.drb) %>%
  #       formatStyle(c('SPG'),backgroundColor = plcl.spg) %>%
  #       formatStyle(c('BPG'),backgroundColor = plcl.bpg) %>%
  #       formatStyle(c('FTA'),backgroundColor = plcl.fta) %>%
  #       formatStyle(c('FT%'),backgroundColor = plcl.ft) %>%
  #       formatStyle(c('2PA'),backgroundColor = plcl.2pa) %>%
  #       formatStyle(c('2P%'),backgroundColor = tmcl.2p) %>%
  #       formatStyle(c('ORtg'),backgroundColor = plcl.ortg) %>%
  #       formatStyle(c('AST:TOV'),backgroundColor = plcl.ratio) %>%
  #       formatStyle(c(2, 5),`border-right` = '4px solid black')%>% 
  #       formatRound(which(sapply(aaFirstTeam4,is.numeric)), digits = 1) %>% 
  #       formatStyle(c(1,9,15,18,20,22),`border-left` = '4px solid black')
  #     
  #   } else {
  #     ### DT
  #     DT::datatable(aaFirstTeam4, rownames = FALSE, 
  #                   
  #                   # extension to keep team name locked from scroll
  #                   extensions = "FixedColumns",
  #                   options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"),list(width = '200px', targets = c(0))),
  #                                  scrollX = '400px', pageLength = 25, lengthChange = FALSE, dom = 't',ordering=F,
  #                                  
  #                                  # keep player name locked from scroll
  #                                  fixedColumns = list(leftColumns = 2))) %>% 
  #       ### styling
  #       
  #       # color all-conference team alternating between purple and grey
  #       formatStyle(colnames(aaFirstTeam4), backgroundColor = styleEqual(c(rep("1st Team", 5), rep("2nd Team", 5), rep("3rd Team", 5), rep("Honorable Mention", 10)), colors)) %>% 
  #       
  #       # bold player and all-conference team columns
  #       formatStyle(fontsize = 20,fontWeight = 'bold',columns = c('All-Conference Team', 'Player'))
  #     
  #     
  #   }
  # })
  
  
  
}

thematic_shiny()
shinyApp(ui, server)
