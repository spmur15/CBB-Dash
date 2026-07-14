import pandas as pd
import requests
import re
from bs4 import BeautifulSoup

######## GET KENPOM DATA ########

def get_kenpom(year):

    kp = pd.read_csv('../teamCSVs/summary'+str(year)+'_pt.csv')
    kpO = pd.read_csv('../teamCSVs/offense'+str(year)+'.csv')
    kpD = pd.read_csv('../teamCSVs/defense'+str(year)+'.csv')
    
    temp = kp.merge(kpO, left_on='TeamName', right_on='TeamName')
    temp = temp.merge(kpD, left_on='TeamName', right_on='TeamName')

    try:kp = kp.drop(['seed'], axis=1)
    except:pass

    return kp

## Get kenpom 2022 as a baseline DF to append to
kp = pd.read_csv('../teamCSVs/summary22.csv')
kpO = pd.read_csv('../teamCSVs/offense22.csv')
kpD = pd.read_csv('../teamCSVs/defense22.csv')

temp = kp.merge(kpO, left_on='TeamName', right_on='TeamName')
temp = temp.merge(kpD, left_on='TeamName', right_on='TeamName')

try:kp = kp.drop(['seed'], axis=1)
except:pass

for i in range(3,22):
    if len(str(i)) == 1:
        i = '0'+str(i)
    else:
        i = str(i)
        
    kp = kp.append(get_kenpom(i)).reset_index(drop=True)
    
kp.rename(columns = {'Season':'Year','TeamName':'Team'}, inplace=True)
kp = kp.loc[:,~kp.columns.duplicated()].copy()

### ADJUST KENPOM TEAM NAMES FOR MERGE ###
newTeams = []
for i in kp['Team']:
    temp = re.sub('BYU', 'Brigham Young', i)
    temp = re.sub('\.', '', temp)
    temp = re.sub('Mississippi$', 'Ole Miss', temp)
    temp = re.sub('^St ', 'St. ', temp)
    temp = re.sub(' St$', ' State', temp)
    temp = re.sub(' St ', ' State ', temp)
    temp = re.sub('^Connecticut$', 'UConn', temp)
    temp = re.sub('^FIU$', 'Florida International', temp)
    temp = re.sub('^Purdue Fort Wayne$', 'Fort Wayne', temp)
    temp = re.sub('^Central Connecticut$', 'Central Connecticut State', temp)
    temp = re.sub('^LIU$', 'Long Island', temp)
    temp = re.sub('^Middle Tennessee$', 'Middle Tennessee State', temp)
    temp = re.sub('^Nebraska Omaha$', 'Omaha', temp)
    temp = re.sub('^Texas A&M Corpus Chris$', 'Texas A&M CC', temp)
    temp = re.sub('^UTSA$', 'UT San Antonio', temp)
    temp = re.sub('^VMI$', 'Virginia Military', temp)
    temp = re.sub('^Arkansas Little Rock$', 'Little Rock', temp)
    temp = re.sub('^College of Charleston$', 'Charleston', temp)
    temp = re.sub('^Detroit$', 'Detroit Mercy', temp)
    temp = re.sub('^IPFW$', 'Fort Wayne', temp)
    temp = re.sub('^Houston Baptist$', 'Houston Christian', temp)
    temp = re.sub('^LIU Brooklyn$', 'Long Island', temp)
    temp = re.sub('^Louisiana Lafayette$', 'Louisiana', temp)
    temp = re.sub('^Dixie State$', 'Utah Tech', temp)
    temp = re.sub('^Mount State Mary\'s$', 'Mount Saint Mary\'s', temp)
    temp = re.sub('^North Carolina State$', 'NC State', temp)
    temp = re.sub('^Southwest Missouri State$', 'Missouri State', temp)
    temp = re.sub('^Southwest Texas State$', 'Texas State', temp)
    temp = re.sub('^Texas Pan American$', 'UT Rio Grande Valley', temp)
    temp = re.sub('^Troy State$', 'Troy', temp)
    temp = re.sub('^Utah Valley State$', 'Utah Valley', temp)
    temp = temp.strip()
    newTeams.append(temp)
    
kp['Team'] = pd.Series(newTeams)


####### GET REALGM TEAM STATS #######


def get_team_stats_year(year):
    oAvg = pd.read_html('https://basketball.realgm.com/ncaa/team-stats/'+str(year)+'/Averages/Team_Totals/0')[0]
    dAvg = pd.read_html('https://basketball.realgm.com/ncaa/team-stats/'+str(year)+'/Averages/Opponent_Totals/0')[0]
    oAdv = pd.read_html('https://basketball.realgm.com/ncaa/team-stats/'+str(year)+'/Advanced_Stats/Team_Totals/0')[0]
    dAdv = pd.read_html('https://basketball.realgm.com/ncaa/team-stats/'+str(year)+'/Advanced_Stats/Opponent_Totals/0')[0]
    
    Adv = oAdv.merge(dAdv, on='Team', suffixes=('', '_Opp'))
    Avg = oAvg.merge(dAvg, on='Team', suffixes=('', '_Opp'))
    statsNew = Adv.merge(Avg, on='Team')
    statsNew['Year'] = year
    
    # ADJUST REALGM TEAM NAME FOR MERGE
    newTeams=[]
    for i in statsNew['Team']:
        i = i.strip()
        temp = re.sub('-', ' ', i)
        temp = re.sub('Miami \(FL\)', 'Miami FL', temp)
        temp = re.sub('Miami \(OH\)', 'Miami OH', temp)
        temp = re.sub('St\. Francis \(NY\)', 'St. Francis NY', temp)
        temp = re.sub('St\. Francis \(PA\)', 'St. Francis PA', temp)
        temp = re.sub('Loyola \(IL\)', 'Loyola Chicago', temp)
        temp = re.sub('Loyola \(MD\)', 'Loyola MD', temp)
        temp = re.sub('Army West Point', 'Army', temp)
        temp = re.sub('American University', 'American', temp)
        temp = re.sub('^Uconn$', 'UConn', temp)
        temp = re.sub('California Baptist', 'Cal Baptist', temp)
        temp = re.sub('Merrimack College', 'Merrimack', temp)
        temp = re.sub('N\.J\.I\.T\.', 'NJIT', temp)
        temp = re.sub(' St\. ', ' State ', temp)
        temp = re.sub('Pennsylvania', 'Penn', temp)
        temp = re.sub('Queens University', 'Queens', temp)
        temp = re.sub('Southern Methodist', 'SMU', temp)
        temp = re.sub('St\. Thomas Minnesota', 'St. Thomas', temp)
        temp = re.sub('Stephen F\. Austin', 'Stephen F Austin', temp)
        temp = re.sub('Southern Mississippi', 'Southern Miss', temp)
        temp = re.sub('Texas RGV', 'UT Rio Grande Valley', temp)
        temp = re.sub('Texas Arlington', 'UT Arlington', temp)
        temp = re.sub('Texas San Antonio', 'UT San Antonio', temp)
        temp = re.sub('^Mount State Mary\'s$', 'Mount Saint Mary\'s', temp)
        temp = re.sub('Texas Christian', 'TCU', temp)
        temp = temp.strip()
        newTeams.append(temp)
    statsNew['Team'] = pd.Series(newTeams)
    
    return statsNew 
    
    
## Get 2022 stats as a baseline DF to append to
oAvg22 = pd.read_html('https://basketball.realgm.com/ncaa/team-stats/2022/Averages/Team_Totals/0')[0]
dAvg22 = pd.read_html('https://basketball.realgm.com/ncaa/team-stats/2022/Averages/Opponent_Totals/0')[0]
oAdv22 = pd.read_html('https://basketball.realgm.com/ncaa/team-stats/2022/Advanced_Stats/Team_Totals/0')[0]
dAdv22 = pd.read_html('https://basketball.realgm.com/ncaa/team-stats/2022/Advanced_Stats/Opponent_Totals/0')[0]

Adv22 = oAdv22.merge(dAdv22, on='Team', suffixes=('', '_Opp'))
Avg22 = oAvg22.merge(dAvg22, on='Team', suffixes=('', '_Opp'))
stats = Adv22.merge(Avg22, on='Team')
stats['Year'] = 2022

# ADJUST REALGM TEAM NAME FOR MERGE
newTeams=[]
for i in stats['Team']:
    i = i.strip()
    temp = re.sub('-', ' ', i)
    temp = re.sub('Miami \(FL\)', 'Miami FL', temp)
    temp = re.sub('Miami \(OH\)', 'Miami OH', temp)
    temp = re.sub('St\. Francis \(NY\)', 'St. Francis NY', temp)
    temp = re.sub('St\. Francis \(PA\)', 'St. Francis PA', temp)
    temp = re.sub('Loyola \(IL\)', 'Loyola Chicago', temp)
    temp = re.sub('Loyola \(MD\)', 'Loyola MD', temp)
    temp = re.sub('Army West Point', 'Army', temp)
    temp = re.sub('American University', 'American', temp)
    temp = re.sub('^Uconn$', 'UConn', temp)
    temp = re.sub('California Baptist', 'Cal Baptist', temp)
    temp = re.sub('Merrimack College', 'Merrimack', temp)
    temp = re.sub('N\.J\.I\.T\.', 'NJIT', temp)
    temp = re.sub(' St\. ', ' State ', temp)
    temp = re.sub('Pennsylvania', 'Penn', temp)
    temp = re.sub('Queens University', 'Queens', temp)
    temp = re.sub('Southern Methodist', 'SMU', temp)
    temp = re.sub('St\. Thomas Minnesota', 'St. Thomas', temp)
    temp = re.sub('Stephen F\. Austin', 'Stephen F Austin', temp)
    temp = re.sub('Southern Mississippi', 'Southern Miss', temp)
    temp = re.sub('Texas RGV', 'UT Rio Grande Valley', temp)
    temp = re.sub('Texas Arlington', 'UT Arlington', temp)
    temp = re.sub('Texas San Antonio', 'UT San Antonio', temp)
    temp = re.sub('^Mount State Mary\'s$', 'Mount Saint Mary\'s', temp)
    temp = re.sub('Texas Christian', 'TCU', temp)
    temp = temp.strip()
    newTeams.append(temp)
stats['Team'] = pd.Series(newTeams)   
    
for i in range(2003, 2022):
    stats = stats.append(get_team_stats_year(i)).reset_index(drop=True)

train = stats.merge(kp, on=['Team','Year'])

print(len(train))
print(len(kp))
print(len(stats))
### WRITE OUT ###

def write_csv_from_pd(df,fileName):
    file = open('../teamCSVs/' + fileName+".csv", 'w')
    file.write(df.to_csv())
    file.close()

write_csv_from_pd(train, 'train')

