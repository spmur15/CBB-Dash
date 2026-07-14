import pandas as pd
import re
import datetime
import requests
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)
warnings.filterwarnings("ignore")

update = datetime.datetime.now().strftime("%B %d, %Y")

headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3'
}

################# 1 REAL GM #################
ap = pd.read_html('https://www.ncaa.com/rankings/basketball-men/d1/associated-press')[0]
ap['TEAM'][ap['TEAM'].str.contains("\(")] = ap['TEAM'].str.replace("\(.*\)", "").str.strip()

### 1) GET ALL ~360 TEAM STATS FROM REALGM
oAvg23 = requests.get('https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0', headers=headers)#[0]
oAvg23 = pd.read_html(oAvg23.text)[0]
dAvg23 = requests.get('https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Opponent_Totals/0', headers=headers)#[0]
dAvg23 = pd.read_html(dAvg23.text)[0]
oAdv23 = requests.get('https://basketball.realgm.com/ncaa/team-stats/2025/Advanced_Stats/Team_Totals/0', headers=headers)#[0]
oAdv23 = pd.read_html(oAdv23.text)[0]
dAdv23 = requests.get('https://basketball.realgm.com/ncaa/team-stats/2025/Advanced_Stats/Opponent_Totals/0', headers=headers)#[0]
dAdv23 = pd.read_html(dAdv23.text)[0]


# 1.1) MERGE CURRENT SEASON STATS
Adv23 = oAdv23.merge(dAdv23, on='Team', suffixes=('', '_Opp'))
Avg23 = oAvg23.merge(dAvg23, on='Team', suffixes=('', '_Opp'))
stats = Adv23.merge(Avg23, on='Team')
stats['Year'] = 2025

# Create a new row as a dictionary
new_row = {'Team': 'Mercyhurst'}
#new_row2 = {'Team': 'Merrimack'}

# Add the new row using loc
stats.loc[len(stats)] = new_row
#stats.loc[len(stats)] = new_row2


# 1.2) ADJUST CURRENT SEASON STATS TEAM NAMES FOR MERGE WITH KP & SOS DATA
newTeams=[]
for i in stats['Team']:
    
    i = i.strip()
    temp = re.sub('-', ' ', i)
    temp = re.sub('Miami \(FL\)', 'Miami FL', temp)
    temp = re.sub('Miami \(OH\)', 'Miami OH', temp)
    temp = re.sub('St\. Francis \(NY\)', 'St. Francis NY', temp)
    temp = re.sub('St\. Francis \(PA\)', 'Saint Francis', temp)
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
    temp = re.sub('Cal State Northridge', 'CSUN', temp)
    temp = re.sub('McNeese State', 'McNeese', temp)
    temp = re.sub('East Texas A&M', 'Texas A&M Commerce', temp)
    temp = re.sub('UMKC', 'Kansas City', temp)
    temp = re.sub('Nicholls State', 'Nicholls', temp)
    temp = re.sub('SIU Edwardsville', 'SIUE', temp)
    temp = re.sub('Southeast Missouri State', 'Southeast Missouri', temp)
    temp = re.sub("Mount State Mary's", "Mount Saint Mary's", temp)
    temp = re.sub("Fort Wayne", "Purdue Fort Wayne", temp)
    #if 'Mary' in i:print(i)
    #temp = re.sub('Arkansas-Pine Bluff', 'Arkansas Pine Bluff', i)
    #print(i)
    newTeams.append(re.sub('Texas Christian', 'TCU', temp))
stats['Team'] = pd.Series(newTeams)

print("=========== One ==============\n")
for i in sorted(stats["Team"]):print(i)
print("=========== One ==============\n\n")

################# KENPOM DATA #################



### 2) LOAD CURRENT SZN KENPOM STATS FROM RECENT DOWNLOAD
kp = pd.read_csv('../teamCSVs/summary25 (1).csv')
kp = kp[(kp["Tempo"]!=0) | (kp["TeamName"]=='Alabama A&M')].reset_index(drop=True)
#kpO = pd.read_csv('../teamCSVs/offense25.csv')
#kpD = pd.read_csv('../teamCSVs/defense25.csv')


# 2.1) MERGE KP STATS
#temp = kp.merge(kpO, left_on='TeamName', right_on='TeamName')
#temp = temp.merge(kpD, left_on='TeamName', right_on='TeamName')


# 2.2) FORMAT KP DATA
try:kp = kp.drop(['seed'], axis=1)
except:pass
kp.rename(columns = {'Season':'Year','TeamName':'Team'}, inplace=True)
kp = kp.loc[:,~kp.columns.duplicated()].copy()


# 2.3) FORMAT KP TEAM NAMES FOR MERGE
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
    #temp = re.sub('^Purdue Fort Wayne$', 'Fort Wayne', temp)
    temp = re.sub('^Central Connecticut$', 'Central Connecticut State', temp)
    temp = re.sub('^LIU$', 'Long Island', temp)
    temp = re.sub('^Middle Tennessee$', 'Middle Tennessee State', temp)
    temp = re.sub('^Nebraska Omaha$', 'Omaha', temp)
    temp = re.sub('^Texas A&M Corpus Chris$', 'Texas A&M CC', temp)
    temp = re.sub('^UTSA$', 'UT San Antonio', temp)
    temp = re.sub('^VMI$', 'Virginia Military', temp)
    temp = re.sub('East Texas A&M', 'Texas A&M Commerce', temp)
    temp = re.sub("Mount State Mary's", "Mount Saint Mary's", temp)
    #print(i)
    newTeams.append(temp)
    
kp['Team'] = pd.Series(newTeams)

# print((stats))
# print((kp))
# teamCheck = dict(map(lambda i,j : (i,j) , sorted(list(stats['Team'])),sorted(list(kp['Team']))))
# for i in teamCheck.items():
#     if i[0] != i[1]:
#         print(i[0], ":", i[1])
        
# 2.4) MERGE WITH TEAM STATS
train = stats.merge(kp, on=['Team'])

print("=========== Two ==============\n")
for i in sorted(kp["Team"]):print(i)
print("=========== Two ==============\n\n")

newTeams = []
for i in train['Team']:
    temp = re.sub('Brigham Young', 'BYU', i)
    temp = re.sub('^Boston University$', 'Boston U', temp)
    temp = re.sub('^Massachusetts$', 'UMass', temp)
    newTeams.append(temp)
train['Team'] = newTeams


################# SOS DATA #################


### 3) GET STRENGTH OF SCHEDULE DATA FROM TEAMRANKINGS
df = pd.read_html('https://www.teamrankings.com/ncaa-basketball/ranking/schedule-strength-by-other?')[0]
df['Year'] = 2025

df = df[['Team', 'Year', 'Rating']]
newTeams = []

# 3.1) FORMAT TEAM NAME FOR MERGE
for i in df['Team']:
    #print(i)
    if i == 'Merrimack': print("AHHHHHHHHHH", i)
    team = re.sub('Miami \(FL\)', 'Miami FL', i)
    team = re.sub('Miami \(OH\)', 'Miami OH', team)
    #team = re.sub('St Fran \(NY\)', 'St. Francis NY', team)
    team = re.sub('St Fran \(PA\)', 'Saint Francis', team)
    team = team.split('(')[0]
    record = '(' + i.split('(')[1]
    team = team.strip()
    team = re.sub('-', ' ', team)
    #
    team = re.sub('^N ', 'North ', team)
    team = re.sub('^S ', 'South ', team)
    team = re.sub('^W ', 'West ', team)
    team = re.sub('^E ', 'East ', team)
    team = re.sub('^C ', 'Central ', team)
    team = re.sub(' St', ' State', team)
    team = re.sub(' Stateate', ' State', team)
    team = re.sub('Miss ', 'Mississippi ', team)
    team = re.sub('VA ', 'Virginia ', team)
    team = re.sub('St ', 'Saint ', team)
    #team = re.sub('\(', '', team)
    #team = re.sub('\)', '', team)
    #team = re.sub('Miami', 'Miami FL', team)
    team = re.sub('TX Christian', 'TCU', team)
    team = re.sub('Central FL', 'UCF', team)
    team = re.sub('GA Tech', 'Georgia Tech', team)
    team = re.sub('Saint Marys', 'Saint Mary\'s', team)
    team = re.sub('Saint Johns', 'St. John\'s', team)
    #team = re.sub('Boston Col', 'Boston College', team)
    team = re.sub('Mississippi', 'Ole Miss', team)
    team = re.sub('Ole Miss State', 'Mississippi State', team)
    team = re.sub('Wash State', 'Washington State', team)
    team = re.sub('Col Charlestn', 'Charleston', team)
    team = re.sub('Sam Hous', 'Sam Houston', team)
    team = re.sub('Utah Val State', 'Utah Valley', team)
    team = re.sub('Loyola Mymt', 'Loyola Marymount', team)
    team = re.sub('James Mad', 'James Madison', team)
    team = re.sub('Fla Atlantic', 'Florida Atlantic', team)
    team = re.sub('Middle Tenn', 'Middle Tennessee', team)
    team = re.sub('LA Tech', 'Louisiana Tech', team)
    team = re.sub('TX ', 'Texas ', team)
    team = re.sub('Alab ', 'Alabama ', team)
    team = re.sub('Car ', 'Carolina ', team)
    team = re.sub('Beth Cook', 'Bethune Cook', team)
    #team = re.sub('Miami FL OH', 'Miami OH', team)
    team = re.sub('^Ste ', 'Steven ', team)
    team = re.sub('Ole Miss Val State', 'Mississippi Valley State', team)
    team = re.sub('Ark Pine Bl', 'Arkansas Pine Bluff', team)
    team = re.sub('Saint Bonavent', 'Saint Bonaventure', team)
    team = re.sub('Geo Wshgtn', 'George Washington', team)
    team = re.sub('U Mass', 'UMass', team)
    team = re.sub('Geo Mason', 'George Mason', team)
    team = re.sub('Mex ', 'Mexico ', team)
    team = re.sub('Abl ', 'Abeline', team)
    team = re.sub('^TX ', 'Texas ', team)
    team = re.sub('Jksnville', 'Jacksonville', team)
    team = re.sub('South Methodist', 'SMU', team)
    team = re.sub('Southern Methodist', 'SMU', team)
    #team = re.sub('Detroit', 'Detroit Mercy', team)
    team = re.sub('AbelineChristian', 'Abilene Christian', team)
    team = re.sub('AR Lit Rock', 'Little Rock', team)
    team = re.sub('App State', 'Appalachian State', team)
    team = re.sub('Bowling Grn', 'Bowling Green', team)
    team = re.sub('^Bethune Cook$', 'Bethune Cookman', team)
    team = re.sub('Bakersfld', 'Bakersfield', team)
    team = re.sub('^CS ', 'Cal State ', team)
    team = re.sub(' Conn$', ' Connecticut', team)
    team = re.sub(' Ark$', ' Arkansas', team)
    team = re.sub(' Mich$', ' Michigan', team)
    team = re.sub('Nrdge', 'Northridge', team)
    team = re.sub('Charl ', 'Charleston ', team)
    team = re.sub('Charleston South', 'Charleston Southern', team)
    team = re.sub('Citadel', 'The Citadel ', team)
    team = re.sub('Central Connecticut', 'Central Connecticut State', team)
    team = re.sub('Coastal Car', 'Coastal Carolina', team)
    team = re.sub('^Connecticut$', 'UConn', team)
    team = re.sub('^East Tenn State$', 'East Tennessee State', team)
    team = re.sub('^East Illinois$', 'Eastern Illinois', team)
    team = re.sub('^East Michigan$', 'Eastern Michigan', team)
    team = re.sub('^East Kentucky$', 'Eastern Kentucky', team)
    team = re.sub('^East Washingtn$', 'Eastern Washington', team)
    team = re.sub('^F Dickinson$', 'Fairleigh Dickinson', team)
    team = re.sub('^Fla Gulf Cst$', 'Florida Gulf Coast', team)
    team = re.sub('^Florida Intl$', 'Florida International', team)
    team = re.sub('^GA Southern$', 'Georgia Southern', team)
    team = re.sub('^Grd Canyon$', 'Grand Canyon', team)
    team = re.sub('^IL Chicago$', 'Illinois Chicago', team)
    team = re.sub('^Gard Webb$', 'Gardner Webb', team)
    team = re.sub('^Hsn Christian$', 'Houston Christian', team)
    team = re.sub('^IPFW$', 'Fort Wayne', team)
    team = re.sub('^Incar Word$', 'Incarnate Word', team)
    team = re.sub('^LA Lafayette$', 'Louisiana', team)
    team = re.sub('^Lg Beach State$', 'Long Beach State', team)
    team = re.sub('^LIU$', 'Long Island', team)
    team = re.sub('^LA Monroe$', 'Louisiana Monroe', team)
    team = re.sub('^Loyola Chi$', 'Loyola Chicago', team)
    team = re.sub('^Maryland ES$', 'Maryland Eastern Shore', team)
    team = re.sub('^Maryland BC$', 'UMBC', team)
    team = re.sub('^WI Grn Bay$', 'Green Bay', team)
    team = re.sub('^Middle Tennessee$', 'Middle Tennessee State', team)
    team = re.sub('^Mass Lowell$', 'UMass Lowell', team)
    team = re.sub('^Wm & Mary$', 'William & Mary', team)
    team = re.sub('^Youngs State$', 'Youngstown State', team)
    team = re.sub('^West Illinois$', 'Western Illinois', team)
    team = re.sub('^West Michigan$', 'Western Michigan', team)
    team = re.sub('^West Kentucky$', 'Western Kentucky', team)
    team = re.sub('^West Washingtn$', 'Western Washington', team)
    team = re.sub('^Mt State Marys$', 'Mount State Mary\'s', team)
    team = re.sub('^NC A&T$', 'North Carolina A&T', team)
    team = re.sub('^NC ', 'North Carolina ', team)
    team = re.sub('^WI Milwkee', 'Milwaukee ', team)
    team = re.sub('^West Carolina', 'Western Carolina ', team)
    team = re.sub('^UNC ', 'North Carolina ', team)
    team = re.sub('^Neb Omaha' , 'Omaha', team)
    team = re.sub('^North Illinois$', 'Northern Illinois', team)
    team = re.sub('^North Arizona$', 'Northern Arizona', team)
    team = re.sub('^North Colorado$', 'Northern Colorado', team)
    team = re.sub('^North Kentucky$', 'Northern Kentucky', team)
    team = re.sub('^North Iowa$', 'Northern Iowa', team)
    team = re.sub('^North Carolina State$', 'NC State', team)
    team = re.sub('^NW State$', 'Northwestern State', team)
    team = re.sub('^North Carolina Asheville$', 'UNC Asheville', team)
    team = re.sub('^North Carolina Grnsboro$', 'UNC Greensboro', team)
    team = re.sub('^North Carolina Wilmgton$', 'UNC Wilmington', team)
    team = re.sub('^North Hampshire$', 'New Hampshire', team)
    team = re.sub('^North Mexico State$', 'New Mexico State', team)
    team = re.sub('^U Penn$', 'Penn', team)
    team = re.sub('^Northeastrn$', 'Northeastern', team)
    team = re.sub('^Prairie View$', 'Prairie View A&M', team)
    team = re.sub('^Rob Morris$', 'Robert Morris', team)
    team = re.sub('^SIU Edward$', 'SIU Edwardsville', team)
    team = re.sub('^SE Missouri$', 'Southeast Missouri State', team)
    team = re.sub('^SE Louisiana$', 'Southeastern Louisiana', team)
    team = re.sub('^Sac State$', 'Sacramento State', team)
    #team = re.sub('^Sac State$', 'Sacramento State', team)
    team = re.sub('^SC Upstate$', 'USC Upstate', team)
    team = re.sub('^Sacred Hrt$', 'Sacred Heart', team)
    team = re.sub('^Saint Josephs$', 'Saint Joseph\'s', team)
    team = re.sub('^Saint Peters$', 'Saint Peter\'s', team)
    team = re.sub('^South Ole Miss$', 'Southern Miss', team)
    team = re.sub('^Saint Bonaventure$', 'St. Bonaventure', team)
    team = re.sub('^South Illinois$', 'Southern Illinois', team)
    team = re.sub('^South Indiana$', 'Southern Indiana', team)
    team = re.sub('^South Utah$', 'Southern Utah', team)
    team = re.sub('^TN ', 'Tennessee ', team)
    team = re.sub('^Texas A&M Com', 'Texas A&M Commerce ', team)
    team = re.sub('^Texas Arlington', 'UT Arlington', team)
    team = re.sub('^Texas San Ant', 'UT San Antonio', team)
    team = re.sub('^Texas El Paso', 'UTEP', team)
    team = re.sub('^Steven F Austin', 'Stephen F Austin', team)
    team = re.sub('^Santa Barbara', 'UC Santa Barbara', team)
    team = re.sub('^Texas Pan Am', 'UT Rio Grande Valley', team)
    team = re.sub('^UCSB', 'UC Santa Barbara', team)
    team = re.sub('^UCSD', 'UC San Diego', team)
    team = re.sub('^Kansas City', 'UMKC', team)
    team = re.sub("Nicholls", "Nicholls State", team)
    team = re.sub("VMI", "Virginia Military", team)
    team = re.sub("UTSA", "UT San Antonio", team)
    team = re.sub("UL Monroe", "Louisiana Monroe", team)
    team = re.sub("^Miami$", "Miami FL", team)
    team = re.sub("UMKC", "Kansas City", team)
    team = re.sub("Cal State Northridge", "CSUN", team)
    team = re.sub("McNeese State", "McNeese", team)
    team = re.sub("Nicholls State", "Nicholls", team)
    team = re.sub("Southeast Missouri State", "Southeast Missouri", team)
    team = re.sub("SIU Edwardsville", "SIUE", team)
    team = re.sub("East Texas A&M", "Texas A&M Commerce", team)
    team = re.sub("FGCU", "Florida Gulf Coast", team)
    team = re.sub("AR Pine Bluff", "Arkansas Pine Bluff", team)
    team = re.sub("Bethune", "Bethune Cookman", team)
    team = re.sub(" So$", " Southern", team)
    team = re.sub("Hou Christian", "Houston Christian", team)
    team = re.sub("G Washington", "George Washington", team)
    team = re.sub("East Washington", "Eastern Washington", team)
    team = re.sub("Hawai'i", "Hawaii", team)
    team = re.sub("J Madison", "James Madison", team)
    team = re.sub("Mt State Mary's", "Mount Saint Mary's", team)
    team = re.sub("Mt St Mary's", "Mount Saint Mary's", team)
    team = re.sub("Purdue FW", "Purdue Fort Wayne", team)
    team = re.sub("Grambling", "Grambling State", team)
    team = re.sub("North Carolina Greensboro", "UNC Greensboro", team)
    team = re.sub("North Carolina Wilmington", "UNC Wilmington", team)
    team = re.sub("SE Missouri State", "Southeast Missouri", team)
    team = re.sub("SF Austin", "Stephen F Austin", team)
    #team = re.sub("UT Rio Grande", "UT Rio Grande Valley", team)
    team = re.sub("UT Rio Grande", "UT Rio Grande Valley", team)
    team = re.sub("Sam Houstonton", "Sam Houston State", team)
    team = re.sub("Ole Miss Valley State", "Mississippi Valley State", team)
    team = re.sub("Saint John's", "St. John's", team)
    team = re.sub("Tenn Tech", "Tennessee Tech", team)
    team = re.sub("The The", "The", team)
    team = re.sub("Saint Thomas", "St. Thomas", team)
    team = re.sub("Saint Bonaventureure", "St. Bonaventure", team)
    team = re.sub("UT Martin", "Tennessee Martin", team)
    team = re.sub('East Texas A&M', 'Texas A&M Commerce', team)
    team = re.sub('Saint Francis PA', 'Saint Francis', team)
    #if 'Mary' in i:print(team)
    newTeams.append(team.strip())
df['Team'] = pd.Series(newTeams)

# print("=========== Three ==============\n")
# for i in sorted(df["Team"]):print(i)
# print("=========== Three ==============\n\n")

# 3.2) GET MASTER LIST OF POWER-6 TEAMS
f = open('../teamCSVs/powerTeams.txt', 'r')
f = f.read()
teams = f.split('\n')

# Power 6 flag
rows = []
for i in df['Team']:
    if i.strip() in teams:rows.append(True)
    else:rows.append(False)
df=df.sort_values(by='Rating', ascending=False).reset_index(drop=True)
df.columns = 'Team', 'Year', 'SOS'
dfCheck = df[rows].sort_values(by='SOS', ascending=False).reset_index(drop=True)  
rows = []
for i in train['Team']:
    if i.strip() in teams:rows.append(True)
    else:rows.append(False)
trainCheck=train[rows].reset_index(drop=True)
trainTeams = list(set(list(trainCheck['Team'])))
sosTeams = list(set(list(dfCheck['Team'])))

# print(len(train))
# print(len(df))
# teamCheck = dict(map(lambda i,j : (i,j) , sorted(list(train['Team'])),sorted(list(df['Team']))))
# for i in teamCheck.items():
#     if i[0] != i[1]:
#         print(i[0], ":", i[1]) 
train=train.merge(df, on='Team')

#print('\n-------------------\n')
#print(train[~train['_merge'].str.contains('both')]['Team'])
#print('\n-------------------\n')
#print(df)
print('after', len(train))

################# CONFERENCE DATA #################


confs = {'ACC':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/1',
         'Big 12':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/3',
         'Big East':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/59',
         #'Pac-12':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/7',
         'SEC':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/8',
         'WCC':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/11',
         'Mountain West':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/6',
         'C-USA':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/9',
         'Big Ten':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/2',
         'WAC':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/13',
         'A-10':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/10',
         'American':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/4',
         'America East': 'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/18',
         'Atlantic Sun':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/19',
         'Big Sky':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/20',
         'Big South':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/21',
         'Big West':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/22',
         'CAA':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/15',
         #'Great West':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/25',
         'Horizon League':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/12',
         #'Independents':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/23',
         'Ivy League':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/14',
         'MAAC':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/17',
         'MAC':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/25',
         'MEAC':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/26',
         'Missouri Valley':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/5',
         'Northeast':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/27',
         'Ohio Valley':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/16',
         'Patriot League':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/28',
         'Southern':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/29',
         'Southland':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/30',
         'SWAC':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/31',
         'Sun Belt':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/33',
         'Summit League':'https://basketball.realgm.com/ncaa/team-stats/2025/Averages/Team_Totals/0/32'}

# adjust conference team names
confDFs = {}
for i in confs.items():
    r = requests.get(i[1], headers=headers)
    confDFs[i[0]] = pd.read_html(r.text)[0]
    #confDFs[i[0]] = pd.read_html(i[1])[0]
    confDFs[i[0]]['Conference'] = i[0]
    confDFs[i[0]] = confDFs[i[0]][['Team', 'Conference']]
    #print(i)
    #print(len(confDFs[i[0]]))
    #print(list(set(confDFs[i[0]]['Team'])))
    #print('-----')
    newTeams=[]
    for j in confDFs[i[0]]['Team']:
        temp = re.sub('-', ' ', j)
        temp = re.sub('Miami \(FL\)', 'Miami FL', temp)
        temp = re.sub('South Methodist', 'SMU', temp)
        temp = re.sub('Southern Methodist', 'SMU', temp)
        temp = re.sub('Texas Christian', 'TCU', temp)
        temp = re.sub('American University', 'American', temp)
        temp = re.sub('Miami \(FL\)', 'Miami FL', temp)
        temp = re.sub('Miami \(OH\)', 'Miami OH', temp)
        temp = re.sub('Loyola \(IL\)', 'Loyola Chicago', temp)
        temp = re.sub('Loyola \(MD\)', 'Loyola MD', temp)
        temp = re.sub('St\. Francis \(NY\)', 'St. Francis NY', temp)
        temp = re.sub('St\. Francis \(PA\)', 'Saint Francis', temp)
        temp = re.sub('Army West Point', 'Army', temp)
        temp = re.sub('N\.J\.I\.T\.', 'NJIT', temp)
        temp = re.sub('Brigham Young', 'BYU', temp)
        temp = re.sub('Boston University', 'Boston U', temp)
        temp = re.sub('California Baptist', 'Cal Baptist', temp)
        temp = re.sub('Massachusetts', 'UMass', temp)
        temp = re.sub('Merrimack College', 'Merrimack', temp)
        temp = re.sub('Mount St.\ Mary\'s', "Mount Saint Mary's", temp)
        temp = re.sub('Pennsylvania', 'Penn', temp)
        temp = re.sub('Queens University', 'Queens', temp)
        temp = re.sub('Southern Mississippi', 'Southern Miss', temp)
        temp = re.sub("Thomas Minnesota", "Thomas", temp)
        temp = re.sub('Stephen F\. Austin', "Stephen F Austin", temp)
        temp = re.sub("Mount State Mary's", "Mount Saint Mary's", temp)
        temp = re.sub("Texas RGV", "UT Rio Grande Valley", temp)
        temp = re.sub("Texas Arlington", "UT Arlington", temp)
        temp = re.sub("Texas San Antonio", "UT San Antonio", temp)
        temp = re.sub("Mount State Mary's", "Mount Saint Mary's", temp)
        temp = re.sub("McNeese State", "McNeese", temp)
        temp = re.sub("Nicholls State", "Nicholls", temp)
        temp = re.sub("Southeast Missouri State", "Southeast Missouri", temp)
        temp = re.sub("SIU Edwardsville", "SIUE", temp)
        temp = re.sub("UMKC", "Kansas City", temp)
        temp = re.sub("Cal State Northridge", "CSUN", temp)
        temp = re.sub("East Texas A&M", "Texas A&M Commerce", temp)
        
        newTeams.append(temp)
    confDFs[i[0]]['Team'] = pd.Series(newTeams)
    
# master list of conference data for checks
confData = confDFs['Big 12'].copy()
for i in confDFs.items():
    if i[0] != 'Big 12':
        confData = pd.concat([confData, i[1]], axis=0)
        #confData = confData.append(i[1])

# adjust master list teams
newTeams=[]
for i in train['Team']:
    temp = re.sub("Mount State Mary's", "Mount Saint Mary's", i)
    newTeams.append(temp)
train['Team'] = newTeams

# Create a new row as a dictionary
new_row = {'Team': 'Mercyhurst'}
# Add the new row using loc
confData.loc[len(confData)] = new_row
confDFs['Northeast'].loc[len(confData)] = new_row
confDFs['Northeast'].loc[confDFs['Northeast']['Team'] == "Mercyhurst", 'Conference'] = 'Northeast'
# print(confDFs['Northeast'])
# print(len(train))
# print(len(confData))
# teamCheck = dict(map(lambda i,j : (i,j) , sorted(list(train['Team'])),sorted(list(confData['Team']))))
# for i in teamCheck.items():
#     if i[0] != i[1]:
#         print(i[0], ":", i[1])    

# append conference data to master list
#print(confDFs.keys())
#print(list(set(train['Conference'])))
newDF = train.merge(confDFs['Big 12'], on=['Team']).reset_index(drop=True)
for i in confDFs.items():
    if i[0] != 'Big 12':
        newDF = pd.concat([newDF, train.merge(i[1], on=['Team'])], axis=0).reset_index(drop=True)
        #newDF = newDF.append().reset_index(drop=True)
#print(len(newDF))        

newDF['Team'] = newDF['Team'].str.replace('McNeese State', 'McNeese')
newDF['Team'] = newDF['Team'].str.replace('Nicholls State', 'Nicholls')
newDF['Team'] = newDF['Team'].str.replace('UMKC', 'Kansas City')

#for i in sorted(list(newDF['Team'])):print(i)


pngs = [ 'Northern-Kentucky-Norse.png','Southern-Indiana.png','Stony-Brook-Seawolves.png','Saint-Josephs-Hawks.png',
        'UAB-Blazers.png','Utah-Utes.png',
         'RPI-Engineers.png','Baylor-Bears.png','Alabama-State-Hornets.png','Bowling-Green-Falcons.png',
        'Florida-State-Seminoles.png',
         'Fordham-Rams.png','Navy-Midshipmen.png','Alaska-Nanooks.png','Alabama-Huntsville-Chargers.png','Buffalo-Bulls.png',
         'Coppin-State-Eagles.png','Louisiana-Ragin-Cajuns.png','Auburn-Tigers.png','North-Carolina-Central-Eagles.png',
         'Eastern-Michigan-Eagles.png','Colorado-Buffaloes.png','Sacred-Heart-Pioneers.png','Monmouth-Hawks.png',
         'Alabama-Crimson-Tide.png','Idaho-State-Bengals.png','Norfolk-State-Spartans.png','Princeton-Tigers.png',
         'Eastern-Kentucky-Colonels.png','Kentucky-Wildcats.png','Samford-Bulldogs.png','Southern-Utah-Thunderbirds.png',
         'UCLA-Bruins.png','Bethune-Cookman-Wildcats.png','Northern-Arizona-Lumberjacks.png','Utah-Tech-Trailblazers.png','Appalachian-State-Mountaineers.png',
         'High-Point-Panthers.png','SIU-Edwardsville-Cougars.png','North-Alabama-Lions.png','Notre-Dame-Fighting-Irish.png',
         'South-Dakota-Coyotes.png','Texas-A&M-Commerce.png','Texas-State-Bobcats.png','South-Alabama-Jaguars.png',
         'Louisville-Cardinals.png','Stanford-Cardinal.png','Columbia-Lions.png','Pacific-Tigers.png',
         'Hampton-Pirates.png','Illinois-Fighting-Illini.png','Cal-Poly-Mustangs.png','Indiana-State-Sycamores.png',
         'Oregon-State-Beavers.png','UTPB-Falcons.png','Michigan-Wolverines.png','Montana-State-Bobcats.png','Kansas-State-Wildcats.png',
         'Fairfield-Stags.png','Middle-Tennessee-State-Blue-Raiders.png','Rice-Owls.png','Southeastern-Louisiana-Lions.png','Towson-Tigers.png',
         'San-Francisco-Dons.png','USC-Upstate-Spartans.png','Lindenwood.png','Northern-Colorado-Bears.png','Maryland-Eastern-Shore-Hawks.png',
         'Robert-Morris-Colonials.png','James-Madison-Dukes.png','Long-Island-Sharks.png','Iowa-Hawkeyes.png','Lake-Superior-State-Lakers.png',
         'Clemson-Tigers.png','Tennessee-Volunteers.png','Troy-Trojans.png','Penn-Quakers.png','Ferris-State-Bulldogs.png',
         'New-Mexico-State-Aggies.png','Arkansas-Razorbacks.png','Charleston-Cougars.png','UMass-Minutemen.png','Ohio-State-Buckeyes.png',
         'George-Washington-Colonials.png','Eastern-Illinois-Panthers.png','New-Orleans-Privateers.png','Purdue-Fort-Wayne-Mastodons.png','UConn-Huskies.png',
         'Presbyterian-Blue-Hose.png','Detroit-Mercy-Titans.png','Charlotte-49ers.png','Vermont-Catamounts.png','USC-Trojans.png','Cincinnati-Bearcats.png',
         'UNC-Asheville-Bulldogs.png','Pittsburgh-Panthers.png','Central-Connecticut-State-Blue-Devils.png','North-Florida-Ospreys.png','Texas-Longhorns.png',
         'Florida-International-Panthers.png','Marshall-Thundering-Herd.png','Missouri-Tigers.png','Portland-State-Vikings.png','Hartford-Hawks.png',
         'Saint-Peters-Peacocks.png','Jacksonville-State-Gamecocks.png','Arizona-State-Sun-Devils.png','Boston-University-Terriers.png',
         'Sam-Houston-State-Bearkats.png','Winthrop-Eagles.png','UCF-Knights.png','Central-Michigan-Chippewas.png','UT-San-Antonio-Roadrunners.png',
         'Louisiana-Tech-Bulldogs.png','Wichita-State-Shockers.png','Grand-Canyon-Antelopes.png','Washington-Huskies.png','Northwestern-State-Demons.png','Bradley-Braves.png',
         'Canisius-Golden-Griffins.png','Western-Illinois-Leathernecks.png','Seton-Hall-Pirates.png','Southern-Miss-Golden-Eagles.png','Santa-Clara-Broncos.png',
         'Alcorn-State-Braves.png','UC-Davis-Aggies.png','Loyola-Marymount-Lions.png','Tennessee-Tech-Golden-Eagles.png',
         'Oklahoma-Sooners.png','Radford-Highlanders.png','Maryland-Terrapins.png','Georgia-Southern-Eagles.png',
         'Georgetown-Hoyas.png','Bryant-Bulldogs.png','Drexel-Dragons.png','Brown-Bears.png','Incarnate-Word-Cardinals.png',
         'Florida-Gators.png','Kennesaw-State-Owls.png','Clarkson-Golden-Knights.png','Hofstra-Pride.png','La-Salle-Explorers.png',
         'Idaho-Vandals.png','Murray-State-Racers.png','Dayton-Flyers.png','Utah-Valley-Wolverines.png','Illinois-Chicago-Flames.png','Abilene-Christian-Wildcats.png',
         'Northern-Illinois-Huskies.png','St.-Francis-NY-Terriers.png','Youngstown-State-Penguins.png','Georgia-Bulldogs.png',
         'Marist-Red-Foxes.png','Northwestern-Wildcats.png','Elon-Phoenix.png','Georgia-Tech-Yellow-Jackets.png','Weber-State-Wildcats.png','UC-Irvine-Anteaters.png',
         'UNC-Greensboro-Spartans.png','Cal-Baptist-Lancers.png','Evansville-Purple-Aces.png','Arizona-Wildcats.png','Virginia-Cavaliers.png',
         'Loyola-Chicago-Ramblers.png','Portland-Pilots.png','Mercer-Bears.png','Holy-Cross-Crusaders.png','Oral-Roberts-Golden-Eagles.png',
         'Binghamton-Bearcats.png','Houston-Cougars.png','Utah-State-Aggies.png','UTEP-Miners.png','Michigan-Tech-Huskies.png',
         'LSU-Tigers.png','Cal-State-Bakersfield-Roadrunners.png','Boise-State-Broncos.png','Texas-Southern-Tigers.png',
         'Fairleigh-Dickinson-Knights.png','Chattanooga-Mocs.png','Iowa-State-Cyclones.png','Virginia-Military-Keydets.png',
         'Merrimack-Warriors.png','Colgate-Raiders.png','Missouri-State-Bears.png','Jacksonville-Dolphins.png','Ole-Miss-Rebels.png',
         'Valparaiso-Beacons.png','Eastern-Washington-Eagles.png','Illinois-State-Redbirds.png','Quinnipiac-Bobcats.png',
         'South-Dakota-State-Jackrabbits.png','VCU-Rams.png','UT-Rio-Grande-Valley-Vaqueros.png','IUPUI-Jaguars.png',
         'Louisiana-Monroe-Warhawks.png','St.-Francis-PA-Red-Flash.png','Queens.png','Georgia-State-Panthers.png','Seattle-Redhawks.png',
         'Michigan-State-Spartans.png','Arkansas-State-Red-Wolves.png','Rhode-Island-Rams.png','North-Dakota-State-Bison.png',
         'Marquette-Golden-Eagles.png','TCU-Horned-Frogs.png','New-Mexico-Lobos.png','Kent-State-Golden-Flashes.png',
         'Oklahoma-State-Cowboys.png','The-Citadel-Bulldogs.png','Drake-Bulldogs.png','Wagner-Seahawks.png','North-Carolina-Tar-Heels.png',
         'Stonehill.png','Temple-Owls.png','Bellarmine-Knights.png','Toledo-Rockets.png','Southern-Illinois-Salukis.png','Campbell-Fighting-Camels.png',
         'Cornell-Big-Red.png','New-Hampshire-Wildcats.png','Army-Black-Knights.png','Duquesne-Dukes.png','Manhattan-Jaspers.png',
         'Minnesota-Duluth-Bulldogs.png','Northern-Iowa-Panthers.png','UC-San-Diego-Toreros.png','Central-Arkansas-Bears.png',
         'Furman-Paladins.png','Liberty-Flames.png','Purdue-Boilermakers.png','Tarleton-State-Texans.png','Tulane-Green-Wave.png','Mississippi-State-Bulldogs.png',
         'Western-Carolina-Catamounts.png','Texas-A&M-Aggies.png','Charleston-Southern-Buccaneers.png','Arkansas-Pine-Bluff-Golden-Lions.png',
         'Saint-Marys-Gaels.png','SMU-Mustangs.png','Western-Kentucky-Hilltoppers.png','Xavier-Musketeers.png','Western-Michigan-Broncos.png',
         'Florida-A&M-Rattlers.png','UC-Santa-Barbara-Gauchos.png','Air-Force-Falcons.png','Lafayette-Leopards.png','Davidson-Wildcats.png',
         'Nebraska-Omaha-Mavericks.png','UMKC-Roos.png','Nevada-Wolf-Pack.png','St.-Bonaventure-Bonnies.png','Miami-FL-Hurricanes.png',
         'St.-Thomas-Tommies.png','UNC-Wilmington-Seahawks.png','Lipscomb-Bisons.png','Bentley-Falcons.png','East-Carolina-Pirates.png',
         'Denver-Pioneers.png','Minnesota-State-Mavericks.png','Centenary-Gentlemen.png','Chicago-State-Cougars.png','Rider-Broncs.png',
         'Little-Rock-Trojans.png','Ball-State-Cardinals.png','Niagara-Purple-Eagles.png','Wisconsin-Milwaukee-Panthers.png',
         'Florida-Atlantic-Owls.png','American-Eagles.png','Austin-Peay-Governors.png','Old-Dominion-Monarchs.png','Stetson-Hatters.png',
         'Akron-Zips.png','Southern-Jaguars.png','Albany-Great-Danes.png','South-Carolina-State-Bulldogs.png','Cleveland-State-Vikings.png',
         'Washington-State-Cougars.png','Butler-Bulldogs.png','UMBC-Retrievers.png','Cal-State-Fullerton-Titans.png','Siena-Saints.png',
         'Northeastern-Huskies.png','NC-State-Wolfpack.png','Richmond-Spiders.png','Southeast-Missouri-State-Redhawks.png',
         'Minnesota-Golden-Gophers.png','AIC-Yellow-Jackets.png','Boston-College-Eagles.png','Gardner-Webb-Bulldogs.png','Tennessee-State-Tigers.png',
         'Bemidji-State-Beavers.png','Providence-Friars.png','Nebraska-Cornhuskers.png','Colorado-State-Rams.png',
         'Miami-OH-Redhawks.png','South-Carolina-Gamecocks.png','Villanova-Wildcats.png','East Tennessee State-Buccaneers.png',
         'North-Texas-Mean-Green.png','Wofford-Terriers.png','North-Carolina-A&T-Aggies.png','Howard-Bison.png','Lehigh-Mountain-Hawks.png','Oakland-Golden-Grizzlies.png',
         'Morehead-State-Eagles.png','Montana-Grizzlies.png','Virginia-Tech-Hokies.png','Morgan-State-Bears.png','Texas-Tech-Red-Raiders.png','Hawaii-Warriors.png',
         'Texas-A&M-CC-Islanders.png','Harvard-Crimson.png','West-Virginia-Mountaineers.png','Gonzaga-Bulldogs.png','Savannah-State-Tigers.png',
         'Wisconsin-Badgers.png','Prairie-View-A&M-Panthers.png','RIT-Tigers.png','Memphis-Tigers.png','Union-Dutchmen.png',
         'UMass-Lowell-River-Hawks.png','Mississippi-Valley-State-Delta-Devils.png','Sacramento-State-Hornets.png','Indiana-Hoosiers.png',
         'Delaware-State-Hornets.png','Tulsa-Golden-Hurricane.png','Loyola-MD-Greyhounds.png','Wyoming-Cowboys.png',
         'Mercyhurst-Lakers.png','San-Diego-State-Aztecs.png','Bucknell-Bison.png','Fresno-State-Bulldogs.png','UT-Arlington-Mavericks.png',
         'Grambling-State-Tigers.png','North-Dakota-Fighting-Hawks.png','NJIT-Highlanders.png','San-Jose-State-Spartans.png','UC-Riverside-Highlanders.png',
         'Oregon-Ducks.png','Jackson-State-Tigers.png','Tennessee-Martin-Skyhawks.png','Syracuse-Orange.png','Coastal-Carolina-Chanticleers.png','Iona-Gaels.png',
         'Ohio-Bobcats.png','South-Florida-Bulls.png','Mount-Saint-Marys-Mountaineers.png','Pepperdine-Waves.png','Creighton-Bluejays.png',
         'Kansas-Jayhawks.png','Colorado-College-Tigers.png','Penn-State-Nittany-Lions.png','George-Mason-Patriots.png','Yale-Bulldogs.png',
         'Vanderbilt-Commodores.png','Lamar-Cardinals.png','Wisconsin-Green-Bay-Phoenix.png','Longwood-Lancers.png','BYU.png','DePaul-Blue-Demons.png',
         'UNLV-Rebels.png','Alaska-Anchorage-Seawolves.png','Maine-Black-Bears.png','Stephen-F-Austin-Lumberjacks.png','Northern-Michigan-Wildcats.png','California-Golden-Bears.png',
         'Long-Beach-State-49ers.png','Dartmouth-Big-Green.png','Wake-Forest-Demon-Deacons.png','Houston-Christian-Huskies.png','Belmont-Bruins.png',
         'Duke-Blue-Devils.png','McNeese-State-Cowboys.png','William-&-Mary-Tribe.png','Rutgers-Scarlet-Knights.png','Wright-State-Raiders.png',
         'St.-Cloud-State-Huskies.png','Cal-State-Northridge-Matadors.png','St.-Johns-Red-Storm.png','Florida-Gulf-Coast-Eagles.png',
         'Alabama-A&M-Bulldogs.png','Delaware-Blue-Hens.png','Nicholls-State-Colonels.png','Saint-Louis-Billikens.png',
       'Le-Moyne.png']

d = {}
for tm in newDF['Team']:
    tm1 = re.sub("'", "", tm)
    for f in pngs:
        f1 = re.sub('-', ' ', f)
        if tm1 in f1:
            
            if "State" in tm and "State" not in f:continue
            if "State" in f and "State" not in tm:continue                    
            if "East" in tm and "East" not in f:continue
            if "East" in f and "East" not in tm:continue                        
            if "West" in tm and "West" not in f:continue
            if "West" in f and "West" not in tm:continue                         
            if "North" in tm and "North" not in f:continue
            if "North" in f and "North" not in tm:continue                         
            if "South" in tm and "South" not in f:continue
            if "South" in f and "South" not in tm:continue                    
            if "Central" in tm and "Central" not in f:continue
            if "Central" in f and "Central" not in tm:continue                    
            if "Tech" in tm and "Tech" not in f:continue
            if "Tech" in f and "Tech" not in tm:continue                        
            if "A&M" in tm and "A&M" not in f:continue
            if "A&M" in f and "A&M" not in tm:continue  
            if "Valley" in tm and "Valley" not in f:continue
            if "Valley" in f and "Valley" not in tm:continue                    
            if "Baptist" in tm and "Baptist" not in f:continue
            if "Baptist" in f and "Baptist" not in tm:continue                   
            if "Christian" in tm and "Christian" not in f:continue
            if "Christian" in f and "Christian" not in tm:continue                    
            if "Loyola" in tm and "Loyola" not in f:continue
            if "Loyola" in f and "Loyola" not in tm:continue                    
            if "Chicago" in tm and "Chicago" not in f:continue
            if "Chicago" in f and "Chicago" not in tm:continue                    
            if "Martin" in tm and "Martin" not in f:continue
            if "Martin" in f and "Martin" not in tm:continue                    
            if "CC" in tm and "CC" not in f:continue
            if "CC" in f and "CC" not in tm:continue                    
            if "Commerce" in tm and "Commerce" not in f:continue
            if "Commerce" in f and "Commerce" not in tm:continue                    
            if "Lowell" in tm and "Lowell" not in f:continue
            if "Lowell" in f and "Lowell" not in tm:continue                        
            if "Military" in tm and "Military" not in f:continue
            if "Military" in f and "Military" not in tm:continue                                      
            if "Monroe" in tm and "Monroe" not in f:continue
            if "Monroe" in f and "Monroe" not in tm:continue 
                
            if "City" in tm and "City" not in f:continue
            if "City" in f and "City" not in tm:continue  
            if "Colorado" in tm and "Colorado" not in f:continue
            if "Colorado" in f and "Colorado" not in tm:continue  
            if "College" in tm and "College" not in f:continue
            if "College" in f and "College" not in tm:continue 
            if "Gulf" in tm and "Gulf" not in f:continue
            if "Gulf" in f and "Gulf" not in tm:continue 
            if "Atlantic" in tm and "Atlantic" not in f:continue
            if "Atlantic" in f and "Atlantic" not in tm:continue 
            if "A&T" in tm and "A&T" not in f:continue
            if "A&T" in f and "A&T" not in tm:continue  
            if "SA" in tm and "SA" not in f:continue
            if "SA" in f and "SA" not in tm:continue 
            if "Arlington" in tm and "Arlington" not in f:continue
            if "Arlington" in f and "Arlington" not in tm:continue
            if "Upstate" in tm and "Upstate" not in f:continue
            if "Upstate" in f and "Upstate" not in tm:continue
            if "Pine" in tm and "Pine" not in f:continue
            if "Pine" in f and "Pine" not in tm:continue
            if "Bay" in tm and "Bay" not in f:continue
            if "Bay" in f and "Bay" not in tm:continue
            if "Mount" in tm and "Mount" not in f:continue
            if "Mount" in f and "Mount" not in tm:continue
                
            d[tm]=f
            
newDF['Logo']=newDF['Team'].map(d)
newDF['Logo'] = 'logos//' + newDF['Logo']

# from PIL import Image 

# for f in tms['Logo']:
#     img = Image.open(f) 
#     rgba = img.convert("RGBA") 
#     datas = rgba.getdata() 

#     newData = [] 
#     for item in datas: 
#         if item[0] > 230 and item[1] > 230 and item[2] > 230:  # finding white colour 
#             # replacing it with a transparent value 
#             newData.append((255, 255, 255, 0)) 
#         else: 
#             newData.append(item) 

#     rgba.putdata(newData) 
#     rgba.save(f, "PNG") 
    
    
# print(newDF['Team'][newDF['Logo'].isna()])
# print(len(newDF))



### WRITE DATA OUT ###

def write_csv_from_pd(df,fileName):
    file = open('../teamCSVs/' + fileName+".csv", 'w')
    file.write(df.to_csv())
    file.close() 
#print("FINAL", len(newDF))      
write_csv_from_pd(newDF, 'currentSzn')

def write_txt_from_pd(df,fileName):
    file = open('../teamCSVs/' + fileName+".txt", 'w')
    file.write(df.tocsv)
    file.close() 
    
    
updateDF = pd.DataFrame(pd.Series([update]), columns=["A"])
write_csv_from_pd(updateDF, "update")

write_csv_from_pd(ap, "currAP")




