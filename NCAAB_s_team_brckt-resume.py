import pandas as pd
import requests
import re
import warnings
warnings.filterwarnings("ignore", category=FutureWarning)

#################### ESPN DATA ######################


url = 'https://www.espn.com/mens-college-basketball/bpi/_/view/resume/season/'

# get all team's ESPN bracketology resume by year
def getSzn(year):
    
    # initial df to add rows to
    df = ''
    
    # 9 pages of teams (~363)
    for n in range(1,9):
        
        # get html table from page and add to this year's df
        if isinstance(df, str):
            page = pd.read_html(url+str(year)+'/page/'+str(n))
            df = page[0].merge(page[1], right_index=True, left_index=True)
        else:
            page = pd.read_html(url+str(year)+'/page/'+str(n))
            df = pd.concat([df,page[0].merge(page[1], right_index=True, left_index=True) ])
            #df = df.append()
    
    # format df and return
    df['Year'] = year
    df=df[['Team', 'CONF', 'Year', 'W-L', 'QUAL WINS', 'SOR RK']].reset_index(drop=True)
    return(df)

# get bracket resume for data from 2008-2024
mDF = ''
for y in range(2008, 2026):
    try:
        if isinstance(mDF, str):
            mDF = getSzn(y)
        else:
            mDF = pd.concat([mDF, getSzn(y)])
    except:
        pass
mDF=mDF.reset_index(drop=True)
#SORsave = mDF[['Team', 'CONF', 'Year', 'SOR RK']][ (mDF['CONF']=='A-10') & (mDF['Year']==2024) ]
#print(mDF)
#print(mDF[(mDF['CONF']=='A-10') & (mDF['Year']==2024)])
#for i in mDF.columns:print(i)
# adjust team names
mDF['Team'][mDF['Team'] == 'UConn Huskies'] = 'Connecticut Huskies'

# get power 6 teams
teams = pd.read_csv('../teamCSVs/power-team-conf.csv')
teams = teams[['Team', 'Conference']]

s = teams.Team.str.len().sort_values(ascending=False).index
teams = teams.reindex(s)
teams = teams.reset_index(drop=True)
teams[teams['Team'] == 'Texas A&M']

# load current szn stats
curr = pd.read_csv('../teamCSVs/currentSzn.csv')
print("ONE: ", len(curr))
#print("currSzn", len(curr))
# adjust conference names
newCol=[]
for i in mDF['CONF']:
    conf = re.sub("Am. East", "America East", i)
    conf = re.sub("CUSA", "C-USA", conf)
    conf = re.sub("A-10", "A-10", conf)
    conf = re.sub("MVC", "Missouri Valley", conf)
    conf = re.sub("Patriot", "Patriot League", conf)
    conf = re.sub("Horizon", "Horizon League", conf)
    conf = re.sub("OVC", "Ohio Valley", conf)
    conf = re.sub("Indep.", "Independents", conf)
    conf = re.sub("ASUN", "Atlantic Sun", conf)
    conf = re.sub("Summit", "Summit League", conf)
    conf = re.sub("NEC", "Northeast", conf)
    conf = re.sub("Ivy", "Ivy League", conf)
    newCol.append(conf)
mDF['CONF'] = newCol

# adjust team names
newCol=[]
mDF = mDF.sort_values('Team')
for i in mDF['Team']:
    #print(i)
    team = re.sub("-", " ", i)
    team = re.sub("St. Francis Brooklyn", "St. Francis NY", team)
    team = re.sub("St. Francis \(PA\)", "Saint Francis", team)
    team = re.sub("Miami \(OH\)", "Miami OH", team)
    team = re.sub("Miami \(FL\)", "Miami FL", team)
    team = re.sub("UIC", "Illinois Chicago", team)
    team = re.sub("UL Monroe", "Louisiana Monroe", team)
    team = re.sub("UT Martin", "Tennessee Martin", team)
    team = re.sub("SE Louisiana", "Southeastern Louisiana", team)
    team = re.sub("FORT WAYNE", "Fort Wayne", team)
    team = re.sub("Hawai'i", "Hawaii", team)
    team = re.sub("Stephen F. Austin", "Stephen F Austin", team)
    team = re.sub("Pennsylvania Quakers", "Penn", team)
    team = re.sub("Middle Tennessee", "Middle Tennessee State", team)
    team = re.sub("José", "Jose", team)
    team = re.sub("Sam Houston", "Sam Houston State", team)
    team = re.sub("^Connecticut", "UConn", team)
    team = re.sub("UTSA", "UT San Antonio", team)
    #team = re.sub("Houston Christian", "Houston Baptist", team)
    team = re.sub("McNeese", "McNeese State", team)
    team = re.sub("VMI", "Virginia Military", team)
    team = re.sub("Loyola Maryland", "Loyola MD", team)
    team = re.sub("Mount St. Mary's", "Mount Saint Mary's", team)
    team = re.sub("American University", "American", team)
    team = re.sub("California Baptist", "Cal Baptist", team)
    team = re.sub("^Massachusetts ", "UMass ", team)
    team = re.sub("Corpus Christi", "CC", team)
    team = re.sub("Boston University", "Boston U", team)
    team = re.sub("South Carolina Upstate", "USC Upstate", team)
    team = re.sub("Grambling", "Grambling State", team)
    team = re.sub("Nicholls", "Nicholls State", team)
    team = re.sub("Kansas City", "UMKC", team)
    team = re.sub("Central Connecticut", "Central Connecticut State", team)
    team = re.sub("UAlbany", "Albany", team)
    team = re.sub("IU Indianapolis", "IU Indy", team)
    team = re.sub("Southeast Missouri State", "Southeast Missouri", team)
    team = re.sub("SIU Edwardsville", "SIUE", team)
    team = re.sub("Nicholls State", "Nicholls", team)
    team = re.sub("McNeese State", "McNeese", team)
    team = re.sub("Cal State Northridge", "CSUN", team)
    team = re.sub("App State", "Appalachian State", team)

    
    newCol.append(team)
mDF['Team'] = newCol
# a=mDF[mDF['Year']==2024]
# print(len(set(mDF['Team'])))
# print(len(list(set(a['Team']))))
# print(len(list(set(curr['Team']))))
# print((list(set(a['CONF']))))
# print((list(set(curr['Conference']))))
#thisYear['Team'][thisYear['Team'].isna()] = 'Le Moyne'
# chk = a.dropna().reset_index(drop=True)
# L1 = sorted(list(set(chk['Team'])))
# L2 = sorted(list(set(curr['Team'])))
# teamCheck = dict(map(lambda i,j : (i,j) ,L1 ,L2))
# for i in teamCheck.items():
#     if i[0] != i[1]:
#         print(i[0], ":", i[1]) 
#print(sum(mDF['Team'].str.contains("Le Moyne")))
### iterate over current szn stats team names
# and search for name in ESPN bracket resume data
# which has longer names (ex. Kansas Jayhawks)
# to match data this way we have to make sure matches like
# Kansas in Kansas State or Illinois in Northern Illinois
# does not occur

# necFix=pd.read_html('https://www.espn.com/mens-college-basketball/bpi/_/view/resume/group/19/sort/resume.sosoutofconfpastrank/dir/asc')
# necFix = pd.concat([necFix[0], necFix[1]], axis=1)
# lm = necFix[necFix['Team']=='Le Moyne Dolphins']
# lm['CONF'][lm['Team']=='Le Moyne Dolphins'] = 'Northeast'
# lm['Year']=2025
# lm=lm[['Team', 'CONF', 'Year', 'W-L', 'QUAL WINS', 'SOR RK']]
# mDF = pd.concat([mDF,lm]).reset_index(drop=True)
# print(mDF[mDF['Team']=='Le Moyne Dolphins'])
# print('\n------')
# for i in curr.columns:print(i)
# print('\n------')
#curr['Conference'] = curr['Conference_x'].copy()
#print(mDF)
#print(len(mDF))

#print(sorted(list(mDF['CONF'].unique())))
#print(sorted(list(curr['Conference'].unique())))
L = []
import re
for row1 in mDF.iterrows():
    tempList = []
    
    for i in row1[1]:
        tempList.append(i)

        
    team1 = row1[1]['Team']
    conf1 = row1[1]['CONF']
    conf1 = re.sub('SoCon', 'Southern', conf1)

    if team1 == 'East Texas A&M':print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    for row2 in curr[['Team', "Conference"]].iterrows():
        
        team2 = row2[1]['Team']
        conf2 = row2[1]['Conference']
        
        #print(conf1, conf2)
        if conf1 == conf2:
            
            if team2 in team1:
                
                if "State" in team2 and "State" not in team1:continue
                if "State" in team1 and "State" not in team2:continue                    
                if "East" in team2 and "East" not in team1:continue
                if "East" in team1 and "East" not in team2:continue                        
                if "West" in team2 and "West" not in team1:continue
                if "West" in team1 and "West" not in team2:continue                         
                if "North" in team2 and "North" not in team1:continue
                if "North" in team1 and "North" not in team2:continue                         
                if "South" in team2 and "South" not in team1:continue
                if "South" in team1 and "South" not in team2:continue                    
                if "Central" in team2 and "Central" not in team1:continue
                if "Central" in team1 and "Central" not in team2:continue                    
                if "Tech" in team2 and "Tech" not in team1:continue
                if "Tech" in team1 and "Tech" not in team2:continue                        
                if "A&M" in team2 and "A&M" not in team1:continue
                if "A&M" in team1 and "A&M" not in team2:continue                    
                if "Valley" in team2 and "Valley" not in team1:continue
                if "Valley" in team1 and "Valley" not in team2:continue                    
                if "Baptist" in team2 and "Baptist" not in team1:continue
                if "Baptist" in team1 and "Baptist" not in team2:continue                   
                if "Christian" in team2 and "Christian" not in team1:continue
                if "Christian" in team1 and "Christian" not in team2:continue                    
                if "Loyola" in team2 and "Loyola" not in team1:continue
                if "Loyola" in team1 and "Loyola" not in team2:continue                    
                if "Chicago" in team2 and "Chicago" not in team1:continue
                if "Chicago" in team1 and "Chicago" not in team2:continue                    
                if "Martin" in team2 and "Martin" not in team1:continue
                if "Martin" in team1 and "Martin" not in team2:continue                    
                if "CC" in team2 and "CC" not in team1:continue
                if "CC" in team1 and "CC" not in team2:continue                    
                if "Commerce" in team2 and "Commerce" not in team1:continue
                if "Commerce" in team1 and "Commerce" not in team2:continue                    
                if "Lowell" in team2 and "Lowell" not in team1:continue
                if "Lowell" in team1 and "Lowell" not in team2:continue                        
                if "Military" in team2 and "Military" not in team1:continue
                if "Military" in team1 and "Military" not in team2:continue                                      
                if "Monroe" in team2 and "Monroe" not in team1:continue
                if "Monroe" in team1 and "Monroe" not in team2:continue 
                if "Texas" in team2 and "Texas" not in team1:continue
                if "Texas" in team1 and "Texas" not in team2:continue 
                #print(team1, team2)
                
                if team1 == 'Texas Southern Tigers':
                    tempList.append('Texas Southern')

                elif team1 == 'Texas A&M Aggies':
                    tempList.append('Texas A&M')
                else:
                    if team2==None:
                        tempList.append(team1)
                    else:
                        tempList.append(team2)
                        
                break
                
            elif team1 == 'Miami Hurricanes':
                tempList.append('Miami FL')
                break
                
    L.append(tempList)

# create df and order columns
df = pd.DataFrame(L, columns = ['a','Conference', 'Year', 'Record', 'Qual Record', 'SOR RK', 'Team'])
df = df[['Team', 'Conference', 'Year', 'Record', 'Qual Record', 'SOR RK']]
#print(len(set(df['Team'])))
#print(sum(df['Team'].str.contains("Le Moyne")))
# get current year stats
#print(df)
#df2=df[df.isna()]
#df3=df[df['Team'].isna()]
#sor = dict(zip(list(thisYear['SOR RK']), list(thisYear['Team'])))
#thisYear['Team'][thisYear['Conference']=='Atlantic-10']=
#print(thisYear['SOR RK'][thisYear['Conference']=='Atlantic-10'].map(sor))
# drop na rows
#df=df.dropna().reset_index(drop=True)

# strip win percentage from string record column
wPct = []
qualGames = []
for i in df['Qual Record']:
    this = i.split('-')
    w = int(this[0])
    l = int(this[1])
    qualGames.append(w+l)
    try:wPct.append(w / (w+l))
    except:
        if w > 0:wPct.append(1)
        else:wPct.append(0)
df['Qual Win Pct'] = wPct
df['Qual Games'] = qualGames

wPct = []
for i in df['Record']:
    this = i.split('-')
    w = int(this[0])
    l = int(this[1])
    try:wPct.append(w / (w+l))
    except:
        if w > 0:wPct.append(1)
        else:wPct.append(0)
df['Record'] = wPct

thisYear = df[df['Year']==2025]
#print(thisYear[thisYear['Conference']=='A-10'])
#print(len(thisYear))
#print(thisYear[thisYear['Conference'].isna()])
#print(thisYear[thisYear['Team'].isna()])
#thisYear['Team'][thisYear['Team'].isna()] = 'St. Francis PA'

#print(df)
# get final data
tr = pd.read_csv('../teamCSVs/team_predictions.csv', index_col=0)
print(tr)
f = tr.merge(df, left_on=['Team', 'Year'], right_on=['Team', 'Year'])

#print(f)
# print(len(list(set(tr['Team']))))
# print(len(list(set(df['Team']))))

# teamCheck = dict(map(lambda i,j : (i,j) , sorted(list(set(tr['Team']))),sorted(list(set(df['Team'])))))
# for i in teamCheck.items():
#     if i[0] != i[1]:
#         print(i[0], ":", i[1])    

def write_csv_from_pd(df,fileName):
    file = open('../teamCSVs/' + fileName+".csv", 'w')
    file.write(df.to_csv())
    file.close() 
    
# try:
#     print(len(list(set(f['Team']))))    
# except:
#     print(len(list(set(f['Team_x']))))
    
    
write_csv_from_pd(f, 'team_predictions')#
#curr = pd.read_csv('../teamCSVs/currentSzn.csv')



# dfnow = df[df['Year'] == 2024].reset_index(drop=True)

# print(len(list(set(thisYear['Team']))))
# print(len(list(set(curr['Team']))))
# thisYear['Team'][thisYear['Team'].isna()] = 'Le Moyne'
# chk = thisYear.dropna().reset_index(drop=True)
thisYear.loc[(thisYear['Team'].isna()) & (thisYear['Conference']=='Summit League'), 'Team'] = 'Kansas City'
thisYear.loc[(thisYear['Team'].isna()) & (thisYear['Conference']=='Southland'), 'Team'] = 'Texas A&M Commerce'

# L1 = sorted(list(set(thisYear['Team'].fillna(''))))
# L2 = sorted(list(set(curr['Team'].fillna(''))))
# print(len(L1), len(L2))
# teamCheck = dict(map(lambda i,j : (i,j) ,L1 ,L2))
# for i in teamCheck.items():
#     if i[0] != i[1]:
#         print(i[0], ":", i[1])  
print("TWO: ", len(curr))


#print(thisYear[thisYear['Team'].isna()])

dfnow = curr.merge(thisYear, on=['Team'], suffixes=['', '_2'])#, how='outer', indicator=True)
#for i in sorted(dfnow.columns):print(i)
# print(dfnow[dfnow['_merge'].str.contains('only')][['Team', 'Conference', 'Conference_2']])
print("three: ", len(dfnow))
#dfnow['Team'][dfnow['Team'].isna()] = 'St. Francis PA'
#for i in dfnow.columns:print(i)
write_csv_from_pd(dfnow, 'currentSzn')






