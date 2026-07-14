import pandas as pd
import re
from statistics import mean
import math
import numpy as np
from datetime import date
import warnings
warnings.filterwarnings("ignore", category=FutureWarning)

# Get today's dates
today = date.today()
thisWeek=23
#today = re.sub("-0","-",str(today))
#today = "2023-2-11"

# season dates
dates = []
months = {11:2024,
          12:2024,
          1:2025,
          2:2025,
          3:2025,
          4:2025}

# get one schedule as a baseline to append to
schDF = pd.read_html('https://www.teamrankings.com/ncb/schedules/')[0]
print(schDF)
#schDF=schDF[~schDF['Date'].isin(['2024-11-31', '2025-2-29', '2025-2-30', '2025-2-31'])].reset_index(drop=True)
week=0
# iterate over season dates to get remaining schedule and append
for month in months.items():
    
    for day in range(31):
        
        # format this date
        dateP = str(month[1]) + '-' + str(month[0]) + '-' + str(day+1)
        #print(dateP)
        if dateP in ['2024-11-31', '2025-2-29', '2025-2-30', '2025-2-31']:
            continue
        #dates.append(dateP)
        
        # get schedule at this date
        url = 'https://www.teamrankings.com/ncb/schedules/?date=' + dateP
        #print(url)
        page = pd.read_html(url)[0]
        
        # add date
        page['Date'] = dateP
        
        # if new week started (Monday), increment week number
        try:
            if date(month[1], month[0], day+1).strftime("%A") == "Monday":
                week+=1
        except:pass
        
        # add week number
        page['Week'] = week        
        if len(page) > 0:
            schDF = pd.concat([schDF, page], axis=0)
            #schDF = schDF.append(page)        
    print(month[0])
        
#schDF=schDF.drop_duplicates(subset=['Matchup']).reset_index(drop=True)
schDF=schDF[~schDF['Date'].isin(['2024-11-31', '2025-2-29', '2025-2-30', '2025-2-31'])].reset_index(drop=True)
schDF['Date'] = pd.to_datetime(schDF['Date'], format="%Y-%m-%d").dt.strftime("%Y-%m-%d")
schDF['Date'] = schDF['Date'].replace(np.nan, today)
schDF['Week'] = schDF['Week'].replace(np.nan, thisWeek)

def clean_name(team):
    
#     team = re.sub('-', ' ', team)
#     team = re.sub('^N ', 'North ', team)
#     team = re.sub('^S ', 'South ', team)
#     team = re.sub('^W ', 'West ', team)
#     team = re.sub('^E ', 'East ', team)
#     team = re.sub(' St', ' State', team)
#     team = re.sub(' Stateate', ' State', team)
#     team = re.sub('Miss ', 'Mississippi ', team)
#     team = re.sub('VA ', 'Virginia ', team)
#     team = re.sub('St ', 'Saint ', team)
#     team = re.sub('\(', '', team)
#     team = re.sub('\)', '', team)
#     team = re.sub('Miami FL', 'Miami FL', team)
#     team = re.sub('TX Christian', 'TCU', team)
#     team = re.sub('Central FL', 'UCF', team)
#     team = re.sub('GA Tech', 'Georgia Tech', team)
#     team = re.sub('Saint Marys', 'Saint Mary\'s', team)
#     team = re.sub('Saint Johns', 'St. John\'s', team)
#     team = re.sub('Boston Col', 'Boston College', team)
#     team = re.sub('Mississippi', 'Ole Miss', team)
#     team = re.sub('Ole Miss State', 'Mississippi State', team)
#     team = re.sub('Wash State', 'Washington State', team)
#     team = re.sub('Col Charlestn', 'Charleston', team)
#     team = re.sub('Sam Hous', 'Sam Houston', team)
#     team = re.sub('Utah Val State', 'Utah Valley', team)
#     team = re.sub('Loyola Mymt', 'Loyola Marymount', team)
#     team = re.sub('James Mad', 'James Madison', team)
#     team = re.sub('Fla Atlantic', 'Florida Atlantic', team)
#     team = re.sub('Middle Tenn', 'Middle Tennessee', team)
#     team = re.sub('LA Tech', 'Louisiana Tech', team)    
#     team = re.sub('TX ', 'Texas ', team)
#     team = re.sub('Alab ', 'Alabama ', team)
#     team = re.sub('Car ', 'Carolina ', team)
#     team = re.sub('Beth Cook', 'Bethune Cook', team)
#     team = re.sub('Miami FL OH', 'Miami OH', team)
#     team = re.sub('^Ste ', 'Steven ', team)
#     team = re.sub('Ole Miss Val State', 'Mississippi Valley State', team)
#     team = re.sub('Ark Pine Bl', 'Arkansas Pine Bluff', team)
#     team = re.sub('Saint Bonavent', 'Saint Bonaventure', team)
#     team = re.sub('Geo Wshgtn', 'George Washington', team)
#     team = re.sub('U Mass', 'UMass', team)
#     team = re.sub('Geo Mason', 'George Mason', team)
#     team = re.sub('Mex ', 'Mexico ', team)
#     team = re.sub('Abl ', 'Abeline', team)
#     team = re.sub('TX ', 'Texas ', team)
#     team = re.sub('Jksnville', 'Jacksonville', team)

    # team = re.sub('Miami \(FL\)', 'Miami FL', team)
    # team = re.sub('Miami \(OH\)', 'Miami OH', team)
    # team = re.sub('St Fran \(NY\)', 'St. Francis NY', team)
    # team = re.sub('St Fran \(PA\)', 'St. Francis PA', team)
    # #team = re.sub('\(', '', team)
    # #team = re.sub('\)', '', team)
    # team = re.sub('-', ' ', team)
    # team = re.sub('^N ', 'North ', team)
    # team = re.sub('^S ', 'South ', team)
    # team = re.sub('^W ', 'West ', team)
    # team = re.sub('^E ', 'East ', team)
    # team = re.sub(' St', ' State', team)
    # team = re.sub(' Stateate', ' State', team)
    # team = re.sub('Miss ', 'Mississippi ', team)
    # team = re.sub('VA ', 'Virginia ', team)
    # team = re.sub('St ', 'Saint ', team)
    # team = re.sub('TX Christian', 'TCU', team)
    # team = re.sub('Central FL', 'UCF', team)
    # team = re.sub('GA Tech', 'Georgia Tech', team)
    # team = re.sub('Saint Marys', 'Saint Mary\'s', team)
    # team = re.sub('Saint Johns', 'St. John\'s', team)
    # team = re.sub('Boston Col', 'Boston College', team)
    # team = re.sub('Mississippi', 'Ole Miss', team)
    # team = re.sub('Ole Miss State', 'Mississippi State', team)
    # team = re.sub('Wash State', 'Washington State', team)
    # team = re.sub('Col Charlestn', 'Charleston', team)
    # team = re.sub('Sam Hous', 'Sam Houston', team)
    # team = re.sub('Utah Val State', 'Utah Valley', team)
    # team = re.sub('Loyola Mymt', 'Loyola Marymount', team)
    # team = re.sub('James Mad', 'James Madison', team)
    # team = re.sub('Fla Atlantic', 'Florida Atlantic', team)
    # team = re.sub('Middle Tenn', 'Middle Tennessee', team)
    # team = re.sub('LA Tech', 'Louisiana Tech', team)
    # team = re.sub('TX ', 'Texas ', team)
    # team = re.sub('Alab ', 'Alabama ', team)
    # team = re.sub('Car ', 'Carolina ', team)
    # team = re.sub('Beth Cook', 'Bethune Cook', team)
    # team = re.sub('^Ste ', 'Steven ', team)
    # team = re.sub('Ole Miss Val State', 'Mississippi Valley State', team)
    # team = re.sub('Ark Pine Bl', 'Arkansas Pine Bluff', team)
    # team = re.sub('Saint Bonavent', 'Saint Bonaventure', team)
    # team = re.sub('Geo Wshgtn', 'George Washington', team)
    # team = re.sub('U Mass', 'UMass', team)
    # team = re.sub('Geo Mason', 'George Mason', team)
    # team = re.sub('Mex ', 'Mexico ', team)
    # team = re.sub('Abl ', 'Abeline', team)
    # team = re.sub('^TX ', 'Texas ', team)
    # team = re.sub('Jksnville', 'Jacksonville', team)
    # team = re.sub('South Methodist', 'SMU', team)
    # team = re.sub('Southern Methodist', 'SMU', team)
    # team = re.sub('Detroit', 'Detroit Mercy', team)
    # team = re.sub('AbelineChristian', 'Abilene Christian', team)
    # team = re.sub('AR Lit Rock', 'Little Rock', team)
    # team = re.sub('App State', 'Appalachian State', team)
    # team = re.sub('Bowling Grn', 'Bowling Green', team)
    # team = re.sub('^Bethune Cook$', 'Bethune Cookman', team)
    # team = re.sub('Bakersfld', 'Bakersfield', team)
    # team = re.sub('^CS ', 'Cal State ', team)
    # team = re.sub(' Conn$', ' Connecticut', team)
    # team = re.sub(' Ark$', ' Arkansas', team)
    # team = re.sub(' Mich$', ' Michigan', team)
    # team = re.sub('Nrdge', 'Northridge', team)
    # team = re.sub('Charl ', 'Charleston ', team)
    # team = re.sub('Charleston South', 'Charleston Southern', team)
    # team = re.sub('Citadel', 'The Citadel ', team)
    # team = re.sub('Central Connecticut', 'Central Connecticut State', team)
    # team = re.sub('Coastal Car', 'Coastal Carolina', team)
    # team = re.sub('^Connecticut$', 'UConn', team)
    # team = re.sub('^East Tenn State$', 'East Tennessee State', team)
    # team = re.sub('^East Illinois$', 'Eastern Illinois', team)
    # team = re.sub('^East Michigan$', 'Eastern Michigan', team)
    # team = re.sub('^East Kentucky$', 'Eastern Kentucky', team)
    # team = re.sub('^East Washingtn$', 'Eastern Washington', team)
    # team = re.sub('^F Dickinson$', 'Fairleigh Dickinson', team)
    # team = re.sub('^Fla Gulf Cst$', 'Florida Gulf Coast', team)
    # team = re.sub('^Florida Intl$', 'Florida International', team)
    # team = re.sub('^GA Southern$', 'Georgia Southern', team)
    # team = re.sub('^Grd Canyon$', 'Grand Canyon', team)
    # team = re.sub('^IL Chicago$', 'Illinois Chicago', team)
    # team = re.sub('^Gard Webb$', 'Gardner Webb', team)
    # team = re.sub('^Hsn Christian$', 'Houston Christian', team)
    # team = re.sub('^IPFW$', 'Fort Wayne', team)
    # team = re.sub('^Incar Word$', 'Incarnate Word', team)
    # team = re.sub('^LA Lafayette$', 'Louisiana', team)
    # team = re.sub('^Lg Beach State$', 'Long Beach State', team)
    # team = re.sub('^LIU$', 'Long Island', team)
    # team = re.sub('^LA Monroe$', 'Louisiana Monroe', team)
    # team = re.sub('^Loyola Chi$', 'Loyola Chicago', team)
    # team = re.sub('^Maryland ES$', 'Maryland Eastern Shore', team)
    # team = re.sub('^Maryland BC$', 'UMBC', team)
    # team = re.sub('^WI Grn Bay$', 'Green Bay', team)
    # team = re.sub('^Middle Tennessee$', 'Middle Tennessee State', team)
    # team = re.sub('^Mass Lowell$', 'UMass Lowell', team)
    # team = re.sub('^Wm & Mary$', 'William & Mary', team)
    # team = re.sub('^Youngs State$', 'Youngstown State', team)
    # team = re.sub('^West Illinois$', 'Western Illinois', team)
    # team = re.sub('^West Michigan$', 'Western Michigan', team)
    # team = re.sub('^West Kentucky$', 'Western Kentucky', team)
    # team = re.sub('^West Washingtn$', 'Western Washington', team)
    # team = re.sub('^Mt State Marys$', 'Mount State Mary\'s', team)
    # team = re.sub('^NC A&T$', 'North Carolina A&T', team)
    # team = re.sub('^NC ', 'North Carolina ', team)
    # team = re.sub('^WI Milwkee', 'Milwaukee ', team)
    # team = re.sub('^West Carolina', 'Western Carolina ', team)
    # team = re.sub('^UNC ', 'North Carolina ', team)
    # team = re.sub('^Neb Omaha' , 'Omaha', team)
    # team = re.sub('^North Illinois$', 'Northern Illinois', team)
    # team = re.sub('^North Arizona$', 'Northern Arizona', team)
    # team = re.sub('^North Colorado$', 'Northern Colorado', team)
    # team = re.sub('^North Kentucky$', 'Northern Kentucky', team)
    # team = re.sub('^North Iowa$', 'Northern Iowa', team)
    # team = re.sub('^North Carolina State$', 'NC State', team)
    # team = re.sub('^NW State$', 'Northwestern State', team)
    # team = re.sub('^North Carolina Asheville$', 'UNC Asheville', team)
    # team = re.sub('^North Carolina Grnsboro$', 'UNC Greensboro', team)
    # team = re.sub('^North Carolina Wilmgton$', 'UNC Wilmington', team)
    # team = re.sub('^North Hampshire$', 'New Hampshire', team)
    # team = re.sub('^North Mexico State$', 'New Mexico State', team)
    # team = re.sub('^U Penn$', 'Penn', team)
    # team = re.sub('^Northeastrn$', 'Northeastern', team)
    # team = re.sub('^Prairie View$', 'Prairie View A&M', team)
    # team = re.sub('^Rob Morris$', 'Robert Morris', team)
    # team = re.sub('^SIU Edward$', 'SIU Edwardsville', team)
    # team = re.sub('^SE Missouri$', 'Southeast Missouri State', team)
    # team = re.sub('^SE Louisiana$', 'Southeastern Louisiana', team)
    # team = re.sub('^Sac State$', 'Sacramento State', team)
    # #team = re.sub('^Sac State$', 'Sacramento State', team)
    # team = re.sub('^SC Upstate$', 'USC Upstate', team)
    # team = re.sub('^Sacred Hrt$', 'Sacred Heart', team)
    # team = re.sub('^Saint Josephs$', 'Saint Joseph\'s', team)
    # team = re.sub('^Saint Peters$', 'Saint Peter\'s', team)
    # team = re.sub('^South Ole Miss$', 'Southern Miss', team)
    # team = re.sub('^Saint Bonaventure$', 'St. Bonaventure', team)
    # team = re.sub('^South Illinois$', 'Southern Illinois', team)
    # team = re.sub('^South Indiana$', 'Southern Indiana', team)
    # team = re.sub('^South Utah$', 'Southern Utah', team)
    # team = re.sub('^TN ', 'Tennessee ', team)
    # team = re.sub('^Texas A&M Com', 'Texas A&M Commerce ', team)
    # team = re.sub('^Texas Arlington', 'UT Arlington', team)
    # team = re.sub('^Texas San Ant', 'UT San Antonio', team)
    # team = re.sub('^Texas El Paso', 'UTEP', team)
    # team = re.sub('^Steven F Austin', 'Stephen F Austin', team)
    # team = re.sub('^Santa Barbara', 'UC Santa Barbara', team)
    # team = re.sub('^Texas Pan Am', 'UT Rio Grande Valley', team)
    # team = re.sub('^UCSB', 'UC Santa Barbara', team)
    # team = re.sub("Cal State Northridge", "CSUN", team)
    # team = re.sub("McNeese State", "McNeese", team)
    # team = re.sub("Nicholls State", "Nicholls", team)
    # team = re.sub("Southeast Missouri State", "Southeast Missouri", team)
    # team = re.sub("SIU Edwardsville", "SIUE", team)
    # team = re.sub("East Texas A&M", "Texas A&M Commerce", team)

    team = re.sub('Miami \(FL\)', 'Miami FL', team)
    team = re.sub('Miami \(OH\)', 'Miami OH', team)
    #team = re.sub('St Fran \(NY\)', 'St. Francis NY', team)
    team = re.sub('St Fran \(PA\)', 'Saint Francis', team)
    #team = team.split('(')[0]
    #record = '(' + i.split('(')[1]
    team = team.strip()
    #team = re.sub('-', ' ', team)
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

    return team.strip()

masterList = []
for row in schDF.iterrows():
    game = row[1]['Matchup']
    
    if ' vs.' in game:
        games = game.split(' vs. ')
    else:
        games = game.split(' at ')
    if '#' in games[0]:
        games[0] = games[0][games[0].index(' '):].strip()    
    if '#' in games[1]:
        games[1] = games[1][games[1].index(' '):].strip()
        
    games[0] = clean_name(games[0])
    games[1] = clean_name(games[1])
    
    tempList = []  
    tempList.append(games[0])
    tempList.append(games[1])
    tempList.append(row[1]['Date'])
    tempList.append(row[1]['Week'])
    tempList.append(row[1]['Location'])
    tempList.append(row[1]['Time'])
    masterList.append(tempList)
    
masterSch = pd.DataFrame(masterList, 
                         columns = ['Away', 'Home',
                                    'Date', 'Week',
                                    'Location', 'Time'])

#print(sorted(list(set(masterSch['Away']))))
#print(sorted(list(set(masterSch['Home']))))
TR = pd.read_csv('../teamCSVs/model-inputs.csv')
#TR['Rank'] = pd.Series(list(range(1,len(TR['Team'])+1)))

def slateDF(date):
    masterList=[]
    df = masterSch[masterSch['Date'] == date]
    
    for game in df.iterrows():
        
        try:
            homeScore = float(TR[TR['Team'] == game[1]['Home']]['team_score'])
#             homeOffScore = float(TR[TR['Team'] == game[1]['Home']]['Offense.Score'])
#             homeDefScore = float(TR[TR['Team'] == game[1]['Home']]['Defense.Score'])
            
            awayScore = float(TR[TR['Team'] == game[1]['Away']]['team_score'])
#             awayOffScore = float(TR[TR['Team'] == game[1]['Away']]['Offense.Score'])
#             awayDefScore = float(TR[TR['Team'] == game[1]['Away']]['Defense.Score'])
            
            homeRank = int(TR[TR['Team'] == game[1]['Home']]['Rank'])
#             homeOffRank = int(TR[TR['Team'] == game[1]['Home']]['Offense.Natl.Rank'])
#             homeDefRank = int(TR[TR['Team'] == game[1]['Home']]['Defense.Natl.Rank'])
            
            awayRank = int(TR[TR['Team'] == game[1]['Away']]['Rank'])
#             awayOffRank = int(TR[TR['Team'] == game[1]['Away']]['Offense.Natl.Rank'])
#             awayDefRank = int(TR[TR['Team'] == game[1]['Away']]['Defense.Natl.Rank'])
            
        except:
            homeScore = None
#             homeOffScore = None
#             homeDefScore = None
            
            awayScore = None
#             awayOffScore = None
#             awayDefScore = None
            
            homeRank = None
#             homeOffRank = None
#             homeDefRank = None
            
            awayRank = None
#             awayOffRank = None
#             awayDefRank = None
        
        tempList = []
    
        tempList.append(game[1]['Away'])
        
        tempList.append(awayScore)
        tempList.append(awayRank)
        
#         tempList.append(awayOffScore)
#         tempList.append(awayOffRank)
        
#         tempList.append(awayDefScore)
#         tempList.append(awayDefRank)
        
        tempList.append(game[1]['Home'])
        
        tempList.append(homeScore)
        tempList.append(homeRank)
        
#         tempList.append(homeOffScore)
#         tempList.append(homeOffRank)
        
#         tempList.append(homeDefScore)
#         tempList.append(homeDefRank)
        
        tempList.append(game[1]['Date'])
        tempList.append(game[1]['Week'])
        tempList.append(game[1]['Location'])
        tempList.append(game[1]['Time'])
        masterList.append(tempList)
        
    return pd.DataFrame(masterList, columns = ['Away.Team', 'Away.Score', 'Away.Rank',
                                               #'Away.Off.Score', 'Away.Off.Rank', 'Away.Def.Score', 'Away.Def.Rank',
                                               'Home.Team', 'Home.Score', 'Home.Rank',
                                               #'Home.Off.Score', 'Home.Off.Rank', 'Home.Def.Score', 'Home.Def.Rank',
                                               'Date', 'Week','Location', 'Time']).dropna().reset_index(drop=True)


def write_csv_from_pd(df,fileName):
    file = open('../teamCSVs/' + fileName+".csv", 'w')
    file.write(df.to_csv())
    file.close() 
    
write_csv_from_pd(masterSch,"checkSch2025")
#### THISWEEK IS NAN
thisWeek = masterSch[masterSch['Date'] == today]['Week'].iloc[0]
print(masterSch[['Week', 'Date']].value_counts())
print(masterSch['Date'])
daysThisWeek = list(dict(masterSch[masterSch['Week'] == thisWeek]['Date'].value_counts()).keys())

#print(len(set(masterSch['Home'])))
#print(masterSch.columns)
#print(today)
#print(masterSch['Date'].value_counts())
#print(masterSch[masterSch['Date'] == date])
# Get today's slate
fDF = slateDF(today)
#print(fDF)

# get week number
#thisWeek = fDF['Week'].iloc[0]

# get this week's slate
daysThisWeek = list(dict(masterSch[masterSch['Week'] == thisWeek]['Date'].value_counts()).keys())# + ['2025-03-01', '2025-03-02']
print('\n------------------\n')
print(daysThisWeek)
print('\n------------------\n')
weekDF = slateDF(daysThisWeek[0])
for d in daysThisWeek:
    if d != daysThisWeek[0]:
        weekDF = pd.concat([weekDF, slateDF(d)], axis=0)
        #weekDF = weekDF.append(slateDF(d)).reset_index(drop=True)
        
        
print(fDF)
print("_________________")
print(thisWeek)
# write out today's and this week's game slates
write_csv_from_pd(fDF, 'today-game-slate')
write_csv_from_pd(weekDF, 'week-game-slate')




