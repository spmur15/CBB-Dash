import pandas as pd

### GET TOURNAMNT FINISHES FOR MODEL OUTCOME ###
y = pd.read_csv('../teamCSVs/tournament_finishes.csv')
y = y[['Team', 'Year', 'Round reached']]

### GET INPUT DATA FROM NCAAB_s_team_current_szn.py 
train = pd.read_csv('../teamCSVs/train.csv')
train=train.drop(['Unnamed: 0', '#_x', '#_y', '#_Opp_x', '#_Opp_y'], axis=1)

# print(sorted(list(set(train['Team']))))
# print(sorted(list(set(y['Team']))))
# print(len(train))
# print(len(y))
# teamCheck = dict(map(lambda i,j : (i,j) , sorted(list(set(train['Team']))),sorted(list(set(y['Team'])))))
# for i in teamCheck.items():
#     if i[0] != i[1]:
#         print(i[0], ":", i[1])

# MERGE OUTCOME WITH TRAINING DATA
final = train.merge(y, on=['Team', 'Year'])
#print(len(final))
##### Assign numeric outcome based on tournament round reached
# Consider weekend reached

result = []
for i in final['Round reached']:
    
    if '6N' in i: result.append(5)
    elif '5' in i: result.append(4)
    elif '4' in i: result.append(3)
    elif '3' in i: result.append(2)
    elif '2' in i: result.append(1)
    elif '1' in i: result.append(0)
    elif i == '0Play-In': result.append(0)
    else: result.append(0)
        
final['Result'] = pd.Series(result)

final.loc[((final['Team']=='Syracuse') & (final['Year']==2003)), 'Result'] = 6
final.loc[((final['Team']=='UConn') & (final['Year']==2004)), 'Result'] = 6
final.loc[((final['Team']=='North Carolina') & (final['Year']==2005)), 'Result'] = 6
final.loc[((final['Team']=='Florida') & (final['Year']==2006)), 'Result'] = 6
final.loc[((final['Team']=='Florida') & (final['Year']==2007)), 'Result'] = 6
final.loc[((final['Team']=='Kansas') & (final['Year']==2008)), 'Result'] = 6
final.loc[((final['Team']=='North Carolina') & (final['Year']==2009)), 'Result'] = 6
final.loc[((final['Team']=='Duke') & (final['Year']==2010)), 'Result'] = 6
final.loc[((final['Team']=='UConn') & (final['Year']==2011)), 'Result'] = 6
final.loc[((final['Team']=='Kentucky') & (final['Year']==2012)), 'Result'] = 6
final.loc[((final['Team']=='Louisville') & (final['Year']==2013)), 'Result'] = 6
final.loc[((final['Team']=='UConn') & (final['Year']==2014)), 'Result'] = 6
final.loc[((final['Team']=='Duke') & (final['Year']==2015)), 'Result'] = 6
final.loc[((final['Team']=='Villanova') & (final['Year']==2016)), 'Result'] = 6
final.loc[((final['Team']=='North Carolina') & (final['Year']==2017)), 'Result'] = 6
final.loc[((final['Team']=='Villanova') & (final['Year']==2018)), 'Result'] = 6
final.loc[((final['Team']=='Virginia') & (final['Year']==2019)), 'Result'] = 6
#final.loc[((final['Team']=='') & (final['Year']==2020)), 'Result'] = 6

### Format columnns
newCols = ['Team', 'Year', 'Round reached', 'Result', 'AdjEM']
for c in final.columns:
    if c not in newCols:
        newCols.append(c)
final = final[newCols]
final = final.sort_values(by='AdjEM', ascending=False).reset_index(drop=True)

# Merge historic SOS values for trainng
sos = pd.read_csv('../teamCSVs/historic_SOS.csv')
sos = sos[['Team', 'Year', 'SOS']]
sos['Team'] = sos['Team'].str.replace("^Connecticut$", "UConn", regex=True)
# print(sorted(list(set(final['Team']))))
# print(sorted(list(set(sos['Team']))))
# print(len(final))
# print(len(sos))
# print(sos['SOS'])


# teamCheck = dict(map(lambda i,j : (i,j) , sorted(list(set(final['Team']))), sorted(list(set(sos['Team'])))))
# for i in teamCheck.items():
#     if i[0] != i[1]:
#         print(i[0], ":", i[1])  


final = final.merge(sos, on=['Team', 'Year'])
final = final[final['Year']!=2020].reset_index(drop=True)
print(final)

### WRITE OUT 
def write_csv_from_pd(df,fileName):
    file = open('../teamCSVs/' + fileName+".csv", 'w')
    file.write(df.to_csv())
    file.close() 
    
write_csv_from_pd(final, 'team_predictions')