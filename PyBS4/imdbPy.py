from datetime import datetime
import forex_python.converter 
import random 
from webbrowser import open_new 
import pickle
from imdb import IMDb
import numpy as np
import pandas as pd
import time #import multiprocessing
import re
from ast import literal_eval

sdata=pd.read_csv('D:\\imdbsql.csv', sep='\t', encoding='utf-8', low_memory=False)
sdata.drop(sdata.columns[0], axis=1, inplace=True)
movies=pd.read_csv("M:\\budget.csv", sep='\t', low_memory=False)        
#df1=pd.read_csv("D:\\df1.csv", sep='\t', low_memory=False)                
#df2=pd.read_csv("D:\\df22.csv", sep='\t', low_memory=False)                
film=pd.read_csv('M:\\ttid.csv', sep='\t', encoding='utf-8' ).drop_duplicates()

data=pd.DataFrame()
wrong=set()
for col in sdata:
    #print(col)
    try:
        data[col]=sdata[col].dropna().apply(literal_eval)
        print(type(data[col].dropna().iloc[0]),": ", col)
    except:
        if re.compile("[A-Za-z0-9\.]+").fullmatch(str(sdata[col].dropna().iloc[0])[0]):
            print('string: ' , col )
            data[col]=sdata[col]
        else:#  re.compile("\[\<").fullmatch(str(sdata[col].dropna().iloc[0])[0:2]):
            try:
                print('list/dict: ', col)
                data[col]=sdata[col].dropna().str.replace("\"","|").str.replace("\[\<","[\"<").str.replace(">]",">\"]").apply(literal_eval)
            except:
                data[col]=sdata[col]
                wrong.add(col)        
del sdata
# __________________________________________________________________________________________####  
###################################### BUDGET
#film=pd.merge(film, data, left_on='ID', right_on='id', how='left')#link, data.loc[~data['id'].isin(set(link[link.duplicated('tconst')].id)),['budget','sequel','production_country','language','languages','id','version_of']], left_on='id', right_on='id', how='right')    
film=pd.concat([film, data.loc[:, data.columns != 'id']], axis=1)
del film['Unnamed: 0']
del film['year_y']
film.columns=film.columns.str.replace('year_x','year').values

movies['budget_y']=[[item] if np.isnan(item)==False else np.nan for item in movies.budget.astype(np.float64) ]
film=film.merge(movies[['tconst', 'budget_y', 'currency']], how='left', on='tconst')
#for t,i,j in zip(film.tconst, film.budget, film.budget_y):
 #   if j is not np.nan:
  #      print(t,i,j)
film.budget.fillna(film.budget_y, inplace=True)

film=film[film.columns[~film.columns.str.contains('budget_')]]
film=pd.concat([film, film.budget.apply(pd.Series).add_prefix('budget_')], axis=1)


[col for col in film if (col.startswith('budget_'))]
for col in [col for col in film if col.startswith('budget_')]:
    print(film[col].dropna().iloc[1:3])
    film[col+'_curr']=      film[col].dropna().str.replace(r',', '').str.replace(r'(\d+)', '')
    film[col]=              film[col].dropna().str.replace(r',', '').str.extract('(\d+)')
    print(film[col].dropna().iloc[1:3])
    print(film[col+'_curr'].dropna().iloc[1:3])
[col for col in film if (col.startswith('budget_'))]

film.budget_0_curr.fillna(film.currency, inplace=True)

'''
film[col]=pd.concat([film,film[col].str.replace('d+', '').add_prefix('curr_')], axis=1).columns
film[col].str.replace('d+', '').add_prefix('curr_')


curr=film[['budget_0_curr','tconst']].dropna()
curr=pd.DataFrame(film[film.tconst.isin(curr.loc[ curr.budget_0_curr != '$','tconst'])][['tconst','budget_0_curr']])
cnv=pd.read_csv("D:\\r\\convesrionrates.csv")#rtng=rtng.loc[:,['originalTitle','startYear']]#rtng=rtng.dropna(how='all')
curr[curr.tconst.isin(cnv.tconst)=False][['tconst','budget_0_curr']]
for curr in film.columns[film.columns.str.contains('curr')]:
    print(set(film[curr]))

from currency_converter import CurrencyConverter
cc=CurrencyConverter()
film.budget_0_curr.str.strip()
wrong=[]
for M in set(film.budget_0_curr.str.strip()):
    #print(M)
    try:        
        print(M, cc.convert(100, M, 'USD', date=date(2013, 3, 21)))
    except:
        wrong.append(M)
'''
 
#film=pd.read_csv("D:\\Py\\film.csv", sep='\t', low_memory=False)

runfile('M:/f_xrate.py', wdir='M:')
#film['tt']=film.tconst.fillna(film.id)
'''
for col in [col for col in film if (col.startswith('budget_')) & (len(col)<10)]:

    col=pd.DataFrame([xrate(tt, M, Y) for M, Y, tt in zip(film[col+'_curr'], film.year, film.tt)])
    film.merge(col, left_on='ID', right_on='id', how='left')


film['converted']=False
wrong={yr:set() for yr in range(1880,2025)}
test=[]
for col in [col for col in film if (col.startswith('budget_')) & (len(col)<10)]:
    #film[col+'_curr']=  film[col+'_curr'].str.strip().str.replace('£', 'GBP').str.replace( '€','EUR').str.replace( '$','USD')
    for B,M,Y in zip(film[col],film[col+'_curr'].str.strip().str.replace('£', 'GBP').str.replace( '€','EUR').str.replace( '$','USD'), film.year):
        if (B!=0) & (np.isnan(np.float64(B))==False) & (np.isnan(Y)==False):            
            print(B,M,Y)
            try:        
                film[col+'_USD']=int(forex_python.converter.convert(M, 'USD', int(B), date_obj=date(int(Y), 1, 1)))           
                film['converted']=True
            except:
                try:
                    film[col+'_USD']=int(forex_python.converter.convert(M, 'USD', int(B), date_obj=date(int(Y), 1, 1)))
                    film['converted']=True
    
                except: 
                    film[col+'_USD']='['+str(B)+','+str(M)+','+str(Y)+']'
                    wrong[Y].update([str(M)])    
                    test.append([B,M,Y])

        else: pass
#wrong  = set(tuple(row) for row in wrong if np.nan not in row)          
    
'''           
xcng=pd.DataFrame([xrate(tt, M, Y) for M, Y, tt in zip(film['budget_0_curr'], film.year, film.ID)])
xcng=pd.concat([film[[col for col in film if (col.startswith('budget_'))]], xcng], axis=1)

xcng['budget_USD']=0
for col in  xcng.loc[:,[col for col in xcng if (col.startswith('budget_')) & (len(col)<10)]]:
    xcng['budget_USD']=xcng['budget_USD']+xcng[col].fillna(0).astype(np.int64).astype(float)
xcng.budget_USD=xcng.budget_USD.replace({0.0:np.nan, 0:np.nan})
xcng.rate=xcng.rate.fillna(xcng.exchanged)

film=film[film.columns[~film.columns.str.contains('budget_')]]
film=pd.concat([film,xcng[['tt','budget_USD','rate']]],axis=1)

film['sequel']= np.select([film.follows.isnull()],[0],1)

f=open("M:/variables.txt",'w')
f.write(str(set(film.columns.tolist()+data.columns.tolist())))
f.close()
# __________________________________________________________________________________________####  
###################################### EXPORT

film.to_csv('D:\\film.csv', sep='\t', encoding='utf-8')
set(film.columns)



open_new('https://help.imdb.com/article/contribution/titles/movie-connections/GNUNL9W2FTZDGF4Y?ref_=helpart_nav_35#')
open_new('https://www.imdb.com/title/tt0071562/movieconnections/?tab=mc&ref_=tt_trv_cnn')



