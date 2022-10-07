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



'''''''''''''''''''''''''''''''''''''''''''SQL'''''''''''''''''''''''''''''''''''''''''''
#!pip install mysqlclient
ia=IMDb('sql', uri='mysql://root:Hstdks87@[::1]:3306/imdb')#rows=(2023558,2656711,ia.search_movie("The Godfather")[3].movieID, ia.search_movie("The Blair Witch Project")[0].movieID, ia.search_movie("Matrix")[0].movieID)#ia.search_movie("Die unendliche Geschichte")

# __________________________________________________________________________________________####  
###################################### FIND MOVIE-IDs

'''
#Getting the list of sample movie titles:
def records(rows):
    wrong=[]
    tmp=pd.DataFrame(columns=[ 'ID' , 'title', 'year'])
    i=1
    for row in rows: 
        i+=1
        print(i)
        try:
            movie=ia.get_movie(row, ia.get_movie_infoset())
            tmp=tmp.append(pd.DataFrame({ 'ID' : movie.movieID, 'title': movie.get('title'), 'year': movie.get('year')}, index=[len(rows)]))
        except:
            wrong.append(row)
    return tmp
        

start=time.time()
df=rtng.iloc[0:3,:]
rows=records(movieID)   
end=time.time()
print(end-start)

with open("M:\\movieID.txt", "wb") as fp:   #Pickling
    pickle.dump(rows, fp)
rows.to_csv('M:\\movieID.csv', sep='\t', encoding='utf-8')
'''

# __________________________________________________________________________________________####  
###################################### INPUT

'''
#movieIDs (reading and cleaning)
with open("movieID.txt", "rb") as fp:   #Unpickling saved list of sample movie titles
    rows = pickle.load(fp)  
rows[0][0]
    
'''
movieID=[line.strip() for line in open("D:\R\movieID.csv")]
movieID = np.unique(movieID)
movieID=pd.read_csv("M:\\movieID.csv", sep='\t', encoding='utf-8').loc[:,['ID', 'title','year']].drop_duplicates()
movieID.year=movieID.year.dropna().astype(int)


#IMDBids and ratings
#rtng=pd.read_csv("M:\\rtng.csv", sep='\t', encoding='utf-8').loc[:,['name','year', 'tconst']].drop_duplicates()#rtng=rtng.dropna(how='all')
    #rtng.columns=[s.replace('name', 'title') for s in rtng.columns]
bscs=pd.read_csv("D:\\R\\title.basics.tsv", sep='\t', encoding='utf-8',na_values=['\\N'], usecols= ['originalTitle','startYear', 'tconst'], dtype={'originalTitle':str,'startYear':float, 'tconst':str}).drop_duplicates()

film=pd.merge(movieID, bscs, how='left', left_on=['title', 'year'], right_on=['originalTitle','startYear'])
film=film.loc[film.ID!=0, ['ID', 'tconst']]

with open("M:\\ttid.txt", "wb") as fp:   #Pickling
    pickle.dump(film, fp)
film.to_csv('M:\\ttid.csv', sep='\t', encoding='utf-8')



''' 


#p = multiprocessing.Pool()
#test=p.map(records, rtng)

with open("movieID.txt", "wb") as fp:   #Pickling
    pickle.dump(rows, fp)
with open("movieID.txt", "rb") as fp:   #Unpickling    
    bckup = pickle.load(fp)  
#with open('movieID.py', 'w') as f:     #Save-as
#   f.write('rows = %s' % rows)
'''

    
# __________________________________________________________________________________________####  
###################################### COMPILING DATA-SET

start1=time.time()
titles=[]
for row in film.ID: #for each title    
    print(row)      
    title={} #empty dataframe
    movie=ia.get_movie(row, ia.get_movie_infoset())#[1], ia.get_movie_infoset()) #['technical','plot']  ia.get_movie_infoset() get all info
    keys = []
    for key in movie.infoset2keys:
        keys += list(movie.infoset2keys[key])
    keys = set(keys)
    #titles.append(keys)
    title.update({'id':row})
    for key in keys:
        if movie.get(key):            
            title.update({key:str(movie.get(key))})

    titles.append(title) #add the retireved information of this title as a new dataframe to the list of all other titles (i.e. all other dataframes) 
end1=time.time()
print(end1-start1)

sdata=pd.DataFrame(titles)
#sdata.to_csv('D:\\imdbsql.csv', sep='\t', encoding='utf-8')
#sdata=pd.read_csv('D:\\imdbsql.csv', sep='\t', encoding='utf-8')

data=pd.DataFrame(columns=sdata.columns, index=sdata.index)
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
                #sdata[col].dropna().str.replace('"', "|").str.replace('^\[', '"').str.replace('\]$', '"').apply(ast.literal_eval)
#        else:
#            wrong.add(col)                
#            data[col]=sdata[col]
        

'''DIAGNOSIS
type(data.director.iloc[253043,])
for i in [253043, 25304, 2530, 253]:
    print(str(i)+'_____________________________')
    for col in wrong: 
        #print(col)
        if (str(type(data[col].iloc[i,]))[8:-2]    ==str(type(sdata[col].iloc[i,]))[8:-2])         &        (~pd.isnull(sdata[col].iloc[i,])):    
            print(str(data[col].iloc[i,])[0:3], str(sdata[col].iloc[i,])[0:3],str(type(data[col].iloc[i,]))[8:-2],str(type(sdata[col].iloc[i,]))[8:-2], col)    

set(    [col for col in sdata if (str(type(data[col].iloc[253043,]))[8:-2]!=str(type(sdata[col].iloc[253043,]))[8:-2]) & (~pd.isnull(sdata[col].iloc[253043,]))]  )
-set(set([col for col in sdata if (str(type(data[col].iloc[253043,]))[8:-2]==str(type(sdata[col].iloc[253043,]))[8:-2]) & (~pd.isnull(sdata[col].iloc[253043,]))]  )        -set([col for col in sdata if (str(type(data[col].iloc[25304,]))[8:-2]!=str(type(sdata[col].iloc[25304,]))[8:-2]) & (~pd.isnull(sdata[col].iloc[25304,]))] ))
'''


'''UNPACK DICT COLUMNS

##check for keys
For every row within the dataframe
if the target column (dictionary) is not missing (float)
add keys in the corresponding cell to the list of (overlapping) keys
convert the list to a set of unique values (of keys)
'''
keys=list()
for i in random.sample(range(1, len(data.business.dropna())), 1000):
    print(i)
    keys+=data.business.dropna().iloc[i].keys()
keys=set(keys)#    if isinstance(data.business[i], float)==False:

#for col in data:        
#    if "dict" in str(data[col].apply(type)): 
#        print(col, data.loc[:,'votes distribution'].dropna().apply(pd.Series).columns)
#    else:
#        pass
    
for col in data:    
    if "dict" in str(type(data[col].dropna().iloc[0])):
        data=data.merge(data[col].dropna().apply(pd.Series), how='outer', left_index=True, right_index=True)
        data=data.drop(col, axis=1)
    else:
        pass
data.columns = data.columns.str.strip().str.lower().str.replace(' |-', '_').str.replace('(', '').str.replace(')', '')


# __________________________________________________________________________________________####  
# __________________________________________________________________________________________####  
###################################### SEQUELS

data['sequel']= np.select([data.follows.isnull()],[0],1)

f=open("M:/variables.txt",'w')
f.write(str(set(data.columns.tolist()+data.columns.tolist())))
f.close()

# __________________________________________________________________________________________####  
###################################### FILTER

#only feature-films
#data=data[data.kind.str[:]=='movie']    

# __________________________________________________________________________________________####  
###################################### BACK-UP and Export

bckp=data

help(pd.concat)


data.to_csv('D:\\imdbsql.csv', sep='\t' encoding='utf-8')














'''
def exchange(B, M, Y):
    from datetime import date
    import forex_python.converter 
    import numpy as np
    converted=[]
    change=[]
    wrong={yr:set() for yr in range(1880,2025)}
    test=[]
#    if (B!=0) & (np.isnan(np.float64(B))==False) & (np.isnan(Y)==False):            
        #print(B,M,Y)
    try:        
        change.append[int(forex_python.converter.convert(str(M).strip().replace('£', 'GBP').replace( '€','EUR').replace( '$','USD'), 'USD', int(B), date_obj=date(int(Y), 1, 1)))]
        converted.append(True)
        print(B,M,Y,change)

    except:
        change.append([None])
        test.append([B,M,Y])
        converted.append([B,M,Y])
        if np.isnan(Y) == False:
            wrong[Y].update([M,Y])   
        else:
            pass
  #  else: pass

    return(change, converted)

# __________________________________________________________________________________________####  
###################################### TSV fILES
principals=pd.read_csv("D:\\R\\title.principals.tsv", sep='\t')
basics=pd.read_csv("D:\\R\\title.basics.tsv", sep='\t')
crew=pd.read_csv("D:\\R\\title.crew.tsv", sep='\t')

from r
py2.robjects import pandas2ri
pandas2ri.activate()

from rpy2.robjects.packages import importr

base = importr('base')
# call an R function on a Pandas DataFrame

for my_pandas_dataframe in ["principals", "basics", "crew"]:
    base.summary(my_pandas_dataframe)

'''









'''
from time import sleep
from random import randint
from time import time


start_time_s = time()


#HTTP datasets

tt=[]
for row in rows: #for each title  
    tt.append(ia.get_imdbMovieID(row))  
    
ia=IMDb()
start_time = time()

titles=[]
for row in tt: #for each title
    title=pd.DataFrame() #empty dataframe
    if row is None:
         title['missing']=[row]
    else:
        # Pause the loop
        sleep(randint(8,15))     
        movie=ia.get_movie(row, ia.get_movie_infoset()) #['technical','plot']  ia.get_movie_infoset() get all info
        elapsed_time = time() - start_time
        
        keys = []
        for key in movie.infoset2keys:
            keys += list(movie.infoset2keys[key])
        keys = set(keys)
        #titles.append(keys)
        
        for i in keys:
            title[i]=[movie.get(i)] 
    titles.append(title) #add the retireved information of this title as a new dataframe to the list of all other titles (i.e. all other dataframes) 


#MERGE all titles into one dataframe 
hdata = pd.DataFrame()     
#columns=ia.get_movie(row, info=print(ia.get_movie_infoset())).infoset2keys.get('main')
for title in titles:
    hdata=pd.concat([hdata,title], axis=0, ignore_index=True, sort=False)




#UNPACK dictionary columns
 
    
for col in hdata:    
    if "dict" in str(hdata[col].apply(type)): 
        if "episodes" in str(hdata[col].apply(type)):
            pass
        elif "vote" in str(hdata[col].apply(type)):
            pass
        else:
            hdata=hdata.merge(hdata[col].dropna().apply(pd.Series), how='outer', left_index=True, right_index=True)
            hdata=hdata.drop(col, axis=1)
    else:
        pass



#COMBINE all data


#data=sdata.merge(hdata, how='outer', left_index=True, right_index=True)
data.drop_duplicates(cols=['color info_x', 'color info_y'],inplace=True)
data=hdata.combine_first(sdata)
'''

#f=open("variables.txt",'w')
#f.write(str(set(sdata.columns.tolist()+sdata.columns.tolist())))
#f.close()








'''rows=[]    
for i in range(0,len(results)):
    rows=rows+[results[i].movieID]



#HTTP infosets
ia=IMDb()
ia
ia.get_movie_infoset()



#SQL infosets
ia=IMDb('sql', uri='mysql://root:Hstdks87@[::1]:3306/imdb')
#ia.get_movie_infoset()

ii) example keys
3365405
68646
ia.get_movie_infoset()
movie=ia.get_movie(1000113, ['main','technical','plot'])
movie.get('title')
#'alternate versions', 'business', 'connections', 'crazy credits', 'episodes', 'goofs', 'keywords', 'literature', 'locations', 'main', 'plot', 'quotes', 'release dates', 'soundtrack', 'taglines', 'technical', 'trivia', 'vote details'])
#'technical', 'plot'


iii) example titles
tmp=["The Matrix","The Blair Witch Project","Dreams","Viva Italia!","The Godfather"]
results=[]
for i in range(0,len(tmp)):  
    print(i)
    results=results+(ia.search_movie(str(tmp[i]))) 


#UNPACK dictionary columns
for col in sdata:    
    if "dict" in str(sdata[col].apply(type)): 
        print (col)
        sdata=sdata.merge(sdata[col].dropna().apply(pd.Series), how='outer', left_index=True, right_index=True)
        sdata=sdata.drop(col, axis=1)
    else:
        pass


for row in rows: #for each title          
    title=pd.DataFrame() #empty dataframe
    movie=ia.get_movie(row, ia.get_movie_infoset()) #['technical','plot']  ia.get_movie_infoset() get all info
    keys = []
    for key in movie.infoset2keys:
        keys += list(movie.infoset2keys[key])
    keys = set(keys)
    #keys = list(keys)
    #titles.append(keys)
    for i in keys:
        title[i]=[movie.get(i)]    
    titles.append(title)

    #movies.append(movie.movieID)
    keys = []
    for key in movie.infoset2keys:
        keys += list(movie.infoset2keys[key])
    keys = set(keys)
    titles.append(keys)
    for i in keys:
        title[i]=[movie.get(i)]

    for i in movie.infoset2keys['main']:
        #print(movie.get(i))
        title[i]=[movie.get(i)]
        titles.append(title)
    
len(movie.infoset2keys['main'])
keys = []
for key in movie.infoset2keys:
    keys += list(movie.infoset2keys[key])
keys = set(keys)
len(keys)

#len(hdata.columns)
#hdata.columns.tolist()

#hdata.dtypes
#tmp=[]
for col in sdata:
    print (col, '\n', str(type(sdata.loc[col]))[6:]) 
    
    
keys = []
for key in movie.infoset2keys:
    keys += list(movie.infoset2keys[key])
keys = set(keys)


titles=[]
for row in rows: #for each title          
    title=pd.DataFrame() #empty dataframe
    movie=ia.get_movie(row, info=ia.get_movie_infoset()) #get all info
    keys = []
    for key in movie.infoset2keys:
        keys += list(movie.infoset2keys[key])
    keys = set(keys)
    for i in keys:
        if i is None:
            pass
        else:
            if type(movie.get(i))==dict:
                title[i]=[movie.get(i)]
                title[i]=sdata.i.dropna().apply(pd.Series)
                
            title[i]=[str(type(movie.get(i)))[6:]]
            #title[i]=[movie.get(i)]      
    titles.append(title) #add the retireved information of this title as a new dataframe to the list of all other titles (i.e. all other dataframes) 



df['Customer Number'].astype('int')
print(str(movie.infoset2keys.get('main'))[1:-1])
tmp.describe
filter(None, tmp)
data.to_csv('d:/data.csv', sep='\t', decimal='.', index=False)


data.head(15)
data.iloc[0:2,0]
tmp=data.dropna(axis=0, how='all', thresh=15, subset=None, inplace=False)
len(tmp)
tmp.iloc[10000:15000, 0:2]
results[0][1].movieID



cnn=set()
bsn=set()
dsk=set()
ltr=set()
key=[]
for i in range(0,len(sdata)):
    try: 
        key=sdata.loc[i,'connections'].keys()
        cnn.update(key)
        key=sdata.loc[i,'business'].keys()
        bsn.update(key)
        key=sdata.loc[i,'laserdisc'].keys()
        dsk.update(key)
        key=sdata.loc[i,'literature'].keys()
        ltr.update(key)
    except: 
        pass

    len(sdata.columns)
sdata.columns.tolist()

sdata.dtypes
tmp=[]
for col in hdata:
    if type(hdata.loc[17,col])==dict:
        print (col, '\n', str(type(hdata.loc[17,col]))[6:])
    else:
        pass

hdata.columns



tmp=pd.DataFrame()
col=sdata.columns[~sdata.columns.type==dict]

sdata.dtypes 
==dict


for col in sdata.loc[:, sdata.dtypes == srt].columns:
    if sdata[col].dtypes==dict:
        tmp=sdata.merge(sdata[col].dropna().apply(pd.Series), how='outer', left_index=True, right_index=True)
        tmp=sdata.drop([col], axis=1)
    else:
        pass     
          
        
#tmp = sdata.merge(sdata.business.dropna().apply(pd.Series), how='outer', left_index=True, right_index=True)
#tmp.drop('budget', axis=1).columns


#df.loc[:, df.dtypes == np.float64]
'''
        
'''junk
        try:

            if re.compile("\[").fullmatch(str(sdata[col].dropna().iloc[0])[0]):
                print('list: ', col)
                data[col]=data.cast.dropna().str.replace("\"","|").str.replace("\[\<","[\"<").str.replace(">]",">\"]").str.replace(", ","\",\"").apply(ast.literal_eval)#sdata[col].dropna().str.replace('"', "|").str.replace('^\[', '"').str.replace('\]$', '"').apply(ast.literal_eval)
            elif re.compile("\{").fullmatch(str(sdata[col].dropna().iloc[0])[0]):
                print('dict: ', col)
                data[col]=sdata[col].dropna().str.replace('"', "|").str.replace('^\{', '"').str.replace('\}$', '"').apply(ast.literal_eval)
            else:
                print(col)
                data[col]=sdata[col].dropna().str.replace('"', "|").str.replace('\': \[', '\': "').str.replace('\]\}', '"}').str.replace("], '", '", \'').apply(ast.literal_eval)
        except:
print("Warning: the following columns were not transformed from original format:", wrong)

'''
