import pickle
import numpy as np
import pandas as pd
from warnings import warn
from requests import get
from time import sleep
from random import randint
from time import time
from bs4 import BeautifulSoup
from IPython.core.display import clear_output

headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36'}
rtng=list(set(pd.read_csv('T:\\Creativity\\1_data\\title_pages.csv', sep='\t', encoding='utf-8').tconst))
#rtng.remove('tt1032757')
        #"D:\\py\\diagnose.csv")
Award=[]
start_time = time()
requests = 0
tt="tt0111161"#"tt1032757"

for tt in list(set(rtng.tconst)):
    print(list(set(rtng.tconst)).index(tt), tt)
    # Make a get request
    #response = get('https://www.imdb.com/search/title?title_type=feature&release_date=1970-01-01,2010-12-31&countries=us&runtime=60,&count=250&sort=num_votes,desc&page=' + page)
    response = get('https://www.imdb.com/title/' + tt + '/awards?ref_=tt_awd', headers=headers)
    # Pause the loop
    sleep(randint(4,8))
    
    # Monitor the requests
    requests += 1
    elapsed_time = time() - start_time
    print('Request:{}; Frequency: {} requests/s'.format(requests, requests/elapsed_time))
    clear_output(wait = True)
    
    # Throw a warning for non-200 status codes
    if response.status_code != 200:
        warn('Request: {}; Status code: {}'.format(requests, response.status_code))
        sleep(randint(60,2000))
    
    # Break the loop if the number of requests is greater than expected
    if requests == 9000:
        warn('Number of requests was greater than expected.')  
        sleep(randint(60,2000))
        #break 
   
    # Parse the content of the request with BeautifulSoup
    html_soup = BeautifulSoup(response.text, "html.parser")
    
    body=html_soup.find_all('div', class_="article listo")
    tmp= html_soup.find_all('table', class_="awards")
    
    head=body[0].find_all('h3')
    for j in range(0,len(tmp)):
        Awards=tmp[j].find_all('tr')
        for i in range(0,len(Awards)):
            print(i)
            if head[j+1] is None:
                event=None #E,Y
            else:
                event=head[j+1].text
                year=list(rtng[rtng.tconst==tt].year)[0]#
                awardyear=head[j+1].a.text
            if Awards[i].b is None:
                winner=None #A
            else:
                winner=Awards[i].b.text
                #Award.append([tt,np.nan,Awards[i].span.text,Awards[i].a['href'][6:15],Awards[i].find('td', class_="award_description").text])
            if Awards[i].span is None:
                prize=None
            else:
                prize=Awards[i].span.text
                #Award.append([tt,Awards[i].b.text,np.nan,Awards[i].a['href'][6:15],Awards[i].find('td', class_="award_description").text])
            if Awards[i].a is None:#C
                nconst=None
            else:
                c=Awards[i].find_all('a')
                nconst=[]
                for k in range(0,len(c)):
                    nconst.append(c[k]['href'][6:15])
                #Award.append([tt,Awards[i].b.text,Awards[i].span.text,np.nan,Awards[i].find('td', class_="award_description").text])
            if Awards[i].find('td', class_="award_description") is None:
                description=None
                #Award.append([tt,Awards[i].b.text,Awards[i].span.text,Awards[i].a['href'][6:15],np.nan])
            else:
                description=Awards[i].find('td', class_="award_description").text               
            #Award.append([tt,Awards[i].b.text,Awards[i].span.text,Awards[i].a['href'][6:15],Awards[i].find('td', class_="award_description").text])
            Award.append([tt,event,year,awardyear,winner,prize,nconst,description])

Awards=pd.DataFrame(Award)
Awards.columns=["tconst", "event", "year", "awardyear", "winner", "prize", "nconst", "description"]
Awards[['winner','prize']] = Awards[['winner','prize']].fillna(method='ffill')    


'''
# __________________________________________________________________________________________####  
###################################### EXPORT
with open("T:\\Creativity\\1_data\\Awardstitle.txt", "wb") as fp:   #Pickling
    pickle.dump(Awards, fp)

Awards.to_csv('T:\\Creativity\\1_data\\Awardstitle.csv', sep=',', encoding='utf-8')
Awards.to_csv('M:\\Awardstitle.csv', sep=',', encoding='utf-8')
'''

# __________________________________________________________________________________________####  
###################################### AWARDS
# import every award
#with open("T:\\Creativity\\1_data\\Awardstitle.txt", "rb") as fp:   #Pickling
#    Awards=pickle.load(fp)
    
awrd=Awards 
awrd = awrd.dropna(subset=['nconst'])



# input sample of individuals
prnc=list(set(pd.read_csv("T:\\Creativity\\1_data\\prnc.csv")["nconst"]))
prnc.extend(('nm0000244','nm0319266','nm0000217'))

# calculate number of awards per person-year
tmp=pd.DataFrame(prnc,columns=['nm'])
for i in tmp.index:
    for j in range(1989,2021):
        tmp.at[i, j] =awrd[~awrd.nconst.isna()][awrd.year.astype(int)==j].nconst.apply(set([tmp.at[i, 'nm']]).issubset).sum()

#awrd=tmp 
with open("T:\\Creativity\\1_data\\awrdall.txt", "wb") as fp:   #Pickling
    pickle.dump(tmp, fp)
tmp.to_csv('T:\\Creativity\\1_data\\awrdall.csv', sep='\t', encoding='utf-8')
tmp.to_csv('M:\\awrdall.csv', sep='\t', encoding='utf-8')

awrd=Awards[Awards.event.str.contains("Academy Awards, USA|Directors Guild of America|Writers Guild of America|American Society of Cinematographers|American Cinema Editors|Producers Guild of America|Golden Globes, USA|National Board of Review|New York Film Critics Circle|Los Angeles Film Critics Association")]
awrd = awrd.dropna(subset=['nconst'])
awrd['event']=awrd['event'].str.strip().str[:-6]
awrd=awrd.merge(rtng.loc[:,['tconst','name']].drop_duplicates(), on="tconst", how='left')
awrd.groupby([ 'event', 'year']).size().unstack(fill_value=0)[2019]
awrd.groupby([  'year', 'event']).size().unstack(fill_value=0).mean()
awrd.groupby([ 'event', 'year']).size().unstack(fill_value=0)[2019] - awrd.groupby([  'year', 'event']).size().unstack(fill_value=0).mean()
inspct=awrd[awrd.event=='Writers Guild of America, USA' ].groupby('year').apply(lambda x: set(x['name']))



Awards[Awards.tconst.str.contains("tt7125860")].event.value_counts()
awrd[awrd.tconst.str.contains("tt7125860")].event.value_counts()


# input sample of individuals
prnc=list(set(pd.read_csv("t:\\Creativity\\1_data\\prnc.csv")['nconst']))
prnc.extend(('nm0000244','nm0319266','nm0000217'))
prnc.index('nm0000217')
# calculate number of awards per person-year
tmp=pd.DataFrame(prnc,columns=['nm'])
for i in tmp.index:
    for j in range(1989,2021):
        tmp.at[i, j] =awrd[~awrd.nconst.isna()][awrd.year.astype(int)==j].nconst.apply(set([tmp.at[i, 'nm']]).issubset).sum()
#awrd=tmp 
tmp.columns        
for i in range(1,10):
    print(tmp.iloc[:,i].value_counts())
    
with open("T:\\Creativity\\1_data\\awrdselect.txt", "wb") as fp:   #Pickling
    pickle.dump(tmp, fp)
tmp.to_csv('t:\\Creativity\\1_data\\awrdselect.csv', sep='\t', encoding='utf-8')
tmp.to_csv('M:\\awrdselect.csv', sep='\t', encoding='utf-8')

