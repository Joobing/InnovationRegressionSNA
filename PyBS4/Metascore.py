import re
from warnings import warn
#warn("Warning Simulation")
from bs4 import BeautifulSoup
from time import sleep
from random import randint
import pandas as pd
from time import time
from requests import get

pages = ["1", "251", "501", "751"]
year_url = "1989-01-01,2021-12-31"

sony=["sony", "co0050868", "co0026545", "co0010568", "co0014453", "co0071094", "co0005883", "co0022781", "co0110101", "co0001799", "co0055524"]
universal=["universal", "co0005073", "co0042399", "co0007554", "co0018555", "co0017323", "co0123433", "co0043969", "co0195910", "co0000534", "co0003687", "co0314851", "co0137262"]
fox=["fox", "co0000756", "co0028932", "co0017497"]
warner=["warner", "co0002663", "co0026841", "co0024579", "co0183824" , "co0011489"] #warner: "co0020670"
mgm=["mgm", "co0007143", "co0456097", "co0043853", "co0046718", "co0106448", "co0110088",  "co0733129", "co0001995", "co0323215"]
disney=["disney" , "co0022594", "co0008970", "co0044374", "co0226183", "co0044279", "co0019626", "co0076240", "co0072626", "co0210118", "co0049348", "co0095134", "co0051941", "co0047120", "co0071326"]# disney: "co0010699"
paramount=["paramount", "co0023400", "co0632516", "co0135185", "co0029291", "co0179341"]
dreamworks=["dreamworks", "co0040938", "co0067641", "co0077647", "co0059559"]
lionsgate =["co0173285", "co0098779", "co0014903", "co0060306"]
regency=["co0021592", "co0025101"]

companies= [sony , paramount , mgm , fox , disney , universal , warner , dreamworks , regency , lionsgate] 


# Redeclaring the lists to store data in
headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36'}
rows=[]
record={}

# Preparing the monitoring of the loop
start_time = time()
requests = 0

# For every year in the interval 2000-2017

for company in [item for sublist in companies for item in sublist]:
    page=1
    number=2    
    print('http://www.imdb.com/search/title/?title_type=feature&release_date=' + year_url +"&companies=" +company+ '&count=250&start=' + str(page)+   "&ref_=adv_nxt")
    
    # For every page in the interval 1-4
    #company='co0323215'
    #pages=['1001', '1251', '1501', '1751', '2001']
    #for page in pages:
    while page <= number:
        #print(page, number)
        
        # Make a get request
        url = 'http://www.imdb.com/search/title/?title_type=feature&release_date=' + year_url + "&companies=" +company+ '&count=250&start=' + str(page) + "&ref_=adv_nxt" 
        response = get(url, headers = headers)

        # Pause the loop
        sleep(randint(8,15))

        # Monitor the requests
        requests += 1
        elapsed_time = time() - start_time
        
        #clear_output(wait = True)


        # Throw a warning for non-200 status codes
        if response.status_code != 200:
            warn('Request: {}; Status code: {}'.format(requests, response.status_code))
            sleep(2000)

        # Break the loop if the number of requests is greater than expected
        if requests > 15000:
            warn('Number of requests was greater than expected.')
            sleep(2000)            
            break


        # Parse the content of the request with BeautifulSoup
        page_html = BeautifulSoup(response.text, 'html.parser')
        try:
            number = next(item  for item in [int( re.sub(',', '', s)) if  re.sub(',', '', s).isdigit() else None for s in page_html.find('div', class_='desc').text.split()] if item is not None )
            record.update({company: number})
            print(company, page, number,'Request:{}; Frequency: {} requests/s'.format(requests, requests/elapsed_time))
        except:
            #print(e)
            number=0
            record.update({company+"-"+page: response})
        #log.append({company+"-"+page:response})

        # Select all the 50 movie containers from a single page
        mv_containers = page_html.find_all('div', class_ = 'lister-item mode-advanced')

        # For every movie of these 50
        for container in mv_containers:
            # If the movie has a Metascore, then:
            #if container.find('div', class_ = 'ratings-metascore') is not None:

                # Scrape the name
            if container.h3.a.get("href").split("/")[2] is None:
                tt= None
            else:
                tt = container.h3.a.get("href").split("/")[2]

            # Scrape the name
            if container.h3.a                                         is None:
                name = None
            else:
                name = container.h3.a.text

            # Scrape the year
            if container.h3.find('span', class_ = 'lister-item-year')  is None:
                year = None
            else: 
                year = container.h3.find('span', class_ = 'lister-item-year').text

            # Scrape the IMDB rating
            if container.strong                                         is None:
                rating = None
            else:
                rating = float(container.strong.text)

            # Scrape the Metascore
            if container.find('span', class_ = 'metascore')             is None:
                m_score = None
            else:
                m_score = container.find('span', class_ = 'metascore').text

            # Scrape the number of votes
            if container.find_all('span', attrs = {'name':'nv'})   is None:                
                vg = None
            else:                vg = [x['data-value'] for x in container.find_all('span', attrs = {'name':'nv'})]

            if container.find('span', class_='certificate')             is None:                
                certificate = None
            else:                certificate = container.find('span', class_='certificate').text
            
            if container.find('span', class_='runtime')                 is None:
                runtime = None
            else:                runtime = container.find('span', class_='runtime').text
            
            if container.find('span', class_='genre')                   is None:
                genre = None
            else:                genre = container.find('span', class_='genre').text
                          
            if container.find_all('p', class_='text-muted')[1]          is None:
                summary = None
            else:                summary = container.find_all('p', class_='text-muted')[1].text
            
            if container.find_all('p')[2]                                    is None:
                nconst = None
            else:                nconst = container.find_all('p')[2]
            
            rows.append([tt, name, year, rating, m_score, vg, genre, nconst, certificate, runtime, summary, company, page])
                
            '''
                if container.find_all('span', attrs = {'name':'nv'})[0]['data-value']     is None:
                    gross  = None
                else:                gross = container.find_all('span', attrs = {'name':'nv'})[1]['data-value']

            '''
        page=page+250


rtng=pd.DataFrame(rows)
rtng.columns= ["tconst", "name", "year", "rating", "metascore", "vg", "genre", "nconst", "certificate", "runtime", "summary", "company", "page"]
count=dict(rtng.company.value_counts())
sample= dict(rtng[rtng.year.str.extract('(\d+)').fillna(0).astype(int).between(1994,2002, inclusive=False)].company.value_counts())
rtng['yearinfo']=rtng.year
rtng.year=rtng.year.str.extract('(\d+)').fillna(0).astype(int)

count==record
{x: count[x] for x in count if x not in 'co0002663' and count[x] is not None}=={x: record[x] for x in record if x not in 'co0002663' and  record[x] is not None}
{ k : count[k] for k in set(count) - set(record) }
count.items() ^ record.items()
#record['co0020670']

#rtng= pd.read_csv('M:\\rtng.csv', sep='|')

{x[0]:sum([value for key, value in sample.items() if key in x]) for x in companies}.items() ^ {x[0]:sum([value for key, value in count.items() if key in x]) for x in companies}.items()
set({sum([value for key, value in sample.items() if key in x]):x[0] for x in companies}.items())
set({sum([value for key, value in count.items() if key in x]):x[0] for x in companies}.items())
{x:[len(set(rtng[rtng.year==x].tconst)),len(set(rtng[(rtng.year==x) & (~rtng.company.isin(regency+lionsgate))].tconst))] for x in range(1992,2003)}

rtng.to_csv('T:\\Creativity\\1_data\\rtng.csv', sep='\t', encoding='utf-8')
rtng.to_csv('M:\\rtng.csv', sep='\t', encoding='utf-8')
'''CAUTION::DANGEROUS!!!
import sys
sys.setrecursionlimit(10000)
with open("T:\\Creativity\\1_data\\rtng.txt", "wb") as fp:   #Pickling
    pickle.dump(rtng, fp)
'''