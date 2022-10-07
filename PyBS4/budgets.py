import pickle
import re
from warnings import warn
from IPython.core.display import clear_output
from time import time
from time import sleep
from random import randint
from requests import get
from bs4 import BeautifulSoup
import pandas as pd

tconsts = list(set(pd.read_csv('T:\\Creativity\\1_data\\rtng.csv', sep='\t', encoding='utf-8').tconst))
#,'tt0262942', 'tt0292977']})#pd.read_csv('F:\\R\\tconsts1.csv', sep=',', delimiter=None, header='infer')

# Redeclaring the lists to store data in


htmls = []
currency= []
budgets = []



# Preparing the monitoring of the loop
start_time = time()
requests = 0

# For every year in the interval 2000-2017
for tconst in tconsts:
    print(tconst)
    # Make a get request
    response = get('https://www.imdb.com/title/' + tconst)
    print(response)
    htmls.append(response)

    # Pause the loop
    sleep(randint(8,15))

    # Monitor the requests
    requests += 1
    elapsed_time = time() - start_time
    print('Request:{}; Frequency: {} requests/s'.format(requests, requests/elapsed_time))
    clear_output(wait = True)

    # Throw a warning for non-200 status codes
    if response.status_code != 200:
        warn('Request: {}; Status code: {}'.format(requests, response.status_code))
        '''
    # Break the loop if the number of requests is greater than expected
    if requests > 72:
        warn('Number of requests was greater than expected.')  
        sleep(randint(15,25))
        '''
    # Parse the content of the request with BeautifulSoup
    html_soup = BeautifulSoup(response.text, 'html.parser')

    # Select all the 50 movie containers from a single page
    if html_soup.find('h4', text = 'Budget:')  is None:
        budget = None
        curr = None
    else: 
        budget = html_soup.find('h4', text = 'Budget:').find_parent('div').text
        print(budget)
        budget = re.sub(',', '', budget)

        curr = re.split('(\d+)',budget)[0].split(":")[1]
        budget = re.split('(\d+)',budget)[1]
    budgets.append(budget) 
    currency.append(curr)
        
'''    info_containers = html_soup.find_all('div', attrs={'class':'txt-block'})
    #print(info_containers)
    # For every movie of these 50
    budget = info_containers[9].text
    print(budget)

'''        
movie_info = pd.DataFrame({'tconst': tconsts,'budget': budgets, 'currency':currency})
movie_pages = pd.DataFrame({'tconst': tconsts,'html':htmls})


movie_info.to_csv('T:\\Creativity\\1_data\\budget.csv', sep='\t', encoding='utf-8')
movie_info.to_csv('M:\\budget.csv', sep='\t', encoding='utf-8')
with open("T:\\Creativity\\1_data\\budget.txt", "wb") as fp:   #Pickling
    pickle.dump(movie_info, fp)

movie_pages.to_csv('T:\\Creativity\\1_data\\title_pages.csv', sep='\t', encoding='utf-8')
movie_pages.to_csv('M:\\title_pages.csv', sep='\t', encoding='utf-8')
with open("T:\\Creativity\\1_data\\title_pages.txt", "wb") as fp:   #Pickling
    pickle.dump(movie_pages, fp)
            # Scrape the name
            #name = container.h3.a.text
            #names.append(name)

            # Scrape the year 
            #year = container.h3.find('span', class_ = 'lister-item-year').text
            #years.append(year)

            # Scrape the IMDB rating
            #imdb = float(container.strong.text)
            #imdb_ratings.append(imdb)

            # Scrape the Metascore
            #m_score = container.find('span', class_ = 'metascore').text
           # metascores.append(int(m_score))

            # Scrape the number of votes
           # vote = container.find('span', attrs = {'name':'nv'})['data-value']
           # votes.append(int(vote))