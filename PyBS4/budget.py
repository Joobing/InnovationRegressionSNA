url = 'https://www.imdb.com/title/tt0209144/'

from requests import get
response = get(url)
print(response.text[:500])


from bs4 import BeautifulSoup
html_soup = BeautifulSoup(response.text, 'html.parser')
type(html_soup)

#info_containers = html_soup.find_all('span', class_= 'awards-blurb')
info_containers = html_soup.find_all('div', class_= 'article')
#<h4 class="inline">Budget:</h4>
info_containers = html_soup.find_all('div', attrs={'class':'txt-block'})
print(info_containers)
type(info_containers)
info_containers[9].text
info_containers.text
first_mscore = info_containers[9].text
first_mscore
import re
line = re.sub(',', '', first_mscore)

line
type(line)   
int(line)
int(float(line))


#line = re.sub('\n', '', first_mscore)
#line = re.sub(',', '', line)
##line = re.sub(':', '', line)
#line = re.sub('udget$', ' ', line)
#line
#int(first_mscore)
#info_containers = html_soup.find_all('div', id = 'titleDetails')
#print(type(movie_containers))
#print(len(movie_containers))

#Using BeautifulSoup to parse the HTML content

#html_soup.Budget



#//*[@id="titleDetails"]/div[7]/h4
#//*[@id="titleDetails"]/div[7]/text()#

#print(type(movie_containers))
#print(len(movie_containers))

#Extracting the data for a single movie
#first_movie = movie_containers[0]
#first_movie

#The name of the movie
#first_movie.div
#
#first_name = first_movie.h3.a.text



#titleDetails
#<div class="txt-block">
 #     <h4 class="inline">Official Sites:</h4>
 #   <a href="/offsite/?page-action=offsite-otnemem&amp;token=BCYiuOwGB_yqD2gSZdYO276sCpnuBwlp3mrBD15WjU9LJ2_xjZKRBN-mHhVrx-OC7iE447y3b3wz%0D%0AfwL08NR776iBTRtiuY5BOg5RrtDitCd0j0MahWUgunzMyjbijaU30arKnbaBtFOkPI9honr6PthV%0D%0AJjbPOF_vl5yG1K1QvuIAGSubTQcKYlTQ_KGMNHICnpYtS6a8ThvmrqbDu2XImtSEdg%0D%0A&amp;ref_=tt_pdt_ofs_offsite_0" rel="nofollow">Memento</a>
 #         <span class="ghost">|</span>
 #       
 #   <a href="/offsite/?page-action=offsite-facebook&amp;token=BCYscRbkfcSVHuK4dcBpzIfwahqaWtymASC7Ww1g1uwGaMvQNBl5k8GRaPc9QFafgc5ZwrTyb-09%0D%0A81nhsE4l8BkyTH4mb9K2djiUKtKByII_j24Hmzlms7m5-NrITY1zVvpfylJN8NrxNh5YUKmX1umK%0D%0AiU9_4gye_wwXHeunlzVpaAOaq4F3cQsJMicnYplPZfENbbUqicwLe9aFJ3uFqfVO8cYOyeHZz46A%0D%0AM2s1YZDa4yM%0D%0A&amp;ref_=tt_pdt_ofs_offsite_1" rel="nofollow">Official Facebook</a>
 #              <span class="see-more inline">
  #    </span>
  #    </div>