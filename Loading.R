#DATA SOURCE: #https://ftp.sunet.se/mirror/archive/ftp.sunet.se/pub/tv+movies/imdb/
#ftp://ftp.fu-berlin.de/pub/misc/movies/database/
#imdbpy2sql.py -d D:/DATA/IMDB/ -u postgresql://postgres:Hstdks87@[::1]:5432/imdb -c D:/DATA/IMDB


# CONFIG####
setwd(ddir)
library(tidyr)
library(dplyr)
library(readr)
library(data.table)
library(plyr)
library(stringr)
library(dummies)
search()# library(tidyverse)# remove("package:tidyverse")# detach("package:tidyverse")

# _______________####  
#RAW DATA SETS####
#read_lines(file.path(ddir, "variables.txt"))
#read.csv(file.path(ddir, "variables.txt"))

awrd <- read_tsv(file.path(ddir, "awrdselect.csv"), col_names = TRUE,  col_types = NULL, na = c("", "NA","\\N"))[,-1]
vote <- read_tsv(file.path(ddir, "title.ratings.tsv"), col_names = TRUE,  na = c("", "NA","\\N"))
film <- read_tsv(file.path(ddir, "film.csv"), col_names = TRUE,  col_types = NULL, na = c("", "NA","\\N"))[,c("ID", "tconst", "year", "sequel", "budget_USD")]#, "converted")]
colnames(film) <- c("id","tconst","year","sequel","budget")

prnc <- read_tsv(file.path(ddir, "title.principals.tsv"), col_names = TRUE, na = c("", "NA","\\N"), 
      col_types = list(characters = col_skip(), ordering = col_skip())) #WITHOUT characters , ordering endYear , originalTitle , primaryTitle, runtimeMinutes, primaryName ,deathYear ,knownForTitles
crew <- read_tsv(file.path(ddir, "title.crew.tsv"), col_names = TRUE,  na = c("", "NA","\\N"))
bscs <- read_tsv(file.path(ddir, "title.basics.tsv"), col_names = TRUE, 
      col_types = list(endYear = col_skip(), primaryTitle= col_skip(), 
      runtimeMinutes= col_skip()), na = c("", "NA","\\N"))
rtng <- read_delim(file.path(ddir, "rtng.csv"),  delim = "\t", col_names = TRUE,  col_types = 
      cols_only(metascore='n',tconst='c'), na = c("", "NA","\\N"))  #read_csv(file.path(PYdir, "Metacritic.csv"), col_names = TRUE,  col_types =       cols_only(metascore='n',tconst='c'), na = c("", "NA","\\N"))
      #for larger sample switch to py\\
rtng<-unique(rtng)

  #adding: year, vote, budget, sequels, etc
  rtng <- merge(x = rtng, y = bscs[ , c("tconst", "startYear")], by = "tconst", all.x=TRUE,  fill=F)#, "originalTitle"
  rtng <- merge(rtng, vote, by="tconst", all.x=T,  fill=F)
  rtng <- merge(rtng, film, by="tconst", all.x=T,  fill=F)
  #!converting all budgets to USD####
  #cur<-read.csv("D:\\py\\currency_rates.csv", sep = ",")
  #rtng$cnv<-cur[match(rtng$tconst, cur$tconst),6]
  #rtng[is.na(rtng$cnv),"cnv"]<-1
  #rtng$budget<-as.numeric(rtng$budget_0)*rtng$cnv
  #rtng<-rtng[,c(1:9)]

  # #parsing failures:
  # errprnc<-as.data.frame(table(problems(prnc)$actual)) #delimiter or quote: 879034 
  # errbscs<-as.data.frame(table(problems(bscs)$actual)) #a double: 215696 | delimiter or quote: 12387 

  #!missing-id: sequel!####
  rtng[is.na(rtng$sequel), "sequel"]<-0
  rtng[rtng$tconst%in%c("tt0158983","tt0112864"), "sequel"]<-1
  #> subseting: feature, non-adult, <=2003 ####
bscs<- bscs[ bscs$titleType=="movie" & bscs$isAdult==0,]# & bscs$startYear <= 2003, ]
prnc<- prnc[ prnc$tconst %in% bscs$tconst, ]
crew<-crew[ crew$tconst %in% bscs$tconst, ]
sapply(rtng, function(y) sum(length(which(is.na(y)))))#missing
sapply(rtng[complete.cases(rtng[ , 'startYear']),], function(y) sum(length(which(is.na(y)))))

nrow(rtng)
nrow(rtng[complete.cases(rtng[ , 'startYear']),])
mssrtng<-sapply(rtng, function(y) sum(length(which(is.na(y)))))
sapply(rtng, function(y) sum(length(which(is.na(y)))))
  
# _______________####  
#FULL CREW####

# A-MULTIPLE DIRECTORS####
crew$ndir <- sapply(strsplit(crew$directors,',' ), uniqueN) # number of directors per title

#> full director affiliations####
if(FALSE) {"
1- Save all titles
2- list of directos to columns
3- replace names  
4- initiating by first directors
5- adding 2nd 3d ... directors in a loop
  "}

tmp<-crew[,"tconst"]
tmp<-cbind(tmp, as.data.frame(str_split_fixed(crew$directors, ",", max(sapply(strsplit(crew$directors,'n' ), uniqueN))),stringsAsFactors = FALSE))
names(tmp)<-gsub(x = names(tmp), pattern = "V+", replacement = "director_")
is.na(tmp) <- tmp=='\\N'
is.na(tmp) <- tmp==''
cd<-unique(tmp[c(1, 2)])
cd<-cd[complete.cases(cd), ]
for(i in 3:max(sapply(strsplit(crew$directors,'n' ), uniqueN))+1)
  {cd<-unique(rbindlist(list(cd,setnames(na.omit(tmp[c(1, i)]), names(cd)))))}

  

# B-MULTIPLE WRITERS####
crew$nwrt <- sapply(strsplit(crew$writers,',' ), uniqueN) # number of writers per title

#> full writer affiliations####
tmp<-crew[,"tconst"]
tmp<-cbind(tmp, as.data.frame(str_split_fixed(crew$writers, ",", max(sapply(strsplit(crew$writers,'n' ), uniqueN))),stringsAsFactors = FALSE))
names(tmp)<-gsub(x = names(tmp), pattern = "V+", replacement = "writer_")
is.na(tmp) <- tmp=='\\N'
is.na(tmp) <- tmp==''
cw<-unique(tmp[c(1, 2)])
cw<-cw[complete.cases(cw), ]
for(i in 3:max(sapply(strsplit(crew$writers,'n' ), uniqueN))+1)
  {cw<-unique(rbindlist(list(cw,setnames(na.omit(tmp[c(1, i)]), names(cw)))))}

# C- PRINCIPALS: complementing by multuple director/writers####
if(FALSE) {"
1- adding category column
2- adding job column
3- 
4- 
  "}

cw<-cbind(cw,category="writer")
cd<-cbind(cd,category="director")

cd <- merge(x = cd, y = prnc, by.x=  c("tconst", "director_1", 'category') ,by.y =  c("tconst", "nconst", "category"), all.x=TRUE)
cw <- merge(x = cw, y = prnc, by.x=  c("tconst", "writer_1", "category") ,by.y =  c("tconst", "nconst", "category"), all.x=TRUE)
prnc <- unique(rbindlist(list(prnc, cd, cw)))
  #unifyig indicators for missing values
  is.na(prnc) <- prnc=='\\N'
  is.na(prnc) <- prnc==''
  gc()
# _______________####  
#GENERE-PERSON:for sampled directors' ALL past titles####
  
gnrs <- prnc[prnc$tconst %in% rtng$tconst, c("tconst", "nconst")]
gnrs <- prnc[prnc$nconst %in% gnrs$nconst , "tconst"]
gnrs <- unique(gnrs)
gnrs <- merge(x = gnrs, y = bscs[ , c("tconst", "genres")], by = "tconst", all.x=TRUE,  fill=F)
is.na(gnrs) <- gnrs==''
is.na(gnrs) <- gnrs=='\\N'
#> genre confusion####
gnrs$gConf<-stringr::str_count(gnrs$genres, ',')+1
#> genre dummies####
gnrs<-gnrs %>% mutate_at(c('genres'), replace_na, '\\N')
xx<- unique(trimws(unlist(strsplit(gnrs$genres, ","))))# gnrs$gConf <- sapply(strsplit(gnrs$gnrs,',' ), uniqueN)# xx<-c("prigen", "secgen", "thrgen")# gnrs[,xx]<-as.data.frame(str_split_fixed(gnrs$gnrs, ",", 3))# #genre dummies# gnrs<-cbind(gnrs, dummy(gnrs$prigen, sep = "_"))# names(gnrs)<-gsub(x = names(gnrs), pattern = "gnrs_", replacement = "pri_")# gnrs<-cbind(gnrs, dummy(gnrs$secgen, sep = "_"))# names(gnrs)<-gsub(x = names(gnrs), pattern = "gnrs_", replacement = "sec_")# gnrs<-cbind(gnrs, dummy(gnrs$thrgen, sep = "_"))# names(gnrs)<-gsub(x = names(gnrs), pattern = "gnrs_", replacement = "thr_")# gnrs$gnrs<-gsub(x = gnrs$gnrs, pattern = " ", replacement = "")
xx<-gsub(x = xx, pattern = "\\-", replacement = "_")
gnrs <- data.frame(lapply(gnrs, function(x) {gsub("\\-", "_", x)}))
for (i in na.omit(xx)) {
  Variable=i;
  print(Variable)
  gnrs[, Variable] <- dum(i, gnrs$genres)
}
#is.na(gnrs) <- gnrs=='\\N'
names(gnrs)<-gsub(x = names(gnrs), pattern = "d:RLoading.r_", replacement = "")

animentry<- gnrs[,c("tconst", "genres")][grep(paste(c("ocumentary", "nimation"), collapse = "|"), gnrs$genres),"tconst"]
# _______________####  
# EXPORTS####

rtng<-merge(rtng, bscs[,c('tconst','originalTitle')], all.x=TRUE)

#write.csv(rtng, file=file("D:\\py\\rtng.csv",encoding="UTF-8"))

#write.csv(gnrs, "D:\\R\\gnrs.csv")

out<-unique(prnc[prnc$tconst  %in%	rtng$tconst, "nconst"])
write.csv(out, file.path(ddir, "prnc.csv"))


# _______________####  
# RECONFIG####
detach("package:plyr")
detach("package:dummies")

rm(tmp)

gc()

library(igraph)

library(Matrix)


# library(stringi)

# 
# library(centiserve)

##library(hhi)

# _______________####  
#FIRST GENRE COMBINATIONS####
#cd <- cd[order(-abs(cd$ndir) ), ] 
#tmp<-cd[duplicated(cd[c(2,3)]),]
#names(gnrs)<-gsub(x = names(gnrs), pattern = "e:RFilms.r_", replacement = "pri_")
#gnrs<-as.data.frame(rtng$tconst)
#setnames(gnrs, old = c('rtng$tconst'), new = c('tconst'))