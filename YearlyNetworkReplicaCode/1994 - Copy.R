

##KEEPING OBSERVATIONS UP TO 1994
temp1994<-basics[basics$startYear <= 1994,]
temp1994<-principals[principals$tconst  %in%	temp1994$tconst,]
rtng1994<-rtng[rtng$startYear <= 1994,]

#rtng1994<-na.omit(rtng1994)
#rtng1994$genreconfusion <-  sapply (if (rtng1994$genres != "\\N"){ strsplit((rtng1994$genres',' ), uniqueN)})

gc()

##CONSTRUCTING DIRECTOR ATTRIBUTES: SO Experience Newcomer Age
dir <- subset(principals, category=='director', select = c("tconst", "nconst"))
dir<- dir[dir$tconst %in% rtng1994$tconst ,]
#colnames(dir) <- c("tconst","dir")
dir<- merge(x = dir[ , c("tconst", "nconst")], y = basics[ , c("tconst", "startYear", "genres")], by = "tconst", all.x=TRUE, fill=F)
nogenre<-dir[dir$genres!="\\N",]
dir <- dir[dir$genres!="\\N",]
na.omit(dir)
#dir<- with(dir,aggregate(tconst ~ nconst, dir, toString))
dir1<-aggregate(genres ~ nconst, dir, toString)
dir2<-aggregate(startYear ~ nconst, dir, paste)
tdir<-aggregate(nconst ~ tconst, dir, toString)
tdir$ndir <- sapply(strsplit(tdir$nconst,',' ), uniqueN)

dir<-merge(dir1, dir2)
rm(dir1, dir2)
dir$SO <- sapply(strsplit(dir$genres,',' ), uniqueN)
dir$Experience <- sapply(dir$startYear, uniqueN)
dir$New <- as.numeric(dir$Experience == 1)
dir<-merge(x = dir, y = ppl[ , c("nconst", "birthYear")], by = "nconst", all.x=TRUE, fill=F)
dir$age<-1994-as.numeric(dir$birthYear)

gc()

##CONSTRUCTING WRITER ATTRIBUTES: SO Experience Newcomer Age
wrt <- subset(principals, category=='writer', select = c("tconst", "nconst"))
wrt<- wrt[wrt$tconst %in% rtng1994$tconst ,]
#colnames(wrt) <- c("tconst","wrt")
wrt<- merge(x = wrt[ , c("tconst", "nconst")], y = basics[ , c("tconst", "startYear", "genres")], by = "tconst", all.x=TRUE, fill=F)
nogenre<-rbind(nogenre,  wrt[wrt$genres!="\\N",])
wrt <- wrt[wrt$genres!="\\N",]
na.omit(wrt)
#wrt<- with(wrt,aggregate(tconst ~ nconst, wrt, toString))
dir1<-aggregate(genres ~ nconst, wrt, toString)
dir2<-aggregate(startYear ~ nconst, wrt, paste)
twrt<-aggregate(nconst ~ tconst, wrt, toString)
twrt$nwrt <- sapply(strsplit(twrt$nconst,',' ), uniqueN)
wrt<-merge(dir1, dir2)
rm(dir1, dir2)
wrt$SO <- sapply(strsplit(wrt$genres,',' ), uniqueN)
wrt$Experience <- sapply(wrt$startYear, uniqueN)
wrt$New <- as.numeric(wrt$Experience == 1)
t<-merge(twrt, tdir, all.y=T)

gc()





##ADDING DIRECTORS

#problem:director missing
#a<-rtng1994[!rtng1994$tconst %in% dir$tconst ,]
#a<-principals[which(principals$tconst == 'tt0066808'),]
#rtng1994 <- merge(dir, rtng1994, by = "tconst", all.y=T, fill=TRUE)

#gc()

##X ADDING WRITERS
#dir <- subset(principals, category=='writer', select = c("tconst", "nconst"))
#dir<- dir[dir$tconst %in% rtng1994$tconst ,]
#dir<- with(dir,aggregate(nconst ~ tconst, dir, toString))
#colnames(dir) <- c("tconst","writer")
#rtng1994<-merge(dir, rtng1994, by = "tconst", all.y=T, fill=TRUE)



##CREATING 5 YEAR WINDOW FOR 1994
w1994<- c(1994-4,1994-3,1994-2,1994-1,1994)

library(plyr)
##SEPARATING PROFESSIONALS BY ROLE
director <- subset(temp1994, category=='director', select = c("tconst","nconst","category", "job"))
director = plyr::rename(director,c("nconst"="director"))
#unique(director$job)
cnmtg <- subset(temp1994, category=='cinematographer' , select = c("tconst","nconst","category", "job"))
#unique(cnmtg$job)
cnmtg <-subset(cnmtg, grepl("*director*",cnmtg$job))
cnmtg = plyr::rename(cnmtg,c("nconst"="cnmtg"))
editor <- subset(temp1994, category=='editor' , select = c("tconst","nconst","category", "job"))
editor = plyr::rename(editor,c("nconst"="editor"))
#unique(editor$job)
producer <- subset(temp1994, category=='producer' , select = c("tconst","nconst","category", "job"))
producer = plyr::rename(producer,c("nconst"="producer"))
#keep an eye on producer job types
#unique(producer$job)
#rm(prdcr)
writer <- subset(temp1994, category=='writer' , select = c("tconst","nconst","category", "job"))
writer = plyr::rename(writer,c("nconst"="writer"))
dsgnr<- subset(temp1994, category=='production_designer' , select = c("tconst","nconst","category", "job"))
dsgnr = plyr::rename(dsgnr,c("nconst"="dsgnr"))
rm(temp1994)



gc()
##CREATING EDGE LIST FOR 1994
Titles<-merge(x = director[ , c("tconst", "director")], y = writer[ , c("tconst", "writer")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
Titles<-merge(Titles, cnmtg[ , c("tconst", "cnmtg")],   by.x='tconst', by.y = 'tconst', all.x = T, all.y=T, fill=TRUE)
Titles<-merge(Titles, editor[ , c("tconst", "editor")],  by.x='tconst', by.y = 'tconst', all.x = T, all.y=T, fill=TRUE )
Titles<-merge(Titles, dsgnr[ , c("tconst", "dsgnr")],  by.x='tconst', by.y = 'tconst', all.x = T, all.y=T, fill=TRUE)
Titles<-merge(Titles, producer[ , c("tconst", "producer")],  by.x='tconst', by.y = 'tconst', all.x = T, all.y=T, fill=TRUE)

rtng1994<-rtng1994[rtng1994$startYear == 1994,]
#rtng1994[rtng1994=='NA'] <- NA
#na<-rtng1994[rowSums(is.na(rtng1994)) > 0,]
rtng1994<-na.omit(rtng1994)



rtng1994<-merge(rtng1994, tdir, by="tconst", all.x = T)
rtng1994<-cbind(rtng1994, as.data.frame(str_split_fixed(rtng1994$nconst, ",", max(tdir$ndir)),stringsAsFactors = FALSE))
names(rtng1994)<-gsub(x = names(rtng1994), pattern = "V+", replacement = "Director_") 

rtng1994<-merge(rtng1994, twrt, by="tconst", all.x = T)
rtng1994<-cbind(rtng1994, as.data.frame(str_split_fixed(rtng1994$nconst.y, ",", max(twrt$nwrt)),stringsAsFactors = FALSE))
names(rtng1994)<-gsub(x = names(rtng1994), pattern = "V+", replacement = "writer_") 
#for( j in 1:twrt$nwrt){for (i in 1:tdir$ndir){rtng1994$rlcl<-within(rtng1994, isTRUE(rtng1994$Director_i==rtng1994$Writer_j) )}}


#tdir<-tdir[tdir$ndir==1,]
#director<-director[director$tconst %in% tdir$tconst,]
#tdir<-merge(x=rtng1994, y=director[,c("tconst", "director")], by.x="tconst", all.x=T)
#dir<-merge(x=tdir, y=dir, by.x="director", by.y= "nconst", all.x=T)

#twrt<-twrt[twrt$nwrt==1,]
#writer<-writer[writer$tconst %in% twrt$tconst,]
#twrt<-merge(x=rtng1994, y=writer[,c("tconst", "writer")], by.x="tconst", all.x=T)
#wrt<-merge(x=twrt, y=wrt, by.x="writer", by.y= "nconst", all.x=T)




rm(director, producer, writer, cnmtg,editor,dsgnr)
gc()

e23<-Titles[c(1, 2,3)]
e24<-Titles[c(1,2,4)]
e25 <- Titles[c(1,2,5)]
e26 <- Titles[c(1,2,6)] 
e27 <- Titles[c(1,2,7)]
e34 <- Titles[c(1,3,4)]
e35 <- Titles[c(1,3,5)]
e36 <- Titles[c(1,3,6)]
e37 <- Titles[c(1,3,7)]
e45 <- Titles[c(1,4,5)]
e46 <- Titles[c(1,4,6)]
e47 <- Titles[c(1,4,7)]
e56 <- Titles[c(1,5,6)]
e57 <- Titles[c(1,5,7)]
e67 <- Titles[c(1,6,7)]

edges <- rbind(e23, setNames(rev(e24), names(e23)), setNames(rev(e25), names(e23))
               , setNames(rev(e26), names(e23))
               , setNames(rev(e27), names(e23))
               , setNames(rev(e34), names(e23))
               , setNames(rev(e35), names(e23))
               , setNames(rev(e36), names(e23))
               , setNames(rev(e37), names(e23))
               , setNames(rev(e45), names(e23))
               , setNames(rev(e46), names(e23))
               , setNames(rev(e47), names(e23))
               , setNames(rev(e56), names(e23))
               , setNames(rev(e57), names(e23))
               , setNames(rev(e67), names(e23)))
edges <- na.omit(edges)
rm(e23, e24, e25, e26, e27, e34, e35, e36, e37, e45, e46, e47, e56, e57, e67) 
edges <- edges[,names(edges)!="tconst"]
#install.packages("igraph")

net <- graph.data.frame(edges, directed=F)
#net <- as.data.frame(net)
V(net)
E(net)

#CALCULATING STRUCTURAL ATTRIBUTES
#hs <- hub_score(net)$vector
c <- constraint(net, nodes = V(net), weights = NULL)
crns <- coreness(net, mode = c("all"))
#library(centiserve)
#katz <- katzcent(net, vids = V(net), alpha = 0.1)
str<-( data.frame(nconst=names(c), constraint=c, core=crns))



wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
dir$startYear <- vapply(dir$startYear, paste, collapse = ", ", character(1L))
nd <- names(rtng1994 %>% dplyr:: select(starts_with("director")))
nw <- names(rtng1994 %>% dplyr:: select(starts_with("writer")))
rtng1994$rlcl<-NA

index<-cbind(rtng1994$tconst,rtng1994[,nw], rtng1994[,nd])
index[] <- lapply(index, str_trim)
is.na(index) <- index==''
for(i in 1:ncol(index)){  index[is.na(index[,i]), i] <- i}
m<-cbind(index[,"rtng1994$tconst"], match_columns(index))
m<-m[rowSums(is.na(m)) != ncol(m)-1, ]
rtng1994$rlcl[rtng1994$tconst %in% m$`index[, "rtng1994$tconst"]`]<-1

rm(m, index, nd, nw)
gc




dir<-merge(dir, str, by = "nconst", all.x=T)
wrt<-merge(wrt, str, by = "nconst", all.x=T)

names(rtng1994)<-gsub(x = names(rtng1994), pattern = "nconst.x", replacement = "directors") 
names(rtng1994)<-gsub(x = names(rtng1994), pattern = "nconst.y", replacement = "writers") 
rtng1994<-rtng1994[,c("tconst","metascore","startYear","directors","writers","rlcl")]

setwd("e:/RMT")
write.table(edges, file='edges1994.tsv', quote=FALSE, sep='\t', col.names = NA)
write.table(dir, file='dir1994.tsv', quote=FALSE, sep='\t', col.names = NA)
write.table(wrt, file='wrt1994.tsv', quote=FALSE, sep='\t', col.names = NA)
write.table(rtng1994, file='rtng1994.tsv', quote=FALSE, sep='\t', col.names = NA)
setwd("c:/R")
rm(edges, Titles, str)
gc()

#wrt<-wrt[wrt$tconst %in% rtng1994$tconst]
#twrt<-twrt[twrt$nwrt==1,]
#wrt<-wrt[wrt$tconst %in% rtng1994$tconst]
#wrt<-wrt[wrt$tconst %in% twrt$tconst]


#VISUALIZING HUBS
#par(mar=c(1,1,1,1))
#plot(simplify(net), edge.arrow.size=0.0003,  vertex.label = ifelse(hs*1e15 > 0.9, V(net)$name, NA), vertex.label.cex=1, vertex.size=pmin(constraint, 25), main='Constraints', vertex.color=rainbow(52), layout= layout.auto)
#plot(simplify(net), edge.arrow.size=0.0003,  vertex.label = ifelse(hs*1e15 > 0.9, V(net)$name, NA), vertex.label.cex=1, vertex.size=pmin(crns, 25), main='Hubs', vertex.color=rainbow(52), layout= layout.auto)
#plot(net, edge.arrow.size=0.0003,  vertex.label = ifelse(hs*1e15 > 0.9, V(net)$name, NA), vertex.label.cex=1, vertex.size=pmin(crns, 25), main='Hubs', vertex.color=rainbow(52), layout= layout.auto)



#nd <- names(rtng1994 %>% dplyr:: select(starts_with("director")))
#nw<- names(rtng1994 %>% dplyr:: select(starts_with("writer")))
#nd <- "director_1"
#nw <- "writer_1"
#rtng1994$rlcl<-NA
#for (i in nd){ for (j in nw) {rtng1994$rlcl[rtng1994[,as.character(i) ]==rtng1994[,as.character(j)]]<-1}}
#rtng1994$rlcl[rtng1994[,"Director_1"]==rtng1994[,"writer_1"]]<-2
#for (i in nd){ for (j in nw) {rtng1994$rlcl[rtng1994[,i]==rtng1994[,j]]<-1}}

#xx<-c("prigen", "secgen", "thrgen")
#genres[xx]<-as.data.frame(str_split_fixed(genres$genres, ",", 3))






