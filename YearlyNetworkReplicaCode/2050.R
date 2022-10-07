
#https://stackoverflow.com/questions/48822710/merge-edges-by-attribute-group-in-igraph-r
#https://www.google.nl/search?biw=1239&bih=621&ei=Ot5ZW_X3L83TkwX176PwDw&q=merge+edges+based+on+vertex+igraph&oq=merge+edges+based+on+vertex+igraph&gs_l=psy-ab.3...39470.44521.0.44882.27.27.0.0.0.0.95.1777.27.27.0....0...1.1.64.psy-ab..0.12.818...0i7i30k1j0i7i10i30k1j0i13k1j0i8i7i30k1j0i30k1j0i5i30k1j0i8i30k1j0i8i13i30k1j35i39k1.0.X-cE_4yiOGo


#KEEPING OBSERVATIONS UP TO 2006####
temp2006<-bscs[bscs$startYear <= 2006,]
temp2006<-prnc[prnc$tconst  %in%	temp2006$tconst,]
rtng2006<-rtng[rtng$startYear == 2006,]
rtng2006<-na.omit(rtng2006)
#rtng2006<-na.omit(rtng2006)
#rtng2006$genreconfusion <-  sapply (if (rtng2006$genres != "\\N"){ strsplit((rtng2006$genres',' ), uniqueN)})
#gc()


#>>>CONSTRUCTING <DIRECTOR> ATTRIBUTES####
#directors that have rated films in 2006####
dir <- temp2006[temp2006$tconst %in% rtng2006$tconst  & temp2006$category=='director', c("tconst", "nconst")]
dir <- temp2006[temp2006$nconst %in% dir$nconst & temp2006$category=='director' , c("tconst", "nconst","category")]
#adding genre and release years####
dir<- merge(x = dir[ , c("tconst", "nconst")], y = bscs[ , c("tconst", "startYear", "genres")], by = "tconst", all.x=TRUE, fill=F)
dir<- merge(x = dir, y = gnrs[ , c("tconst", "gConf")], by = "tconst", all.x=TRUE, fill=F)
dir<- merge(x = dir, y = rtng[ , c("tconst", "metascore")], by = "tconst", all.x=TRUE, fill=F)
#!!!!!!!!!!!!!!!!ATTENTION: NO GENRE!!!!!!!!!!!!!!!####
nogenre<-dir[dir$genres=="\\N",]
nogenre<-rbind(nogenre, dir[is.na(dir$genres)==TRUE,])
dir<- dir[dir$genres!="\\N",]
dir<-na.omit(dir)
tdir<-aggregate(nconst ~ tconst, dir, toString)
tdir$ndir <- sapply(strsplit(tdir$nconst,"\\, |\\,| " ), uniqueN)
#Experience SO AverageConf Newcomer Tenure####
dir0<-aggregate(metascore ~ nconst, dir, mean)
dir1<-aggregate(startYear ~ nconst, dir, paste)
dir2<-aggregate(gnrs ~ nconst, dir, toString)
dir3<-aggregate(gConf ~ nconst, dir, toString)
dir3$aConf<-sapply(strsplit(as.character(dir3$gConf), ",", fixed=T), function(x) mean(as.numeric(x)))
dir3<-dir3[,c(1,3)]
dir1<-na.omit(dir1)
dir2<-na.omit(dir2)
dir3<-na.omit(dir3)
dir<-merge(dir1, dir2)
dir<-merge(dir, dir3)
dir<-merge(dir, dir0)
dir$Experience <- sapply(dir$startYear, uniqueN)
dir$New <- as.numeric(dir$Experience == 1)
dir$SO <-sapply(strsplit(dir$genres,"\\, |\\,| " ), uniqueN)/dir$Experience
dir$tenure<-sapply(dir$startYear, min)
dir$tenure<-2006-as.numeric(dir$tenure)
#Hirfindahl####
dir2$drama<-str_count(dir2$genres, "Drama")
dir2$comedy<-str_count(dir2$genres, "Comedy")
dir2$fantasy<-str_count(dir2$genres, "Fantasy")
dir2$romance<-str_count(dir2$genres, "Romance")
dir2$action<-str_count(dir2$genres, "Action")
dir2$thriller<-str_count(dir2$genres, "Thriller")
dir2$adventure<-str_count(dir2$genres, "Adventure")
dir2$animation<-str_count(dir2$genres, "Animation")
dir2$scifi<-str_count(dir2$genres, "Sci-Fi")
dir2$music<-str_count(dir2$genres, "Music")
dir2$war<-str_count(dir2$genres, "War")
dir2$biography<-str_count(dir2$genres, "Biography")
dir2$crime<-str_count(dir2$genres, "Crime")
dir2$family<-str_count(dir2$genres, "Family")
dir2$musical<-str_count(dir2$genres, "Musical")
dir2$mystery<-str_count(dir2$genres, "Mystery")
dir2$western<-str_count(dir2$genres, "Western")
dir2$horror<-str_count(dir2$genres, "Horror")
dir2$sport<-str_count(dir2$genres, "Sport")
dir2$history<-str_count(dir2$genres, "History")

dir[,c("hhi","Entropy")]<-hhi(dir2[,3:22])

gc()

#>>>CONSTRUCTING WRITER ATTRIBUTES####
#writers that have rated films in 2006#### 
# wrt <- temp2006[temp2006$tconst %in% rtng2006$tconst  & temp2006$category=='writer', c("tconst", "nconst")]
# wrt <- temp2006[temp2006$nconst %in% wrt$nconst & temp2006$category=='writer' , c("tconst", "nconst")]
# #adding genre and release years####
# wrt<- merge(x = wrt[ , c("tconst", "nconst")], y = bscs[ , c("tconst", "startYear", "genres")], by = "tconst", all.x=TRUE, fill=F)
# wrt<- merge(x = wrt, y = gnrs[ , c("tconst", "gConf")], by = "tconst", all.x=TRUE, fill=F)
# #!!!!!!!!!!!!!!!!ATTENTION: NO GENRE!!!!!!!!!!!!!!!----
# nogenre<-wrt[wrt$genres=="\\N",]
# nogenre<-rbind(nogenre, wrt[is.na(wrt$genres)==TRUE,])
# wrt<- wrt[wrt$genres!="\\N",]
# wrt<-na.omit(wrt)
# twrt<-aggregate(nconst ~ tconst, wrt, toString)
# twrt$nwrt <- sapply(strsplit(twrt$nconst,"\\, |\\,| " ), uniqueN)
# #Experience SO AverageConf Newcomer Tenure#### 
# wrt1<-aggregate(startYear ~ nconst, wrt, paste)
# wrt2<-aggregate(gnrs ~ nconst, wrt, toString)
# wrt3<-aggregate(gConf ~ nconst, wrt, toString)
# wrt3$aConf<-sapply(strsplit(as.character(wrt3$gConf), ",", fixed=T), function(x) mean(as.numeric(x)))
# wrt3<-wrt3[,c(1,3)]
# wrt1<-na.omit(wrt1)
# wrt2<-na.omit(wrt2)
# wrt3<-na.omit(wrt3)
# wrt<-merge(wrt1, wrt2)
# wrt<-merge(wrt, wrt3)
# wrt$Experience <- sapply(wrt$startYear, uniqueN)
# wrt$New <- as.numeric(wrt$Experience == 1)
# wrt$SO <-sapply(strsplit(wrt$genres,"\\, |\\,| " ), uniqueN)/wrt$Experience
# wrt$tenure<-sapply(wrt$startYear, min)
# wrt$tenure<-2006-as.numeric(wrt$tenure)
# #Hirfindahl####
# wrt2$drama<-str_count(wrt2$genres, "Drama")
# wrt2$comedy<-str_count(wrt2$genres, "Comedy")
# wrt2$fantasy<-str_count(wrt2$genres, "Fantasy")
# wrt2$romance<-str_count(wrt2$genres, "Romance")
# wrt2$action<-str_count(wrt2$genres, "Action")
# wrt2$thriller<-str_count(wrt2$genres, "Thriller")
# wrt2$adventure<-str_count(wrt2$genres, "Adventure")
# wrt2$animation<-str_count(wrt2$genres, "Animation")
# wrt2$scifi<-str_count(wrt2$genres, "Sci-Fi")
# wrt2$music<-str_count(wrt2$genres, "Music")
# wrt2$war<-str_count(wrt2$genres, "War")
# wrt2$biography<-str_count(wrt2$genres, "Biography")
# wrt2$crime<-str_count(wrt2$genres, "Crime")
# wrt2$family<-str_count(wrt2$genres, "Family")
# wrt2$musical<-str_count(wrt2$genres, "Musical")
# wrt2$mystery<-str_count(wrt2$genres, "Mystery")
# wrt2$western<-str_count(wrt2$genres, "Western")
# wrt2$horror<-str_count(wrt2$genres, "Horror")
# wrt2$sport<-str_count(wrt2$genres, "Sport")
# wrt2$history<-str_count(wrt2$genres, "History")
# wrt[,c("hhi","Entropy")]<-hhi(wrt2[,3:22])
# rm(dir0, dir1, dir2, dir3)
# rm(wrt1, wrt2, wrt3)
# wrt<-wrt[,c(1,4:10,2,3)]#reordering and merging writer-director information
dir<-dir[,c(1,4:10,2,3)]
#t<-merge(twrt, tdir, all.x=T, all.y = T)

#CREATING 5 YEAR WINDOW FOR 2006####
w2006<- c(2006-4,2006-3,2006-2,2006-1,2006)

library(plyr)
#SEPARATING PROFESSIONALS BY ROLE####
director <- subset(temp2006, category=='director', select = c("tconst","nconst","category", "job"))
director = plyr::rename(director,c("nconst"="director"))
#unique(director$job)
cnmtg <- subset(temp2006, category=='cinematographer' , select = c("tconst","nconst","category", "job"))
#unique(cnmtg$job)
cnmtg <-subset(cnmtg, grepl("*director*",cnmtg$job))
cnmtg = plyr::rename(cnmtg,c("nconst"="cnmtg"))
editor <- subset(temp2006, category=='editor' , select = c("tconst","nconst","category", "job"))
editor = plyr::rename(editor,c("nconst"="editor"))
#unique(editor$job)
producer <- subset(temp2006, category=='producer' , select = c("tconst","nconst","category", "job"))
producer = plyr::rename(producer,c("nconst"="producer"))#keep an eye on producer job types

#unique(producer$job)
#rm(prdcr)
writer <- subset(temp2006, category=='writer' , select = c("tconst","nconst","category", "job"))
writer = plyr::rename(writer,c("nconst"="writer"))
dsgnr<- subset(temp2006, category=='production_designer' , select = c("tconst","nconst","category", "job"))
dsgnr = plyr::rename(dsgnr,c("nconst"="dsgnr"))
#rm(temp2006)

#CONSTRUCTING NETWORK####
#>>>DIRECTORS (AND ALL OTHER COLLABORATORS)####
affiliation <- as.data.frame(rbind(director
                                   , setNames(writer, names(director))
                                   , setNames(editor, names(director))
                                   , setNames(dsgnr, names(director))
                                   , setNames(producer, names(director))
                                   , setNames(cnmtg, names(director))))

sum(is.na(affiliation$tconst))
sum(is.na(affiliation$director)) #'remove missing before 'affcon'; if you also have missing values in tconst you will get something like:

#'"Error in validObject(.Object) : invalid class "dgTMatrix" object:all row indices (slot 'i') must be between 0 and nrow-1 in a TsparseMatrix" 
affiliation<-na.omit(affiliation, cols=director)

net<-affcon(affiliation) #Calculating the adjacency matrix
gc()
net<-graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
net<-simplify(net)

#Calculating structutal attributes####
dcr <- coreness(net, mode = c("all"))
dhs  <- hub_score(net)$vector
arpack_defaults$maxiter = 1000000000
dev   <- as.data.frame(eigen_centrality(net, options=arpack_defaults, directed=F))
dev   <- dev[,1]
dts <-neighborhood.size(net, nodes = V(net), 2, mode="all")-1
dcn <- constraint(net, nodes = V(net))
dpr <-page_rank(net, algo = c("prpack", "arpack", "power"), v=V(net),directed = FALSE)
dpr<-dpr$vector
#ev <- arpack(closeness(net), options=list(n=3, nev=2, ncv=3, which="LM", maxiter=200), sym=TRUE, complex = FALSE)

#>>>WRITERS (DIRECTORS, AND PRODUCERS)####
# affiliation <- as.data.frame(rbind(director
#                                    , setNames(producer, names(director))
#                                    , setNames(writer, names(director))))
# affiliation<-na.omit(affiliation, cols=director)
# net<-affcon(affiliation)
# gc()
# net<-graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
# net<-simplify(net)
# 
# #Calculating structutal attributes####
# wcr <- coreness(net, mode = c("all"))
# whs  <- hub_score(net)$vector
# arpack_defaults$maxiter = 1000000000
# wev   <- as.data.frame(eigen_centrality(net, options=arpack_defaults, directed=F))
# wev   <- wev[,1]
# wts <-neighborhood.size(net, nodes = V(net), 2, mode="all")-1
# wcn <- constraint(net, nodes = V(net))
# wpr <-page_rank(net, algo = c("prpack", "arpack", "power"), v=V(net),directed = FALSE)
# wpr<-wpr$vector

#SIMPLIFIED NETWORK####
affiliation<-as.data.frame(director)
affiliation<-na.omit(affiliation, cols=director)
net1<-affcon(as.data.frame(affiliation))
net1<-graph_from_adjacency_matrix(net1, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
net1<-get.edgelist(net1)

net2<-merge(x = director[ , c("tconst", "director")], y = writer[ , c("tconst", "writer")], by = "tconst", fill=TRUE, all.x=TRUE, all.y=T, allow.cartesian=TRUE)
net3<-merge(x = director[ , c("tconst", "director")], y = producer[ , c("tconst", "producer")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
net4<-merge(x = director[ , c("tconst", "director")], y =  dsgnr[ , c("tconst", "dsgnr")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
net5<-merge(x = director[ , c("tconst", "director")], y = cnmtg[ , c("tconst", "cnmtg")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
net6<-merge(x = director[ , c("tconst", "director")], y = editor[ , c("tconst", "editor")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)

net2<-net2[,c(3,2)]
net2<-na.omit(net2)
net2<-affcon(as.data.frame(net2))
net2<-graph_from_adjacency_matrix(net2, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
net2<-get.edgelist(net2)

net3<-net3[,c(3,2)]
net3<-na.omit(net3)
net3<-affcon(as.data.frame(net3))
net3<-graph_from_adjacency_matrix(net3, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
net3<-get.edgelist(net3)

net4<-net4[,c(3,2)]
net4<-na.omit(net4)
net4<-affcon(as.data.frame(net4))
net4<-graph_from_adjacency_matrix(net4, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
net4<-get.edgelist(net4)

net5<-net5[,c(3,2)]
net5<-na.omit(net5)
net5<-affcon(as.data.frame(net5))
net5<-graph_from_adjacency_matrix(net5, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
net5<-get.edgelist(net5)

net6<-net6[,c(3,2)]
net6<-na.omit(net6)
net6<-affcon(as.data.frame(net6))
net6<-graph_from_adjacency_matrix(net6, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
net6<-get.edgelist(net6)

edges <- rbind(net1, setNames(net2, names(net1))
               , setNames(net3, names(net1))
               , setNames(net4, names(net1))
               , setNames(net5, names(net1))
               , setNames(net6, names(net1)))
edges <- na.omit(edges)
snet<-graph_from_edgelist(edges, directed = FALSE)
snet<-simplify(snet)

#Calculating structutal attributes####
sdcr <- coreness(snet, mode = c("all"))
sdhs  <- hub_score(snet)$vector
arpack_defaults$maxiter = 1000000000
sdev   <- as.data.frame(eigen_centrality(snet, options=arpack_defaults, directed=F))
sdev   <- sdev[,1]
#sdts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")-1
sdcn <- constraint(snet, nodes = V(snet))
sdpr <-page_rank(snet, algo = c("prpack", "arpack", "power"), v=V(snet),directed = FALSE)
sdpr<-sdpr$vector
#ev <- arpack(closeness(snet), options=list(n=3, nev=2, ncv=3, which="LM", maxiter=200), sym=TRUE, complex = FALSE)
#sum(degree(dnet) < 1) # if value is non-zero you have isolates
#dnet <- delete_vertices(dnet, which(degree(dnet) < 1))
#cmpt<-neighborhood.size(dnet, vcount(dnet), mode="all")
#sclz <-closeness(dnet, normalized=TRUE)

#HEREEEEEEEEEEEEcentrality measures####
# net1<-affcon(as.data.frame(na.omit(writer, "writer")))
# net1<-graph_from_adjacency_matrix(net1, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
# net1<-get.edgelist(net1)
# 
# net2<-merge(x = writer[ , c("tconst", "writer")], y = director[ , c("tconst", "director")], by = "tconst", fill=TRUE, all.x=TRUE, all.y=T, allow.cartesian=TRUE)
# net3<-merge(x = writer[ , c("tconst", "writer")], y = producer[ , c("tconst", "producer")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
# 
# net2<-net2[,c(3,2)]
# net2<-na.omit(net2)
# net2<-affcon(as.data.frame(net2))
# net2<-graph_from_adjacency_matrix(net2, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
# net2<-get.edgelist(net2)
# 
# 
# net3<-net3[,c(3,2)]
# net3<-na.omit(net3)
# net3<-affcon(as.data.frame(net3))
# net3<-graph_from_adjacency_matrix(net3, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
# net3<-get.edgelist(net3)
# 
# edges <- rbind(net1, setNames(net2, names(net1))
#                , setNames(net3, names(net1)))
# edges <- na.omit(edges)
# snet<-graph_from_edgelist(edges, directed = FALSE)
# snet<-simplify(snet)
# 
# gc()

# swcr <- coreness(snet, mode = c("all"))
# swhs  <- hub_score(snet)$vector
# arpack_defaults$maxiter = 1000000000
# swev   <- as.data.frame(eigen_centrality(snet, options=arpack_defaults, directed=F))
# swev   <- swev[,1]
# #swts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")-1
# swcn <- constraint(snet, nodes = V(snet))
# swpr <-page_rank(snet, algo = c("prpack", "arpack", "power"), v=V(snet),directed = FALSE)
# swpr<-swpr$vector
# 
################MERGING DATA INTO ONE TABLE##################################################
rm(nconst)
dstr<- data.frame(nconst=names(dcn),hubs_score=dhs, page_rank=dpr, constraint=dcn, coreness=dcr, eigenvector_centrality=dev, two_step_degree=dts)
sdstr<- data.frame(nconst=names(sdcn),hubs_score=sdhs, page_rank=sdpr, constraint=sdcn, coreness=sdcr, eigenvector_centrality=sdev)
#, two_step_degree=sdts
dstr<-dstr[dstr$nconst%in% director$director,]
#sdtr<-sdtr[sdtr$nconst%in% director$director,]

# wstr<- data.frame(nconst=names(wcn),hubs_score=whs, page_rank=wpr, constraint=wcn, coreness=wcr, eigenvector_centrality=wev, two_step_degree=wts)
# swstr<- data.frame(nconst=names(swcn),hubs_score=swhs, page_rank=swpr, constraint=swcn, coreness=swcr, eigenvector_centrality=swev)
# wstr<-wstr[wstr$nconst%in% writer$writer,]
#, two_step_degree=swts
#swtr<-swtr[swtr$nconst%in% writer$writer,]
#%in% director$director|writer$writer)
rm(director, producer, writer, cnmtg,editor,dsgnr)

rtng2006<-merge(rtng2006, tdir, by="tconst", all.x = T)
rtng2006<-cbind(rtng2006, as.data.frame(str_split_fixed(rtng2006$nconst, ",", max(tdir$ndir)),stringsAsFactors = FALSE))
names(rtng2006)<-gsub(x = names(rtng2006), pattern = "V+", replacement = "Director_") 
# rtng2006<-merge(rtng2006, twrt, by="tconst", all.x = T)
# rtng2006<-cbind(rtng2006, as.data.frame(str_split_fixed(rtng2006$nconst.y, ",", max(twrt$nwrt)),stringsAsFactors = FALSE))
# names(rtng2006)<-gsub(x = names(rtng2006), pattern = "V+", replacement = "writer_") 

# wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
dir$startYear <- vapply(dir$startYear, paste, collapse = ", ", character(1L))

#ROLE-CONSOLIDATION####
nd <- names(rtng2006 %>% dplyr:: select(starts_with("director")))
nw <- names(rtng2006 %>% dplyr:: select(starts_with("writer")))
rtng2006$rlcl<-NA

index<-cbind(rtng2006$tconst,rtng2006[,nw], rtng2006[,nd])
index[] <- lapply(index, str_trim)
is.na(index) <- index==''
for(i in 1:ncol(index)){  index[is.na(index[,i]), i] <- i}
m<-cbind(index[,"rtng2006$tconst"], match_columns(index))
m<-m[rowSums(is.na(m)) != ncol(m)-1, ]
rtng2006$rlcl[rtng2006$tconst %in% m$`index[, "rtng2006$tconst"]`]<-1

rm(m, index, nd, nw)
gc()

dir<-merge(dir, dstr, by = "nconst", all.x=T)
dir<-merge(dir, sdstr, by = "nconst", all.x=T)
# wrt<-merge(wrt, wstr, by = "nconst", all.x=T)
# wrt<-merge(wrt, swstr, by = "nconst", all.x=T)
dir=dir[,c(1:8, 11:21, 9, 10)]
# wrt=wrt[,c(1:7, 10:21, 8, 9)]

names(dir)<-gsub(x = names(dir), pattern = "\\.x", replacement = "") 
names(dir)<-gsub(x = names(dir), pattern = "\\.y", replacement = "_smp") 
# names(wrt)<-gsub(x = names(wrt), pattern = ".x", replacement = "") 
# names(wrt)<-gsub(x = names(wrt), pattern = ".y", replacement = "_smp") 
names(rtng2006)<-gsub(x = names(rtng2006), pattern = "nconst.x", replacement = "directors") 
# names(rtng2006)<-gsub(x = names(rtng2006), pattern = "nconst.y", replacement = "writers") 

temp2006<-aggregate(category ~ nconst, temp2006, toString)


temp2006<-temp2006[temp2006$nconst  %in%	dir$nconst, 1:2]
temp2006$actress<-str_count(temp2006$category,             	"actress"            	)
temp2006$actor<-str_count(temp2006$category,               	"actor"              	)
temp2006$director<-str_count(temp2006$category,            	"director"           	)
temp2006$producer<-str_count(temp2006$category,            	"producer"           	)
temp2006$writer<-str_count(temp2006$category,              	"writer"             	)
temp2006$cinematographer<-str_count(temp2006$category,     	"cinematographer"    	)
temp2006$composer<-str_count(temp2006$category,            	"composer"           	)
temp2006$production_designer<-str_count(temp2006$category, 	"production_designer"	)
temp2006$editor<-str_count(temp2006$category,              	"editor"             	)


dir[,c("rlfcs","entropyrlfcs")]<-hhi(temp2006[,3:11])

rtng2006<-merge(x=rtng2006[,c("tconst","startYear", "Director_1", "ndir", "metascore","rlcl")], y=dir[,c(1:19,22)], by.x="Director_1", by.y="nconst", all.x = T)
#"Director_2", "writer_2","Director_3", "writer_3",
# rtng2006<-merge(x=rtng2006, y=wrt[,c(1:19)], by.x="writer_1", by.y="nconst", all.x = T)
names(rtng2006)<-gsub(x = names(rtng2006), pattern = "metascore.x", replacement = "metascore") 
names(rtng2006)<-gsub(x = names(rtng2006), pattern = "metascore.y", replacement = "avgmtscr") 
names(rtng2006)<-gsub(x = names(rtng2006), pattern = ".x", replacement = "_dir1")
names(rtng2006)<-gsub(x = names(rtng2006), pattern = "_dir1perience", replacement = "experience")
# names(rtng2006)<-gsub(x = names(rtng2006), pattern = ".y", replacement = "_wrt1")


#setwd("e:/RMT")
write.table(edges, file='edges2006.tsv', quote=FALSE, sep='\t', col.names = NA)
write.table(dir, file='dir2006.tsv', quote=FALSE, sep='\t', col.names = NA)
write.table(wrt, file='wrt2006.tsv', quote=FALSE, sep='\t', col.names = NA)
write.table(rtng2006, file='rtng2006.tsv', quote=FALSE, sep='\t', col.names = NA)
#setwd("c:/R")
rm(edges, Titles, str)
gc()




