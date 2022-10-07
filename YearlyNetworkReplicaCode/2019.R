# __________________________________________________________________________________________####  
#############################RESAMPLE2019 ####
if(FALSE) {"
1- 2019 (number of) titles
2- relevant (present and past) affiliations to 2019
  # Crew of titles of major-STUDIOs in 2019
  # Titles that share CREW with titles of major-STUDIOs in 2019
  # Crew of aLL titles that share CREW with titles of major-STUDIOs in 2019
  # Up to 2019
3- THREE YEAR window for 2019
4- dropping animation and documentary
2016, 2017, 2018, 2019, 2001, 2002
  "}

rtng2019<-rtng[rtng$startYear == 2019,]
#rtng2019<-rtng2019[complete.cases(rtng2019[,1]),]
rtng2019<-unique(rtng2019)
rtng2019=rtng2019[!sum(is.na(rtng2019))==ncol(rtng2019),]


n2019<-nrow(rtng2019)

temp2019<-unique(prnc[prnc$tconst  %in%	rtng2019$tconst, "nconst"])#
temp2019<-prnc[prnc$nconst  %in%	temp2019$nconst, "tconst"]#
temp2019<-unique(prnc[prnc$tconst  %in%	temp2019$tconst,])#
temp2019<-temp2019[temp2019$tconst %in% bscs[bscs$startYear <= 2019,]$tconst,]# #temp2019<-merge(temp2019,bscs[,c("tconst","startYear")], by="tconst", all.x = T)# temp2019<-merge(temp2019, bscs[,c("tconst", "startYear")], by="tconst", all.x = TRUE)


temp2019<-temp2019[!(temp2019$tconst %in% animentry),]
rtng2019<-rtng2019[!(rtng2019$tconst %in% animentry),]


w2019 <- c(2019-4, 2019-3, 2019-2 ,2019-1, 2019)#library(plyr)#,2019-3,2019-4,2019-5,2019-6
w2019 <-unique(bscs[bscs$startYear %in% w2019,"tconst"])


# __________________________________________________________________________________________####  
#############################NETWORK of COLLABORATION####
#A-SEPARATING PROFESSIONALS BY ROLE####
tmp <- temp2019[(temp2019$tconst %in% w2019$tconst) & (temp2019$nconst %in% temp2019[temp2019$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"),]$nconst) , ]
tmp <- tmp[complete.cases(tmp[,c("tconst","nconst")]),]

#B-GRAPH FROM EDGE-LIST#### 
g<-          graph_from_data_frame(tmp, directed=FALSE)
g<-          simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-  V(g)$name %in% unique(tmp$nconst)
PROJECTION<- bipartite.projection(g) #two-mode
tnet<-       PROJECTION[[1]] #one-mode
g<-          PROJECTION[[2]] #one-mode
g<-          set.edge.attribute(g, "weight", E(g), 1)
cmpnt2019 <- no.clusters(g);           # Number of components
larg2019 <- sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1]; # Size of largest component
rlarg2019 <- (sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1])/vcount(g); # Relative size of largest component

#C-CALCULATING STRUCTUTAL ATTRIBUTES####
dcr <-coreness(g, mode = c("all")) #_coreness####
dhs <-hub_score(g)$vector#_hubs####
arpack_defaults$maxiter = 1000000000
dev <-as.data.frame(eigen_centrality(g, options=arpack_defaults, directed=F)) #_eigenvector####
dev <-dev[,1]
dts <-neighborhood.size(g, nodes = V(g), 2, mode="all") #_component size####
dcn <- 1- constraint(g, nodes = V(g)) #_brokerage 1.125####
#dcn[(!is.na(dcn))&(dcn<0&dcn)]<-0
dpr <-page_rank(g, algo = c("prpack", "arpack", "power"), v=V(g),directed = FALSE) #_pagerank####
dpr <-dpr$vector #ev <- arpack(closeness(g), options=list(n=3, nev=2, ncv=3, which="LM", maxiter=200), sym=TRUE, complex = FALSE)

#D-ORDERING STRUCTUTAL ATTRIBUTES####
dstr<- data.frame(nconst=names(dcn),hubs_score=dhs, page_rank=dpr, brokerage=dcn, coreness=dcr, eigenvector_centrality=dev, two_step_degree=dts) 
#dstr<-dstr[dstr$nconst%in% director$director,]

# __________________________________________________________________________________________####  
#############################INDIVIDUAL LEVEL MODEL####

#cast and crew of titles from major-studios in 2019 
link2019<-temp2019[temp2019$tconst %in% rtng2019$tconst,] #as_tibble(temp2019[temp2019$tconst %in% rtng2019$tconst,1:3])

g<- graph_from_data_frame(link2019[,1:2], directed=FALSE)
g<-simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-    V(g)$name %in% unique(link2019$nconst)
PROJECTION<-bipartite.projection(g) #two-mode
tnet<-PROJECTION[[1]] #one-mode
g<-PROJECTION[[2]]#one-mode
g <- set.edge.attribute(g, "weight", E(g), 1)


#A-CREW with titles from major-studios in 2019#### 
#!!!when individual performs multiple roles in a movie, that movie is entering multiple times in the calculations for that individual!!!####
indv2019 <- temp2019[temp2019$nconst %in% link2019[link2019$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst,c("tconst", "nconst","category")]
  #indv2019 <- indv2019[indv2019$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,] #& temp2019$category=='director' #[,-3] 
  #_adding genre, years, gconf, and metascores####
  indv2019<- merge(x = indv2019, y = bscs[ , c("tconst", "startYear", "genres")], all.x = T)#, by = "tconst", all.x=TRUE, fill=F
  #_adding gconf####
  indv2019<- merge(x = indv2019, y = gnrs[gnrs$tconst %in% w2019$tconst, c("tconst", "gConf")], all.x = TRUE)
  #_adding metascores####
  indv2019<- merge(x = indv2019, y = rtng2019[ , c("tconst", "metascore")], all.x = TRUE)
#k<-length(indv2019) # indv2019 <-indv2019[na.omit(indv2019[,1:as.numeric(k)])]
  indv2019 <- unique(indv2019)
  #!NO GENRE!####
  mssgnrs2019<-indv2019[indv2019$genres=="\\N",]
  mssgnrs2019<-rbind(mssgnrs2019, indv2019[is.na(indv2019$genres)==TRUE,])#indv2019<- indv2019[indv2019$genres!="\\N",]
  #indv2019<-link2019 %>% mutate_at(c('genres'), replace_na, '\\N')
  


length(unique(link2019[link2019$category %in%  c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst))
length(unique(indv2019$nconst))

#B-INDIVIDUAL ATTRIBUTES #### 
indv2019<-merge(aggregate(startYear ~ nconst, indv2019, paste, na.action=NULL),merge(
  aggregate(metascore ~ nconst, indv2019, function(x) mean(as.numeric(x), na.rm=TRUE), na.action=NULL),merge(#_average METASCORE ####
#!!!!!if one genre is NA gconf becomes NA  ######################                                                                                                             
    aggregate(gConf ~ nconst, indv2019, toString, na.action=na.omit),
    aggregate(genres ~ nconst, indv2019, toString, na.action=NULL)
    ))) #indv2019<-indv2019[,c(1,4:10,2,3)] for reordering columns
setnames(indv2019, "startYear", "year")
indv2019$startYear<-as.numeric(sapply(indv2019$year, min))#_startYear####
# sy2019<-indv2019[,c("nconst","sy")]
# indv2019<-indv2019[,!(names(indv2019)%in%c("sy"))]

indv2019$year <- vapply(indv2019$year, paste, collapse = ", ", character(1L)) # wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
indv2019$tenure<-2019-indv2019$startYear#_tenure####
indv2019$experience <- sapply(indv2019$year, function(x) str_count(x,"\\,"))#_experience####
indv2019$newcomer <- as.numeric(indv2019$experience == 0)#_newcomer####
indv2019$average_confusion<-sapply(strsplit(as.character(indv2019$gConf), ",", fixed=T), function(x) mean(as.numeric(x), na.rm=TRUE))#_average confusion####
indv2019$orientation <-sapply(strsplit(indv2019$genres,"\\, |\\,| " ), uniqueN)/(1+indv2019$experience)#!_so####
indv2019$year <- 2019 #_focal year####


#_role consolidation####
indv2019<-merge(indv2019,aggregate(n ~ nconst, dplyr::add_count(link2019, tconst, nconst), function(x) mean(as.numeric(x), na.rm=TRUE)), all.x = T)
setnames(indv2019, "n", "role_consolidation")


#_awards quality####
  #for all collaborators with titles in 2019 
  link2019<-merge(link2019,awrd,by.x='nconst', by.y='nm', all.x=TRUE) #nrow(na.omit(temp2019[,-4]))==nrow(temp2019) #nrow(na.omit(awrd))==nrow(awrd)
#!awards for missing nm in awrd replaced with zero####
  link2019<-link2019 %>% mutate_at(c('2019', '2018','2017','2016','2015','1995'), replace_na, 0)
  link2019[,'awards']<-link2019$`2019` #names(indv2019$`2019`)<-"awards"
  link2019[link2019$nconst=="nm7016360",c('nconst','awards')]
  link2019[is.na(link2019$awards)==TRUE,'awards']
  link2019[,'quality']<-link2019$`2018`+link2019$`2017`+link2019$`2016`+link2019$`2015`
indv2019<-unique(merge(indv2019,link2019[,c("nconst","awards","quality")],by='nconst', all.x=TRUE))




indv2019<-merge(indv2019,aggregate(budget ~ nconst, merge(link2019[,c("nconst","tconst")],rtng2019[,c("tconst","budget")], by="tconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst", all.x=T) #_budget####
indv2019<-merge(indv2019,aggregate(sequel ~ nconst, merge(link2019[,c("nconst","tconst")],rtng2019[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel####

#############################C-ALTER ATTRIBUTES####
#_alter quality####
tmp<-data.frame()
test<-data.frame()
#ind <-   which(sapply(link2019, is.numeric))
#for(j in ind){
  #set(link2019, i = which(is.na(link2019[[j]])), j = j, value = 0)}
for(nconst in names(V(delete.vertices(g, which(degree(g)==0))))){
  c<-neighbors(g, nconst, mode = "total")
  tmp<-rbind(tmp,data.frame(nconst, 
                            setNames(
                              aggregate(quality ~ tconst, 
                              link2019[link2019$nconst %in% str_trim(names(c)),], function(x) mean(as.numeric(x), na.rm=TRUE))
                              , c("tconst", "team_quality"))
                            )
             )} # V(tmp1)$name[c] 
indv2019<-merge(indv2019,aggregate(team_quality ~ nconst, tmp[tmp$tconst %in% rtng2019$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) 

#_team coreness####
link2019<-merge(x=link2019,y=dstr[,c("nconst","coreness")],by="nconst",all.x = TRUE)#[0:2,]
tmp<-data.frame()
for(nconst in link2019$nconst){
  c<-neighbors(g, nconst, mode = "total")
    if(nrow(link2019[link2019$nconst %in% names(c) & !is.na(link2019$coreness),])>0){
    tmp<-rbind(tmp,data.frame(nconst, na.omit(aggregate(coreness ~ tconst, link2019[link2019$nconst %in% str_trim(names(c)) & !is.na(link2019$coreness),], function(x) mean(as.numeric(x), na.rm=TRUE)))))}} # V(tmp1)$name[c] 
indv2019<-merge(indv2019,aggregate(coreness ~ nconst, tmp[tmp$tconst %in% rtng2019$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) # unique(link2019[link2019$nconst %in% names(c) & !link2019$category %in% c("actor","actress" ),]$category)# str(link2019[link2019$nconst %in% names(c) & !link2019$category %in% c("actor","actress" ),])
names(indv2019)<-gsub(x = names(indv2019), pattern = "coreness", replacement = "team_coreness") 

# #_role dummies####
# roles<-unique(temp2019$category)
# roles<-roles[!(roles %in% c("archive_footage", "self"))]
# indv2019<-merge(indv2019,na.omit(aggregate(category ~ nconst, temp2019[temp2019$tconst %in% rtng2019$tconst], toString)), by="nconst", all.x=T)
# for (i in na.omit(roles)) {
#   Variable=i;
#   print(Variable)
#   indv2019[, Variable] <- dum(i, indv2019$category)
# }

# __________________________________________________________________________________________####  
#############################MERGING DATA####

indv2019<-merge(indv2019, dstr, by = "nconst", all.x = TRUE)# wrt<-merge(wrt, wstr, by = "nconst", all.x=T)
#indv2019<-merge(indv2019, sdstr, by = "nconst", all.x=T)# wrt<-merge(wrt, swstr, by = "nconst", all.x=T)
#names(indv2019)<-gsub(x = names(indv2019), pattern = "\\.x", replacement = "") 
#names(indv2019)<-gsub(x = names(indv2019), pattern = "\\.y", replacement = "_smp") 

# __________________________________________________________________________________________####  
#############################FOCUS####
#_focus on genres####
for(i in c(na.omit(xx))){if(i!="\\N"){indv2019[,i]<-str_count(indv2019$genres, i)}}

indv2019[,c("focus_genres")]<-  #,"focus_genres_ent"
  hhi(indv2019[,(length(indv2019)-27):length(indv2019)])
#t<-merge(twrt, tdir, all.x=T, all.y = T)

#_focus on roles####
temp2019<-aggregate(category ~ nconst, temp2019, toString)
temp2019<-temp2019[temp2019$nconst  %in%	indv2019$nconst, 1:2]
temp2019$actress<-str_count(temp2019$category,             	"actress"            	)
temp2019$actor<-str_count(temp2019$category,               	"actor"              	)
temp2019$director<-str_count(temp2019$category,            	"director"           	)
temp2019$producer<-str_count(temp2019$category,            	"producer"           	)
temp2019$writer<-str_count(temp2019$category,              	"writer"             	)
temp2019$cinematographer<-str_count(temp2019$category,     	"cinematographer"    	)
temp2019$composer<-str_count(temp2019$category,            	"composer"           	)
temp2019$production_designer<-str_count(temp2019$category, 	"production_designer"	)
temp2019$editor<-str_count(temp2019$category,              	"editor"             	)
indv2019[,c("focus_roles","focus_roles_ent")]<-hhi(temp2019[,3:11])

link2019  <-merge(x = link2019, y = gnrs[, c("tconst", "genres")], by = "tconst", all.x=TRUE)
for(i in c(na.omit(xx))){if(i!="\\N"){link2019[,i]<-str_count(link2019$genres, i)}}
indv2019  <- indv2019[,-which(names(indv2019)%in%xx)]
for(i in c(na.omit(xx))){if(i!="\\N"){indv2019  <- merge(x = indv2019, y = 
                     aggregate(as.formula(paste0(i, "~ nconst")), link2019, sum)
                     , by = "nconst", all.x=TRUE)}}

# __________________________________________________________________________________________####  
#############################TITLE LEVEL MODEL####
#A-TITLE ATTRIBUTES####

#!_average_confusion(!try below comment!)####
#rtng2019$average_confusion  <-aggregate(average_confusion ~ tconst, merge(link2019[,c("nconst","tconst")], indv2019, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$average_confusion #link2019$category %in% c("director")
rtng2019                    <-merge(x = rtng2019, y = aggregate(average_confusion ~ tconst, merge(link2019[,c("nconst","tconst")], indv2019, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link2019$category %in% c("director")
#_experience####
rtng2019                    <-merge(x = rtng2019, y = aggregate(experience ~ tconst, merge(link2019[,c("nconst","tconst")], indv2019, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link2019$category %in% c("director")
#_genre_confusion####
rtng2019                    <-merge(x = rtng2019, y = gnrs[, c("tconst", "gConf")], by = "tconst", all.x=TRUE)
#_so####
rtng2019                    <-merge(x = rtng2019, y = aggregate(orientation ~ tconst, merge(link2019[,c("nconst","tconst")], indv2019, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_role_consolidation####
rtng2019                    <-merge(x = rtng2019, y = aggregate(role_consolidation ~ tconst, merge(link2019[,c("nconst","tconst")], indv2019, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_awards####
rtng2019                    <-merge(x = rtng2019, y = aggregate(awards ~ tconst, merge(link2019[,c("nconst","tconst")],indv2019, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) 
#_quality####
rtng2019                    <-merge(x = rtng2019, y = aggregate(quality ~ tconst, link2019, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)
#_focus on roles####
rtng2019                    <-merge(x = rtng2019, y = aggregate(focus_genres ~ tconst, merge(link2019[,c("nconst","tconst")],indv2019, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_focus on genres####
rtng2019                    <-merge(x = rtng2019, y = aggregate(focus_roles ~ tconst, merge(link2019[,c("nconst","tconst")],indv2019, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_brokerage####
rtng2019                    <-merge(x = rtng2019, y = aggregate(brokerage ~ tconst, merge(link2019[,c("nconst","tconst")],indv2019, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)#rtng2019$brokerage_smp       <-na.omit(aggregate(brokerage_smp ~ tconst, merge(link2019[,c("nconst","tconst")],indv2019, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$brokerage_smp)
#_coreness####
rtng2019                    <-merge(x = rtng2019, y = aggregate(coreness ~ tconst, link2019, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)#setNames(aggregate(coreness ~ tconst, link2019, function(x) mean(as.numeric(x), na.rm=TRUE)), c("tconst", "team_coreness"))$team_coreness
#!newcomer# rtng2019$newcomer<-NA # for (tconst in merge(link2019[!,c("nconst","tconst")])){#   rtng2019[rtng2019$tconst=="rtng2019", "newcomer"]<-#   ifelse(aggregate(newcomer ~ tconst, merge(link2019[!,c("nconst","tconst")], indv2019, by="nconst", all.x=T)$newcomer, function(x) mean(as.numeric(x), na.rm=TRUE)) #          ==1#          , 1, NA)}####

colSums(is.na(link2019))
colSums(is.na(indv2019))
colSums(is.na(rtng2019))

# # __________________________________________________________________________________________####  
# #############################REFERENCES####

#https://stackoverflow.com/questions/48822710/merge-edges-by-attribute-group-in-igraph-r
#https://www.google.nl/search?biw=1239&bih=621&ei=Ot5ZW_X3L83TkwX176PwDw&q=merge+edges+based+on+vertex+igraph&oq=merge+edges+based+on+vertex+igraph&gs_l=psy-ab.3...39470.44521.0.44882.27.27.0.0.0.0.95.1777.27.27.0....0...1.1.64.psy-ab..0.12.818...0i7i30k1j0i7i10i30k1j0i13k1j0i8i7i30k1j0i30k1j0i5i30k1j0i8i30k1j0i8i13i30k1j35i39k1.0.X-cE_4yiOGo


# # __________________________________________________________________________________________####  
# #############################SIMPLIFIED NETWORK####
# #B-ADJACENCY MATRIX and edge-list####
# 
# net1<-get.edgelist(graph_from_adjacency_matrix(affcon(as.data.frame(na.omit(as.data.frame(director), cols=director))), mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL))
# net2<-merge(x = writer[ , c("tconst", "writer")], y = director[ , c("tconst", "director")], by = "tconst", fill=TRUE, all.x=TRUE, all.y=T, allow.cartesian=TRUE)
# net3<-merge(x = producer[ , c("tconst", "producer")], y = director[ , c("tconst", "director")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
# net4<-merge(x = dsgnr[ , c("tconst", "dsgnr")],   y =  director[ , c("tconst", "director")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
# net5<-merge(x = cnmtg[ , c("tconst", "cnmtg")],   y = director[ , c("tconst", "director")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
# net6<-merge(x = editor[ , c("tconst", "editor")], y = director[ , c("tconst", "director")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
# net7<-merge(x = cmpsr[ , c("tconst", "composer")], y = director[ , c("tconst", "director")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
# #net8<-merge(x = actor[ , c("tconst", "actor")], y = director[ , c("tconst", "director")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
# #net9<-merge(x = actress[ , c("tconst", "actress")], y = director[ , c("tconst", "director")], by = "tconst", all.x=TRUE, all.y=T, fill=TRUE)
# 
# for(i in 1:9){
#   object=get(paste0("net", i));
#   object<-get.edgelist(graph_from_adjacency_matrix(affcon(as.data.frame(na.omit(object))), mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL))
#   assign(paste0("net", i), object)
# }
# 
# edges <- na.omit(rbind(net1, setNames(net2, names(net1))
#                        , setNames(net3, names(net1))
#                        , setNames(net4, names(net1))
#                        , setNames(net5, names(net1))
#                        , setNames(net6, names(net1))
#                        , setNames(net7, names(net1))))
# snet<-graph_from_edgelist(edges, directed = FALSE)
# #snet<-simplify(snet)
# 
# #C-STRUCTURAL ATTRIBUTES####
# sdcr <- coreness(snet, mode = c("all"))#_coreness
# sdhs <- hub_score(snet)$vector#_hubs
# arpack_defaults$maxiter = 1000000000
# sdev <- as.data.frame(eigen_centrality(snet, options=arpack_defaults, directed=F))#_eigenvector
# sdev <- sdev[,1]
# sdcn <- 1 - constraint(snet, nodes = V(snet))#_brokerage
# sdpr <-page_rank(snet, algo = c("prpack", "arpack", "power"), v=V(snet),directed = FALSE)#_pagerank
# sdpr <-sdpr$vector
# #sdts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")2017
# 
# sdstr<- data.frame(nconst=names(sdcn),hubs_score=sdhs, page_rank=sdpr, brokerage=sdcn, coreness=sdcr, eigenvector_centrality=sdev)
# nrow(complete.cases(sdstr))
# 
# sapply(indv2019, function(y) sum(length(which(is.na(y)))))
# nrow(indv2019)
# sum(!is.na(sdstr$brokerage_smp))
# 
# 
#*manual method####
# #B-GRAPH FROM EDGE-LIST 
# tmp<-na.omit(aggregate(nconst ~ tconst, tmp, toString))# tmp$edges<-sapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], function(x) combn(x, 2, FUN = NULL, simplify = F))# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
# #tmp<-tmp %>% mutate_if(is.character, str_trim)
# tmp<-cbind(tmp, as.data.frame(str_split_fixed(tmp$nconst, ",", max(sapply(strsplit(tmp$nconst,'n' ), uniqueN))),stringsAsFactors = FALSE))
# is.na(tmp) <- tmp==''
# tmp<-tmp[!is.na(tmp["V2"]),]
# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
# for(i in 2:nrow(tmp))
# {print(i)
# edges<-rbind(edges,data.frame(rbindlist(combn(janitor::remove_empty(tmp[i,3:length(tmp)]), 2, FUN = NULL, simplify = F))))} # edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))# for(i in 2:nrow(tmp))# {print(i)# edges<-function(x)# {edges<-combn(janitor::remove_empty(x), 2, FUN = NULL, simplify = F)# return(data.frame(rbindlist(edges)))}# edge<-lapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], edges)# bind_rows(edges, .id = "column_label")#tmp[12,3:length(tmp)]
# 
# edges<-edges %>%
#   group_by(V1,V2) %>%    # for each combination of a and b
#   mutate(frequency = n()) %>%  # count times they appear
#   ungroup()
# 
# g<-graph_from_edgelist(as.matrix(edges[,-3]), directed = FALSE)
# g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

#*transpose method####
# net<-affcon(na.omit(as.data.frame(rbind(director
#                                         , setNames(writer, names(director))
#                                         , setNames(editor, names(director))
#                                         , setNames(dsgnr, names(director))
#                                         , setNames(producer, names(director))
#                                         , setNames(cnmtg, names(director))
#                                         , setNames(cmpsr, names(director))
#                                         # , setNames(actor, names(director))
#                                         # , setNames(actress, names(director))
#                                         )), cols=director)) #'"Error in validObject(.Object) : invalid class "dgTMatrix" object:all row indices (slot 'i') must be between 0 and nrow2017 in a TsparseMatrix"

#graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
#OR
# g <- link2019[,1:2] %*% t(link2019[,1:2]) %>%
#   graph_from_adjacency_matrix(mode = "undirected", diag = FALSE, weighted = TRUE)







# #manual construction of their network####
# tmp<-na.omit(aggregate(nconst ~ tconst, link2019, toString))# tmp$edges<-sapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], function(x) combn(x, 2, FUN = NULL, simplify = F))# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
# #tmp<-tmp %>% mutate_if(is.character, str_trim)
#   tmp<-cbind(tmp, as.data.frame(str_split_fixed(tmp$nconst, ",", max(sapply(strsplit(tmp$nconst,'n' ), uniqueN))),stringsAsFactors = FALSE))
# 
# is.na(tmp) <- tmp==''
# tmp<-tmp[!is.na(tmp["V2"]),]
# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
# for(i in 2:nrow(tmp))
# {print(i)
#   edges<-rbind(edges,data.frame(rbindlist(combn(janitor::remove_empty(tmp[i,3:length(tmp)]), 2, FUN = NULL, simplify = F))))} # edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))# for(i in 2:nrow(tmp))# {print(i)# edges<-function(x)# {edges<-combn(janitor::remove_empty(x), 2, FUN = NULL, simplify = F)# return(data.frame(rbindlist(edges)))}# edge<-lapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], edges)# bind_rows(edges, .id = "column_label")#tmp[12,3:length(tmp)]
# g<-graph.data.frame(edges, directed = FALSE, vertices = NULL)
# 
# unique(unique(link2019$nconst) %in% unique(names(V(g))))
# "nm0413541" %in% link2019[which(link2019$nconst %in% unique(V(g)$name))]$nconst
# "nm0413541" %in% V(g)$name
# "nm0413541" %in% link2019$nconst
# 
# "nm0000436" %in% link2019$nconst
# "nm0000436" %in% V(g)$name
# "nm0000436" %in% link2019[which(link2019$nconst %in% unique(V(g)$name))]$nconst
# 
# neighbors(g, "nm0413541", mode = "total")
# 
# 
# g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

