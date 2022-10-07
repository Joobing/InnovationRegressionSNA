# __________________________________________________________________________________________####  
#############################RESAMPLE2011 ####
if(FALSE) {"
1- 2011 (number of) titles
2- relevant (present and past) affiliations to 2011
  # Crew of titles of major-STUDIOs in 2011
  # Titles that share CREW with titles of major-STUDIOs in 2011
  # Crew of aLL titles that share CREW with titles of major-STUDIOs in 2011
  # Up to 2011
3- THREE YEAR window for 2011
4- dropping animation and documentary
2008, 2009, 2010, 2011, 2001, 2002
  "}

rtng2011<-rtng[rtng$startYear == 2011,]
#rtng2011<-rtng2011[complete.cases(rtng2011[,1]),]
rtng2011<-unique(rtng2011)
rtng2011=rtng2011[!sum(is.na(rtng2011))==ncol(rtng2011),]


n2011<-nrow(rtng2011)

temp2011<-unique(prnc[prnc$tconst  %in%	rtng2011$tconst, "nconst"])#
temp2011<-prnc[prnc$nconst  %in%	temp2011$nconst, "tconst"]#
temp2011<-unique(prnc[prnc$tconst  %in%	temp2011$tconst,])#
temp2011<-temp2011[temp2011$tconst %in% bscs[bscs$startYear <= 2011,]$tconst,]# #temp2011<-merge(temp2011,bscs[,c("tconst","startYear")], by="tconst", all.x = T)# temp2011<-merge(temp2011, bscs[,c("tconst", "startYear")], by="tconst", all.x = TRUE)


temp2011<-temp2011[!(temp2011$tconst %in% animentry),]
rtng2011<-rtng2011[!(rtng2011$tconst %in% animentry),]


w2011 <- c(2011-4, 2011-3, 2011-2 ,2011-1, 2011)#library(plyr)#,2011-3,2011-4,2011-5,2011-6
w2011 <-unique(bscs[bscs$startYear %in% w2011,"tconst"])


# __________________________________________________________________________________________####  
#############################NETWORK of COLLABORATION####
#A-SEPARATING PROFESSIONALS BY ROLE####
tmp <- temp2011[(temp2011$tconst %in% w2011$tconst) & (temp2011$nconst %in% temp2011[temp2011$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"),]$nconst) , ]
tmp <- tmp[complete.cases(tmp[,c("tconst","nconst")]),]

#B-GRAPH FROM EDGE-LIST#### 
g<-          graph_from_data_frame(tmp, directed=FALSE)
g<-          simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-  V(g)$name %in% unique(tmp$nconst)
PROJECTION<- bipartite.projection(g) #two-mode
tnet<-       PROJECTION[[1]] #one-mode
g<-          PROJECTION[[2]] #one-mode
g<-          set.edge.attribute(g, "weight", E(g), 1)
cmpnt2011 <- no.clusters(g);           # Number of components
larg2011 <- sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1]; # Size of largest component
rlarg2011 <- (sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1])/vcount(g); # Relative size of largest component

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

#cast and crew of titles from major-studios in 2011 
link2011<-temp2011[temp2011$tconst %in% rtng2011$tconst,] #as_tibble(temp2011[temp2011$tconst %in% rtng2011$tconst,1:3])

g<- graph_from_data_frame(link2011[,1:2], directed=FALSE)
g<-simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-    V(g)$name %in% unique(link2011$nconst)
PROJECTION<-bipartite.projection(g) #two-mode
tnet<-PROJECTION[[1]] #one-mode
g<-PROJECTION[[2]]#one-mode
g <- set.edge.attribute(g, "weight", E(g), 1)


#A-CREW with titles from major-studios in 2011#### 
#!!!when individual performs multiple roles in a movie, that movie is entering multiple times in the calculations for that individual!!!####
indv2011 <- temp2011[temp2011$nconst %in% link2011[link2011$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst,c("tconst", "nconst","category")]
  #indv2011 <- indv2011[indv2011$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,] #& temp2011$category=='director' #[,-3] 
  #_adding genre, years, gconf, and metascores####
  indv2011<- merge(x = indv2011, y = bscs[ , c("tconst", "startYear", "genres")], all.x = T)#, by = "tconst", all.x=TRUE, fill=F
  #_adding gconf####
  indv2011<- merge(x = indv2011, y = gnrs[gnrs$tconst %in% w2011$tconst, c("tconst", "gConf")], all.x = TRUE)
  #_adding metascores####
  indv2011<- merge(x = indv2011, y = rtng2011[ , c("tconst", "metascore")], all.x = TRUE)
#k<-length(indv2011) # indv2011 <-indv2011[na.omit(indv2011[,1:as.numeric(k)])]
  indv2011 <- unique(indv2011)
  #!NO GENRE!####
  mssgnrs2011<-indv2011[indv2011$genres=="\\N",]
  mssgnrs2011<-rbind(mssgnrs2011, indv2011[is.na(indv2011$genres)==TRUE,])#indv2011<- indv2011[indv2011$genres!="\\N",]
  #indv2011<-link2011 %>% mutate_at(c('genres'), replace_na, '\\N')
  


length(unique(link2011[link2011$category %in%  c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst))
length(unique(indv2011$nconst))

#B-INDIVIDUAL ATTRIBUTES #### 
indv2011<-merge(aggregate(startYear ~ nconst, indv2011, paste, na.action=NULL),merge(
  aggregate(metascore ~ nconst, indv2011, function(x) mean(as.numeric(x), na.rm=TRUE), na.action=NULL),merge(#_average METASCORE ####
#!!!!!if one genre is NA gconf becomes NA  ######################                                                                                                             
    aggregate(gConf ~ nconst, indv2011, toString, na.action=na.omit),
    aggregate(genres ~ nconst, indv2011, toString, na.action=NULL)
    ))) #indv2011<-indv2011[,c(1,4:10,2,3)] for reordering columns
setnames(indv2011, "startYear", "year")
indv2011$startYear<-as.numeric(sapply(indv2011$year, min))#_startYear####
# sy2011<-indv2011[,c("nconst","sy")]
# indv2011<-indv2011[,!(names(indv2011)%in%c("sy"))]

indv2011$year <- vapply(indv2011$year, paste, collapse = ", ", character(1L)) # wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
indv2011$tenure<-2011-indv2011$startYear#_tenure####
indv2011$experience <- sapply(indv2011$year, function(x) str_count(x,"\\,"))#_experience####
indv2011$newcomer <- as.numeric(indv2011$experience == 0)#_newcomer####
indv2011$average_confusion<-sapply(strsplit(as.character(indv2011$gConf), ",", fixed=T), function(x) mean(as.numeric(x), na.rm=TRUE))#_average confusion####
indv2011$orientation <-sapply(strsplit(indv2011$genres,"\\, |\\,| " ), uniqueN)/(1+indv2011$experience)#!_so####
indv2011$year <- 2011 #_focal year####


#_role consolidation####
indv2011<-merge(indv2011,aggregate(n ~ nconst, dplyr::add_count(link2011, tconst, nconst), function(x) mean(as.numeric(x), na.rm=TRUE)), all.x = T)
setnames(indv2011, "n", "role_consolidation")


#_awards quality####
  #for all collaborators with titles in 2011 
  link2011<-merge(link2011,awrd,by.x='nconst', by.y='nm', all.x=TRUE) #nrow(na.omit(temp2011[,-4]))==nrow(temp2011) #nrow(na.omit(awrd))==nrow(awrd)
#!awards for missing nm in awrd replaced with zero####
  link2011<-link2011 %>% mutate_at(c('2011', '2010','2009','2008','2007','1995'), replace_na, 0)
  link2011[,'awards']<-link2011$`2011` #names(indv2011$`2011`)<-"awards"
  link2011[link2011$nconst=="nm7016360",c('nconst','awards')]
  link2011[is.na(link2011$awards)==TRUE,'awards']
  link2011[,'quality']<-link2011$`2010`+link2011$`2009`+link2011$`2008`+link2011$`2007`
indv2011<-unique(merge(indv2011,link2011[,c("nconst","awards","quality")],by='nconst', all.x=TRUE))




indv2011<-merge(indv2011,aggregate(budget ~ nconst, merge(link2011[,c("nconst","tconst")],rtng2011[,c("tconst","budget")], by="tconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst", all.x=T) #_budget####
indv2011<-merge(indv2011,aggregate(sequel ~ nconst, merge(link2011[,c("nconst","tconst")],rtng2011[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel####

#############################C-ALTER ATTRIBUTES####
#_alter quality####
tmp<-data.frame()
test<-data.frame()
#ind <-   which(sapply(link2011, is.numeric))
#for(j in ind){
  #set(link2011, i = which(is.na(link2011[[j]])), j = j, value = 0)}
for(nconst in names(V(delete.vertices(g, which(degree(g)==0))))){
  c<-neighbors(g, nconst, mode = "total")
  tmp<-rbind(tmp,data.frame(nconst, 
                            setNames(
                              aggregate(quality ~ tconst, 
                              link2011[link2011$nconst %in% str_trim(names(c)),], function(x) mean(as.numeric(x), na.rm=TRUE))
                              , c("tconst", "team_quality"))
                            )
             )} # V(tmp1)$name[c] 
indv2011<-merge(indv2011,aggregate(team_quality ~ nconst, tmp[tmp$tconst %in% rtng2011$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) 

#_team coreness####
link2011<-merge(x=link2011,y=dstr[,c("nconst","coreness")],by="nconst",all.x = TRUE)#[0:2,]
tmp<-data.frame()
for(nconst in link2011$nconst){
  c<-neighbors(g, nconst, mode = "total")
    if(nrow(link2011[link2011$nconst %in% names(c) & !is.na(link2011$coreness),])>0){
    tmp<-rbind(tmp,data.frame(nconst, na.omit(aggregate(coreness ~ tconst, link2011[link2011$nconst %in% str_trim(names(c)) & !is.na(link2011$coreness),], function(x) mean(as.numeric(x), na.rm=TRUE)))))}} # V(tmp1)$name[c] 
indv2011<-merge(indv2011,aggregate(coreness ~ nconst, tmp[tmp$tconst %in% rtng2011$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) # unique(link2011[link2011$nconst %in% names(c) & !link2011$category %in% c("actor","actress" ),]$category)# str(link2011[link2011$nconst %in% names(c) & !link2011$category %in% c("actor","actress" ),])
names(indv2011)<-gsub(x = names(indv2011), pattern = "coreness", replacement = "team_coreness") 

# #_role dummies####
# roles<-unique(temp2011$category)
# roles<-roles[!(roles %in% c("archive_footage", "self"))]
# indv2011<-merge(indv2011,na.omit(aggregate(category ~ nconst, temp2011[temp2011$tconst %in% rtng2011$tconst], toString)), by="nconst", all.x=T)
# for (i in na.omit(roles)) {
#   Variable=i;
#   print(Variable)
#   indv2011[, Variable] <- dum(i, indv2011$category)
# }

# __________________________________________________________________________________________####  
#############################MERGING DATA####

indv2011<-merge(indv2011, dstr, by = "nconst", all.x = TRUE)# wrt<-merge(wrt, wstr, by = "nconst", all.x=T)
#indv2011<-merge(indv2011, sdstr, by = "nconst", all.x=T)# wrt<-merge(wrt, swstr, by = "nconst", all.x=T)
#names(indv2011)<-gsub(x = names(indv2011), pattern = "\\.x", replacement = "") 
#names(indv2011)<-gsub(x = names(indv2011), pattern = "\\.y", replacement = "_smp") 

# __________________________________________________________________________________________####  
#############################FOCUS####
#_focus on genres####
for(i in c(na.omit(xx))){if(i!="\\N"){indv2011[,i]<-str_count(indv2011$genres, i)}}

indv2011[,c("focus_genres")]<-  #,"focus_genres_ent"
  hhi(indv2011[,(length(indv2011)-27):length(indv2011)])
#t<-merge(twrt, tdir, all.x=T, all.y = T)

#_focus on roles####
temp2011<-aggregate(category ~ nconst, temp2011, toString)
temp2011<-temp2011[temp2011$nconst  %in%	indv2011$nconst, 1:2]
temp2011$actress<-str_count(temp2011$category,             	"actress"            	)
temp2011$actor<-str_count(temp2011$category,               	"actor"              	)
temp2011$director<-str_count(temp2011$category,            	"director"           	)
temp2011$producer<-str_count(temp2011$category,            	"producer"           	)
temp2011$writer<-str_count(temp2011$category,              	"writer"             	)
temp2011$cinematographer<-str_count(temp2011$category,     	"cinematographer"    	)
temp2011$composer<-str_count(temp2011$category,            	"composer"           	)
temp2011$production_designer<-str_count(temp2011$category, 	"production_designer"	)
temp2011$editor<-str_count(temp2011$category,              	"editor"             	)
indv2011[,c("focus_roles","focus_roles_ent")]<-hhi(temp2011[,3:11])

link2011  <-merge(x = link2011, y = gnrs[, c("tconst", "genres")], by = "tconst", all.x=TRUE)
for(i in c(na.omit(xx))){if(i!="\\N"){link2011[,i]<-str_count(link2011$genres, i)}}
indv2011  <- indv2011[,-which(names(indv2011)%in%xx)]
for(i in c(na.omit(xx))){if(i!="\\N"){indv2011  <- merge(x = indv2011, y = 
                     aggregate(as.formula(paste0(i, "~ nconst")), link2011, sum)
                     , by = "nconst", all.x=TRUE)}}

# __________________________________________________________________________________________####  
#############################TITLE LEVEL MODEL####
#A-TITLE ATTRIBUTES####

#!_average_confusion(!try below comment!)####
#rtng2011$average_confusion  <-aggregate(average_confusion ~ tconst, merge(link2011[,c("nconst","tconst")], indv2011, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$average_confusion #link2011$category %in% c("director")
rtng2011                    <-merge(x = rtng2011, y = aggregate(average_confusion ~ tconst, merge(link2011[,c("nconst","tconst")], indv2011, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link2011$category %in% c("director")
#_experience####
rtng2011                    <-merge(x = rtng2011, y = aggregate(experience ~ tconst, merge(link2011[,c("nconst","tconst")], indv2011, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link2011$category %in% c("director")
#_genre_confusion####
rtng2011                    <-merge(x = rtng2011, y = gnrs[, c("tconst", "gConf")], by = "tconst", all.x=TRUE)
#_so####
rtng2011                    <-merge(x = rtng2011, y = aggregate(orientation ~ tconst, merge(link2011[,c("nconst","tconst")], indv2011, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_role_consolidation####
rtng2011                    <-merge(x = rtng2011, y = aggregate(role_consolidation ~ tconst, merge(link2011[,c("nconst","tconst")], indv2011, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_awards####
rtng2011                    <-merge(x = rtng2011, y = aggregate(awards ~ tconst, merge(link2011[,c("nconst","tconst")],indv2011, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) 
#_quality####
rtng2011                    <-merge(x = rtng2011, y = aggregate(quality ~ tconst, link2011, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)
#_focus on roles####
rtng2011                    <-merge(x = rtng2011, y = aggregate(focus_genres ~ tconst, merge(link2011[,c("nconst","tconst")],indv2011, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_focus on genres####
rtng2011                    <-merge(x = rtng2011, y = aggregate(focus_roles ~ tconst, merge(link2011[,c("nconst","tconst")],indv2011, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_brokerage####
rtng2011                    <-merge(x = rtng2011, y = aggregate(brokerage ~ tconst, merge(link2011[,c("nconst","tconst")],indv2011, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)#rtng2011$brokerage_smp       <-na.omit(aggregate(brokerage_smp ~ tconst, merge(link2011[,c("nconst","tconst")],indv2011, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$brokerage_smp)
#_coreness####
rtng2011                    <-merge(x = rtng2011, y = aggregate(coreness ~ tconst, link2011, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)#setNames(aggregate(coreness ~ tconst, link2011, function(x) mean(as.numeric(x), na.rm=TRUE)), c("tconst", "team_coreness"))$team_coreness
#!newcomer# rtng2011$newcomer<-NA # for (tconst in merge(link2011[!,c("nconst","tconst")])){#   rtng2011[rtng2011$tconst=="rtng2011", "newcomer"]<-#   ifelse(aggregate(newcomer ~ tconst, merge(link2011[!,c("nconst","tconst")], indv2011, by="nconst", all.x=T)$newcomer, function(x) mean(as.numeric(x), na.rm=TRUE)) #          ==1#          , 1, NA)}####

colSums(is.na(link2011))
colSums(is.na(indv2011))
colSums(is.na(rtng2011))

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
# #sdts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")2009
# 
# sdstr<- data.frame(nconst=names(sdcn),hubs_score=sdhs, page_rank=sdpr, brokerage=sdcn, coreness=sdcr, eigenvector_centrality=sdev)
# nrow(complete.cases(sdstr))
# 
# sapply(indv2011, function(y) sum(length(which(is.na(y)))))
# nrow(indv2011)
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
#                                         )), cols=director)) #'"Error in validObject(.Object) : invalid class "dgTMatrix" object:all row indices (slot 'i') must be between 0 and nrow2009 in a TsparseMatrix"

#graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
#OR
# g <- link2011[,1:2] %*% t(link2011[,1:2]) %>%
#   graph_from_adjacency_matrix(mode = "undirected", diag = FALSE, weighted = TRUE)







# #manual construction of their network####
# tmp<-na.omit(aggregate(nconst ~ tconst, link2011, toString))# tmp$edges<-sapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], function(x) combn(x, 2, FUN = NULL, simplify = F))# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
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
# unique(unique(link2011$nconst) %in% unique(names(V(g))))
# "nm0413541" %in% link2011[which(link2011$nconst %in% unique(V(g)$name))]$nconst
# "nm0413541" %in% V(g)$name
# "nm0413541" %in% link2011$nconst
# 
# "nm0000436" %in% link2011$nconst
# "nm0000436" %in% V(g)$name
# "nm0000436" %in% link2011[which(link2011$nconst %in% unique(V(g)$name))]$nconst
# 
# neighbors(g, "nm0413541", mode = "total")
# 
# 
# g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

