# __________________________________________________________________________________________####  
#############################RESAMPLE2000 ####
if(FALSE) {"
1- 2000 (number of) titles
2- relevant (present and past) affiliations to 2000
  # Crew of titles of major-STUDIOs in 2000
  # Titles that share CREW with titles of major-STUDIOs in 2000
  # Crew of aLL titles that share CREW with titles of major-STUDIOs in 2000
  # Up to 2000
3- THREE YEAR window for 2000
4- dropping animation and documentary
1997, 1998, 1999, 2000, 2001, 2002
  "}

rtng2000<-rtng[rtng$startYear == 2000,]
#rtng2000<-rtng2000[complete.cases(rtng2000[,1]),]
rtng2000<-unique(rtng2000)
rtng2000=rtng2000[!sum(is.na(rtng2000))==ncol(rtng2000),]


n2000<-nrow(rtng2000)

temp2000<-unique(prnc[prnc$tconst  %in%	rtng2000$tconst, "nconst"])#
temp2000<-prnc[prnc$nconst  %in%	temp2000$nconst, "tconst"]#
temp2000<-unique(prnc[prnc$tconst  %in%	temp2000$tconst,])#
temp2000<-temp2000[temp2000$tconst %in% bscs[bscs$startYear <= 2000,]$tconst,]# #temp2000<-merge(temp2000,bscs[,c("tconst","startYear")], by="tconst", all.x = T)# temp2000<-merge(temp2000, bscs[,c("tconst", "startYear")], by="tconst", all.x = TRUE)


temp2000<-temp2000[!(temp2000$tconst %in% animentry),]
rtng2000<-rtng2000[!(rtng2000$tconst %in% animentry),]


w2000 <- c(2000-4, 2000-3, 2000-2 ,2000-1, 2000)#library(plyr)#,2000-3,2000-4,2000-5,2000-6
w2000 <-unique(bscs[bscs$startYear %in% w2000,"tconst"])


# __________________________________________________________________________________________####  
#############################NETWORK of COLLABORATION####
#A-SEPARATING PROFESSIONALS BY ROLE####
tmp <- temp2000[(temp2000$tconst %in% w2000$tconst) & (temp2000$nconst %in% temp2000[temp2000$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"),]$nconst) , ]
tmp <- tmp[complete.cases(tmp[,c("tconst","nconst")]),]

#B-GRAPH FROM EDGE-LIST#### 
g<-          graph_from_data_frame(tmp, directed=FALSE)
g<-          simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-  V(g)$name %in% unique(tmp$nconst)
PROJECTION<- bipartite.projection(g) #two-mode
tnet<-       PROJECTION[[1]] #one-mode
g<-          PROJECTION[[2]] #one-mode
g<-          set.edge.attribute(g, "weight", E(g), 1)
cmpnt2000 <- no.clusters(g);           # Number of components
larg2000 <- sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1]; # Size of largest component
rlarg2000 <- (sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1])/vcount(g); # Relative size of largest component

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

#cast and crew of titles from major-studios in 2000 
link2000<-temp2000[temp2000$tconst %in% rtng2000$tconst,] #as_tibble(temp2000[temp2000$tconst %in% rtng2000$tconst,1:3])

g<- graph_from_data_frame(link2000[,1:2], directed=FALSE)
g<-simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-    V(g)$name %in% unique(link2000$nconst)
PROJECTION<-bipartite.projection(g) #two-mode
tnet<-PROJECTION[[1]] #one-mode
g<-PROJECTION[[2]]#one-mode
g <- set.edge.attribute(g, "weight", E(g), 1)


#A-CREW with titles from major-studios in 2000#### 
#!!!when individual performs multiple roles in a movie, that movie is entering multiple times in the calculations for that individual!!!####
indv2000 <- temp2000[temp2000$nconst %in% link2000[link2000$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst,c("tconst", "nconst","category")]
  #indv2000 <- indv2000[indv2000$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,] #& temp2000$category=='director' #[,-3] 
  #_adding genre, years, gconf, and metascores####
  indv2000<- merge(x = indv2000, y = bscs[ , c("tconst", "startYear", "genres")], all.x = T)#, by = "tconst", all.x=TRUE, fill=F
  #_adding gconf####
  indv2000<- merge(x = indv2000, y = gnrs[gnrs$tconst %in% w2000$tconst, c("tconst", "gConf")], all.x = TRUE)
  #_adding metascores####
  indv2000<- merge(x = indv2000, y = rtng2000[ , c("tconst", "metascore")], all.x = TRUE)
#k<-length(indv2000) # indv2000 <-indv2000[na.omit(indv2000[,1:as.numeric(k)])]
  indv2000 <- unique(indv2000)
  #!NO GENRE!####
  mssgnrs2000<-indv2000[indv2000$genres=="\\N",]
  mssgnrs2000<-rbind(mssgnrs2000, indv2000[is.na(indv2000$genres)==TRUE,])#indv2000<- indv2000[indv2000$genres!="\\N",]
  #indv2000<-link2000 %>% mutate_at(c('genres'), replace_na, '\\N')
  


length(unique(link2000[link2000$category %in%  c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst))
length(unique(indv2000$nconst))

#B-INDIVIDUAL ATTRIBUTES #### 
indv2000<-merge(aggregate(startYear ~ nconst, indv2000, paste, na.action=NULL),merge(
  aggregate(metascore ~ nconst, indv2000, function(x) mean(as.numeric(x), na.rm=TRUE), na.action=NULL),merge(#_average METASCORE ####
#!!!!!if one genre is NA gconf becomes NA  ######################                                                                                                             
    aggregate(gConf ~ nconst, indv2000, toString, na.action=na.omit),
    aggregate(genres ~ nconst, indv2000, toString, na.action=NULL)
    ))) #indv2000<-indv2000[,c(1,4:10,2,3)] for reordering columns
setnames(indv2000, "startYear", "year")
indv2000$startYear<-as.numeric(sapply(indv2000$year, min))#_startYear####
# sy2000<-indv2000[,c("nconst","sy")]
# indv2000<-indv2000[,!(names(indv2000)%in%c("sy"))]

indv2000$year <- vapply(indv2000$year, paste, collapse = ", ", character(1L)) # wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
indv2000$tenure<-2000-indv2000$startYear#_tenure####
indv2000$experience <- sapply(indv2000$year, function(x) str_count(x,"\\,"))#_experience####
indv2000$newcomer <- as.numeric(indv2000$experience == 0)#_newcomer####
indv2000$average_confusion<-sapply(strsplit(as.character(indv2000$gConf), ",", fixed=T), function(x) mean(as.numeric(x), na.rm=TRUE))#_average confusion####
indv2000$orientation <-sapply(strsplit(indv2000$genres,"\\, |\\,| " ), uniqueN)/(1+indv2000$experience)#!_so####
indv2000$year <- 2000 #_focal year####


#_role consolidation####
indv2000<-merge(indv2000,aggregate(n ~ nconst, dplyr::add_count(link2000, tconst, nconst), function(x) mean(as.numeric(x), na.rm=TRUE)), all.x = T)
setnames(indv2000, "n", "role_consolidation")


#_awards quality####
  #for all collaborators with titles in 2000 
  link2000<-merge(link2000,awrd,by.x='nconst', by.y='nm', all.x=TRUE) #nrow(na.omit(temp2000[,-4]))==nrow(temp2000) #nrow(na.omit(awrd))==nrow(awrd)
#!awards for missing nm in awrd replaced with zero####
  link2000<-link2000 %>% mutate_at(c('2000', '1999','1998','1997','1996','1995'), replace_na, 0)
  link2000[,'awards']<-link2000$`2000` #names(indv2000$`2000`)<-"awards"
  link2000[link2000$nconst=="nm7016360",c('nconst','awards')]
  link2000[is.na(link2000$awards)==TRUE,'awards']
  link2000[,'quality']<-link2000$`1999`+link2000$`1998`+link2000$`1997`+link2000$`1996`
indv2000<-unique(merge(indv2000,link2000[,c("nconst","awards","quality")],by='nconst', all.x=TRUE))




indv2000<-merge(indv2000,aggregate(budget ~ nconst, merge(link2000[,c("nconst","tconst")],rtng2000[,c("tconst","budget")], by="tconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst", all.x=T) #_budget####
indv2000<-merge(indv2000,aggregate(sequel ~ nconst, merge(link2000[,c("nconst","tconst")],rtng2000[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel####

#############################C-ALTER ATTRIBUTES####
#_alter quality####
tmp<-data.frame()
test<-data.frame()
#ind <-   which(sapply(link2000, is.numeric))
#for(j in ind){
  #set(link2000, i = which(is.na(link2000[[j]])), j = j, value = 0)}
for(nconst in names(V(delete.vertices(g, which(degree(g)==0))))){
  c<-neighbors(g, nconst, mode = "total")
  tmp<-rbind(tmp,data.frame(nconst, 
                            setNames(
                              aggregate(quality ~ tconst, 
                              link2000[link2000$nconst %in% str_trim(names(c)),], function(x) mean(as.numeric(x), na.rm=TRUE))
                              , c("tconst", "team_quality"))
                            )
             )} # V(tmp1)$name[c] 
indv2000<-merge(indv2000,aggregate(team_quality ~ nconst, tmp[tmp$tconst %in% rtng2000$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) 

#_team coreness####
link2000<-merge(x=link2000,y=dstr[,c("nconst","coreness")],by="nconst",all.x = TRUE)#[0:2,]
tmp<-data.frame()
for(nconst in link2000$nconst){
  c<-neighbors(g, nconst, mode = "total")
    if(nrow(link2000[link2000$nconst %in% names(c) & !is.na(link2000$coreness),])>0){
    tmp<-rbind(tmp,data.frame(nconst, na.omit(aggregate(coreness ~ tconst, link2000[link2000$nconst %in% str_trim(names(c)) & !is.na(link2000$coreness),], function(x) mean(as.numeric(x), na.rm=TRUE)))))}} # V(tmp1)$name[c] 
indv2000<-merge(indv2000,aggregate(coreness ~ nconst, tmp[tmp$tconst %in% rtng2000$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) # unique(link2000[link2000$nconst %in% names(c) & !link2000$category %in% c("actor","actress" ),]$category)# str(link2000[link2000$nconst %in% names(c) & !link2000$category %in% c("actor","actress" ),])
names(indv2000)<-gsub(x = names(indv2000), pattern = "coreness", replacement = "team_coreness") 

# #_role dummies####
# roles<-unique(temp2000$category)
# roles<-roles[!(roles %in% c("archive_footage", "self"))]
# indv2000<-merge(indv2000,na.omit(aggregate(category ~ nconst, temp2000[temp2000$tconst %in% rtng2000$tconst], toString)), by="nconst", all.x=T)
# for (i in na.omit(roles)) {
#   Variable=i;
#   print(Variable)
#   indv2000[, Variable] <- dum(i, indv2000$category)
# }

# __________________________________________________________________________________________####  
#############################MERGING DATA####

indv2000<-merge(indv2000, dstr, by = "nconst", all.x = TRUE)# wrt<-merge(wrt, wstr, by = "nconst", all.x=T)
#indv2000<-merge(indv2000, sdstr, by = "nconst", all.x=T)# wrt<-merge(wrt, swstr, by = "nconst", all.x=T)
#names(indv2000)<-gsub(x = names(indv2000), pattern = "\\.x", replacement = "") 
#names(indv2000)<-gsub(x = names(indv2000), pattern = "\\.y", replacement = "_smp") 

# __________________________________________________________________________________________####  
#############################FOCUS####
#_focus on genres####
for(i in c(na.omit(xx))){if(i!="\\N"){indv2000[,i]<-str_count(indv2000$genres, i)}}

indv2000[,c("focus_genres")]<-  #,"focus_genres_ent"
  hhi(indv2000[,(length(indv2000)-27):length(indv2000)])
#t<-merge(twrt, tdir, all.x=T, all.y = T)

#_focus on roles####
temp2000<-aggregate(category ~ nconst, temp2000, toString)
temp2000<-temp2000[temp2000$nconst  %in%	indv2000$nconst, 1:2]
temp2000$actress<-str_count(temp2000$category,             	"actress"            	)
temp2000$actor<-str_count(temp2000$category,               	"actor"              	)
temp2000$director<-str_count(temp2000$category,            	"director"           	)
temp2000$producer<-str_count(temp2000$category,            	"producer"           	)
temp2000$writer<-str_count(temp2000$category,              	"writer"             	)
temp2000$cinematographer<-str_count(temp2000$category,     	"cinematographer"    	)
temp2000$composer<-str_count(temp2000$category,            	"composer"           	)
temp2000$production_designer<-str_count(temp2000$category, 	"production_designer"	)
temp2000$editor<-str_count(temp2000$category,              	"editor"             	)
indv2000[,c("focus_roles","focus_roles_ent")]<-hhi(temp2000[,3:11])

link2000  <-merge(x = link2000, y = gnrs[, c("tconst", "genres")], by = "tconst", all.x=TRUE)
for(i in c(na.omit(xx))){if(i!="\\N"){link2000[,i]<-str_count(link2000$genres, i)}}
indv2000  <- indv2000[,-which(names(indv2000)%in%xx)]
for(i in c(na.omit(xx))){if(i!="\\N"){indv2000  <- merge(x = indv2000, y = 
                     aggregate(as.formula(paste0(i, "~ nconst")), link2000, sum)
                     , by = "nconst", all.x=TRUE)}}

# __________________________________________________________________________________________####  
#############################TITLE LEVEL MODEL####
#A-TITLE ATTRIBUTES####

#!_average_confusion(!try below comment!)####
#rtng2000$average_confusion  <-aggregate(average_confusion ~ tconst, merge(link2000[,c("nconst","tconst")], indv2000, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$average_confusion #link2000$category %in% c("director")
rtng2000                    <-merge(x = rtng2000, y = aggregate(average_confusion ~ tconst, merge(link2000[,c("nconst","tconst")], indv2000, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link2000$category %in% c("director")
#_experience####
rtng2000                    <-merge(x = rtng2000, y = aggregate(experience ~ tconst, merge(link2000[,c("nconst","tconst")], indv2000, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link2000$category %in% c("director")
#_genre_confusion####
rtng2000                    <-merge(x = rtng2000, y = gnrs[, c("tconst", "gConf")], by = "tconst", all.x=TRUE)
#_so####
rtng2000                    <-merge(x = rtng2000, y = aggregate(orientation ~ tconst, merge(link2000[,c("nconst","tconst")], indv2000, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_role_consolidation####
rtng2000                    <-merge(x = rtng2000, y = aggregate(role_consolidation ~ tconst, merge(link2000[,c("nconst","tconst")], indv2000, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_awards####
rtng2000                    <-merge(x = rtng2000, y = aggregate(awards ~ tconst, merge(link2000[,c("nconst","tconst")],indv2000, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) 
#_quality####
rtng2000                    <-merge(x = rtng2000, y = aggregate(quality ~ tconst, link2000, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)
#_focus on roles####
rtng2000                    <-merge(x = rtng2000, y = aggregate(focus_genres ~ tconst, merge(link2000[,c("nconst","tconst")],indv2000, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_focus on genres####
rtng2000                    <-merge(x = rtng2000, y = aggregate(focus_roles ~ tconst, merge(link2000[,c("nconst","tconst")],indv2000, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_brokerage####
rtng2000                    <-merge(x = rtng2000, y = aggregate(brokerage ~ tconst, merge(link2000[,c("nconst","tconst")],indv2000, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)#rtng2000$brokerage_smp       <-na.omit(aggregate(brokerage_smp ~ tconst, merge(link2000[,c("nconst","tconst")],indv2000, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$brokerage_smp)
#_coreness####
rtng2000                    <-merge(x = rtng2000, y = aggregate(coreness ~ tconst, link2000, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)#setNames(aggregate(coreness ~ tconst, link2000, function(x) mean(as.numeric(x), na.rm=TRUE)), c("tconst", "team_coreness"))$team_coreness
#!newcomer# rtng2000$newcomer<-NA # for (tconst in merge(link2000[!,c("nconst","tconst")])){#   rtng2000[rtng2000$tconst=="rtng2000", "newcomer"]<-#   ifelse(aggregate(newcomer ~ tconst, merge(link2000[!,c("nconst","tconst")], indv2000, by="nconst", all.x=T)$newcomer, function(x) mean(as.numeric(x), na.rm=TRUE)) #          ==1#          , 1, NA)}####

colSums(is.na(link2000))
colSums(is.na(indv2000))
colSums(is.na(rtng2000))

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
# #sdts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")1998
# 
# sdstr<- data.frame(nconst=names(sdcn),hubs_score=sdhs, page_rank=sdpr, brokerage=sdcn, coreness=sdcr, eigenvector_centrality=sdev)
# nrow(complete.cases(sdstr))
# 
# sapply(indv2000, function(y) sum(length(which(is.na(y)))))
# nrow(indv2000)
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
#                                         )), cols=director)) #'"Error in validObject(.Object) : invalid class "dgTMatrix" object:all row indices (slot 'i') must be between 0 and nrow1998 in a TsparseMatrix"

#graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
#OR
# g <- link2000[,1:2] %*% t(link2000[,1:2]) %>%
#   graph_from_adjacency_matrix(mode = "undirected", diag = FALSE, weighted = TRUE)







# #manual construction of their network####
# tmp<-na.omit(aggregate(nconst ~ tconst, link2000, toString))# tmp$edges<-sapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], function(x) combn(x, 2, FUN = NULL, simplify = F))# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
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
# unique(unique(link2000$nconst) %in% unique(names(V(g))))
# "nm0413541" %in% link2000[which(link2000$nconst %in% unique(V(g)$name))]$nconst
# "nm0413541" %in% V(g)$name
# "nm0413541" %in% link2000$nconst
# 
# "nm0000436" %in% link2000$nconst
# "nm0000436" %in% V(g)$name
# "nm0000436" %in% link2000[which(link2000$nconst %in% unique(V(g)$name))]$nconst
# 
# neighbors(g, "nm0413541", mode = "total")
# 
# 
# g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

