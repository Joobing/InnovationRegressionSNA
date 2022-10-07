# __________________________________________________________________________________________####  
#############################RESAMPLE1977 ####
if(FALSE) {"
1- 1977 (number of) titles
2- relevant (present and past) affiliations to 1977
  # Crew of titles of major-STUDIOs in 1977
  # Titles that share CREW with titles of major-STUDIOs in 1977
  # Crew of aLL titles that share CREW with titles of major-STUDIOs in 1977
  # Up to 1977
3- THREE YEAR window for 1977
4- dropping animation and documentary
1974, 1975, 1976, 1977, 2001, 2002
  "}

rtng1977<-rtng[rtng$startYear == 1977,]
#rtng1977<-rtng1977[complete.cases(rtng1977[,1]),]
rtng1977<-unique(rtng1977)
rtng1977=rtng1977[!sum(is.na(rtng1977))==ncol(rtng1977),]


n1977<-nrow(rtng1977)

temp1977<-unique(prnc[prnc$tconst  %in%	rtng1977$tconst, "nconst"])#
temp1977<-prnc[prnc$nconst  %in%	temp1977$nconst, "tconst"]#
temp1977<-unique(prnc[prnc$tconst  %in%	temp1977$tconst,])#
temp1977<-temp1977[temp1977$tconst %in% bscs[bscs$startYear <= 1977,]$tconst,]# #temp1977<-merge(temp1977,bscs[,c("tconst","startYear")], by="tconst", all.x = T)# temp1977<-merge(temp1977, bscs[,c("tconst", "startYear")], by="tconst", all.x = TRUE)


temp1977<-temp1977[!(temp1977$tconst %in% animentry),]
rtng1977<-rtng1977[!(rtng1977$tconst %in% animentry),]


w1977 <- c(1977-4, 1977-3, 1977-2 ,1977-1, 1977)#library(plyr)#,1977-3,1977-4,1977-5,1977-6
w1977 <-unique(bscs[bscs$startYear %in% w1977,"tconst"])


# __________________________________________________________________________________________####  
#############################NETWORK of COLLABORATION####
#A-SEPARATING PROFESSIONALS BY ROLE####
tmp <- temp1977[(temp1977$tconst %in% w1977$tconst) & (temp1977$nconst %in% temp1977[temp1977$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"),]$nconst) , ]
tmp <- tmp[complete.cases(tmp[,c("tconst","nconst")]),]

#B-GRAPH FROM EDGE-LIST#### 
g<-          graph_from_data_frame(tmp, directed=FALSE)
g<-          simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-  V(g)$name %in% unique(tmp$nconst)
PROJECTION<- bipartite.projection(g) #two-mode
tnet<-       PROJECTION[[1]] #one-mode
g<-          PROJECTION[[2]] #one-mode
g<-          set.edge.attribute(g, "weight", E(g), 1)
cmpnt1977 <- no.clusters(g);           # Number of components
larg1977 <- sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1]; # Size of largest component
rlarg1977 <- (sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1])/vcount(g); # Relative size of largest component

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

#cast and crew of titles from major-studios in 1977 
link1977<-temp1977[temp1977$tconst %in% rtng1977$tconst,] #as_tibble(temp1977[temp1977$tconst %in% rtng1977$tconst,1:3])

g<- graph_from_data_frame(link1977[,1:2], directed=FALSE)
g<-simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-    V(g)$name %in% unique(link1977$nconst)
PROJECTION<-bipartite.projection(g) #two-mode
tnet<-PROJECTION[[1]] #one-mode
g<-PROJECTION[[2]]#one-mode
g <- set.edge.attribute(g, "weight", E(g), 1)


#A-CREW with titles from major-studios in 1977#### 
#!!!when individual performs multiple roles in a movie, that movie is entering multiple times in the calculations for that individual!!!####
indv1977 <- temp1977[temp1977$nconst %in% link1977[link1977$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst,c("tconst", "nconst","category")]
  #indv1977 <- indv1977[indv1977$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,] #& temp1977$category=='director' #[,-3] 
  #_adding genre, years, gconf, and metascores####
  indv1977<- merge(x = indv1977, y = bscs[ , c("tconst", "startYear", "genres")], all.x = T)#, by = "tconst", all.x=TRUE, fill=F
  #_adding gconf####
  indv1977<- merge(x = indv1977, y = gnrs[gnrs$tconst %in% w1977$tconst, c("tconst", "gConf")], all.x = TRUE)
  #_adding metascores####
  indv1977<- merge(x = indv1977, y = rtng1977[ , c("tconst", "metascore")], all.x = TRUE)
#k<-length(indv1977) # indv1977 <-indv1977[na.omit(indv1977[,1:as.numeric(k)])]
  indv1977 <- unique(indv1977)
  #!NO GENRE!####
  mssgnrs1977<-indv1977[indv1977$genres=="\\N",]
  mssgnrs1977<-rbind(mssgnrs1977, indv1977[is.na(indv1977$genres)==TRUE,])#indv1977<- indv1977[indv1977$genres!="\\N",]
  #indv1977<-link1977 %>% mutate_at(c('genres'), replace_na, '\\N')
  


length(unique(link1977[link1977$category %in%  c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst))
length(unique(indv1977$nconst))

#B-INDIVIDUAL ATTRIBUTES #### 
indv1977<-merge(aggregate(startYear ~ nconst, indv1977, paste, na.action=NULL),merge(
  aggregate(metascore ~ nconst, indv1977, function(x) mean(as.numeric(x), na.rm=TRUE), na.action=NULL),merge(#_average METASCORE ####
#!!!!!if one genre is NA gconf becomes NA  ######################                                                                                                             
    aggregate(gConf ~ nconst, indv1977, toString, na.action=na.omit),
    aggregate(genres ~ nconst, indv1977, toString, na.action=NULL)
    ))) #indv1977<-indv1977[,c(1,4:10,2,3)] for reordering columns
setnames(indv1977, "startYear", "year")
indv1977$startYear<-as.numeric(sapply(indv1977$year, min))#_startYear####
# sy1977<-indv1977[,c("nconst","sy")]
# indv1977<-indv1977[,!(names(indv1977)%in%c("sy"))]

indv1977$year <- vapply(indv1977$year, paste, collapse = ", ", character(1L)) # wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
indv1977$tenure<-1977-indv1977$startYear#_tenure####
indv1977$experience <- sapply(indv1977$year, function(x) str_count(x,"\\,"))#_experience####
indv1977$newcomer <- as.numeric(indv1977$experience == 0)#_newcomer####
indv1977$average_confusion<-sapply(strsplit(as.character(indv1977$gConf), ",", fixed=T), function(x) mean(as.numeric(x), na.rm=TRUE))#_average confusion####
indv1977$orientation <-sapply(strsplit(indv1977$genres,"\\, |\\,| " ), uniqueN)/(1+indv1977$experience)#!_so####
indv1977$year <- 1977 #_focal year####


#_role consolidation####
indv1977<-merge(indv1977,aggregate(n ~ nconst, dplyr::add_count(link1977, tconst, nconst), function(x) mean(as.numeric(x), na.rm=TRUE)), all.x = T)
setnames(indv1977, "n", "role_consolidation")


#_awards quality####
  #for all collaborators with titles in 1977 
  link1977<-merge(link1977,awrd,by.x='nconst', by.y='nm', all.x=TRUE) #nrow(na.omit(temp1977[,-4]))==nrow(temp1977) #nrow(na.omit(awrd))==nrow(awrd)
#!awards for missing nm in awrd replaced with zero####
  link1977<-link1977 %>% mutate_at(c('1977', '1976','1975','1974','1973','1995'), replace_na, 0)
  link1977[,'awards']<-link1977$`1977` #names(indv1977$`1977`)<-"awards"
  link1977[link1977$nconst=="nm7016360",c('nconst','awards')]
  link1977[is.na(link1977$awards)==TRUE,'awards']
  link1977[,'quality']<-link1977$`1976`+link1977$`1975`+link1977$`1974`+link1977$`1973`
indv1977<-unique(merge(indv1977,link1977[,c("nconst","awards","quality")],by='nconst', all.x=TRUE))




indv1977<-merge(indv1977,aggregate(budget ~ nconst, merge(link1977[,c("nconst","tconst")],rtng1977[,c("tconst","budget")], by="tconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst", all.x=T) #_budget####
indv1977<-merge(indv1977,aggregate(sequel ~ nconst, merge(link1977[,c("nconst","tconst")],rtng1977[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel####

#############################C-ALTER ATTRIBUTES####
#_alter quality####
tmp<-data.frame()
test<-data.frame()
#ind <-   which(sapply(link1977, is.numeric))
#for(j in ind){
  #set(link1977, i = which(is.na(link1977[[j]])), j = j, value = 0)}
for(nconst in names(V(delete.vertices(g, which(degree(g)==0))))){
  c<-neighbors(g, nconst, mode = "total")
  tmp<-rbind(tmp,data.frame(nconst, 
                            setNames(
                              aggregate(quality ~ tconst, 
                              link1977[link1977$nconst %in% str_trim(names(c)),], function(x) mean(as.numeric(x), na.rm=TRUE))
                              , c("tconst", "team_quality"))
                            )
             )} # V(tmp1)$name[c] 
indv1977<-merge(indv1977,aggregate(team_quality ~ nconst, tmp[tmp$tconst %in% rtng1977$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) 

#_team coreness####
link1977<-merge(x=link1977,y=dstr[,c("nconst","coreness")],by="nconst",all.x = TRUE)#[0:2,]
tmp<-data.frame()
for(nconst in link1977$nconst){
  c<-neighbors(g, nconst, mode = "total")
    if(nrow(link1977[link1977$nconst %in% names(c) & !is.na(link1977$coreness),])>0){
    tmp<-rbind(tmp,data.frame(nconst, na.omit(aggregate(coreness ~ tconst, link1977[link1977$nconst %in% str_trim(names(c)) & !is.na(link1977$coreness),], function(x) mean(as.numeric(x), na.rm=TRUE)))))}} # V(tmp1)$name[c] 
indv1977<-merge(indv1977,aggregate(coreness ~ nconst, tmp[tmp$tconst %in% rtng1977$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) # unique(link1977[link1977$nconst %in% names(c) & !link1977$category %in% c("actor","actress" ),]$category)# str(link1977[link1977$nconst %in% names(c) & !link1977$category %in% c("actor","actress" ),])
names(indv1977)<-gsub(x = names(indv1977), pattern = "coreness", replacement = "team_coreness") 

# #_role dummies####
# roles<-unique(temp1977$category)
# roles<-roles[!(roles %in% c("archive_footage", "self"))]
# indv1977<-merge(indv1977,na.omit(aggregate(category ~ nconst, temp1977[temp1977$tconst %in% rtng1977$tconst], toString)), by="nconst", all.x=T)
# for (i in na.omit(roles)) {
#   Variable=i;
#   print(Variable)
#   indv1977[, Variable] <- dum(i, indv1977$category)
# }

# __________________________________________________________________________________________####  
#############################MERGING DATA####

indv1977<-merge(indv1977, dstr, by = "nconst", all.x = TRUE)# wrt<-merge(wrt, wstr, by = "nconst", all.x=T)
#indv1977<-merge(indv1977, sdstr, by = "nconst", all.x=T)# wrt<-merge(wrt, swstr, by = "nconst", all.x=T)
#names(indv1977)<-gsub(x = names(indv1977), pattern = "\\.x", replacement = "") 
#names(indv1977)<-gsub(x = names(indv1977), pattern = "\\.y", replacement = "_smp") 

# __________________________________________________________________________________________####  
#############################FOCUS####
#_focus on genres####
for(i in c(na.omit(xx))){if(i!="\\N"){indv1977[,i]<-str_count(indv1977$genres, i)}}

indv1977[,c("focus_genres")]<-  #,"focus_genres_ent"
  hhi(indv1977[,(length(indv1977)-27):length(indv1977)])
#t<-merge(twrt, tdir, all.x=T, all.y = T)

#_focus on roles####
temp1977<-aggregate(category ~ nconst, temp1977, toString)
temp1977<-temp1977[temp1977$nconst  %in%	indv1977$nconst, 1:2]
temp1977$actress<-str_count(temp1977$category,             	"actress"            	)
temp1977$actor<-str_count(temp1977$category,               	"actor"              	)
temp1977$director<-str_count(temp1977$category,            	"director"           	)
temp1977$producer<-str_count(temp1977$category,            	"producer"           	)
temp1977$writer<-str_count(temp1977$category,              	"writer"             	)
temp1977$cinematographer<-str_count(temp1977$category,     	"cinematographer"    	)
temp1977$composer<-str_count(temp1977$category,            	"composer"           	)
temp1977$production_designer<-str_count(temp1977$category, 	"production_designer"	)
temp1977$editor<-str_count(temp1977$category,              	"editor"             	)
indv1977[,c("focus_roles","focus_roles_ent")]<-hhi(temp1977[,3:11])

link1977  <-merge(x = link1977, y = gnrs[, c("tconst", "genres")], by = "tconst", all.x=TRUE)
for(i in c(na.omit(xx))){if(i!="\\N"){link1977[,i]<-str_count(link1977$genres, i)}}
indv1977  <- indv1977[,-which(names(indv1977)%in%xx)]
for(i in c(na.omit(xx))){if(i!="\\N"){indv1977  <- merge(x = indv1977, y = 
                     aggregate(as.formula(paste0(i, "~ nconst")), link1977, sum)
                     , by = "nconst", all.x=TRUE)}}

# __________________________________________________________________________________________####  
#############################TITLE LEVEL MODEL####
#A-TITLE ATTRIBUTES####

#!_average_confusion(!try below comment!)####
#rtng1977$average_confusion  <-aggregate(average_confusion ~ tconst, merge(link1977[,c("nconst","tconst")], indv1977, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$average_confusion #link1977$category %in% c("director")
rtng1977                    <-merge(x = rtng1977, y = aggregate(average_confusion ~ tconst, merge(link1977[,c("nconst","tconst")], indv1977, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link1977$category %in% c("director")
#_experience####
rtng1977                    <-merge(x = rtng1977, y = aggregate(experience ~ tconst, merge(link1977[,c("nconst","tconst")], indv1977, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link1977$category %in% c("director")
#_genre_confusion####
rtng1977                    <-merge(x = rtng1977, y = gnrs[, c("tconst", "gConf")], by = "tconst", all.x=TRUE)
#_so####
rtng1977                    <-merge(x = rtng1977, y = aggregate(orientation ~ tconst, merge(link1977[,c("nconst","tconst")], indv1977, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_role_consolidation####
rtng1977                    <-merge(x = rtng1977, y = aggregate(role_consolidation ~ tconst, merge(link1977[,c("nconst","tconst")], indv1977, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_awards####
rtng1977                    <-merge(x = rtng1977, y = aggregate(awards ~ tconst, merge(link1977[,c("nconst","tconst")],indv1977, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) 
#_quality####
rtng1977                    <-merge(x = rtng1977, y = aggregate(quality ~ tconst, link1977, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)
#_focus on roles####
rtng1977                    <-merge(x = rtng1977, y = aggregate(focus_genres ~ tconst, merge(link1977[,c("nconst","tconst")],indv1977, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_focus on genres####
rtng1977                    <-merge(x = rtng1977, y = aggregate(focus_roles ~ tconst, merge(link1977[,c("nconst","tconst")],indv1977, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_brokerage####
rtng1977                    <-merge(x = rtng1977, y = aggregate(brokerage ~ tconst, merge(link1977[,c("nconst","tconst")],indv1977, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)#rtng1977$brokerage_smp       <-na.omit(aggregate(brokerage_smp ~ tconst, merge(link1977[,c("nconst","tconst")],indv1977, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$brokerage_smp)
#_coreness####
rtng1977                    <-merge(x = rtng1977, y = aggregate(coreness ~ tconst, link1977, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)#setNames(aggregate(coreness ~ tconst, link1977, function(x) mean(as.numeric(x), na.rm=TRUE)), c("tconst", "team_coreness"))$team_coreness
#!newcomer# rtng1977$newcomer<-NA # for (tconst in merge(link1977[!,c("nconst","tconst")])){#   rtng1977[rtng1977$tconst=="rtng1977", "newcomer"]<-#   ifelse(aggregate(newcomer ~ tconst, merge(link1977[!,c("nconst","tconst")], indv1977, by="nconst", all.x=T)$newcomer, function(x) mean(as.numeric(x), na.rm=TRUE)) #          ==1#          , 1, NA)}####

colSums(is.na(link1977))
colSums(is.na(indv1977))
colSums(is.na(rtng1977))

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
# #sdts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")1975
# 
# sdstr<- data.frame(nconst=names(sdcn),hubs_score=sdhs, page_rank=sdpr, brokerage=sdcn, coreness=sdcr, eigenvector_centrality=sdev)
# nrow(complete.cases(sdstr))
# 
# sapply(indv1977, function(y) sum(length(which(is.na(y)))))
# nrow(indv1977)
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
#                                         )), cols=director)) #'"Error in validObject(.Object) : invalid class "dgTMatrix" object:all row indices (slot 'i') must be between 0 and nrow1975 in a TsparseMatrix"

#graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
#OR
# g <- link1977[,1:2] %*% t(link1977[,1:2]) %>%
#   graph_from_adjacency_matrix(mode = "undirected", diag = FALSE, weighted = TRUE)







# #manual construction of their network####
# tmp<-na.omit(aggregate(nconst ~ tconst, link1977, toString))# tmp$edges<-sapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], function(x) combn(x, 2, FUN = NULL, simplify = F))# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
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
# unique(unique(link1977$nconst) %in% unique(names(V(g))))
# "nm0413541" %in% link1977[which(link1977$nconst %in% unique(V(g)$name))]$nconst
# "nm0413541" %in% V(g)$name
# "nm0413541" %in% link1977$nconst
# 
# "nm0000436" %in% link1977$nconst
# "nm0000436" %in% V(g)$name
# "nm0000436" %in% link1977[which(link1977$nconst %in% unique(V(g)$name))]$nconst
# 
# neighbors(g, "nm0413541", mode = "total")
# 
# 
# g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

