# __________________________________________________________________________________________####  
#############################RESAMPLE1949 ####
if(FALSE) {"
1- 1949 (number of) titles
2- relevant (present and past) affiliations to 1949
  # Crew of titles of major-STUDIOs in 1949
  # Titles that share CREW with titles of major-STUDIOs in 1949
  # Crew of aLL titles that share CREW with titles of major-STUDIOs in 1949
  # Up to 1949
3- THREE YEAR window for 1949
4- dropping animation and documentary
1946, 1947, 1948, 1949, 2001, 2002
  "}

rtng1949<-rtng[rtng$startYear == 1949,]
#rtng1949<-rtng1949[complete.cases(rtng1949[,1]),]
rtng1949<-unique(rtng1949)
rtng1949=rtng1949[!sum(is.na(rtng1949))==ncol(rtng1949),]


n1949<-nrow(rtng1949)

temp1949<-unique(prnc[prnc$tconst  %in%	rtng1949$tconst, "nconst"])#
temp1949<-prnc[prnc$nconst  %in%	temp1949$nconst, "tconst"]#
temp1949<-unique(prnc[prnc$tconst  %in%	temp1949$tconst,])#
temp1949<-temp1949[temp1949$tconst %in% bscs[bscs$startYear <= 1949,]$tconst,]# #temp1949<-merge(temp1949,bscs[,c("tconst","startYear")], by="tconst", all.x = T)# temp1949<-merge(temp1949, bscs[,c("tconst", "startYear")], by="tconst", all.x = TRUE)


temp1949<-temp1949[!(temp1949$tconst %in% animentry),]
rtng1949<-rtng1949[!(rtng1949$tconst %in% animentry),]


w1949 <- c(1949-4, 1949-3, 1949-2 ,1949-1, 1949)#library(plyr)#,1949-3,1949-4,1949-5,1949-6
w1949 <-unique(bscs[bscs$startYear %in% w1949,"tconst"])


# __________________________________________________________________________________________####  
#############################NETWORK of COLLABORATION####
#A-SEPARATING PROFESSIONALS BY ROLE####
tmp <- temp1949[(temp1949$tconst %in% w1949$tconst) & (temp1949$nconst %in% temp1949[temp1949$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"),]$nconst) , ]
tmp <- tmp[complete.cases(tmp[,c("tconst","nconst")]),]

#B-GRAPH FROM EDGE-LIST#### 
g<-          graph_from_data_frame(tmp, directed=FALSE)
g<-          simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-  V(g)$name %in% unique(tmp$nconst)
PROJECTION<- bipartite.projection(g) #two-mode
tnet<-       PROJECTION[[1]] #one-mode
g<-          PROJECTION[[2]] #one-mode
g<-          set.edge.attribute(g, "weight", E(g), 1)
cmpnt1949 <- no.clusters(g);           # Number of components
larg1949 <- sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1]; # Size of largest component
rlarg1949 <- (sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1])/vcount(g); # Relative size of largest component

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

#cast and crew of titles from major-studios in 1949 
link1949<-temp1949[temp1949$tconst %in% rtng1949$tconst,] #as_tibble(temp1949[temp1949$tconst %in% rtng1949$tconst,1:3])

g<- graph_from_data_frame(link1949[,1:2], directed=FALSE)
g<-simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-    V(g)$name %in% unique(link1949$nconst)
PROJECTION<-bipartite.projection(g) #two-mode
tnet<-PROJECTION[[1]] #one-mode
g<-PROJECTION[[2]]#one-mode
g <- set.edge.attribute(g, "weight", E(g), 1)


#A-CREW with titles from major-studios in 1949#### 
#!!!when individual performs multiple roles in a movie, that movie is entering multiple times in the calculations for that individual!!!####
indv1949 <- temp1949[temp1949$nconst %in% link1949[link1949$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst,c("tconst", "nconst","category")]
  #indv1949 <- indv1949[indv1949$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,] #& temp1949$category=='director' #[,-3] 
  #_adding genre, years, gconf, and metascores####
  indv1949<- merge(x = indv1949, y = bscs[ , c("tconst", "startYear", "genres")], all.x = T)#, by = "tconst", all.x=TRUE, fill=F
  #_adding gconf####
  indv1949<- merge(x = indv1949, y = gnrs[gnrs$tconst %in% w1949$tconst, c("tconst", "gConf")], all.x = TRUE)
  #_adding metascores####
  indv1949<- merge(x = indv1949, y = rtng1949[ , c("tconst", "metascore")], all.x = TRUE)
#k<-length(indv1949) # indv1949 <-indv1949[na.omit(indv1949[,1:as.numeric(k)])]
  indv1949 <- unique(indv1949)
  #!NO GENRE!####
  mssgnrs1949<-indv1949[indv1949$genres=="\\N",]
  mssgnrs1949<-rbind(mssgnrs1949, indv1949[is.na(indv1949$genres)==TRUE,])#indv1949<- indv1949[indv1949$genres!="\\N",]
  #indv1949<-link1949 %>% mutate_at(c('genres'), replace_na, '\\N')
  


length(unique(link1949[link1949$category %in%  c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst))
length(unique(indv1949$nconst))

#B-INDIVIDUAL ATTRIBUTES #### 
indv1949<-merge(aggregate(startYear ~ nconst, indv1949, paste, na.action=NULL),merge(
  aggregate(metascore ~ nconst, indv1949, function(x) mean(as.numeric(x), na.rm=TRUE), na.action=NULL),merge(#_average METASCORE ####
#!!!!!if one genre is NA gconf becomes NA  ######################                                                                                                             
    aggregate(gConf ~ nconst, indv1949, toString, na.action=na.omit),
    aggregate(genres ~ nconst, indv1949, toString, na.action=NULL)
    ))) #indv1949<-indv1949[,c(1,4:10,2,3)] for reordering columns
setnames(indv1949, "startYear", "year")
indv1949$startYear<-as.numeric(sapply(indv1949$year, min))#_startYear####
# sy1949<-indv1949[,c("nconst","sy")]
# indv1949<-indv1949[,!(names(indv1949)%in%c("sy"))]

indv1949$year <- vapply(indv1949$year, paste, collapse = ", ", character(1L)) # wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
indv1949$tenure<-1949-indv1949$startYear#_tenure####
indv1949$experience <- sapply(indv1949$year, function(x) str_count(x,"\\,"))#_experience####
indv1949$newcomer <- as.numeric(indv1949$experience == 0)#_newcomer####
indv1949$average_confusion<-sapply(strsplit(as.character(indv1949$gConf), ",", fixed=T), function(x) mean(as.numeric(x), na.rm=TRUE))#_average confusion####
indv1949$orientation <-sapply(strsplit(indv1949$genres,"\\, |\\,| " ), uniqueN)/(1+indv1949$experience)#!_so####
indv1949$year <- 1949 #_focal year####


#_role consolidation####
indv1949<-merge(indv1949,aggregate(n ~ nconst, dplyr::add_count(link1949, tconst, nconst), function(x) mean(as.numeric(x), na.rm=TRUE)), all.x = T)
setnames(indv1949, "n", "role_consolidation")


#_awards quality####
  #for all collaborators with titles in 1949 
  link1949<-merge(link1949,awrd,by.x='nconst', by.y='nm', all.x=TRUE) #nrow(na.omit(temp1949[,-4]))==nrow(temp1949) #nrow(na.omit(awrd))==nrow(awrd)
#!awards for missing nm in awrd replaced with zero####
  link1949<-link1949 %>% mutate_at(c('1949', '1948','1947','1946','1945','1995'), replace_na, 0)
  link1949[,'awards']<-link1949$`1949` #names(indv1949$`1949`)<-"awards"
  link1949[link1949$nconst=="nm7016360",c('nconst','awards')]
  link1949[is.na(link1949$awards)==TRUE,'awards']
  link1949[,'quality']<-link1949$`1948`+link1949$`1947`+link1949$`1946`+link1949$`1945`
indv1949<-unique(merge(indv1949,link1949[,c("nconst","awards","quality")],by='nconst', all.x=TRUE))




indv1949<-merge(indv1949,aggregate(budget ~ nconst, merge(link1949[,c("nconst","tconst")],rtng1949[,c("tconst","budget")], by="tconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst", all.x=T) #_budget####
indv1949<-merge(indv1949,aggregate(sequel ~ nconst, merge(link1949[,c("nconst","tconst")],rtng1949[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel####

#############################C-ALTER ATTRIBUTES####
#_alter quality####
tmp<-data.frame()
test<-data.frame()
#ind <-   which(sapply(link1949, is.numeric))
#for(j in ind){
  #set(link1949, i = which(is.na(link1949[[j]])), j = j, value = 0)}
for(nconst in names(V(delete.vertices(g, which(degree(g)==0))))){
  c<-neighbors(g, nconst, mode = "total")
  tmp<-rbind(tmp,data.frame(nconst, 
                            setNames(
                              aggregate(quality ~ tconst, 
                              link1949[link1949$nconst %in% str_trim(names(c)),], function(x) mean(as.numeric(x), na.rm=TRUE))
                              , c("tconst", "team_quality"))
                            )
             )} # V(tmp1)$name[c] 
indv1949<-merge(indv1949,aggregate(team_quality ~ nconst, tmp[tmp$tconst %in% rtng1949$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) 

#_team coreness####
link1949<-merge(x=link1949,y=dstr[,c("nconst","coreness")],by="nconst",all.x = TRUE)#[0:2,]
tmp<-data.frame()
for(nconst in link1949$nconst){
  c<-neighbors(g, nconst, mode = "total")
    if(nrow(link1949[link1949$nconst %in% names(c) & !is.na(link1949$coreness),])>0){
    tmp<-rbind(tmp,data.frame(nconst, na.omit(aggregate(coreness ~ tconst, link1949[link1949$nconst %in% str_trim(names(c)) & !is.na(link1949$coreness),], function(x) mean(as.numeric(x), na.rm=TRUE)))))}} # V(tmp1)$name[c] 
indv1949<-merge(indv1949,aggregate(coreness ~ nconst, tmp[tmp$tconst %in% rtng1949$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) # unique(link1949[link1949$nconst %in% names(c) & !link1949$category %in% c("actor","actress" ),]$category)# str(link1949[link1949$nconst %in% names(c) & !link1949$category %in% c("actor","actress" ),])
names(indv1949)<-gsub(x = names(indv1949), pattern = "coreness", replacement = "team_coreness") 

# #_role dummies####
# roles<-unique(temp1949$category)
# roles<-roles[!(roles %in% c("archive_footage", "self"))]
# indv1949<-merge(indv1949,na.omit(aggregate(category ~ nconst, temp1949[temp1949$tconst %in% rtng1949$tconst], toString)), by="nconst", all.x=T)
# for (i in na.omit(roles)) {
#   Variable=i;
#   print(Variable)
#   indv1949[, Variable] <- dum(i, indv1949$category)
# }

# __________________________________________________________________________________________####  
#############################MERGING DATA####

indv1949<-merge(indv1949, dstr, by = "nconst", all.x = TRUE)# wrt<-merge(wrt, wstr, by = "nconst", all.x=T)
#indv1949<-merge(indv1949, sdstr, by = "nconst", all.x=T)# wrt<-merge(wrt, swstr, by = "nconst", all.x=T)
#names(indv1949)<-gsub(x = names(indv1949), pattern = "\\.x", replacement = "") 
#names(indv1949)<-gsub(x = names(indv1949), pattern = "\\.y", replacement = "_smp") 

# __________________________________________________________________________________________####  
#############################FOCUS####
#_focus on genres####
for(i in c(na.omit(xx))){if(i!="\\N"){indv1949[,i]<-str_count(indv1949$genres, i)}}

indv1949[,c("focus_genres")]<-  #,"focus_genres_ent"
  hhi(indv1949[,(length(indv1949)-27):length(indv1949)])
#t<-merge(twrt, tdir, all.x=T, all.y = T)

#_focus on roles####
temp1949<-aggregate(category ~ nconst, temp1949, toString)
temp1949<-temp1949[temp1949$nconst  %in%	indv1949$nconst, 1:2]
temp1949$actress<-str_count(temp1949$category,             	"actress"            	)
temp1949$actor<-str_count(temp1949$category,               	"actor"              	)
temp1949$director<-str_count(temp1949$category,            	"director"           	)
temp1949$producer<-str_count(temp1949$category,            	"producer"           	)
temp1949$writer<-str_count(temp1949$category,              	"writer"             	)
temp1949$cinematographer<-str_count(temp1949$category,     	"cinematographer"    	)
temp1949$composer<-str_count(temp1949$category,            	"composer"           	)
temp1949$production_designer<-str_count(temp1949$category, 	"production_designer"	)
temp1949$editor<-str_count(temp1949$category,              	"editor"             	)
indv1949[,c("focus_roles","focus_roles_ent")]<-hhi(temp1949[,3:11])

link1949  <-merge(x = link1949, y = gnrs[, c("tconst", "genres")], by = "tconst", all.x=TRUE)
for(i in c(na.omit(xx))){if(i!="\\N"){link1949[,i]<-str_count(link1949$genres, i)}}
indv1949  <- indv1949[,-which(names(indv1949)%in%xx)]
for(i in c(na.omit(xx))){if(i!="\\N"){indv1949  <- merge(x = indv1949, y = 
                     aggregate(as.formula(paste0(i, "~ nconst")), link1949, sum)
                     , by = "nconst", all.x=TRUE)}}

# __________________________________________________________________________________________####  
#############################TITLE LEVEL MODEL####
#A-TITLE ATTRIBUTES####

#!_average_confusion(!try below comment!)####
#rtng1949$average_confusion  <-aggregate(average_confusion ~ tconst, merge(link1949[,c("nconst","tconst")], indv1949, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$average_confusion #link1949$category %in% c("director")
rtng1949                    <-merge(x = rtng1949, y = aggregate(average_confusion ~ tconst, merge(link1949[,c("nconst","tconst")], indv1949, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link1949$category %in% c("director")
#_experience####
rtng1949                    <-merge(x = rtng1949, y = aggregate(experience ~ tconst, merge(link1949[,c("nconst","tconst")], indv1949, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link1949$category %in% c("director")
#_genre_confusion####
rtng1949                    <-merge(x = rtng1949, y = gnrs[, c("tconst", "gConf")], by = "tconst", all.x=TRUE)
#_so####
rtng1949                    <-merge(x = rtng1949, y = aggregate(orientation ~ tconst, merge(link1949[,c("nconst","tconst")], indv1949, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_role_consolidation####
rtng1949                    <-merge(x = rtng1949, y = aggregate(role_consolidation ~ tconst, merge(link1949[,c("nconst","tconst")], indv1949, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_awards####
rtng1949                    <-merge(x = rtng1949, y = aggregate(awards ~ tconst, merge(link1949[,c("nconst","tconst")],indv1949, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) 
#_quality####
rtng1949                    <-merge(x = rtng1949, y = aggregate(quality ~ tconst, link1949, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)
#_focus on roles####
rtng1949                    <-merge(x = rtng1949, y = aggregate(focus_genres ~ tconst, merge(link1949[,c("nconst","tconst")],indv1949, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_focus on genres####
rtng1949                    <-merge(x = rtng1949, y = aggregate(focus_roles ~ tconst, merge(link1949[,c("nconst","tconst")],indv1949, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_brokerage####
rtng1949                    <-merge(x = rtng1949, y = aggregate(brokerage ~ tconst, merge(link1949[,c("nconst","tconst")],indv1949, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)#rtng1949$brokerage_smp       <-na.omit(aggregate(brokerage_smp ~ tconst, merge(link1949[,c("nconst","tconst")],indv1949, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$brokerage_smp)
#_coreness####
rtng1949                    <-merge(x = rtng1949, y = aggregate(coreness ~ tconst, link1949, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)#setNames(aggregate(coreness ~ tconst, link1949, function(x) mean(as.numeric(x), na.rm=TRUE)), c("tconst", "team_coreness"))$team_coreness
#!newcomer# rtng1949$newcomer<-NA # for (tconst in merge(link1949[!,c("nconst","tconst")])){#   rtng1949[rtng1949$tconst=="rtng1949", "newcomer"]<-#   ifelse(aggregate(newcomer ~ tconst, merge(link1949[!,c("nconst","tconst")], indv1949, by="nconst", all.x=T)$newcomer, function(x) mean(as.numeric(x), na.rm=TRUE)) #          ==1#          , 1, NA)}####

colSums(is.na(link1949))
colSums(is.na(indv1949))
colSums(is.na(rtng1949))

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
# #sdts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")1947
# 
# sdstr<- data.frame(nconst=names(sdcn),hubs_score=sdhs, page_rank=sdpr, brokerage=sdcn, coreness=sdcr, eigenvector_centrality=sdev)
# nrow(complete.cases(sdstr))
# 
# sapply(indv1949, function(y) sum(length(which(is.na(y)))))
# nrow(indv1949)
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
#                                         )), cols=director)) #'"Error in validObject(.Object) : invalid class "dgTMatrix" object:all row indices (slot 'i') must be between 0 and nrow1947 in a TsparseMatrix"

#graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
#OR
# g <- link1949[,1:2] %*% t(link1949[,1:2]) %>%
#   graph_from_adjacency_matrix(mode = "undirected", diag = FALSE, weighted = TRUE)







# #manual construction of their network####
# tmp<-na.omit(aggregate(nconst ~ tconst, link1949, toString))# tmp$edges<-sapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], function(x) combn(x, 2, FUN = NULL, simplify = F))# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
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
# unique(unique(link1949$nconst) %in% unique(names(V(g))))
# "nm0413541" %in% link1949[which(link1949$nconst %in% unique(V(g)$name))]$nconst
# "nm0413541" %in% V(g)$name
# "nm0413541" %in% link1949$nconst
# 
# "nm0000436" %in% link1949$nconst
# "nm0000436" %in% V(g)$name
# "nm0000436" %in% link1949[which(link1949$nconst %in% unique(V(g)$name))]$nconst
# 
# neighbors(g, "nm0413541", mode = "total")
# 
# 
# g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

