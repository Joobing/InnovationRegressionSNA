# __________________________________________________________________________________________####  
#############################RESAMPLE2009 ####
if(FALSE) {"
1- 2009 (number of) titles
2- relevant (present and past) affiliations to 2009
  # Crew of titles of major-STUDIOs in 2009
  # Titles that share CREW with titles of major-STUDIOs in 2009
  # Crew of aLL titles that share CREW with titles of major-STUDIOs in 2009
  # Up to 2009
3- THREE YEAR window for 2009
4- dropping animation and documentary
2006, 2007, 2008, 2009, 2001, 2002
  "}

rtng2009<-rtng[rtng$startYear == 2009,]
#rtng2009<-rtng2009[complete.cases(rtng2009[,1]),]
rtng2009<-unique(rtng2009)
rtng2009=rtng2009[!sum(is.na(rtng2009))==ncol(rtng2009),]


n2009<-nrow(rtng2009)

temp2009<-unique(prnc[prnc$tconst  %in%	rtng2009$tconst, "nconst"])#
temp2009<-prnc[prnc$nconst  %in%	temp2009$nconst, "tconst"]#
temp2009<-unique(prnc[prnc$tconst  %in%	temp2009$tconst,])#
temp2009<-temp2009[temp2009$tconst %in% bscs[bscs$startYear <= 2009,]$tconst,]# #temp2009<-merge(temp2009,bscs[,c("tconst","startYear")], by="tconst", all.x = T)# temp2009<-merge(temp2009, bscs[,c("tconst", "startYear")], by="tconst", all.x = TRUE)


temp2009<-temp2009[!(temp2009$tconst %in% animentry),]
rtng2009<-rtng2009[!(rtng2009$tconst %in% animentry),]


w2009 <- c(2009-4, 2009-3, 2009-2 ,2009-1, 2009)#library(plyr)#,2009-3,2009-4,2009-5,2009-6
w2009 <-unique(bscs[bscs$startYear %in% w2009,"tconst"])


# __________________________________________________________________________________________####  
#############################NETWORK of COLLABORATION####
#A-SEPARATING PROFESSIONALS BY ROLE####
tmp <- temp2009[(temp2009$tconst %in% w2009$tconst) & (temp2009$nconst %in% temp2009[temp2009$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"),]$nconst) , ]
tmp <- tmp[complete.cases(tmp[,c("tconst","nconst")]),]

#B-GRAPH FROM EDGE-LIST#### 
g<-          graph_from_data_frame(tmp, directed=FALSE)
g<-          simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-  V(g)$name %in% unique(tmp$nconst)
PROJECTION<- bipartite.projection(g) #two-mode
tnet<-       PROJECTION[[1]] #one-mode
g<-          PROJECTION[[2]] #one-mode
g<-          set.edge.attribute(g, "weight", E(g), 1)
cmpnt2009 <- no.clusters(g);           # Number of components
larg2009 <- sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1]; # Size of largest component
rlarg2009 <- (sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1])/vcount(g); # Relative size of largest component

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

#cast and crew of titles from major-studios in 2009 
link2009<-temp2009[temp2009$tconst %in% rtng2009$tconst,] #as_tibble(temp2009[temp2009$tconst %in% rtng2009$tconst,1:3])

g<- graph_from_data_frame(link2009[,1:2], directed=FALSE)
g<-simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-    V(g)$name %in% unique(link2009$nconst)
PROJECTION<-bipartite.projection(g) #two-mode
tnet<-PROJECTION[[1]] #one-mode
g<-PROJECTION[[2]]#one-mode
g <- set.edge.attribute(g, "weight", E(g), 1)


#A-CREW with titles from major-studios in 2009#### 
#!!!when individual performs multiple roles in a movie, that movie is entering multiple times in the calculations for that individual!!!####
indv2009 <- temp2009[temp2009$nconst %in% link2009[link2009$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst,c("tconst", "nconst","category")]
  #indv2009 <- indv2009[indv2009$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,] #& temp2009$category=='director' #[,-3] 
  #_adding genre, years, gconf, and metascores####
  indv2009<- merge(x = indv2009, y = bscs[ , c("tconst", "startYear", "genres")], all.x = T)#, by = "tconst", all.x=TRUE, fill=F
  #_adding gconf####
  indv2009<- merge(x = indv2009, y = gnrs[gnrs$tconst %in% w2009$tconst, c("tconst", "gConf")], all.x = TRUE)
  #_adding metascores####
  indv2009<- merge(x = indv2009, y = rtng2009[ , c("tconst", "metascore")], all.x = TRUE)
#k<-length(indv2009) # indv2009 <-indv2009[na.omit(indv2009[,1:as.numeric(k)])]
  indv2009 <- unique(indv2009)
  #!NO GENRE!####
  mssgnrs2009<-indv2009[indv2009$genres=="\\N",]
  mssgnrs2009<-rbind(mssgnrs2009, indv2009[is.na(indv2009$genres)==TRUE,])#indv2009<- indv2009[indv2009$genres!="\\N",]
  #indv2009<-link2009 %>% mutate_at(c('genres'), replace_na, '\\N')
  


length(unique(link2009[link2009$category %in%  c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst))
length(unique(indv2009$nconst))

#B-INDIVIDUAL ATTRIBUTES #### 
indv2009<-merge(aggregate(startYear ~ nconst, indv2009, paste, na.action=NULL),merge(
  aggregate(metascore ~ nconst, indv2009, function(x) mean(as.numeric(x), na.rm=TRUE), na.action=NULL),merge(#_average METASCORE ####
#!!!!!if one genre is NA gconf becomes NA  ######################                                                                                                             
    aggregate(gConf ~ nconst, indv2009, toString, na.action=na.omit),
    aggregate(genres ~ nconst, indv2009, toString, na.action=NULL)
    ))) #indv2009<-indv2009[,c(1,4:10,2,3)] for reordering columns
setnames(indv2009, "startYear", "year")
indv2009$startYear<-as.numeric(sapply(indv2009$year, min))#_startYear####
# sy2009<-indv2009[,c("nconst","sy")]
# indv2009<-indv2009[,!(names(indv2009)%in%c("sy"))]

indv2009$year <- vapply(indv2009$year, paste, collapse = ", ", character(1L)) # wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
indv2009$tenure<-2009-indv2009$startYear#_tenure####
indv2009$experience <- sapply(indv2009$year, function(x) str_count(x,"\\,"))#_experience####
indv2009$newcomer <- as.numeric(indv2009$experience == 0)#_newcomer####
indv2009$average_confusion<-sapply(strsplit(as.character(indv2009$gConf), ",", fixed=T), function(x) mean(as.numeric(x), na.rm=TRUE))#_average confusion####
indv2009$orientation <-sapply(strsplit(indv2009$genres,"\\, |\\,| " ), uniqueN)/(1+indv2009$experience)#!_so####
indv2009$year <- 2009 #_focal year####


#_role consolidation####
indv2009<-merge(indv2009,aggregate(n ~ nconst, dplyr::add_count(link2009, tconst, nconst), function(x) mean(as.numeric(x), na.rm=TRUE)), all.x = T)
setnames(indv2009, "n", "role_consolidation")


#_awards quality####
  #for all collaborators with titles in 2009 
  link2009<-merge(link2009,awrd,by.x='nconst', by.y='nm', all.x=TRUE) #nrow(na.omit(temp2009[,-4]))==nrow(temp2009) #nrow(na.omit(awrd))==nrow(awrd)
#!awards for missing nm in awrd replaced with zero####
  link2009<-link2009 %>% mutate_at(c('2009', '2008','2007','2006','2005','1995'), replace_na, 0)
  link2009[,'awards']<-link2009$`2009` #names(indv2009$`2009`)<-"awards"
  link2009[link2009$nconst=="nm7016360",c('nconst','awards')]
  link2009[is.na(link2009$awards)==TRUE,'awards']
  link2009[,'quality']<-link2009$`2008`+link2009$`2007`+link2009$`2006`+link2009$`2005`
indv2009<-unique(merge(indv2009,link2009[,c("nconst","awards","quality")],by='nconst', all.x=TRUE))




indv2009<-merge(indv2009,aggregate(budget ~ nconst, merge(link2009[,c("nconst","tconst")],rtng2009[,c("tconst","budget")], by="tconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst", all.x=T) #_budget####
indv2009<-merge(indv2009,aggregate(sequel ~ nconst, merge(link2009[,c("nconst","tconst")],rtng2009[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel####

#############################C-ALTER ATTRIBUTES####
#_alter quality####
tmp<-data.frame()
test<-data.frame()
#ind <-   which(sapply(link2009, is.numeric))
#for(j in ind){
  #set(link2009, i = which(is.na(link2009[[j]])), j = j, value = 0)}
for(nconst in names(V(delete.vertices(g, which(degree(g)==0))))){
  c<-neighbors(g, nconst, mode = "total")
  tmp<-rbind(tmp,data.frame(nconst, 
                            setNames(
                              aggregate(quality ~ tconst, 
                              link2009[link2009$nconst %in% str_trim(names(c)),], function(x) mean(as.numeric(x), na.rm=TRUE))
                              , c("tconst", "team_quality"))
                            )
             )} # V(tmp1)$name[c] 
indv2009<-merge(indv2009,aggregate(team_quality ~ nconst, tmp[tmp$tconst %in% rtng2009$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) 

#_team coreness####
link2009<-merge(x=link2009,y=dstr[,c("nconst","coreness")],by="nconst",all.x = TRUE)#[0:2,]
tmp<-data.frame()
for(nconst in link2009$nconst){
  c<-neighbors(g, nconst, mode = "total")
    if(nrow(link2009[link2009$nconst %in% names(c) & !is.na(link2009$coreness),])>0){
    tmp<-rbind(tmp,data.frame(nconst, na.omit(aggregate(coreness ~ tconst, link2009[link2009$nconst %in% str_trim(names(c)) & !is.na(link2009$coreness),], function(x) mean(as.numeric(x), na.rm=TRUE)))))}} # V(tmp1)$name[c] 
indv2009<-merge(indv2009,aggregate(coreness ~ nconst, tmp[tmp$tconst %in% rtng2009$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) # unique(link2009[link2009$nconst %in% names(c) & !link2009$category %in% c("actor","actress" ),]$category)# str(link2009[link2009$nconst %in% names(c) & !link2009$category %in% c("actor","actress" ),])
names(indv2009)<-gsub(x = names(indv2009), pattern = "coreness", replacement = "team_coreness") 

# #_role dummies####
# roles<-unique(temp2009$category)
# roles<-roles[!(roles %in% c("archive_footage", "self"))]
# indv2009<-merge(indv2009,na.omit(aggregate(category ~ nconst, temp2009[temp2009$tconst %in% rtng2009$tconst], toString)), by="nconst", all.x=T)
# for (i in na.omit(roles)) {
#   Variable=i;
#   print(Variable)
#   indv2009[, Variable] <- dum(i, indv2009$category)
# }

# __________________________________________________________________________________________####  
#############################MERGING DATA####

indv2009<-merge(indv2009, dstr, by = "nconst", all.x = TRUE)# wrt<-merge(wrt, wstr, by = "nconst", all.x=T)
#indv2009<-merge(indv2009, sdstr, by = "nconst", all.x=T)# wrt<-merge(wrt, swstr, by = "nconst", all.x=T)
#names(indv2009)<-gsub(x = names(indv2009), pattern = "\\.x", replacement = "") 
#names(indv2009)<-gsub(x = names(indv2009), pattern = "\\.y", replacement = "_smp") 

# __________________________________________________________________________________________####  
#############################FOCUS####
#_focus on genres####
for(i in c(na.omit(xx))){if(i!="\\N"){indv2009[,i]<-str_count(indv2009$genres, i)}}

indv2009[,c("focus_genres")]<-  #,"focus_genres_ent"
  hhi(indv2009[,(length(indv2009)-27):length(indv2009)])
#t<-merge(twrt, tdir, all.x=T, all.y = T)

#_focus on roles####
temp2009<-aggregate(category ~ nconst, temp2009, toString)
temp2009<-temp2009[temp2009$nconst  %in%	indv2009$nconst, 1:2]
temp2009$actress<-str_count(temp2009$category,             	"actress"            	)
temp2009$actor<-str_count(temp2009$category,               	"actor"              	)
temp2009$director<-str_count(temp2009$category,            	"director"           	)
temp2009$producer<-str_count(temp2009$category,            	"producer"           	)
temp2009$writer<-str_count(temp2009$category,              	"writer"             	)
temp2009$cinematographer<-str_count(temp2009$category,     	"cinematographer"    	)
temp2009$composer<-str_count(temp2009$category,            	"composer"           	)
temp2009$production_designer<-str_count(temp2009$category, 	"production_designer"	)
temp2009$editor<-str_count(temp2009$category,              	"editor"             	)
indv2009[,c("focus_roles","focus_roles_ent")]<-hhi(temp2009[,3:11])

link2009  <-merge(x = link2009, y = gnrs[, c("tconst", "genres")], by = "tconst", all.x=TRUE)
for(i in c(na.omit(xx))){if(i!="\\N"){link2009[,i]<-str_count(link2009$genres, i)}}
indv2009  <- indv2009[,-which(names(indv2009)%in%xx)]
for(i in c(na.omit(xx))){if(i!="\\N"){indv2009  <- merge(x = indv2009, y = 
                     aggregate(as.formula(paste0(i, "~ nconst")), link2009, sum)
                     , by = "nconst", all.x=TRUE)}}

# __________________________________________________________________________________________####  
#############################TITLE LEVEL MODEL####
#A-TITLE ATTRIBUTES####

#!_average_confusion(!try below comment!)####
#rtng2009$average_confusion  <-aggregate(average_confusion ~ tconst, merge(link2009[,c("nconst","tconst")], indv2009, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$average_confusion #link2009$category %in% c("director")
rtng2009                    <-merge(x = rtng2009, y = aggregate(average_confusion ~ tconst, merge(link2009[,c("nconst","tconst")], indv2009, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link2009$category %in% c("director")
#_experience####
rtng2009                    <-merge(x = rtng2009, y = aggregate(experience ~ tconst, merge(link2009[,c("nconst","tconst")], indv2009, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link2009$category %in% c("director")
#_genre_confusion####
rtng2009                    <-merge(x = rtng2009, y = gnrs[, c("tconst", "gConf")], by = "tconst", all.x=TRUE)
#_so####
rtng2009                    <-merge(x = rtng2009, y = aggregate(orientation ~ tconst, merge(link2009[,c("nconst","tconst")], indv2009, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_role_consolidation####
rtng2009                    <-merge(x = rtng2009, y = aggregate(role_consolidation ~ tconst, merge(link2009[,c("nconst","tconst")], indv2009, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_awards####
rtng2009                    <-merge(x = rtng2009, y = aggregate(awards ~ tconst, merge(link2009[,c("nconst","tconst")],indv2009, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) 
#_quality####
rtng2009                    <-merge(x = rtng2009, y = aggregate(quality ~ tconst, link2009, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)
#_focus on roles####
rtng2009                    <-merge(x = rtng2009, y = aggregate(focus_genres ~ tconst, merge(link2009[,c("nconst","tconst")],indv2009, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_focus on genres####
rtng2009                    <-merge(x = rtng2009, y = aggregate(focus_roles ~ tconst, merge(link2009[,c("nconst","tconst")],indv2009, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_brokerage####
rtng2009                    <-merge(x = rtng2009, y = aggregate(brokerage ~ tconst, merge(link2009[,c("nconst","tconst")],indv2009, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)#rtng2009$brokerage_smp       <-na.omit(aggregate(brokerage_smp ~ tconst, merge(link2009[,c("nconst","tconst")],indv2009, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$brokerage_smp)
#_coreness####
rtng2009                    <-merge(x = rtng2009, y = aggregate(coreness ~ tconst, link2009, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)#setNames(aggregate(coreness ~ tconst, link2009, function(x) mean(as.numeric(x), na.rm=TRUE)), c("tconst", "team_coreness"))$team_coreness
#!newcomer# rtng2009$newcomer<-NA # for (tconst in merge(link2009[!,c("nconst","tconst")])){#   rtng2009[rtng2009$tconst=="rtng2009", "newcomer"]<-#   ifelse(aggregate(newcomer ~ tconst, merge(link2009[!,c("nconst","tconst")], indv2009, by="nconst", all.x=T)$newcomer, function(x) mean(as.numeric(x), na.rm=TRUE)) #          ==1#          , 1, NA)}####

colSums(is.na(link2009))
colSums(is.na(indv2009))
colSums(is.na(rtng2009))

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
# #sdts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")2007
# 
# sdstr<- data.frame(nconst=names(sdcn),hubs_score=sdhs, page_rank=sdpr, brokerage=sdcn, coreness=sdcr, eigenvector_centrality=sdev)
# nrow(complete.cases(sdstr))
# 
# sapply(indv2009, function(y) sum(length(which(is.na(y)))))
# nrow(indv2009)
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
#                                         )), cols=director)) #'"Error in validObject(.Object) : invalid class "dgTMatrix" object:all row indices (slot 'i') must be between 0 and nrow2007 in a TsparseMatrix"

#graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
#OR
# g <- link2009[,1:2] %*% t(link2009[,1:2]) %>%
#   graph_from_adjacency_matrix(mode = "undirected", diag = FALSE, weighted = TRUE)







# #manual construction of their network####
# tmp<-na.omit(aggregate(nconst ~ tconst, link2009, toString))# tmp$edges<-sapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], function(x) combn(x, 2, FUN = NULL, simplify = F))# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
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
# unique(unique(link2009$nconst) %in% unique(names(V(g))))
# "nm0413541" %in% link2009[which(link2009$nconst %in% unique(V(g)$name))]$nconst
# "nm0413541" %in% V(g)$name
# "nm0413541" %in% link2009$nconst
# 
# "nm0000436" %in% link2009$nconst
# "nm0000436" %in% V(g)$name
# "nm0000436" %in% link2009[which(link2009$nconst %in% unique(V(g)$name))]$nconst
# 
# neighbors(g, "nm0413541", mode = "total")
# 
# 
# g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

