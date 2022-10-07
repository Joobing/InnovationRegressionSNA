# __________________________________________________________________________________________####  
#############################RESAMPLE1932 ####
if(FALSE) {"
1- 1932 (number of) titles
2- relevant (present and past) affiliations to 1932
  # Crew of titles of major-STUDIOs in 1932
  # Titles that share CREW with titles of major-STUDIOs in 1932
  # Crew of aLL titles that share CREW with titles of major-STUDIOs in 1932
  # Up to 1932
3- THREE YEAR window for 1932
4- dropping animation and documentary
1929, 1930, 1931, 1932, 2001, 2002
  "}

rtng1932<-rtng[rtng$startYear == 1932,]
#rtng1932<-rtng1932[complete.cases(rtng1932[,1]),]
rtng1932<-unique(rtng1932)
rtng1932=rtng1932[!sum(is.na(rtng1932))==ncol(rtng1932),]


n1932<-nrow(rtng1932)

temp1932<-unique(prnc[prnc$tconst  %in%	rtng1932$tconst, "nconst"])#
temp1932<-prnc[prnc$nconst  %in%	temp1932$nconst, "tconst"]#
temp1932<-unique(prnc[prnc$tconst  %in%	temp1932$tconst,])#
temp1932<-temp1932[temp1932$tconst %in% bscs[bscs$startYear <= 1932,]$tconst,]# #temp1932<-merge(temp1932,bscs[,c("tconst","startYear")], by="tconst", all.x = T)# temp1932<-merge(temp1932, bscs[,c("tconst", "startYear")], by="tconst", all.x = TRUE)


temp1932<-temp1932[!(temp1932$tconst %in% animentry),]
rtng1932<-rtng1932[!(rtng1932$tconst %in% animentry),]


w1932 <- c(1932-4, 1932-3, 1932-2 ,1932-1, 1932)#library(plyr)#,1932-3,1932-4,1932-5,1932-6
w1932 <-unique(bscs[bscs$startYear %in% w1932,"tconst"])


# __________________________________________________________________________________________####  
#############################NETWORK of COLLABORATION####
#A-SEPARATING PROFESSIONALS BY ROLE####
tmp <- temp1932[(temp1932$tconst %in% w1932$tconst) & (temp1932$nconst %in% temp1932[temp1932$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"),]$nconst) , ]
tmp <- tmp[complete.cases(tmp[,c("tconst","nconst")]),]

#B-GRAPH FROM EDGE-LIST#### 
g<-          graph_from_data_frame(tmp, directed=FALSE)
g<-          simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-  V(g)$name %in% unique(tmp$nconst)
PROJECTION<- bipartite.projection(g) #two-mode
tnet<-       PROJECTION[[1]] #one-mode
g<-          PROJECTION[[2]] #one-mode
g<-          set.edge.attribute(g, "weight", E(g), 1)
cmpnt1932 <- no.clusters(g);           # Number of components
larg1932 <- sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1]; # Size of largest component
rlarg1932 <- (sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1])/vcount(g); # Relative size of largest component

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

#cast and crew of titles from major-studios in 1932 
link1932<-temp1932[temp1932$tconst %in% rtng1932$tconst,] #as_tibble(temp1932[temp1932$tconst %in% rtng1932$tconst,1:3])

g<- graph_from_data_frame(link1932[,1:2], directed=FALSE)
g<-simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-    V(g)$name %in% unique(link1932$nconst)
PROJECTION<-bipartite.projection(g) #two-mode
tnet<-PROJECTION[[1]] #one-mode
g<-PROJECTION[[2]]#one-mode
g <- set.edge.attribute(g, "weight", E(g), 1)


#A-CREW with titles from major-studios in 1932#### 
#!!!when individual performs multiple roles in a movie, that movie is entering multiple times in the calculations for that individual!!!####
indv1932 <- temp1932[temp1932$nconst %in% link1932[link1932$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst,c("tconst", "nconst","category")]
  #indv1932 <- indv1932[indv1932$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,] #& temp1932$category=='director' #[,-3] 
  #_adding genre, years, gconf, and metascores####
  indv1932<- merge(x = indv1932, y = bscs[ , c("tconst", "startYear", "genres")], all.x = T)#, by = "tconst", all.x=TRUE, fill=F
  #_adding gconf####
  indv1932<- merge(x = indv1932, y = gnrs[gnrs$tconst %in% w1932$tconst, c("tconst", "gConf")], all.x = TRUE)
  #_adding metascores####
  indv1932<- merge(x = indv1932, y = rtng1932[ , c("tconst", "metascore")], all.x = TRUE)
#k<-length(indv1932) # indv1932 <-indv1932[na.omit(indv1932[,1:as.numeric(k)])]
  indv1932 <- unique(indv1932)
  #!NO GENRE!####
  mssgnrs1932<-indv1932[indv1932$genres=="\\N",]
  mssgnrs1932<-rbind(mssgnrs1932, indv1932[is.na(indv1932$genres)==TRUE,])#indv1932<- indv1932[indv1932$genres!="\\N",]
  #indv1932<-link1932 %>% mutate_at(c('genres'), replace_na, '\\N')
  


length(unique(link1932[link1932$category %in%  c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst))
length(unique(indv1932$nconst))

#B-INDIVIDUAL ATTRIBUTES #### 
indv1932<-merge(aggregate(startYear ~ nconst, indv1932, paste, na.action=NULL),merge(
  aggregate(metascore ~ nconst, indv1932, function(x) mean(as.numeric(x), na.rm=TRUE), na.action=NULL),merge(#_average METASCORE ####
#!!!!!if one genre is NA gconf becomes NA  ######################                                                                                                             
    aggregate(gConf ~ nconst, indv1932, toString, na.action=na.omit),
    aggregate(genres ~ nconst, indv1932, toString, na.action=NULL)
    ))) #indv1932<-indv1932[,c(1,4:10,2,3)] for reordering columns
setnames(indv1932, "startYear", "year")
indv1932$startYear<-as.numeric(sapply(indv1932$year, min))#_startYear####
# sy1932<-indv1932[,c("nconst","sy")]
# indv1932<-indv1932[,!(names(indv1932)%in%c("sy"))]

indv1932$year <- vapply(indv1932$year, paste, collapse = ", ", character(1L)) # wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
indv1932$tenure<-1932-indv1932$startYear#_tenure####
indv1932$experience <- sapply(indv1932$year, function(x) str_count(x,"\\,"))#_experience####
indv1932$newcomer <- as.numeric(indv1932$experience == 0)#_newcomer####
indv1932$average_confusion<-sapply(strsplit(as.character(indv1932$gConf), ",", fixed=T), function(x) mean(as.numeric(x), na.rm=TRUE))#_average confusion####
indv1932$orientation <-sapply(strsplit(indv1932$genres,"\\, |\\,| " ), uniqueN)/(1+indv1932$experience)#!_so####
indv1932$year <- 1932 #_focal year####


#_role consolidation####
indv1932<-merge(indv1932,aggregate(n ~ nconst, dplyr::add_count(link1932, tconst, nconst), function(x) mean(as.numeric(x), na.rm=TRUE)), all.x = T)
setnames(indv1932, "n", "role_consolidation")


#_awards quality####
  #for all collaborators with titles in 1932 
  link1932<-merge(link1932,awrd,by.x='nconst', by.y='nm', all.x=TRUE) #nrow(na.omit(temp1932[,-4]))==nrow(temp1932) #nrow(na.omit(awrd))==nrow(awrd)
#!awards for missing nm in awrd replaced with zero####
  link1932<-link1932 %>% mutate_at(c('1932', '1931','1930','1929','1928','1995'), replace_na, 0)
  link1932[,'awards']<-link1932$`1932` #names(indv1932$`1932`)<-"awards"
  link1932[link1932$nconst=="nm7016360",c('nconst','awards')]
  link1932[is.na(link1932$awards)==TRUE,'awards']
  link1932[,'quality']<-link1932$`1931`+link1932$`1930`+link1932$`1929`+link1932$`1928`
indv1932<-unique(merge(indv1932,link1932[,c("nconst","awards","quality")],by='nconst', all.x=TRUE))




indv1932<-merge(indv1932,aggregate(budget ~ nconst, merge(link1932[,c("nconst","tconst")],rtng1932[,c("tconst","budget")], by="tconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst", all.x=T) #_budget####
indv1932<-merge(indv1932,aggregate(sequel ~ nconst, merge(link1932[,c("nconst","tconst")],rtng1932[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel####

#############################C-ALTER ATTRIBUTES####
#_alter quality####
tmp<-data.frame()
test<-data.frame()
#ind <-   which(sapply(link1932, is.numeric))
#for(j in ind){
  #set(link1932, i = which(is.na(link1932[[j]])), j = j, value = 0)}
for(nconst in names(V(delete.vertices(g, which(degree(g)==0))))){
  c<-neighbors(g, nconst, mode = "total")
  tmp<-rbind(tmp,data.frame(nconst, 
                            setNames(
                              aggregate(quality ~ tconst, 
                              link1932[link1932$nconst %in% str_trim(names(c)),], function(x) mean(as.numeric(x), na.rm=TRUE))
                              , c("tconst", "team_quality"))
                            )
             )} # V(tmp1)$name[c] 
indv1932<-merge(indv1932,aggregate(team_quality ~ nconst, tmp[tmp$tconst %in% rtng1932$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) 

#_team coreness####
link1932<-merge(x=link1932,y=dstr[,c("nconst","coreness")],by="nconst",all.x = TRUE)#[0:2,]
tmp<-data.frame()
for(nconst in link1932$nconst){
  c<-neighbors(g, nconst, mode = "total")
    if(nrow(link1932[link1932$nconst %in% names(c) & !is.na(link1932$coreness),])>0){
    tmp<-rbind(tmp,data.frame(nconst, na.omit(aggregate(coreness ~ tconst, link1932[link1932$nconst %in% str_trim(names(c)) & !is.na(link1932$coreness),], function(x) mean(as.numeric(x), na.rm=TRUE)))))}} # V(tmp1)$name[c] 
indv1932<-merge(indv1932,aggregate(coreness ~ nconst, tmp[tmp$tconst %in% rtng1932$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) # unique(link1932[link1932$nconst %in% names(c) & !link1932$category %in% c("actor","actress" ),]$category)# str(link1932[link1932$nconst %in% names(c) & !link1932$category %in% c("actor","actress" ),])
names(indv1932)<-gsub(x = names(indv1932), pattern = "coreness", replacement = "team_coreness") 

# #_role dummies####
# roles<-unique(temp1932$category)
# roles<-roles[!(roles %in% c("archive_footage", "self"))]
# indv1932<-merge(indv1932,na.omit(aggregate(category ~ nconst, temp1932[temp1932$tconst %in% rtng1932$tconst], toString)), by="nconst", all.x=T)
# for (i in na.omit(roles)) {
#   Variable=i;
#   print(Variable)
#   indv1932[, Variable] <- dum(i, indv1932$category)
# }

# __________________________________________________________________________________________####  
#############################MERGING DATA####

indv1932<-merge(indv1932, dstr, by = "nconst", all.x = TRUE)# wrt<-merge(wrt, wstr, by = "nconst", all.x=T)
#indv1932<-merge(indv1932, sdstr, by = "nconst", all.x=T)# wrt<-merge(wrt, swstr, by = "nconst", all.x=T)
#names(indv1932)<-gsub(x = names(indv1932), pattern = "\\.x", replacement = "") 
#names(indv1932)<-gsub(x = names(indv1932), pattern = "\\.y", replacement = "_smp") 

# __________________________________________________________________________________________####  
#############################FOCUS####
#_focus on genres####
for(i in c(na.omit(xx))){if(i!="\\N"){indv1932[,i]<-str_count(indv1932$genres, i)}}

indv1932[,c("focus_genres")]<-  #,"focus_genres_ent"
  hhi(indv1932[,(length(indv1932)-27):length(indv1932)])
#t<-merge(twrt, tdir, all.x=T, all.y = T)

#_focus on roles####
temp1932<-aggregate(category ~ nconst, temp1932, toString)
temp1932<-temp1932[temp1932$nconst  %in%	indv1932$nconst, 1:2]
temp1932$actress<-str_count(temp1932$category,             	"actress"            	)
temp1932$actor<-str_count(temp1932$category,               	"actor"              	)
temp1932$director<-str_count(temp1932$category,            	"director"           	)
temp1932$producer<-str_count(temp1932$category,            	"producer"           	)
temp1932$writer<-str_count(temp1932$category,              	"writer"             	)
temp1932$cinematographer<-str_count(temp1932$category,     	"cinematographer"    	)
temp1932$composer<-str_count(temp1932$category,            	"composer"           	)
temp1932$production_designer<-str_count(temp1932$category, 	"production_designer"	)
temp1932$editor<-str_count(temp1932$category,              	"editor"             	)
indv1932[,c("focus_roles","focus_roles_ent")]<-hhi(temp1932[,3:11])

link1932  <-merge(x = link1932, y = gnrs[, c("tconst", "genres")], by = "tconst", all.x=TRUE)
for(i in c(na.omit(xx))){if(i!="\\N"){link1932[,i]<-str_count(link1932$genres, i)}}
indv1932  <- indv1932[,-which(names(indv1932)%in%xx)]
for(i in c(na.omit(xx))){if(i!="\\N"){indv1932  <- merge(x = indv1932, y = 
                     aggregate(as.formula(paste0(i, "~ nconst")), link1932, sum)
                     , by = "nconst", all.x=TRUE)}}

# __________________________________________________________________________________________####  
#############################TITLE LEVEL MODEL####
#A-TITLE ATTRIBUTES####

#!_average_confusion(!try below comment!)####
#rtng1932$average_confusion  <-aggregate(average_confusion ~ tconst, merge(link1932[,c("nconst","tconst")], indv1932, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$average_confusion #link1932$category %in% c("director")
rtng1932                    <-merge(x = rtng1932, y = aggregate(average_confusion ~ tconst, merge(link1932[,c("nconst","tconst")], indv1932, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link1932$category %in% c("director")
#_experience####
rtng1932                    <-merge(x = rtng1932, y = aggregate(experience ~ tconst, merge(link1932[,c("nconst","tconst")], indv1932, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link1932$category %in% c("director")
#_genre_confusion####
rtng1932                    <-merge(x = rtng1932, y = gnrs[, c("tconst", "gConf")], by = "tconst", all.x=TRUE)
#_so####
rtng1932                    <-merge(x = rtng1932, y = aggregate(orientation ~ tconst, merge(link1932[,c("nconst","tconst")], indv1932, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_role_consolidation####
rtng1932                    <-merge(x = rtng1932, y = aggregate(role_consolidation ~ tconst, merge(link1932[,c("nconst","tconst")], indv1932, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_awards####
rtng1932                    <-merge(x = rtng1932, y = aggregate(awards ~ tconst, merge(link1932[,c("nconst","tconst")],indv1932, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) 
#_quality####
rtng1932                    <-merge(x = rtng1932, y = aggregate(quality ~ tconst, link1932, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)
#_focus on roles####
rtng1932                    <-merge(x = rtng1932, y = aggregate(focus_genres ~ tconst, merge(link1932[,c("nconst","tconst")],indv1932, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_focus on genres####
rtng1932                    <-merge(x = rtng1932, y = aggregate(focus_roles ~ tconst, merge(link1932[,c("nconst","tconst")],indv1932, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_brokerage####
rtng1932                    <-merge(x = rtng1932, y = aggregate(brokerage ~ tconst, merge(link1932[,c("nconst","tconst")],indv1932, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)#rtng1932$brokerage_smp       <-na.omit(aggregate(brokerage_smp ~ tconst, merge(link1932[,c("nconst","tconst")],indv1932, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$brokerage_smp)
#_coreness####
rtng1932                    <-merge(x = rtng1932, y = aggregate(coreness ~ tconst, link1932, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)#setNames(aggregate(coreness ~ tconst, link1932, function(x) mean(as.numeric(x), na.rm=TRUE)), c("tconst", "team_coreness"))$team_coreness
#!newcomer# rtng1932$newcomer<-NA # for (tconst in merge(link1932[!,c("nconst","tconst")])){#   rtng1932[rtng1932$tconst=="rtng1932", "newcomer"]<-#   ifelse(aggregate(newcomer ~ tconst, merge(link1932[!,c("nconst","tconst")], indv1932, by="nconst", all.x=T)$newcomer, function(x) mean(as.numeric(x), na.rm=TRUE)) #          ==1#          , 1, NA)}####

colSums(is.na(link1932))
colSums(is.na(indv1932))
colSums(is.na(rtng1932))

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
# #sdts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")1930
# 
# sdstr<- data.frame(nconst=names(sdcn),hubs_score=sdhs, page_rank=sdpr, brokerage=sdcn, coreness=sdcr, eigenvector_centrality=sdev)
# nrow(complete.cases(sdstr))
# 
# sapply(indv1932, function(y) sum(length(which(is.na(y)))))
# nrow(indv1932)
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
#                                         )), cols=director)) #'"Error in validObject(.Object) : invalid class "dgTMatrix" object:all row indices (slot 'i') must be between 0 and nrow1930 in a TsparseMatrix"

#graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
#OR
# g <- link1932[,1:2] %*% t(link1932[,1:2]) %>%
#   graph_from_adjacency_matrix(mode = "undirected", diag = FALSE, weighted = TRUE)







# #manual construction of their network####
# tmp<-na.omit(aggregate(nconst ~ tconst, link1932, toString))# tmp$edges<-sapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], function(x) combn(x, 2, FUN = NULL, simplify = F))# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
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
# unique(unique(link1932$nconst) %in% unique(names(V(g))))
# "nm0413541" %in% link1932[which(link1932$nconst %in% unique(V(g)$name))]$nconst
# "nm0413541" %in% V(g)$name
# "nm0413541" %in% link1932$nconst
# 
# "nm0000436" %in% link1932$nconst
# "nm0000436" %in% V(g)$name
# "nm0000436" %in% link1932[which(link1932$nconst %in% unique(V(g)$name))]$nconst
# 
# neighbors(g, "nm0413541", mode = "total")
# 
# 
# g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

