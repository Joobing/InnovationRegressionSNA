# __________________________________________________________________________________________####  
#############################RESAMPLE1961 ####
if(FALSE) {"
1- 1961 (number of) titles
2- relevant (present and past) affiliations to 1961
  # Crew of titles of major-STUDIOs in 1961
  # Titles that share CREW with titles of major-STUDIOs in 1961
  # Crew of aLL titles that share CREW with titles of major-STUDIOs in 1961
  # Up to 1961
3- THREE YEAR window for 1961
4- dropping animation and documentary
1958, 1959, 1960, 1961, 2001, 2002
  "}

rtng1961<-rtng[rtng$startYear == 1961,]
#rtng1961<-rtng1961[complete.cases(rtng1961[,1]),]
rtng1961<-unique(rtng1961)
rtng1961=rtng1961[!sum(is.na(rtng1961))==ncol(rtng1961),]


n1961<-nrow(rtng1961)

temp1961<-unique(prnc[prnc$tconst  %in%	rtng1961$tconst, "nconst"])#
temp1961<-prnc[prnc$nconst  %in%	temp1961$nconst, "tconst"]#
temp1961<-unique(prnc[prnc$tconst  %in%	temp1961$tconst,])#
temp1961<-temp1961[temp1961$tconst %in% bscs[bscs$startYear <= 1961,]$tconst,]# #temp1961<-merge(temp1961,bscs[,c("tconst","startYear")], by="tconst", all.x = T)# temp1961<-merge(temp1961, bscs[,c("tconst", "startYear")], by="tconst", all.x = TRUE)


temp1961<-temp1961[!(temp1961$tconst %in% animentry),]
rtng1961<-rtng1961[!(rtng1961$tconst %in% animentry),]


w1961 <- c(1961-4, 1961-3, 1961-2 ,1961-1, 1961)#library(plyr)#,1961-3,1961-4,1961-5,1961-6
w1961 <-unique(bscs[bscs$startYear %in% w1961,"tconst"])


# __________________________________________________________________________________________####  
#############################NETWORK of COLLABORATION####
#A-SEPARATING PROFESSIONALS BY ROLE####
tmp <- temp1961[(temp1961$tconst %in% w1961$tconst) & (temp1961$nconst %in% temp1961[temp1961$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"),]$nconst) , ]
tmp <- tmp[complete.cases(tmp[,c("tconst","nconst")]),]

#B-GRAPH FROM EDGE-LIST#### 
g<-          graph_from_data_frame(tmp, directed=FALSE)
g<-          simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-  V(g)$name %in% unique(tmp$nconst)
PROJECTION<- bipartite.projection(g) #two-mode
tnet<-       PROJECTION[[1]] #one-mode
g<-          PROJECTION[[2]] #one-mode
g<-          set.edge.attribute(g, "weight", E(g), 1)
cmpnt1961 <- no.clusters(g);           # Number of components
larg1961 <- sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1]; # Size of largest component
rlarg1961 <- (sort(clusters(g)$csize, decreasing=TRUE, na.last=TRUE)[1])/vcount(g); # Relative size of largest component

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

#cast and crew of titles from major-studios in 1961 
link1961<-temp1961[temp1961$tconst %in% rtng1961$tconst,] #as_tibble(temp1961[temp1961$tconst %in% rtng1961$tconst,1:3])

g<- graph_from_data_frame(link1961[,1:2], directed=FALSE)
g<-simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-    V(g)$name %in% unique(link1961$nconst)
PROJECTION<-bipartite.projection(g) #two-mode
tnet<-PROJECTION[[1]] #one-mode
g<-PROJECTION[[2]]#one-mode
g <- set.edge.attribute(g, "weight", E(g), 1)


#A-CREW with titles from major-studios in 1961#### 
#!!!when individual performs multiple roles in a movie, that movie is entering multiple times in the calculations for that individual!!!####
indv1961 <- temp1961[temp1961$nconst %in% link1961[link1961$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst,c("tconst", "nconst","category")]
  #indv1961 <- indv1961[indv1961$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,] #& temp1961$category=='director' #[,-3] 
  #_adding genre, years, gconf, and metascores####
  indv1961<- merge(x = indv1961, y = bscs[ , c("tconst", "startYear", "genres")], all.x = T)#, by = "tconst", all.x=TRUE, fill=F
  #_adding gconf####
  indv1961<- merge(x = indv1961, y = gnrs[gnrs$tconst %in% w1961$tconst, c("tconst", "gConf")], all.x = TRUE)
  #_adding metascores####
  indv1961<- merge(x = indv1961, y = rtng1961[ , c("tconst", "metascore")], all.x = TRUE)
#k<-length(indv1961) # indv1961 <-indv1961[na.omit(indv1961[,1:as.numeric(k)])]
  indv1961 <- unique(indv1961)
  #!NO GENRE!####
  mssgnrs1961<-indv1961[indv1961$genres=="\\N",]
  mssgnrs1961<-rbind(mssgnrs1961, indv1961[is.na(indv1961$genres)==TRUE,])#indv1961<- indv1961[indv1961$genres!="\\N",]
  #indv1961<-link1961 %>% mutate_at(c('genres'), replace_na, '\\N')
  


length(unique(link1961[link1961$category %in%  c("director", "cinematographer", "editor", "writer", "production_designer", "composer") ,]$nconst))
length(unique(indv1961$nconst))

#B-INDIVIDUAL ATTRIBUTES #### 
indv1961<-merge(aggregate(startYear ~ nconst, indv1961, paste, na.action=NULL),merge(
  aggregate(metascore ~ nconst, indv1961, function(x) mean(as.numeric(x), na.rm=TRUE), na.action=NULL),merge(#_average METASCORE ####
#!!!!!if one genre is NA gconf becomes NA  ######################                                                                                                             
    aggregate(gConf ~ nconst, indv1961, toString, na.action=na.omit),
    aggregate(genres ~ nconst, indv1961, toString, na.action=NULL)
    ))) #indv1961<-indv1961[,c(1,4:10,2,3)] for reordering columns
setnames(indv1961, "startYear", "year")
indv1961$startYear<-as.numeric(sapply(indv1961$year, min))#_startYear####
# sy1961<-indv1961[,c("nconst","sy")]
# indv1961<-indv1961[,!(names(indv1961)%in%c("sy"))]

indv1961$year <- vapply(indv1961$year, paste, collapse = ", ", character(1L)) # wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
indv1961$tenure<-1961-indv1961$startYear#_tenure####
indv1961$experience <- sapply(indv1961$year, function(x) str_count(x,"\\,"))#_experience####
indv1961$newcomer <- as.numeric(indv1961$experience == 0)#_newcomer####
indv1961$average_confusion<-sapply(strsplit(as.character(indv1961$gConf), ",", fixed=T), function(x) mean(as.numeric(x), na.rm=TRUE))#_average confusion####
indv1961$orientation <-sapply(strsplit(indv1961$genres,"\\, |\\,| " ), uniqueN)/(1+indv1961$experience)#!_so####
indv1961$year <- 1961 #_focal year####


#_role consolidation####
indv1961<-merge(indv1961,aggregate(n ~ nconst, dplyr::add_count(link1961, tconst, nconst), function(x) mean(as.numeric(x), na.rm=TRUE)), all.x = T)
setnames(indv1961, "n", "role_consolidation")


#_awards quality####
  #for all collaborators with titles in 1961 
  link1961<-merge(link1961,awrd,by.x='nconst', by.y='nm', all.x=TRUE) #nrow(na.omit(temp1961[,-4]))==nrow(temp1961) #nrow(na.omit(awrd))==nrow(awrd)
#!awards for missing nm in awrd replaced with zero####
  link1961<-link1961 %>% mutate_at(c('1961', '1960','1959','1958','1957','1995'), replace_na, 0)
  link1961[,'awards']<-link1961$`1961` #names(indv1961$`1961`)<-"awards"
  link1961[link1961$nconst=="nm7016360",c('nconst','awards')]
  link1961[is.na(link1961$awards)==TRUE,'awards']
  link1961[,'quality']<-link1961$`1960`+link1961$`1959`+link1961$`1958`+link1961$`1957`
indv1961<-unique(merge(indv1961,link1961[,c("nconst","awards","quality")],by='nconst', all.x=TRUE))




indv1961<-merge(indv1961,aggregate(budget ~ nconst, merge(link1961[,c("nconst","tconst")],rtng1961[,c("tconst","budget")], by="tconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst", all.x=T) #_budget####
indv1961<-merge(indv1961,aggregate(sequel ~ nconst, merge(link1961[,c("nconst","tconst")],rtng1961[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel####

#############################C-ALTER ATTRIBUTES####
#_alter quality####
tmp<-data.frame()
test<-data.frame()
#ind <-   which(sapply(link1961, is.numeric))
#for(j in ind){
  #set(link1961, i = which(is.na(link1961[[j]])), j = j, value = 0)}
for(nconst in names(V(delete.vertices(g, which(degree(g)==0))))){
  c<-neighbors(g, nconst, mode = "total")
  tmp<-rbind(tmp,data.frame(nconst, 
                            setNames(
                              aggregate(quality ~ tconst, 
                              link1961[link1961$nconst %in% str_trim(names(c)),], function(x) mean(as.numeric(x), na.rm=TRUE))
                              , c("tconst", "team_quality"))
                            )
             )} # V(tmp1)$name[c] 
indv1961<-merge(indv1961,aggregate(team_quality ~ nconst, tmp[tmp$tconst %in% rtng1961$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) 

#_team coreness####
link1961<-merge(x=link1961,y=dstr[,c("nconst","coreness")],by="nconst",all.x = TRUE)#[0:2,]
tmp<-data.frame()
for(nconst in link1961$nconst){
  c<-neighbors(g, nconst, mode = "total")
    if(nrow(link1961[link1961$nconst %in% names(c) & !is.na(link1961$coreness),])>0){
    tmp<-rbind(tmp,data.frame(nconst, na.omit(aggregate(coreness ~ tconst, link1961[link1961$nconst %in% str_trim(names(c)) & !is.na(link1961$coreness),], function(x) mean(as.numeric(x), na.rm=TRUE)))))}} # V(tmp1)$name[c] 
indv1961<-merge(indv1961,aggregate(coreness ~ nconst, tmp[tmp$tconst %in% rtng1961$tconst,], function(x) mean(as.numeric(x), na.rm=TRUE)),by="nconst",all.x=TRUE) # unique(link1961[link1961$nconst %in% names(c) & !link1961$category %in% c("actor","actress" ),]$category)# str(link1961[link1961$nconst %in% names(c) & !link1961$category %in% c("actor","actress" ),])
names(indv1961)<-gsub(x = names(indv1961), pattern = "coreness", replacement = "team_coreness") 

# #_role dummies####
# roles<-unique(temp1961$category)
# roles<-roles[!(roles %in% c("archive_footage", "self"))]
# indv1961<-merge(indv1961,na.omit(aggregate(category ~ nconst, temp1961[temp1961$tconst %in% rtng1961$tconst], toString)), by="nconst", all.x=T)
# for (i in na.omit(roles)) {
#   Variable=i;
#   print(Variable)
#   indv1961[, Variable] <- dum(i, indv1961$category)
# }

# __________________________________________________________________________________________####  
#############################MERGING DATA####

indv1961<-merge(indv1961, dstr, by = "nconst", all.x = TRUE)# wrt<-merge(wrt, wstr, by = "nconst", all.x=T)
#indv1961<-merge(indv1961, sdstr, by = "nconst", all.x=T)# wrt<-merge(wrt, swstr, by = "nconst", all.x=T)
#names(indv1961)<-gsub(x = names(indv1961), pattern = "\\.x", replacement = "") 
#names(indv1961)<-gsub(x = names(indv1961), pattern = "\\.y", replacement = "_smp") 

# __________________________________________________________________________________________####  
#############################FOCUS####
#_focus on genres####
for(i in c(na.omit(xx))){if(i!="\\N"){indv1961[,i]<-str_count(indv1961$genres, i)}}

indv1961[,c("focus_genres")]<-  #,"focus_genres_ent"
  hhi(indv1961[,(length(indv1961)-27):length(indv1961)])
#t<-merge(twrt, tdir, all.x=T, all.y = T)

#_focus on roles####
temp1961<-aggregate(category ~ nconst, temp1961, toString)
temp1961<-temp1961[temp1961$nconst  %in%	indv1961$nconst, 1:2]
temp1961$actress<-str_count(temp1961$category,             	"actress"            	)
temp1961$actor<-str_count(temp1961$category,               	"actor"              	)
temp1961$director<-str_count(temp1961$category,            	"director"           	)
temp1961$producer<-str_count(temp1961$category,            	"producer"           	)
temp1961$writer<-str_count(temp1961$category,              	"writer"             	)
temp1961$cinematographer<-str_count(temp1961$category,     	"cinematographer"    	)
temp1961$composer<-str_count(temp1961$category,            	"composer"           	)
temp1961$production_designer<-str_count(temp1961$category, 	"production_designer"	)
temp1961$editor<-str_count(temp1961$category,              	"editor"             	)
indv1961[,c("focus_roles","focus_roles_ent")]<-hhi(temp1961[,3:11])

link1961  <-merge(x = link1961, y = gnrs[, c("tconst", "genres")], by = "tconst", all.x=TRUE)
for(i in c(na.omit(xx))){if(i!="\\N"){link1961[,i]<-str_count(link1961$genres, i)}}
indv1961  <- indv1961[,-which(names(indv1961)%in%xx)]
for(i in c(na.omit(xx))){if(i!="\\N"){indv1961  <- merge(x = indv1961, y = 
                     aggregate(as.formula(paste0(i, "~ nconst")), link1961, sum)
                     , by = "nconst", all.x=TRUE)}}

# __________________________________________________________________________________________####  
#############################TITLE LEVEL MODEL####
#A-TITLE ATTRIBUTES####

#!_average_confusion(!try below comment!)####
#rtng1961$average_confusion  <-aggregate(average_confusion ~ tconst, merge(link1961[,c("nconst","tconst")], indv1961, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$average_confusion #link1961$category %in% c("director")
rtng1961                    <-merge(x = rtng1961, y = aggregate(average_confusion ~ tconst, merge(link1961[,c("nconst","tconst")], indv1961, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link1961$category %in% c("director")
#_experience####
rtng1961                    <-merge(x = rtng1961, y = aggregate(experience ~ tconst, merge(link1961[,c("nconst","tconst")], indv1961, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) #link1961$category %in% c("director")
#_genre_confusion####
rtng1961                    <-merge(x = rtng1961, y = gnrs[, c("tconst", "gConf")], by = "tconst", all.x=TRUE)
#_so####
rtng1961                    <-merge(x = rtng1961, y = aggregate(orientation ~ tconst, merge(link1961[,c("nconst","tconst")], indv1961, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_role_consolidation####
rtng1961                    <-merge(x = rtng1961, y = aggregate(role_consolidation ~ tconst, merge(link1961[,c("nconst","tconst")], indv1961, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_awards####
rtng1961                    <-merge(x = rtng1961, y = aggregate(awards ~ tconst, merge(link1961[,c("nconst","tconst")],indv1961, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE) 
#_quality####
rtng1961                    <-merge(x = rtng1961, y = aggregate(quality ~ tconst, link1961, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)
#_focus on roles####
rtng1961                    <-merge(x = rtng1961, y = aggregate(focus_genres ~ tconst, merge(link1961[,c("nconst","tconst")],indv1961, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_focus on genres####
rtng1961                    <-merge(x = rtng1961, y = aggregate(focus_roles ~ tconst, merge(link1961[,c("nconst","tconst")],indv1961, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)
#_brokerage####
rtng1961                    <-merge(x = rtng1961, y = aggregate(brokerage ~ tconst, merge(link1961[,c("nconst","tconst")],indv1961, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE)), by = "tconst", all.x=TRUE)#rtng1961$brokerage_smp       <-na.omit(aggregate(brokerage_smp ~ tconst, merge(link1961[,c("nconst","tconst")],indv1961, by="nconst", all.x=T), function(x) mean(as.numeric(x), na.rm=TRUE))$brokerage_smp)
#_coreness####
rtng1961                    <-merge(x = rtng1961, y = aggregate(coreness ~ tconst, link1961, function(x) mean(as.numeric(x), na.rm=TRUE)), all.x=TRUE)#setNames(aggregate(coreness ~ tconst, link1961, function(x) mean(as.numeric(x), na.rm=TRUE)), c("tconst", "team_coreness"))$team_coreness
#!newcomer# rtng1961$newcomer<-NA # for (tconst in merge(link1961[!,c("nconst","tconst")])){#   rtng1961[rtng1961$tconst=="rtng1961", "newcomer"]<-#   ifelse(aggregate(newcomer ~ tconst, merge(link1961[!,c("nconst","tconst")], indv1961, by="nconst", all.x=T)$newcomer, function(x) mean(as.numeric(x), na.rm=TRUE)) #          ==1#          , 1, NA)}####

colSums(is.na(link1961))
colSums(is.na(indv1961))
colSums(is.na(rtng1961))

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
# #sdts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")1959
# 
# sdstr<- data.frame(nconst=names(sdcn),hubs_score=sdhs, page_rank=sdpr, brokerage=sdcn, coreness=sdcr, eigenvector_centrality=sdev)
# nrow(complete.cases(sdstr))
# 
# sapply(indv1961, function(y) sum(length(which(is.na(y)))))
# nrow(indv1961)
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
#                                         )), cols=director)) #'"Error in validObject(.Object) : invalid class "dgTMatrix" object:all row indices (slot 'i') must be between 0 and nrow1959 in a TsparseMatrix"

#graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
#OR
# g <- link1961[,1:2] %*% t(link1961[,1:2]) %>%
#   graph_from_adjacency_matrix(mode = "undirected", diag = FALSE, weighted = TRUE)







# #manual construction of their network####
# tmp<-na.omit(aggregate(nconst ~ tconst, link1961, toString))# tmp$edges<-sapply(tmp[!is.na(tmp["V2"]),3:length(tmp)], function(x) combn(x, 2, FUN = NULL, simplify = F))# edges<-data.frame(rbindlist(combn(janitor::remove_empty(tmp[1,3:length(tmp)]), 2, FUN = NULL, simplify = F)))
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
# unique(unique(link1961$nconst) %in% unique(names(V(g))))
# "nm0413541" %in% link1961[which(link1961$nconst %in% unique(V(g)$name))]$nconst
# "nm0413541" %in% V(g)$name
# "nm0413541" %in% link1961$nconst
# 
# "nm0000436" %in% link1961$nconst
# "nm0000436" %in% V(g)$name
# "nm0000436" %in% link1961[which(link1961$nconst %in% unique(V(g)$name))]$nconst
# 
# neighbors(g, "nm0413541", mode = "total")
# 
# 
# g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

