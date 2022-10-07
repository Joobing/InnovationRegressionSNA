
#https://stackoverflow.com/questions/48822710/merge-edges-by-attribute-group-in-igraph-r
#https://www.google.nl/search?biw=1239&bih=621&ei=Ot5ZW_X3L83TkwX176PwDw&q=merge+edges+based+on+vertex+igraph&oq=merge+edges+based+on+vertex+igraph&gs_l=psy-ab.3...39470.44521.0.44882.27.27.0.0.0.0.95.1777.27.27.0....0...1.1.64.psy-ab..0.12.818...0i7i30k1j0i7i10i30k1j0i13k1j0i8i7i30k1j0i30k1j0i5i30k1j0i8i30k1j0i8i13i30k1j35i39k1.0.X-cE_4yiOGo

# __________________________________________________________________________________________####  
#############################RESAMPLE2000 ####
rtng2000<-rtng[rtng$startYear == 2000,]
  rtng2000<-rtng2000[complete.cases(rtng2000[,1:3]),] #rtng2000$genreconfusion <-  sapply (if (rtng2000$genres != "\\N"){ strsplit((rtng2000$genres',' ), uniqueN)})#gc()
  rtng2000<-unique(rtng2000)
  n2000<-nrow(rtng2000)

#CREW of ALL titles that share CREW with titles of major-STUDIOs in 2000
temp2000<-unique(prnc[prnc$tconst  %in%	rtng2000$tconst, "nconst"])#CREW of titles of major-STUDIOs in 2000
temp2000<-prnc[prnc$nconst  %in%	temp2000$nconst, "tconst"]#titles that share CREW with titles of major-STUDIOs in 2000
temp2000<-unique(prnc[prnc$tconst  %in%	temp2000$tconst,])##CREW of ALL titles that share CREW with titles of major-STUDIOs in 2000
#CREW of ALL titles up-to 2000 that share CREW with titles of major-STUDIOs in 2000
temp2000<-temp2000[temp2000$tconst %in% bscs[bscs$startYear <= 2000,]$tconst,] #temp2000<-merge(temp2000,bscs[,c("tconst","startYear")], by="tconst", all.x = T)# temp2000<-merge(temp2000, bscs[,c("tconst", "startYear")], by="tconst", all.x = TRUE)

#CREATING 5 YEAR WINDOW FOR 2000
w2000 <- c(2000-2,2000-1,2000)#library(plyr)#,2000-3,2000-4,2000-5,2000-6
w2000 <-unique(bscs[bscs$startYear %in% w2000,"tconst"])

# __________________________________________________________________________________________####  
#############################COLLABORATION NETWORK of the crew####
#A-SEPARATING PROFESSIONALS BY ROLE####
tmp <- temp2000[(temp2000$tconst %in% w2000$tconst) & (temp2000$category %in% c("director", "cinematographer", "editor", "producer", "writer", "production_designer", "composer")), c("tconst","nconst","category", "job")]
tmp <- tmp[complete.cases(tmp[,c("tconst","nconst")]),]
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
#                                         )), cols=director)) #'"Error in validObject(.Object) : invalid class "dgTMatrix" object:all row indices (slot 'i') must be between 0 and nrow-1 in a TsparseMatrix"

#graph_from_adjacency_matrix(net, mode = "undirected", weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NULL)
#OR
# g <- link2000[,1:2] %*% t(link2000[,1:2]) %>%
#   graph_from_adjacency_matrix(mode = "undirected", diag = FALSE, weighted = TRUE)






#B-GRAPH FROM EDGE-LIST#### 
g<-          graph_from_data_frame(tmp, directed=FALSE)
g<-          simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
V(g)$type<-  V(g)$name %in% unique(tmp$nconst)
PROJECTION<- bipartite.projection(g) #two-mode
tnet<-       PROJECTION[[1]] #one-mode
g<-          PROJECTION[[2]] #one-mode
g <-         set.edge.attribute(g, "weight", E(g), 1)

#C-CALCULATING STRUCTUTAL ATTRIBUTES####
dcr <-coreness(g, mode = c("all")) #_coreness####
dhs <-hub_score(g)$vector#_hubs####
arpack_defaults$maxiter = 1000000000
dev <-as.data.frame(eigen_centrality(g, options=arpack_defaults, directed=F)) #_eigenvector####
dev <-dev[,1]
dts <-neighborhood.size(g, nodes = V(g), 2, mode="all")-1 #_component size####
dcn <-1 - constraint(g, nodes = V(g)) #_brokerage####
dpr <-page_rank(g, algo = c("prpack", "arpack", "power"), v=V(g),directed = FALSE) #_pagerank####
dpr<-dpr$vector #ev <- arpack(closeness(g), options=list(n=3, nev=2, ncv=3, which="LM", maxiter=200), sym=TRUE, complex = FALSE)

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


#A-CREW with titles from major-studios in 2000#### 
indv2000 <- temp2000[temp2000$nconst %in% link2000$nconst,c("tconst", "nconst","category")]
  indv2000 <- indv2000[!indv2000$category %in% c("actor","actress") ,] #& temp2000$category=='director' #[,-3] 
  #_adding genre, years, and metascores####
  indv2000<- merge(x = indv2000, y = bscs[ , c("tconst", "startYear", "genres")], all.x = T)#, by = "tconst", all.x=TRUE, fill=F
  indv2000<- merge(x = indv2000, y = gnrs[gnrs$tconst %in% w2000$tconst, c("tconst", "gConf")], all.x = TRUE)
  indv2000<- merge(x = indv2000, y = rtng[ , c("tconst", "metascore")], all.x = TRUE)
#k<-length(indv2000) # indv2000 <-indv2000[na.omit(indv2000[,1:as.numeric(k)])]
indv2000 <- unique(indv2000)
  #!NO GENRE!####
  mssgnrs2000<-indv2000[indv2000$genres=="\\N",]
  mssgnrs2000<-rbind(mssgnrs2000, indv2000[is.na(indv2000$genres)==TRUE,])#indv2000<- indv2000[indv2000$genres!="\\N",]

#B-INDIVIDUAL ATTRIBUTES #### 
indv2000<-merge(na.omit(aggregate(startYear ~ nconst, indv2000, paste)),merge(
  aggregate(metascore ~ nconst, indv2000, mean),merge(#_average METASCORE ####
    na.omit(aggregate(gConf ~ nconst, indv2000, toString)),
    na.omit(aggregate(genres ~ nconst, indv2000, toString))
    ))) #indv2000<-indv2000[,c(1,4:10,2,3)] for reordering columns


indv2000$tenure<-2000-as.numeric(sapply(indv2000$startYear, min))#_tenure####
indv2000$startYear <- vapply(indv2000$startYear, paste, collapse = ", ", character(1L)) # wrt$startYear <- vapply(wrt$startYear, paste, collapse = ", ", character(1L))
indv2000$experience <- sapply(indv2000$startYear, uniqueN)#_experience####
indv2000$newcomer <- as.numeric(indv2000$experience == 1)#_newcomer####
indv2000$average_confusion<-sapply(strsplit(as.character(indv2000$gConf), ",", fixed=T), function(x) mean(as.numeric(x)))#_average confusion####
indv2000$orientation <-sapply(strsplit(indv2000$genres,"\\, |\\,| " ), uniqueN)/indv2000$experience#_so####

#_focus on genres####
for(i in na.omit(xx)){indv2000[,i]<-str_count(indv2000$genres, i)}
indv2000[,c("focus_genres","focus_genres_ent")]<-hhi(indv2000[,(length(indv2000)-24):length(indv2000)])#t<-merge(twrt, tdir, all.x=T, all.y = T)

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

#_role consolidation####
indv2000<-merge(indv2000,aggregate(n ~ nconst, dplyr::add_count(link2000, tconst, nconst), mean), all.x = T)
setnames(indv2000, "n", "role_consolidation")

#_awards####
  #for all collaborators with titles in 2000 
  link2000<-merge(link2000,awrd,by.x='nconst', by.y='nm', all.x=TRUE) #nrow(na.omit(temp2000[,-4]))==nrow(temp2000) #nrow(na.omit(awrd))==nrow(awrd)
  link2000[,'awards']<-link2000$`2000` #names(indv2000$`2000`)<-"awards"
  link2000[,'quality']<-link2000$`1999`+link2000$`1998`
indv2000<-unique(merge(indv2000,link2000[,c("nconst","awards")],by='nconst', all.x=TRUE))

indv2000<-merge(indv2000,aggregate(budget ~ nconst, merge(link2000[,c("nconst","tconst")],rtng2000[,c("tconst","budget")], by="tconst", all.x=T), mean),by="nconst", all.x=T) #_budget####
indv2000<-merge(indv2000,aggregate(sequel ~ nconst, merge(link2000[,c("nconst","tconst")],rtng2000[,c("tconst","sequel")], by="tconst", all.x=T), sum),by="nconst" , all.x=T) #_sequel####

#############################C-TEAM ATTRIBUTES####
#_team quality####
tmp<-data.frame()
test<-data.frame()
for(nconst in link2000$nconst){
  c<-neighbors(g, nconst, mode = "total")
  tmp<-rbind(tmp,data.frame(nconst, na.omit(aggregate(quality ~ tconst, link2000[link2000$nconst %in% str_trim(names(c)),], mean))))} # V(tmp1)$name[c] 
indv2000<-merge(indv2000,aggregate(quality ~ nconst, tmp[tmp$tconst %in% rtng2000$tconst,], mean),by="nconst",all.x=TRUE) 

#_team coreness####
link2000<-merge(x=link2000,y=dstr[,c("nconst","coreness")],by="nconst",all.x = TRUE)#[0:2,]
tmp<-data.frame()
for(nconst in link2000$nconst){
  c<-neighbors(g, nconst, mode = "total")
    if(nrow(link2000[link2000$nconst %in% names(c) & !is.na(link2000$coreness),])>0){
    tmp<-rbind(tmp,data.frame(nconst, na.omit(aggregate(coreness ~ tconst, link2000[link2000$nconst %in% str_trim(names(c)) & !is.na(link2000$coreness),], mean))))}} # V(tmp1)$name[c] 
indv2000<-merge(indv2000,aggregate(coreness ~ nconst, tmp[tmp$tconst %in% rtng2000$tconst,], mean),by="nconst",all.x=TRUE) # unique(link2000[link2000$nconst %in% names(c) & !link2000$category %in% c("actor","actress" ),]$category)# str(link2000[link2000$nconst %in% names(c) & !link2000$category %in% c("actor","actress" ),])
names(indv2000)<-gsub(x = names(indv2000), pattern = "coreness", replacement = "team_coreness") 

# __________________________________________________________________________________________####  
#############################MERGING DATA####

indv2000<-merge(indv2000, dstr, by = "nconst", all.x = TRUE)# wrt<-merge(wrt, wstr, by = "nconst", all.x=T)
#indv2000<-merge(indv2000, sdstr, by = "nconst", all.x=T)# wrt<-merge(wrt, swstr, by = "nconst", all.x=T)
names(indv2000)<-gsub(x = names(indv2000), pattern = "\\.x", replacement = "") 
names(indv2000)<-gsub(x = names(indv2000), pattern = "\\.y", replacement = "_smp") 

# __________________________________________________________________________________________####  
#############################TITLE LEVEL MODEL####
#A-TITLE ATTRIBUTES####

rtng2000$average_confusion   <-aggregate(average_confusion ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")], indv2000, by="nconst", all.x=T), mean)$average_confusion #_average_confusion####

rtng2000$experience   <-aggregate(experience ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")], indv2000, by="nconst", all.x=T), mean)$experience #_experience####
rtng2000    <-merge(x = rtng2000, y = gnrs[, c("tconst", "gConf")], by = "tconst", all.x=TRUE)#_genre_confusion####
rtng2000$orientation  <-aggregate(orientation ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")], indv2000, by="nconst", all.x=T), mean)$orientation #_so####

rtng2000$role_consolidation <-aggregate(role_consolidation ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")], indv2000, by="nconst", all.x=T), mean)$role_consolidation#_role_consolidation####
rtng2000$awards             <-aggregate(awards ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")],indv2000, by="nconst", all.x=T), mean)$awards #_awards####
rtng2000$focus_genres       <-aggregate(focus_genres ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")],indv2000, by="nconst", all.x=T), mean)$focus_genres #_focus on roles####
rtng2000$focus_roles        <-aggregate(focus_roles ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")],indv2000, by="nconst", all.x=T), mean)$focus_roles#_focus on genres####

#_brokerage####
rtng2000$brokerage   <-aggregate(brokerage ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")],indv2000, by="nconst", all.x=T), mean)$brokerage#rtng2000$brokerage_smp       <-na.omit(aggregate(brokerage_smp ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")],indv2000, by="nconst", all.x=T), mean)$brokerage_smp)#_brokerage_smp####
#_coreness####
rtng2000$coreness   <-aggregate(coreness ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")],indv2000, by="nconst", all.x=T), mean)$coreness #rtng2000$coreness_smp        <-na.omit(aggregate(coreness_smp ~ tconst, merge(link2000[link2000$category %in% c("director"),c("nconst","tconst")],indv2000, by="nconst", all.x=T), mean)$coreness_smp)#_coreness_smp####

#!newcomer####
# rtng2000$newcomer<-NA 
# for (tconst in merge(link2000[!link2000$category %in% c("director"),c("nconst","tconst")])){
#   rtng2000[rtng2000$tconst=="rtng2000", "newcomer"]<- 

#   ifelse(aggregate(newcomer ~ tconst, merge(link2000[!link2000$category %in% c("director"),c("nconst","tconst")], indv2000, by="nconst", all.x=T)$newcomer, mean) 
#          ==1
#          , 1, NA)}


#############################B-TEAM ATTRIBUTES####
#_team quality####
rtng2000$quality<-aggregate(quality ~ tconst, link2000, mean)$quality

#_team coreness####
rtng2000$team_coreness<-setNames(aggregate(coreness ~ tconst, link2000, mean), c("tconst", "team_coreness"))$team_coreness
#length(unique(rtng2000$tconst))#length(unique(na.omit(aggregate(coreness ~ tconst, link2000, mean))$tconst)) #rtng2000$role_consolidation<-NA


colSums(is.na(link2000))
colSums(is.na(indv2000))
colSums(is.na(rtng2000))

# #!!!!!!!!!!!!!!!!!!FINALIZE!!!!!!!!!!!!!!!!!!####
# length(indv2000)
# indv2000<-indv2000[,c("nconst", "metascore","average_confusion","experience","newcomer","orientation",  
#             "tenure", "focus_genres","focus_genres_ent","focus_roles","focus_roles_ent",
#             
#             "hubs_score","page_rank","constraint","coreness","eigenvector_centrality","two_step_degree",    
#             "hubs_score_smp","page_rank_smp","constraint_smp","coreness_smp","eigenvector_centrality_smp",        
#             
#             "gConf","startYear", "genres",
#             "Drama","Comedy","History","Crime","Romance","Fantasy","Mystery","War","Adventure", "Horror", "Action","Family", "Western","Thriller","Sci-Fi", "Biography", "Sport", "Documentary","Musical","Film-Noir","Music","Animation", "Adult","News", "Reality-TV")]
# names(indv2000)
# 
# rtng2000<-merge(x=rtng2000[,c("tconst","startYear", "Director_1", "ndir", "metascore","rlcl")], y=indv2000[,c(1:19,22)], by.x="Director_1", by.y="nconst", all.x = T)
# #"Director_2", "writer_2","Director_3", "writer_3",
# # rtng2000<-merge(x=rtng2000, y=wrt[,c(1:19)], by.x="writer_1", by.y="nconst", all.x = T)
# names(rtng2000)<-gsub(x = names(rtng2000), pattern = "metascore.x", replacement = "metascore") 
# names(rtng2000)<-gsub(x = names(rtng2000), pattern = "metascore.y", replacement = "average metascore") 
# names(rtng2000)<-gsub(x = names(rtng2000), pattern = ".x", replacement = "_dir1")
# names(rtng2000)<-gsub(x = names(rtng2000), pattern = "_dir1perience", replacement = "experience")
# # names(rtng2000)<-gsub(x = names(rtng2000), pattern = ".y", replacement = "_wrt1")
# 
# 
# #setwd("e:/RMT")
# write.table(edges, file='edges2000.tsv', quote=FALSE, sep='\t', col.names = NA)
# write.table(indv2000, file='indv20002000.tsv', quote=FALSE, sep='\t', col.names = NA)
# write.table(wrt, file='wrt2000.tsv', quote=FALSE, sep='\t', col.names = NA)
# write.table(rtng2000, file='rtng2000.tsv', quote=FALSE, sep='\t', col.names = NA)
# #setwd("c:/R")
# rm(edges)
# gc()
# 
# 
# 
# 
# 
# 
# 
# # __________________________________________________________________________________________####  
# #############################SIMPLIFIED NETWORK####
# 
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
# #sdts <-neighborhood.size(snet, nodes = V(snet), 2, mode="all")-1
# 
# sdstr<- data.frame(nconst=names(sdcn),hubs_score=sdhs, page_rank=sdpr, brokerage=sdcn, coreness=sdcr, eigenvector_centrality=sdev)
# nrow(complete.cases(sdstr))
# 
# sapply(indv2000, function(y) sum(length(which(is.na(y)))))
# nrow(indv2000)
# sum(!is.na(sdstr$brokerage_smp))
# 
# 
