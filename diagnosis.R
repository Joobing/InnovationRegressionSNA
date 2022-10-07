TRUE %in% (unique(indv$nconst) %in% unique(link$nconst))#&(link$nconst %in% indv$nconst)

mean(aggregate(category ~ tconst , link, FUN=function(x) length(unique(x)))$category)
sd(aggregate(category ~ tconst , link, FUN=function(x) length(unique(x)))$category)
mean(aggregate(category ~ tconst , link[link$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer")], FUN=function(x) length(unique(x)))$category)
sd(aggregate(category ~ tconst , link[link$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer")], FUN=function(x) length(unique(x)))$category)


mean(aggregate(nconst ~ tconst , link, length)$nconst)
sd(aggregate(nconst ~ tconst , link, length)$nconst)
mean(aggregate(nconst ~ tconst , link[link$category %in% 
  c("director", "cinematographer", "editor", "writer", "production_designer", "composer")], length)
  $nconst)
sd(aggregate(nconst ~ tconst , link[link$category %in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer")], length)$nconst)

for (r in c("director", "cinematographer", "editor", "writer", "production_designer", "composer"))
{print(c(r,round(mean(aggregate(nconst ~ tconst , link[link$category %in% c(r)], length)$nconst),2),
  round(sd(aggregate(nconst ~ tconst , link[link$category %in% c(r)], length)$nconst),2)))}

length(unique(indv$nconst))

round(aggregate(nconst ~ year , indv, FUN=function(x) length(unique(x))),2)

for(y in unique(indv$year)){
  print(c(y
    ,length(unique(link[(link$tconst %in% rtng[(rtng$startYear==y)&(!(rtng$tconst %in% animentry)),"tconst"])&(link$category%in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer")),]$nconst))
    #,length(unique(indv[(indv$year==y)&(indv$nconst%in%unique(link$nconst)),]$nconst))
    ,length(link[(link$tconst %in% rtng[(rtng$startYear==y)&(!(rtng$tconst %in% animentry)),"tconst"])&(link$category%in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer")),]$nconst)
    #,length(indv[(indv$year==y)&(indv$nconst%in%unique(link$nconst)),]$nconst)
  ))}

for(y in unique(indv$year)){
  print(c(y, length(indv[indv$year==y,]$nconst)))}
# setdiff( unique(link[(link$tconst %in% rtng[(rtng$startYear==y)&(!(rtng$tconst %in% animentry)),"tconst"])&(link$category%in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"
# )),]$nconst) ,indv[indv$year==y,]$nconst) 
    
round(mean(aggregate(year ~ nconst , indv, FUN=function(x) length(unique(x)))$year),2)

round(mean(aggregate(year ~ nconst , indv, length)$year),2)
round(sd(aggregate(year ~ nconst , indv, length)$year),2)
nrow(indv[indv$nconst=='nm0000265',])


colSums(is.na(indv[,(colSums(is.na(indv)) != 0)]))

indv[(colSums(is.na(indv$brokerage)) != 0),]

colSums(is.na(indv[,(colSums(is.na(indv)) != 0)]))
colSums(is.na(indv2000[,(colSums(is.na(indv2000)) != 0)]))
nrow(indv2000)
use <- names(indv2000)[names(indv2000) %in% colnames(indv[colSums(is.na(indv)) != 0])]
colSums(is.na(indv2000[,use]))
colSums(is.na(indv2000[,(length(indv2000)-25):length(indv2000)]))

colnames(indv2000[,colSums(is.na(indv2000))!=0])


indv[is.na(indv$brokerage),"year"]
indv[is.na(indv$metascore),"year"]

#ISOLATES?####
degree(g)
range(degree(g))
tkplot(g)
range(coreness(g))
str(coreness(g))
coreness(g)
range(max(dcn,0))


# STATA####
# sum coreness
# sum coreness
# tab corenes
# tab brokeage
# tab brokerage
# tab brokerage if brokerage<0.5
# replace brokerage=0 if brokerage < 0
# tab brokerage if brokerage<0.5
# desk
# desc
# summ two_step_degree, detail
# sum coreness
# summ coreness



















# length(unique(link2000[
#               (link2000$tconst %in% rtng2000$tconst)&
#               (link2000$category%in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"))
#               ,]$nconst))
# 
# length(unique(indv2000[
#   (indv2000$tconst %in% rtng2000$tconst)&
#     (indv2000$category%in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer"))
#   ,]$nconst))
# length(unique(indv2000$nconst))
# 
# print(c(y,
#         length(unique(link[(link$tconst %in% rtng[(rtng$startYear==y)&(!(rtng$tconst %in% animentry)),"tconst"])&(link$category%in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer")),]$nconst)),
#         length(unique(indv[(indv$year==y)&(indv$nconst%in%unique(link$nconst)),]$nconst)),
#         length(link[(link$tconst %in% rtng[(rtng$startYear==y)&(!(rtng$tconst %in% animentry)),"tconst"])&(link$category%in% c("director", "cinematographer", "editor", "writer", "production_designer", "composer")),]$nconst),
#         length(indv[(indv$year==y)&(indv$nconst%in%unique(link$nconst)),]$nconst)
# ))
# 
