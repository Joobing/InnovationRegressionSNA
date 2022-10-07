missingrtng<-lapply(mget(ls(pattern="^rtng")), function(i) colSums(is.na(i)))
missingindv<-lapply(mget(ls(pattern="^indv")), function(i) colSums(is.na(i)))
lapply(mget(ls(pattern="^rtng")), function(i) length(i))
lapply(mget(ls(pattern="^indv")), function(i) length(i))

ttls <- rbind(rtng2000, rtng2001, rtng2002, rtng2003, rtng2004, rtng2005, rtng2006, rtng2007, rtng2008, rtng2009, rtng2010, rtng2011, rtng2012, rtng2013, rtng2014, rtng2015, rtng2016, rtng2017,  rtng1993, rtng1994, rtng1995, rtng1996, rtng1997, rtng1998, rtng1999)
indv <- rbind(indv2000, indv2001, indv2002, indv2003, indv2004, indv2005, indv2006, indv2007, indv2008, indv2009, indv2010, indv2011, indv2012, indv2013, indv2014, indv2015, indv2016, indv2017,  indv1993, indv1994, indv1995, indv1996, indv1997, indv1998, indv1999)
link <- rbind(link2000, link2001, link2002, link2003, link2004, link2005, link2006, link2007, link2008, link2009, link2010, link2011, link2012, link2013, link2014, link2015, link2016, link2017,  link1993, link1994, link1995, link1996, link1997, link1998, link1999)

indv <-setorder(indv,"nconst","year")
rownames(indv) <- 1:nrow(indv)

#_role dummies####
roles<-unique(link$category)
roles<-roles[!(roles %in% c("archive_footage", "self"))]
indv<-merge(indv,na.omit(aggregate(category ~ nconst, link[link$tconst %in% ttls$tconst,], toString)), by="nconst", all.x=T)
for (i in na.omit(roles)) {
  Variable=i;
  print(Variable)
  indv[, Variable] <- dum(i, indv$category)
}

getwd() 
#saveRDS(indv, "indv.rds")


# sy <- rbind(sy2000, sy2001, sy2002, sy1994, sy1995, sy1996, sy1997, sy1998, sy1999)#sy1990, sy1991, sy1992, sy1993, sy1994,sy1987, sy1988, sy1989
# str(sy)
#_genres####
#rtng <- merge(rtng, gnrs[,-3], by="tconst", all.x=T,  fill=F)
#_difference####
ttls$dif<-ttls$metascore-10*ttls$averageRating
#rtng <- merge(rtng, gnrs[,c("tconst", "prigen", "secgen", "thrgen")], by="tconst", all.x=T,  fill=F) # rtng <- merge(rtng, budget, by="tconst", all.x=T,  fill=F)
#_country####
# bdata<-cbind(bdata, dummy(bdata$currency, sep = "_"))
# names(bdata)<-gsub(x = names(bdata), pattern = "bdata_", replacement = "currency_") 


str(link)
indv$year

colSums(is.na(link))
colSums(is.na(indv))
colSums(is.na(ttls))
str(ttls)
nrow(unique(indv))
colSums(is.na((indv[,(colSums(is.na(indv)) != 0)])))






library(readstata13) 
save.dta13(indv,file ="M:\\indv20.dta")



# 
# write_rds(ttls, "rtng.rds", "gz")
# write_rds(indv, "indv.rds", "gz")
# 
# write.csv(indv, file=file("D:\\indv.csv" ,encoding="UTF-8"))
# write.csv(ttls, file=file("D:\\rtng.csv" ,encoding="UTF-8"))










