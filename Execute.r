#Cattani Ferriani take only 1992-2003
# 
# PYdir<-"d:/py"
Cdir<-"m:/Creativity/" # Cdir<-"m:/"
ddir<-"D:/#CREATIVITY/"# Ddir<-"d:/R" 


source(file.path(Cdir, "functions.r") , echo = TRUE)
source(file.path(Cdir, "Loading.r") , echo = TRUE)
# gc()

# for (id in 1894:2021)#source_path, target_path, pattern, replacement
# { replace<-list("2000" = id,  "1999"= id-1,  "1998"= id-2, "1997"= id-3, "1996"=id-4); file_find_replace("M:\\2000.R", file.path(Ddir, paste(id, ".R", sep=""))                     , replace);}

#for (id in 1993:2013) {
for (id in 1993:2020) {
  source(file.path(Cdir, paste(id, ".R", sep="")) ,  echo = TRUE)
}
gc()

#source(file.path(Cdir, "Creativity_BP.r") , echo = TRUE)


# source(file.path(Ddir, "database.r") , echo = TRUE)
#source(file.path(Ddir, "diagnosis.r") , echo = TRUE)


# 
# rm(list=setdiff(ls(), c("xx", "rtng", "mssrtng","missingrtng","missingindv")))
# 
# source(file.path(Rdir, "Directors.r") , echo = TRUE)
# source(file.path(Rdir, "preAnalysis.r") , echo = TRUE)