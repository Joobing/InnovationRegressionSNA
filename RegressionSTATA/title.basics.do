import delimited S:\Joobin\title.basics.tsv, varnames(1) 
gen tmp = _n
sort isadult
replace genres = runtimeminutes if isadult!="0" & isadult!="1"
replace runtimeminutes = endyear if isadult!="0" & isadult!="1"
replace endyear = startyear if isadult!="0" & isadult!="1"
replace startyear = isadult if isadult!="0" & isadult!="1"
replace isadult = "" if isadult!="0" & isadult!="1"
destring isadult startyear endyear runtimeminutes, replace force
sort tmp
drop tmp
export delimited using "S:\Joobin\title.basics.csv", quote replace
recast strL *
compress
save "S:\Joobin\title.basics.dta" 
export delimited using "T:\Creativity\1_data\title.basics.csv", quote replace
clear
exit
