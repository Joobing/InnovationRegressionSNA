clear
set max_memory 256g
set more off, permanently
set rmsg on, permanently
set maxiter 1000, permanently
set matsize 11000
use "T:\Creativity\3_sample\indv20.dta", clear
cd  "T:\Creativity\4_analysis\V8\"
*log using "Log.smcl", append


***********************SAMPLE********************************************************************************************
count  if budget==. & metascore==.
count  if budget==. 
count  if metascore==. 

drop if metascore==. 
drop if budget==.


***********************subSAMPLE********************************************************************************************


by nconst (year), sort: gen byte first = sum(awards) > 0  & sum(awards[_n - 1]) == 0  


egen x = min(experience), by(nconst)
*list nconst year x experience if x > 0
drop if x > 0

egen y = min(first), by(nconst)
levelsof nconst if y!=first
list nconst year first if nconst== "nm0003298"

summ first if first > 0




***********************VARIABLES********************************************************************************************
ssc install estout
estpost summarize 
esttab using descriptives_first_nom.rtf ,  replace cells("count(fmt(2)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") noobs 


******************************************************************************
** Set variables used in Summary and Correlation (NOTE: coeflabel has to be adjusted if changes are made)
global variables first brokerage coreness focus_genres focus_roles experience quality  team_quality  sequel metascore budget average_confusion role_consolidation team_coreness  
*mkcorr $variables , log(descriptiives.xls) replace means sig cdec(2) mdec(2)
*******************************************************************************************************

** log-transform coreness
gen coreness_logged = ln(coreness)

** mean-center high VIFs
foreach var of varlist brokerage coreness coreness_logged focus_genres focus_roles average_confusion role_consolidation team_coreness {
	summ `var'
	gen `var'_mc = `var' - r(mean)
	summ `var'
}

** label variables
label var first "first"
label var brokerage_mc "Brokerage"
label var coreness_mc "Coreness"
label var focus_genres_mc "Genre focus"
label var focus_roles_mc "Role focus"
label var role_consolidation "Role consolidation"
label var experience "Experience"
label var quality "Quality"
label var team_coreness "Team coreness"
label var team_quality "Team quality"
label var average_confusion "Confusion (avg)"
label var sequel "Sequel"
label var metascore "Metascore"
label var budget "Movie budget"
label var Comedy "Comedy"
label var Romance "Romance"
label var Horror "Horror"
label var Action "Action"
label var Western "Western"
label var Thriller "Thriller"
label var Sci_Fi "Sci-Fi"
label var War "War"
label var Family "Family"
label var Mystery "Mystery"
label var Fantasy "Fantasy"
label var Adventure "Adventure"
label var Crime "Crime"
label var History "History"
label var actor "Actor"
label var actress "Actress"
label var producer "Producer"
label var director "Director"
label var writer "Writer"
label var composer "Composer"
label var cinematographer "Cinematographer"
label var editor "Editor"
label var production_designer "Production_designer"

** group controls
global key_vars first brokerage_mc coreness_mc focus_genres_mc focus_roles_mc 
global cv_main  role_consolidation_mc experience quality team_coreness_mc team_quality average_confusion_mc sequel metascore budget   
global cv_genre Comedy Romance Horror Action Western Thriller Sci_Fi War Family Mystery Fantasy Adventure Crime History 
global cv_role  actor actress producer director writer composer cinematographer editor production_designer
global cv_time  i.year 

foreach var of varlist $keyvars $cv_main $cv_genre $cv_role {
	count if `var'==.
}
** set panel
encode nconst, generate(id) label(nconst) 
xtset id year

***********************REGRESSIONS********************************************************************************************
eststo clear

** BASELINE ESTIMATION
eststo: xtprobit first $cv_main $cv_genre $cv_role $cv_time /// 
focus_genres_mc focus_roles_mc  ///
if actor!=role_consolidation & producer!=role_consolidation, re

** MAIN EFFECTS 
eststo: xtprobit first $cv_main $cv_genre $cv_role $cv_time /// 
focus_genres_mc focus_roles_mc brokerage_mc ///
if actor!=role_consolidation & producer!=role_consolidation, re

margins, at( brokerage_mc=(-0.7 0.3)) vsquish 
marginsplot, xdimension(brokerage_mc) recast(line) recastci(rarea) xlabel(-0.8(0.2)0.4)

eststo: xtprobit first $cv_main $cv_genre $cv_role $cv_time /// 
focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc ///
if actor!=role_consolidation & producer!=role_consolidation, re

** BROKERAGE

eststo: xtprobit first $cv_main $cv_genre $cv_role $cv_time ///
c.focus_roles_mc##c.brokerage_mc c.focus_genres_mc##c.brokerage_mc /// 
if actor!=role_consolidation & producer!=role_consolidation , re

hist brokerage_mc
centile (brokerage_mc) if actor!=role_consolidation & producer!=role_consolidation, centile (1 5 95 99)

margins, at( focus_genres_mc=(-0.2 .6) brokerage_mc=(-0.7 0.3)) vsquish 
marginsplot, noci x(brokerage_mc) recast(line) xlabel(-0.8(0.2)0.5)
marginsplot, xdimension(brokerage_mc) recast(line) recastci(rarea)

margins, at( focus_roles_mc=(-0.50 0.15) brokerage_mc=(-0.7 0.3)) vsquish 
marginsplot, noci x(brokerage_mc) recast(line) xlabel(-0.8(0.2)0.5)
marginsplot, xdimension(brokerage_mc) recast(line) recastci(rarea)

** CORENESS

eststo: 
xtprobit first $cv_main $cv_genre $cv_role $cv_time ///
c.focus_genres_mc##c.coreness_mc##c.coreness_mc c.focus_roles_mc##c.coreness_mc##c.coreness_mc ///
if actor!=role_consolidation & producer!=role_consolidation, re 
*c.coreness_mc##c.coreness_mc focus_genres_mc focus_roles_mc   if budget<9.9e9


**ALL EFFECTS

eststo: xtprobit first $cv_main $cv_genre $cv_role $cv_time ///
c.focus_roles_mc##c.coreness_mc##c.coreness_mc c.focus_genres_mc##c.coreness_mc##c.coreness_mc c.focus_roles_mc##c.brokerage_mc c.focus_genres_mc##c.brokerage_mc ///
if actor!=role_consolidation & producer!=role_consolidation, re 

esttab using results_first_nom.rtf, replace compress label b(3) se(3) title(RANDOM EFFECTS ESTIMATE OF INDIVIDUAL CREATIVITY) nonumbers mtitles("Baseline" "Brokerage" "Coreness" "Brokerage-Focus" "Coreness-Focus" "All Effects")  addnote("ADD DESCRIPTIONS HERE...")    star(* 0.10 ** 0.05 *** 0.01)


***********************MARGINS*********************************************************************
hist coreness_mc
centile (coreness_mc) if actor!=role_consolidation & producer!=role_consolidation, centile (0 1 5 95 99 100)

margins, at( focus_genres_mc=(-0.2 .6) coreness_mc=(-4 -1	2	5	8	11	14	17	20	23	26	29	32	35	)) vsquish 
marginsplot, xdimension(coreness_mc) recast(line) recastci(rarea)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)

margins, at( focus_roles_mc=(-0.50 .15) coreness_mc=(-4 -1	2	5	8	11	14	17	20	23	26	29	32	35	)) vsquish 
marginsplot, xdimension(coreness_mc) recast(line) recastci(rarea)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)

**99 percentile

margins, at( focus_genres_mc=(-0.2 .6) coreness_mc=(-4 -3	-2	-1	0	1	2	3	4	5	6	)) vsquish 
marginsplot, xdimension(coreness_mc) recast(line) recastci(rarea)
marginsplot, noci x(coreness_mc) recast(line) xlabel(-3(1)6)

margins, at( focus_roles_mc=(-0.50 0.15) coreness_mc=(-4 -3	-2	-1	0	1	2	3	4	5	6	)) vsquish 
marginsplot, xdimension(coreness_mc) recast(line) recastci(rarea)
marginsplot, noci x(coreness_mc) recast(line) xlabel(-3(1)6)

***********************CORENESS SHAPE*********************************************************************


** U-TEST
gen coreness_mc_sq = coreness_mc*coreness_mc
xtprobit first $cv_main $cv_genre $cv_role $cv_time coreness_mc coreness_mc_sq  if actor!=role_consolidation & producer!=role_consolidation, re

*centile (coreness_mc) if actor!=role_consolidation & producer!=role_consolidation, centile (0 1 5 95 99 100)
summ coreness, detail

utest coreness_mc coreness_mc_sq , prefix(first)
// Extreme point:  17.47012 outside 99 percentile


** MARGINS
*** Full range
xtprobit first $cv_main $cv_genre $cv_role $cv_time c.coreness_mc##c.coreness_mc ///
if actor!=role_consolidation & producer!=role_consolidation, re


margins, dydx(coreness_mc) at( coreness=( -5 (3) 36.5) )
marginsplot, xdimension(coreness_mc) recast(line) recastci(rarea)


margins, at(coreness_mc==(-5 (2) 10)) vsquish 
marginsplot, noci x(coreness_mc) recast(line) xlabel(-5(2)10)
marginsplot, xdimension(coreness_mc) recast(line) recastci(rarea)


margins, at(coreness_mc==(-5 (5) 35)) vsquish 
marginsplot, noci x(coreness_mc) recast(line) xlabel(-5(5)35)
marginsplot, xdimension(coreness_mc) recast(line) recastci(rarea)

**SPLIT SAMPLE
eststo clear

eststo: xtprobit first $cv_main $cv_genre $cv_role $cv_time c.coreness_mc ///
if actor!=role_consolidation & producer!=role_consolidation & c.coreness_mc < 17.47012, re


* eststo: xtprobit first $cv_main $cv_genre $cv_role $cv_time c.coreness_mc ///
*if actor!=role_consolidation & producer!=role_consolidation & c.coreness_mc > 17.47012, re
// does not converge

** LOGGED
summ coreness_logged

eststo: xtprobit first $cv_main $cv_genre $cv_role $cv_time ///
c.focus_roles_mc##c.coreness_logged##c.coreness_logged c.focus_genres_mc##c.coreness_logged##c.coreness_logged if actor!=role_consolidation & producer!=role_consolidation, re 
*if budget<9.9e9 c.coreness_logged##c.coreness_logged focus_genres_mc focus_roles_mc 

summ coreness_mc, detail
replace coreness_mc= r(p99) if coreness_mc>r(p99)

** winsorized

eststo: xtprobit first $cv_main $cv_genre $cv_role $cv_time ///
c.focus_roles_mc##c.coreness_mc##c.coreness_mc c.focus_genres_mc##c.coreness_mc##c.coreness_mc if actor!=role_consolidation & producer!=role_consolidation, re 

esttab using corness_first_nom.rtf, replace compress label b(3) se(3) title(RANDOM EFFECTS ESTIMATE OF INDIVIDUAL CREATIVITY) nonumbers mtitles("left" "right" "logged" )  addnote("ADD DESCRIPTIONS HERE...")    star(* 0.10 ** 0.05 *** 0.01)


margins, at( focus_genres_mc=(-0.2 .6) coreness_logged=(0	0.5	1 1.5 2	2.5 3 3.5)) vsquish 
marginsplot, xdimension(coreness_logged) recast(line) recastci(rarea)
marginsplot, noci x(coreness_logged) recast(line) xlabel(0(0.5)4)

margins, at( focus_roles_mc=(-0.50 0.15) coreness_logged=(0	0.5	1 1.5 2	2.5 3 3.5)) vsquish 
marginsplot, xdimension(coreness_logged) recast(line) recastci(rarea)
marginsplot, noci x(coreness_logged) recast(line) xlabel(0 (0.5)4)

centile (coreness_logged) if actor!=role_consolidation & producer!=role_consolidation, centile (1 5 95 99)










