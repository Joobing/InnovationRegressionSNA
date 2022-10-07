clear
set max_memory 256g
set more off, permanently
set rmsg on, permanently
set maxiter 1000, permanently
set matsize 11000
use "T:\Creativity\3_sample\indv20.dta", clear
cd  "T:\Creativity\4_analysis\missing\"
*log using "Log.smcl", append

* variables
** mean-center high VIFs
foreach var of varlist brokerage coreness focus_genres focus_roles average_confusion role_consolidation team_coreness {
	summ `var'
	gen `var'_mc = `var' - r(mean)
	summ `var'
}

foreach var of varlist brokerage coreness focus_genres focus_roles average_confusion role_consolidation team_coreness {
	summ `var'
	gen C_`var' = `var' - r(mean)
	summ `var'
}

** label variables
label var awards "Awards"
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
*global key_vars awards brokerage_mc coreness_mc focus_genres_mc focus_roles_mc 
global cv_main  role_consolidation_mc experience quality team_coreness_mc team_quality average_confusion_mc sequel 
global cv_miss	metascore  budget  
global cv_genre Comedy Romance Horror Action Western Thriller Sci_Fi War Family Mystery Fantasy Adventure Crime History 
global cv_role  actor actress producer director writer composer cinematographer editor production_designer
global cv_time  i.year 

** set panel
encode nconst, generate(id) label(nconst) 
xtset id year

* regressions
** direct effects
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.coreness_mc##c.coreness_mc , re
margins, at(coreness_mc==(-4 1 5 10 15 20 25 30 36)) vsquish pr(nu0)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)

* this one is the one I had used in the earliest version V0(cv_miss = budget+metascore):
xtnbreg awards $cv_main $cv_miss $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.coreness_mc##c.coreness_mc , re
margins, at(coreness_mc==(-4 1 5 10 15 20 25 30 36)) vsquish pr(nu0)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)



** brokerage interaction effects
*brokerage x genrefocus (without bugdet metascore)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time ///
focus_genres_mc focus_roles_mc brokerage_mc c.focus_genres_mc#c.brokerage_mc /// 
if actor!=role_consolidation & producer!=role_consolidation ,re
margins, at( focus_genres_mc=(-0.2 .6) brokerage_mc=(-0.7 0.3)) vsquish pr(nu0)
marginsplot, noci x(brokerage_mc) recast(line) xlabel(-0.8(0.2)0.5)

*brokerage x genrefocus (with bugdet metascore)
xtnbreg awards $cv_main $cv_miss $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.focus_genres_mc#c.brokerage_mc /// 
if actor!=role_consolidation & producer!=role_consolidation , re
margins, at( focus_genres_mc=(-0.2 .6) brokerage_mc=(-0.7 0.3)) vsquish pr(nu0)
marginsplot, noci x(brokerage_mc) recast(line) xlabel(-0.8(0.2)0.5)

*brokerage x rolefocus (without bugdet metascore)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.focus_roles_mc#c.brokerage_mc /// 
if actor!=role_consolidation & producer!=role_consolidation , re
margins, at( focus_roles_mc=(-0.50 0.15) brokerage_mc=(-0.7 0.3)) vsquish pr(nu0)
marginsplot, noci x(brokerage_mc) recast(line) xlabel(-0.8(0.2)0.5)

*brokerage x rolefocus (with bugdet metascore)
xtnbreg awards $cv_main $cv_miss $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.focus_roles_mc#c.brokerage_mc /// 
if actor!=role_consolidation & producer!=role_consolidation , re
margins, at( focus_roles_mc=(-0.50 0.15) brokerage_mc=(-0.7 0.3)) vsquish pr(nu0)
marginsplot, noci x(brokerage_mc) recast(line) xlabel(-0.8(0.2)0.5)

** coreness interaction effects
*coreness x genrefocus (without bugdet metascore)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc c.focus_genres_mc#c.coreness_mc##c.coreness_mc brokerage_mc /// 
if actor!=role_consolidation & producer!=role_consolidation, re
margins, at( focus_genres_mc=(-0.2 .6) coreness_mc=(-4	-3	-2	-1	0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36)) vsquish pr(nu0)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)

*coreness x genrefocus (with bugdet metascore)
xtnbreg awards $cv_main $cv_miss $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc c.focus_genres_mc#c.coreness_mc##c.coreness_mc brokerage_mc /// 
if actor!=role_consolidation & producer!=role_consolidation, re
margins, at( focus_genres_mc=(-0.2 .6) coreness_mc=(-4	-3	-2	-1	0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36)) vsquish pr(nu0)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)

*coreness x rolefocus (without bugdet metascore)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc c.focus_roles_mc#c.coreness_mc##c.coreness_mc brokerage_mc /// 
if actor!=role_consolidation & producer!=role_consolidation, re
margins, at( focus_roles_mc=(-0.50 0.15) coreness_mc=(-4	-3	-2	-1	0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36)) vsquish pr(nu0)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)

*coreness x rolefocus (with bugdet metascore)
xtnbreg awards $cv_main $cv_miss $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc c.focus_roles_mc#c.coreness_mc##c.coreness_mc brokerage_mc /// 
if actor!=role_consolidation & producer!=role_consolidation, re
margins, at( focus_roles_mc=(-0.50 0.15) coreness_mc=(-4	-3	-2	-1	0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36)) vsquish pr(nu0)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)


**********************************Final Comparison***************************************

*without bugdet metascore 
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.coreness_mc##c.coreness_mc ///
c.focus_genres_mc#c.brokerage_mc c.focus_roles_mc#c.brokerage_mc c.focus_roles_mc#c.coreness_mc##c.coreness_mc c.focus_genres_mc#c.coreness_mc##c.coreness_mc, re

margins, at( focus_genres_mc=(-0.2 .6) brokerage_mc=(-0.7 0.3)) vsquish pr(nu0)
marginsplot, noci x(brokerage_mc) recast(line) xlabel(-0.8(0.2)0.5)
margins, at( focus_roles_mc=(-0.50 0.15) brokerage_mc=(-0.7 0.3)) vsquish pr(nu0)
marginsplot, noci x(brokerage_mc) recast(line) xlabel(-0.8(0.2)0.5)

margins, at( focus_genres_mc=(-0.2 .6) coreness_mc=(-4	-3	-2	-1	0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36)) vsquish pr(nu0)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)
margins, at( focus_roles_mc=(-0.50 0.15) coreness_mc=(-4	-3	-2	-1	0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36)) vsquish pr(nu0)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)

*with bugdet metascore (V0 Reproduced)
xtnbreg awards $cv_main $cv_miss $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.coreness_mc##c.coreness_mc ///
c.focus_genres_mc#c.brokerage_mc c.focus_roles_mc#c.brokerage_mc c.focus_roles_mc#c.coreness_mc##c.coreness_mc c.focus_genres_mc#c.coreness_mc##c.coreness_mc, re

margins, at( focus_genres_mc=(-0.2 .6) brokerage_mc=(-0.7 0.3)) vsquish pr(nu0)
marginsplot, noci x(brokerage_mc) recast(line) xlabel(-0.8(0.2)0.5)
margins, at( focus_roles_mc=(-0.50 0.15) brokerage_mc=(-0.7 0.3)) vsquish pr(nu0)
marginsplot, noci x(brokerage_mc) recast(line) xlabel(-0.8(0.2)0.5)

margins, at( focus_genres_mc=(-0.2 .6) coreness_mc=(-4	-3	-2	-1	0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36)) vsquish pr(nu0)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)
margins, at( focus_roles_mc=(-0.50 0.15) coreness_mc=(-4	-3	-2	-1	0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18	19	20	21	22	23	24	25	26	27	28	29	30	31	32	33	34	35	36)) vsquish pr(nu0)
marginsplot, noci x(coreness_mc) recast(line) xlabel(0(5)40)


count  if budget==. & metascore==.
count  if budget==. 
count  if metascore==. 

foreach var of varlist $cv_miss $keyvars $cv_main $cv_genre $cv_role {
	count if `var'==.
}


drop if metascore==. & budget==.


