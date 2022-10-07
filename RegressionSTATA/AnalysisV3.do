clear
set max_memory 256g
set more off, permanently
set rmsg on, permanently
set maxiter 1000, permanently
set matsize 11000
use "T:\Creativity\3_sample\indv20.dta", clear
cd  "T:\Creativity\4_analysis\V3\"
log using "Log.smcl", append

* variables
summ coreness, detail
replace coreness = r(p99) if coreness>r(p99)
** mean-center IVs
foreach var of varlist brokerage coreness focus_genres focus_roles {
	summ `var'
	gen `var'_mc = `var' - r(mean)
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
global key_vars awards brokerage_mc coreness_mc focus_genres_mc focus_roles_mc 
global cv_main  role_consolidation experience quality team_coreness team_quality average_confusion sequel metascore budget 
global cv_genre Comedy Romance Horror Action Western Thriller Sci_Fi War Family Mystery Fantasy Adventure Crime History 
global cv_role  actor actress producer director writer composer cinematographer editor production_designer 
global cv_time  i.year 

* sample
** remove incomplete observations
foreach var of varlist $keyvars $cv_main $cv_genre $cv_role {
	drop if `var'==.
}
** set panel
encode nconst, generate(id) label(nconst) 
xtset id year

** descriptives
mkcorr $keyvars $cv_main $cv_genre $cv_role , log("Descriptives.xls") means cdec(3) mdec(3) num lab

* regressions
** direct effects
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time , re difficult iter(250)
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time brokerage_mc , re difficult iter(250)
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time coreness_mc , re difficult iter(250)
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time c.coreness_mc##c.coreness_mc , re difficult iter(250)
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc , re difficult iter(250)
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_roles_mc , re difficult iter(250)
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc , re difficult iter(250) 
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc , re difficult iter(250)
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc coreness_mc , re difficult iter(250)
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc , re difficult iter(250) 
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc coreness_mc , re difficult iter(250)
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.coreness_mc##c.coreness_mc , re difficult iter(250)
outreg2 using "Regressions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
** interaction effects
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time , re difficult iter(250)
outreg2 using "Interactions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc , re difficult iter(250) 
outreg2 using "Interactions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc , re difficult iter(250)
outreg2 using "Interactions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.focus_genres_mc#c.brokerage_mc , re difficult iter(250)
outreg2 using "Interactions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.focus_roles_mc#c.brokerage_mc , re difficult iter(250)
outreg2 using "Interactions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.focus_genres_mc#c.brokerage_mc c.focus_roles_mc#c.brokerage_mc , re difficult iter(250)
outreg2 using "Interactions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc , re difficult iter(250) 
outreg2 using "Interactions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc c.focus_genres_mc#c.coreness_mc##c.coreness_mc , re difficult iter(250)
outreg2 using "Interactions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc c.focus_roles_mc#c.coreness_mc##c.coreness_mc , re difficult iter(250)
outreg2 using "Interactions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc c.focus_genres_mc#c.coreness_mc##c.coreness_mc c.focus_roles_mc#c.coreness_mc##c.coreness_mc , re difficult iter(250)
outreg2 using "Interactions.xls", dec(3) label alpha(.001, .01, .05, .1) symbol(***, **, *, +) e(all)

* test for u-curve
** test 1: utest
gen coreness_mc_sq = coreness_mc*coreness_mc
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time coreness_mc coreness_mc_sq , re difficult iter(250) 
utest coreness_mc coreness_mc_sq , prefix(awards)
** test 2: peak within observed values?
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time coreness_mc coreness_mc_sq , re difficult iter(250) 
global peak = _b[coreness_mc] / (-2*_b[coreness_mc_sq])
disp $peak
summ coreness_mc , detail
** test 3: marginal effects
summ coreness_mc , detail
global core_min = r(min)
global core_p10 = r(p10)
global core_p50 = r(p50)
global core_p90 = r(p90)
global core_max = r(max)
global core_step = ( $core_max - $core_min ) / 10
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time c.coreness_mc##c.coreness_mc , re difficult iter(250) 
margins, dydx(coreness_mc) at( coreness=( $core_min $core_p10 $core_p50 $core_p90 $core_max ) )
margins, dydx(coreness_mc) at( coreness=( $core_min ( $core_step ) $core_max ) )
** test 4: split-sample
count if coreness_mc < $peak
count if coreness_mc > $peak
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time coreness_mc if coreness_mc < $peak, re difficult iter(250) 
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time coreness_mc if coreness_mc > $peak, re difficult iter(250) 
** test 5: cube effect
gen coreness_mc_cu = coreness_mc*coreness_mc*coreness_mc
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time coreness_mc coreness_mc_sq coreness_mc_cu , re difficult iter(250) 

* marginplots
** define values
summ brokerage_mc , detail
global broker_min = r(min)
global broker_max = r(max)
global broker_step = ( $broker_max - $broker_min ) / 20
summ coreness_mc , detail
global core_min = r(min)
global core_max = r(max)
global core_step = ( $core_max - $core_min ) / 20
summ focus_genres_mc , detail
global genre_p10 = r(p10)
global genre_p90 = r(p90)
summ focus_roles_mc , detail
global roles_p10 = r(p10)
global roles_p90 = r(p90)
** create plots brokerage
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc , re difficult iter(250)
margins, at( brokerage_mc=( $broker_min ( $broker_step ) $broker_max ) ) vsquish noatlegend pr(nu0)
marginsplot, xdimension(brokerage_mc) recast(line) recastci(rarea)
graph save Graph "Brokerage.gph"
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc brokerage_mc c.focus_genres_mc#c.brokerage_mc c.focus_roles_mc#c.brokerage_mc , re difficult iter(250)
margins, at( brokerage_mc=( $broker_min ( $broker_step ) $broker_max ) focus_genres_mc=( $genre_p10 $genre_p90 ) ) vsquish noatlegend pr(nu0)
marginsplot, xdimension(brokerage_mc) recast(line) recastci(rarea)
graph save Graph "BrokerageGenre.gph"
margins, at( brokerage_mc=( $broker_min ( $broker_step ) $broker_max ) focus_roles_mc=( $roles_p10 $roles_p90 ) ) vsquish noatlegend pr(nu0)
marginsplot, xdimension(brokerage_mc) recast(line) recastci(rarea)
graph save Graph "BrokerageRoles.gph"
** create plots coreness
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc , re difficult iter(250) 
margins, at( coreness_mc=( $core_min ( $core_step ) $core_max ) ) vsquish noatlegend pr(nu0)
marginsplot, xdimension(coreness_mc) recast(line) recastci(rarea)
graph save Graph "Coreness.gph"
xtnbreg awards $cv_main $cv_genre $cv_role $cv_time focus_genres_mc focus_roles_mc c.coreness_mc##c.coreness_mc c.focus_genres_mc#c.coreness_mc##c.coreness_mc c.focus_roles_mc#c.coreness_mc##c.coreness_mc , re difficult iter(250)
margins, at( coreness_mc=( $core_min ( $core_step ) $core_max ) focus_genres_mc=( $genre_p10 $genre_p90 ) ) vsquish noatlegend pr(nu0)
marginsplot, xdimension(coreness_mc) recast(line) recastci(rarea)
graph save Graph "CorenessGenre.gph"
margins, at( coreness_mc=( $core_min ( $core_step ) $core_max ) focus_roles_mc=( $roles_p10 $roles_p90 ) ) vsquish noatlegend pr(nu0)
marginsplot, xdimension(coreness_mc) recast(line) recastci(rarea)
graph save Graph "CorenessRoles.gph"


log close
/*
variables:
awards brokerage coreness focus_genres focus_roles 
role_consolidation experience quality team_coreness team_quality average_confusion sequel metascore budget 
Comedy Romance Horror Action Western Thriller Sci_Fi War Family Mystery Fantasy Adventure Crime History 
actor actress producer director writer composer cinematographer editor production_designer
i.year


** tests
