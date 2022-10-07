*Run the following in console:: set more off , permanently

clear
 
sysdir set PLUS D:\StataPackages
cd "m:\"
use "m:\indv20.dta", clear
*use "m:\indvSLCT.dta", clear

list year nconst awards if awards>10

tab awards

estpost summarize 
esttab using sum20.rtf ,  replace cells("count(fmt(2)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") noobs 


encode nconst, generate(id) label(nconst) 
xtset id year

mcenter(brokerage coreness focus_roles focus_genres)

eststo clear

****************MODEL-1: MAIN EFFECTS: Inverted-U
eststo: xtnbreg awards c.C_brokerage c.C_coreness##c.C_coreness c.C_focus_roles##c.C_focus_roles  c.C_focus_genres##c.C_focus_genres  role_consolidation experience quality c.team_coreness c.team_quality sequel c.metascore c.average_confusion i.year ///
  Comedy Romance Horror Action Western Thriller Sci_Fi War Family Mystery Fantasy Adventure Crime History   ///
 *Drama Comedy Romance Horror Action  ///
 actor actress producer director writer composer cinematographer editor production_designer, re

margins, at(C_coreness==(1 3 5 7 9 11 13 15 17 19 21 23 25)) vsquish pr(nu0)
marginsplot, noci x(C_coreness) recast(line) 
*xlabel(0.1(0.1)1)
margins, at(C_focus_roles=(0.1 0.3 0.5 0.7 0.9)) vsquish pr(nu0)
marginsplot, noci x(C_focus_roles) recast(line) 
margins, at(C_focus_genres=(0.1 0.3 0.5 0.7 0.9)) vsquish pr(nu0)
marginsplot, noci x(C_focus_genres) recast(line) 

****************UTEST
gen coreness_squared=coreness*coreness
gen focus_squared_roles=focus_roles*focus_roles
gen focus_squared_genres=focus_genres*focus_genres
xtnbreg awards coreness coreness_squared c.C_focus_roles focus_genres focus_squared_genres c.C_brokerage role_consolidation experience quality c.team_coreness c.team_quality sequel c.metascore c.average_confusion i.year ///
 Comedy Romance Horror Action Western Thriller Sci_Fi War Family Mystery Fantasy Adventure Crime History   ///
 actor actress producer director writer composer cinematographer editor production_designer, re

utest coreness coreness_squared , min(0) max(1746) prefix(awards)
utest focus_genres focus_squared_genres , min(0) max(1) prefix(awards)

* MODEL-2: BROKERAGE X FOCUS
eststo: xtnbreg awards c.C_brokerage c.C_brokerage#c.C_focus_genres c.C_brokerage#c.C_focus_roles c.C_coreness  c.C_focus_roles c.C_focus_genres role_consolidation experience quality c.team_coreness c.team_quality sequel c.average_confusion i.year ///
 Comedy Romance Horror Action Western Thriller Sci_Fi War Family Mystery Fantasy Adventure Crime History   ///
 *Drama Comedy Romance Horror Action  ///
 actor actress producer director writer composer cinematographer editor production_designer, re
 
margins, at(C_brokerage=(-0.1 0.95) C_focus_genres=(0.1 1)) vsquish pr(nu0)
marginsplot, noci x(C_brokerage) recast(line) xlabel(0.1(0.1)1)
margins, at(C_brokerage=(-0.1 0.95) C_focus_roles=(0.1 1)) vsquish pr(nu0)
marginsplot, noci x(C_brokerage) recast(line) xlabel(0.1(0.1)1)
* if I add metascore or budget smaller half of the the obserations drop leading to insignificant results

****************MODEL-3: CORENESS X FOCUS
eststo: xtnbreg awards c.C_coreness##c.C_coreness##c.C_focus_genres c.C_focus_genres#c.C_focus_genres c.C_coreness##c.C_coreness##c.C_focus_roles c.C_brokerage role_consolidation experience quality c.team_coreness c.team_quality c.average_confusion i.year ///
 Comedy Romance Horror Action  ///
 actor actress producer director writer composer cinematographer editor production_designer, re
*NOte that when including interaction with focus, coreness_squared is no more significant! niether is coreness!
*but coreness*focus is significant (almost) for both dimensions of focus
margins, at(C_coreness=(1 10) C_focus_genres=(0.1 1)) vsquish pr(nu0)
marginsplot, noci x(C_coreness) recast(line) xlabel(0(1)10)
margins, at(C_coreness=(1 10) C_focus_roles=(0.1 1)) vsquish pr(nu0)
marginsplot, noci x(C_coreness) recast(line) xlabel(0(1)10)

****************TO-BE COMPLETED
****************MODEL-4: ALL-INCLUSIVE PACKAGE
eststo: xtnbreg awards c.C_brokerage c.C_brokerage#c.C_focus_genres c.C_brokerage#c.C_focus_roles c.C_coreness#c.C_focus_genres c.C_coreness#c.C_focus_roles c.C_coreness c.C_focus_genres c.C_focus_roles role_consolidation experience quality c.team_coreness c.team_quality sequel c.metascore c.average_confusion i.year ///
 Comedy Romance Horror Action  ///
 actor actress producer director writer composer cinematographer editor production_designer, re

margins, at(C_brokerage=(-0.1 0.95) C_focus_genres=(0.1 1)) vsquish pr(nu0)
marginsplot, noci x(C_brokerage) recast(line) xlabel(0.1(0.1)1)
margins, at(C_brokerage=(-0.1 0.95) C_focus_roles=(0.1 1)) vsquish pr( 	nu0)
marginsplot, noci x(C_brokerage) recast(line) xlabel(0.1(0.1)1)
margins, at(C_coreness=(1 10) C_focus_genres=(0.1 1)) vsquish pr(nu0)
marginsplot, noci x(C_coreness) recast(line) xlabel(0(1)10)
margins, at(C_coreness=(1 10) C_focus_roles=(0.1 1)) vsquish pr(nu0)
marginsplot, noci x(C_coreness) recast(line) xlabel(0(1)10)


*esttab using 25May.rtf, replace compress label b(2) se(2) title(RANDOM EFFECTS ESTIMATE OF INDIVIDUAL CREATIVITY) nonumbers mtitles("Main Effects" "Brokerage-Types" "Brokerage-Aspects" "Coreness-Types" "Coreness-Aspects")  addnote("ADD DESCRIPTIONS HERE...") order(c.C_coreness  c.C_brokerage c.C_brokerage#c.C_focus_genres c.C_brokerage#c.C_focus_roles c.C_coreness#c.C_focus_genres c.C_coreness#c.C_focus_roles)  star(* 0.10 ** 0.05 *** 0.01)
esttab using 25May.rtf, replace compress label b(2) se(2) title(RANDOM EFFECTS ESTIMATE OF INDIVIDUAL CREATIVITY) nonumbers mtitles("Main Effects" "Brokerage-Focus" "Coreness-Focus" "All Effects")  addnote("ADD DESCRIPTIONS HERE...") order(c.C_coreness  c.C_brokerage c.C_brokerage#c.C_focus_genres c.C_brokerage#c.C_focus_roles c.C_coreness#c.C_focus_genres c.C_coreness#c.C_focus_roles)  star(* 0.10 ** 0.05 *** 0.01)






