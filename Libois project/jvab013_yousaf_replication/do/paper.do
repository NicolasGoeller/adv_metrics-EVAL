* This Do file replicates the results in "Sticking to one's guns:
* Mass Shootings and the Political Economy of Gun Control in the U.S.”
* by Hasin Yousaf
* Accepted for Publication in the Journal of European Economic Association 

* The do file refers to the following three directories:

* data: includes data files
* tables: includes output tables
* figures: includes output figures

********************************************************************************
clear all
set more off
set matsize 11000

* Define the main directory
global main="C:\Users\Nicolas\OneDrive - Zeppelin-University gGmbH\Dokumente\Studium\Advanced Micrometrics\adv_metrics-EVAL\Libois project\jvab013_yousaf_replication"

* Define data directory
global data="$main\data"

* Define tables directory
global tables="$main\tables"

* Define figures directory
global figures="$main\figures"

********************************************************************************
*** Table 1: Descriptive Statistics: ***
********************************************************************************

use "$data\yearly-obs.dta", clear
sort fips year
estpost tabstat repshare turnout popsd capital_state airport lnincome poverty gini ///
racialhhi mentalhealth pop15t39 nevermarried sc lnhom_gun lnsui_gun ///
lncrime_violent lncrime_proprty, ///
statistics(mean sd p25 p50 p75 min max) columns(statistics)

esttab . using "$tables\table1", cells(mean(fmt(%12.2f)) sd(fmt(%12.2f)) ///
p25(fmt(%12.2f)) p50(fmt(%12.2f)) p75(fmt(%12.2f)) min(fmt(%12.2f)) max(fmt(%12.2f)) ///
) nomtitle nonumber nonote noobs label replace

********************************************************************************
*** Table 2: Predicting MS: ***
********************************************************************************

*Columns 1 to 3:
reg L1.repshare ms popsd if (mse==0 | ms==1), cluster(fips)
outreg2 using "$tables\table2.txt", replace dec(2)
foreach var of varlist turnout popsd capital_state airport lnincome ///
poverty gini racialhhi mentalhealth pop15t39 nevermarried sc ///
lnhom_gun lnsui_gun lncrime_violent lncrime_proprty { 
reg L1.`var' ms popsd if (mse==0 | ms==1), cluster(fips)
outreg2 using "$tables\table2.txt", append dec(2)
}

*Columns 4 to 5:
tabstat repshare turnout popsd capital_state airport lnincome poverty gini ///
racialhhi mentalhealth pop15t39 nevermarried sc lnhom_gun lnsui_gun ///
lncrime_violent lncrime_proprty if F1.failms==1, statistics(mean sd) columns(statistics)
tabstat repshare turnout popsd capital_state airport lnincome poverty gini ///
racialhhi mentalhealth pop15t39 nevermarried sc lnhom_gun lnsui_gun ///
lncrime_violent lncrime_proprty if F1.ms==1, statistics(mean sd) columns(statistics)
foreach var of varlist repshare turnout popsd capital_state airport lnincome ///
poverty gini racialhhi mentalhealth pop15t39 nevermarried sc ///
lnhom_gun lnsui_gun lncrime_violent lncrime_proprty { 
*Columns 6:
reg L1.`var' ms if (ms==1 | failms==1), cluster(fips)
outreg2 using "$tables\table2.txt", append dec(2)
}

********************************************************************************
*** Table 3: Predicting MS: ***
********************************************************************************

reg ms L1.repsharey L1.popsd capital_state airport L1.lnincome L1.poverty L1.gini ///
L1.racialhhi L1.mentalhealth L1.pop15t39 L1.nevermarried L1.sc L1.lnhom_gun ///
L1.lnsui_gun L1.lncrime_violent L1.lncrime_proprty i.year, cluster(fips)
test L1.repsharey capital_state airport L1.lnincome L1.poverty L1.gini ///
L1.nevermarried L1.sc L1.racialhhi L1.pop15t39 L1.lnhom_gun L1.lnsui_gun ///
L1.lncrime_violent L1.lncrime_proprty
outreg2 using "$tables\table3.txt", replace dec(2)

reg ms L1.turnouty L1.popsd capital_state airport L1.lnincome L1.poverty L1.gini ///
L1.racialhhi L1.mentalhealth L1.pop15t39 L1.nevermarried L1.sc  L1.lnhom_gun ///
L1.lnsui_gun L1.lncrime_violent L1.lncrime_proprty i.year, cluster(fips)
test L1.turnouty capital_state airport L1.lnincome L1.poverty L1.gini ///
L1.nevermarried L1.sc L1.racialhhi L1.pop15t39 L1.lnhom_gun L1.lnsui_gun ///
L1.lncrime_violent L1.lncrime_proprty
outreg2 using "$tables\table3.txt", append dec(2)

reg ms L1.repsharey L1.turnouty L1.popsd capital_state airport L1.lnincome ///
L1.poverty L1.gini L1.racialhhi L1.mentalhealth L1.pop15t39 L1.nevermarried L1.sc  ///
L1.lnhom_gun L1.lnsui_gun L1.lncrime_violent L1.lncrime_proprty i.year, cluster(fips)
test L1.repsharey L1.turnouty capital_state airport L1.lnincome L1.poverty L1.gini ///
L1.nevermarried L1.sc L1.racialhhi L1.pop15t39 L1.lnhom_gun L1.lnsui_gun ///
L1.lncrime_violent L1.lncrime_proprty
outreg2 using "$tables\table3.txt", append dec(2)

reg ms L1.repsharey L1.popsd capital_state airport L1.lnincome ///
L1.poverty L1.gini L1.racialhhi L1.mentalhealth L1.pop15t39 L1.nevermarried L1.sc  ///
L1.lnhom_gun L1.lnsui_gun L1.lncrime_violent L1.lncrime_proprty i.year if (mse==1 | failmse==1), cluster(fips)
test L1.repsharey capital_state airport L1.lnincome L1.poverty L1.gini ///
L1.nevermarried L1.sc L1.racialhhi L1.pop15t39 L1.lnhom_gun L1.lnsui_gun ///
L1.lncrime_violent L1.lncrime_proprty
outreg2 using "$tables\table3.txt", append dec(2)

reg ms L1.turnouty L1.popsd capital_state airport L1.lnincome ///
L1.poverty L1.gini L1.racialhhi L1.mentalhealth L1.pop15t39 L1.nevermarried L1.sc  ///
L1.lnhom_gun L1.lnsui_gun L1.lncrime_violent L1.lncrime_proprty i.year if (mse==1 | failmse==1), cluster(fips)
test L1.turnouty capital_state airport L1.lnincome L1.poverty L1.gini ///
L1.nevermarried L1.sc L1.racialhhi L1.pop15t39 L1.lnhom_gun L1.lnsui_gun ///
L1.lncrime_violent L1.lncrime_proprty
outreg2 using "$tables\table3.txt", append dec(2)

reg ms L1.repsharey L1.turnouty L1.popsd capital_state airport L1.lnincome ///
L1.poverty L1.gini L1.racialhhi L1.mentalhealth L1.pop15t39 L1.nevermarried L1.sc  ///
L1.lnhom_gun L1.lnsui_gun L1.lncrime_violent L1.lncrime_proprty i.year if (mse==1 | failmse==1), cluster(fips)
test L1.repsharey L1.turnouty capital_state airport L1.lnincome L1.poverty L1.gini ///
L1.nevermarried L1.sc L1.racialhhi L1.pop15t39 L1.lnhom_gun L1.lnsui_gun ///
L1.lncrime_violent L1.lncrime_proprty
outreg2 using "$tables\table3.txt", append dec(2)

********************************************************************************
*** Table 4: Impact of MS on Republican Presidential Vote Share: ***
********************************************************************************

* Panel A
use "$data\pres-elec.dta", clear
reghdfe repshare mspost, absorb(county year) cluster(state)
outreg2 using "$tables\table4.txt", replace dec(3) keep(mspost)
reghdfe repshare mspost, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table4.txt", append dec(3) keep(mspost)
reghdfe repshare mspost, absorb(county c.popsd##year stateyear) cluster(state)
outreg2 using "$tables\table4.txt", append dec(3) keep(mspost)
reghdfe repshare mspost [aweight=pop], absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table4.txt", append dec(3) keep(mspost)
reghdfe repshare mspost [aweight=pop], absorb(county c.popsd##year stateyear) cluster(state) 
outreg2 using "$tables\table4.txt", append dec(3) keep(mspost)

* Panel B
use "$data\pres-elec-succfail.dta", clear
reghdfe repshare successful postattack if mse==1 | failmse==1, absorb(county year) cluster(state)
outreg2 using "$tables\table4.txt", append dec(3) keep(postattack successful)
reghdfe repshare successful postattack, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table4.txt", append dec(3) keep(postattack successful)
reghdfe repshare successful postattack if mse==1 | failmse==1, absorb(county c.popsd##year stateyear) cluster(state)
outreg2 using "$tables\table4.txt", append dec(3) keep(postattack successful)
reghdfe repshare successful postattack [aweight=pop], absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table4.txt", append dec(3) keep(postattack successful)
reghdfe repshare successful postattack [aweight=pop], absorb(county c.popsd##year stateyear) cluster(state)
outreg2 using "$tables\table4.txt", append dec(3) keep(postattack successful)

********************************************************************************
*** Table 5: Impact of MS on incumbent and turnout: ***
********************************************************************************

*Panel A
use "$data\pres-elec.dta", clear
reghdfe incumbent mspost [aweight=pop], absorb(county c.popsd##year stateyear) cluster(state)
outreg2 using "$tables\table5.txt", replace dec(3) keep(mspost)
reghdfe incumbent mspost [aweight=pop] if incumbent==repshare, absorb(county c.popsd##year stateyear) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(mspost)
reghdfe incumbent mspost [aweight=pop] if incumbent==demshare, absorb(county c.popsd##year stateyear) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(mspost)

reghdfe turnout mspost [aweight=pop], absorb(county c.popsd##year state##year) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(mspost)
reghdfe turnout mspost [aweight=pop] if incumbent==repshare, absorb(county c.popsd##year stateyear) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(mspost)
reghdfe turnout mspost [aweight=pop] if incumbent==demshare, absorb(county c.popsd##year stateyear) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(mspost)

*Panel B
use "$data\pres-elec-succfail.dta", clear

reghdfe incumbent successful postattack [aweight=pop], absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(postattack successful)
reghdfe incumbent successful postattack [aweight=pop] if incumbent==repshare, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(postattack successful)
reghdfe incumbent successful postattack [aweight=pop] if incumbent==demshare, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(postattack successful)

reghdfe turnout successful postattack, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(postattack successful)
reghdfe turnout successful postattack [aweight=pop] if incumbent==repshare, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(postattack successful)
reghdfe turnout successful postattack [aweight=pop] if incumbent==demshare, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table5.txt", append dec(3) keep(postattack successful)

********************************************************************************
*** Table 6: Impact of MS on Republican vote share in other elections: ***
********************************************************************************

*Panel A
use "$data\othr-elec.dta", clear
areg repshare mspost incumbentrep incumbentdem c.popsd##year2 if election=="House", absorb(cd106u) cluster(state)
outreg2 using "$tables\table6.txt", replace dec(3) keep(mspost)
areg repshare mspost incumbentrep incumbentdem c.popsd##year2 [aweight=pop] if election=="House", absorb(cd106u) cluster(state)
outreg2 using "$tables\table6.txt", append dec(3) keep(mspost)
reghdfe repsharesen mspost if election=="Senate", absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table6.txt", append dec(3) keep(mspost)
reghdfe repsharesen mspost [aweight=pop] if election=="Senate", absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table6.txt", append dec(3) keep(mspost)
reghdfe repsharegov mspost if election=="Gubernatorial", absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table6.txt", append dec(3) keep(mspost)
reghdfe repsharegov mspost [aweight=pop] if election=="Gubernatorial", absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table6.txt", append dec(3) keep(mspost)

*Panel B
use "$data\othr-elec-succfail.dta", clear
areg repshare postattack successful incumbentrep incumbentdem c.popsd##year2 if election=="House", absorb(cd106u) cluster(state)
outreg2 using "$tables\table6.txt", append dec(3) keep(postattack successful)
areg repshare postattack successful incumbentrep incumbentdem c.popsd##year2 [aweight=pop] if election=="House", absorb(cd106u) cluster(state)
outreg2 using "$tables\table6.txt", append dec(3) keep(postattack successful)
reghdfe repsharesen successful postattack if election=="Senate", absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table6.txt", append dec(3) keep(postattack successful)
reghdfe repsharesen successful postattack [aweight=pop] if election=="Senate", absorb(county c.popsd##year) cluster(state) 
outreg2 using "$tables\table6.txt", append dec(3) keep(postattack successful)
reghdfe repsharegov successful postattack if election=="Gubernatorial", absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\table6.txt", append dec(3) keep(postattack successful)
reghdfe repsharegov successful postattack [aweight=pop] if election=="Gubernatorial", absorb(county c.popsd##year) cluster(state) 
outreg2 using "$tables\table6.txt", append dec(3) keep(postattack successful)

********************************************************************************
*** Table 7: Effect of MS on importance and preference for gun policy: ***
********************************************************************************

use "$data\survey.dta", clear
global controlsanes = "white black i.educ i.income married age agesq religious"
global controlscces = "i.year i.race i.gender i.educ i.married i.pray i.employed i.income regvoter homeowner i.partyid_7pt age agesq"

* Column 1
areg impgun_ord mspost $controlsanes i.year [aweight=weight] if survey=="ANES", absorb(cd106u) cluster(cd106u)
outreg2 using "$tables\table7.txt", replace dec(3) keep(mspost)
summ impgun_ord [aweight=weight] if survey=="ANES"

* Columns 2 to 6
areg inc_gc mspost $controlscces [aweight=weight] if (inc_gc==1 | sam_gc==1) & survey=="CCES", absorb(fips) cluster(state)
outreg2 using "$tables\table7.txt", append dec(3) keep(mspost)
areg backgroundcheck_for mspost $controlscces [aweight=weight] if survey=="CCES", absorb(fips) cluster(state)
outreg2 using "$tables\table7.txt", append dec(3) keep(mspost)
areg banassault_for mspost $controlscces [aweight=weight] if survey=="CCES", absorb(fips) cluster(state)
outreg2 using "$tables\table7.txt", append dec(3) keep(mspost)
areg concealedcarry_against mspost $controlscces [aweight=weight] if survey=="CCES", absorb(fips) cluster(state)
outreg2 using "$tables\table7.txt", append dec(3) keep(mspost)
areg ownerlist_against mspost $controlscces [aweight=weight] if survey=="CCES", absorb(fips) cluster(state)
outreg2 using "$tables\table7.txt", append dec(3) keep(mspost)
summ backgroundcheck_for banassault_for concealedcarry_against ownerlist_against [aweight=weight] if survey=="CCES"

********************************************************************************
*** Table 8: Effect of MS on political polarization among voters: ***
********************************************************************************

use "$data\survey.dta", clear
global controlsanes = "white black i.educ i.income married age agesq religious"
global controlscces = "i.year i.race i.gender i.educ i.married i.pray i.employed i.income regvoter homeowner i.partyid_7pt age agesq"

* Column 1
areg impgun_ord mspost mspostrep mspostdem rep dem $controlsanes i.year [aweight=weight] , absorb(cd106u) cluster(cd106u)
outreg2 using "$tables\table8.txt", replace dec(3) keep(mspost mspostrep mspostdem)
summ impgun_ord [aweight=weight] if survey=="ANES"

* Columns 2 to 6
areg inc_gc mspost mspostrep mspostdem rep dem $controlscces [aweight=weight] if (inc_gc==1 | sam_gc==1) & survey=="CCES", absorb(fips) cluster(state)
outreg2 using "$tables\table8.txt", append dec(3) keep(mspost mspostrep mspostdem)
areg backgroundcheck_for mspost mspostrep mspostdem rep dem $controlscces [aweight=weight] if survey=="CCES", absorb(fips) cluster(state)
outreg2 using "$tables\table8.txt", append dec(3) keep(mspost mspostrep mspostdem)
areg banassault_for mspost mspostrep mspostdem rep dem $controlscces [aweight=weight] if survey=="CCES", absorb(fips) cluster(state)
outreg2 using "$tables\table8.txt", append dec(3) keep(mspost mspostrep mspostdem)
areg concealedcarry_against mspost mspostrep mspostdem rep dem $controlscces [aweight=weight] if survey=="CCES", absorb(fips) cluster(state)
outreg2 using "$tables\table8.txt", append dec(3) keep(mspost mspostrep mspostdem)
areg ownerlist_against mspost mspostrep mspostdem rep dem $controlscces [aweight=weight] if survey=="CCES", absorb(fips) cluster(state)
outreg2 using "$tables\table8.txt", append dec(3) keep(mspost mspostrep mspostdem)
summ backgroundcheck_for banassault_for concealedcarry_against ownerlist_against if survey=="CCES" & rep==1 [aweight=weight]
summ backgroundcheck_for banassault_for concealedcarry_against ownerlist_against if survey=="CCES" & dem==1 [aweight=weight]

********************************************************************************
*** Table 9: Effect of MS on politicians' roll call voting: ***
********************************************************************************

use "$data\rollcall.dta", clear
xtreg coord1d_gun mspost lnpop i.year dem incumbent repshare, cluster(state) fe
outreg2 using "$tables\table9.txt", replace tex(frag) dec(3) keep(mspost) label nonotes nocons noni
xtreg coord1d_gun mspost mspostdem lnpop i.year dem incumbent repshare, cluster(state) fe
outreg2 using "$tables\table9.txt", append tex(frag) dec(3) keep(mspost mspostdem) label nonotes nocons noni
xtreg coord1d_gun mspost dem pnra_totl1 mspostdem mspostpnra_tot dempnra_tot mspostdempnra_tot lnpop i.year incumbent repshare, fe
outreg2 using "$tables\table9.txt", append tex(frag) dec(3) keep(mspost mspostdem mspostpnra_tot mspostdempnra_tot) label nonotes nocons noni
xtreg coord1d_gun mspost dem mspostswingcd demswingcd swingcd mspostdem mspostdemswingcd lnpop i.year incumbent repshare, cluster(state) fe
outreg2 using "$tables\table9.txt", append tex(frag) dec(3) keep(mspost mspostdem mspostswingcd mspostdemswingcd) label nonotes nocons noni
xtreg coord1d_gun mspost mspostdem lnpop i.year dem incumbent repshare if incumbentwon==1, cluster(state) fe
outreg2 using "$tables\table9.txt", append tex(frag) dec(3) keep(mspost mspostdem) label nonotes nocons noni
xtreg coord1d_gun mspost mspostdem lnpop i.year dem incumbent repshare if incumbentwon==0, cluster(state) fe
outreg2 using "$tables\table9.txt", append tex(frag) dec(3) keep(mspost mspostdem) label nonotes nocons noni

********************************************************************************
*** Figure 1: Evolution of MS over time: ***
********************************************************************************

use "$data\yearly-obs.dta", clear
duplicates drop fips year, force
bysort year: egen msinyr = sum(ms)
duplicates drop year, force
twoway bar msinyr year, xlabel(2000(4)2016) ylabel(0(5)30) xtitle("Year") ytitle("Number of MS") scheme(s2mono)
graph save Graph "$figures\figure1.gph", replace
graph export "$figures\figure1.pdf", as(pdf) replace
capture: erase "$figures\figure1.gph"

********************************************************************************
*** Figure 2: Evolution of preference over gun policy over time: ***
********************************************************************************

insheet using "$data\gun_control_right_pew.txt", tab clear
collapse right* control*, by(year)
foreach var of varlist right* control* {
replace `var' = ceil(`var')
}
twoway (connected right_rep year) (connected control_dem year, xlabel(1995(5)2015) ylabel(30(10)90) legend(label(1 "REP for Gun Right") label(2 "DEM for Gun Control")) ///
xtitle("Year") ytitle("% supporting") graphregion(color(gs15)) scheme(s2mono))
graph save Graph "$figures\figure2.gph", replace
graph export "$figures\figure2.pdf", as(pdf) replace
capture: erase "$figures\figure2.gph"

********************************************************************************
********************************************************************************
********************************************************************************
*** Appendix ***
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
*** Table A1: Impact of MS on Republican vote share: ***
********************************************************************************

use "$data\pres-elec.dta", clear
reghdfe repshare mspost, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA1.txt", replace dec(3)
reghdfe repshare mspost, absorb(county c.popsd##year c.popsdsq##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost, absorb(county c.lnpop##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost, absorb(county i.popqrt##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost, absorb(county c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost, absorb(county c.popsd##year c.popsdsq##year state##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost, absorb(county c.lnpop##year state##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost, absorb(county i.popqrt##year state##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost [aweight=lnpop], absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost [aweight=lnpop], absorb(county c.popsd##year c.popsdsq##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost [aweight=lnpop], absorb(county c.lnpop##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost [aweight=lnpop], absorb(county i.popqrt##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost [aweight=lnpop], absorb(county c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost [aweight=lnpop], absorb(county c.popsd##year c.popsdsq##year state##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost [aweight=lnpop], absorb(county c.lnpop##year state##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)
reghdfe repshare mspost [aweight=lnpop], absorb(county i.popqrt##year state##year) cluster(state)
outreg2 using "$tables\tableA1.txt", append dec(3)

********************************************************************************
*** Table A2: Impact of MS on Republican vote share: ***
********************************************************************************

use "$data\pres-elec-succfail.dta", clear
reghdfe repshare successful postattack, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA2.txt", replace dec(3)
reghdfe repshare successful postattack, absorb(county c.popsd##year c.popsdsq##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack, absorb(county c.lnpop##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack, absorb(county i.popqrt##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack, absorb(county c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack, absorb(county c.popsd##year c.popsdsq##year state##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack, absorb(county c.lnpop##year state##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack, absorb(county i.popqrt##year state##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack [aweight=lnpop], absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack [aweight=lnpop], absorb(county c.popsd##year c.popsdsq##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack [aweight=lnpop], absorb(county c.lnpop##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack [aweight=lnpop], absorb(county i.popqrt##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack [aweight=lnpop], absorb(county c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack [aweight=lnpop], absorb(county c.popsd##year c.popsdsq##year state##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack [aweight=lnpop], absorb(county c.lnpop##year state##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)
reghdfe repshare successful postattack [aweight=lnpop], absorb(county i.popqrt##year state##year) cluster(state)
outreg2 using "$tables\tableA2.txt", append dec(3)

********************************************************************************
*** Table A3: Impact of MS on Republican vote share (Other data sources): ***
********************************************************************************

use "$data\pres-elec.dta", clear
reghdfe repshare ustoday_post if year>=2004 & year<=2016 [aweight=pop], absorb(county year c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA3.txt", replace dec(3)
reghdfe repshare brady_post if year>=2004 & year<=2016 [aweight=pop], absorb(county year c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA3.txt", append dec(3)
reghdfe repshare gva_post if year>=2012 & year<=2016 [aweight=pop], absorb(county year c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA3.txt", append dec(3)
reghdfe repshare fbi_post [aweight=pop], absorb(county year c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA3.txt", append dec(3)
reghdfe repshare all_post [aweight=pop], absorb(county year c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA3.txt", append dec(3)

********************************************************************************
*** Table A4: Impact of MS on Republican vote share over time: ***
********************************************************************************

use "$data\pres-elec.dta", clear
reghdfe repshare timeperiod2-timeperiod4 timeperiod6-timeperiod9, absorb(county c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA4.txt", replace dec(3)
reghdfe repshare timeperiod2-timeperiod4 timeperiod6-timeperiod9, absorb(county c.popsd##year c.popsdsq##year state##year) cluster(state)
outreg2 using "$tables\tableA4.txt", append dec(3)
reghdfe repshare timeperiod2-timeperiod4 timeperiod6-timeperiod9, absorb(county c.lnpop##year state##year) cluster(state)
outreg2 using "$tables\tableA4.txt", append dec(3)
reghdfe repshare timeperiod2-timeperiod4 timeperiod6-timeperiod9, absorb(county i.popqrt##year state##year) cluster(state)
outreg2 using "$tables\tableA4.txt", append dec(3)

********************************************************************************
*** Table A5: How local is the impact of MS: ***
********************************************************************************

use "$data\geospillover.dta", clear
reghdfe repshare mspost mswithin50post, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA5.txt", replace dec(3)
reghdfe repshare mspost mswithin75post, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA5.txt", append dec(3)
reghdfe repshare mspost mswithin100post, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA5.txt", append dec(3)
reghdfe repshare mspost mswithin125post, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA5.txt", append dec(3)
reghdfe repshare mspost mswithin150post, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA5.txt", append dec(3)
reghdfe repshare mspost mswithincdpost, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA5.txt", append dec(3)
reghdfe repshare mspost mswithinpumapost, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA5.txt", append dec(3)
reghdfe repshare mspost mswithinczonepost, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA5.txt", append dec(3)
reghdfe repshare mspost mswithinstatepost, absorb(county c.popsd##year) cluster(state)
outreg2 using "$tables\tableA5.txt", append dec(3)

********************************************************************************
*** Table A6: The role of media coverage: ***
********************************************************************************

use "$data\media-data.dta", clear
reghdfe repshare mspost mspostallany lnTotal_city_minus_ms victimsm , absorb(county c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA6.txt", replace dec(3) keep(mspost mspostallany)
reghdfe repshare mspost mspostall lnTotal_city_minus_ms victimsm, absorb(county c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA6.txt", append dec(3) keep(mspost mspostall)
reghdfe repshare mspost mspostallmin lnTotal_city_minus_ms victimsm, absorb(county c.popsd##year state##year) cluster(state)
outreg2 using "$tables\tableA6.txt", append dec(3) keep(mspost mspostallmin)

********************************************************************************
*** Table A7: Descriptive Statistics of ANES data: ***
********************************************************************************

use "$data\survey.dta", clear
global controlsanes = "white black i.educ i.income married age agesq religious"
global controlscces = "i.year i.race i.gender i.educ i.married i.pray i.employed i.income regvoter homeowner i.partyid_7pt age agesq"
quietly: areg impgun_ord mspost mspostrep mspostdem rep dem $controlsanes i.year [aweight=weight] , absorb(cd106u) cluster(cd106u)
g esample = (e(sample)==1)
estpost tabstat impgun_ord guncontrol_increase rep dem ///
white black educ income married age religious [aweight=weight] if survey=="ANES" & esample==1, statistics(count mean sd p25 p50 p75 min max) columns(statistics)
esttab . using "$tables\tableA7", cells(count(fmt(%12.2f)) mean(fmt(%12.2f)) sd(fmt(%12.2f)) ///
p25(fmt(%12.2f)) p50(fmt(%12.2f)) p75(fmt(%12.2f)) min(fmt(%12.2f)) max(fmt(%12.2f)) ///
) nomtitle nonumber nonote noobs label replace

********************************************************************************
*** Table A8: Descriptive Statistics of CCES data: ***
********************************************************************************

use "$data\survey.dta", clear
global controlsanes = "white black i.educ i.income married age agesq religious"
global controlscces = "i.year i.race i.gender i.educ i.married i.pray i.employed i.income regvoter homeowner i.partyid_7pt age agesq"
quietly: areg backgroundcheck_for mspost mspostrep mspostdem rep dem $controlscces [aweight=weight] if survey=="CCES", absorb(fips) cluster(state)
g esample = (e(sample)==1)
quietly: areg inc_gc mspost mspostrep mspostdem rep dem $controlscces [aweight=weight] if survey=="CCES" & (inc_gc==1 | sam_gc==1), absorb(fips) cluster(state)
g esample2 = (e(sample)==1)
estpost tabstat inc_gc backgroundcheck_for banassault_for concealedcarry_against ownerlist_against ///
male age regvoter white black educ income employed homeowner married pray partyid_7pt [aweight=weight] if survey=="CCES" & (esample==1 | esample2==1), ///
statistics(count mean sd p25 p50 p75 min max) columns(statistics)
esttab . using "$tables\tableA8", cells(count(fmt(%12.2f)) mean(fmt(%12.2f)) sd(fmt(%12.2f)) ///
p25(fmt(%12.2f)) p50(fmt(%12.2f)) p75(fmt(%12.2f)) min(fmt(%12.2f)) max(fmt(%12.2f)) ///
) nomtitle nonumber nonote noobs label replace

********************************************************************************
*** Table A9: Impact of MS on other issues: ***
********************************************************************************

use "$data\survey.dta", clear
append using "$data\anes_pre2010.dta"
replace survey="Pre 2010" if survey==""
global controlsanes = "white black i.educ i.income married age agesq religious"
global controlscces = "i.year i.race i.gender i.educ i.married i.pray i.employed i.income regvoter homeowner i.partyid_7pt age agesq"
reg impenviron_ord mspost $controlsanes i.year [aweight=weight] if survey!="CCES", cluster(cd106u)
outreg2 using "$tables\tableA9.txt", replace dec(3) keep(mspost)
reg impillimm_ord mspost $controlsanes i.year [aweight=weight] if survey!="CCES", cluster(cd106u)
outreg2 using "$tables\tableA9.txt", append dec(3) keep(mspost)
areg impspending_ord mspost $controlsanes i.year [aweight=weight] if survey!="CCES", absorb(cd106u) cluster(cd106u)
outreg2 using "$tables\tableA9.txt", append dec(3) keep(mspost)
areg spendingincrease mspost $controlsanes i.year [aweight=weight] if survey!="CCES", absorb(cd106u) cluster(cd106u)
outreg2 using "$tables\tableA9.txt", append dec(3) keep(mspost)
reg environincrease mspost $controlsanes i.year [aweight=weight] if survey!="CCES", cluster(cd106u)
outreg2 using "$tables\tableA9.txt", append dec(3) keep(mspost)
reg immiincrease mspost $controlsanes i.year [aweight=weight] if survey!="CCES", cluster(cd106u)
outreg2 using "$tables\tableA9.txt", append dec(3) keep(mspost)
areg spendingdecrease mspost $controlsanes i.year [aweight=weight] if survey!="CCES", absorb(cd106u) cluster(cd106u)
outreg2 using "$tables\tableA9.txt", append dec(3) keep(mspost)
reg environdecrease mspost $controlsanes i.year [aweight=weight] if survey!="CCES", cluster(cd106u)
outreg2 using "$tables\tableA9.txt", append dec(3) keep(mspost)
reg immidecrease mspost $controlsanes i.year [aweight=weight] if survey!="CCES", cluster(cd106u)
outreg2 using "$tables\tableA9.txt", append dec(3) keep(mspost)

********************************************************************************
*** Figure A1: Impact of MS depending on distance from MS: ***
********************************************************************************
use "$data\geospillover.dta", clear
g rps=.
g beta=.
g se=.
reghdfe repshare mspost mspostdistmin mspostdistminsq mspostdistmincu mspostdistminqu distancemin distanceminsq distancemincu distanceminqu [aweight=lnpop], absorb(county c.popsd##year state##year) cluster(state)
forvalues j=1(10)201 {
local i = `j'
lincom mspost + `i'*mspostdistmin + `i'*`i'*mspostdistminsq + `i'*`i'*`i'*mspostdistmincu + `i'*`i'*`i'*`i'*mspostdistminqu
replace rps = `i' in `j'
replace beta = `r(estimate)' in `j'
replace se = `r(se)' in `j'
}
g lb = beta - 1.96*se
g ub = beta + 1.96*se
label variable beta "Coefficient"
label variable lb "95% CI"
label variable ub "95% CI"

preserve
duplicates drop rps, force
drop if rps==.
sort rps
two (line lb rps, lpattern(dash)) (line ub rps, lpattern(dash))  (line beta rps, lpattern(solid) yline(0) xlabel(0(25)200) ylabel(-0.06(0.02)0.06) ///
xtitle("Distance from closest MS") ytitle("Impact of Mass Shooting on REP vote share") legend(order(3 1)) scheme(s2mono) )  
graph save Graph "$figures\figureA1.gph", replace
graph export "$figures\figureA1.pdf", as(pdf) replace
capture: erase "$figures\figureA1.gph"

********************************************************************************
*** Figure A2: Distribution from Falsification Exercise: ***
********************************************************************************

use "$data\placebo.dta", clear

hist betast, xline(0.024, lwidth(1.25)) percent xtitle("Coefficient on Fake MS*Post") xlabel(-0.025(0.005)0.025) scheme(s2mono)
graph save Graph "$figures\figureA2.gph", replace
graph export "$figures\figureA2.pdf", as(pdf) replace
capture: erase "$figures\figureA2.gph"

********************************************************************************
*** Figure A3: Impact of MS over time: ***
********************************************************************************

use "$data\pres-elec.dta", clear
g beta1 = .
g se1=.

reghdfe repshare timeperiod2-timeperiod4 timeperiod6-timeperiod9, absorb(county i.popqrt##year state##year) cluster(state)
forvalues j=2(1)9{
capture: replace beta1=_b[timeperiod`j'] if timeperiod`j'==1
capture: replace se1=_se[timeperiod`j'] if timeperiod`j'==1
}
replace beta1=0 if timeperiod5==1
replace se1=0 if timeperiod5==1
g lbar1 = beta1 - 1.96*se1
g ubar1 = beta1 + 1.96*se1
replace lbar1=0 if timeperiod5==1
replace ubar1=0 if timeperiod5==1
g timesince=-4*(timeperiod2==1)-3*(timeperiod3==1)-2*(timeperiod4==1)-1*(timeperiod5==1)+1*(timeperiod7==1)+2*(timeperiod8==1)+3*(timeperiod9==1)
replace timesince=-99 if timeperiod1==1
duplicates drop timesince, force
drop if beta1==.
label variable beta1 "Estimate"
label variable lbar1 "95% CI"
label variable ubar1 "95% CI"
twoway (scatter beta1 timesince) (rcap lbar1 ubar1 timesince, yline(0, lcolor(black)) ), ///
xtitle("Elections since Mass Shooting") ytitle("Impact of Mass Shooting on REP vote share") ///
ylabel(-0.06(0.01)0.06) xlabel(-4(1)3) legend(on) scheme(s2mono)
graph save Graph "$figures\figureA3.gph", replace
graph export "$figures\figureA3.pdf", as(pdf) replace
capture: erase "$figures\figureA3.gph"

********************************************************************************
*** Figure A4: Heterogeneous Impact of Mass Shootings on Republican Vote share: ***
********************************************************************************

use "$data\geospillover.dta", clear
g rps=.
g beta=.
g se= .

reghdfe repshare mspost mspostmnthelec mspostmnthelecsq, absorb(county c.popsd##year state##year) cluster(state)
forvalues j=1(1)19 {
local i = -`j' + 1
lincom mspost + `i'*mspostmnthelec + `i'*`i'*mspostmnthelecsq
replace rps = `i' in `j'
replace beta = `r(estimate)' in `j'
replace se = `r(se)' in `j'
}
g lb = beta - 1.96*se
g ub = beta + 1.96*se
label variable beta "Coefficient"
label variable lb "95% CI"
label variable ub "95% CI"

duplicates drop rps, force
drop if rps==.
sort rps
twoway (line lb rps, lpattern(dash)) (line ub rps, lpattern(dash))  (line beta rps, lpattern(solid) yline(0) xlabel(-18(2)0) ylabel(-0.06(0.02)0.06) ///
xtitle("Months before the election") ytitle("Impact of Mass Shooting on REP vote share") legend(order(3 1)) scheme(s2mono) ) 
graph save Graph "$figures\figureA4a.gph", replace
graph export "$figures\figureA4a.pdf", as(pdf) replace
capture: erase "$figures\figureA4a.gph"

use "$data\geospillover.dta", clear
g rps=.
g beta=.
g se= .

reghdfe repshare mspost mspostmargin mspostmarginsq mspostmargincu mspostmarginqu, absorb(county c.popsd##year state##year) cluster(state)
forvalues j=1(1)50 {
local i = (`j'-25)/100
lincom mspost + `i'*mspostmargin + `i'*`i'*mspostmarginsq + `i'*`i'*`i'*mspostmarginsq + `i'*`i'*`i'*`i'*mspostmarginsq
replace rps = `i' in `j'
replace beta = `r(estimate)' in `j'
replace se = `r(se)' in `j'
}
g lb = beta - 1.96*se
g ub = beta + 1.96*se
label variable beta "Coefficient"
label variable lb "95% CI"
label variable ub "95% CI"

duplicates drop rps, force
drop if rps==.
sort rps
twoway (line lb rps, lpattern(dash)) (line ub rps, lpattern(dash))  (line beta rps, lpattern(solid) yline(0, lcolor(black)) xlabel(-0.25(0.05)0.25) ylabel(-0.12(0.03)0.06) ///
xtitle("REP Victory Margin in the past") ytitle("Impact of Mass Shooting on REP vote share") legend(order(3 1)) scheme(s2mono) )
graph save Graph "$figures\figureA4b.gph", replace
graph export "$figures\figureA4b.pdf", as(pdf) replace
capture: erase "$figures\figureA4b.gph"
