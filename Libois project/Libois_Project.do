/*
ADVANCED MICROECONOMETRICS - 2022 

OUTLIER- ROBUST REPLICATION: YOUASAF (2021) - "STICKING TO ONE'S GUNS: MASS SHOOTINGS AND THE POLITICAL ECONOMY OF GUN CONTROL IN THE UNITED STATES"

PROJECT BY: MUDIGONDA SARATH CHANDRA & NICOLAS GOELLER 

SUBMITTED TO : PROFESSOR FRANCOIS LIBOIS 

PARIS SCHOOL OF ECONOMICS
*/

program drop _all 
/*ssc install robbox
ssc install robreg
ssc install moremata
ssc install outreg2
ssc install mmregress
ssc install xtrobreg
ssc install robmv */

// directory
global data "/Users/sarathchandra/Desktop/M2/Adv Microeconometrics /Endterm Project/Libois/Replication/data"
global results "/Users/sarathchandra/Desktop/M2/Adv Microeconometrics /Endterm Project/Libois/Results"
global tables "/Users/sarathchandra/Desktop/M2/Adv Microeconometrics /Endterm Project/Libois/Tables"
global var1 "repshare poverty lncrime_violent turnout racialhhi lnhom_gun"
global var2 "repshare turnout lnincome poverty gini racialhhi mentalhealth pop15t39 nevermarried sc lnhom_gun lnsui_gun lncrime_violent lncrime_proprty"

u "$data/yearly-obs.dta", clear 


/* Generalized boxplots - not shown in report, plotted for better understanding the data*/

foreach var of varlist repshare poverty lncrime_violent turnout racialhhi lnhom_gun{
	robbox `var', general over(ms)  ytitle("`var'") xtitle("mass shootings")
	gr save "$results/`var'", replace
}

cd "$results"
gr combine repshare.gph poverty.gph lncrime_violent.gph turnout.gph racialhhi.gph lnhom_gun.gph 
gr save "$results/gen_boxplots",replace 
gr export "$results/gen_boxplots.png", replace 


// Figure 3 - generalised boxplot for standardised population to determine the threshold 
robbox popsd, general  ytitle("standardised population") xtitle("mass shootings") //upper limit for outlier = 11.23
gr export "$results/population.png", replace 


// TABLE 6 - identification assumption - strategy 1
est clear 

// column 1 to 3  
eststo : robreg mm L1.repshare ms popsd if (mse==0 | ms==1), cluster(fips)
foreach var of varlist turnout lnincome ///
poverty gini racialhhi mentalhealth pop15t39 nevermarried sc ///
lnhom_gun lnsui_gun lncrime_violent lncrime_proprty { 
eststo : robreg mm L1.`var' ms popsd if (mse==0 | ms==1), cluster(fips)
} 


//column 4 and 5
estpost tabstat repshare turnout capital_state airport lnincome poverty gini ///
racialhhi mentalhealth pop15t39 nevermarried sc lnhom_gun lnsui_gun ///
lncrime_violent lncrime_proprty if F1.failms==1, statistics(median sd) columns(statistics)
estpost tabstat repshare turnout capital_state airport lnincome poverty gini ///
racialhhi mentalhealth pop15t39 nevermarried sc lnhom_gun lnsui_gun ///
lncrime_violent lncrime_proprty if F1.ms==1, statistics(median sd) columns(statistics)

esttab using "$results/table2main.tex", replace star compress 

// TABLE 1: Impact of mass shootings on Republican Presidential vote share
clear all 
u "$data/pres-elec.dta"


eststo : reghdfe repshare mspost [aweight=pop], absorb(county c.popsd##year) cluster(state) 
	estadd local popyear "YES"
	estadd local weightpop "YES"

xtset, clear
xtset county year

estimates clear
// panel A

//// we run a loop for the Hausmann tests until we find the highest efficiency for which the null hypothesis is not rejected and use this level of efficiency to run the robust regression. 
 
forvalues v = 99(-5)30 {
  quietly xtrobreg mm repshare mspost*, cluster(state) eff(`v')
  quietly ereturn list
  di "repshare `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list



eststo : xtrobreg mm repshare mspost*, cluster(state) eff(98.99)

// panel B 
use "$data/pres-elec-succfail.dta", clear 
eststo : reghdfe repshare successful postattack [aweight=pop], absorb(county c.popsd##year stateyear) cluster(state)
	estadd local popyear "YES"
	estadd local stateyear "YES"
	estadd local weightpop "YES"

eststo : reghdfe repshare successful postattack [aweight=pop], absorb(county c.popsd##year) cluster(state)
	estadd local popyear "YES"
	estadd local weightpop "YES"

xtset, clear  

xtset county year

forvalues v = 99(-5)30 {
  quietly xtrobreg mm repshare successful postattack if mse==1 | failmse == 1 , cluster(state) eff(`v')
  quietly ereturn list
  di "repshare `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list


eststo : xtrobreg mm repshare successful postattack if mse==1 | failmse == 1 , cluster(state) eff(98.99)

esttab using "$results/table 4.tex", replace title("Impact of mass shooting on Republican presidential vote share") scalars(popyear "PopulationYear FEs" stateyear "StateYear FEs" weightpop "Population weighted")

// Table 2 - Strategy 1 - incumbent vote share
use "$data/pres-elec.dta", clear
est clear 
//// paper's results
eststo : reghdfe incumbent mspost [aweight=pop], absorb(county c.popsd##year stateyear) cluster(state)

eststo : reghdfe incumbent mspost [aweight=pop] if incumbent==repshare, absorb(county c.popsd##year stateyear) cluster(state)

eststo : reghdfe incumbent mspost [aweight=pop] if incumbent==demshare, absorb(county c.popsd##year stateyear) cluster(state)

//// robust regressions 
xtset, clear  
xtset county year

forvalues v = 99(-5)30 {
  quietly xtrobreg mm incumbent mspost*, cluster(state) eff(`v')
  quietly ereturn list
  di "incumbent `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list

eststo : xtrobreg mm incumbent mspost*, cluster(state) eff(78.99)

forvalues v = 99(-5)30 {
  quietly xtrobreg mm incumbent mspost if incumbent==repshare, cluster(state) eff(`v')
  quietly ereturn list
  di "incumbent `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list

eststo : xtrobreg mm incumbent mspost if incumbent==repshare, cluster(state) eff(98.99)

forvalues v = 99(-5)30 {
  quietly xtrobreg mm incumbent mspost if incumbent==demshare, cluster(state) eff(`v')
  quietly ereturn list
  di "incumbent `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list

eststo : xtrobreg mm incumbent mspost if incumbent==demshare, cluster(state) eff(98.99)

esttab using "$results/table5A.tex", replace title("Effect of Mass Shootings on Incumbent Vote Share Sorted by Incumbent - Strategy 1")

// Table 3 - strategy 1 - turnout 
use "$data/pres-elec.dta", clear
est clear 

//// Paper's results 
eststo : reghdfe turnout mspost [aweight=pop], absorb(county c.popsd##year state##year) cluster(state)

eststo : reghdfe turnout mspost [aweight=pop] if incumbent==repshare, absorb(county c.popsd##year stateyear) cluster(state)

eststo : reghdfe turnout mspost [aweight=pop] if incumbent==demshare, absorb(county c.popsd##year stateyear) cluster(state)

//// Robust regressions 
xtset, clear  
xtset county year

forvalues v = 99(-5)30 {
  quietly xtrobreg mm turnout mspost*, cluster(state) eff(`v')
  quietly ereturn list
  di "turnout `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list

eststo : xtrobreg mm turnout mspost*, cluster(state) eff(54)

forvalues v = 99(-5)30 {
  quietly xtrobreg mm turnout mspost if incumbent==repshare, cluster(state) eff(`v')
  quietly ereturn list
  di "turnout `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list

eststo : xtrobreg mm turnout mspost if incumbent==repshare, cluster(state) eff(98.99)

forvalues v = 99(-5)30 {
  quietly xtrobreg mm turnout mspost if incumbent==demshare, cluster(state) eff(`v')
  quietly ereturn list
  di "turnout `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list


eststo : xtrobreg mm turnout mspost if incumbent==demshare, cluster(state) eff(98.99)

esttab using "$results/table5B.tex", replace title("Effect of Mass Shootings on Turnout Sorted by Incumbent - Strategy 1")


// Table 4 - - Strategy 2 - incumbent vote share
use "$data/pres-elec-succfail.dta", clear
est clear

//// paper's results
eststo : quietly : reghdfe incumbent successful postattack [aweight=pop], absorb(county c.popsd##year) cluster(state)

eststo : quietly : reghdfe incumbent successful postattack [aweight=pop] if incumbent==repshare, absorb(county c.popsd##year) cluster(state)

eststo : quietly : reghdfe incumbent successful postattack [aweight=pop] if incumbent==demshare, absorb(county c.popsd##year) cluster(state)

//// Robust regressions 
xtset, clear  
xtset county year

forvalues v = 99(-5)30 {
  quietly xtrobreg mm incumbent successful postattack, cluster(state) eff(`v')
  quietly ereturn list
  di "incumbent `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list

eststo : xtrobreg mm incumbent successful postattack, cluster(state) eff(98.99)

forvalues v = 99(-5)30 {
  quietly xtrobreg mm incumbent successful postattack, cluster(state) eff(`v')
  quietly ereturn list
  di "incumbent `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list


eststo : xtrobreg mm incumbent successful postattack if incumbent==repshare, cluster(state) eff(98.99)

forvalues v = 99(-5)30 {
  quietly xtrobreg mm incumbent successful postattack if incumbent==demshare, cluster(state) eff(`v')
  quietly ereturn list
  di "incumbent `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list


eststo : xtrobreg mm incumbent successful postattack if incumbent==demshare, cluster(state) eff(98.99)

esttab using "$results/table5C.tex", replace title("Effect of Mass Shootings on Incumbent Vote Share Sorted by Incumbent - Strategy 2")

// Table 5 - - Strategy 2 -  turnout
use "$data/pres-elec-succfail.dta", clear
est clear

//// paper's results
eststo : quietly : reghdfe turnout successful postattack, absorb(county c.popsd##year) cluster(state)

eststo : quietly : reghdfe turnout successful postattack [aweight=pop] if incumbent==repshare, absorb(county c.popsd##year) cluster(state)

eststo : quietly : reghdfe turnout successful postattack [aweight=pop] if incumbent==demshare, absorb(county c.popsd##year) cluster(state)

//// Robust regressions
xtset, clear  
xtset county year

forvalues v = 99(-5)30 {
  quietly xtrobreg mm turnout successful postattack, cluster(state) eff(`v')
  quietly ereturn list
  di "turnout `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list

eststo : xtrobreg mm turnout successful postattack, cluster(state) eff(38.99)

forvalues v = 99(-5)30 {
  quietly xtrobreg mm turnout successful postattack if incumbent==repshare, cluster(state) eff(`v')
  quietly ereturn list
  di "turnout `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list

eststo : xtrobreg mm turnout successful postattack if incumbent==repshare, cluster(state) eff(98.99)

forvalues v = 99(-5)30 {
  quietly xtrobreg mm turnout successful postattack if incumbent==demshare, cluster(state)  eff(`v')
  quietly ereturn list
  di "turnout `v'" 
  if `e(hausman_p)' > 0.05 {
  di "`e(hausman_p)' break "
  continue, break 
  } 
  else {
  di "`e(hausman_p)'< 0.05"
  } 
}
ereturn list

eststo : xtrobreg mm turnout successful postattack if incumbent==demshare, cluster(state) eff(98.99)

esttab using "$results/table5D.tex", replace title("Effect of Mass Shootings on Turnout Sorted by Incumbent - Strategy 2")




// Heterogeniety - Figure 4
use "$data/geospillover.dta", clear
g rps=.
g beta=.
g se= .

// A4A
quietly : xtrobreg mm repshare mspost mspostmnthelec mspostmnthelecsq, cluster(state)
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
xtitle("Months before the election") legend(order(3 1)) scheme(s2mono) title("Using Robust Regressions")) 
graph save Graph "$results/figA4aMy.gph", replace

use "$data/geospillover.dta", clear
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
xtitle("Months before the election") legend(order(3 1)) scheme(s2mono)title("Regular Regressions")) 
graph save Graph "$results/figureA4aOG.gph", replace

cd "$results"
graph combine figA4aMy.gph figureA4aOG.gph
graph save "$results/A4HeterogenousGraph.gph", replace 

//A4B
use "$data/geospillover.dta", clear
g rps=.
g beta=.
g se= .

quietly : xtrobreg mm repshare mspost mspostmargin mspostmarginsq mspostmargincu mspostmarginqu, cluster(state)
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
xtitle("REP Victory Margin in the past") legend(order(3 1)) scheme(s2mono) )
graph save Graph "$results/figureA4b.gph", replace

use "$data/geospillover.dta", clear
g rps=.
g beta=.
g se= .

quietly : reghdfe repshare mspost mspostmargin mspostmarginsq mspostmargincu mspostmarginqu, absorb(county c.popsd##year state##year) cluster(state)
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
xtitle("REP Victory Margin in the past")  legend(order(3 1)) scheme(s2mono) )
graph save Graph "$results/figureA4bOG.gph", replace

cd "$results"
graph combine figureA4b.gph figureA4bOG.gph, 
graph save "$results/A4HeterogenousGraph2.gph", replace

cd "$results"
graph combine A4HeterogenousGraph.gph A4HeterogenousGraph2.gph, col(1) iscale(1) ycommon title("Impact on Republican Vote Share")
 graph export "/Users/sarathchandra/Desktop/M2/Adv Microeconometrics /Endterm Project/Libois/Results/mainA4.png", as(png) name("Graph")
