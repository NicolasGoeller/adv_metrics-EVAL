macro drop _all 
ssc install robbox
ssc install robreg
ssc install moremata
ssc install outreg2
//ssc install roblogit 
ssc install mmregress
ssc install xtrobreg
ssc install robmv 

// Sarath's directory
//global data "/Users/sarathchandra/Desktop/M2/Adv Microeconometrics /Endterm Project/Libois/Replication/data"
//global results "/Users/sarathchandra/Desktop/M2/Adv Microeconometrics /Endterm Project/Libois/Results"
//global var1 "repshare poverty lncrime_violent turnout racialhhi lnhom_gun"


// Nico's directory
global data "C:/Users/Nicolas/OneDrive - Zeppelin-University gGmbH/Dokumente/Studium/Advanced Micrometrics/adv_metrics-EVAL/Libois project/data"
//global results "/Users/sarathchandra/Desktop/M2/Adv Microeconometrics /Endterm Project/Libois/Results"
global var1 "repshare poverty lncrime_violent turnout racialhhi lnhom_gun"

u "$data/yearly-obs.dta", clear 
graph box poverty , over(ms, label(angle(90)))  marker(1,msymbol(oh))   title("poverty box plot") 


/* Generalized boxplot */
* standard
foreach var of varlist repshare poverty lncrime_violent turnout racialhhi lnhom_gun{
	robbox `var', general over(ms)  ytitle("`var'") xtitle("mass shootings")
	gr save "$results/`var'", replace
}
cd "$results"
gr combine repshare.gph poverty.gph lncrime_violent.gph turnout.gph racialhhi.gph lnhom_gun.gph 
gr save "$results/gen_boxplots",replace 
gr export "$results/gen_boxplots.png", replace 

robbox popsd, general  ytitle("standardised population") xtitle("mass shootings") //upper limit for outlier = 11.23
graph box popsd , over(ms, label(angle(90)))  marker(1,msymbol(oh))   title("poverty box plot") 

// TABLE 2 
est clear 

// column 1 to 3  - airport and capital_state are binary - need to us roblogit 
eststo : robreg mm L1.repshare ms popsd if (mse==0 | ms==1), cluster(fips)
foreach var of varlist turnout lnincome ///
poverty gini racialhhi mentalhealth pop15t39 nevermarried sc ///
lnhom_gun lnsui_gun lncrime_violent lncrime_proprty { 
eststo : robreg mm L1.`var' ms popsd if (mse==0 | ms==1), cluster(fips)
} 
// column 4
eststo : mean(repshare) if failms == 1 

roblogit L1.airport ms popsd if (mse==0 | ms==1), cluster(fips)

// column 5
eststo : mean(repshare) if ms == 1 

//column 6
foreach var of varlist repshare turnout lnincome ///
poverty gini racialhhi mentalhealth pop15t39 nevermarried sc ///
lnhom_gun lnsui_gun lncrime_violent lncrime_proprty { 
eststo : robreg mm L1.`var' ms if (ms==1 | failms==1), cluster(fips)
}

esttab using "$results/table2main.tex", replace // correct table and upload final table with column 4 to 6 

// TABLE 3 - need to install roblogit 

// TABLE 4 
clear all 
u "$data/pres-elec.dta"
xtset, clear
xtset county year

// panel A

xtrobreg mm repshare mspost*, cluster(state)
 
xtrobreg mm repshare mspost* [aweight=pop], cluster(state)

// panel B 
use "$data/pres-elec-succfail.dta", clear 
xtset, clear  
xtset county year

xtrobreg mm repshare successful postattack if mse==1 | failmse == 1 , cluster(state)

// Table 5 
use "$data\pres-elec.dta", clear
xtset, clear  
xtset county year

// panel A 

// incumbent vote share
xtrobreg mm incumbent mspost*, cluster(state)
xtrobreg mm incumbent mspost if incumbent==repshare, cluster(state)
xtrobreg mm incumbent mspost if incumbent==demshare, cluster(state)

// turnout 

xtrobreg mm turnout mspost*, cluster(state)
xtrobreg mm turnout mspost if incumbent==repshare, cluster(state)
xtrobreg mm turnout mspost if incumbent==demshare, cluster(state)

// Panel B 
use "$data\pres-elec-succfail.dta", clear
xtset, clear  
xtset county year

// incumbent vote share 

xtrobreg mm incumbent successful postattack, cluster(state)
xtrobreg mm incumbent successful postattack if incumbent==repshare, cluster(state)
xtrobreg mm incumbent successful postattack if incumbent==demshare, cluster(state)

// turnout

xtrobreg mm turnout successful postattack, cluster(state)
xtrobreg mm turnout successful postattack if incumbent==repshare, cluster(state)
xtrobreg mm turnout successful postattack if incumbent==demshare, cluster(state)

// Heterogeniety - Figure A4
use "$data\geospillover.dta", clear
g rps=.
g beta=.
g se= .

// A4A
xtrobreg mm repshare mspost mspostmnthelec mspostmnthelecsq, cluster(state)
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

//A4B
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

