macro drop _all

global data "/Users/sarathchandra/Desktop/M2/Adv Microeconometrics /Endterm Project/Data_Margolis"
global results "/Users/sarathchandra/Desktop/M2/Adv Microeconometrics /Endterm Project/Results_Margolis"

global data "C:\Users\Nicolas\OneDrive - Zeppelin-University gGmbH\Dokumente\Studium\Advanced Micrometrics\adv_metrics-EVAL\Margolis project\data"
global results "C:\Users\Nicolas\OneDrive - Zeppelin-University gGmbH\Dokumente\Studium\Advanced Micrometrics\adv_metrics-EVAL\Margolis project\output"


u "$data/trade_panel_hs2_zeros.dta", clear 
//first stage 
probit has_trade pci vae pve gee rqe rle cce inflation xr dist contig comlang_off col_dep_ever gatt_d wto_d eu_d rta gdpcap_d 

predict xb, xb 
gen imr = normalden(xb)/normal(xb) // to calculate IMR

save "$data/trade_panel_hs2_zeros.dta", replace

u "$data/trade_panel_hs2_nozeros.dta", clear 

//merging to get the inverse mills ratio 
merge 1:1 year hs2_id iso3 using "$data/trade_panel_hs2_zeros.dta"
drop if _merge == 2
drop if _merge == 1 

// second stage 
//mixed value pci vae rqe rle cce inflation dist xr contig col_dep_ever gatt_d wto_d eu_d rta gdpcap_d imr || _all: R.hs4_id || iso3: 

mixed value pci gee rqe rle cce inflation dist xr contig col_dep_ever gatt_d wto_d eu_d rta gdpcap_d imr || _all: R.hs2_id || importer: 

//old reg - ignore 
meprobit has_trade pci vae pve gee rqe rle cce inflation dist xr contig comlang_off col_dep_ever gatt_d wto_d eu_d rta gdpcap_d || _all: R.hs4_id || iso3: , iterate(3)

