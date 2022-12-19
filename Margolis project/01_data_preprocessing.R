library(tidyverse)
library(haven)
library(data.table)


### Set year period
year_set <- c(2015:2019)

### Country codes
countries <- read_csv("data/country_codes.csv") %>% 
  rename(iso3 = iso_3digit_alpha, iso2 = iso_2digit_alpha) %>%
  # Exclude these codes because of historical iso code doubles
  filter(!(country_code %in% c(58,280,711,736)))

################ Country Panel ##################################

### Exchange rates
xr <- read_csv("data/Exchange_rates_WB.csv") %>% 
  filter(`Series Code` == "PA.NUS.FCRF") %>% 
  select(`Country Code`,`1960 [YR1960]`:`2021 [YR2021]`) %>% 
  pivot_longer(cols = `1960 [YR1960]`:`2021 [YR2021]`, names_to = "year", 
               values_to= "xr") %>% 
  mutate(year = as.numeric(str_sub(year,1,4)),
         xr = as.numeric(xr)) %>% 
  rename(iso3 = `Country Code`)

### Inflation rates
inflation <- read.csv("data/Inflation_WB.csv", skip = 4) %>% 
  select(Country.Code, X1995:X2020) %>% 
  pivot_longer(cols = X1995:X2020, names_to = "year", 
               values_to= "inflation") %>% 
  mutate(year = as.numeric(str_replace(year, "X",""))) %>% 
  rename(iso3 = Country.Code)

### WGI Institutional quality
wgi <- read_dta("data/wgidataset.dta") %>% 
  left_join(countries %>% select(iso3), by=c("code"="iso3"), keep=TRUE) %>% 
  left_join(countries %>% 
              select(iso3, country_name_abbreviation) %>% 
              rename(replace = iso3), 
            by= c("countryname"="country_name_abbreviation")) %>% 
  mutate(iso3 = if_else(is.na(iso3), replace, iso3)) %>% 
  select(iso3,year,vae,pve,gee,rqe,rle,cce)

### Bilateral variables and GDP
gravity <- fread("data/Gravity_V202010.csv") %>% 
  filter(iso3_o == "FRA") %>% 
  select(iso3_d,year,dist,contig,comlang_off,
         col_dep_ever,gatt_d,wto_d,eu_d,rta,gdpcap_d) %>% #comrelig and comcur are missing
  rename(iso3 = iso3_d)

### String country panel together
country_panel <- wgi %>% 
  left_join(inflation, by= c("iso3","year")) %>% 
  left_join(xr, by= c("iso3","year")) %>% 
  left_join(gravity, by= c("iso3","year")) 

############## Product Level Variables #######################

pci_hs4 <- read_csv("data/pci_hs4_hs92.csv") %>% 
  pivot_longer(`1995`:`2020`, names_to = "year", 
               values_to= "pci") %>% 
  rename(hs4=HS4, hs4_id="HS4 ID") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(-hs4)

############## CEPII Trade Volume Panel #############################

### Define processing function
process_baci <- function(path){
  baci_names <- c("exporter"="i","importer"="j",
                  "year"="t","hs6_id"="k",
                  "quantity"="q","value"="v")
  baci <- fread(path) %>% 
    rename(!!!baci_names) %>% 
    filter(exporter == 251) %>%  ## country code France
    mutate(hs4_id = str_sub(hs6_id,1,4)) %>% 
    group_by(year, importer, hs4_id) %>% 
    summarise(value = sum(value), quantity= sum(quantity)) %>% 
    ungroup()
  print(unique(baci$year))
  
  return(baci)
}

### Run function over relevant years
full_baci <- tibble()
for (year in year_set){
  path <- paste0("data/BACI_HS92/BACI_HS92_Y",year,"_V202201.csv")
  baci <- process_baci(path)
  full_baci <- bind_rows(full_baci, baci)
}

############### Full Panel Creation ###################################

### Create full product-country space to merge into
productcountry_space <- full_baci %>% 
  distinct(hs4_id) %>% 
  merge(full_baci %>% distinct(year,importer),
        by = integer(0))

### Join product-country- space with trade records and fill the NAs
trade_panel <- productcountry_space %>% 
  left_join(full_baci, by = c("year","importer","hs4_id")) %>% 
  mutate(value = replace_na(value, 0),
         quantity = replace_na(quantity, 0),
         has_trade = if_else(value>0,1,0)) %>%
### Join all other tables on
  left_join(pci_hs4, by= c("year","hs4_id")) %>% 
  left_join(countries %>% select(iso3, country_code, country_name_full), 
            by=c("importer"="country_code")) %>% 
  left_join(country_panel, by= c("year","iso3")) 

############# NA exclusions and Imputations ############################

# Exclude N/A mix categories, French and British oversee territories, and 
# heavily warring states for reasons of unreasonable country-level imputation 
# and sample bias

exclusions <- c("N/A","NCL","PYF","COD","GIB","BLM","VGB","PSE","SXM","SPM",
                "WLF","CUW","TCA","BES","MNP","ATF","MSR","FLK","TKL","SHN",
                "COK","PCN","SMR","NFK","IOT","CXR","CCK")

trade_panel <- trade_panel %>% 
  filter(!(iso3 %in% exclusions)) %>% 
  #### now impute NAs for continuous country-level variables with the year mean
  group_by(year) %>% 
  mutate(across(.cols= c("inflation","xr","vae","pve","gee","rqe","rle","cce","gdpcap_d","pci"),
                .fns = ~mean(.x,na.rm=TRUE), .names = "{.col}_m")) %>% 
  mutate(across(c(str_subset(names(.), "_m") %>% str_remove("_m")) ,
                ~ coalesce( ., get(str_c(cur_column(), "_m"))  ), .names = "{col}")) %>% 
  select(-c(str_subset(names(.), "_m"))) %>% 
  #### impute distances with internet research results
  mutate(dist = case_when(
    iso3 =="ASM" ~ 16098,
    iso3 =="GUM" ~ 12157,
    iso3 =="CXR" ~ 11847,
    iso3 =="CCK" ~ 11344,
    TRUE ~ dist
  )) %>% 
  #### Impute missings for border dummy and common language by internet research
  mutate(across(.cols = c("contig","comlang_off"), .fns = ~replace_na(.x,0), .names="{.col}"))


########################## Export final tables #########################
write_dta(trade_panel %>% select(-value, -quantity), "data/trade_panel_zeros.dta")

write_dta(trade_panel %>% filter(value > 0) %>% 
            select(-has_trade), "data/trade_panel_nozeros.dta")

