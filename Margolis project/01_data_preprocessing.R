library(tidyverse)
library(haven)
library(data.table)


### Set year period
year_set <- c(2014:2019)

continents <- read_csv("data/continents2.csv")

### Country codes
countries <- read_csv("data/country_codes.csv") %>% 
  rename(iso3 = iso_3digit_alpha, iso2 = iso_2digit_alpha) %>%
  # Exclude these codes because of historical iso code doubles
  filter(!(country_code %in% c(58,280,711,736))) %>% 
  left_join(continents %>% select(`alpha-3`,region),
            by=c("iso3"="alpha-3"))
  
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

pci_hs2 <- pci_hs4 %>% 
  mutate(hs2_id = str_sub(hs4_id,1,2)) %>% 
  group_by(year, hs2_id) %>% 
  summarise(pci = mean(pci,na.rm=TRUE))

############## CEPII Trade Volume Panel #############################

### Define processing function
process_baci <- function(path, agg = TRUE){
  baci_names <- c("exporter"="i","importer"="j",
                  "year"="t","hs6_id"="k",
                  "quantity"="q","value"="v")
  baci <- fread(path) %>% 
    rename(!!!baci_names) %>% 
    filter(exporter == 251) %>%  ## country code France
    mutate(hs6_id = as.character(hs6_id),
           hs6_id = if_else(str_length(hs6_id) == 5,paste0("0",hs6_id),hs6_id),
           hs4_id = str_sub(hs6_id,1,4)) %>% 
    group_by(year, importer, hs4_id) %>% 
    summarise(value = sum(value), 
              quantity= sum(quantity)) %>% 
    ungroup()
  if (agg == TRUE) {
    baci <- baci %>% 
      mutate(hs2_id = str_sub(hs4_id,1,2)) %>% 
      group_by(year, importer, hs2_id) %>% 
      summarise(quantity = sum(quantity, na.rm = T),
                value = sum(value, na.rm = T)) %>% 
      ungroup()
  }
 
  print(unique(baci$year))
  
  return(baci)
}


### Run function over relevant years
full_baci <- tibble()
for (year in year_set){
  path <- paste0("data/BACI_HS92/BACI_HS92_Y",year,"_V202201.csv")
  baci <- process_baci(path, agg=TRUE)
  full_baci <- bind_rows(full_baci, baci)
}

############### Full Panel Creation ###################################

### Create full product-country space to merge into
productcountry_space <- full_baci %>% 
  distinct(hs2_id) %>% 
  merge(full_baci %>% distinct(year,importer),
        by = integer(0))

### Join product-country- space with trade records and fill the NAs
trade_panel <- productcountry_space %>% 
  left_join(full_baci, by = c("year","importer","hs2_id")) %>% 
  mutate(value = replace_na(value, 0),
         quantity = replace_na(quantity, 0),
         has_trade = if_else(value>0,1,0)) %>% 
### Generate lagged differences to assess absolute growth
  group_by(importer,hs2_id) %>% 
  mutate(value_diff = value - lag(value),
         quantity_diff = quantity - lag(quantity)) %>%
  ungroup() %>% 
  filter(year %in% year_set[2:length(year_set)]) %>% 
### Join all other tables on
  left_join(pci_hs2, by= c("year","hs2_id")) %>% 
  left_join(countries %>% select(iso3, country_code, country_name_full, region), 
            by=c("importer"="country_code")) %>% 
  left_join(country_panel, by= c("year","iso3")) 

n_distinct(trade_panel$iso3)

############# NA exclusions and Imputations ############################

# Exclude N/A mix categories, French and British oversee territories, and 
# heavily warring states for reasons of unreasonable country-level imputation 
# and sample bias

exclusions <- c("N/A","NCL","PYF","COD","GIB","BLM","VGB","PSE","SXM","SPM",
                "WLF","CUW","TCA","BES","MNP","ATF","MSR","FLK","TKL","SHN",
                "COK","PCN","SMR","NFK","IOT","CXR","CCK")

trade_panel <- trade_panel %>% 
  filter(!(iso3 %in% exclusions)) 

n_distinct(trade_panel$iso3)

########################## Export intermediate tables #########################
write_csv(trade_panel, "data/trade_panel_noimpute.csv")


trade_panel <- trade_panel %>% 
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
  mutate(across(.cols = c("contig","comlang_off"), .fns = ~replace_na(.x,0), .names="{.col}")) %>% 
  #### Create categorical variable for year_FE
  mutate(year_cat = as.character(year))


########################## Export final tables #########################
write_dta(trade_panel %>% select(-value, -quantity), "data/trade_panel_hs2_zeros.dta")

write_dta(trade_panel %>% filter(value > 0) %>% 
            select(-has_trade), "data/trade_panel_hs2_nozeros.dta")

