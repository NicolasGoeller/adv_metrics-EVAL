library(tidyverse)
library(haven)
library(stargazer)
#library(modelsummary)
library(lme4)
library(reghelper)

trade_zeros <- read_dta("data/trade_panel_hs2_zeros.dta")

trade_panel <- read_dta("data/trade_panel_hs2_nozeros.dta")

trade_noimpute <- read_csv("data/trade_panel_noimpute.csv")


## Share of zeros barchart

zero_share <- trade_zeros %>% 
  group_by(year, has_trade) %>% 
  summarise(trade = n()) %>% 
  mutate(total = sum(trade),
         zero_share = trade/total) %>% 
  filter(has_trade == 0) %>% 
  ungroup() %>% 
  select(zero_share)

mean(zero_share$zero_share)


#### Before and after imputation => create extra dataset

varnames <- c("GDP per capita","Inflation","Exchange rate",
              "Voice and Accountablity","Political stability",
              "Government effective","Regulatory quality",
              "Rule of law","Corruption control","Distance",
              "Product Complexity")

var_select <- c("GDP per capita"="gdpcap_d","Inflation"="inflation",
                "Exchange rate"="xr","Voice and accountablity"="vae",
                "Political stability"="pve","Government effectiveness"="gee",
                "Regulatory quality"="rqe","Rule of law"="rle",
                "Corruption control"="cce")

## General summary table
trade_vars <- trade_panel %>% 
  select(value, quantity) %>% 
  as.data.frame()

stargazer(trade_vars, type = "latex", digits=2,
          summary.stat = c("min", "p25", "median", "mean", "p75", "max", "sd"),
          covariate.labels = c("Export value","Export quantity"),
          title = "Summary statistics of trade outcomes",
          label = "fig:sum_tradevar",
          out ="output/summary_stats_tradevars.tex",
          table.placement = "H")
n_distinct(trade_panel$hs2_id)
############################################################
## Summary table split by has_trade
country_vars_trade <- trade_zeros %>% 
  filter(has_trade == 1) %>% 
  select(year,iso3,gdpcap_d,inflation,xr,vae,pve,gee,rqe,rle,cce,dist,pci) %>% 
  group_by(year,iso3) %>%
  summarise(across(.cols= everything(), .fns=~mean(.x, na.rm=T))) %>% 
  ungroup() %>% 
  select(-year,-iso3) %>% 
  #select(all_of(var_select)) %>% 
  as.data.frame()

stargazer(country_vars_trade, type = "latex", digits=2,
          summary.stat = c("min", "p25", "median", "mean", "p75", "max", "sd"),
          covariate.labels = varnames,
          title = "Summary statistics non-zero trade with imputation",
          label = "fig:sum_trade_imp",
          out = "output/summary_stats_trade_impute.tex",
          table.placement = "H")

#######################################

country_vars_notrade <- trade_zeros %>% 
  filter(has_trade == 0) %>% 
  select(year,iso3,gdpcap_d,inflation,xr,vae,pve,gee,rqe,rle,cce,dist,pci) %>% 
  group_by(year,iso3) %>%
  summarise(across(.cols= everything(), .fns=~mean(.x, na.rm=T))) %>% 
  ungroup() %>% 
  select(-year,-iso3) %>% 
  #select(all_of(var_select)) %>%
  as.data.frame()

stargazer(country_vars_notrade, type = "latex", digits=2,
          summary.stat = c("min", "p25", "median", "mean", "p75", "max", "sd"),
          covariate.labels = varnames,
          title = "Summary statistics zero trade with imputation",
          label = "fig:sum_notrade_imp",
          out = "output/summary_stats_notrade_impute.tex",
          table.placement = "H")

## Summary table before imputation, split by has_trade
country_vars_trade2 <- trade_noimpute %>% 
  filter(has_trade == 1) %>% 
  select(year,iso3,gdpcap_d,inflation,xr,vae,pve,gee,rqe,rle,cce,dist,pci) %>% 
  group_by(year,iso3) %>%
  summarise(across(.cols= everything(), .fns=~mean(.x, na.rm=T))) %>% 
  ungroup() %>% 
  select(-year,-iso3) %>% 
  #select(all_of(var_select)) %>% 
  as.data.frame()

stargazer(country_vars_trade2, type = "latex", digits=2,
          summary.stat = c("min", "p25", "median", "mean", "p75", "max", "sd"),
          covariate.labels = varnames,
          title = "Summary statistics non-zero trade without imputation",
          label = "fig:sum_trade_noimp",
          out = "output/summary_stats_trade_noimpute.tex",
          table.placement = "H")



country_vars_notrade2 <- trade_noimpute %>% 
  filter(has_trade == 0) %>% 
  select(year,iso3,gdpcap_d,inflation,xr,vae,pve,gee,rqe,rle,cce,dist,pci) %>% 
  group_by(year,iso3) %>%
  summarise(across(.cols= everything(), .fns=~mean(.x, na.rm=T))) %>% 
  ungroup() %>% 
  select(-year,-iso3) %>% 
  #select(all_of(var_select)) %>% 
  as.data.frame()

stargazer(country_vars_notrade2, type = "latex", digits=2,
          summary.stat = c("min", "p25", "median", "mean", "p75", "max", "sd"),
          covariate.labels = varnames,
          title = "Summary statistics zero trade without imputation",
          label = "fig:sum_notrade_noimp",
          out="output/summary_stats_notrade_noimpute.tex",
          table.placement = "H")


product_vars <- trade_zeros %>% 
  group_by(year, hs2_id) %>% 
  summarise(pci = mean(pci, na.rm=T)) %>%  
  full_join(trade_noimpute %>% 
              group_by(year, hs2_id) %>% 
              summarise(pci_noimpute = mean(pci, na.rm=T)),
            by= c("hs2_id","year")) %>% 
  ungroup() %>% 
  select(-year,-hs2_id) %>% 
  as.data.frame()

stargazer(product_vars, type = "latex", digits=2,
          covariate.labels = c("PCI imputed","PCI non-imputed"),
          summary.stat = c("min", "p25", "median", "mean", "p75", "max", "sd"),
          label = "fig:sum_pci",
          title = "Summary statistics Product-level Complexity",
          out = "output/summary_stats_pci.tex",
          table.placement = "H")


dist_var <- trade_zeros %>% 
  group_by(has_trade, iso3) %>% 
  summarise(dist = mean(dist, na.rm=T)) %>%  
  full_join(trade_noimpute %>% 
              group_by(has_trade, iso3) %>% 
              summarise(dist_noimpute = mean(dist, na.rm=T)),
            by= c("has_trade","iso3")) %>% 
  pivot_wider(id_cols = c("iso3"), names_from = has_trade, 
              values_from = c("dist","dist_noimpute")) %>% 
  ungroup() %>% 
  select(-iso3) %>% 
  as.data.frame()

stargazer(dist_var, type = "text", digits=2,
          summary.stat = c("min", "p25", "median", "mean", "p75", "max", "sd"))

####### Compute null models and ICCs

value1 <- lmer(value ~ 1 + (1|hs2_id), data= trade_panel)
value2 <- lmer(value ~ 1 + (1|iso3), data= trade_panel)
value3 <- lmer(value ~ 1 + (1|hs2_id) + (1|iso3), data= trade_panel)

quantity1 <- lmer(quantity ~ 1 + (1|hs2_id), data= trade_panel)
quantity2 <- lmer(quantity ~ 1 + (1|iso3), data= trade_panel)
quantity3 <- lmer(quantity ~ 1 + (1|hs2_id) + (1|iso3), data= trade_panel)

ICC(value1)
ICC(value2)
ICC(value3)

ICC(quantity1)
ICC(quantity2)
ICC(quantity3)

a <- trade_panel %>% 
  select(iso3, value, rta, eu_d) %>% 
  arrange(desc(value)) %>% 
  head(50)
