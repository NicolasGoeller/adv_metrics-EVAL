library(tidyverse)
library(haven)
library(reshape2)
library(gridExtra)
#library(robustbase)
#library(rlang)
source("watercolorplot_function.R")

table2 <- read_dta("data/yearly-obs.dta")

test <- table2 %>% 
  select(fips, year4, ms, popsd, repshare) %>% 
  filter(fips != 206) %>% 
  group_by(year4, fips) %>%
  summarise(ms = sum(ms), 
            repshare = median(repshare, na.rm=T),
            popsd = median(popsd, na.rm=T)) %>% 
  mutate(ms = if_else(ms>0,2,1),
         ms = factor(ms, labels = c("no MS", "MS")),
         popsd)

table2 %>% 
  select(fips, popsd) %>%
  group_by(fips) %>% 
  summarise(max_pop = max(popsd), 
            mean_pop = mean(popsd)) %>% 
  ggplot(aes(x=mean_pop))+
  geom_density()




  arrange(desc(mean_pop)) %>% 
  head(15)

#summary(test$popsd)

#vwReg(repshare ~ popsd, test)


ms_scatter <- function(df, y, y_lab){
## This function will calculate all MS per election cycle-county
## Set thos with > 0 to 1 and calculate medians for the 4 year on the other vars  
  plot <- df %>% 
    select(fips, year4, ms, popsd, !!y) %>%
    group_by(year4, fips) %>%
    summarise(ms = sum(ms), 
              !!y := median(!!sym(y), na.rm=T),
              popsd = median(popsd, na.rm=T)) %>% 
    mutate(ms = if_else(ms>0,2,1),
           ms = factor(ms, labels = c("no MS", "MS"))) %>% 
    ggplot(aes(x= popsd, y= !!sym(y), color=ms, by=ms))+
      geom_point(alpha = 0.5)+
      geom_smooth(method="lm", se=F)+
    labs(x="Standardized population", 
         y=y_lab,
         color='Mass Shooting')+
    theme_minimal()
  
  return(plot)
}

p1 <- ms_scatter(table2, "repshare", "Republican vote share")

p2 <- ms_scatter(table2, "turnout", "Turnout")

p3 <- ms_scatter(table2, "poverty", "Poverty rate")

p4 <- ms_scatter(table2, "racialhhi", "Racial HHI index")

p5 <- ms_scatter(table2, "lncrime_violent", "Log of violent crime")

p6 <- ms_scatter(table2, "lnhom_gun", "Log of gun homicides")

p_final_1 <- grid.arrange(grobs= list(p1,p2,p3,p4,p5,p6), nrow = 3, ncol = 2,
                          layout_matrix= rbind(c(1,2),c(3,4),c(5,6)))
ggsave("graphs/scatterplots_mass-shootings.png", p_final_1, 
       width = 32, height= 18, units="cm")

outlier_boundary <- 11.23
p7 <- ms_scatter(table2 %>% filter(popsd < outlier_boundary), "repshare", "Republican vote share")

p8 <- ms_scatter(table2 %>% filter(popsd < outlier_boundary), "turnout", "Turnout")

p9 <- ms_scatter(table2 %>% filter(popsd < outlier_boundary), "poverty", "Poverty rate")

p10 <- ms_scatter(table2 %>% filter(popsd < outlier_boundary), "racialhhi", "Racial HHI index")

p11 <- ms_scatter(table2%>% filter(popsd < outlier_boundary) , "lncrime_violent", "Log of violent crime")

p12 <- ms_scatter(table2 %>% filter(popsd < outlier_boundary), "lnhom_gun", "Log of gun homicides")

p_final_2 <- grid.arrange(grobs= list(p7,p8,p9,p10,p11,p12), nrow = 3, ncol = 2,
                          layout_matrix= rbind(c(1,2),c(3,4),c(5,6)))
ggsave("graphs/scatterplots_mass-shootings_2.png", p_final_2, 
       width = 32, height= 18, units="cm")

