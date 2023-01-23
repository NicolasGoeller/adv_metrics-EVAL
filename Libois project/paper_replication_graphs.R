library(tidyverse)
library(haven)
library(reshape2)
library(gridExtra)

### Read in data
table <- read_dta("data/yearly-obs.dta")

### Set outlier boundary as obtained from stata
outlier_boundary <- 11.23

### Define plot function
ms_scatter <- function(df, y, y_lab, legend=TRUE, title=""){
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
      geom_point(alpha = 0.5, show.legend = legend)+
      geom_smooth(method="lm", se=F,show.legend = legend)+
    labs(x="Standardized population", 
         y=y_lab,
         color='Mass Shooting',
         title= title)+
    theme_minimal()
  
  return(plot)
}

### Run plots for full sample
p1 <- ms_scatter(table, "repshare", "Republican vote share")

p2 <- ms_scatter(table, "turnout", "Turnout")

p3 <- ms_scatter(table, "poverty", "Poverty rate", legend=TRUE, title="Full Sample")

p4 <- ms_scatter(table, "racialhhi", "Racial HHI index")

p5 <- ms_scatter(table, "lncrime_violent", "Log of violent crime")

p6 <- ms_scatter(table, "lnhom_gun", "Log of gun homicides")

#p_final_1 <- grid.arrange(grobs= list(p1,p2,p3,p4,p5,p6), nrow = 3, ncol = 2,
#                          layout_matrix= rbind(c(1,2),c(3,4),c(5,6)))
#ggsave("graphs/scatterplots_mass-shootings.png", p_final_1, 
#       width = 32, height= 18, units="cm")

### Runn plot for reduced sample
p7 <- ms_scatter(table %>% filter(popsd < outlier_boundary), "repshare", "Republican vote share")

p8 <- ms_scatter(table %>% filter(popsd < outlier_boundary), "turnout", "Turnout")

p9 <- ms_scatter(table %>% filter(popsd < outlier_boundary), 
                 "poverty", "Poverty rate", legend=TRUE, title="Trimmed Sample (pop < 11.23)")

p10 <- ms_scatter(table %>% filter(popsd < outlier_boundary), "racialhhi", "Racial HHI index")

p11 <- ms_scatter(table%>% filter(popsd < outlier_boundary) , "lncrime_violent", "Log of violent crime")

p12 <- ms_scatter(table %>% filter(popsd < outlier_boundary), "lnhom_gun", "Log of gun homicides")

#p_final_2 <- grid.arrange(grobs= list(p7,p8,p9,p10,p11,p12), nrow = 3, ncol = 2,
#                          layout_matrix= rbind(c(1,2),c(3,4),c(5,6)))
#ggsave("graphs/scatterplots_mass-shootings_2.png", p_final_2, 
#       width = 32, height= 18, units="cm")

p_final_pres <- grid.arrange(grobs= list(p3,p9), nrow = 1, ncol = 2,
                          layout_matrix= rbind(c(1,2)))
ggsave("graphs/scatterplots_mass-shootings_pres.png", p_final_pres, 
       width = 26, height= 9, units="cm")

