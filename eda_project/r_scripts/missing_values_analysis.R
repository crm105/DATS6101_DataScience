#Title: missing_values_analysis.R
#Author: Chris Montgomery
#Last Revised: 2/24/2019
#working_directory: ~DATS6101_DataScience/eda_project
#Input: data/dc_residential_data/DC_properties.csv
#Output: 

#Description: This script tests differences in housing characteristics between homes with and without price
#values. Characteristics of interest were determined by conversation with group members and can be found in
#EDA_outline.doc


#Import necessary libraries
library(dplyr); library(ggplot2); library(gplots); library(tidyverse)
   

#Read in the original source data
df.orig <- df <- read.csv("data/dc_residential_data/DC_properties.csv")

#Convert saledate to date, and add sale year column

df$SALEDATE <- as.Date(df$SALEDATE) 
df$sale.year <- lubridate::year(df$SALEDATE)

#Create a dataframe that contains clean housing values that will not be dropped
df.clean <- df %>% filter(!is.na(PRICE),  sale.year > 2015)

#Create a dataframe with dropped values for comparison 
df.dropped <- df %>% filter(is.na(PRICE),  sale.year > 2015)

#Identify continuous variables of interest for T-tests 

cont.variables <- c("BATHRM", "HF_BATHRM", "ROOMS", "BEDRM", "AYB", "YR_RMDL", "EYB", "STORIES", "GBA",
                    "KITCHENS", "FIREPLACES")

#Identify categorical variables of interest for chi-square tests 

cat.variables <- c("HEAT","AC", "CNDTN", "EXTWALL", "ROOF", "INTWALL", "QUADRANT")


#Create a loop that conducts a t-test for each of the above variables


 t.test.results <-  lapply(seq(1:length(cont.variables)),function(i){
  x <- df.clean[,cont.variables[i]]
  y <- df.dropped[,cont.variables[i]]
 t.test(x,y, alternative = "two.sided", na.action = na.omit)
  
})
 
 t.test.results[[1]]$p.value

 #Create a loop that reports the significance of each pairwise t-test 
for (i in 1:length(cont.variables)){
  print(
    paste ( "On average, non-missing values have ", signif (100 * t.test.results[[i]]$estimate[[1]] / 
              t.test.results[[i]]$estimate[[2]], digits = 3 ),"% ", cont.variables[i], " with a t-stat of ",
            t.test.results[[i]]$statistic, sep = ""
  ))
}
 
#Create a loop that conducts chi-square for each of the categorical variables
 df.cat <- df %>% filter( sale.year > 2015)
 df.cat$missing <- 0; df.cat[is.na(df.cat$PRICE),"missing"] = 1; df.cat$missing <- as.factor(df.cat$missing)
 df.cat <- df.cat %>%  select(cat.variables, missing)
 

 chi.test.results <-  lapply(seq(1:length(cat.variables)),function(i){
   x <- df.cat[,cat.variables[i]]
   y <- df.cat[,"missing"]
   chisq.test(x,y)
   
 })
 

for (i in chi.test.results){
  print(i$p.value)
}
 
 chi.test.results[1]
 
 #Tukey's HSD???
 

