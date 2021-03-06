#Title: exploration.R
#Author: Chris Montgomery
#Last Revised: 2/24/2019
#working_directory: ~DATS6101_DataScience/eda_project
#Input: data/dc_residential_data/DC_properties.csv
#Output: data/dc_residential_data_clean.csv

#Description: The file processes DC residential house source data. It drops unecessary variables, 
#removes observations prior to 2015, converts categorical variables to factor. Cleaned data is output
#to the above output file. 



#Import necessary libraries
library(dplyr); library(ggplot2); library("rgdal"); library("sp") ; library(tmap); library(leaflet)      


df.orig <- df <- read.csv("data/dc_residential_data/DC_properties.csv")

df$SALEDATE <- as.Date(df$SALEDATE) 
df$sale.year <-  (lubridate::year(df$SALEDATE))
df$log.PRICE <- log(df$PRICE)
df$LANDAREA <- log(df$LANDAREA)
df$LIVING_GBA <- log (df$LIVING_GBA)

#Filter out homes with no price, no full address, or sold earlier than 2015
df <- df %>% filter(!is.na(PRICE),  sale.year > 2015, !is.na(QUADRANT), QUADRANT != '')
#Drop variables in drop list

drop.list <- c("BLDG_NUM", "SALE_NUM", "USECODE", "GIS_LAST_MOD_DTTM", "CMPLX_NUM",
       "GBA", "CITY", "STATE", "NATIONALGRID", "X","Y",
       "X.1")


df <- df[,!colnames(df) %in% drop.list]


#Convert to factor : 
convert.factor <- c("ZIPCODE", "QUALIFIED", "STYLE", "STRUCTURE", "ZIPCODE", "sale.year")

for (i in convert.factor){
  df[,i] <- as.factor(df[,i])
}




cap.coordinates <- c(38.8899, -77.0091)
df$dist <-( (df$LONGITUDE - cap.coordinates[2])^2 + (df$LATITUDE - cap.coordinates[1])^2)^.5;
df$sale.year <- as.factor(df$sale.year)


#Manually code cut offs for outliers

cont.variables <- c('BATHRM', 'HF_BATHRM', 'ROOMS', 'BEDRM', 'AYB', 'YR_RMDL','EYB', 'STORIES', 'KITCHENS', 'FIREPLACES', 'LIVING_GBA')
summary(df$PRICE)



#df <- df[df$LANDAREA < 9,]; summary(df$PRICE)
#df <- df[df$LANDAREA > 5,]; summary(df$PRICE)

df <- df[df$FIREPLACES < 6,]; summary(df$PRICE)
df <- df[df$BEDRM < 10, ]; summary(df$PRICE)
df <- df[df$ROOMS < 15, ]; summary(df$PRICE)
df <- df[df$HF_BATHRM < 5,]; summary(df$PRICE)
df <- df[df$BATHRM < 6,]; summary(df$PRICE)
df <- df[df$log.PRICE < 15.75,]; summary(df$PRICE)
df <- df[df$log.PRICE > 11,]; summary(df$PRICE)
df <- df[df$LANDAREA > 5.5 & df$LANDAREA < 9.75, ]


#Remove observations with more than 3 kitchens and 6 stories

df<- df[order(df$KITCHENS),]; tail(na.omit(df$KITCHENS), 10)
df <- df[!df$KITCHENS %in% tail(na.omit(df$KITCHENS), 10),]; summary(df$PRICE)

df<- df[order(df$STORIES),]
df <- df[!df$STORIES %in% tail(na.omit(df$STORIES), 20),]; summary(df$STORIES)

#Some variables logically need to be greater than 0 
pos.variables <- c('AYB', 'YR_RMDL','EYB', 'STORIES', 'LANDAREA', 'BEDRM', 'ROOMS')
for (i in pos.variables) {
  df <- df[df[,i] > 0 | is.na(df[,i]) , ]
}


summary(df$log.PRICE)
summary(df$PRICE)
df$ZIPCODE <- as.factor(df$ZIPCODE)
write.csv(df, "data/dc_residential_data_clean.csv")

length(na.omit(df$LANDAREA))


