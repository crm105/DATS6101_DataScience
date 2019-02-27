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
df$sale.year <- lubridate::year(df$SALEDATE)

#Filter out homes with no price, no full address, or sold earlier than 2015
df <- df %>% filter(!is.na(PRICE), !is.na(FULLADDRESS),  sale.year > 2015)

#Drop variables in drop list

drop.list <- c("BLDG_NUM", "SALE_NUM", "USECODE", "GIS_LAST_MOD_DTTM", "CMPLX_NUM",
      "LIVING_GBA", "GBA", "CITY", "STATE", "NATIONALGRID", "X","Y",
      "ASSESSMENT_SUBNBHD", "CENSUS_TRACT", "CENSUS_BLOCK", "SQUARE" , "X.1")


df <- df[,!colnames(df) %in% drop.list]

#Convert to factor : 
convert.factor <- c("ZIPCODE", "QUALIFIED", "STYLE", "STRUCTURE")


df[,colnames(df) %in% convert.factor] <- as.factor(df[,colnames(df) %in% convert.factor])




#code to remove outliers
# df$PRICE <- log(df$PRICE)
# outliers <- c("PRICE")
# lbound <- c()
# ubound <- c() 
# IQR <- c()
# for (i in outliers){
#   IQR[i] <- quantile(df[,i],.75, na.rm = TRUE) - quantile(df[,i],.25, na.rm = TRUE)
#   lbound[i] <- as.numeric (quantile(df[,i], .25,na.rm = TRUE) - (2.5 *IQR[i])) 
#   ubound[i] <- as.numeric(quantile(df[,i], .75, na.rm = TRUE) + (2.5 * IQR[i]) ) 
# 
# }
# for(i in seq(1:length(outliers))){
#  df <- df[df[,outliers[i]] > lbound & df[,outliers[i]]  < ubound        ,]
# }

summary(df$PRICE)





summary(df$BEDRM)

df[df$PRICE > 25000000,]
#Identify the dwelling characteristics we are interested in
continuous.characteristics <- c("BATHRM", "HF_BATHRM", "ROOMS", "BEDRM", "AYB", "YR_RMDL",
                      "STORIES", "PRICE")

categorical.characteristics <- c("CNDTN", "SOURCE", "AC", "")


unique(df$GRADE)
unique(df$CNDTN)
summary(df$NUM_UNITS)




map <- readOGR("Neighborhood_Clusters")

df$content <- paste(df$FULLADDRESS, sep = "<br/>", df$PRICE)


  
  # m <- leaflet() %>%
  #   addProviderTiles(providers$Esri.NatGeoWorldMap) %>%  # Add default OpenStreetMap map tiles
  #   addMarkers(lng=df$LONGITUDE[1:1000], lat=df$LATITUDE[1:1000], popup=df$content[1:1000],
  #              options = popupOptions(closeButton = FALSE))
  # 
  # m
  # 
  # 
  
#Include a variable that measures distance from city center
  cap.coordinates <- c(38.8899, -77.0091)
  
  df$dist <-( (df$LONGITUDE + cap.coordinates[2])^2 + (df$LATITUDE - cap.coordinates[1]))^.5
  

write.csv(df, "data/dc_residential_data_clean.csv")




