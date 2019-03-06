library(tidyverse); library(broom); library(corrplot); library(leaps)
library(ggplot2); library(reshape2)

#Load in and preproccess uncleanded data 
full.df <- read.csv("data/dc_residential_data/DC_properties.csv")
full.df$ZIPCODE <- as.factor (full.df$ZIPCODE)
full.df$X <- NULL
df$SALEDATE <- as.Date(df$SALEDATE) 

cont <- c("BATHRM", "HF_BATHRM", "ROOMS", "BEDRM", "AYB", "YR_RMDL",
              "EYB", "STORIES", "PRICE", "KITCHENS", "FIREPLACES", 
              "LANDAREA", "QUADRANT")

df.cont <- full.df[,cont]
long <- melt(df.cont, id.var = "QUADRANT")
long$value <- as.numeric(long$value)

#Create box plots of uncleaned data 

p <- ggplot(data = long, aes(x=QUADRANT, y=value)) + 
  geom_boxplot(aes(fill=QUADRANT))
p + facet_wrap( ~ variable, scales="free")
p


#Repeat Process for Cleaned Data

df <- read.csv("data/dc_residential_data_clean.csv")
df$sale.year <- as.factor(df$sale.year)
df$ZIPCODE <- as.factor (df$ZIPCODE)
df$X <- NULL
df$PRICE <- log(df$PRICE)


cont <- c("BATHRM", "HF_BATHRM", "ROOMS", "BEDRM", "AYB", "YR_RMDL",
          "EYB", "STORIES", "PRICE", "KITCHENS", "FIREPLACES", 
          "LANDAREA", "QUADRANT")

df.cont <- df[,cont]
long <- melt(df.cont, id.var = "QUADRANT")
long$value <- as.numeric(long$value)


#Create box plots of cleaned data 

p <- ggplot(data = long, aes(x=QUADRANT, y=value)) + 
  geom_boxplot(aes(fill=QUADRANT))
p + facet_wrap( ~ variable, scales="free")
p


