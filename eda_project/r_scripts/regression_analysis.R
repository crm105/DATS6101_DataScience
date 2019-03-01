#Title: regression_analyis.R
#Author: Chris Montgomery
#Last Revised: 3/1/2019
#Input: dc_residential_data_clean.csv
#Output:

#Description: This script fits a linear regression model to explain
#Price determinates of DC houses


library(tidyverse); library(broom); library(corrplot)

df <- read.csv('data/dc_residential_data_clean.csv')
df$sale.year <- as.factor(df$sale.year)
df$ZIPCODE <- as.factor (df$ZIPCODE)
df$X <- NULL



#Correlation matrix shows very high degree of multicollinearity among variables

cont.variables <- c('HF_BATHRM', 'BATHRM', 'ROOMS','BEDRM', 'AYB',
                      'PRICE', 'FIREPLACES', 'LIVING_GBA',
                    'dist')

res <- cor(df[,cont.variables], use = 'complete.obs' )
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Remove remaining quirks from data 
df <- df[!df$AC == '0',]

df[df$CNDTN == '', 'CNDTN'] = 'Default'
levels(df$CNDTN) <- c( '','Default', 'Poor', 'Average', 'Fair', 'Good', 'Very Good',
                      'Excellent')



levels(df$CNDTN)
# Fit a linear model

lm1 <- lm(log.PRICE ~ BEDRM + dist + AYB   + SOURCE + QUADRANT +
            AC + CNDTN ,data = df); summary(lm1)



