#Title: regression_analyis.R
#Author: Chris Montgomery
#Last Revised: 3/1/2019
#Input: dc_residential_data_clean.csv
#Output:

#Description: This script fits a linear regression model to explain
#Price determinates of DC houses


library(tidyverse); library(broom); library(corrplot); library(leaps)

df <- read.csv('data/dc_residential_data_clean.csv')
df$sale.year <- as.factor(df$sale.year)
df$ZIPCODE <- as.factor (df$ZIPCODE)
df$X <- NULL



#Correlation matrix shows very high degree of multicollinearity among variables

cont.variables <- c('HF_BATHRM', 'BATHRM', 'ROOMS','BEDRM', 'AYB',
                      'PRICE', 'FIREPLACES' , 'LANDAREA',
                    'dist', 'YR_RMDL', 'LIVING_GBA', 'EYB')


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

continuous_variables <- c("BATHRM", "HF_BATHRM", "NUM_UNITS", "ROOMS", "BEDRM",
                          "AYB", "YR_RMDL", "EYB", "KITCHENS", "FIREPLACES", "LANDAREA")

for(i in continuous_variables){
print(i)
print(length(na.omit(df[,i])))
  }

reg.best <- regsubsets(log.PRICE~ BATHRM + HF_BATHRM + ROOMS + BEDRM + dist + AYB + YR_RMDL + EYB + KITCHENS + FIREPLACES + LANDAREA + 
                         STORIES, data = df,
                       nbest = 2, method = "seqrep")

plot(reg.best, scale = "adjr2", main = "Best fit models using sequential rep method in Leaps Package")
plot(reg.best, scale = "bic", main = "Best fit models using sequential rep method in Leaps Package")



?regsubsets

#Output from Leaps feature selection 
lm1 <- lm(log.PRICE ~ BATHRM + HF_BATHRM  + ROOMS + AYB + YR_RMDL + EYB + FIREPLACES + STORIES, data = df); summary(lm1)

#Include other continuous variables of interest

lm2 <- lm(log.PRICE ~ LANDAREA + BEDRM + ROOMS + BATHRM + HF_BATHRM + AYB + EYB + FIREPLACES + STORIES, data = df); summary(lm2)

#Include Categorical Variables to lm1 specification

lm3 <- lm(log.PRICE ~ BATHRM + HF_BATHRM + AYB  + EYB + FIREPLACES + STORIES + 
            QUALIFIED + CNDTN, data = df); summary(lm3)

#Include Location variable to lm3 specification

lm4 <- lm(log.PRICE ~ BATHRM + HF_BATHRM  + ROOMS + AYB + EYB  + STORIES + 
            QUALIFIED + CNDTN + QUADRANT, data = df); summary(lm4)

lm5 <- lm(log.PRICE ~ BATHRM + HF_BATHRM  + ROOMS + AYB  + STORIES + 
            QUALIFIED + CNDTN + WARD, data = df); summary(lm5)

lm6 <- lm(log.PRICE ~ BATHRM + HF_BATHRM  + ROOMS + AYB + EYB  + STORIES + 
            QUALIFIED + CNDTN + ZIPCODE, data = df); summary(lm6)

lm7 <- lm(log.PRICE ~ BATHRM + HF_BATHRM  + ROOMS + AYB + EYB  + STORIES + 
            QUALIFIED + CNDTN + ASSESSMENT_NBHD, data = df); summary(lm7)




