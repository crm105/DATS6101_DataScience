#Title: anova_analysis.R
#author: Chris Montgomery
#Input: 'dc_residential_data_clean.csv'
#Output:

#Description: This script conducts ANOVA and chi-square analyses for household characteristics of
#interest grouped by quadrant

library(tidyverse); library(broom)

df <- read.csv('data/dc_residential_data_clean.csv')
df$sale.year <- as.factor(df$sale.year)


#Identify continuous variables of interest for anova tests 

cont.variables <- c('BATHRM', 'HF_BATHRM', 'ROOMS', 'BEDRM', 'AYB',
                     'STORIES', 'PRICE', 'KITCHENS', 'FIREPLACES', 'LIVING_GBA', 'log.PRICE')

#Identify categorical variables of interest for chi-square tests

cat.variables <- c('HEAT', 'AC', 'GRADE', 'SOURCE', 'sale.year')

#Create an apply function that loops through each categorical variable and runs an ANOVA

chi.test.results <-  lapply(seq(1:length(cat.variables)),function(i){
  x <- df[,cat.variables[i]]
  y <- df[,"QUADRANT"]
  chisq.test(x,y)
  
})


#This loop runs ANOVA tests for each of our continuous variables

anova.test.results  <-  lapply(seq(1:length(cont.variables)),function(i){
   tidy ( (aov (df[,cont.variables[i]] ~ df[,'QUADRANT'])))
})


#This loop creates a data frame that houses the results of the anova tests
p.value <- c()
variable <- c()
for (i in 1:length(anova.test.results)){
  
  variable [i] <- cont.variables[i]
  p.value[i] <- as.numeric( anova.test.results[[i]]$p.value[1])
}

anova.results <- as.data.frame (cbind(variable,p.value))

#This loop produces box plots for each continuous variable in question 
p<- c()
for(i in cont.variables){
 print (ggplot(df, aes(x= QUADRANT, y = df[,i] )) + geom_boxplot()+ ylab(i) )

  
}

df$ZIPCODE <- as.factor(df$ZIPCODE)

lm1 <- lm(log.PRICE ~ BATHRM + BEDRM + ROOMS + HF_BATHRM + LIVING_GBA + ZIPCODE + dist  , data = df ); summary(lm1)


cor(df$BATHRM, df$HF_BATHRM)



