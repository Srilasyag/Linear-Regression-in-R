#H0: A relationship between dividend and stock_return_scaled doesn't exist
#Ha: A relationship between dividend and stock_return_scaled does exist

#Load Libraries
library(dplyr)
library(ggplot2)

#Dataset Review
ols_stock
str(ols_stock)

#Disp vs. mpg 
ggplot(ols_stock,aes(x =dividend, y =stock_return_scaled )) + geom_point(colour = "red")


#Correlation
cor(ols_stock$dividend,ols_stock$stock_return_scaled)

#Correlation with Transformation
cor(sqrt(ols_stock$dividend), sqrt(ols_stock$stock_return_scaled))

#OLS line
ggplot(ols_stock, aes(x = sqrt(dividend), y = sqrt(stock_return_scaled ))) + geom_point(colour = "red") + geom_smooth(method = "lm", fill = NA)

#Create Linear Model
lmodel <- lm(sqrt(stock_return_scaled ) ~ sqrt(dividend), data = ols_stock)
summary(lmodel)

#Since the P-value (0.8955) is larger than 0.001 we do not reject the Ho and conclude that relationship between dividend  and stock_return_scaled does not exist


_____________________
#Build Linear Model
simple.fit<-lm(dividend~stock_return_scaled, data=ols_stock)
LinearModel<-simple.fit 
#Summary of Key Statistics of the Model 
summary(LinearModel)

#Null and alternate hypothesis
#Ho: ??=0, co-efficient ?? of the predictor is zero and not statistically significant
#Ha: ?? ???0, co-efficient ?? of the predictor is not equal to zero and is statistically significant

#The P-value of the model is 0.7579 and the p-value of the predictor variables is 0.757925 which are both greater than the significance level of 0.001
#which indicates that co-efficient ?? of the predictor is zero and not statistically significant. 
#Here the t-value is small and equal to -0.310, indicating that the the coefficient is more likely equal to zero purely by chance.


#__________
library(correlation)
correlation(data=ols_stock)

cor(ols_stock$debt_to_equity,ols_stock$stock_return_scaled)
  
  
cor(ols_stock$marketcap,ols_stock$stock_return_scaled) 

cor(ols_stock$earnings_ranking,ols_stock$stock_return_scaled)
  
  
cor(ols_stock$stock_return,ols_stock$stock_return_scaled)  
  
)

