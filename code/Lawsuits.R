library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(rpart)
library(rpart.plot)
library(writexl)
library(car)
library(lmtest)
library(sandwich)
library(plm)
library(Rcpp)
getwd()
data = read_xlsx("Lawsuits.xlsx")

head(data)
tail(data)


#DV(Y): Revenue_Change , Stock_Price_Change
#IV: Count_Lawsuits 

model <- lm(Revenue_Change ~ Count_Lawsuits, data = data)
summary(model)

model2 <- lm(Stock_Price_Change ~ Count_Lawsuits, data = data)
summary(model2)


cor(data$Count_Lawsuits, data$Revenue_Change)
cor(data$Count_Lawsuits, data$Stock_Price_Change)
cor(data$Stock_Price_Change, data$Revenue_Change)

#Plots
plot(data$Count_Lawsuits , data$Revenue_Change , xlab = "Count_Lawsuits", ylab = "Revenue_Change",
     main = "Plot Revenue_Change against Count_Lawsuits")
abline(model, col = "red")

plot(data$Count_Lawsuits , data$Stock_Price_Change , xlab = "Count_Lawsuits", ylab = "Stock_Price_Change",
     main = "Plot Stock_Price_Change against Count_Lawsuits")

plot(data$Stock_Price_Change , data$Revenue_Change , xlab = "Stock_Price_Change", ylab = "Revenue_Change",
     main = "Plot Revenue_Change against Stock_Price_Change")

plot(data$x, residual, xlab="X", ylab="Residuals",
     main = "Plot residuals against X") 


#Checking for linearity & other assumptions :(  ______________________________________________

# Calculate the residuals
residual <- resid(model)

# Plot residuals and x
plot(data$Count_Lawsuits, residual, xlab="Count_Lawsuits", ylab="Residuals",
     main = "Plot residuals against Count_Lawsuits") 
# Add the horizontal line
abline(0, 0, col = "red")                  

# Plot residuals against the fitted values
plot(fitted(model), residuals(model), main = "Residuals vs. Fitted")
# Add the horizontal line
abline(0, 0, col = "red") 

# Create a histogram of residuals
hist(residual, breaks = 25, xlab = "Residuals", ylab = "Frequency",
     main = "Histogram of Residuals", col = "white", prob = TRUE)
# Add a density line
lines(density(residual), col = "red")

# Create a Q-Q plot for residuals
qqnorm(residual)
# Add a straight diagonal line to the plot
qqline(residual) 




#_______________________________________________________________________
data2 = read_xlsx("Financial.xlsx")

head(data2)
tail(data2)

pdata <- pdata.frame(data2, index = c("Company", "Year"))

is.pbalanced(pdata)
pdim(pdata)
pdata$Year

#some graphs:
d1=aggregate(pdata$Revenue, list(pdata$Company), FUN=mean)
colnames(d1)=c("Company", "Revenue")
ggplot(data=d1) +geom_point(aes(x=Company, y=Revenue), col="red")

d2=aggregate(pdata$Revenue, list(pdata$Year), FUN=mean)
colnames(d2)=c("Year", "Revenue")
ggplot(data=d2) +geom_point(aes(x=Year, y=Revenue), col="red") 


#Pooled
pooled=plm(Revenue~Count, data=pdata, 
           index=c("Company", "Year"),
           model="pooling")
summary(pooled)

pooled2=plm(Stock_Price~Count, data=pdata, 
           index=c("Company", "Year"),
           model="pooling")
summary(pooled2)

#________________________________________________________
#FE models:
fe=plm(Revenue~Count, data=pdata, 
       index=c("Company", "Year"), effect="individual",
       model="within")
summary(fe)
fixef(fe)

fe2=plm(Stock_Price~Count, data=pdata, 
       index=c("Company", "Year"), effect="individual",
       model="within")
summary(fe2)

lsdv=lm(Revenue~Count+factor(Company), data=pdata)
summary(lsdv)

lsdv2=lm(Stock_Price~Count+factor(Company), data=pdata)
summary(lsdv2)

str(pdata)


#___________________________________________________________
#RE model:
re=plm(Revenue~Count, data=pdata, 
       index=c("Company", "Year"), effect="individual",
       model="random")
summary(re)

re2=plm(Stock_Price~Count, data=pdata, 
       index=c("Company", "Year"), effect="individual",
       model="random")
summary(re2)

#Model Selection:
linearHypothesis(lsdv, matchCoefs(lsdv, "Company"))
linearHypothesis(lsdv2, matchCoefs(lsdv2, "Company"))
#p-value<0.05 --> FE model
phtest(fe,re)
#p-value<0.05 --> FE model
phtest(fe2,re2)
#p-value>0.05 --> RE model


#Thus we use FE Estimator for Revenue:
fe=plm(Revenue~Count, data=pdata, 
       index=c("Company", "Year"), effect="individual",
       model="within")
summary(fe)
fixef(fe)
#p-value = 0.0263 :)
#Adj. R-Squared = -0.17299 :(

#And RE model for Stock_price:
re2=plm(Stock_Price~Count, data=pdata, 
        index=c("Company", "Year"), effect="individual",
        model="random")
summary(re2)
#p-value = 0.70421 :(
#Adj. R-Squared = -0.0091887 :(

#Test for serial correlation:
pbgtest(fe) 
pbgtest(re2) 
#--> there is serial correlation for Revenue model --> use robust standard error
coeftest(fe,vcovHC)
#now, p-value = 0.191
#--> not as good as before :(

#Plots:
plot(data2$Count , pdata$Revenue , xlab = "Count", ylab = "Revenue",
     main = "Plot Revenue against Lawsuits")

plot(data2$Count , pdata$Stock_Price , xlab = "Count", ylab = "Stock_Price",
     main = "Plot Stock_Price against Lawsuits")

#Should we check the linear regression assumptions here too?
# Calculate the residuals
residual <- resid(fe)

# Plot residuals and x
plot(data2$Count, residual, xlab="Count", ylab="Residuals",
     main = "Plot residuals against Count_Lawsuits") 
# Add the horizontal line
abline(0, 0, col = "red")                  

# Create a histogram of residuals
hist(residual, breaks = 25, xlab = "Residuals", ylab = "Frequency",
     main = "Histogram of Residuals", col = "white", prob = TRUE)
# Add a density line
lines(density(residual), col = "red")

# Create a Q-Q plot for residuals
qqnorm(residual)
# Add a straight diagonal line to the plot
qqline(residual) 
 
#The "Financial" data plot look better than "Lawsuits"

