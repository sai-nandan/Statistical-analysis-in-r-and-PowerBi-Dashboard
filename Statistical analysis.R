
#set working directory
setwd("F:/SALFORD DATA SCIENCE/Semester 2/Applied Statistics & Data Visualization/R stats")

###  installing required packages ###
install.packages("datarium")
install.packages("qqplotr")
install.packages("RVAideMemoire")
install.packages("corrplot")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("test")
install.packages("psych")
install.packages("ggpubr")
install.packages ("pastecs")
install.packages("e1071")
install.packages("car")
install.packages("TTR")
install.packages("forecast")

### importing required packaged ###
library(datarium)
library(qqplotr)
library(RVAideMemoire)
library(corrplot)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(zoo)
library (pastecs)
library(stats)
library(psych)
library(e1071)
library(ggpubr)
library(car)
library(TTR)
library(forecast)

#import Data
new_val = read.csv('Task_2.csv', header = TRUE)


new_val <- new_val %>% mutate(Group =
                                case_when(Country_Name == "China" ~ "Upper", 
                                          Country_Name == "Brazil" ~ "Upper", 
                                          Country_Name == "Malaysia" ~ "Upper", 
                                          Country_Name == "Thailand" ~ "Upper", 
                                          Country_Name == "Mauritius" ~ "Upper", 
                                          Country_Name == "Maldives" ~ "Upper", 
                                          Country_Name == "India" ~ "lower", 
                                          Country_Name == "Indonesia" ~ "lower", 
                                          Country_Name == "Morocco" ~ "lower", 
                                          Country_Name == "Bangladesh" ~ "lower", 
                                          Country_Name == "Philippines" ~ "lower", 
                                          Country_Name == "Vietnam" ~ "lower", ))

#converting the data to numeric
new_val[,3:11] = sapply(new_val[, 3:11],as.numeric)

View(new_val)
data.frame(colnames(new_val))

#outlier detection
IQR(new_val$Life_expectancy)
IQR(new_val$Out.of.pocket_exp_...of.current.health.expenditure.)
IQR(new_val$X.People_using_basic.drinking_water_services)



#column wise missing values
colSums(is.na(new_val))

#view dimension of data
dim(new_val)

##  4.1 DESCRIPTIVE ANALYSIS ##

#complete statistics of data frame
stat.desc(new_val)

#skewness
skewness(new_val$Life_expectancy)
skewness(new_val$Out.of.pocket_exp_...of.current.health.expenditure.)
skewness(new_val$X.People_using_basic.drinking_water_services)

#kurtosis
kurtosis(new_val$Life_expectancy)
kurtosis(new_val$Out.of.pocket_exp_...of.current.health.expenditure.)
kurtosis(new_val$X.People_using_basic.drinking_water_services)




###   4.2 correlation analysis   ###
cormat <- round(cor(new_val[,3:11]),2)
head(cormat)


#correlation plot
corrplot(cor(new_val[,3:11]), method = "number")



hist(Life_expectancy  ~  Out.of.pocket_exp_...of.current.health.expenditure., col='blue', xlab='Health Expenditure')
ggplot(new_val) + geom_point(aes(x = Out.of.pocket_exp_...of.current.health.expenditure. , y = Life_expectancy)) 
+ stat_smooth()




plot(Life_expectancy  ~  Out.of.pocket_exp_...of.current.health.expenditure., data=new_val)

plot(Life_expectancy  ~  X.People_using_basic_sanitation_services, data=new_val)






################      _ 4.3  Regression Analysis           ###################

#####     Single regression   #####

Model <- lm(Life_expectancy  ~  Out.of.pocket_exp_...of.current.health.expenditure., data = new_val)
summary(Model)



#plot to visualize fitted regression line
ggplot(Model, aes(x = Out.of.pocket_exp_...of.current.health.expenditure. , y = Life_expectancy)) +
  geom_point() +
  stat_smooth()


#residual independence plot for Single regression
plot(Model, 1)

#residual normality plotfor Single regression
plot(Model, 2)

#equal variance(Homoscedasticity) plot for Single regression
plot(Model, 3)


# Model coeficients for Single regression
reg_model <- summary(Model)
reg_model$coefficients

#95% confidence interval of the regression coefficients  for Single regression
confint(Model, level=0.95)

#Fitted values
uety <- fitted.values(Model) 
sum(uety)#total fitted values
sum(Model$Life_expectancy) #sum of observed values for Single regression

#residuals
res = residuals(Model)
sum(res)


########       Multiple regression analysis       ##########
Model1 <- lm(Life_expectancy  ~  Out.of.pocket_exp_...of.current.health.expenditure.+ 
               X.People_using_basic_sanitation_services, data = new_val)
summary(Model1)

#plot to visualize fitted regression line for multiple regression
ggplot(Model1, aes(x = Out.of.pocket_exp_...of.current.health.expenditure. , y = Life_expectancy)) +
  geom_point() +
  stat_smooth()


#residual independence plot for multiple regression
plot(Model1, 1)

#residual normality plot for multiple regression
plot(Model1, 2)

#equal variance(Homoscedasticity) plot for multiple regression
plot(Model1, 3)


#Variation inflation factor measure collonearity for multiple regression
vif(Model1)

#model summary
reg_model <- summary(Model1)
reg_model$coefficients

#95% confidence interval of the regression coefficients for multiple regression
confint(Model1, level=0.95)

#Fitted values
uety <- fitted.values(Model1) 
sum(uety)#total fitted values
sum(Model1$Life_expectancy) #sum of observed values

#residuals
upper_res = residuals(Model1)
sum(upper_res)



############     4.4    Time series analysis            ###########
life_expectancy_ts <- ts(new_val$Life_expectancy, start = 2002, end = 2019)
life_expectancy_ts


#plotting and Forecasting time series
plot.ts(life_expectancy_ts)
uf <- forecast(life_expectancy_ts)
plot(uf)

#Holtwinters exponential smoothing
hw1 <- HoltWinters(life_expectancy_ts, beta = FALSE, gamma = FALSE)
hw1
hw1$fitted
plot(hw1)

#sum of squared errors
hw1$SSE
uf <- forecast(hw1, h=10)
uf
plot(uf)


#acf to look at maximum lag
acf(uf$residuals, lag.max=5, na.action = na.pass)

#BOX Test
Box.test(uf$residuals, lag=5, type="Ljung-Box")

#plot the residuals
plot.ts(uf$residuals)

#removing null values from residual vector
uf$residuals <-uf$residuals[!is.na(uf$residuals)]

#forecast errors
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(100, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=TRUE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(uf$residuals)




#Applying auto arima
Am <- auto.arima(life_expectancy_ts, seasonal =TRUE)
summary(Am)
accuracy(forecast(Am))

checkresiduals(Am)


########    4.5 Hypothesis Testing     ##########

#assumptions
shapiro.test(new_val$Life_expectancy)
shapiro.test(new_val$Out.of.pocket_exp_...of.current.health.expenditure.)

ks.test(new_val$Life_expectancy, 'pnorm')
ks.test(new_val$Out.of.pocket_exp_...of.current.health.expenditure., 'pnorm')

#sample t.test
#check if the Life_expectancy is equal to Out.of.pocket_exp_...of.current.health.expenditure.
t.test(new_val$Life_expectancy, new_val$Out.of.pocket_exp_...of.current.health.expenditure., alternative = "two.sided")



#Anova Test

aov <- aov(Life_expectancy  ~  Out.of.pocket_exp_...of.current.health.expenditure., data = new_val)

summary(aov)




### Chi square test ###
chi <- chisq.test(new_val$Life_expectancy, new_val$Group)
chi

