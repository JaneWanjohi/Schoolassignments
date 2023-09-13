rm(list=ls())#clears your environment
require(graphics)
#For each of 24 males, the maximum volume of oxygen uptake in the blood 
#and the time taken to run 2 miles (in seconds) were measured.  
#Interest lies on how the time to run 2 miles depends on the oxygen uptake.


oxygen=c(42.3,53.1,42.1,50.1,42.5,42.5,47.8,49.9,36.2,49.7,41.5,46.2,48.2,43.2,51.8, 53.3, 53.3,47.2,56.9,47.8,48.7,53.7,60.6,56.7)
time=c(918, 805, 892, 962, 968, 907, 770, 744, 1045, 810, 927, 813, 858, 860, 760, 747, 743, 803, 683, 844, 755, 700, 748, 775)

#create a dataframe
oxtime<-data.frame(oxygen, time)

summary(oxygen)
summary(time)
X11()##to show graphs in another screen
plot(time, oxygen, col="red", main="Oxygen uptake", type="p",pch=16)

##Fit a linear model
lmoxy<-lm(oxygen~time)

summary(lmoxy)
names(lmoxy)
confint(lmoxy)
confint(lmoxy, level=0.99)

sld<-summary(lmoxy)
coef(sld)  #  more details on coefficients


##plot the regression line
plot(time, oxygen, col="red", main="Oxygen uptake", type="p",pch=16)
abline(lmoxy)

X11()
##Analysis of variance table
anova(lmoxy)
par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lmoxy, las = 1)      #  Plot Residuals, Fitted, ...
dev.off()
lm.influence(lmoxy)
coef(lmoxy)  # the bare coefficients


##prediction interval
lmoxy = lm(oxygen~time)
newx = seq(min(time),max(time),by = 0.05)
pred=predict.lm(lmoxy, interval="confidence")
plot(time, oxygen, col="red", main="Oxygen uptake", type="p",pch=16)
lines(time, pred[,"fit"])
pred.new=predict(lmoxy, newdata =data.frame(time=newx),interval="prediction",level=.95)
lines(newx, pred.new[,2], col="blue", lty=2)
lines(newx, pred.new[,3], col="blue", lty=2)






##install.packages("smss","faraway","MASS","alr4")
library(alr4)
require(smss)
require(faraway)
require(MASS)
data(statewide.crime.2)
crime<-statewide.crime.2
attach(crime)
head(crime)
##The data are from Statistical Abstract of the 
##United States and most variables were measured in 1993. 

##51 observations of the following 8 variables,
#State  U.S. State
#VR violent crime rate (per 100,000 people in population)
#MR murder rate (per 100,000 people in population)
#M percent in metropolitan areas
#W percent white
#H percent high school graduates
#P percent with income below the poverty level
#S percent of families headed by a single parent


#X11()
plot(crime$VR, crime$M, col="red", 
     main="Violent crime rate vs % metropolitan area",
     type="p",pch=16)
##try other plots to see the relationship between variables
X11()
##Martix plot
pairs(crime[,2:8], pch = 19, col="red", lower.panel = NULL)


#Fit linear models
lm1<-lm(VR ~ M)
summary(lm1)
lm2<-lm(VR ~ M+W)
summary(lm2)

lmall<-lm(VR ~ M+W+H+P+S)
summary(lmall)
##single term additions
add1(lm1,scope=lmall,test="F")
#Akaike information criterion (AIC) is an estimator of 
#prediction error and thereby relative quality of 
#statistical models for a given set of data.

#Let k be the number of estimated parameters in the model. #
#Let  l the likelihood function for the model. 
#Then the AIC value of the model is the 
#AIC = 2 k ??? 2 ln \hat{l}, so smaller AIC the better the model.
##single term additions
add1(lm1,scope=lmall,test="F")


fit<-fitted(lmall)  # predicted values
res<-residuals(lmall)  # residuals
cat('\nfitted values:',fit,'\n')
cat('\nresidual values:',res,'\n')

Cl<-cor(crime[2:8])
symnum(Cl)### are some of the explanatory variables highly correlated ?

vif(lmall)##Variance Inflation Factor
#########################
##What do you conclude?##
#########################
summary(lmall)

## Excluding terms that are not significant
X11()
lmupd<-lm(VR ~ M+P+S)
summary(lmupd)
par(mfrow = c(2,2))
plot(lmupd)
State[51]

vif(lmupd)

##prediction
predict(lmupd, newdata=data.frame( 
  M=c(60,80), P=c(10,20), S=c(8,15)))


#stepwise wise regression
step(lmall<-lm(VR ~ M+W+H+P+S))
#####
##Compute Cp Mallows
library(leaps)
Cp<-leaps(crime[,2:8],VR, method=c("Cp", "adjr2", "r2"),nbest=4)
summary(regsubsets(crime[,2:8],VR))

####
#setwd("~/Cameroon/AIMS 2022/Slides/Rcode")
load('LM.Rdata')
ls()
### ozone
#A study the relationship between atmospheric ozone concentration and meteorology 
#in the Los Angeles Basin in 1976. A number of cases with missing variables have been removed for simplicity.
# These data record the level of atmospheric ozone concentration 
# from eight daily meteorologicalmeasurements made in the 
# Los Angeles basin in 1976.  We have the 330 complete cases.  
# We want  to  find  climate/weather  factors  that  impact  ozone  readings.   Ozone  is  a  hazardous by product of burning fossil fuels 
# and can harm lung function.  The data set for this problem is

data(ozone)
# 
# Variable Name
# Definition
# O3 Log Maximum Ozone
# vh Vandenberg 500 mb Height
# wind Wind Speed (mph)
# humidity Humidity (%)
# temp Sandburg AFB Temperature
# ibh Inversion Base Height
# dpg Daggot Pressure Gradient
# ibt Inversion Base Temperature
# vis Visibility (miles)
# doy Day of the Year
# Source
# Breiman, L. and J. H. Friedman (1985). Estimating optimal transformations for multiple regression and correlation. Journal of the American Statistical Association 80, 580-598.

## Data 

str(ozone) #Displays the internal structure of the data
head(ozone)

###DO EDA, e.g.
X11()
pairs(ozone[1:5], pch = 19, lower.panel = NULL)
pairs(c(ozone[1], ozone[6:10]), pch = 19, lower.panel = NULL)


##Ozone data is to be analysed in Assignment 1


