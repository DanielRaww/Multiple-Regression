rm(list=ls()) 

#Model 1 (Life.expectancy)
life<-read.csv("lifeexp2015.csv")
str(life)
lifeC<-life[,-c(1,2)]
str(lifeC)
lifeO<-na.omit(lifeC)
str(lifeO)
library(stats)
cor(lifeO)
#high inverse correlation for Adult.Mortality, HIV.AIDS. And positive correlation
#with income and schooling
lmLife<-lm(Life.expectancy~., data = lifeO)
summary(lmLife)
#takeout percentage.expenditure, Hepatitis.B, measles, BMI,  Polio, Diphtheria, GDP, Population, thinness, schooling since there
#individual hypothesis is greater than 0.05
lmLife<-lm(Life.expectancy~Adult.Mortality+infant.deaths+under.five.deaths+HIV.AIDS+Income.composition.of.resources, data = lifeO)
summary(lmLife)
library(car)
vif(lmLife)
#Only the independent variables: Adult.Mortality, HIV.AIDS, Income.composition.of.resources have a 
#VIF of under 5.  On the other hand, infant.deaths and under.five.deaths have a problematic amount of
#collinearity.  Hence, they will be removed.
lmLife<-lm(Life.expectancy~Adult.Mortality+HIV.AIDS+Income.composition.of.resources, data = lifeO)
summary(lmLife)
plot(lmLife)
#Residuals vs fitted: shows a random pattern (183, 4, 53)
#Normal Q-Q: has points that are very close to the line (183, 53, 4)
#Scale-Location: has a somewhat flatline (183, 4, 53)
#Cooks distance: suggests 53 and 183 could be outliers
plot(cooks.distance(lmLife))
plot(lmLife, which = c(4))
#53 and 183 are outliers
lifeE<-lifeO[-c(53,183),]
#I removed 53 and 183 as both of the cooks distance graphs suggests that they are outliers as these points
#are nowhere near the line of best fit.
str(lifeE)
lmLife<-lm(Life.expectancy~Adult.Mortality+HIV.AIDS+Income.composition.of.resources, data = lifeE)
summary(lmLife)
#Yes, this model is statistically significant.
#The F-statistic shows that the p-value is less than 0.05, suggesting that the model is sound with the variables in it
#The multiple R-squared is ~0.88 and the adjusted R-squared is ~0.88, which explains about 88% of the variance
#In the individual hypothesis, all of the independent variables have a p-value of less than 0.05, suggesting that the independent
#variables in the model are statistically significant
#In the estimate, the slopes for each of these variables are small except for 
#HIV.AIDS and Income.composition.of.resources. The negative slope for HIV makes sense 
#and the positive slope for Income makes sense as well
#Life.expectancy = 50.113682 - 0.019662 (Adult.Mortality) - 0.748200 (HIV.AIDS)
#                   +36.102925 (Income.composition.of.resources)
hist(lmLife$residuals)
mean(lmLife$residuals)
#The mean is close to 0
library(lmtest)
lmtest::bptest(lmLife)
#Since P is less than 0.05, the model is heteroscedastic


#Model 2 (Income and HIV on Life expectancy)
expe<-read.csv("lifeexp2015.csv")
str(expe)
expe<-expe[,-c(1,2)]
expe<-na.omit(expe)
lmHIV<-lm(Life.expectancy~HIV.AIDS, data=expe)
summary(lmHIV)
#In the regression between HIV.AIDS and Life.expectancy, the F-statistic shows that
#the p value is less than 0.05,indicating that HIV.AIDS is statistically significant.
#The multiple R-squared and Adjusted R-squared reveals that HIV.AIDs explains around
#38% of the variance, which is quite low.  The individual hypothesis is less than 0.05, indicating that it is
#statistically significant. Finally, the slope is negative, and for every one unit increase
#in HIV.AIDS causes a 3.2643 unit decrease in life expectancy.
lmIncome<-lm(Life.expectancy~Income.composition.of.resources, data = expe)
summary(lmIncome)
#In the regression between Income and Life.expectancy, the F-statistic shows that
#the p value is less than 0.05,indicating that Income is statistically significant.
#The multiple R-squared and Adjusted R-squared reveals that HIV.AIDs explains around
#80% of the variance, a high number.The individual hypothesis is less than 0.05, indicating that it is
#statistically significant. Finally, the slope is positive, and for every one unit increase
#in Income causes a 47.496 unit increase in life expectancy.
lmCombo<-lm(Life.expectancy~HIV.AIDS*Income.composition.of.resources, data = expe)
summary(lmCombo)
#The F-statistic shows that the model has a p-value that is less than 0.05, indicating
#that the model is sound with the current independent variables
#The multiple R-squared is ~0.86 and the adjusted r-squared is ~0.86, which explains about
#86% of the variance, but more importantly,the variance with both of these variables together is higher than with Income alone.
#In the individual hypothesis, all of the variables are statistically significant except for HIV.AIDS. 
#Being in a country that has a high income.composition.of.resources greatly increases your life expectancy
#as it has a direct effect.Although Income along with HIV AIDS gives a statistically significant value
#(p=value of less than 0.05), considering them together actually make it a little bit less important due to the
#negative slope.  Although it is still important, it will not help your life expectancy, rather these two
#variables together will flatten the curve. Similarly, the slope of Income compared to the regression that
#does not involve any other independent variables actually decreased from ~47 to ~42, making it less important.
#this makes sense since other variables are being considered into the regression now.

#Life.expectancy = 42.908 + 1.138 (HIV.AIDS) + 42.870 (Income.composition.of.resources) 
#                 - 4.717 (HID.AIDS * Income.composition.of.resources)
plot(lmCombo)
#Residuals vs fitted: shows a random pattern (183, 4, 144)
#Normal Q-Q: has points that are very close to the line (183, 144, 4)
#Scale-Location: has a somewhat flatline (183, 4, 144)
#Cooks distance: 183 is close to being an outlier, but not certain. Do further tests.
plot(cooks.distance(lmCombo))
plot(lmCombo, which = c(4))
str(expe)
#I am not considering 183 to be an outlier due to the lack of observations
hist(lmCombo$residuals)
mean(lmCombo$residuals)
#The mean is close to 0
library(lmtest)
lmtest::bptest(lmCombo)
#Since P is 0.1562, which is greater than 0.05, the model is homoscedastic
str(expe)
expeR<-expe[,-c(2,3,4,5,6,7,8,9)]
expeR<-expeR[,-c(2,4,5,6,7,9)]
str(expeR)
attach(expeR)
Combo<-HIV.AIDS*Income.composition.of.resources
scatterplot3d::scatterplot3d(HIV.AIDS,Income.composition.of.resources,Combo,angle = 60, color  = "Blue", pch = 4)
#Model 3 
#Evaluate a nonlinear polinomial relationship of GDP that best predicts Life.expectancy
poly<-read.csv("lifeexp2015.csv")
str(poly)
poly<-poly[,-c(1,2)]
str(poly)
poly<-na.omit(poly)
str(poly)
lmSim<-lm(Life.expectancy~GDP, data = poly)
summary(lmSim)
#The F-statistic and the individual hypothesis all suggest that the model is significant.
#Multiple r-squared is ~0.24 and adjusted r-squared is 0.23, explaining ~23% of the variance
#However, lets test for a nonlinear relationship
plot(Life.expectancy~GDP, data = poly) #seems logarithmic at first glance
plot(lmSim)
#Residuals vs fitted: Is not a random pattern, which is concerning.(33, 144)
#Normal Q-Q: has points that are very close to the line, 144 is concerning (33, 144)
#Scale-Location: has a somewhat flatline (33, 144)
#Cooks distance: Do further tests, seems like there are no outliers
plot(cooks.distance(lmSim))
plot(lmSim, which = c(4))
hist(lmSim$residuals)
mean(lmSim$residuals)
#The mean is close to 0
library(lmtest)
lmtest::bptest(lmSim)
#Has a p value that is less than 0.05, it is heteroscedastic
library(ggplot2)
ggplot(poly, aes(x=GDP,y=Life.expectancy))+geom_point() +geom_smooth(method="lm")+geom_smooth(method="loess",color="Red")

##lmLog = lm(log1p(Life.expectancy) ~ log1p(GDP), data = poly)
##summary(lmLog)
##thought it was logarithmic at first.
##Only explains about 22% of the variance

##lmSim<-lm(Life.expectancy~GDP, data = poly) #linear
##summary(lmSim)
##explains about 23% of variance

#lmSimQuad<-lm(Life.expectancy~GDP + I(GDP^2), data = poly) #quadratic improvement
#summary(lmSimQuad)
##explains about 29% of variance

#lmSimQuart<-lm(Life.expectancy~poly(GDP,4), data= poly) #quartic improvement
#summary(lmSimQuart)
##explains about 30% of variance, but does not pass individual hypothesis test


###In the cubic model, the third order is not significant - pointing to the fact that you went one too far.
###A quadratic model is superior because you don't need to worry about the linear assumptions and the quad effect was significant

lmSimCubic<-lm(Life.expectancy~poly(GDP,3), data= poly) #cubic improvement
summary(lmSimCubic)
#explains about 30% of variance, residual standard error slightly decreased as well
#The f-statistic shows that the model has a p-value that is less than 0.05, a strong
#indication that the model is sound.
#The multiple r-squared is ~0.30 and the adjusted r-squared is ~0.29, explaining about
#30% of the variance.
#the individual hypothesis for each degree is less than 0.05, an indication that it is
#statistically significant. The residual error is also small in this model

#Life.expectancy = 70.7415 + 44.2372(GDP) - 21.4798(GDP)^2 + 7.9820 (GDP)^3

#attach(poly)
#lnLife<-log(Life.expectancy)
#lnGDP<-log(GDP)
#plot(lnLife~GDP)
#plot(Life.expectancy~lnGDP) #linear
#plot(lnLife~lnGDP) #some what linear
#summary(lm(Life.expectancy~GDP))
#summary(lm(Life.expectancy~lnGDP))
##explains about 23% of variance
#summary(lm(lnLife~lnGDP))
##explains about 22% of variance