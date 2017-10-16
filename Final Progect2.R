.libPaths("E:/R-3.3.1/library")
library(faraway)
library(ggplot2)
library(car)
library(AER)
library(nlme)
library(GGally)
library(gridExtra)

data("USSeatBelts")
names(USSeatBelts)

#  Example
library("lattice")
xyplot(fatalities ~ as.numeric(as.character(year)) | state, data = USSeatBelts, type = "l")
#  We think that we can use the GLS model to predict the fatalities becuase it looks like that this dataset is a time series data.
#  But at the same time we try to use liner model to compare with the GLS one.

#  Find the NA in data
head(USSeatBelts)
summary(USSeatBelts)

#  There are 209 NAs in seatbelt, so remove those NAs
USSeatBelts <- na.omit (USSeatBelts)
head(USSeatBelts)
summary(USSeatBelts)

#  Use the scatter plots to visualize the relationships between each variables.
plot(USSeatBelts)
#ggpairs(USSeatBelts, columns=c(4,1:3,5:12))

#  Use the scatter plots to visualize the relationships between each numeric variables. 
pairs(~ fatalities + miles + seatbelt + income + age + year, data = USSeatBelts)

#  It is still confused to see the relationship between each numeric variables. So we chose one state 
#  OH for scatter plot.
USOH <- as.data.frame(subset(USSeatBelts, state == "OH"))
summary(USOH)
pairs(~ fatalities + miles + seatbelt + income + age + year, data = USOH)

#  It looks like that all numeric variables are correlated to each other in our GGPairs and scatter plots, 
#  and further we can use the ACF to check the relationship between each variables with year.
par(mfrow = c(3, 4))
acf(USSeatBelts$fatalities)
acf(USSeatBelts$miles)
acf(USSeatBelts$seatbelt)
acf(USSeatBelts$income)
acf(USSeatBelts$age)
acf(USSeatBelts$state)
acf(USSeatBelts$drinkage)
acf(USSeatBelts$speed65)
acf(USSeatBelts$speed70)
acf(USSeatBelts$alcohol)
acf(USSeatBelts$enforce)

#  We can see in these ACF graphs that all variables except drinkage in the first 2 lagged auto correlations are statistically different from zero. 
#  And it looks like that miles, seatbelt, income and age have similiar auto correlation structure as do fatalities. 

#  So here, we chose to use the data of one state OH to construct a simple GLS model and use a linear model as a comparasion. 
USOH <- as.data.frame(subset(USSeatBelts, state == "OH"))
summary(USOH)

g1 <- lm(fatalities ~ miles + seatbelt + income + age, data = USOH)
summary(g1)
shapiro.test(residuals(g1))
car::durbinWatsonTest(residuals(g1))
car::vif(g1)
par(mfrow = c(2, 2))
plot(g1)

#  The shapiro.test shows that the residual fit in normal density distribution, and we try to adjust the fatalities by log to optimize it.
glog <- lm(log(fatalities) ~ miles + seatbelt + income + age, data = USOH)
summary(glog)
shapiro.test(residuals(glog))
car::durbinWatsonTest(residuals(glog))
car::vif(glog)
par(mfrow = c(2, 2))
plot(glog)
par(mfrow = c(1, 1))
#  It looks like that risiduals are not independent and we accept the Null hypothesis that residuals come from normal density distribution.
#  In the four anomaly plots, we see that there is a sinusoidal pattern in the residuals against the fitted values, which is a clear indication that the residuals are not independent.
#  Besides, because the durbinwatsontest value 2.3395 is bigger than 2, we can conclude that there is positive auto correlation in this model.
#  And, by using VIF, we can find out the high collinearity in this model.
#  So we can't use OLS to fit this dataset.

#  Use the ACF to analyze residuals.
acf(residuals(glog))
#  We can find that the residuals are correlated with the year in the first lag because the ACF is close to 0.5.

#  Becuase these numeric variables are highly correlated with the year, we can use GLS to fit.
ggls <- gls(log(fatalities) ~ miles + seatbelt + income + age, 
            correlation = corAR1(form = ~ as.numeric(year)), data = USOH)
summary(ggls)
compareCoefs(glog, ggls)
#  When we use the GLS model, the Standard Error of efficiencies are less than those when we use the OLS model, and the P-values of efficiencies are less than those in OLS model,
#  So we can conclude that the GLS model is better than OLS model.

#  And the ACF analysis too
plot(ACF(ggls), alpha = 0.1)
#  In this model ACF is moderate in the first 1 lag, the result is better than that when we use the OLS model.

#  Draw a graph of fitted.value and the log fatalities.
qplot(log(USOH$fatalities), fitted.values(ggls)) + geom_abline(slope = 1, intercept = 0) + labs(title = "log(fatalities) VS Fitted.value")
#  So when we try to use one state and part of numeric variables to fit, it looks like that it fits very well. So what if we use whole variabels to fit.




# use linear model to fit and as a comparsion
g.lm <- lm(fatalities ~ . , data = USSeatBelts)
summary(g.lm)
shapiro.test(residuals(g.lm))
car::durbinWatsonTest(residuals(g.lm))
car::vif(g.lm)
# According to the shapiro test, we can find out that the residuals of this model is not normal density distribution because the p-value of test is less than 0.05.
# According to the durbinwatson test, we can find out that the residuals in this model is positive correlated with each others, because the DW value is 1.39 which is less than 1.5.
# According to the VIF test, we can find out that there are three variables multilineary with other variables, miles, income, and age.

par(mfrow = c(2, 2))
plot(g.lm)
par(mfrow = c(1,1))
plot(fitted.values(g.lm), USSeatBelts$fatalities, 
     main = "Fit Plot of g.lm",
     xlab = "Fitted Value",
     ylab = "Output")
abline(0, 1 , lty = 5)

# use adjusted linear model to fit
g.ad.lm <- lm(log(fatalities) ~ ., data = USSeatBelts)
summary(g.ad.lm)
shapiro.test(residuals(g.ad.lm))
car::durbinWatsonTest(residuals(g.ad.lm))
car::vif(g.ad.lm)
# According to the shapiro test, we can find out that the residuals of this model is not normal density distribution because the p-value of test is less than 0.05.
# According to the durbinwatson test, we can find out that the residuals in this model is positive correlated with each others, because the DW value is 1.48 which is less than 1.5.
# According to the VIF test, we can find out that there are three variables multilineary with other variables, miles, income, and age.

par(mfrow = c(2, 2))
plot(g.ad.lm)
par(mfrow = c(1, 1))
plot(fitted.values(g.ad.lm), log(USSeatBelts$fatalities), 
     main = "Fit Plot of g.ad.lm",
     xlab = "Fitted Value",
     ylab = "Output")
abline(0, 1, lty = 5)

# compare two kinds of linear model
compareCoefs(g.lm, g.ad.lm)

p1 <- qplot(.fitted, .resid, data = g.lm) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red", 
                                                                                   se = F)
p2 <- qplot(.fitted, abs(.resid), data = g.lm) + geom_hline(yintercept = 0, 
                                                            linetype = "dashed") + labs(title = "Scale-Location", x = "Fitted", y = "|Residuals|") + 
  geom_smooth(method = "lm", color = "red", se = F)
p3 <- qplot(.fitted, .resid, data =g.ad.lm) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted (Log)", x = "Fitted", y = "Residuals") + geom_smooth(color = "red", 
                                                                                         se = F)
p4 <- qplot(.fitted, abs(.resid), data = g.ad.lm) + geom_hline(yintercept = 0, 
                                                               linetype = "dashed") + labs(title = "Scale-Location", x = "Fitted", y = "|Residuals|") + 
  geom_smooth(method = "lm", color = "red", se = F)
grid.arrange(p1, p2, p3, p4, nrow = 2)
#  In the normal linear model, we can see that there is evidence of heterskedasticity, in other words nonconstant error variance, so the unadjusted one is not good.
#  In the adjusted model, the residuals look like randomly distributed, so we think that this one is better.
#  But there are evident about serial dependency, multilinearity. We can use the GLS model to analyze whole variables.




#  GLS for all variables except state and year.
g.all <- gls(log(fatalities) ~ .- state - year, 
             data = USSeatBelts, correlation = corAR1(form = ~ as.numeric(year) | state))
summary(g.all)
shapiro.test(residuals(g.all))
plot(g.all)

# since the P-value of "age" is 0.7429, so we decided to exclude "age"
g.all.f <- gls(log(fatalities) ~ .- state - year - age, 
             data = USSeatBelts, correlation = corAR1(form = ~ as.numeric(year) | state))
summary(g.all.f)

# since the P-value of "enforceprimary" is 0.4114, and the P-value of 
# "enforcesecondary" is 0.6136 so we decided to exclude "enforce"
g.all.f <- gls(log(fatalities) ~ .- state - year - age - enforce, 
             data = USSeatBelts, correlation = corAR1(form = ~ as.numeric(year) | state))
summary(g.all.f)

# since the P-value of "drinkage" is 0.6444, so we decided to exclude "drinkage"
g.all.f <- gls(log(fatalities) ~ .- state - year - age - enforce - drinkage , 
             data = USSeatBelts, correlation = corAR1(form = ~ as.numeric(year) | state))
summary(g.all.f)

# since the P-value of "speed65" is 0.6092, so we decided to exclude "speed65"
g.all.f <- gls(log(fatalities) ~ .- state - year - age - enforce - drinkage 
             - speed65, 
             data = USSeatBelts, correlation = corAR1(form = ~ as.numeric(year) | state))
summary(g.all.f)

# since the P-value of "alcohol" is 0.4687, so we decided to exclude "alcohol"
g.all.f <- gls(log(fatalities) ~ .- state - year - age - enforce - drinkage 
             - speed65 - alcohol, 
             data = USSeatBelts, correlation = corAR1(form = ~ as.numeric(year) | state))
summary(g.all.f)

# since the P-value of "miles" is 0.2620, so we decided to exclude "miles"
g.all.f <- gls(log(fatalities) ~ .- state - year - age - enforce - drinkage 
             - speed65 - alcohol - miles, 
             data = USSeatBelts, correlation = corAR1(form = ~ as.numeric(year) | state))
summary(g.all.f)
shapiro.test(residuals(g.all.f))
plot(g.all.f)

# Compare the fitted plot of g.all and g.all.f
par(mfrow = c(1,2))
plot(fitted.values(g.all), log(USSeatBelts$fatalities), 
     main = "Fit Plot of g.all",
     xlab = "Fitted Value",
     ylab = "Output")
abline(0, 1, lty = 5)

plot(fitted.values(g.all.f), log(USSeatBelts$fatalities), 
     main = "Fit Plot of g.all.f",
     xlab = "Fitted Value",
     ylab = "Output")
abline(0, 1, lty = 5)

# Use AIC value to compare these two GLS model.
AIC(g.all)
AIC(g.all.f)
#  According to the AIC value, we can conclude that the second model is better because it has a smaller AIC value
#  than the first one.

# compare residuals between g.ad.lm and g.all.f
par(mfrow = c(1, 2))
plot(fitted.values(g.ad.lm), abs(g.ad.lm$residuals), 
     main = "Residual Plot of g.ad.lm",
     xlab = "Fitted Value",
     ylab = "Residuals")
abline(0, 0, lty = 5)

plot(fitted.values(g.all.f), abs(g.all.f$residuals), 
     main = "Residual Plot of g.all.f",
     xlab = "Fitted Value",
     ylab = "Residuals")
abline(0, 0, lty = 5)
  # we concluede that we need to use the g.all.f model to fit this dataset.

































