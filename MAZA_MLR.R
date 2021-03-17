# Required package 
install.packages("foreign")
install.packages("rstatix")
install.packages("MASS")
install.packages("apaTables")
library(foreign)
library(rstatix)
library(MASS)
library(apaTables)
library(finalfit)

#-----------------------------------------------------------------------------------------------------------------------------

# Data preparation 
setwd("~/Desktop/Medical Statistic/IIUM Biostatistic Workshop/IIUM_Biostatistic_workshop")
data <- read.csv(file="healthstatus6.csv") 
attach(data)
str(data)

#-----------------------------------------------------------------------------------------------------------------------------

# Simple Linear regression
# Previously we have seen that there is a significant linear relationship between blood pressure and HbA1C  

plot(hba1c, sbp, main="HbA1c versus Systolic blood pressure", xlab="HBA1C ", ylab="Systolic blood pressure ", pch=19)
abline(lm(sbp~hba1c), col="red") # regression line (y~x) 
cor.test(hba1c, sbp, method=c("pearson")) #correlation for normally distributed data

plot(hba1c, dbp, main="HbA1c versus Diastolic blood pressure", xlab="HBA1C ", ylab="Diastolic blood pressure ", pch=19)
abline(lm(dbp~hba1c), col="red") # regression line (y~x) 
cor.test(hba1c, dbp, method=c("pearson")) #correlation for normally distributed data

# Now we can also measure the linear relationship between each blood pressure and HbA1c using simple linear regression
# This relationship between 2 variable is stil bivariate analysis
# This relationship can be represented by the equation SBP = HBA1c * b1 + a
# Our objective here is how hba1c value can affect the sbp value.
# In this formula, a is the intercept and b is the slope, hence 1 unit increase in HBA1c,the SBP will increase by "b" coefficient unit
# And similarly for Diastolic blood pressure 
# A linear regression can be calculated in R with the command lm which stands for linear model
# You can get the summary of the model using summary command
# Compute the model and model summary
lmsbp <- lm(sbp~hba1c,data=data)
summary(lmsbp)

lmdbp <- lm(dbp~hba1c,data=data)
summary(lmdbp)

# You will get the a and b
# a is the intercepts 122.9670 and b is the slope 1.3169
# The coefficients estimates show the value for intercepts a and slope b value.
# You can fit in the equation, SBP = HbA1c(1.3169) + 122.9670
# So if a person with HbA1c value of 10, the systolic blood pressure is = 1.3169(10) + 122.9670 = 136.136 mmHg
# Now interpret the p value for each coefficients 
# For HbA1c which is 0.00021, the null null hypothesis is that b = 0 which mean that the population mean of Y is a for every x value, which tells us that x has no effect on Y. 
# The p value of F-statistic tells whether your model provide better fit than a model with no predictors. Here the p value is 0.0002095 signficant which mean it is better with a predictor.
# If your model is significant you can measure the strength of the relationship using adjusted R-squared
# The R squared here 8.1% of the SBP variation can be explained by HBA1C

#-----------------------------------------------------------------------------------------------------------------------------

# You can also apply categorical variable but this is essentially t-test for the hypothesis of the slope not equal to 0
# But in linear regression you can add / control for other variable 
lmsbp2 <- lm(sbp~sex,data=data)
summary(lmsbp2)
t.test(sbp ~ sex, data=data)

#-----------------------------------------------------------------------------------------------------------------------------
  
# Multiple linear regression
# When a regression takes into account 2 or more predictors, its called multiple linear regression.
# Yesterday we have seen that exercise is also significantly associated with SBP when conducting anova
# We can also add exercise in our model 
# For example adding exercise in the model
lmsbp3 <- lm(sbp~hba1c+exercise,data=data)
summary(lmsbp3)

# The exercise is a categorical predictor is treated as dummy variable, the reference here is high exercise, if one reduce from high to moderate, the SBP increase by 5.489 mmHg on average.
# The model omnibus p value is significant and the adjusted R-squared is better 0.1579, thus our model has become better. 
# We can recall the reference category being used by using contrasts command.
# We can change the reference category using relevel function.
contrasts(exercise)
data <- within(data, 
               exercise <- relevel(exercise, ref = "Low"))
contrasts(data$exercise)
# Now i would like undo that.
data <- within(data, 
               exercise <- relevel(exercise, ref = "High"))
# For exercise since it has more than 2 level, we can get the p value for overall exercise variable in which essentially an anova test
Anova(lmsbp3)
# Since it is significant 0.0005615, we can then further describe which pair are significant in which both are significant
summary(lmsbp3)
# We can also derived the 95% Confidence interval for the estimate using confint command
confint(lmsbp3)

#-----------------------------------------------------------------------------------------------------------------------------

# Identifying confounder
# This also suggests a useful way of identifying confounding. 
# Typically, we try to establish the association between a primary risk factor and a given outcome after adjusting for one or more other risk factors.
# One useful strategy is to use multiple regression models to examine the association between the primary risk factor and the outcome before and after including possible confounding factors.
# If the inclusion of a possible confounding variable in the model causes the association between the primary risk factor and the outcome to change by 10% or more
# Then the additional variable is a confounder.
# In first model, for every HbA1c 1 unit increase cause average increase of SBP by 1.3169
# When we add exercise,for every HbA1C 1 unit increase cause average increase of SBP by 1.138
# The changes was 0.1789/1.138 = 15.72%
# So increase by more than 10% so it is a confounder, so we can controlled by using multiple linear regression
# We can also include confounder variable from literature review

#-----------------------------------------------------------------------------------------------------------------------------

# We can build better model by including confounder in our model
# We can also building better model from a selection of variables using statistics
# Stepwise Regression
lmsbp4 <- lm(sbp~hba1c+sex+exercise+smoking,data=data)
step <- stepAIC(lmsbp4, direction="both")
step$anova # display results
# Using AIC as the parameter, the model with smaller AIC is better

#-----------------------------------------------------------------------------------------------------------------------------

# Linear regression makes few assumptions about the data 
# 1.Linearity of the data. 2. Normality of residuals 3. Homogeneity of residuals variance 4. Independence of residuals error terms

# 1. The first one is the linearity of the data between predictor x and outcome y
# This can be checked by inspecting Residuals vs Fitted 
# Ideally, the residual plot will show no fitted pattern and the redline should be approximately horizontal at zero.
plot(lmsbp, 1)
# Note that, if the residual plot indicates a non-linear relationship in the data
# Then a simple approach is to use non-linear transformations of the predictors, such as log(x), sqrt(x) and x^2, in the regression model.

# 2. The normality of residuals must be normally distributed. This can be plot using Q-Q Plot.
# It’s good if residuals points follow the straight dashed line.
plot(lmsbp, 2)

# 3. The scale location is used to check the homogeneity of variance of the residuals (homoscedasticity).
# Horizontal line with equally spread points is a good indication of homoscedasticity. 
plot(lmsbp, 3)
# A possible solution to reduce the heteroscedasticity problem is to use a log or square root transformation of the outcome variable (y).
# lmsbplog <- lm(log(sbp) ~ hba1c, data = data)
# plot(lmsbplog, 3)

# 4. No outliers 
# The Residuals vs Leverage plot can help us to find influential observations if any. On this plot, outlying values are generally located at the upper right corner or at the lower right corner. 
# Those spots are the places where data points can be influential against a regression line.
# An outlier is a point that has an extreme outcome variable value.
# Observations whose standardized residuals are greater than 3 in absolute value are possible outliers (James et al. 2014).
# Here we don't have . If we have, we might want to remove that outliers 
plot(lmsbp, 5)

#-----------------------------------------------------------------------------------------------------------------------------

#How to summarize findings using apaTable
apa.reg.table(lmsbp3, filename = "Table1_APA_MLR.doc", table.number = 1) 

#How to summarize findings using apaTable
explanatory = c("hba1c", "exercise","sex","smoking")
explanatory_multi = c("hba1c", "exercise","sex","smoking")
dependent = 'sbp'
data %>% finalfit(dependent, explanatory, explanatory_multi)

