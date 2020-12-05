setwd("~/Desktop/Medical Statistic/IIUM Biostatistic Workshop/IIUM_Biostatistic_workshop")
data <- read.spss(file="healthstatus.sav", to.data.frame=TRUE)
attach(data)
str(data)
data$bmi <- (data$wt / ((data$ht)/100)^2)
hist(age)

## Simple and Multiple Linear regression
#Previously we have seen that there is a significant linear relationship between systolic blood pressure and HbA1C  
attach(data)
plot(hba1c, sbp, main="HbA1c versus Systolic blood pressure", xlab="HBA1C ", ylab="Systolic blood pressure ", pch=19)
abline(lm(sbp~hba1c), col="red") # regression line (y~x) 
cor.test(hba1c, sbp, method=c("pearson")) #correlation for normally distributed data

# Now we can also observe the relationship between systolic blood pressure and HbA1c using simple linear regression
# Simple linear regression also can predict
# This relationship between 2 variable is stil bivariate analysis
# But this relationship can be represented by this equation SBP = HBA1c * b1 + a
# In this formula, a is the intercept and b is the slope, hence 1 unit increase in HBA1c,the SBP will increase by "b"

# A linear regression can be calculated in R with the command lm which stands for linear model
# You can get the summary of the model using summary command
lmsbp <- lm(sbp~hba1c,data=data)
summary(lmsbp)
lmsbp1 <- lm(sbp~sex,data=data)
summary(lmsbp1)
t.test(sbp ~ sex, data=data)

# You will get the a and b, a is 116.534 and b is 1.052
# The coefficients show the value for intercepts a and slope b value.
# So if a person with HbA1c value of 10, the systolic blood pressure is = 1.05(10) + 116.5 = 127mmHg
# Interpret the p value for each coffecients of HbA1c which is 0.0263, the null null hypothesis is that b = 0 which mean that the population mean of Y is a for every x value, which tells us that x has no effect on Y. 
# The p value of F-statistic tells whether your model provide better fit than a model with no predictors. Here the p value is 0.026 signficant which mean it is better with a predictor.
# If your model is significant you can measure the strength of the relationship using adjusted R-squared, here 2.6% of SBP variation can be explained by HBA1C

## Multiple linear regression
# When a regression takes into account 2 or more predictors, its called multiple linear regression.
# Yesterday we have seen that exercise is also significantly associated with SBP
# We can also add exercise in our model 
# For example adding exercise in the model
lmsbp1 <- lm(sbp~hba1c + exercise,data=data)
summary(lmsbp1)

# The exercise is a categorical predictor, the reference here is low exercise, if on increase from low to moderate, the SBP reduce by 5.7mmHg on average.
# The model omnibus p value is significant and the adjusted R-squared is better 0.07889. our model has become better. 
# We can recall the reference category being used by using contrasts command.
# We can change the reference category using relevel function.
contrasts(exercise)
data <- within(data, 
               exercise <- relevel(exercise, ref = "High"))
contrasts(data$exercise)
# Now i would like undo that.
data <- within(data, 
               exercise <- relevel(exercise, ref = "Low"))
# For exercise since it has more than 2 level, we can get the p value for overall exercise variable in which essentially an anova test
Anova(lmsbp1)
# Since it is significant, we can then further describe which pair are significant in which both are significant
lmsbp1 <- lm(sbp~hba1c + exercise,data=data)
summary(lmsbp1)
# We can also derived the 95% Confidence interval for the estimate using confint command
confint(lmsbp1)

# Linear regression makes few assumptions about the data 1.Linearity of the data. 2. Normality of residuals 3. Homogeneity of residuals variance 4. Independence of residuals error terms
# 1. The first one is the linearity of the data between predictor x and outcome y
# This can be checked by inspecting Residuals vs Fitted 
# Ideally, the residual plot will show no fitted pattern and the redline should be approximately horizontal at zero.
plot(lmsbp1, 1)
# 2. The normality of residuals must be normally distributed. This can be plot using Q-Q Plot. It’s good if residuals points follow the straight dashed line.
plot(lmsbp1, 2)
# 3. The scale location is used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal line with equally spread points is a good indication of homoscedasticity. 
plot(lmsbp1, 3)
# 4. The independence of residual error terms. Used to identify influential cases, that is extreme values that might influence the regression results when included or excluded from the analysis.
# The Residuals vs Leverage plot can help us to find influential observations if any. On this plot, outlying values are generally located at the upper right corner or at the lower right corner. Those spots are the places where data points can be influential against a regression line.
plot(lmsbp1, 5)

#How to summarize findings using apatable


