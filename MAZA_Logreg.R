## Simple and multiple logistic regression
install.packages("aod")
library(aod)
# Make hypertension or composite variable
sbpc <- cut(sbp , breaks = c(-Inf,129.9,Inf), labels = c("1","2"))
sbpc <-as.numeric(sbpc)
dbpc <- cut(dbp , breaks = c(-Inf,89.9,Inf), labels = c("1","2"))
dbpc <-as.numeric(dbpc)
bpc <- dbpc + sbpc
hpt <- cut(bpc, breaks = c(-Inf,2,Inf), labels = c("normal","hpt"))
hpt <- as.factor(hpt)
summary(hpt)

# Now logistic regression is the appropriate regression analysis when the dependent is dichotomous or binary
# It is useful when we want to calculate the probability.
# In r we will use glm function which is generally used to fit generalized linear models to fit the logistic regression model.
# We need to make it clear that you want to fit a logistic regression model. We resolve this by setting the family argument to binomial. This way, you tell glm() to put fit a logistic regression model instead of one of the many other models that can be fit to the glm.
contrasts(hpt)
logmodel <- glm(hpt ~ exercise, data=data, family=binomial)
summary(logmodel)

# The coefficient in logistic regression is in the log odds of the Hpt for a unit increase in predictor exercise
# Having moderate exercise compareto low exercise, change the log odds of hypertension by -1.8801
# We can test the overall effect of exercise using the wald.test in the aod library
wald.test(b = coef(logmodel), Sigma = vcov(logmodel), Terms = 2:3)
# The p value for the ovrall variable exercise is significant a p<0.05

# We then need to exponentiating the coefficient from log odds into odds 
exp(coef(logmodel))
# We also can get the confidence interval using the confint command
exp(confint(logmodel))
# Now we can interpret that high exercise have 0.15 lower odds of having hypertension with the 95% CI (0.07,0.33)

# We can now see the model fit, first by the significance of the overall model. This test whether our model with predictors is better than a model with just an intercept( the null model)
with(logmodel, null.deviance - deviance) # To get the test statistic
with(logmodel, df.null - df.residual) # To get the degree of freedom
with(logmodel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) # To get the p value
# Our model is as a whole fits significantly better than an empty model
mlogmodel <- glm(hpt ~ exercise + hba1c, data=data, family=binomial)
summary(mlogmodel)
wald.test(b = coef(mlogmodel), Sigma = vcov(mlogmodel), Terms = 2:3)
exp(coef(mlogmodel))
exp(confint(mlogmodel))
with(mlogmodel, null.deviance - deviance) 
with(mlogmodel, df.null - df.residual) 
with(mlogmodel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) # To get the p value
# After controlled for HbA1c, high exercise have 0.16 lower odds of having hypertension. This is also called the adjusted odd ratios
