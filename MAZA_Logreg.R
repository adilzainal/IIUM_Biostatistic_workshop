# Required package
install.packages("aod")
install.packages("dplyr")
install.packages("ggplot")
install.packages("rstan")
install.packages("boot")
install.packages("RcmdrPlugin.EZR")
install.packages("stargazer")
install.packages("effects")
library(aod)
library(dplyr)
library(ggplot)
library(tidyverse)
library(broom)
library(car)
library(rstan)
library(boot)
library(knitr)
library(stargazer)
library(effects)

#-----------------------------------------------------------------------------------------------------------------------------

# Data preparation
# Make hypertension or composite variable
attach(data)
data$hpt<-(sbp>=140|dbp>=90)
data$hpt <- as.factor(data$hpt)
summary(data$hpt)

#-----------------------------------------------------------------------------------------------------------------------------

#Simple logistic regression
# Now logistic regression is the appropriate regression analysis when the dependent is dichotomous or binary 
# This is very useful because majority of our medical outcome is in binary such as mortality or hypertension
# It is useful when we want to calculate the probability.
# In r we will use glm function which is generally used to fit generalized linear models to fit the logistic regression model.
# We need to make it clear that you want to fit a logistic regression model. We resolve this by setting the family argument to binomial. This way, you tell glm() to put fit a logistic regression model instead of one of the many other models that can be fit to the glm.
contrasts(data$hpt)
contrasts(exercise)
# So we know the reference category is normotensive, so we want to predict or the odds of having hypertension
# So now we can run the simple logistic regression
logmodel <- glm(hpt ~ exercise, data=data, family=binomial)
summary(logmodel)

# The coefficient in logistic regression is in the log odds of the Hpt for a unit increase in predictor exercise
# Having high exercise compare to low exercise, change the log odds of hypertension by -2.9414
# We can test the overall effect of exercise using the wald.test in the aod library
wald.test(b = coef(logmodel), Sigma = vcov(logmodel), Terms = 2:3)
# The p value for the overall variable exercise is significant a p<0.05

# We then need to exponentiating the coefficient from log odds into odds 
exp(coef(logmodel))
# We also can get the confidence interval using the confint command
exp(confint(logmodel))
# Now we can interpret that high exercise have 0.05 lower odds of having hypertension with the 95% CI (0.002,0.277)

# We can now see the model fit, first by the significance of the overall model. This test whether our model with predictors is better than a model with just an intercept( the null model)
with(logmodel, null.deviance - deviance) # To get the test statistic
with(logmodel, df.null - df.residual) # To get the degree of freedom
with(logmodel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) # To get the p value
# Our model is as a whole fits significantly better than an empty model
summary(logmodel) # look for AIC for model fit, smaller AIC is better

dataA<-data[age<45,]
dataB<-data[age>=45,]
logmodelA <- glm(hpt ~ exercise, data=dataA, family=binomial)
#Predicting using logistic model
glm.probs = predict(logmodelA, newdata = dataB, type = "response")
dataB$pred_glm = ifelse(glm.probs > 0.5, "FALSE", "TRUE")
dataB$pred_glm = as.factor(dataB$pred_glm)
#Checking the accuracy of the logistic model
confusionMatrix(dataB$hpt,dataB$pred_glm)
# As you can see, we are able to obtain about 39.66% accuracy overall, 
# which isn’t very good.
# However, we have 36.36% prediction accuracy on ‘TRUE’ which is hypertensive
# Perhaps this means we can't trade with confidence when our model predicts hypertensive in the future. 

#-----------------------------------------------------------------------------------------------------------------------------

# Multiple logistic regression
mlogmodel <- glm(hpt ~ exercise + hba1c, data=data, family=binomial)
summary(mlogmodel)
wald.test(b = coef(mlogmodel), Sigma = vcov(mlogmodel), Terms = 2:3)
exp(coef(mlogmodel))
exp(confint(mlogmodel))

with(mlogmodel, null.deviance - deviance) 
with(mlogmodel, df.null - df.residual) 
with(mlogmodel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) # To get the p value

# After controlled for HbA1c, those who practice high exercise intensity have 0.06 lower odds compared to those in low exercise to get hypertension.
# This is also called the adjusted odd ratios.

probabilities <- predict(mlogmodel, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
head(predicted.classes)

#-----------------------------------------------------------------------------------------------------------------------------

# Logistic regression assumptions
# 1. Linearity assumption
# Here, we’ll check the linear relationship between continuous predictor variables and the logit of the outcome. 
# This can be done by visually inspecting the scatter plot between each predictor and the logit values.
# Select only numeric predictors
data1 <- data %>% dplyr::select_if(is.numeric)
predictors <- colnames(data1)
# Bind the logit and tidying the data for plot
data1 <- data1 %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
# Create scatter plots
ggplot(data1, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
# If is has linearity plot than it is ok
# If the scatter plot shows non-linearity, you need other methods to build the model 
# such as including 2 or 3-power terms, fractional polynomials and spline function 

# 2. No influential values
# Influential values are extreme individual data points that can alter the quality of the logistic regression model.
#  The most extreme values in the data can be examined by visualizing the Cook’s distance values.
# Here we label the top 3 largest values:
plot(mlogmodel, which = 4, id.n = 3)
# Note that, not all outliers are influential observations.
# To check whether the data contains potential influential observations, the standardized residual error can be inspected.
# Data points with an absolute standardized residuals above 3 represent possible outliers and may deserve closer attention.
# The following R code computes the standardized residuals (.std.resid) and the Cook’s distance (.cooksd) using the R function augment()
# Extract model results
model.data <- augment(mlogmodel) %>% 
  mutate(index = 1:n()) 
# The data for the top 3 largest values, according to the Cook’s distance, can be displayed as follow:
model.data %>% top_n(3, .cooksd)
# Plot the standardized residuals
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = hpt), alpha = .5) +
  theme_bw()
# Filter the infuential data points
model.data %>% 
  filter(abs(.std.resid) > 3)
# There is no influential data in our points
# When you have outliers in a continuous predictor, potential solutions include:
# Removing the concerned data
# Transform the data into log scale

# 3. Multicollinearity
mlogmodel2 <- glm(hpt ~ exercise + hba1c + age, data=data, family=binomial)
summary(mlogmodel2)
car::vif(mlogmodel2)
# As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity. 
# In our example, there is no collinearity: all variables have a value of VIF well below 5.

#-----------------------------------------------------------------------------------------------------------------------------

# Table final result using stargazer
stargazer(mlogmodel, type="text", out="models.txt")
exp(coef(mlogmodel))
stargazer(exp(coef(mlogmodel)), type="text", out="models.txt")




