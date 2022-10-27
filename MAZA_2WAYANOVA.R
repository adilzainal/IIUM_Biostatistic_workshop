## Package requireMethods
library(psych)
library(car)
library(dplyr)
#-------------------------------------------------------------------------------------------------------------------

## 2 Way ANOVA

# We have seen that there is signficant different in systolic blood pressure between the exercise group.
# We can also evaluate simultaneously the effect of 2 group variable (2way) on a response variable (sbp)
# Let say we want to know the effect of exercise on systolic blood pressure after control for sex
# The hypothesis for anova is to know whether is there any significant different of sbp value between exercise level.
# For 2 way anova, we want to know the effect of exercise and sex on sbp value

#-------------------------------------------------------------------------------------------------------------------

# We visualise first our group differences using ggpubr package and describe the data
data$exercise <- factor(data$exercise,levels = c("Low", "Moderate", "High"))
interaction.plot(x.factor = data$exercise, trace.factor = data$sex, 
                 response = data$sbp, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Exercise", ylab="SBP(mmHg)",
                 pch=c(2,19), col = c("#00AFBB", "#E7B800")) 

# Describe the data
describeBy(data$sbp,list(data$exercise,data$sex),mat=TRUE) # Method 1
#method 2 
group_by(data, exercise, sex) %>%
  summary(
    count = n(),
    mean = mean(sbp, na.rm = TRUE),
    sd = sd(sbp, na.rm = TRUE))
#-------------------------------------------------------------------------------------------------------------------

# We can then run the 2 way anova by using aov command and see the summary of the variance model using summary command
wanova <- aov(sbp ~ exercise + sex, data = data)
summary(wanova)

# From the anova table we can see that main effect of exercise is statistically significant whereas sex is not significant.
# Or there is still significant different of sbp between exercise level after control for sex
# When we have 2 category we also want to check for interaction if we suspect interaction looking at the graph

# We can also check the interaction effect by adding interaction term
wanova2 <- aov(sbp ~ exercise + sex + exercise:sex, data=data)
summary(wanova2, type=3)
# It can be seen that the interaction hypothesis is not significant, thus the relationship of exercise and sbp does not depend on sex

# Since the main effect of exercise is significant we need to do post hoc test to see which pair is significant
TukeyHSD(wanova, which = "exercise")

# There is significant different of sbp between high with moderate and low after control for sex
# If significant interaction, the relatioship between exercise and sbp depends on gender

#-------------------------------------------------------------------------------------------------------------------

# We need to check the assumptions for the anova
# 1. Homogeneity of variance
plot(wanova2, 1)
leveneTest(sbp ~ exercise*sex, data = data)
# The levene test is significant, thus the variance across group is significantly different

# 2. Normality assumption by plotting the residuals, as all the points fall approximately along this reference line, we can assume normality.
plot(wanova2, 2)
aov_residuals <- residuals(object = wanova2)
shapiro.test(x=aov_residuals)
# We can support this by doing the Shapiro Wilk test on the ANOVA Residuals

# Since there is no interaction, so the final model should use type 2 anova.
# If the interaction is significant, we should specify the type 3 anova, such as below.
wanova3 <- aov(sbp ~ exercise + sex + exercise*sex, data = data)
Anova(wanova3, type=3)

# If the interaction is significant, present the result for different level of the effect modifier
emwanova3 <- emmeans(wanova3, ~ exercise*sex)
emwanova3

# criteria for confounder - 1.assoc with outcome 2.assoc with disease and unequal distribution 3.not in pathway
# confounder distort relationship so need to adjust the main effect
# criteria for effect modifier - 1.assoc with outcome 2.not assoc with factor
# effect of main variable differ according to different level of effect modifier
#-------------------------------------------------------------------------------------------------------------------

# Report writing using APA Table
library(apaTables)
apa.aov.table(wanova2, filename = "Table3_APA_2Wayanova.doc", table.number = 3)



