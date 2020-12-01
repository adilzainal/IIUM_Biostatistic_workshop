## 2 Way ANOVA

# We have seen that there is signficant differnet in systolic blood pressure between the exercise group.
# We can also evaluate simultaneously the effect of 2 group variable (2way) on a response variable (sbp)
# Let say we want to know the effect of sex and exercise on systolic blood pressure

# We visualise first our group differences using ggpubr package
install.packages("ggpubr")
library(ggpubr)
ggboxplot(data, x = "exercise", y = "sbp", color = "sex",palette = c("#00AFBB", "#E7B800"))
contrasts(exercise)
interaction.plot(x.factor = data$exercise, trace.factor = data$sex, 
                 response = data$sbp, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Exercise", ylab="SBP",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

# We can then run the 2 way anova by using aov command and see the summary of the variance model using summary command
wanova <- aov(sbp ~ exercise + sex, data = data)
summary(wanova)
# From the anova table we can see that exercise is statistically significant whereas sex is not significant.
# This indicate that exercise levels are associated with different SBP and sex does not associated with different SBP

# We can also check the interaction effect by adding interaction term
wanova2 <- aov(sbp ~ exercise + sex + exercise:sex, data=data)
summary(wanova2)
# It can be seen that the interaction is not significant, thus the relationship of exercise and sbp does not depend on sex
library(dplyr)
group_by(data, exercise, sex) %>%
  summarise(
    count = n(),
    mean = mean(sbp, na.rm = TRUE),
    sd = sd(sbp, na.rm = TRUE))

# Since the main effect of exercise is significant we need to do post hoc test to see which pair is significant
TukeyHSD(wanova2, which = "exercise")

# We need to check the assumptions for the anova
# 1. Homogeneity of variance
plot(wanova2, 1)
leveneTest(sbp ~ exercise*sex, data = data)
# The levene test is significant, thus the variance across group is significantly different
# 2. Normality assumpton by plotting the residuals, as all the points fall approximately along this reference line, we can assume normality.
plot(wanova2, 2)
aov_residuals <- residuals(object = wanova2)
shapiro.test(x=aov_residuals)
# We can support this by doing the Shapiro Wilk test on the ANOVA Residuals
# 2 Way ANOVA Test for unbalanced designs, basically the above test is for balanced design, if we have unbalanced design we should use the following test.
wanova3 <- aov(sbp ~ exercise*sex, data = data)
Anova(wanova3, type ="III")
