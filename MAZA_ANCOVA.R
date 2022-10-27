# Package required
library(ggpubr)
library(emmeans)
library(rstatix)
require(devtools)
install_version("emmeans",version="1.5.2-1",repos="http://cran.us.r-project.org")
#-----------------------------------------------------------------------------------------------------------------------------

## ANCOVA 
# ANCOVA is used to compare means of the outcome between a group (which have 2 or more category) taking into account of other continous variable (covariate)
# One-way ANCOVA is just an extension of one-way ANOVA but adding a covariate
# Let say we want to know whether is there significant different of systolic blood pressure between exercise level taking into account HbA1c level
# The different between ANCOVA and regression is that regression is more advanced analysis when you want to do prediction
# Whereas ancova is when the focus is to compare the effect of categorical iv on  the dependent variable groups

#-----------------------------------------------------------------------------------------------------------------------------
# Run ANCOVA main effect
res.aov <- data %>%
  anova_test(sbp ~ exercise + hba1c)
get_anova_table(res.aov)

# Run post hoc test pairwise comparisons
pwc <- data %>% 
  emmeans_test(
    sbp ~ exercise, covariate = hba1c,
    p.adjust.method = "bonferroni")
pwc

# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc)

# An ANCOVA was run to determine the effect of exercises on the sbp  after controlling for HBA1C of participants.
# Even after control for HbA1C, the benefit of exercise is still similar where at least moderate exercise intensity. 
# After adjustment for HBA1c, there was a statistically significant difference in sbp between the exercise level, F(2, 41) = 218.63, p < 0.0001.
# Post hoc analysis was performed with a Bonferroni adjustment. 
# The mean sbp  was statistically significantly greater in high (126) compared to the Moderate (132) and low (134)

#-----------------------------------------------------------------------------------------------------------------------------

# Visualization: line plots with p-values
pwc <- pwc %>% add_xy_position(x = "exercise", fun = "mean_se")
ggline(get_emmeans(pwc), x = "exercise", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs(subtitle = get_test_label(res.aov, detailed = TRUE),
       caption = get_pwc_label(pwc))

#-----------------------------------------------------------------------------------------------------------------------------

# We then checking the assumption of ANCOVA
# 1. Linearity between covariate and the outcome variable. We can see there is a linear relationship between SBP and HbA1C.
ggscatter(data, x = "hba1c", y = "sbp",
          color = "exercise", add = "reg.line")+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = exercise))

# 2. Homogeneity of regression slopes. The slopes of the regression lines formed by the covariate and the outcome variable should be the same for each group.
# It should be parallel and assumed there is no interaction between outcome and the covariate.
data %>% anova_test(sbp ~ exercise*hba1c)

# 3. The outcome variable should be normally distributed by checking using Shapiro Wilk test of normality on the test residuals
# Fit the model, the covariate goes first
model <- lm(sbp ~ hba1c + exercise, data = data)
summary(model)
# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted, -.se.fit) # Remove details
head(model.metrics, 3)

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)

# 4. Homoscedasticity or homogeneity of residuals variance for all groups.
model.metrics %>% levene_test(.resid ~ exercise)

# 5. No significant outliers in the groups.
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

#-----------------------------------------------------------------------------------------------------------------------------
wanova5 <- aov(sbp ~ exercise + hba1c, data = data)
summary(wanova5)
# Reporting the result
apa.aov.table(wanova5, filename = "Table5_APA.doc", table.number = 5)




