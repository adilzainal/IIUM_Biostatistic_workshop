# Package required
install.packages("GGally")
library(GGally)

#-----------------------------------------------------------------------------------------------------------------------------

## MANOVA
# In the situation where we have multiple response variables, we can test them simultaneously using MANOVA
# We want to know whether exercise affect both the sbp and dbp. MANOVA can test this hypothesis
# Instead of conducting 2 anova seperately, we can conduct manova to see the effect of a factor (exercise) on the linear combination of the outcome

#-----------------------------------------------------------------------------------------------------------------------------

# We visualize our data first
data$exercise <- factor(data$exercise,levels = c("Low", "Moderate", "High"))
ggboxplot(data, x = "exercise", y = c("sbp", "dbp"), 
          merge = TRUE, palette = "jco")

# We describe our data 
data %>%
  group_by(exercise) %>%
  get_summary_stats(sbp, dbp, type = "mean_sd")

#-----------------------------------------------------------------------------------------------------------------------------

# Compute manova , we use pillai trace as the multivariate statistic as it is recommended. There are 3 other test.
model <- lm(cbind(sbp, dbp) ~ exercise, data)
Manova(model, test.statistic = "Pillai")
summary.aov(model)
# There was statistically significant difference between the exercise on the linear combination of sbp and dbp
# Seperate anova also show significant different between exercise on sbp and dbp respectively

# We then now run multiple pairwise comparisons for each outcome
grouped.data <- data %>%
  gather(key = "variables", value = "value", sbp, dbp) %>%
  group_by(variables)

pwc <- data %>%
  gather(key = "variables", value = "value", sbp, dbp) %>%
  group_by(variables) %>%
  games_howell_test(value ~ exercise) 
pwc

# We can conclude that there is significant different between low and high for dbp and between low and moderate with high for sbp

#-----------------------------------------------------------------------------------------------------------------------------

# Assumptions for MANOVA are
# 1. Adequate sample size , the rule of thumb is the n in each cell > the number of outcome variables
data %>%
  group_by(exercise) %>% 
  summarise(N=n())

# 2. independence of variables, the selection of the sample should be completely random

# 3. Absence of univariate or multivariate outliers
data %>%  # univariate outliers
  group_by(exercise) %>%
  identify_outliers(sbp)
data %>% # univariate outliers
  group_by(exercise) %>%
  identify_outliers(dbp)
data %>% # multivariate outliers using mahalanobis distance
  group_by(exercise) %>%
  mahalanobis_distance(-id) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

# 4. Multivariate normality using mshapiro_Test() from rstatix package
data %>% group_by(exercise) %>% shapiro_test(sbp, dbp) %>% arrange(variable) # univariate
data %>% select(sbp, dbp) %>% mshapiro_test() # multivariate

# 5. Absence of multicollinearity between the dependent outcomes. The correlation should not be above 0.9
data %>% cor_test(sbp, dbp)

# 6. Linearity between all outcome variables for each group
results <- data %>%
  select(sbp, dbp, exercise) %>%
  group_by(exercise) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results
results$plots

# 7. Homogeneity of variances using Levene's test
data %>% 
  gather(key = "variable", value = "value", sbp, dbp) %>%
  group_by(variable) %>%
  levene_test(value ~ exercise)

# 8. Homogeneity of variance-covariance matrices using Box's M Test, 
box_m(data[, c("sbp", "dbp")], data$exercise)
# p value is significant so it has violate the assumption
# Note that, if you have balanced design (i.e., groups with similar sizes),
# you don’t need to worry too much about violation of the homogeneity of variances-covariance matrices 
# and you can continue your analysis.
# However, having an unbalanced design is problematic. 
# Possible solutions include: 1) transforming the dependent variables; 
# 2) running the test anyway, but using Pillai’s multivariate statistic instead of Wilks’ statistic.

# Although we have many assumptions, the most important assumptions are .....

#-----------------------------------------------------------------------------------------------------------------------------

# Reporting of MANOVA result using APATable



