## Repeated measure ANOVA

library(tidyverse)
library(ggpubr)
library(rstatix)

# The repeated measure ANOVA is used to analyze when we measure more than once.
# One way repeated measure ANOVA is essentially an extension of paired sample t-test to compare means of 3 or more level of within subject variable
# A 2 way ANOVA is to evaluate simultaneously effect of 2 within subject factors on a continous outcome

# Let us first create a random variable for SBP at time 2 and time 3
data$sbp1 <- rnorm(153, mean=121, sd=10)
data$sbp2 <- rnorm(153, mean=118, sd=12.1)

# We change the data to long format first
# Gather columns t1, t2 and t3 into long format
# Convert id and time into factor variables
data1 <- data1 %>%
  gather(key = "time", value = "sbpmeasure", sbp, sbp1, sbp2) %>%
  convert_as_factor(id, time)
head(data1, 3)

# Get the summary statistics
data1 %>%
  group_by(time) %>%
  get_summary_stats(sbpmeasure, type = "mean_sd")

# Visualise the differences
bxp <- ggboxplot(data1, x = "time", y = "sbpmeasure", add = "point")
bxp


# The assumption for RM-ANOVA are
# 1. No significant outliers using identify_outliers
data1 %>%
  group_by(time) %>%
  identify_outliers(sbpmeasure)
# 2. Normality of the outcome using shapiro-wilk normality test or visual qq plot
data1 %>% # statistically
  group_by(time) %>%
  shapiro_test(sbpmeasure)
ggqqplot(data1, "sbpscore", facet.by = "time") # visually
# 3. Assumption of spherecity. Sphericity is the variance of the difference between group should be equal using Mauchly test
res.aov <- anova_test(data = data1, dv = sbpscore, wid = id, within = time)
get_anova_table(res.aov)
# We will get the Greenhouse-Geisser spherecity correction automatically applied to factors violating the spherecity assumption
# The ges is the generalized effect size (amount of variability due to within subject factor)
# We can then run posthoc test
# pairwise comparisons
pwc <- data1 %>%
  pairwise_t_test(sbpmeasure ~ time, paired = TRUE,p.adjust.method = "bonferroni")
pwc






















