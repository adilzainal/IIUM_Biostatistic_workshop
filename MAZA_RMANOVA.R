# Package required 
install.packages("ez")
library(ez)
library(tidyverse)

#-----------------------------------------------------------------------------------------------------------------------------

## Repeated measure ANOVA
# The repeated measure ANOVA is used to analyze when we measure more than once.
# One way repeated measure ANOVA is essentially an extension of paired sample t-test to compare means of 3 or more level of within subject variable
# A 2 way ANOVA is to evaluate simultaneously effect of 2 within subject factors on a continous outcome

#-----------------------------------------------------------------------------------------------------------------------------

# We change the data to long format first
# Gather columns t1, t2 and t3 into long format
# Convert id and time into factor variables
data1 <- data %>%
  gather(key = "time", value = "sbpmeasure", sbp, sbp2, sbp3) %>%
  convert_as_factor(id, time)
attach(data1)

#-----------------------------------------------------------------------------------------------------------------------------

# Get the summary statistics
data1 %>%
  group_by(time) %>%
  get_summary_stats(sbpmeasure, type = "mean_sd")

# Visualise the differences
bxp <- ggboxplot(data1, x = "time", y = "sbpmeasure", add = "point")
bxp

#-----------------------------------------------------------------------------------------------------------------------------

# The assumption for RM-ANOVA are
# 1. No significant outliers using identify_outliers
data1 %>%
  group_by(time) %>%
  identify_outliers(sbpmeasure)

# 2. Normality of the outcome using shapiro-wilk normality test or visual qq plot
data1 %>% # statistically
  group_by(time) %>%
  shapiro_test(sbpmeasure)
ggqqplot(data1, "sbpmeasure", facet.by = "time") # visually

# 3. Assumption of spherecity. Sphericity is the variance of the difference between group should be equal using Mauchly test
res.aov <- anova_test(data = data1, dv = sbpmeasure, wid = id, within = time)
get_anova_table(res.aov)
# We will get the Greenhouse-Geisser spherecity correction automatically applied to factors violating the spherecity assumption
# The ges is the generalized effect size (amount of variability due to within subject factor)

# We can then run posthoc test
# pairwise comparisons
pwc <- data1 %>%
  pairwise_t_test(sbpmeasure ~ time, paired = TRUE,p.adjust.method = "bonferroni")
pwc

# Interpretation
# The sbp was statistically significantly different at the different time points, F(1.45, 219.87) = 164.356, p < 0.01, generalized eta squared = 0.006.
# Post-hoc analyses with a Bonferroni adjustment revealed that all the pairwise differences, between time points, were statistically significantly different (p <= 0.05).

#-----------------------------------------------------------------------------------------------------------------------------

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#-----------------------------------------------------------------------------------------------------------------------------

# Include exercise variables in 2 way RM-Anova

# summary statistics
data1 %>%
  group_by(exercise, time) %>%
  get_summary_stats(sbpmeasure, type = "mean_sd")

# visualize 
bxp <- ggboxplot(
  data1, x = "time", y = "sbpmeasure",
  color = "exercise", palette = "jco"
)
bxp

#assumption
data1 %>%
  group_by(exercise, time) %>%
  identify_outliers(sbpmeasure)

data1 %>%
  group_by(exercise, time) %>%
  shapiro_test(sbpmeasure)

ggqqplot(data1, "sbpmeasure", ggtheme = theme_bw()) +
  facet_grid(time ~ exercise, labeller = "label_both")

# 2 way RM-ANOVA
twrmanova <- anova_test(dv = sbpmeasure, between = c(exercise, time), data = data1)
twrmanova

# There is significant different of sbp between time, p <0.01
# There is significant different of sbp between exercise, p<0.01
# There is a statistically significant two-way interactions between exercise and time, p < 0.01.

interaction.plot(data1$time, factor(data1$exercise),
                 data1$sbpmeasure, fun = mean, type="b", pch = c(2,4,6),
                 legend = "F", 
                 col = c(3,4,6), ylab = "SBP", xlab = "Time",
                 legend(4, 300, c("Low", "Moderate", "High"), col = c(4,6,3),
                        text.col = "green4", lty = c(2, 1, 3), pch = c(4, 6, 2),
                        merge = TRUE, bg = 'gray90')
)

# Post hoc

# Effect of exercise at each time point which is significant at all time point
one.way <- data1 %>%
  group_by(time) %>%
  anova_test(dv = sbpmeasure, between = exercise) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between exercise groups, show that significant different between high with low and moderate at all time 
pwc <- data1 %>%
  group_by(time) %>%
  tukey_hsd(sbpmeasure ~ exercise,
    p.adjust.method = "bonferroni"
  )
pwc

#-----------------------------------------------------------------------------------------------------------------------------


                 
