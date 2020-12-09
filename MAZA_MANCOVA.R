# Package required
install.packages("jmv")
library(jmv)

#-----------------------------------------------------------------------------------------------------------------------------

## MANCOVA
# We can now also extend our MANOVA to control for a covariate

# Just now we have see that .......
# It is basically running ancova on 2 linear combination of the outcome instead of run 2 seperate ancova.
mancova(data = data,
        deps = vars(sbp, dbp),
        factors = exercise,
        cov = hba1c)

# Pillai trace is recommended
# There is significant different effect of different exercise level on the linear combination of SBP and DBP (p<0.01) after controlled for HBA1C
# On univariate analysis, the different is both on sbp and dbp( p<0.05)
# Then conduct posthoc test, by conducting seperate ancova

res.aov <- data %>%
  anova_test(sbp ~ hba1c + exercise)
get_anova_table(res.aov)

pwc <- data %>% 
  emmeans_test(
    sbp ~ exercise, covariate = hba1c,
    p.adjust.method = "bonferroni")
pwc

res.aov <- data %>%
  anova_test(dbp ~ hba1c + exercise)
get_anova_table(res.aov)

pwc <- data %>% 
  emmeans_test(
    dbp ~ exercise, covariate = hba1c,
    p.adjust.method = "bonferroni")
pwc

#-----------------------------------------------------------------------------------------------------------------------------

# The assumptions is as in MANOVA and ANCOVA  

#-----------------------------------------------------------------------------------------------------------------------------

# Reporting of MANCOVA Result
