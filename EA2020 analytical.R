#=======================
#Analytical statistics
#R Biostat Workshop IIUM
#Edre MA, DrPH
#=======================

---------------------------------------
#Comparing numerical values: parametric
---------------------------------------

#independent sample t test
  
#one way ANOVA

one.way =aov(sbp ~ exercise, data = healthstat)
summary(one.way)

tukey.one.way<-TukeyHSD(one.way) #assuming equal variance
tukey.one.way

#paired t test
#pearson correlation coefficient test
  
#------------------------------------------
#comparing numerical values: non-parametric
#------------------------------------------
  
#mann whitney U test
#kruskal wallis test
#wilcoxon signed rank test
#spearman correlation coefficient test


#----------------------------
#comparing categorical values
#----------------------------

#chi square test
#fisher's exact test

--------------------------------------
#reporting your findings in table form
--------------------------------------
#package needed
install.packages("sjPlot")
install.packages("stargazer")
library(sjPlot)
library(stargazer)

# contingency table created in word file in your directory!
sjt.xtab(healthstat$sex, healthstat$smoking, file = "sjt_contingency.doc")