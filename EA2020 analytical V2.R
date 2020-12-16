#=======================
#Analytical statistics
#R Biostat Workshop IIUM
#Edre MA, DrPH
#=======================

#objective 3: To determine the factors contributing to hypertension
#we want to know first what contributes to systolic hypertension

----------------------------------------
#Comparing numerical values: parametric
----------------------------------------

library(readr)
healthstat <- read_csv("healthstatus6.csv") #load the file and make as object
View(healthstat) 

#if our IV is categorical with 2 categories
#example, we want to know if being male has any relationship with sbp
#independent sample t test

install.packages("car") #testing for homogeneity of variance
library(car)
leveneTest(sbp ~ sex, data = healthstat, center=mean)
t.test(sbp ~ sex, data = healthstat)

library(ggpubr)
ggboxplot(healthstat, x = "sex", y = "sbp",
          color = "sex", 
          palette = "jco", 
          add = "mean") + 
          stat_compare_means(method = "t.test")
      
 
#one way ANOVA

library(psych)
describe.by(healthstat$sbp, healthstat$exercise)
one.way =aov(sbp ~ exercise, data = healthstat)
summary(one.way)

ggboxplot(healthstat, x = "exercise", y = "sbp",
          color = "exercise", 
          palette = "jco", 
          add = "jitter") + 
  stat_compare_means(method = "anova")


leveneTest(sbp ~ exercise, data = healthstat, center=mean)
tukey.one.way<-TukeyHSD(one.way) #assuming equal variance
tukey.one.way

#paired t test

t.test(healthstat$wt, healthstat$wt2, paired=TRUE)

#pearson correlation coefficient test

cor.test(healthstat$hba1c,healthstat$sbp, method="pearson")
ggscatter(healthstat, x = "hba1c", y = "sbp", 
          add = "reg.line", 
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.method = "pearson", 
          xlab = "Age (years)", ylab = "SBP (mmHg)")

#------------------------------------------
#comparing numerical values: non-parametric
#------------------------------------------

#try transforming data into normal distribution by ln
healthstat$lnhcy= log(healthstat$hcy)
ln

hist(healthstat$lnhcy)
ggdensity(healthstat$lnhcy, fill = "lightgray")

#mann whitney U test

wilcox.test(hcy~sex, data=healthstat)

#kruskal wallis test

kruskal.test(hcy ~ exercise, data = healthstat) #if significant, proceed with pairwise comparison
pairwise.wilcox.test(healthstat$hcy, healthstat$exercise,p.adjust.method = "BH")


#wilcoxon signed rank test

wilcox.test(healthstat$wt,healthstat$wt2,paired=TRUE)

#spearman correlation coefficient test

cor.test(healthstat$hba1c,healthstat$sbp, method="spearman")

#----------------------------
#comparing categorical variables
#----------------------------

#chi square test

chisq.test(healthstat$sex,healthstat$smoking,correct=F)

#fisher's exact test (used when more than 20% celss with expected count less than 5)

chisq.test(healthstat$exercise,healthstat$smoking)$expected
fisher.test(healthstat$exercise,healthstat$smoking)

---------------------------------------
#reporting your findings in table form
---------------------------------------
#package needed
#"sjPlot"
#"apaTables"
library(sjPlot)
library(apaTables)
library(stargazer)

#table created in word file in your directory!
sjt.xtab(healthstat$sex, healthstat$smoking, file = "sjt_contingency.doc")
apa.aov.table(one.way, filename="Table_anova.doc", table.number = 2)

