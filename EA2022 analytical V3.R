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

install.packages('readr')
library(readr)
hstat <- read_csv("healthstatus6.csv")
View(hstat)

#if our IV is categorical with 2 categories
#example, we want to know if being male has any relationship with sbp
#independent sample t test

install.packages("car") #testing for homogeneity of variance
library(car)
leveneTest(sbp ~ sex, data = hstat, center=mean)
t.test(sbp ~ sex, data = hstat)

#we want to visualize the comparison

install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
ggboxplot(hstat, x = "sex", y = "sbp",
          color = "sex", 
          palette = "jco", 
          add = "mean") + 
          stat_compare_means(method = "t.test")
      
#now we know sex has no effect on sbp in our study
#we want to know now, does exercise have an effect (low,mod,high intensity)
#one way ANOVA

install.packages("psych")
library(psych)
describe.by(hstat$sbp, hstat$exercise)
one.way =aov(sbp ~ exercise, data = hstat)
summary(one.way)

ggboxplot(hstat, x = "exercise", y = "sbp",
          color = "exercise", 
          palette = "jco", 
          add = "jitter") + 
          stat_compare_means(method = "anova")

#yes, there is significant effect of exercise on sbp
#but, which pair comparison has most effect?

leveneTest(sbp ~ exercise, data = hstat, center=mean)

#significant p value, thus equal variance not assumed
#use Welch one way ANOVA

oneway.test(sbp ~ exercise, data = hstat) #for overall significance
pairwise.t.test(hstat$sbp, hstat$exercise,
                p.adjust.method = "BH", pool.sd = FALSE)

#if equal variance assumed, use Tukey
tukey.one.way<-TukeyHSD(one.way) 
tukey.one.way

#Exercise adds benefit in sbp reduction
#does it correlate with weight?

#pearson correlation coefficient test

cor.test(hstat$wt,hstat$sbp, method="pearson")
ggscatter(hstat, x = "wt", y = "sbp", 
          add = "reg.line", 
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.method = "pearson", 
          xlab = "Weight (kg)", ylab = "SBP (mmHg)")

#yes, the heavier the person, the higher the sbp
#now you conducted a high-intensity interval training (HIIT) intervention
#you want to measure pre and post HIIT effect on weight
#paired t test

t.test(hstat$wt, hstat$wt2, paired=TRUE)
ggscatter(hstat, x = "wt", y = "wt2", 
          add = "reg.line", 
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.method = "pearson", 
          xlab = "pre-HIIT weight (kg)", ylab = "post-HIIT weight (kg)") +
          grids(linetype = "solid")

#Now we know that sbp is affected by weight.
#exercise gives additional benefit to weight reduction
#you are now concerned with the exercise giving effect on cardiovascular health
#homocysteine (hcy) relates to cardiovascular heath from literature
#what are the factors contributing to hcy level?

#------------------------------------------
#comparing numerical values: non-parametric
#------------------------------------------

#try transforming data into normal distribution by ln
hstat$lnhcy= log(hstat$hcy)
hist(hstat$lnhcy)
ggdensity(hstat$lnhcy, fill = "lightgray")

#still not normally distributed
#need to do non-parametric test

#female has higher or lower hcy level compared to male?
#mann whitney U test

install.packages("SmartEDA")
library(SmartEDA)
ExpCustomStat(hstat,
              Cvar="sex",
              Nvar="hcy",
              stat=c("median","IQR"),
              gpby=TRUE,
              dcast=F)

wilcox.test(hcy~sex, data=hstat) #synonym to Mann Whitney U test
#sex has no signififant relationship with hcy

#how about exercise intensity?
#kruskal wallis test


ExpCustomStat(hstat,
              Cvar="exercise",
              Nvar="hcy",
              stat=c("median","IQR"),
              gpby=TRUE,
              dcast=F)

kruskal.test(hcy ~ exercise, data = hstat) #if significant, proceed with pairwise comparison
pairwise.wilcox.test(hstat$hcy, hstat$exercise,p.adjust.method = "BH")

#high intensity exercise significantly gives lower HCY compared to low/mod 
#you proceed in continuing the HIIT intervention as it gives benefit to both sbp and hcy

#measure effectiveness again on weight reduction
#wilcoxon signed rank test

wilcox.test(hstat$wt,hstat$wt2,paired=TRUE)
#yes, the HIIT is effective in weight reduction
#however you noticed some of your respondents are diabetic
#worry that your intervention gives more harm than good
#finding relationship between hba1c and both sbp/hcy
#spearman correlation coefficient test

cor.test(hstat$hba1c,hstat$sbp, method="spearman")
cor.test(hstat$hba1c,hstat$hcy, method="spearman")

#you conclude that only sbp has a significant correlation with hba1c
#in future, you would prioritize 
#giving HITT intervention to the diabetic & hypertensive patients

#now, you are focused back to your objective 3
#factors contributing to hypertension (hpt)

#----------------------------
#comparing categorical variables
#----------------------------

install.packages("dplyr")
library(dplyr)

hstat2<-hstat %>%
  mutate(hpt=if_else(hstat$sbp<140 & hstat$dbp<90,'normal','high')) 
View(hstat2)

#smoking has a relationship with hpt?
#chi square test

chisq.test(hstat2$hpt,hstat2$smoking,correct=F)
chisq.test(hstat2$hpt,hstat2$smoking)$observed
#yes, smoking is significantly related to hpt. More smokers are hypertensive

#how about BMI status and hpt?

hstat3<- hstat2 %>% 
  mutate(height_m = ht / 100,bmi = wt / (height_m^2))
View(hstat3)
hstat3$bmistatus<- cut(hstat3$bmi, 
                                 breaks=c(-Inf, 18.49999, 24.9999, 29.9999, Inf), 
                                 labels=c("underweight", "normal", "overweight", "obese"))

#fisher's exact test (used when more than 20% celss with expected count less than 5)
chisq.test(hstat3$hpt,hstat3$bmistatus)$expected
fisher.test(hstat3$hpt,hstat3$bmistatus)
#significant relationship between hpt and bmi status

---------------------------------------
#reporting your findings in table form
---------------------------------------
#package needed
#"sjPlot"
#"apaTables"

install.packages("sjPlot")
library(sjPlot)
install.packages("apaTables")
library(apaTables)

#table created in word file in your directory!
#sjt.xtab(hstat3$smoking, hstat3$hpt, file = "sjt_contingency.doc")
#apa.aov.table(one.way, filename="Table_anova.doc", table.number = 2)

