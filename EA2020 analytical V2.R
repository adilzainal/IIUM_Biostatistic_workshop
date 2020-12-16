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

#we want to visualize the comparison

library(ggpubr)
ggboxplot(healthstat, x = "sex", y = "sbp",
          color = "sex", 
          palette = "jco", 
          add = "mean") + 
          stat_compare_means(method = "t.test")
      
#now we know sex has no effect on sbp in our study
#we want to know now, does exercise have an effect (low,mod,high intensity)
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

#yes, there is significant effect of exercise on sbp
#but, which pair comparison has most effect?

leveneTest(sbp ~ exercise, data = healthstat, center=mean)

#significant p value, thus equal variance not assumed
#Post Hoc test - Games Howell

install.packages("userfriendlyscience")
library(userfriendlyscience)
oneway(healthstat$exercise, y = healthstat$sbp, posthoc = 'games-howell')

#if equal variance assumed, use Tukey
tukey.one.way<-TukeyHSD(one.way) 
tukey.one.way

#Exercise adds benefit in sbp reduction
#does it correlate with weight?

#pearson correlation coefficient test

cor.test(healthstat$wt,healthstat$sbp, method="pearson")
ggscatter(healthstat, x = "wt", y = "sbp", 
          add = "reg.line", 
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.method = "pearson", 
          xlab = "Weight (kg)", ylab = "SBP (mmHg)")

#yes, the heavier the person, the higher the sbp
#now you conducted a high-intensity interval training (HIIT) intervention
#you want to measure pre and post HIIT effect on weight
#paired t test

t.test(healthstat$wt, healthstat$wt2, paired=TRUE)
ggscatter(healthstat, x = "wt", y = "wt2", 
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
healthstat$lnhcy= log(healthstat$hcy)
hist(healthstat$lnhcy)
ggdensity(healthstat$lnhcy, fill = "lightgray")

#still not normally distributed
#need to do non-parametric test

#female has higher or lower hcy level compared to male?
#mann whitney U test

ExpCustomStat(healthstat,
              Cvar="sex",
              Nvar="hcy",
              stat=c("median","IQR"),
              gpby=TRUE,
              dcast=F)

wilcox.test(hcy~sex, data=healthstat)
#sex has no signififant relationship with hcy

#how about exercise intensity?
#kruskal wallis test

ExpCustomStat(healthstat,
              Cvar="exercise",
              Nvar="hcy",
              stat=c("median","IQR"),
              gpby=TRUE,
              dcast=F)

kruskal.test(hcy ~ exercise, data = healthstat) #if significant, proceed with pairwise comparison
pairwise.wilcox.test(healthstat$hcy, healthstat$exercise,p.adjust.method = "BH")

#high intensity exercise significantly gives lower HCY compared to low/mod 
#you proceed in continuing the HIIT intervention as it gives benefit to both sbp and hcy

#measure effectiveness again on weight reduction
#wilcoxon signed rank test

wilcox.test(healthstat$wt,healthstat$wt2,paired=TRUE)

#you notice some of your respondents are diabetic
#worry that your intervention gives more harm than good
#finding relationship between hba1c and both sbp/hcy
#spearman correlation coefficient test

cor.test(healthstat$hba1c,healthstat$sbp, method="spearman")
cor.test(healthstat$hba1c,healthstat$hcy, method="spearman")

#you conclude that only sbp has a significant correlation with hba1c
#in future, you would prioritize giving HITT intervention to hypertensive patients

#now, you are focused back to your objective 3
#factors contributing to hypertension (hpt)

#----------------------------
#comparing categorical variables
#----------------------------

healthstatcat<-healthstat %>%
  mutate(hpt=if_else(healthstat$sbp<140 & healthstat$dbp<90,'normal','high')) 
View(healthstatcat)

#smoking has a relationship with hpt?
#chi square test

chisq.test(healthstatcat$hpt,healthstatcat$smoking,correct=F)
chisq.test(healthstatcat$hpt,healthstatcat$smoking)$observed
#yes, smoking is significantly related to hpt. More smokers are hypertensive

#how about BMI status and hpt?

healthstatcatbmi<- healthstatcat %>% 
  mutate(height_m = ht / 100,bmi = wt / (height_m^2))
View(healthstatcatbmi)
healthstatcatbmi$bmistatus<- cut(healthstatbmi$bmi, 
                                 breaks=c(-Inf, 18.49999, 24.9999, 29.9999, Inf), 
                                 labels=c("underweight", "normal", "overweight", "obese"))

#fisher's exact test (used when more than 20% celss with expected count less than 5)
chisq.test(healthstatcatbmi$hpt,healthstatcatbmi$bmistatus)$expected
fisher.test(healthstatcatbmi$hpt,healthstatcatbmi$bmistatus)
#significant relationship between hpt and bmi status

---------------------------------------
#reporting your findings in table form
---------------------------------------
#package needed
#"sjPlot"
#"apaTables"
library(sjPlot)
library(apaTables)

#table created in word file in your directory!
sjt.xtab(healthstatcatbmi$smoking, healthstatcatbmi$hpt, file = "sjt_contingency.doc")
apa.aov.table(one.way, filename="Table_anova.doc", table.number = 2)

