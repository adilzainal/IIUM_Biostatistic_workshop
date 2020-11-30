# ======================
# Descriptive statistics
#
# Edre MA, DrPH
# ======================

#--------------------------------
#~libraries needed to be installed~ 
#foreign
#epiDisplay
#psych
#ggubr
#usingR
#--------------------------------

# data
library(foreign)

healthstat = read.spss("healthstatus.sav", to.data.frame = TRUE) #if csv, just change the extension
str(healthstat)
summary(healthstat)

# central tendency & dispersion
mean(healthstat$sbp)
mean(healthstat$age)

sd(healthstat$sbp)
sd(healthstat$age)

median(healthstat$sbp)
median(healthstat$age)

IQR(healthstat$sbp)
IQR(healthstat$age)

# proportion
tab_sex = table(healthstat$sex)
tab_smoking = table(healthstat$smoking)
tab_sex
tab_smoking
str(tab_sex)
str(tab_smoking)

prop.table(tab_sex)
prop.table(tab_smoking)

prop.table(tab_sex)*100
prop.table(tab_smoking)*100

# describe using sapply
mean_all = sapply(healthstat[c("age", "sbp", "dbp")], mean)
mean_all
sd_all = sapply(healthstat[c("age", "sbp", "dbp")], sd)
sd_all
median_all = sapply(healthstat[c("age", "sbp", "dbp")], median)
median_all
iqr_all = sapply(healthstat[c("age", "sbp", "dbp")], IQR)
iqr_all

cbind(Mean = mean_all, SD = sd_all, Median = median_all, IQR = iqr_all)
rbind(Mean = mean_all, SD = sd_all, Median = median_all, IQR = iqr_all)

tab_ss = sapply(healthstat[c("sex", "smoking")], table)
tab_ss
prop_ss = prop.table(tab_ss, margin=2)
prop_ss
per_ss = (prop_ss)*100
per_ss

# describe using codebook
library(epiDisplay)
codebook(healthstat)
codebook(healthstat[c("age", "sbp", "dbp")])

# describe using describe
library(psych)
describe(healthstat[c("age", "sbp", "dbp")])

# Determining normality of numerical data: bell shaped curve
library(ggpubr)
ggdensity(healthstat$age, fill = "lightgray")

library(UsingR)
hist(healthstat$age, freq = FALSE)
x <- seq(21, 64, length.out=100)
y <- with(healthstat, dnorm(x, mean(age), sd(age)))
lines(x, y, col = "red")

# Determining normality of numerical data: normality test
shapiro.test(healthstat$age) #if data sample size is <50


# by groups (Stratified by a categorical variable)
by(healthstat$age, healthstat$sex, mean)
by(healthstat$age, healthstat$sex, sd)

by(healthstat$age, healthstat$smoking, mean)
by(healthstat$age, healthstat$smoking, sd)

by(healthstat$age, healthstat$sex, median)
by(healthstat$age, healthstat$sex, IQR)

by(healthstat$age, healthstat$smoking, median)
by(healthstat$age, healthstat$smoking, IQR)

# cross-tabulation
tab_sex_smoking = table(Gender = healthstat$sex, Smoking = healthstat$smoking)
tab_sex_smoking

prop_sex_smoking = prop.table(tab_sex_smoking, margin=1)
prop_sex_smoking

per_sex_smoking = prop_sex_smoking*100
per_sex_smoking


