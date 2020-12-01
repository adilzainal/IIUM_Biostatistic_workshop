# =======================
# Descriptive Statistics
# R Biostat Workshop IIUM
# Edre MA, DrPH
# =======================

#----------------------------------
#libraries needed to be installed
#----------------------------------
#foreign
#epiDisplay
#psych
#ggubr
#usingR


#--------------
# data
#--------------

#pulling the data from GitHub

#go to https://github.com/adilzainal/IIUM_Biostatistic_workshop
#click "code" -> "Download ZIP" 
#extract the ZIP file using WinRAR
#Create a new specific folder to store all files in your desktop
#set as working directory

#loading the data

#if spss (.sav)
library(foreign)
healthstat = read.spss("healthstatus.sav", to.data.frame = TRUE) 
str(healthstat)
summary(healthstat)
View(healthstat)

#if excel (.xlsx)
library(readxl)
healthstat <- read_excel("healthstatus.xlsx")
View(healthstat)

#----------------------------
#summarising numerical values
#----------------------------

# central tendency & dispersion
mean(healthstat$sbp)
mean(healthstat$age)

sd(healthstat$sbp)
sd(healthstat$age)

median(healthstat$sbp)
median(healthstat$age)

IQR(healthstat$sbp)
IQR(healthstat$age)

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

#--------------------
#normality assumption
#--------------------
#mean~median
#acceptable skewness & kurtosis +-2d
#bell shaped curve
#normality test

# describe using codebook, gives you mean~median
library(epiDisplay)
codebook(healthstat)
codebook(healthstat[c("age", "sbp", "dbp")])

# describe using describe, gives you skewness & kurtosis
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

#------------------------------
#summarising categorical values
#------------------------------

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

#crosstabulation
smokingbygender<-table(healthstat$sex, healthstat$smoking)
prop.table(smokingbygender, margin=1)
prop.table(smokingbygender, margin=1)*100

# by groups (Stratified by a categorical variable)
by(healthstat$age, healthstat$sex, mean)
by(healthstat$age, healthstat$sex, sd)

by(healthstat$age, healthstat$smoking, mean)
by(healthstat$age, healthstat$smoking, sd)

by(healthstat$age, healthstat$sex, median)
by(healthstat$age, healthstat$sex, IQR)

by(healthstat$age, healthstat$smoking, median)
by(healthstat$age, healthstat$smoking, IQR)

#-------------
#missing data
#-------------

#usually coded as "NA" in the dataset

is.na(healthstat)
which (is.na(healthstat$sbp))

#demonstrating the row to show the missing value using dummy data
x<- c(1,13,14,NA,2,44)
which (is.na(x))


#-----------------
#outlier detection
#-----------------

#visual method
ggplot(healthstat, aes(x = "sbp", y = sbp)) + geom_boxplot()

#data row method

is_outlier <- healthstat$age > 150 | healthstat$age < 0
is_outlier

#--------------------------------------
#basic data transformation:categorizing
#--------------------------------------

#glucose control (6.5% and above considered poor)
healthstat$glucontrol<-cut(healthstat$hba1c, breaks=c(-Inf,6.49,Inf),labels=c("good", "poor"))
summary(healthstat)

#bmistatus (WHO classification)
healthstat$bmi <- (healthstat$wt)/(healthstat$ht/100)**2
healthstat$bmi
healthstat$bmistat <- cut(healthstat$bmi, breaks=c(-Inf, 18.49999, 24.9999, 29.9999, Inf), labels=c("underweight", "normal", "overweight", "obese"))
healthstat$bmistat

#hypertension status (either sbp or dbp equal or more than 140/90mmHg, respectively, considered hypertensive)
healthstat$hpt<-(healthstat$sbp>=140|healthstat$dbp>=90)
summary(healthstat) #logical class for the new outcome
healthstat$hpt2 <- as.factor(healthstat$hpt) #convert from logical to a factor variable
summary(healthstat)

#Acknowledgement : Dr WNAriffin (USM)
