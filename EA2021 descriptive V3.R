# =======================
# Descriptive Statistics
# R Biostat Workshop IIUM
# Edre MA, DrPH
# =======================


#You you are a researcher involved in a hypertension study
#objective 1: To describe the background characteristics of respondents
#objective 2: To determine the prevalence of hypertension
#objective 3: To determine the factors contributing to hypertension

#----------------------------------
#libraries needed to be installed
#----------------------------------
#readr - read csv file
#smartEDA - custom descriptive stat
#moments - skewness and kurtosis(normality)
#ggpubr - visualization of density (normal curve)
#usingR - histogram (normality)
#car - qqplot (normality)
#ggplot2 - visualization of boxplot
#dplyr - transform / mutate variables
#table1 - basic descriptive table 

install.packages ("name of the package")

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

#if csv (.csv)
install.packages("readr")
library(readr)
hstat <- read_csv("healthstatus6.csv") #load the file and make as object
View(hstat) 

#----------------------------
#objective 1: To describe the background characteristics of respondents
#summarising numerical values
#----------------------------

# we choose 3 IVs: age,sbp,dbp

install.packages("SmartEDA")
library(SmartEDA)
ExpCustomStat(hstat,
              Nvar=c("age","sbp","dbp"),
              stat = c('mean', 'sd', 'median', 'IQR'))

ExpCustomStat(hstat,
              Cvar=c("exercise","sex","smoking"),
              stat = c('count', 'prop'),gpby= FALSE)

#--------------------------
#normality assumption check
#--------------------------
#there are 5 criteria before you make decision what to report:

#1.mean~median

ExpCustomStat(hstat,
              Nvar=c("age","sbp","dbp"),
              stat = c('mean','median'))

#2. acceptable skewness & kurtosis +-2d

install.packages("moments")
library(moments) 
ExpCustomStat(hstat,
              Nvar=c("age","sbp","dbp"),
              stat = c('skewness','kurtosis'))

#3. bell shaped curve (The MOST powerful determinant of normality)
install.packages("ggpubr")
library(ggpubr)
ggdensity(hstat$sbp, fill = "lightgray")

install.packages("usingR")
library(UsingR)
hist(hstat$sbp, freq = FALSE)
x <- seq(110, 160, length.out=170)
y <- with(hstat, dnorm(x, mean(sbp), sd(sbp)))
lines(x, y, col = "red")

#4. qqplot

install.packages("car")
library(car)
qqPlot(hstat$sbp)

#5. normality test
shapiro.test(hstat$sbp) #sample size less than 50
ks.test(x, "pnorm", mean=mean(hstat$sbp), sd=sd(hstat$sbp))

#finally, make your decision
ExpCustomStat(hstat,
              Nvar=c("age","sbp","dbp"),
              stat = c('mean', 'sd'))


#------------------------------
#summarising categorical values
#------------------------------

ExpCustomStat(hstat,
              Cvar=c("sex", "smoking"),
              gpby=FALSE)

#count refers to the frequency, n
#proportion here refers to the percentage distribution of that category

#-------------
#missing data
#-------------

#usually coded as "NA" in the dataset
#we create a dummy object first to showcase this exercise
missing <- hstat
missing[missing$id==57, "sbp"] <- NA

#demonstrating the row to show the missing value using dummy data
missing$sbp
which (is.na(missing$sbp))

#-----------------
#outlier detection
#-----------------

#create an outlier dummy data 
outlierdummy <- hstat
outlierdummy[outlierdummy$id==131, "sbp"] <- 1244

#visual method
install.packages("ggplot2")
library(ggplot2)
ggplot(outlierdummy, aes(x = "sbp", y = sbp)) + geom_boxplot()

#data row method

is_outlier <- outlierdummy$sbp > 250 | outlierdummy$sbp < 70
is_outlier

#--------------------------------------
#objective 2: To determine the prevalence of hypertension
#objective 3: To determine the factors contributing to hypertension
#basic data transformation:categorizing
#--------------------------------------

install.packages("dplyr")
library(dplyr)

#hypertension status (either sbp or dbp equal or more than 140/90mmHg, respectively, considered hypertensive)
#to answer objective 2

hstat2 <-hstat %>%
  mutate(hpt=if_else(hstat$sbp<140 & hstat$dbp<90,'normal','high')) 

View(hstat2)

ExpCustomStat(hstat2,
              Cvar="hpt",
              stat=c("count","prop"))

#to make data preparation for objective 3
#glucose control (6.5% and above considered poor)


hstat2$glucontrol<-cut(hstat2$hba1c, 
                           breaks=c(-Inf,6.49,Inf),
                           labels=c("Good", "Poor"))
summary(hstat2)

#bmistatus (WHO classification)

hstat3<- hstat2 %>% 
  mutate(height_m = ht / 100,bmi = wt / (height_m^2))

View(hstat3)

hstat3$bmistatus<- cut(hstat3$bmi, 
                           breaks=c(-Inf, 18.49999, 24.9999, 29.9999, Inf), 
                           labels=c("underweight", "normal", "overweight", "obese"))
summary(hstat3)

#-------------------------------------------------
#Reporting your descriptive analysis
#-------------------------------------------------
install.packages("table1")
library(table1)
table1(~  age + factor(smoking) + factor(exercise) + wt + bmi, data=hstat3)

#thank you
          