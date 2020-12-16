# =======================
# Descriptive Statistics
# R Biostat Workshop IIUM
# Edre MA, DrPH
# =======================


# You you are a researcher involved in a hypertension study
#objective 1: To describe the background characteristics of respondents
#objective 2: To determine the prevalence of hypertension
#objective 3: To determine the factors contributing to hypertension

#----------------------------------
#libraries needed to be installed
#----------------------------------
#readr
#smartEDA
#moments
#ggpubr
#usingR
#car
#ggplot2
#dplyr

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
library(readr)
healthstat <- read_csv("healthstatus6.csv") #load the file and make as object
View(healthstat) 

#----------------------------
#objective 1: To describe the background characteristics of respondents
#summarising numerical values
#----------------------------

# we choose 3 IVs: age,sbp,dbp

library(smartEDA)
ExpCustomStat(healthstat,
              Nvar=c("age","sbp","dbp"),
              stat = c('mean', 'sd', 'median', 'IQR'))


#--------------------------
#normality assumption check
#--------------------------
#there are 5 criteria before you make decision what to report:

#1.mean~median

ExpCustomStat(healthstat,
              Nvar=c("age","sbp","dbp"),
              stat = c('mean','median'))

#2. acceptable skewness & kurtosis +-2d

library(moments)
ExpCustomStat(healthstat,
              Nvar=c("age","sbp","dbp"),
              stat = c('skewness','kurtosis'))

#3. bell shaped curve (The MOST powerful determinant of normality)
library(ggpubr)
ggdensity(healthstat$sbp, fill = "lightgray")

library(UsingR)
hist(healthstat$sbp, freq = FALSE)
x <- seq(110, 160, length.out=170)
y <- with(healthstat, dnorm(x, mean(sbp), sd(sbp)))
lines(x, y, col = "red")

#4. qqplot

library(car)
qqPlot(healthstat$sbp)

#5. normality test
shapiro.test(healthstat$sbp) #sample size less than 50
ks.test(x, "pnorm", mean=mean(healthstat$sbp), sd=sd(healthstat$sbp))

#finally, make your decision
ExpCustomStat(healthstat,
              Nvar=c("age","sbp","dbp"),
              stat = c('mean', 'sd'))


#------------------------------
#summarising categorical values
#------------------------------

ExpCustomStat(healthstat,
              Cvar=c("sex", "smoking"),
              gpby=FALSE)

#count refers to the frequency, n
#proportion here refers to the percentage distribution of that category

#-------------
#missing data
#-------------

#usually coded as "NA" in the dataset
#we create a dummy object first to showcase this exercise
missing <- healthstat
missing[missing$id==57, "sbp"] <- NA

#demonstrating the row to show the missing value using dummy data
missing$sbp
which (is.na(missing$sbp))

#-----------------
#outlier detection
#-----------------

#create an outlier dummy data 
outlierdummy <- healthstat
outlierdummy[outlierdummy$id==131, "sbp"] <- 1244

#visual method
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

library(dplyr)

#hypertension status (either sbp or dbp equal or more than 140/90mmHg, respectively, considered hypertensive)
#to answer objective 2

healthstatcat<-healthstat %>%
  mutate(hpt=if_else(healthstat$sbp<140 & healthstat$dbp<90,'normal','high')) 

View(healthstatcat)

ExpCustomStat(healthstatcat,
              Cvar="hpt",
              stat=c("count","prop"))

#to make data preparation for objective 3
#glucose control (6.5% and above considered poor)


healthstatcat$glucontrol<-cut(healthstatcat$hba1c, 
                           breaks=c(-Inf,6.49,Inf),
                           labels=c("Good", "Poor"))
summary(healthstatcat)

#bmistatus (WHO classification)

healthstatcatbmi<- healthstatcat %>% 
  mutate(height_m = ht / 100,bmi = wt / (height_m^2))

View(healthstatcatbmi)

healthstatcatbmi$bmistatus<- cut(healthstatbmi$bmi, 
                           breaks=c(-Inf, 18.49999, 24.9999, 29.9999, Inf), 
                           labels=c("underweight", "normal", "overweight", "obese"))
summary(healthstatcatbmi)

#-------------------------------------------------
#Reporting your descriptive analysis
#-------------------------------------------------
library(stargazer)
stargazer(healthstatcatbmi)
stargazer(healthstatcatbmi, type = "html", 
          title="Descriptive statistics", 
          digits=1, out="table1.doc")

#for categorical data, just edit and add in the current table 
#limitation in the r package stargazer
          