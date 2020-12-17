# Introduction to R
setwd(getwd())

# Data types

ID <- 1:100
Age <- rnorm(100, 40, 10)
head(Age)
Age <- round(Age, digit=0)
Sex <- rbinom(100, 1, .5)
table(Sex)
Sex <- factor(Sex, levels = c(0,1), labels = c("Female", "Male"))
table(Sex)
data <- data.frame(cbind(ID, Age, Sex))
head(data)

# Basic operations

summary(Age)
table(Sex)

hist(Age)
plot(Sex)

plot(data)
plot(Sex, Age)

# Packages

# Tidyverse
install.packages("tidyverse")
library(tidyverse)

data %>% 
  group_by(Sex) %>% 
  summarise(
    N = n(),
    Mean = mean(Age)
  )


ggplot(data, aes(Age))+
  geom_histogram(fill="blue", alpha=.6, bins = 20)

