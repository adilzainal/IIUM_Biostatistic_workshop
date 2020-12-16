# Introduction to R

# Data types

ID <- 1:50
Age <- rnorm(50, 40, 10)
head(Age)
Age <- round(Age, digit=0)
Sex <- rbinom(50, 1, .5)
table(Sex)
Sex <- factor(Sex, labels = c("Female", "Male"))
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

