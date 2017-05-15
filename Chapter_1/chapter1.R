setwd("~/Statistical Learning/Chapter_1")
# Load the Dataset
library(readr)
library(dplyr)
library(magrittr)
College_DB  <- read_csv("~/Statistical Learning/Chapter_1/College.csv")
rownames(College_DB) <- College_DB$X1
College_DB <- College_DB %>%
  dplyr::select(-1)
summary(College_DB)
pairs(College_DB[,2:10])
plot(College_DB$Private,College_DB$Outstate)

# iv.
College_DB$Elite <- "No"
College_DB$Elite <- ifelse(College_DB$Top10perc > 50,"Yes",College_DB$Elite)
College_DB$Elite <- as.factor(College_DB$Elite)
summary(College_DB$Elite)
plot(College_DB$Elite,College_DB$Outstate)
# v.
par(mfrow=c(1,1))
hist(College_DB$Apps)
hist(College_DB$Accept)
hist(College_DB$Enroll)
hist(College_DB$S.F.Ratio)

# vi.
plot(College_DB$Outstate,College_DB$Grad.Rate)
plot(College_DB$Accept / College_DB$Apps, College_DB$S.F.Ratio)

# A9.
Auto_DB <- read_csv("~/Statistical Learning/Chapter_1/Auto.csv")
# sapply(Auto_DB, function(x) sum(is.na(x))) [See if the dataset consists of NA values]
Auto_DB$horsepower <- ifelse(Auto_DB$horsepower == "?",NA,Auto_DB$horsepower)
typeof(Auto_DB$horsepower)
Auto_DB$horsepower <- as.integer(Auto_DB$horsepower)
Auto_DB <- na.omit(Auto_DB)
summary(Auto_DB)

# b.
# Sapply function used to perform an inbuilt operation in each column 
# of a dataframe. Each column is treated like a vector.
sapply(Auto_DB,range)

# c.
sapply(Auto_DB[,1:7],mean)
sapply(Auto_DB[,1:7],sd)

# d.
newAuto_DB <- Auto_DB[-(10:85),]
sapply(newAuto_DB[,1:7],mean)
sapply(newAuto_DB[,1:7],sd)

# e. 
pairs(Auto_DB[,1:7])
plot(Auto_DB$year,Auto_DB$mpg)

# A10.
library(MASS)
Boston_DB <- Boston
par(mfrow=c(2,2))
plot(Boston_DB$age,Boston_DB$nox)
plot(Boston_DB$crim,Boston_DB$tax)
plot(Boston_DB$medv,Boston_DB$crim)
plot(Boston_DB$black,Boston_DB$crim)

# c.
plot(Boston_DB$age, Boston_DB$crim)
# Older homes, more crime
plot(Boston_DB$dis, Boston_DB$crim)
# Closer to work-area, more crime
plot(Boston_DB$rad, Boston_DB$crim)
# Higher index of accessibility to radial highways, more crime
plot(Boston_DB$tax, Boston_DB$crim)
# Higher tax rate, more crime
plot(Boston_DB$ptratio, Boston_DB$crim)
# Higher pupil:teacher ratio, more crime


# d. 
par(mfrow=c(1,3))
hist(Boston_DB$crim[Boston_DB$crim>1], breaks=25)
hist(Boston_DB$tax[Boston_DB$tax],breaks = 25)
hist(Boston_DB$ptratio,breaks = 25)

# e. 
BostonCharles_DB <- subset(Boston_DB,Boston_DB$chas == 1)

# g. 
subset(Boston_DB,Boston_DB$medv == min(Boston_DB$medv))

sapply(Boston_DB,range)
sapply(subset(Boston_DB,Boston_DB$medv == min(Boston_DB$medv)),range)

# h.
dim(subset(Boston_DB,Boston_DB$rm>7))
dim(subset(Boston_DB,Boston_DB$rm>8))
summary(subset(Boston_DB,Boston_DB$rm>8))
