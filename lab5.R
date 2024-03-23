head(mtcars)
str(mtcars)
model1 = lm(mpg ~ cyl + wt, data = mtcars )
model1
plot(model1, pch = 18, col = 'red', which = c(4))

cooks.distance(model1)
CookDistance = cooks.distance(model1)
r_CookDist = round(CookDistance,5)
sort(r_CookDist)

library(ISLR)
head(Hitters)
dim(Hitters)
is.na(Hitters)
HittersData = na.omit(Hitters)
dim(HittersData)
head(HittersData)
SalaryPredictModel1 = lm(Salary ~ .,data = HittersData)
summary(SalaryPredictModel1)

cooksD = cooks.distance(SalaryPredictModel1)
influential = cooksD[(cooksD>(3*mean(cooksD, na.rm = TRUE)))]
influential

name_of_influential = names(influential)
name_of_influential
library(dplyr)
outliers = HittersData[name_of_influential,]
Hitter_without_outliers = HittersData %>% anti_join(outliers)

SalaryPredictModel2 = lm(Salary ~ . , data = Hitter_without_outliers)
summary(SalaryPredictModel2)

set.seed(10)

data1 = rnorm(50)

set.seed(30)

data2 = rnorm(50)

shapiro.test(data1)
hist(data1,col = 'green')

shapiro.test(data2)
hist(data1,col = 'steelblue')

set.seed(0)
data = rnorm(100)

shapiro.test(data)

set.seed(0)
data = rpois(n=100, lambda = 3)
shapiro.test(data)
hist(data,col='yellow')

library(nortest)
set.seed(1)

x = rnorm(100,0,1)
ad.test(x)

set.seed(1)
x = runif(100,0,1)
ad.test(x)



