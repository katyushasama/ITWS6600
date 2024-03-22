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

SalaryPredictModel2 = lm(Salary !. , data = Hitter_without_outliers)
summary(SalaryPredictModel2)