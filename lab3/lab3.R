#exercise1
set.seed(42)
par(mar = rep(0.2,4))
data_matrix = matrix(rnorm(400),nrow = 40)
image(1:10,1:40,t(data_matrix)[,nrow(data_matrix):1])
heatmap(data_matrix)

for(i in 1:40){
  coin_flip = rbinom(1,1,0.5)
  if(coin_flip){
    data_matrix[i,] = data_matrix[i,]+rep(c(0,3),each = 5)
  }
}
image(1:10,1:40,t(data_matrix)[,nrow(data_matrix):1])
heatmap(data_matrix)

hh = hclust(dist(data_matrix))
data_matrix_ordered = data_matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_matrix)[,nrow(data_matrix):1])
plot(rowMeans(data_matrix_ordered),40:1, , xlab="The Row Mean",ylab = "row", pch = 19)
plot(colMeans(data_matrix_ordered), xlab="The Row Mean",ylab = "row", pch = 19)
#exercise 2
AB_data <- read.csv("~/Downloads/Co-term/ITWS6600/lab3/abalone.csv")
attach(AB_data)
summary(AB_data)
str(AB_data)
summary(AB_data$rings)
AB_data$Rings <- as.numeric(AB_data$Rings)
AB_data$Rings <- cut(AB_data$Rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
AB_data$Rings <- as.factor(AB_data$Rings)
summary(AB_data$Rings)
aba = AB_data
normalize <- function(x) {
  return ((as.numeric(x) - min(as.numeric(x))) / (max(as.numeric(x)) - min(as.numeric(x))))
}
aba[2:7] <- as.data.frame(lapply(aba[2:7], normalize))
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)
library(class)
KNNpred <- knn(train = KNNtrain[2:7], test = KNNtest[2:7], cl = KNNtrain$Rings, k = 55)
KNNpred
table(KNNpred)