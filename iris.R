library(datasets)
data(iris)
summary(iris)

View(iris)

## Charts

plot(iris)

library(ggplot2)
library(GGally)
ggpairs(iris,mapping=ggplot2::aes(colour=Species))

## K nearest neighbors

library(class)

train=sample(seq_len(nrow(iris)),size=floor(0.5*nrow(iris)))

iris.train <- iris[train,c(1:4)]
iris.test <- iris[-train,c(1:4)]
response <- iris[train,5]
tresponse <- iris[-train,5]

knn.pred <- knn(iris.train,iris.test,response,k=3)
table(knn.pred,tresponse)

library(gmodels)
CrossTable(x = tresponse,y=knn.pred,prop.chisq=FALSE)

## Another method of KNN, now choosing k endogenously

library(caret)
set.seed(1)
ctrl <- trainControl(method="repeatedcv",repeats=3)
knnFit <- train(Species ~ .,data=iris[train,],method="knn",trControl=ctrl)
knnFit

plot(knnFit)

knn.pred <- knn(iris.train,iris.test,response,k=3)
table(knn.pred,tresponse) ## 96% accuracy

## Using linear discriminant analysis

library(tidyverse)
library(MASS)
model <- lda(Species~.,data=iris[train,])
model
predict <- model %>% predict(iris.test)
mean(predict$class==tresponse) ## 96% accuracy
