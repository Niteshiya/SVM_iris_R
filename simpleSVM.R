#Load data
data <- iris
str(data)

#library
library(ggplot2)
library(Amelia)
library(e1071)

#visulaization
pairs(data,col=data$Species)
qplot(Petal.Length,Petal.Width,data=data,col=Species)

#Support Vector Machine
model <- svm(Species~.,data=data)
#we can use kernal="linear"/"polynomial"/"sigmoid"
summary(model)
plot(model,data=data,Petal.Width~Petal.Length,slice=list(Sepal.Length=4,Sepal.Width=3))

#Confusion Matrix and mis classification error
pred <- predict(model,iris)
tab <- table(predicted = pred,actual=iris$Species)
tab
acc=100*(sum(diag(tab))/(sum(tab)))
acc

#Tuning/Hyper papameter optimization
set.seed(123)
tmodel <- tune(svm,Species~.,data=data,ranges=list(espilon=seq(0,1,0.1),cost=2^(2:7)))
plot(tmodel)
summary(tmodel)

#best model
my_model <- tmodel$best.model 
summary(my_model)
plot(my_model,data=data,Petal.Width~Petal.Length,slice=list(Sepal.Length=4,Sepal.Width=3))
pred1 <- predict(my_model,iris)
tab1 <- table(predicted = pred1,actual=iris$Species)
tab1
acc1=100*(sum(diag(tab1))/(sum(tab1)))
acc1
