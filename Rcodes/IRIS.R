library(ggplot2)
library(grid)
library(gridExtra)
library(plyr)
library(MASS)

#Checkout IRIS Data
dim(iris)
summary(iris)

#Box Plot Visualisation of variables.
BpSl <- ggplot(iris, aes(Species, Sepal.Length, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Sepal Length (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpSw <-  ggplot(iris, aes(Species, Sepal.Width, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Sepal Width (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpPl <- ggplot(iris, aes(Species, Petal.Length, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Petal Length (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpPw <-  ggplot(iris, aes(Species, Petal.Width, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Petal Width (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

grid.arrange(BpSl  + ggtitle(""),
             BpSw  + ggtitle(""),
             BpPl + ggtitle(""),
             BpPw + ggtitle(""),
             nrow = 2,
             top = textGrob("Sepal and Petal Box Plot")
)

#Split Train & Test Data

index=sample(1:nrow(iris),size=0.6*nrow(iris))
iris.train=iris[index,]
iris.test=iris[-index,]
dim(iris.train)
dim(iris.test)

#LDA model

fit.LDA = lda( Species ~ .-Species, iris.train)
fit.LDA

pred.LDA=predict(fit.LDA,iris.test[,1:4])$class
final.LDA=cbind(iris.test,pred.LDA)
table(final.LDA$Species,pred.LDA)

final.LDA$isTrue<- ifelse((final.LDA$Species==pred.LDA),1,0)
accuracy=(sum(final.LDA$isTrue==1)/nrow(final.LDA))
accuracy
