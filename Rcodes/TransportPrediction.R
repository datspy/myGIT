library(corrplot)
library(psych)
library(randomForest)
library(ggplot2)
library(scales) # visualization
library(dplyr)

#Checkout the dataset
setwd('C:/Users/MG Gokhul/Desktop/GreatLakes/MachineLearning')
carsdata = read.csv(file.choose(),header=T)
summary(carsdata)
attach(carsdata)
names(carsdata)
table(is.na(carsdata))
names(which(sapply(carsdata, function(x) any(is.na(x)))))
table(MBA)
MBA.median=median(MBA,na.rm=T)
carsdata[is.na(MBA),"MBA"]=MBA.median
numericGender=as.numeric(Gender)

#Creating a new column for Car users
carsdata$isCar<- ifelse((Transport=="Car"),1,0)
str(carsdata)
attach(carsdata)

#people who do not have license also use Car for transport.
table(license,isCar)


hist(Distance,col='orange')
hist(Work.Exp,xlab="Work Experience(in Years)",col='light blue')

pdat <- carsdata %>% group_by(Gender,license) %>% summarise(count=n()) %>% 
  mutate(cnt = count)
ggplot(data=pdat, aes(x=Gender,y=cnt,fill=as.factor(license))) +
  geom_bar(stat="identity", position="fill")

pdat <- carsdata %>% group_by(Transport) %>% summarise(count=n()) %>% 
  mutate(cnt = count)
ggplot(data=pdat, aes(x=as.factor(Transport),y=cnt,fill=factor(Transport))) +
  geom_bar(stat="identity", position="dodge")

pdat <- carsdata %>% group_by(license,isCar) %>% summarise(count=n())
ggplot(data=pdat, aes(x=as.factor(license),y=count,fill=factor(isCar))) +
  geom_bar(stat="identity", position="fill") + xlab('Has License')

cordata=carsdata[c(-2,-9)]
str(cordata)
corrx <- cor(cordata)
corrplot(corrx)

#splitting training & testing data

indexes = sample(1:nrow(carsdata), size=0.7*nrow(carsdata))
train.data = carsdata[indexes,]
test.data = carsdata[-indexes,]
dim(test.data) 
Gender=factor(Gender)


rf_model <- randomForest(factor(Transport) ~ Age + Gender + Engineer + 
                           MBA + Work.Exp + Salary + 
                           Distance + license,
                         data = train.data)

pred.data = read.csv(file.choose(),header=T)

plot(rf_model,main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates")

summary(rf_model)
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = (dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  labs(x = 'Variables') +  coord_flip()

prediction <- predict(rf_model, pred.data)
pcheck<-cbind(pred.data,prediction)
pcheck
