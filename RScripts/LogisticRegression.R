#Importing the DataSet
dataset<-read.csv('Social_Network_Ads.csv')

dataset=dataset[,3:5]

#Splitting Data in Test and Train test

#install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

#Feature Scaling
training_set[,1:2]=scale(training_set[,1:2])
testing_set[,1:2]=scale(testing_set[,1:2])

#Fitting Data in Logistic Regression
classifier=glm(formula = Purchased~.,family = binomial,data =training_set)

prob_predict=predict(classifier,type = 'response',newdata = testing_set[-3])

y_pred=ifelse(prob_predict>0.5,1,0)
