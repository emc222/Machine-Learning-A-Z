#Importing the DataSet
dataset<-read.csv('50_Startups.csv')

#Encoding Categorical Data
dataset$State=factor(dataset$State,levels = c('Florida','New York','California'),
                     labels = c(1,2,3)
                     )

#Splitting Data in Test and Train test

#install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(dataset$Profit,SplitRatio = 0.8)
train_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Fiting the data in Multiple Regression Model
#regressor=lm(formula = Profit~ R&D.Spend+Administration+Marketing.Spend+State)
regressor=lm(formula = Profit~.,data = train_set)

y_pred=predict(regressor,newdata = test_set)


