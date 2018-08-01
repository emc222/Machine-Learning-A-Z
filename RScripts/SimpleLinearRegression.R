#Importing the Dataset
dataset<-read.csv('Salary_Data.csv')

#Splitting Data into Training and Testing sets
library(caTools)
set.seed(123)
split=sample.split(dataset$Salary,SplitRatio = 2/3)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

#Fitting Simple Linear Regression to Training Set
regressor=lm(formula = Salary~YearsExperience,data=training_set)

#Predecting the Test Set Result
y_pred=predict(regressor,newdata = testing_set)

#Visualising the Model Traing Set
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x=training_set$YearsExperience,y=training_set$Salary),
             colour='red'
             )+
  geom_line(aes(x=training_set$YearsExperience,predict(regressor,newdata=training_set)),
                         colour='blue'
                         )+
  ggtitle('Salary Vs Years Experience (Training Set)')+
  xlab('Years Experience')+
  ylab('Salary')

#Visualising the Model Test Set
ggplot() +
  geom_point(aes(x=testing_set$YearsExperience,y=testing_set$Salary),
             colour='red'
  )+
  geom_line(aes(x=training_set$YearsExperience,predict(regressor,newdata=training_set)),
            colour='blue'
  )+
  ggtitle('Salary Vs Years Experience (Training Set)')+
  xlab('Years Experience')+
  ylab('Salary')