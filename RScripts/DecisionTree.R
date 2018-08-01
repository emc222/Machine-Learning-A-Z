#Importing the DataSet
dataset<-read.csv('Position_Salaries.csv')
dataset=dataset[2:3]
#Fitting the Data in the Decision Tree Rregressor Model
#install.packages('rpart')
library(rpart)

regressor=rpart(formula = Salary~.,data = dataset,
                control = rpart.control(minsplit = 1)
                )

y_pred=predict(regressor,data.frame(Level=6.5))


#Visulising the SVR Model
library(ggplot2)
ggplot()+geom_point(aes(x=dataset$Level,y=dataset$Salary),colour='red')+
  geom_line(aes(x=dataset$Level,y=predict(regressor,newdata = dataset)),colour='blue')+
  ggtitle('Level VS Salary Linear')+xlab('Level')+ylab('Salary')


