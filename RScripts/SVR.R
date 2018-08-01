#Importing the DataSet
dataset<-read.csv('Position_Salaries.csv')
dataset=dataset[2:3]
#Fitting the Data in the SVR Model
#install.packages('e1071')
library(e1071)
regressor=svm(formula=Salary~.,
              data = dataset,
              type='eps-regression'
              )
y_pred=predict(regressor,data.frame(Level=6.5))
#Visulising the SVR Model
library(ggplot2)
ggplot()+geom_point(aes(x=dataset$Level,y=dataset$Salary),colour='red')+
  geom_line(aes(x=dataset$Level,y=predict(regressor,newdata = dataset)),colour='blue')+
  ggtitle('Level VS Salary Linear')+xlab('Level')+ylab('Salary')


