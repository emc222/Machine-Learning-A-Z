#Importing the DataSet
dataset<-read.csv('Position_Salaries.csv')
dataset=dataset[2:3]


#Fitting the Data in Linear Model
lin_reg=lm(formula = Salary~.,data = dataset)

#Fitting Data in Polynomial Model
dataset$Level2=dataset$Level^2
dataset$Level3=dataset$Level^3
poly_reg=lm(formula = Salary~.,data = dataset)

#Visulising the Linear Model
library(ggplot2)
ggplot()+geom_point(aes(x=dataset$Level,y=dataset$Salary),colour='red')+
  geom_line(aes(x=dataset$Level,y=predict(lin_reg,newdata = dataset)),colour='blue')+
  ggtitle('Level VS Salary Linear')+xlab('Level')+ylab('Salary')

#Visulising the Polynomial Model
ggplot()+geom_point(aes(x=dataset$Level,y=dataset$Salary),colour='red')+
  geom_line(aes(x=dataset$Level,y=predict(poly_reg,newdata = dataset)),colour='blue')+
  ggtitle('Level VS Salary Linear')+xlab('Level')+ylab('Salary')

