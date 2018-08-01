#Importing the DataSet
dataset<-read.csv('Data.csv')


#Handling the Missing Data
dataset$Age=ifelse(is.na(dataset$Age),ave(dataset$Age,FUN=function(x) mean(x,na.rm = TRUE)),
                   dataset$Age
                   )
dataset$Salary=ifelse(is.na(dataset$Salary),ave(dataset$Salary,FUN=function(x) mean(x,na.rm = TRUE)),
                   dataset$Salary
)


sum(dataset$age=='unknown')
sum(dataset$nr.employed=='unknown')

install.packages('gdata')
#Encoding Categorical Data
dataset$Country=factor(dataset$Country,levels = c('France','Spain','Germany'),
                       labels = c(1,2,3)
                       )
dataset$Purchased=factor(dataset$Purchased,levels = c('No','Yes'),
                       labels = c(0,1)
)

#Splitting Data in Test and Train test

#install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

#Feature Scaling
training_set[,2:3]=scale(training_set[,2:3])
testing_set[,2:3]=scale(testing_set[,2:3])