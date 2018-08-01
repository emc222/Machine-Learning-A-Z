#Importing the DataSet
dataset<-read.csv('mpgdataset.csv')

dataset <- subset(dataset, select = -c(carName))
# dataset <- subset(dataset, select = -c(cylinders))
# dataset <- subset(dataset, select = -c(horsepower))
#Handling the Missing Data
dataset$horsepower=ifelse(is.na(dataset$horsepower),ave(dataset$horsepower,FUN=function(x) mean(x,na.rm = TRUE)),
                   dataset$horsepower
)

#Encoding Categorical Data
dataset$origin=factor(dataset$origin,levels = c(1,2,3),
                     labels = c(1,2,3)
)

#Splitting Data in Test and Train test

#install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(dataset$mpg,SplitRatio = 0.8)
train_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#Fiting the data in Multiple Regression Model
#regressor=lm(formula = Profit~ R&D.Spend+Administration+Marketing.Spend+State)
regressor=lm(formula = mpg~.,data = train_set)

y_pred=predict(regressor,newdata = test_set)

#Changing the formula after analyzing the regressor
regressor=lm(formula = mpg~displacement+weight+acceleration+modelYear+origin,data = train_set)

y_pred=predict(regressor,newdata = test_set,interval = 'confidence')

y_pred=predict(regressor,newdata = test_set)
library(ggplot)

plot(y_pred,test_set$mpg,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

res <- stack(data.frame(Observed = test_set$mpg, Predicted = y_pred))
res <- cbind(res, x = rep(test_set$mpg, 2))
head(res)
require("lattice")

xyplot(values ~ x, data = res, group = ind, auto.key = TRUE)