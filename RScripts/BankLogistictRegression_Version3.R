#Importing the Data
dataset <- read.table("bank-additional-full.csv",
                      sep = ";",
                      header = T)
summary(dataset)

#As descovered in the Version2 removing some of the variables
dataset=subset(dataset, select=-c(pdays,duration,euribor3m,nr.employed,
                                  job,loan,previous,age,marital,education,housing))

#Removing Rows with missing Numeric Values
dataset=subset(dataset,default!='yes')
dataset=subset(dataset,marital!='unknown')

dataset$marital

#Encoding the Catagorical variables

#marital
# dataset$marital = factor(dataset$marital,
#                          levels = (unique(dataset$marital)),
#                          labels = (seq(1, length(
#                            unique(dataset$marital)
#                          ), by = 1)))
# sum(is.na(dataset$marital))

#education
dataset$education = factor(dataset$education,
                           levels = (unique(dataset$education)),
                           labels = (seq(1, length(
                             unique(dataset$education)
                           ), by = 1)))
sum(is.na(dataset$education))

#default
dataset$default=factor(dataset$default,
                       levels = (unique(dataset$default)),
                       labels = (seq(1,length(unique(dataset$default)),by=1))
)
sum(is.na(dataset$default))

#housing
# dataset$housing=factor(dataset$housing,
#                        levels = (unique(dataset$housing)),
#                        labels = (seq(1,length(unique(dataset$housing)),by=1))
# )
# sum(is.na(dataset$housing))

# #loan
# dataset$loan = factor(dataset$loan,
#                       levels = (unique(dataset$loan)),
#                       labels = (seq(1, length(
#                         unique(dataset$loan)
#                       ), by = 1)))
# sum(is.na(dataset$loan))
  
#contact
dataset$contact = factor(dataset$contact,
                         levels = (unique(dataset$contact)),
                         labels = (seq(1, length(
                           unique(dataset$contact)
                         ), by = 1)))
sum(is.na(dataset$contact))

#Month
dataset$month = factor(dataset$month,
                       levels = (unique(dataset$month)),
                       labels = (seq(1, length(
                         unique(dataset$month)
                       ), by = 1)))
sum(is.na(dataset$month))

#day_of_week
dataset$day_of_week = factor(dataset$day_of_week,
                             levels = (unique(dataset$day_of_week)),
                             labels = (seq(1, length(
                               unique(dataset$day_of_week)
                             ), by = 1)))
sum(is.na(dataset$day_of_week))

#poutcome
dataset$poutcome = factor(dataset$poutcome,
                          levels = (unique(dataset$poutcome)),
                          labels = (seq(1, length(
                            unique(dataset$poutcome)
                          ), by = 1)))
sum(is.na(dataset$poutcome))

# y This is the dependent variable
dataset$y= factor(dataset$y,
                  levels = c('yes','no'),
                  labels =c(1,0) 
)


IV <- create_infotables(data=dataset, 
                        valid=dataset$y,
                        y="yes",
                        parallel=FALSE)
sum(is.na(dataset$y))

#summary(dataset)

#Splitting Data into test set and training set
library(caTools)
set.seed(123)
split=sample.split(dataset$y,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)



#Performing Feature Scaling for Numerical Method
## Now we scale the numeric variables and fit the training set
training_set$campaign=scale(training_set$campaign)
training_set$emp.var.rate=scale(training_set$emp.var.rate)
training_set$cons.price.idx=scale(training_set$cons.price.idx)
training_set$cons.conf.idx=scale(training_set$cons.conf.idx)


#For Testing Set
testing_set$campaign=scale(testing_set$campaign)
testing_set$emp.var.rate=scale(testing_set$emp.var.rate)
testing_set$cons.price.idx=scale(testing_set$cons.price.idx)
testing_set$cons.conf.idx=scale(testing_set$cons.conf.idx)




#Creating the classifier


classifier <- glm(formula=y~.,data=training_set,family=binomial())

summary(classifier)

#Fitting the Data in the model
summary(training_set)

summary(testing_set)

prob_predict=predict(classifier,type = 'response',newdata = testing_set[-10])

prob_predict

y_pred=ifelse(prob_predict>0.5,1,0)
#Checking the Confusion Matrix
library(caret)
confusionMatrix(table(y_pred, testing_set$y))


install.packages('Information')
library('Information')

WOE(X=dataset$education,Y=dataset$y)

library(devtools)
install_github("riv","tomasgreif")
library(woe)

iv.mult(dataset,dataset$y,TRUE)

iv.mult(dataset,1)

iv.plot.summary(iv.mult(dataset,"gb"))

summary(german_data)

typeof(dataset)

iv.str(dataset,dataset$education)