#Logistic Regression on the Bank Marketing Dataset

#Importing the dataset. 
dataset <- read.table("bank-additional-full.csv",
                      sep = ";",
                      header = T)

#Now First Have a Sneak Peek in the imported data
summary(dataset)

unique(dataset$pdays)
qplot(dataset$cons.price.idx,geom = "histogram")

unique(dataset$pdays)
qplot(dataset$cons.price.idx,geom = "histogram")

#Dropping the Numerical Columns that we do not need. The conclusion to drop the table was 
#obtained from performing VIF using numeric data only
dataset=subset(dataset, select=-c(pdays,duration,euribor3m,nr.employed))


# Things Observed
# 1. The count of yes in default is just 3 so we can drop rows with yes in default
# 2. The count of unknown for marital is just 80 so we can drop rows with unknown in marital
#X[X$Variable1!=11 & X$Variable1!=12, ]
#myData = myData[!myData$A > 4,] # equal to myData[myData$A <= 4,]
# dataset=dataset[!dataset$default=='yes']

#r<-with(dataset,which(default=='yes',arr.ind = TRUE))
#dataset<-dataset[-r,]

dataset=subset(dataset,default!='yes')
dataset=subset(dataset,marital!='unknown')
summary(dataset)

#Encoding the catagorical variables
#job
dataset$job = factor(dataset$job,
                         levels = (unique(dataset$job)),
                         labels = (seq(1, length(
                           unique(dataset$job)
                         ), by = 1)))
sum(is.na(dataset$job))

#marital
dataset$marital = factor(dataset$marital,
                     levels = (unique(dataset$marital)),
                     labels = (seq(1, length(
                       unique(dataset$marital)
                     ), by = 1)))
sum(is.na(dataset$marital))

#education
dataset$education = factor(dataset$education,
                         levels = (unique(dataset$education)),
                         labels = (seq(1, length(
                           unique(dataset$education)
                         ), by = 1)))
sum(is.na(dataset$education))
summary(dataset)

#default
dataset$default=factor(dataset$default,
                       levels = (unique(dataset$default)),
                       labels = (seq(1,length(unique(dataset$default)),by=1))
                       )
sum(is.na(dataset$default))
summary(dataset)

#housing
dataset$housing=factor(dataset$housing,
                       levels = (unique(dataset$housing)),
                       labels = (seq(1,length(unique(dataset$housing)),by=1))
)
sum(is.na(dataset$housing))
summary(dataset)

#loan
dataset$loan = factor(dataset$loan,
                     levels = (unique(dataset$loan)),
                     labels = (seq(1, length(
                       unique(dataset$loan)
                     ), by = 1)))
sum(is.na(dataset$loan))
summary(dataset)

#contact
dataset$contact = factor(dataset$contact,
                      levels = (unique(dataset$contact)),
                      labels = (seq(1, length(
                        unique(dataset$contact)
                      ), by = 1)))
sum(is.na(dataset$contact))
summary(dataset)

#Month
dataset$month = factor(dataset$month,
                         levels = (unique(dataset$month)),
                         labels = (seq(1, length(
                           unique(dataset$month)
                         ), by = 1)))
sum(is.na(dataset$month))
summary(dataset)

#day_of_week
dataset$day_of_week = factor(dataset$day_of_week,
                       levels = (unique(dataset$day_of_week)),
                       labels = (seq(1, length(
                         unique(dataset$day_of_week)
                       ), by = 1)))
sum(is.na(dataset$day_of_week))
summary(dataset)

#poutcome
dataset$poutcome = factor(dataset$poutcome,
                             levels = (unique(dataset$poutcome)),
                             labels = (seq(1, length(
                               unique(dataset$poutcome)
                             ), by = 1)))
sum(is.na(dataset$poutcome))
summary(dataset)

# y This is the dependent variable
dataset$y= factor(dataset$y,
                  levels = c('yes','no'),
                  labels =c(1,2) 
                    )
sum(is.na(dataset$y))
summary(dataset)

#Now Let's take a look at the numeric variableaa

#First we will not perform any scaling and transform any of the numeric column. 

#Splitting the Data in Test and Traing set. 

library(caTools)
set.seed(123)
split=sample.split(dataset$y,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

classifier=glm(formula = y~.,family = binomial,data =training_set)

summary(classifier)

prob_predict=predict(classifier,type = 'response',newdata = testing_set[-21])

y_pred=ifelse(prob_predict>0.5,1,2)


#install.packages('caret')
library(caret)
confusionMatrix(table(y_pred, testing_set$y))

library(car)
classifier=glm(formula = y~age+previous+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed,
               family =binomial,
               data = training_set
               )
summary(classifier)

library(car)
vif(classifier)



classifier=glm(formula = y~age+previous+emp.var.rate+cons.price.idx+cons.conf.idx+nr.employed,
               family =binomial,
               data = training_set
)


##Removing nr.employed
classifier=glm(formula = y~age+previous+emp.var.rate+cons.price.idx+cons.conf.idx,
               family =binomial,
               data = training_set
)



#Now Fitting the formula after removing some nuneric variables
classifier=glm(formula = y~age+job+marital+education+default+housing+loan+contact+
                 month+day_of_week+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,
               family =binomial,
               data = training_set
)

summary(classifier)

prob_predict=predict(classifier,type = 'response',newdata = testing_set[-20])

y_pred=ifelse(prob_predict>0.5,1,2)


#install.packages('caret')
library(caret)
confusionMatrix(table(y_pred, testing_set$y))

## Now we scale the numeric variables and fit the training set
training_set$age=scale(training_set$age)
training_set$campaign=scale(training_set$campaign)
training_set$previous=scale(training_set$previous)
training_set$emp.var.rate=scale(training_set$emp.var.rate)
training_set$cons.price.idx=scale(training_set$cons.price.idx)
training_set$cons.conf.idx=scale(training_set$cons.conf.idx)


#For Testing Set
testing_set$age=scale(testing_set$age)
testing_set$campaign=scale(testing_set$campaign)
testing_set$previous=scale(testing_set$previous)
testing_set$emp.var.rate=scale(testing_set$emp.var.rate)
testing_set$cons.price.idx=scale(testing_set$cons.price.idx)
testing_set$cons.conf.idx=scale(testing_set$cons.conf.idx)

#Fitting the Data into Model
classifier=glm(formula = y~age+job+marital+education+default+housing+loan+contact+
                 month+day_of_week+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,
               family =binomial,
               data = training_set
)

summary(classifier)

prob_predict=predict(classifier,type = 'response',newdata = testing_set[-20])
#The predict function gives probability so we need to interpert the probability 
y_pred=ifelse(prob_predict>0.5,1,2)

library(caret)
confusionMatrix(table(y_pred, testing_set$y))
