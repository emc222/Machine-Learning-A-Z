#Logistict Regression Data of  Portuguese banking institution

#Importing Dataset
dataset <- read.table("bank-additional-full.csv",
                      sep = ";",
                      header = T)

#Checking for Missing Data in the dataset
which(is.na(dataset))
colSums(is.na(dataset))

# sum(dataset$marital=='unknown')
#
#
library(ggplot2)
ggplot(data.frame(dataset$job), aes(x=dataset$job)) +
  geom_bar()

#Encoding the Categorical Data
#Job
dataset$job = factor(dataset$job,
                     levels = c('admin.','blue-collar','entrepreneur',
                                'housemaid','management','retired','self-employed','services',
                                'student','technician','unemployed','unknown'),
                     labels =c(1,2,3,4,5,6,7,8,9,10,11,12))
sum(is.na(dataset$job))

dataset<-transform(dataset,job=ifelse(job==12,1,job))

#marital
dataset$marital = factor(dataset$marital,
                         levels = c('divorced','married','single','unknown'),
                         labels =c(1,2,3,4) 
                           )
sum(is.na(dataset$marital))

#Plotting the catagorical data
ggplot(data.frame(dataset$marital), aes(x=dataset$marital)) +
  geom_bar()

dataset<-transform(dataset,marital=ifelse(marital==4,1,marital))
#education
ggplot(data.frame(dataset$education), aes(x=dataset$education)) +
  geom_bar()

dataset$education = factor(dataset$education,
                           levels = c('basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course',
                                      'university.degree','unknown'),
                           labels = (seq(1, length(
                             unique(dataset$education)
                           ), by = 1)))
sum(is.na(dataset$education))

#default
ggplot(data.frame(dataset$default), aes(x=dataset$default)) +
  geom_bar()

dataset$default = factor(dataset$default,
                         levels = (unique(dataset$default)),
                         labels = (seq(1, length(
                           unique(dataset$default)
                         ), by = 1)))
sum(is.na(dataset$default))


#housing
ggplot(data.frame(dataset$housing), aes(x=dataset$housing)) +
  geom_bar()
sum(dataset$housing=='unknown')
#990
dataset$housing = factor(dataset$housing,
                         levels = (unique(dataset$housing)),
                         labels = (seq(1, length(
                           unique(dataset$housing)
                         ), by = 1)))
sum(is.na(dataset$housing))

#loan
ggplot(data.frame(dataset$loan), aes(x=dataset$loan)) +
  geom_bar()
sum(dataset$loan=='unknown')
dataset$loan = factor(dataset$loan,
                      levels = (unique(dataset$loan)),
                      labels = (seq(1, length(
                        unique(dataset$loan)
                      ), by = 1)))
sum(is.na(dataset$loan))

#contact
dataset$contact = factor(dataset$contact,
                         levels = (unique(dataset$contact)),
                         labels = (seq(1, length(
                           unique(dataset$contact)
                         ), by = 1)))
sum(is.na(dataset$contact))

#month
# unique(dataset$month)
dataset$month = factor(dataset$month,
                       levels = (unique(dataset$month)),
                       labels = (seq(1, length(
                         unique(dataset$month)
                       ), by = 1)))
sum(is.na(dataset$month))

#dayoftheweek
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


#y
dataset$y = factor(dataset$y,
                   levels = (unique(dataset$y)),
                   labels = (seq(1, length(unique(
                     dataset$y
                   )), by = 1)))
sum(is.na(dataset$y))

#Feature Scaling and Transformation
#Transformation
#Looking at the dataset summary we can transform cons.price.idx field.
dataset$cons.price.idx<-dataset$cons.price.idx-min(dataset$cons.price.idx)

#Let's First Try Without Feature Scaling

#Splitting Data in Training Set and Testing Set
library(caTools)
set.seed(123)
split=sample.split(dataset$y,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

#Fitting the Training Set in the classifier
#classifier=glm(formula = y~.,family = binomial,data =training_set)


#Feature Scaling
#For Training Set
training_set$age=scale(training_set$age)
training_set$campaign=scale(training_set$campaign)
training_set$pdays=scale(training_set$pdays)
training_set$previous=scale(training_set$previous)
training_set$emp.var.rate=scale(training_set$emp.var.rate)
training_set$cons.price.idx=scale(training_set$cons.price.idx)
training_set$cons.conf.idx=scale(training_set$cons.conf.idx)
training_set$euribor3m=scale(training_set$euribor3m)
training_set$nr.employed=scale(training_set$nr.employed)

#For Testing Set
testing_set$age=scale(testing_set$age)
testing_set$campaign=scale(testing_set$campaign)
testing_set$pdays=scale(testing_set$pdays)
testing_set$previous=scale(testing_set$previous)
testing_set$emp.var.rate=scale(testing_set$emp.var.rate)
testing_set$cons.price.idx=scale(testing_set$cons.price.idx)
testing_set$cons.conf.idx=scale(testing_set$cons.conf.idx)
testing_set$euribor3m=scale(testing_set$euribor3m)
testing_set$nr.employed=scale(testing_set$nr.employed)

classifier=glm(formula = y~.,family = binomial,data =training_set)
#install.packages('pedometrics')
library(pedometrics)
require(car)
stepVIF(classifier, threshold = 10, verbose = TRUE)

unique(dataset$campaign)

unique(dataset$nr.employed)

sum(dataset$pdays==999)
# glm.probs = predict(classifier, newdata = training_set, type = "response")
# glm.pred = ifelse(glm.probs > 0.5, 1, 0)
# table(glm.pred)

classifier_a=glm(formula = y~age+campaign+pdays+previous+emp.var.rate+cons.price.idx+cons.conf.idx+nr.employed,family = binomial,data =training_set)


#Computing Variance Inflation Factor (VIF) for checking MultiCollinearity
install.packages('car')
library(car)


classifier_a=glm(formula = y~age+campaign+pdays+previous+cons.price.idx+cons.conf.idx,family = binomial,data =training_set)
