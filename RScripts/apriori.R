#install.packages('arules')
library(arules)

#We need to convert the orignal dataset into Sparse Matrix. So we use arules function 
#read.transactions

#Importing Dataset
dataset<-read.transactions('Market_Basket_Optimisation.csv',sep = ",",rm.duplicates = TRUE)
summary(dataset)

#Plotting the 100 Most Frequent Items 
itemFrequencyPlot(dataset,topN=100)

#Plotting the 10 Most Frequent Items 
itemFrequencyPlot(dataset,topN=10)

#Fitting the data in apriori algorithm
rules=apriori(data = dataset,parameter = list(support=0.004,confidence=0.2))

#Visualising the Rules
inspect(sort(rules,by='lift')[1:10])