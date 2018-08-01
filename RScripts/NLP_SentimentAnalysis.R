#Importing Dataset
dataset_original = read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE)


#Cleaning the texts
library(tm)
library(SnowballC)
corpus=VCorpus(VectorSource(dataset_original$Review))
#Converting to lower case
corpus=tm_map(corpus,content_transformer(tolower))
#Removing Numbers
corpus=tm_map(corpus,removeNumbers)
#Removing Punctuation
corpus=tm_map(corpus,removePunctuation)
#Removing stopwords
corpus=tm_map(corpus,removeWords,stopwords())
#Converting words to base form eg loved to love
corpus=tm_map(corpus,stemDocument)
corpus=tm_map(corpus,stripWhitespace)

#Creating Bag of Words MOdel.  
#Each unique words is placed in column and each row contains number of time
#words appear in each row

dtm=DocumentTermMatrix(corpus)
dtm=removeSparseTerms(dtm,0.99)

dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_original$Liked

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[,692], y_pred)