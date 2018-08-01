library(Information)
options(scipen=10)

### Loading the data
data(train, package="Information")
data(valid, package="Information")

dataSet <- read.table("bank-additional-full.csv",
                      sep = ";",
                      header = T)

str(dataSet)
table(dataSet$y)

dataset$y= factor(dataset$y,
                  levels = c('yes','no'),
                  labels =c(1,0) 
)
bins <- 10
q <- quantile(dataSet$age,probs = c(1:(bins – 1)/bins),na.rm = TRUE,type = 3)

q<-quantile(dataset$age,probs = c(1:(bins-1)/bins))

cuts <- unique(q)

aggAge <- table(findInterval(dataSet$age,vec = cuts,rightmost.closed = FALSE),dataSet$y)

infoTables <- create_infotables(data = dataSet,
                                
                                y = “y”,
                                bins = 10,
                                parallel = T)

# – WOE table:
infoTables$Tables$age$WOE

infoTables<-create_infotables(data = dataset,y='y',bins = 10,parallel = TRUE)