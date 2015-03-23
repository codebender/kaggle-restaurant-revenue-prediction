#load dependencies
library(randomForest)

#load data
train = read.csv("Data/train.csv", header = TRUE, stringsAsFactors = FALSE)
test = read.csv("Data/test.csv", header = TRUE, stringsAsFactors = FALSE)

# make string factors
train$city_group = as.factor(train$City.Group)
test$city_group = as.factor(test$City.Group)

train$City <- NULL
train$Open.Date <- NULL
train$City.Group <- NULL
test$City <- NULL
test$Open.Date <- NULL
test$City.Group <- NULL

train$Type = as.factor(train$Type)
test$Type = as.factor(test$Type)


rf = randomForest(revenue ~.  , data = train[,-1], ntree=500, do.trace=50, importance=TRUE)

Prediction = predict(rf, test[,-1])

id<-test[,1]
submission<-cbind(id,Prediction)

write.csv(submission, "Output/random_forest_ntree_500.csv", row.names = FALSE, quote = FALSE)
