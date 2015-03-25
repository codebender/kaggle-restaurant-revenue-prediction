#load dependencies
library(party)

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
test$Type[test$Type == "MB"] <- NA
test$Type = as.factor(test$Type)

rf = cforest(revenue ~., data = train[,-1], controls=cforest_unbiased(ntree=4000))

Prediction = predict(rf, test[,-1], OOB=TRUE, type = "response")

id<-test[,1]
submission<-cbind(id,Prediction)
colnames(submission)[2] <- "Prediction"

write.csv(submission, "Output/conditional_forest_mb_set_to_na_ntree_2000.csv", row.names = FALSE, quote = FALSE)
