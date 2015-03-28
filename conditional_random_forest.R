#load dependencies
library(party)

#load data
train = read.csv("Data/train.csv", header = TRUE, stringsAsFactors = FALSE)
test = read.csv("Data/test.csv", header = TRUE, stringsAsFactors = FALSE)

# make city group string factors
train$city_group = as.factor(train$City.Group)
test$city_group = as.factor(test$City.Group)

# parse days open from start of competetion
competition_start <- strptime('23.03.2015', format='%d.%m.%Y')
train$days_open <- as.numeric(difftime(competition_start, strptime(train$Open.Date, format='%m/%d/%Y'), units='days'))
test$days_open <- as.numeric(difftime(competition_start, strptime(test$Open.Date, format='%m/%d/%Y'), units='days'))

# remove unused columns
train$City <- NULL
train$Open.Date <- NULL
train$City.Group <- NULL
test$City <- NULL
test$Open.Date <- NULL
test$City.Group <- NULL

train$Type = as.factor(train$Type)
test$Type[test$Type == "MB"] <- NA
test$Type = as.factor(test$Type)

rf = cforest(revenue ~., data = train[,-1], controls=cforest_unbiased(ntree=2000))

Prediction = predict(rf, test[,-1], OOB=TRUE, type = "response")

id<-test[,1]
submission<-cbind(id,Prediction)
colnames(submission)[2] <- "Prediction"

write.csv(submission, "Output/conditional_forest_days_open_ntree_2000.csv", row.names = FALSE, quote = FALSE)
