library(Boruta)
library(caret)

train <- read.csv("Data/train.csv")
test  <- read.csv("Data/test.csv")

n.train <- nrow(train)

test$revenue <- 1
myData <- rbind(train, test)
rm(train, test)

#Tranform Time
myData$Open.Date <- as.POSIXlt("01/01/2015", format="%m/%d/%Y") - as.POSIXlt(myData$Open.Date, format="%m/%d/%Y")
myData$Open.Date <- as.numeric(myData$Open.Date / 1000) #Scale for factors

#Consolidate Cities
myData$City                                      <- as.character(myData$City)
myData$City[myData$City.Group == "Other"]        <- "Other"
myData$City[myData$City == unique(myData$City)[4]] <- unique(myData$City)[2]
myData$City                                      <- as.factor(myData$City)
myData$City.Group                                <- NULL

#Consolidate Types
myData$Type <- as.character(myData$Type)
myData$Type[myData$Type=="DT"] <- "IL"
myData$Type[myData$Type=="MB"] <- "FC"
myData$Type <- as.factor(myData$Type)

#Log Transform P Variables and Revenue
myData[, paste("P", 1:37, sep="")] <- log(1 +myData[, paste("P", 1:37, sep="")])

myData$revenue <- log(myData$revenue)

important <- Boruta(revenue~., data=myData[1:n.train, ])

#Random Forest
set.seed(1337)
model <- train(revenue~., 
               data=myData[1:n.train, c(important$finalDecision != "Rejected", TRUE)])

#Make a Prediction
prediction <- predict(model, myData[-c(1:n.train), ])

#Make Submission
submit<-as.data.frame(cbind(seq(0, length(prediction) - 1, by=1), exp(prediction)))
colnames(submit)<-c("Id","Prediction")
write.csv(submit,"Output/beat_benchmark.csv",row.names=FALSE,quote=FALSE)
