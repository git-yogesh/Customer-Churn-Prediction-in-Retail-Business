print("Training models...")
set.seed(999)
customerData <- custaggdata[, -c(1,3)]
trainIndex <- createDataPartition(customerData$churn, p = .8,list = FALSE, times = 1)
head(trainIndex)

dataTrain <- customerData[ trainIndex,]
dataTest  <- customerData[-trainIndex,]

library("randomForest")
library("kernlab")
library("xgboost")

#RF
print("RANDOM FOREST...")
startTime <- proc.time()
modelrf <- train(churn ~ ., data = dataTrain, method = "rf", trControl = trainControl(method = "cv", number = 10))
endTime <- proc.time() - startTime
modelrf
confusionMatrix(modelrf)
rfTime <- paste("Finished training RF model in", endTime[1], "seconds. CPU Time =", endTime[2], "seconds.")
print(rfTime)

#SVM
print("SUPPORT VECTOR MACHINE...")
startTime <- proc.time()
modelsvm <- train(churn ~ ., data = dataTrain, method = "svmRadialWeights", trControl = trainControl(method = "cv", number = 10))
endTime <- proc.time() - startTime
modelsvm
confusionMatrix(modelsvm)
svmTime <- paste("Finished training SVM model in", endTime[1], "seconds. CPU Time =", endTime[2], "seconds.")
print(svmTime)

#XGB
print("XTREME GRADIENT BOOSTING...")
startTime <- proc.time()
modelxgb <- train(churn ~ ., data = dataTrain, method = "xgbTree", trControl = trainControl(method = "cv", number = 10))
endTime <- proc.time() - startTime
modelxgb
confusionMatrix(modelxgb)
xgbTime <- paste("Finished training XGB model in", endTime[1], "seconds. CPU Time =", endTime[2], "seconds.")
print(xgbTime)
