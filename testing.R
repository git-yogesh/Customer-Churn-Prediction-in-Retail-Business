print("Testing models over data...")
#Cross validation results
results <- resamples(list(RF=modelrf, XGB=modelxgb, SVM=modelsvm))
summary(results)
dotplot(results)

###Testing
#RF
testPred <- predict(modelrf, dataTest)
postResample(testPred, dataTest$churn)
sensitivity(testPred, dataTest$churn)
confusionMatrix(testPred, dataTest$churn)
rfTable=data.frame(Observed=dataTest$churn,Predicted=testPred)
rfAcc = sum(testPred==dataTest$churn)/length(dataTest$churn)
rfErr=1-rfAcc
print(paste("RF Accuracy: ", rfAcc))

#SVM
testPred <- predict(modelsvm, dataTest)
postResample(testPred, dataTest$churn)
sensitivity(testPred, dataTest$churn)
confusionMatrix(testPred, dataTest$churn)
svmTable=data.frame(Observed=dataTest$churn,Predicted=testPred)
svmAcc = sum(testPred==dataTest$churn)/length(dataTest$churn)
svmErr=1-svmAcc
print(paste("SVM Accuracy: ", svmAcc))

#XGB
testPred <- predict(modelxgb, dataTest)
postResample(testPred, dataTest$churn)
sensitivity(testPred, dataTest$churn)
confusionMatrix(testPred, dataTest$churn)
xgbTable=data.frame(Observed=dataTest$churn,Predicted=testPred)
xgbAcc = sum(testPred==dataTest$churn)/length(dataTest$churn)
xgbErr=1-xgbAcc
print(paste("XGB Accuracy: ", xgbAcc))

print("Finshed testing. Check on the right for results.")
