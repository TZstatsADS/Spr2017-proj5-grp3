library(rpart)
library(party)
library(randomForest)
library(gamboostLSS)
library(e1071)
library(foreach)
library(mlbench)
library(caret)

# Error Function, Evaluation method
Error<-function(test, pred){
  return(mean((test[,1]-pred)^2))
}

Error2<-function(test, pred){
  return(mean(test[,1]!=pred))
}

# Model Selection for Continuous response variables
model_selection_con<-function(train,test,y){
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  seed <- 7
  metric <- "RMSE"
  # rpart function applied to a numeric variable => regression tree
  rt <- rpart(y~., data=train[,-1], method = "anova")
  test.pred.rtree <- predict(rt,test[,-1]) 
  treeErr<-Error(test, test.pred.rtree)
  min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
  
  # use it to prune the tree
  rt.pruned <- prune(rt,cp = min.xerror) 
  test.pred.rtree.p <- predict(rt.pruned,test[,-1])
  pruneErr<-Error(test, test.pred.rtree.p) 
  
  # Conditional inference trees via party
  ct<-ctree(y~., data = train[,-1])
  test.ct<-predict(ct, test[,-1])
  ctErr<-Error(test, test.ct) 
  
  # Random Forest
  rf <- train(x=train[,-1], y=y, method="rf", trControl=control, metric=metric, verbose=FALSE)
  test.rf<-predict(rf, test[,-1])
  rfErr<-Error(test, test.rf) 

  # Stochastic Gradient Boosting
  set.seed(seed)
  fit.gbm <- train(x=train[,-1], y=y, method="gbm", metric=metric, trControl=control, verbose=FALSE)
  gbm_prediction<-predict(fit.gbm, test[,-1])
  gbmErr<-Error(test, gbm_prediction)
  # Model Comparision
  # Create a data frame with the error metrics for each method
  accuracy <- data.frame(Method = c("Full tree","Pruned tree","Random Forest", "Conditional inference trees", "Gradient Boosting"),
                         Test.Error = c(treeErr,pruneErr, rfErr, ctErr, gbmErr))
  
  # Round the values and print the table
  accuracy$Test.Error <- round(accuracy$Test.Error,4)
  return(accuracy)
}

# Model Selection for Categorical response variables
model_selection_cat<-function(train, test, y){

  
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  seed <- 7
  metric <- "Accuracy"
  set.seed(seed)

   # rpart function applied to a numeric variable => regression tree
  rt <- rpart(y~., data=train[,-1], method = "class")
  test.pred.rtree <- ifelse(predict(rt,test[,-1])[,1]>=predict(rt,test[,-1])[,2], 0, 1)
  treeErr<-Error2(test, test.pred.rtree)
  min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
  # use it to prune the tree
  rt.pruned <- prune(rt,cp = min.xerror) 
  test.pred.rtree.p <- ifelse(predict(rt.pruned,test[,-1])[,1]>=predict(rt.pruned,test[,-1])[,2], 0, 1)
  pruneErr<-Error2(test, test.pred.rtree.p) 
  # Conditional inference trees via party
  ct<-ctree(y~., data = train[,-1])
  test.ct<-predict(ct, test[,-1])
  ctErr<-Error2(test, test.ct) 
  # Random Forest
  rf <- train(x=train[,-1], y=y, method="rf", trControl=control, metric=metric, verbose=FALSE)
  test.rf<-predict(rf, test[,-1])
  rfErr<-Error2(test, test.rf) 
  # SVM
  #svm_fit <- train(x=train[,-1], y=y, method="svmRadial", metric=metric, trControl=control, verbose=FALSE)
  #svm_predictions<-predict(svm_fit,newdata=test[,-1])
  #svmErr<-Error2(test, svm_predictions)
  # Stochastic Gradient Boosting
  fit.gbm <- train(x=train[,-1], y=y, method="gbm", metric=metric, trControl=control, verbose=FALSE)
  gbm_prediction<-predict(fit.gbm, test[,-1])
  gbmErr<-Error2(test, gbm_prediction)
  # C5.0
#  fit.c50 <- train(x=train[,-1], y=y, method="C5.0", metric=metric, trControl=control)
#  c50_prediction<-predict(fit.c50 , test[,-1])
#  c50Err<-Error2(test, c50_prediction)
  # lda
 # fit.lda <- train(x=train[,-1], y=factor(y), method="lda", metric=metric, trControl=control)
  #lda_prediction<-predict(fit.lda, test[,-1])
 # ldaErr<-Error2(test, lda_prediction)
  # knn
  fit.knn <- train(x=train[,-1], y=factor(y), method="knn", metric=metric, trControl=control)
  knn_prediction<-predict(fit.knn, test[,-1])
  knnErr<-Error2(test, knn_prediction)
  
  # Model Comparision
  # Create a data frame with the error metrics for each method
  accuracy <- data.frame(Method = c("Full tree","Pruned tree","Random Forest", "Conditional inference trees", "Gradient Boosting", "KNN"),
                         Test.Error = c(treeErr,pruneErr, rfErr, ctErr, gbmErr, knnErr))
  
  # Round the values and print the table
  accuracy$Test.Error <- round(accuracy$Test.Error,4)
  return(accuracy)
}
