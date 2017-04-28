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
model_selection_con<-function(train, test, y){
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  seed <- 7
  metric <- "RMSE"
  # Linear Regression
  lin.reg <- lm(y~.,data = train)
  test.pred.lin <- predict(lin.reg,test[,-1])
  linErr<-Error(test, test.pred.lin)
  # rpart function applied to a numeric variable => regression tree
  rt <- rpart(y~., data=train, method = "anova")
  test.pred.rtree <- predict(rt,test[,-1]) 
  treeErr<-Error(test, test.pred.rtree)
  min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
  # use it to prune the tree
  rt.pruned <- prune(rt,cp = min.xerror) 
  test.pred.rtree.p <- predict(rt.pruned,test[,-1])
  pruneErr<-Error(test, test.pred.rtree.p) 
  # Conditional inference trees via party
  ct<-ctree(y~., data = train)
  test.ct<-predict(ct, test[,-1])
  ctErr<-Error(test, test.ct) 
  # Random Forest
  rf <- train(x=train, y=y, method="rf", trControl=control, metric=metric, verbose=FALSE)
  test.rf<-predict(rf, test[,-1])
  rfErr<-Error(test, test.rf) 
  print(2)
  # # gamboostLSS
  # lmLSS <-  train(x=train, y=y, method="gamboost", trControl=control, metric=metric, verbose=FALSE)
  # test.pred.gam<- predict(lmLSS,test[,-1])
  # gamErr<-Error(test, test.pred.gam[[1]])
  # SVM
  svm_fit <- train(x=train, y=y, method="svmRadial", metric=metric, trControl=control, verbose=FALSE)
  svm_predictions<-predict(svm_fit,newdata=test[,-1])
  svmErr<-Error(test, svm_predictions)
  # Linear Bagging
  length_divisor<-6
  iterations<-5000
  predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
    training_positions <- sample(nrow(train), size=floor((nrow(train)/length_divisor)))
    train_pos<-1:nrow(train) %in% training_positions
    lm_fit<-lm(y[train_pos]~.,data=train[train_pos,])
    predict(lm_fit,newdata=test[,-1])
  }
  lm_predictions<-rowMeans(predictions)
  lmbagErr<-Error(test, lm_predictions)
  #Ensemble Linear Regression and Random Forest
  predictions<-(lm_predictions+test.rf)/2
  lmrfErr<-Error(test, predictions)
  # Ensemble SVM and Random Forest
  predictions<-(svm_predictions+test.rf)/2
  svmrfErr<-Error(test, predictions)
  # Ensemble--Boosting
  # Example of Boosting Algorithms
  # Stochastic Gradient Boosting
  set.seed(seed)
  fit.gbm <- train(x=train, y=y, method="gbm", metric=metric, trControl=control, verbose=FALSE)
  gbm_prediction<-predict(fit.gbm, test[,-1])
  gbmErr<-Error(test, gbm_prediction)
  # Model Comparision
  # Create a data frame with the error metrics for each method
  accuracy <- data.frame(Method = c("Linear Regression","Full tree","Pruned tree","Random Forest", "Conditional inference trees", "Gradient Boosting", "Support Vector Machine", "LM+RF", "SVM+RF"),
                         Test.Error = c(linErr,treeErr,pruneErr, rfErr, ctErr,gbmErr, svmErr, lmrfErr, svmrfErr))
  
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
  # Linear Regression
  lin.reg <- train(y=factor(y), x=train, method="glm", metric=metric, trControl=control)
  test.pred.lin <- predict(lin.reg,test[,-1])
  linErr<-Error2(test, test.pred.lin)
  # rpart function applied to a numeric variable => regression tree
  rt <- rpart(y~., data=train, method = "class")
  test.pred.rtree <- ifelse(predict(rt,test[,-1])[,1]>=predict(rt,test[,-1])[,2], 0, 1)
  treeErr<-Error2(test, test.pred.rtree)
  min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
  # use it to prune the tree
  rt.pruned <- prune(rt,cp = min.xerror) 
  test.pred.rtree.p <- ifelse(predict(rt.pruned,test[,-1])[,1]>=predict(rt.pruned,test[,-1])[,2], 0, 1)
  pruneErr<-Error2(test, test.pred.rtree.p) 
  # Conditional inference trees via party
  ct<-ctree(y~., data = train)
  test.ct<-predict(ct, test[,-1])
  ctErr<-Error2(test, test.ct) 
  # Random Forest
  rf <- train(x=train, y=y, method="rf", trControl=control, metric=metric, verbose=FALSE)
  test.rf<-predict(rf, test[,-1])
  rfErr<-Error2(test, test.rf) 
  # SVM
  svm_fit <- train(x=train, y=y, method="svmRadial", metric=metric, trControl=control, verbose=FALSE)
  svm_predictions<-predict(svm_fit,newdata=test[,-1])
  svmErr<-Error2(test, svm_predictions)
  # Stochastic Gradient Boosting
  fit.gbm <- train(x=train, y=y, method="gbm", metric=metric, trControl=control, verbose=FALSE)
  gbm_prediction<-predict(fit.gbm, test[,-1])
  gbmErr<-Error2(test, gbm_prediction)
  # C5.0
  fit.c50 <- train(x=train, y=y, method="C5.0", metric=metric, trControl=control)
  c50_prediction<-predict(fit.c50 , test[,-1])
  c50Err<-Error2(test, c50_prediction)
  # lda
  fit.lda <- train(x=train, y=factor(y), method="lda", metric=metric, trControl=control)
  lda_prediction<-predict(fit.lda, test[,-1])
  ldaErr<-Error2(test, lda_prediction)
  # knn
  fit.knn <- train(x=train, y=factor(y), method="knn", metric=metric, trControl=control)
  knn_prediction<-predict(fit.knn, test[,-1])
  knnErr<-Error2(test, knn_prediction)
  
  # Model Comparision
  # Create a data frame with the error metrics for each method
  accuracy <- data.frame(Method = c("glm","Full tree","Pruned tree","Random Forest", "Conditional inference trees", "Gradient Boosting", "Support Vector Machine", "C5.0", "LDA", "KNN"),
                         Test.Error = c(linErr,treeErr,pruneErr, rfErr, ctErr, gbmErr, svmErr, c50Err, ldaErr, knnErr))
  
  # Round the values and print the table
  accuracy$Test.Error <- round(accuracy$Test.Error,4)
  return(accuracy)
}
