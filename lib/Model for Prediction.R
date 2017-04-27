### Possible Model for Prediction_Mengchen Li ###

setwd("~/Desktop/Spr2017-proj5-grp3/data/")
data.filtered <- read.csv('data.filtered.csv')#2330*94
train.index <- sample(1:nrow(data.filtered),2000,replace = F)
train <- data.filtered[train.index,] #2000*94
test <- data.filtered[-train.index,] #330*94

##------------------------------Model 1: Multiple linear regression---------------------------------##
# train model
lin.reg <- lm(gpa~.,data = train[,3:ncol(data.filtered)])
summary(lin.reg) #Multiple R-squared:0.009817   
# test error
test.pred.lin <- predict(lin.reg,test[,3:ncol(data.filtered)])
RMSE.lin.reg <- sqrt(mean((test.pred.lin-test$gpa)^2))                                      
RMSE.lin.reg #0.6455476

MAE.lin.reg <- mean(abs(test.pred.lin-test$gpa))
MAE.lin.reg # 0.524629

# conclusion: R^2 is too low, RMSE is high

##------------------------------Model 2: Decision trees------------------------------------------##
#install.packages('rattle')
#install.packages('rpart.plot')
library(rpart)
library(rattle)
library(rpart.plot)

# train model
# rpart function applied to a numeric variable => regression tree
rt <- rpart(gpa~., data=train[,3:ncol(data.filtered)])
test.pred.rtree <- predict(rt,test[,3:ncol(data.filtered)]) 

# test error
RMSE.rtree <- sqrt(mean((test.pred.rtree-test$gpa)^2))
RMSE.rtree # 0.6365596

MAE.rtree <- mean(abs(test.pred.rtree-test$gpa))
MAE.rtree #0.5188985

# check cross-validation results
printcp(rt) #Root node error: 896.78/2000 = 0.44839

# get the optimal
min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
min.xerror #0.003978073

# use it to prune the tree
rt.pruned <- prune(rt,cp = min.xerror) 

# plot the pruned tree
fancyRpartPlot(rt.pruned)

# evaluate the new pruned tree on the test set
test.pred.rtree.p <- predict(rt.pruned,test[,3:ncol(data.filtered)])
RMSE.rtree.pruned <- sqrt(mean((test.pred.rtree.p-test$gpa)^2))
RMSE.rtree.pruned# 0.6365596
MAE.rtree.pruned <- mean(abs(test.pred.rtree.p-test$gpa))
MAE.rtree.pruned # 0.5188985

# conclusion: result is a little improved. try the new feature data later

##------------------------------Model 3: random forests------------------------------------------##
library(randomForest)

set.seed(123)
# train model: create a random forest with 100 trees
rf <- randomForest(gpa~.,data=train[,3:ncol(data.filtered)], importance = TRUE, ntree=100)

# number of trees needed to reach min error
which.min(rf$mse) # 97
plot(rf)

## Using the importance() function to calculate the importance of each variable
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

# test error
test.pred.forest <- predict(rf,test[,3:ncol(data.filtered)])
RMSE.forest <- sqrt(mean((test.pred.forest-test$gpa)^2))
RMSE.forest #0.6871446

MAE.forest <- mean(abs(test.pred.forest-test$gpa))
MAE.forest # 0.556289

# Conclusion: worse than decision tree model. use imp to select feature??

##------------------------------Model 4: gamboostLSS------------------------------------------##
#install.packages('gamboostLSS')
library('gamboostLSS')

# train model
lmLSS <- glmboostLSS(gpa~., data = train[,3:ncol(data.filtered)])
coef(lmLSS, off2int = TRUE)

# plot mu and sigma
par(mfrow = c(1, 2), mar = c(4, 4, 2, 5))
plot(lmLSS, off2int = TRUE)

# test error for mu
test.pred.gam<- predict(lmLSS,test[,3:ncol(data.filtered)])
RMSE.gam <- sqrt(mean((test.pred.gam[[1]]-test$gpa)^2))
RMSE.gam #0.684915

MAE.gam <- mean(abs(test.pred.forest[[1]]-test$gpa))
MAE.gam # 0.560673

# Conclusion: similar result with full tree method. Try selected feature data later

##------------------------------Model Comparision------------------------------------------##
# Create a data frame with the error metrics for each method
accuracy <- data.frame(Method = c("Linear Regression","Full tree","Pruned tree","Random forest",'gamboostLSS'),
                       RMSE   = c(RMSE.lin.reg,RMSE.rtree,RMSE.rtree.pruned,RMSE.forest,RMSE.gam),
                       MAE    = c(MAE.lin.reg,MAE.rtree,MAE.rtree.pruned,MAE.forest,MAE.gam))

# Round the values and print the table
accuracy$RMSE <- round(accuracy$RMSE,2)
accuracy$MAE <- round(accuracy$MAE,2) 
accuracy
#              Method RMSE  MAE
#1 Linear Regression 0.69 0.56
#2         Full tree 0.68 0.56
#3       Pruned tree 0.68 0.56
#4     Random forest 0.70 0.57
#5       gamboostLSS 0.68 0.56

# create a data frame with the predictions for each method
all.predictions <- data.frame(actual = test$gpa,
                              linear.regression = test.pred.lin,
                              full.tree = test.pred.rtree,
                              pruned.tree = test.pred.rtree.p,
                              random.forest = test.pred.forest,
                              gamboostLSS = test.pred.gam[[1]])

# first six observations
head(all.predictions)
#actual linear.regression full.tree pruned.tree random.forest gamboostLSS
#8    2.50          2.833682     2.863       2.863      2.962708    2.847461
#9    3.25          2.911093     2.863       2.863      3.122250    2.917697
#13   3.00          2.980639     2.863       2.863      2.872333    2.920579
#22   4.00          2.731246     2.863       2.863      2.557583    2.772785
#27   3.50          2.959609     2.863       2.863      2.900750    2.902612
#40   3.50          2.868494     2.863       2.863      2.793042    2.860424


