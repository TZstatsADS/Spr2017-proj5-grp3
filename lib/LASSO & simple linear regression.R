#------- original code: Mengchen Li in Group3 for project 5-----------#

## 1. Load data
# 1.1 Load background data and codebook
setwd("~/Desktop/Spr2017-proj5-grp3/")
load("data/background.RData")
ff_child_cb9 <- read.csv('data/ff_child_cb9.csv')
ff_dad_cb9 <- read.csv('data/ff_dad_cb9.csv')
ff_mom_cb9 <- read.csv('data/ff_mom_cb9.csv')
ff_teacher_cb9 <- read.csv('data/ff_teacher_cb9.csv')
train <- read.csv('data/train.csv')

# 1.2 all features related to child,dad,mom,teacher
background_child <- subset(background,select = ff_child_cb9$Code)
background_dad <- subset(background,select = ff_dad_cb9$Code)
background_mom <- subset(background,select = ff_mom_cb9$Code)
background_teacher <- subset(background,select = ff_teacher_cb9$Code)

# 1.3 data selection
gpa <- train$gpa
ID <- train$challengeID
data <- cbind(background_child,background_dad,background_mom,background_teacher)
# replace data -9,-6,-3 with NA
data[data==-9] <- NA
data[data==-6] <- NA
data[data==-3] <- NA
data <- cbind(ID,gpa,data)
# select rows with gpa data
data.filtered <- subset(data,!is.na(data$gpa))
# remove all columns containing at least on NA
data.filtered <- data.filtered[,colSums(is.na(data.filtered))==0] # 2330*93
write.csv(data.filtered,file='~/Desktop/Spr2017-proj5-grp3/data/data.filtered.csv')

## 2.Subset Selection
# 2.1 linear regression model before LASSO
names(data.filtered)
# define training and testing data set
train.index <- sample(1:nrow(data.filtered),2000,replace = F)
dat.train <- data.filtered[train.index,]
dat.test <- data.filtered[-train.index,]
# try to fit a linear model
gpa.lm <- lm(gpa~.,data = dat.train)
summary(gpa.lm)
gpa.lm$coefficients
# compute MSPE
gpa.pred.lm <- predict(gpa.lm,dat.test)
mean((gpa.pred.lm-dat.test$gpa)^2)#  0.4365751


# 2.2 LASSO
library(glmnet)
train.x <- as.matrix(dat.train[,3:ncol(dat.train)])
x.sd <- apply(train.x,2,function(train.x)(train.x-mean(train.x))/sd(train.x))
ID.train <- dat.train$ID
x.sd <- data.frame(ID.train,x.sd)#2000*92
x.sd <- x.sd[,colSums(is.na(x.sd))==0]# 2000*89
y <- dat.train$gpa
nfolds <- 10
cv.fit <- cv.glmnet(as.matrix(x.sd),y,nfolds = nfolds)
plot(cv.fit)
# optimal lambda
cv.fit$lambda.min # 0.03161628
coef(cv.fit, lambda = "lambda.min")
predictors <- which(coef(cv.fit, lambda = "lambda.min")[,1] < 0.0001)
predictors
dim(data.filtered[,predictors])
fea_lasso <- data.filtered[,predictors]



