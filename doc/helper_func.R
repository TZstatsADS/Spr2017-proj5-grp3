
get.error<-function(f){

  source("../lib/helper_modelFunc.R")
  data.filtered <- read.csv('../data/NAreplaced.csv') #4242 1388
  select <- read.csv(paste0('../data/Updated_Features/',f,'_features.csv'),stringsAsFactors = FALSE)
  data.filtered <- subset(data.filtered,select=c("challengeID",select$Codes)) # 4242*64
  
  label <- read.csv('../data/train.csv')
  label<-na.omit(label)
  label<-subset(label,select=c("challengeID",f))
  
  Index<-data.filtered$challengeID %in% label$challengeID
  
  data.train<-data.filtered[Index,]
  data.train<-as.data.frame(data.train)
  data.train<-cbind(label[,-1], data.train[,-1])
  colnames(data.train)[1]<-f

  
  load("~/GitHub/Spr2017-proj5-grp3/data/categorical.RData")
  cat=select$Codes[select$Codes %in% categorical]
  data.train[,cat]=lapply(data.train[,cat],factor)
  # create training and test data set
  #set.seed(123)
  train.index <- sample(1:nrow(data.train),800,replace = F)
  train <- data.train[train.index,] #800*64
  test <- data.train[-train.index,] #214*64
  
  
  for(i in cat){
    for(j in 1:nrow(test)){
      t=unique(train[,i])
      if(!test[j,i] %in% t){
        test[j,i]=t[sample(1:length(t),1)]
      }
    }
  }
  

  
  if(f %in% c("gpa","grit","materialHardship")){
    y=train[,1]
  tep=model_selection_con(train,test,y)
  }else{
    y=factor(train[,1])
    tep=model_selection_cat(train,test,y)
  }
  tep
  save(tep,file=paste0("../data/",f,"_model.RData"))
}