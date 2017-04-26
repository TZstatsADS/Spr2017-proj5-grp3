

divide.data<-function(extract.data){
  
  ED<-as.data.table(extract.data)
  ED=ED[,which(unlist(lapply(ED, function(x)!(sum(x %in% c("-9","-8","-7","-6","-5","-3",NA,""))/nrow(ED)>=0.8)))),with=F]
  
  #calculate the unique levels of data to split into categorical data and continuous data
  ED.unique=apply(ED, 2, function(x) uniqueN(x[! x %in% c("-9","-8","-7","-6","-5","-3",NA,"")]))
  
  #change from list to data frame
  ED1=do.call(cbind, lapply(ED, data.frame, stringsAsFactors=FALSE))
  row.names(ED1)=row.names(ED)
  colnames(ED1)=colnames(ED)
  
  #split the data
  ED.categorical=ED1[,ED.unique<=10]
  ED.continuous=ED1[,ED.unique>10]
  
  #replace NA in categorical data wth -3
  ED.categorical[is.na(ED.categorical)==TRUE]<-"-3"
  
  return(list(ED.categorical,ED.continuous))
}


clean.factor<-function(ED.continuous){
  
ED.factor=ED.continuous[,which(unlist(lapply(ED.continuous, is.factor)))]
#data.info$description[data.info$code %in% colnames(ED.factor)]

#manually clean these data
ED.factor[ED.factor==""]<-NA

t1=strsplit(as.character(ED.factor$hv5_ppvtae),"-")
t2=do.call(rbind, lapply(t1, function(x) as.integer(x[1])*12+as.integer(x[2])))
ED.factor$hv5_ppvtae=t2
hv5_ppvtae_isna=numeric(length=nrow(ED.factor))
hv5_ppvtae_isna[is.na(ED.factor$hv5_ppvtae)]=1
ED.factor=cbind(ED.factor,hv5_ppvtae_isna)


t1=strsplit(as.character(ED.factor$hv5_wj9ae),"-")
t2=do.call(rbind, lapply(t1, function(x) as.integer(x[1])*12+as.integer(x[2])))
ED.factor$hv5_wj9ae=t2
hv5_wj9ae_isna=numeric(length=nrow(ED.factor))
hv5_wj9ae_isna[is.na(ED.factor$hv5_wj9ae)]=1
ED.factor=cbind(ED.factor,hv5_wj9ae_isna)


t1=strsplit(as.character(ED.factor$hv5_wj10ae),"-")
t2=do.call(rbind, lapply(t1, function(x) as.integer(x[1])*12+as.integer(x[2])))
ED.factor$hv5_wj10ae=t2
hv5_wj10ae_isna=numeric(length=nrow(ED.factor))
hv5_wj10ae_isna[is.na(ED.factor$hv5_wj10ae)]=1
ED.factor=cbind(ED.factor,hv5_wj10ae_isna)


hv5_ppvtpr_isna=numeric(length=nrow(ED.factor))
hv5_ppvtpr_isna[ED.factor$hv5_ppvtpr=="Other"]=2
hv5_ppvtpr_isna[is.na(ED.factor$hv5_ppvtpr)]=1
ED.factor$hv5_ppvtpr[ED.factor$hv5_ppvtpr=="Other"]=NA
ED.factor$hv5_ppvtpr=unfactor(ED.factor$hv5_ppvtpr)
ED.factor=cbind(ED.factor,hv5_ppvtpr_isna)


hv5_wj9pr_isna=numeric(length=nrow(ED.factor))
hv5_wj9pr_isna[ED.factor$hv5_wj9pr=="Other"]=2

hv5_wj9pr_isna[is.na(ED.factor$hv5_wj9pr)]=1
ED.factor$hv5_wj9pr[ED.factor$hv5_wj9pr=="Other"]=NA
ED.factor$hv5_wj9pr=unfactor(ED.factor$hv5_wj9pr)
ED.factor=cbind(ED.factor,hv5_wj9pr_isna)

hv5_wj10pr_isna=numeric(length=nrow(ED.factor))
hv5_wj10pr_isna[ED.factor$hv5_wj10pr=="Other"]=2
hv5_wj10pr_isna[ED.factor$hv5_wj10pr=="<0.1"]=3
hv5_wj10pr_isna[is.na(ED.factor$hv5_wj10pr)]=1
ED.factor$hv5_wj10pr[ED.factor$hv5_wj10pr=="Other"]=NA
ED.factor$hv5_wj10pr=unfactor(ED.factor$hv5_wj10pr)
ED.factor=cbind(ED.factor,hv5_wj10pr_isna)
ED.factor$hv5_wj10pr[hv5_wj10pr_isna==3]<-0.1

hv5_dsae_isna=numeric(length=nrow(ED.factor))
hv5_dsae_isna[ED.factor$hv5_dsae==">16:10"]=2
hv5_dsae_isna[ED.factor$hv5_dsae=="<6:2"]=3
hv5_dsae_isna[is.na(ED.factor$hv5_dsae)]=1

t1=str_replace(as.character(ED.factor$hv5_dsae), c("<",">"), "")
t1=strsplit(t1,":")
t2=do.call(rbind, lapply(t1, function(x) as.integer(x[1])*12+as.integer(x[2])))
ED.factor$hv5_dsae=t2
ED.factor=cbind(ED.factor,hv5_dsae_isna)

return(ED.factor)
}

clean.continuous<-function(ED.cont2){
  
  #add a binary indicator to indicate if NA
  ED.indina=apply(ED.cont2,c(1,2), function(x) 
    if(any(x %in% c("-14","-10","-9","-8","-7","-6","-5","-3","-2","-1",NA,""))) {x}else{0} )
  
  ED.indina[is.na(ED.indina)]="-3"
  ED.indina=ED.indina[,colSums(ED.indina!=0)>0]
  ED.indina=as.data.frame(ED.indina)
  colnames(ED.indina)<-paste0(colnames(ED.indina),"_isna")
  
  
  ED.cont2[ED.cont2=="-9"|ED.cont2=="-6"]<-NA
  ED.cont2[ED.cont2=="-8"|ED.cont2=="-7"]<-NA
  ED.cont2[ED.cont2=="-5"|ED.cont2=="-3"]<-NA
  ED.cont2[ED.cont2=="-2"|ED.cont2=="-1"]<-NA
  ED.cont2[ED.cont2=="-14"|ED.cont2=="-10"]<-NA
  ED.cont2[ED.cont2==""]<-NA
  
  ED.cont2=cbind(ED.cont2,ED.indina)
  
  return(ED.cont2)
  
}
