#mature RNA mode prediction
train_lnc=read.csv("/lncRNA-m6A-prediction/exonic_train.csv",header = T)
train_lnc=train_lnc[,-c(1,3)]
train_m=read.csv("/lncRNA-m6A-prediction/exon.train.csv",header = T)
train_m=train_m[,-1]
test=read.csv("/lncRNA-m6A-prediction/exonic_test.csv",header = T)
test=test[,-c(1,3)]
#Make mRNA and lncrna have the same characteristics

index=which(!is.na(match(colnames(train_m),colnames(test))))
if(length(index)>0){
  train_m=train_m[,-index]
}

index=which(!is.na(match(colnames(test),colnames(train_m))))
if(length(index)>0){
  test=test[,index]
}
result=mature_rf(train_lnc,train_m,test,0.3,"/lncRNA-m6A-prediction/","mature.predict")

#fulltx mode prediction
train_lnc=read.csv("/lncRNA-m6A-prediction/fulltx_train.csv",header = T)
train_lnc=train_lnc[,-c(1,3)]
test=read.csv("/lncRNA-m6A-prediction/fulltx_test.csv",header = T)
test=test[,-c(1,3)]
result=fulltx_rf(train_lnc,test,"/lncRNA-m6A-prediction/","fulltx.predict")