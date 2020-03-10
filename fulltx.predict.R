library(randomForest)
print("read test...")
test=read.csv("/home/liulian/unlabeled_data_full.csv",header=T)
print("read train...")
train=read.csv("/home/liulian/fulltx_train.csv",header=T)
train=train[,-c(1,3)]

train.name=colnames(train)
test.name=colnames(test)
index=which(!is.na(match(train.name,test.name)))
if(length(index)>0){
  train=cbind(train[,1],train[,index])
}


index=which(!is.na(match(test.name,train.name)))
if(length(index)>0){
  test=test[,index]
}
colnames(train)<-c("Target_label",colnames(train[,2:ncol(train)]))
print("read ending...")
print("Predicting...")
rf <- randomForest(Target_label ~ ., data=train)
classification=predict(rf,newdata=test)
prob=predict(rf,newdata=test,type="prob")
print("Predicting end...")

write.csv(classification,"/home/liulian/fulltx.predictor.csv",row.names = F)
write.csv(prob[,2],"/home/liulian/fulltx.probability.csv",row.names = F)