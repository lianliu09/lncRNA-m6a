library(randomForest)
library(caret)
library(pROC)
#train: the train data
#test: the test data
#dir: the directory where the results are saved
#filename: the result files name
independent_rf<-function(train,test,dir,filename){
  #train
  print("Predicting...")
  rf <- randomForest(Target_label ~ ., data=train)
  classification=predict(rf,newdata=test)
  pro=predict(rf,newdata=test,type="prob")
  TP <- as.numeric(sum(classification=="m1A_positive" & test$Target_label=="m1A_positive"))
  FP <- as.numeric(sum(classification=="m1A_positive" & test$Target_label=="m1A_negative"))
  TN <- as.numeric(sum(classification=="m1A_negative" & test$Target_label=="m1A_negative"))
  FN <- as.numeric(sum(classification=="m1A_negative" & test$Target_label=="m1A_positive"))
  TPR=TP/(TP+FN)
  FPR=TN/(TN+FP)
  MCC=(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  F1=(2*TP)/(2*TP+FP+FN)
  ACC=(TP+TN)/(TP+TN+FP+FN)
  precision=TP/(TP+FP)
  recall=TP/(TP+FN)
  ROCArea=auc(roc(test$Target_label,as.numeric(pro[,2])))
  prob=pro[,2]
  
  result=data.frame(TPR,FPR,F1,precision,ACC,recall,MCC,ROCArea)
  colnames(result)<-c("TPR","FPR","F-Measure","Precision","ACC","Recall","MCC","AUC")
  write.csv(result,paste(dir,paste(filename,"independent.result.rf.csv"),sep = '/'),row.names = F)
  write.csv(pro[,2],paste(dir,paste(filename,"independent.probability.rf.csv"),sep = '/'),row.names = F)
  
  return(result)
}
