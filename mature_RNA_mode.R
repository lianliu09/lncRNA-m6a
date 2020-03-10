library(randomForest)
library(caret)
library(pROC)
mature_rf<-function(train_lnc,train_m,test,alpha,dir,filename){
  #train
  print("Predicting...")
  rf <- randomForest(Target_label ~ ., data=train_lnc)
  p_lnc = predict(rf,newdata=test,type="prob")
  rf <- randomForest(Target_label ~ ., data=train_m)
  p_m = predict(rf,newdata=test,type="prob")
  p = alpha*p_m[,2]+(1-alpha)*p_lnc[,2]
  TP <- as.numeric(sum(p>0.5 & test$Target_label=="m6A_positive"))
  FP <- as.numeric(sum(p>0.5 & test$Target_label=="m6A_negative"))
  TN <- as.numeric(sum(p<0.5 & test$Target_label=="m6A_negative"))
  FN <- as.numeric(sum(p<0.5 & test$Target_label=="m6A_positive"))
  TPR=TP/(TP+FN)
  FPR=TN/(TN+FP)
  MCC=(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  F1=(2*TP)/(2*TP+FP+FN)
  ACC=(TP+TN)/(TP+TN+FP+FN)
  precision=TP/(TP+FP)
  recall=TP/(TP+FN)
  ROCArea=auc(roc(test$Target_label,as.numeric(p)))
  
  result=data.frame(TPR,FPR,F1,precision,ACC,recall,MCC,ROCArea)
  colnames(result)<-c("TPR","FPR","F-Measure","Precision","ACC","Recall","MCC","AUC")
  write.csv(result,paste(dir,paste(filename,"independent.result.rf.csv"),sep = '/'),row.names = F)
  write.csv(p,paste(dir,paste(filename,"independent.probability.rf.csv"),sep = '/'),row.names = F)
  
  return(result)
}