library('rpart')
library('class') # for knn used in subset selecting

# 1 read input data
args = commandArgs(trailingOnly=TRUE)
i<-1 
while(i<length(args)){
  if(args[i]=="--fold"){
    foldnum<-args[i+1]
    i<-i+1
  }else if(args[i]=="--input"){
    i_f<-args[i+1]
    i<-i+1
  }else if(args[i]=="--output"){
    o_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Input error", args[i]), call.=FALSE)
  }
  i<-i+1
}
foldnum<-strtoi(foldnum)
d<-read.csv(i_f, header = F)
# label to be predicted
all_labels<-levels(d[,2]) #for what?
d<-d[,1:5602]

# 2 select subset
set.seed(88)
sub_select<-d[sample(nrow(d)),]
sub_fold<-cut(seq(1,nrow(sub_select)),breaks=5,labels=FALSE)
testIndexes<-which(sub_fold==1,arr.ind=TRUE)
testData<-sub_select[testIndexes, ]
trainData<-sub_select[-testIndexes, ]
num_list<-c()
acc_list<-c()
for(w in 3:5602){
  knn_pred<-knn(data.frame(trainData[,w]),data.frame(testData[,w]),trainData[,2],k=20,l=5)
  cm<-as.matrix(table(Actual = testData[,2], Predicted = knn_pred))
  correct<-cm[1,'CP']+cm[2,'CW']+cm[3,'EC']+cm[4,'IM']
  all<-0
  for(j in 1:4){
    all<-cm[j,'CP']+cm[j,'CW']+cm[j,'EC']+cm[j,'IM']+all
  }
  acc<-correct/all
  num_list<-c(num_list,w)
  acc_list<-c(acc_list,acc)
}
knn_select<-data.frame(num=num_list,acc=acc_list)
knn_select<-knn_select[order(knn_select$acc,decreasing=TRUE),]
tmp<-d[ ,c(2,knn_select[c(1:50),1])]

# 3 fold
set.seed(888)
tmp<-tmp[sample(nrow(tmp)),]
folds<-cut(seq(1,nrow(tmp)),breaks=foldnum,labels=FALSE)

# 4 perform CV
set<-c()
training<-c()
validation<-c()
test<-c()
for(q in 1:foldnum){
  if(q!=foldnum){
    testIndexes<-which(folds==q,arr.ind=TRUE)
    valiIndexes<-which(folds==q+1,arr.ind=TRUE)
    zz<-c(testIndexes,valiIndexes)
    testData<-tmp[testIndexes, ]
    valiData<-tmp[valiIndexes, ]
    trainData<-tmp[-zz, ]
  }else{
    testIndexes<-which(folds==q,arr.ind=TRUE)
    valiIndexes<-which(folds==1,arr.ind=TRUE)
    zz<-c(testIndexes,valiIndexes)
    testData<-tmp[testIndexes, ]
    valiData<-tmp[valiIndexes, ]
    trainData<-tmp[-zz, ]
  }
  # train
  model<-rpart(V2 ~ .,
               data=trainData, control=rpart.control(maxdepth=5),
               method="class")
  train_result<-data.frame(truth=trainData$V2,pred=predict(model, type="class"))
  train_cm<-table(train_result)
  train_correct<-train_cm[1,'CP']+train_cm[2,'CW']+train_cm[3,'EC']+train_cm[4,'IM']
  train_all<-0
  for(j in 1:4){
    train_all<-train_cm[j,'CP']+train_cm[j,'CW']+train_cm[j,'EC']+train_cm[j,'IM']+train_all
  }
  train_acc<-round(train_correct/train_all,digits=2)
  # validation
  pred<-predict(model, valiData, type="class")
  vali_result<-data.frame(truth=valiData$V2,pred=pred)
  vali_cm<-table(vali_result)
  vali_correct<-vali_cm[1,'CP']+vali_cm[2,'CW']+vali_cm[3,'EC']+vali_cm[4,'IM']
  vali_all<-0
  for(j in 1:4){
    vali_all<-vali_cm[j,'CP']+vali_cm[j,'CW']+vali_cm[j,'EC']+vali_cm[j,'IM']+vali_all
  }
  vali_acc<-round(vali_correct/vali_all,digits=2)
  # test
  pred1<-predict(model, testData, type="class")
  test_result<-data.frame(truth=testData$V2,pred=pred1)
  test_cm<-table(test_result)
  test_correct<-test_cm[1,'CP']+test_cm[2,'CW']+test_cm[3,'EC']+test_cm[4,'IM']
  test_all<-0
  for(j in 1:4){
    test_all<-test_cm[j,'CP']+test_cm[j,'CW']+test_cm[j,'EC']+test_cm[j,'IM']+test_all
  }
  test_acc<-round(test_correct/test_all,digits=2)
  # for output
  set_name<-paste('fold',toString(q))
  set<-c(set,set_name)
  training<-c(training,train_acc)
  validation<-c(validation,vali_acc)
  test<-c(test,test_acc)
}

# 5 output file
out_data<-data.frame(set=set,training=training,validation=validation,test=test,stringsAsFactors=FALSE)
ave<-round(sapply(out_data[2:4], mean),digits=2)
out_data<-rbind(out_data,c('ave.',ave))
write.table(out_data, file=o_f, row.names = F, quote = F, sep=",")


