library('rpart')
library('class') # for knn used in subset selecting   

# 1 read input data
args = commandArgs(trailingOnly=TRUE)
i<-1 
while(i<length(args)){
  if(args[i]=="--fold"){
    foldnum<-args[i+1]
    i<-i+1
  }else if(args[i]=="--train"){
    train_f<-args[i+1]
    i<-i+1
  }else if(args[i]=="--test"){
    test_f<-args[i+1]
    i<-i+1
  }else if(args[i]=="--report"){
    report_f<-args[i+1]
    i<-i+1
  }else if(args[i]=="--predict"){
    predict_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Input error", args[i]), call.=FALSE)
  }
  i<-i+1
}
foldnum<-strtoi(foldnum)
titanic<-data.frame()
# 1 load data
data <- read.csv(train_f, header = TRUE)
compete <- read.csv(test_f, header = TRUE)
compete$Survived <- NA
#compete<-compete[,c(1,12,2,3,4,5,6,7,8,9,10,11)]
#head(compete)
titanic <- rbind(titanic,data)
titanic <- rbind(titanic,compete)
# knn, used later, need all input to be numeric so convert all into it

# give NA in column age other's average
#NAindexes<-c()
#for(i in c(1:length(titanic$Age))){
#    if(is.na(titanic$Age[i])){
#        NAindexes<-c(NAindexes,i)}
#}
#tmp_age<-titanic$Age[-NAindexes]
#titanic$Age_1<-titanic$Age
#titanic$Age_1[NAindexes]<-round(mean(tmp_age))

# give NA in column fare other's average
NAindexes<-c()
for(i in c(1:length(titanic$Fare))){
    if(is.na(titanic$Fare[i])){
        NAindexes<-c(NAindexes,i)}
}
tmp_fare<-titanic$Fare[-NAindexes]
titanic$Fare_1<-titanic$Fare
titanic$Fare_1[NAindexes]<-mean(tmp_fare)
# rename/reassign Mlle, Ms, and Mme in column name to a new column title
titanic$title<-gsub('(.*, )|(\\..*)', '', titanic$Name)
unusual_title<-c('Dona', 'Lady', 'the Countess','Capt', 'Col' , 'Don',
                 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
titanic$title[titanic$title=='Mlle']<-'Miss'
titanic$title[titanic$title=='Ms']<-'Miss'
titanic$title[titanic$title=='Mme']<-'Mrs'
titanic$title[titanic$title %in% unusual_title]<-'Unusual Title'
# titanic$title into Master=1 Miss=2 Mr=3 Mrs=4 Unusual Title=5
titanic$title[titanic$title=='Master']<-1
titanic$title[titanic$title=='Miss']<-2
titanic$title[titanic$title=='Mr']<-3
titanic$title[titanic$title=='Mrs']<-4
titanic$title[titanic$title=='Unusual Title']<-5
# turn column sex into male=0 female=1
titanic$Sex_1<-titanic$Sex
titanic$Sex_1 <- as.character.factor(titanic$Sex_1)
titanic$Sex_1[titanic$Sex_1=='male']<-0
titanic$Sex_1[titanic$Sex_1=='female']<-1
# turn column embarked into S=1 C=2 Q=3
s<-0
c<-0
q<-0
for(i in titanic$Embarked){
    if(i == 'S'){s<-s+1}
    else if(i == 'C'){c<-c+1}
    else if(i == 'Q'){q<-q+1}
}
titanic$Embarked_1<-titanic$Embarked
titanic$Embarked_1 <- as.character.factor(titanic$Embarked_1)
titanic$Embarked_1[62]<-'S'
titanic$Embarked_1[830]<-'C'
titanic$Embarked_1[titanic$Embarked_1=='S']<-1
titanic$Embarked_1[titanic$Embarked_1=='C']<-2
titanic$Embarked_1[titanic$Embarked_1=='Q']<-3
# age goes by predict
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare_1 + Embarked_1 + title,
                data=titanic[!is.na(titanic$Age),], method="anova")
titanic$Age_1<-titanic$Age
titanic$Age_1[is.na(titanic$Age_1)] <- predict(Agefit, titanic[is.na(titanic$Age_1),])
titanic$Age_1<-round(titanic$Age_1,digit=2)

# separate male and female, then build model
data <- titanic[1:891,]
compete <- titanic[892:1309,]
maleIndexes<-which(data$Sex_1==0,arr.ind=TRUE)
test_male<-data[maleIndexes, ]
test_fem<-data[-maleIndexes, ]
maleIndexes<-which(compete$Sex_1==0,arr.ind=TRUE)
compete_male<-compete[maleIndexes, ]
compete_fem<-compete[-maleIndexes, ]
compete_fem$title[compete_fem$title==1]<-5 # there are MR & Master appear in compete data
compete_fem$title[compete_fem$title==3]<-5 # strange value in place it should jot have been
compete_male$title[compete_male$title==2]<-5
compete_male$title[compete_male$title==4]<-5
compete_fem$Sex_1[compete_fem$Sex_1==0]<-1
compete_male$Sex_1[compete_male$Sex_1==1]<-0

# male
# 2 select subset
set.seed(88)
sub_select<-test_male[sample(length(test_male$PassengerId)),]
sub_fold<-cut(seq(1,nrow(sub_select)),breaks=3,labels=FALSE)
testIndexes<-which(sub_fold==1,arr.ind=TRUE)
testData<-sub_select[testIndexes, ]
trainData<-sub_select[-testIndexes, ]
num_list<-c()
acc_list<-c()
for(w in c(3,7,8,13,14,15,16,17)){
    knn_pred<-knn(data.frame(trainData[,w]),data.frame(testData[,w]),trainData[,2],k=3)
    cm<-as.matrix(table(Actual = testData[,2], Predicted = knn_pred))
    correct<-cm[1,1]+cm[2,2]
    all<-correct+cm[1,2]+cm[2,1]
    acc<-correct/all
    num_list<-c(num_list,w)
    acc_list<-c(acc_list,acc)
}
knn_select<-data.frame(num=num_list,acc=acc_list)
knn_select<-knn_select[order(knn_select$acc,decreasing=TRUE),]
tmp<-test_male[ ,c(2,knn_select[c(1:7),1])]
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
  model<-rpart(Survived ~ .,
               data=trainData, control=rpart.control(maxdepth=5),
               method="class")
  train_result<-data.frame(truth=trainData$Survived,pred=predict(model, type="class"))
  train_cm<-table(train_result)
  train_correct<-train_cm[1,1]+train_cm[2,2]
  train_all<-train_correct+train_cm[1,2]+train_cm[2,1]
  train_acc<-round(train_correct/train_all,digits=2)
  # validation
  pred<-predict(model, valiData, type="class")
  vali_result<-data.frame(truth=valiData$Survived,pred=pred)
  vali_cm<-table(vali_result)
  vali_correct<-vali_cm[1,1]+vali_cm[2,2]
  vali_all<-vali_correct+vali_cm[1,2]+vali_cm[2,1]
  vali_acc<-round(vali_correct/vali_all,digits=2)
  # test
  pred1<-predict(model, testData, type="class")
  test_result<-data.frame(truth=testData$Survived,pred=pred1)
  test_cm<-table(test_result)
  test_cm<-table(test_result)
  test_correct<-test_cm[1,1]+test_cm[2,2]
  test_all<-test_correct+test_cm[1,2]+test_cm[2,1]
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
out_data_male<-rbind(out_data,c('ave.',ave))
prediction_male <- predict(model, compete_male, type="class")

# female
# 2 select subset
set.seed(88)
sub_select<-test_fem[sample(length(test_fem$PassengerId)),]
sub_fold<-cut(seq(1,nrow(sub_select)),breaks=3,labels=FALSE)
testIndexes<-which(sub_fold==1,arr.ind=TRUE)
testData<-sub_select[testIndexes, ]
trainData<-sub_select[-testIndexes, ]
num_list<-c()
acc_list<-c()
for(w in c(3,7,8,13,14,15,16,17)){
    knn_pred<-knn(data.frame(trainData[,w]),data.frame(testData[,w]),trainData[,2],k=3)
    cm<-as.matrix(table(Actual = testData[,2], Predicted = knn_pred))
    correct<-cm[1,1]+cm[2,2]
    all<-correct+cm[1,2]+cm[2,1]
    acc<-correct/all
    num_list<-c(num_list,w)
    acc_list<-c(acc_list,acc)
}
knn_select<-data.frame(num=num_list,acc=acc_list)
knn_select<-knn_select[order(knn_select$acc,decreasing=TRUE),]
tmp<-test_fem[ ,c(2,knn_select[c(1:7),1])]
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
  model<-rpart(Survived ~ .,
               data=trainData, control=rpart.control(maxdepth=5),
               method="class")
  train_result<-data.frame(truth=trainData$Survived,pred=predict(model, type="class"))
  train_cm<-table(train_result)
  train_correct<-train_cm[1,1]+train_cm[2,2]
  train_all<-train_correct+train_cm[1,2]+train_cm[2,1]
  train_acc<-round(train_correct/train_all,digits=2)
  # validation
  pred<-predict(model, valiData, type="class")
  vali_result<-data.frame(truth=valiData$Survived,pred=pred)
  vali_cm<-table(vali_result)
  vali_correct<-vali_cm[1,1]+vali_cm[2,2]
  vali_all<-vali_correct+vali_cm[1,2]+vali_cm[2,1]
  vali_acc<-round(vali_correct/vali_all,digits=2)
  # test
  pred1<-predict(model, testData, type="class")
  test_result<-data.frame(truth=testData$Survived,pred=pred1)
  test_cm<-table(test_result)
  test_cm<-table(test_result)
  test_correct<-test_cm[1,1]+test_cm[2,2]
  test_all<-test_correct+test_cm[1,2]+test_cm[2,1]
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
out_data_fem<-rbind(out_data,c('ave.',ave))
prediction_fem <- predict(model, compete_fem, type="class")

# output prediction
Output_fem<- data.frame(PassengerID = compete_fem$PassengerId, Survived = prediction_fem)
Output_male<- data.frame(PassengerID = compete_male$PassengerId, Survived = prediction_male)
Output <- rbind(Output_male,Output_fem)
#Output<-arrange(Output, PassengerID)
Output<-Output[order(Output$PassengerID),]
write.csv(Output, file = predict_f, row.names = F)
#577 male in test.csv, 314 female, total 891
for(i in c(2:length(out_data_male))){
  for(q in c(1:length(out_data_male[,1]))){
    oo<-(as.numeric(out_data_male[q,i])*577+as.numeric(out_data_fem[q,i])*314)/891
    oo<-round(oo,digit=2)
    out_data[q,i]<-oo
  }
}
out_data[length(out_data_male[,1]),1]<-'ave.'
write.csv(out_data, file = report_f, row.names = F)