#install.packages('randomForest')
#install.packages('ROCR')
library(ggplot2)
library(randomForest)
library(ROCR)

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
train<-read.csv(train_f, header = TRUE)
test<-read.csv(test_f, header = TRUE)
data<-data.frame()
data<-rbind(data,train)
data<-rbind(data,test)
data<-data[2:12] # cut column X (just no meaning serial number)
str(data)

# 2 missing value imputation
# 2.1 RevolvingUtilizationOfUnsecuredLines
summary(data$RevolvingUtilizationOfUnsecuredLines)
boxplot(data$RevolvingUtilizationOfUnsecuredLines) # ratio should be between 0 and 1
sum(is.na(data$RevolvingUtilizationOfUnsecuredLines)) # number of missing values
sum(data$RevolvingUtilizationOfUnsecuredLines>1) # outliers
qqnorm(data$RevolvingUtilizationOfUnsecuredLines[data$RevolvingUtilizationOfUnsecuredLines<=1])
qqline(data$RevolvingUtilizationOfUnsecuredLines) # Quantile-Quantile Plots, indicates not a normal dist.
# replace the abnormal values with median
data$RevolvingUtilizationOfUnsecuredLines[data$RevolvingUtilizationOfUnsecuredLines>1]<-median(data$RevolvingUtilizationOfUnsecuredLines)
boxplot(data$RevolvingUtilizationOfUnsecuredLines)

# 2.2 age
ggplot(data = data,aes(age))+geom_histogram(col='red',fill='red')+labs(title='Histogram of Age')
sum(is.na(data$age))
summary(data$age) # no missing & abnormal value

# 2.3 NumberOfTime30.59DaysPastDueNotWorse
summary(data$NumberOfTime30.59DaysPastDueNotWorse)
sum(is.na(data$NumberOfTime30.59DaysPastDueNotWorse))
ggplot(data = data,aes(NumberOfTime30.59DaysPastDueNotWorse))+geom_histogram(col='black',fill='red')+
  labs(title='Histogram Num of time past 30-59days')
table(data$NumberOfTime30.59DaysPastDueNotWorse) # shows a few people with 96 and 98 times which is quite absurd
data$NumberOfTime30.59DaysPastDueNotWorse[data$NumberOfTime30.59DaysPastDueNotWorse>=96]<-0 # replace with mode
ggplot(data = data,aes(NumberOfTime30.59DaysPastDueNotWorse))+geom_histogram(col='black',fill='red')+
  labs(title='Histogram Num of time past 30-59days')

# 2.4 DebtRatio
summary(data$DebtRatio) 
sum(is.na(data$DebtRatio))
data$DebtRatio[data$DebtRatio>100000]<-mean(data$DebtRatio)
boxplot(data$DebtRatio)

# 2.5 MonthlyIncome
sum(is.na(data$MonthlyIncome)) # many missing values
summary(data$MonthlyIncome)
data$MonthlyIncome[is.na(data$MonthlyIncome)]<-median(data$MonthlyIncome,na.rm = TRUE) # replace missing one with median
data$MonthlyIncome[data$MonthlyIncome>300000]<-median(data$MonthlyIncome)
boxplot(data$MonthlyIncome)

# 2.6 NumberOfOpenCreditLinesAndLoans
sum(is.na(data$NumberOfOpenCreditLinesAndLoans))
summary(data$NumberOfOpenCreditLinesAndLoans)
ggplot(data = data,aes(NumberOfOpenCreditLinesAndLoans))+geom_histogram(col='red',fill='green')+
  labs(title='Histogram of num of open credit lines')
boxplot(data$NumberOfOpenCreditLinesAndLoans) # seems like this variable needs no imputations

# 2.7 NumberOfTimes90DaysLate
sum(is.na(data$NumberOfTimes90DaysLate))
summary(data$NumberOfTimes90DaysLate)
boxplot(data$NumberOfTimes90DaysLate)
table(data$NumberOfTimes90DaysLate) # over 90 is too odd
data$NumberOfTimes90DaysLate[data$NumberOfTimes90DaysLate>90]<-0 # replace with median 0

# 2.8 NumberRealEstateLoansOrLines
summary(data$NumberRealEstateLoansOrLines)
sum(is.na(data$NumberRealEstateLoansOrLines))
table(data$NumberRealEstateLoansOrLines)
data$NumberRealEstateLoansOrLines[data$NumberRealEstateLoansOrLinese>45]<-1 # median

# 2.9 NumberOfTime60.89DaysPastDueNotWorse
sum(is.na(data$NumberOfTime60.89DaysPastDueNotWorse))
table(data$NumberOfTime60.89DaysPastDueNotWorse)
summary(data$NumberOfTime60.89DaysPastDueNotWorse)
data$NumberOfTime60.89DaysPastDueNotWorse[data$NumberOfTime60.89DaysPastDueNotWorse>90]<-0 # replace with median

# 2.10 NumberOfDependents
summary(data$NumberOfDependents)
data$NumberOfDependents[is.na(data$NumberOfDependents)]<-0 # median

# 3 data balencing
index<-c(1:length(train[,2]))
train1<-data[index,]
test2<-data[-index,]
prop.table(table(train1$SeriousDlqin2yrs)) # serious data imbalance, do 0 class downsampling
sum(train1$SeriousDlqin2yrs==1)
newtrainData<-train1[train1$SeriousDlqin2yrs==1,]
downsampleData<-train1[train1$SeriousDlqin2yrs==0,]
set.seed(8880)
downsam<-sample(1:length(downsampleData[,2]),length(data[,2])*0.08)
train2<-rbind(newtrainData,downsampleData[downsam,])
train2<-train2[sample(nrow(train2)),]
rownames(train2)<-NULL
head(train2)
train2$SeriousDlqin2yrs<-as.factor(train2$SeriousDlqin2yrs)

# 4 build model & k-fold CV
folds<-cut(seq(1,nrow(train2)),breaks=foldnum,labels=FALSE)
set<-c()
training<-c()
validation<-c()
test<-c()
best<-0
for(q in 1:foldnum){
  if(q!=foldnum){
    testIndexes<-which(folds==q,arr.ind=TRUE)
    valiIndexes<-which(folds==q+1,arr.ind=TRUE)
    zz<-c(testIndexes,valiIndexes)
    testData<-train2[testIndexes, ]
    valiData<-train2[valiIndexes, ]
    trainData<-train2[-zz, ]
  }else{
    testIndexes<-which(folds==q,arr.ind=TRUE)
    valiIndexes<-which(folds==1,arr.ind=TRUE)
    zz<-c(testIndexes,valiIndexes)
    testData<-train2[testIndexes, ]
    valiData<-train2[valiIndexes, ]
    trainData<-train2[-zz, ]
  }  
  
  # train
  model<-randomForest(
    SeriousDlqin2yrs~.,
    data=trainData,mtry=2,
    importance=TRUE,
    ntree=3000
  )
  result<-predict(model,newdata = trainData[,-1],'prob')
  output<-result[,2]
  pr<-prediction(output, trainData$SeriousDlqin2yrs)
  auc<-performance(pr, measure = "auc")
  auc_train<-auc@y.values[[1]]  
  
  # validation
  pred<-predict(model,newdata = valiData[,-1],'prob')
  output<-pred[,2]
  pr<-prediction(output, valiData$SeriousDlqin2yrs)
  auc<-performance(pr, measure = "auc")
  auc_vali<-auc@y.values[[1]]
  
  # testing
  pred<-predict(model,newdata = testData[,-1],'prob')
  output<-pred[,2]
  pr<-prediction(output, testData$SeriousDlqin2yrs)
  auc<-performance(pr, measure = "auc")
  auc_test<-auc@y.values[[1]]
  
  # report
  set_name<-paste('fold',toString(q))
  set<-c(set,set_name)
  training<-c(training,round(auc_train,digits=2))
  validation<-c(validation,round(auc_vali,digits=2))
  test<-c(test,round(auc_test,digits=2)) 
  
  # get best model for predict
  if(auc_train+auc_vali>best){
    best<-auc_train+auc_vali
    best_model<-model
  }
}

# 5 report csv
out_data<-data.frame(set=set,training=training,validation=validation,test=test,stringsAsFactors=FALSE)
ave<-round(sapply(out_data[2:4], mean),digits=2)
out_data<-rbind(out_data,c('ave.',ave))
write.csv(out_data, file=report_f, row.names = F)

# 6 predict csv
pred<-predict(best_model,newdata = test2[,-1],'prob')
id<-c(1:length(test2[,3]))
prob<-pred[,2]
out_pred<-data.frame(Id=id,probability=round(prob,digit=2))
write.csv(out_pred, file=predict_f, row.names = F)

