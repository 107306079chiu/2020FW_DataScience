library(ggplot2)
library(randomForest)
library(ROCR)
library(e1071)
library(rpart)
library(caret)

# 1 read input data & modify column
train<-read.csv('hw7_train.csv', header = TRUE)
test<-read.csv('hw7_test.csv', header = TRUE)
length(train[,2])
length(test[,2])
label<-c()
for(i in 1:length(test[,2])){
  label<-c(label, 0)
}
test<-cbind(test, label)
test<-test[c(1,22,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)]
data<-data.frame()
data<-rbind(data,train)
data<-rbind(data,test)
data<-data[2:22] # cut column X (just no meaning serial number)
str(data)

# 2 missing & odd value imputation
summary(data)
sum(is.na(data)) # no missings

# 2.1 feature1
#boxplot(data$feature1) # outliers BAD
#sum(data$feature1< -1)
lower_bound<-quantile(data$feature1, 0.025)
upper_bound<-quantile(data$feature1, 0.975)
#sum(data$feature1 < lower_bound)
data$feature1[data$feature1<lower_bound]<-median(data$feature1)
data$feature1[data$feature1>upper_bound]<-median(data$feature1)
boxplot(data$feature1)
# 2.2 
lower_bound<-quantile(data$feature2, 0.025)
upper_bound<-quantile(data$feature2, 0.975)
data$feature2[data$feature2<lower_bound]<-median(data$feature2)
data$feature2[data$feature2>upper_bound]<-median(data$feature2)
boxplot(data$feature2)
# 2.3
lower_bound<-quantile(data$feature3, 0.025)
upper_bound<-quantile(data$feature3, 0.975)
data$feature3[data$feature3<lower_bound]<-median(data$feature3)
data$feature3[data$feature3>upper_bound]<-median(data$feature3)
boxplot(data$feature3)
# 2.4
lower_bound<-quantile(data$feature4, 0.025)
upper_bound<-quantile(data$feature4, 0.975)
data$feature4[data$feature4<lower_bound]<-median(data$feature4)
data$feature4[data$feature4>upper_bound]<-median(data$feature4)
boxplot(data$feature4)
# 2.5
lower_bound<-quantile(data$feature5, 0.025)
upper_bound<-quantile(data$feature5, 0.975)
data$feature5[data$feature5<lower_bound]<-median(data$feature5)
data$feature5[data$feature5>upper_bound]<-median(data$feature5)
boxplot(data$feature5)
# 2.6
lower_bound<-quantile(data$feature6, 0.025)
upper_bound<-quantile(data$feature6, 0.975)
data$feature6[data$feature6<lower_bound]<-median(data$feature6)
data$feature6[data$feature6>upper_bound]<-median(data$feature6)
boxplot(data$feature6)
# 2.7
lower_bound<-quantile(data$feature7, 0.025)
upper_bound<-quantile(data$feature7, 0.975)
data$feature7[data$feature7<lower_bound]<-median(data$feature7)
data$feature7[data$feature7>upper_bound]<-median(data$feature7)
boxplot(data$feature7)
# 2.8
lower_bound<-quantile(data$feature8, 0.025)
upper_bound<-quantile(data$feature8, 0.975)
data$feature8[data$feature8<lower_bound]<-median(data$feature8)
data$feature8[data$feature8>upper_bound]<-median(data$feature8)
boxplot(data$feature8)
# 2.9
lower_bound<-quantile(data$feature9, 0.025)
upper_bound<-quantile(data$feature9, 0.975)
data$feature9[data$feature9<lower_bound]<-median(data$feature9)
data$feature9[data$feature9>upper_bound]<-median(data$feature9)
boxplot(data$feature9)
# 2.10
lower_bound<-quantile(data$feature10, 0.025)
upper_bound<-quantile(data$feature10, 0.975)
data$feature10[data$feature10<lower_bound]<-median(data$feature10)
data$feature10[data$feature10>upper_bound]<-median(data$feature10)
boxplot(data$feature10)
# 2.11
lower_bound<-quantile(data$feature11, 0.025)
upper_bound<-quantile(data$feature11, 0.975)
data$feature11[data$feature11<lower_bound]<-median(data$feature11)
data$feature11[data$feature11>upper_bound]<-median(data$feature11)
boxplot(data$feature11)
# 2.12
lower_bound<-quantile(data$feature12, 0.025)
upper_bound<-quantile(data$feature12, 0.975)
data$feature12[data$feature12<lower_bound]<-median(data$feature12)
data$feature12[data$feature12>upper_bound]<-median(data$feature12)
boxplot(data$feature12)
# 2.13
lower_bound<-quantile(data$feature13, 0.025)
upper_bound<-quantile(data$feature13, 0.975)
data$feature13[data$feature13<lower_bound]<-median(data$feature13)
data$feature13[data$feature13>upper_bound]<-median(data$feature13)
boxplot(data$feature13)
# 2.14
lower_bound<-quantile(data$feature14, 0.025)
upper_bound<-quantile(data$feature14, 0.975)
data$feature14[data$feature14<lower_bound]<-median(data$feature14)
data$feature14[data$feature14>upper_bound]<-median(data$feature14)
boxplot(data$feature14)
# 2.15
lower_bound<-quantile(data$feature15, 0.025)
upper_bound<-quantile(data$feature15, 0.975)
data$feature15[data$feature15<lower_bound]<-median(data$feature15)
data$feature15[data$feature15>upper_bound]<-median(data$feature15)
boxplot(data$feature15)
# 2.16
lower_bound<-quantile(data$feature16, 0.025)
upper_bound<-quantile(data$feature16, 0.975)
data$feature16[data$feature16<lower_bound]<-median(data$feature16)
data$feature16[data$feature16>upper_bound]<-median(data$feature16)
boxplot(data$feature16)
# 2.17
lower_bound<-quantile(data$feature17, 0.025)
upper_bound<-quantile(data$feature17, 0.975)
data$feature17[data$feature17<lower_bound]<-median(data$feature17)
data$feature17[data$feature17>upper_bound]<-median(data$feature17)
boxplot(data$feature17)
# 2.18
lower_bound<-quantile(data$feature18, 0.025)
upper_bound<-quantile(data$feature18, 0.975)
data$feature18[data$feature18<lower_bound]<-median(data$feature18)
data$feature18[data$feature18>upper_bound]<-median(data$feature18)
boxplot(data$feature18)
# 2.19
lower_bound<-quantile(data$feature19, 0.025)
upper_bound<-quantile(data$feature19, 0.975)
data$feature19[data$feature19<lower_bound]<-median(data$feature19)
data$feature19[data$feature19>upper_bound]<-median(data$feature19)
boxplot(data$feature19)
# 2.20
lower_bound<-quantile(data$feature20, 0.025)
upper_bound<-quantile(data$feature20, 0.975)
data$feature20[data$feature20<lower_bound]<-median(data$feature20)
data$feature20[data$feature20>upper_bound]<-median(data$feature20)
boxplot(data$feature20)

# 3 data balencing
index<-c(1:length(train[,2]))
train1<-data[index,]
test1<-data[-index,]
#train1$label<-as.factor(train1$label)

# made 3 kinds of data: (1)original (2)downsampled (3)significant features only
# (1)original
train1_o<-train1
train1_o$label<-as.factor(train1_o$label)

# (2)downsampled: do -1 class downsampling
prop.table(table(train1$label)) # no serious data imbalance -1:1=65:35 WRONG!!
newtrainData<-train1[train1$label==1,]
downsampleData<-train1[train1$label==-1,]
set.seed(8880)
downsam<-sample(1:length(downsampleData[,2]),round(length(train1[,2])*0.42))
train2<-rbind(newtrainData,downsampleData[downsam,])
train1_d<-train2[sample(nrow(train2)),]
rownames(train1_d)<-NULL
head(train1_d)
train1_d$label<-as.factor(train1_d$label)
prop.table(table(train1_d$label))

# (3)significant features only
eda<-lm(formula=label~., data=train1)
summary(eda) #feature1,2,4,5,6,10,13,14,15,16,17 are significant
fea<-c(1,2,3,5,6,7,11,14,15,16,17,18)
train1_s<-train1[,fea]
head(train1_s)
train1_s$label<-as.factor(train1_s$label)

# (4)2+3
train1_a<-train1_d[,fea]

# 4 build model
set.seed(2021)
#(1)&(3)
index<-sample(1:length(train1_o[,2]),length(train1_o[,2])*0.2)
train1_1_test<-train1_o[index,]
train1_1_train<-train1_o[-index,]
train1_3_test<-train1_s[index,]
train1_3_train<-train1_s[-index,]

#(2)&(4)
index<-sample(1:length(train1_d[,2]),length(train1_d[,2])*0.2)
train1_2_test<-train1_d[index,]
train1_2_train<-train1_d[-index,]
train1_4_test<-train1_a[index,]
train1_4_train<-train1_a[-index,]

# ====naive bayes====
#install.packages("e1071")
model_nb_1<-naiveBayes(label~., data=train1_1_train)
model_nb_2<-naiveBayes(label~., data=train1_2_train)
model_nb_3<-naiveBayes(label~., data=train1_3_train)
model_nb_4<-naiveBayes(label~., data=train1_4_train)
pred_nb_1<-predict(model_nb_1, train1_1_test)
pred_nb_2<-predict(model_nb_2, train1_2_test)
pred_nb_3<-predict(model_nb_3, train1_3_test)
pred_nb_4<-predict(model_nb_4, train1_4_test)
cM_nb_1<-table(pred_nb_1, train1_1_test$label)
cM_nb_2<-table(pred_nb_2, train1_2_test$label)
cM_nb_3<-table(pred_nb_3, train1_3_test$label)
cM_nb_4<-table(pred_nb_4, train1_4_test$label)
sum(diag(cM_nb_1))/sum(cM_nb_1)
sum(diag(cM_nb_2))/sum(cM_nb_2)
sum(diag(cM_nb_3))/sum(cM_nb_3)
sum(diag(cM_nb_4))/sum(cM_nb_4)
pred_nb<-predict(model_nb_3, test1)
head(pred_nb)
out_pred<-data.frame(id=id,label=pred_nb)
write.csv(out_pred, file='result_3_nb.csv', row.names = F) # one best or all model?

# ====decision tree====
model_dt_1<-rpart(label~., data=train1_1_train)
model_dt_2<-rpart(label~., data=train1_2_train)
model_dt_3<-rpart(label~., data=train1_3_train)
model_dt_4<-rpart(label~., data=train1_4_train)
pred_dt_1<-predict(model_dt_1, newdata=train1_1_test, type="class")
pred_dt_2<-predict(model_dt_2, newdata=train1_2_test, type="class")
pred_dt_3<-predict(model_dt_3, newdata=train1_3_test, type="class")
pred_dt_4<-predict(model_dt_4, newdata=train1_4_test, type="class")
cM_dt_1<-table(pred_dt_1, train1_1_test$label)
cM_dt_2<-table(pred_dt_2, train1_2_test$label)
cM_dt_3<-table(pred_dt_3, train1_3_test$label)
cM_dt_4<-table(pred_dt_4, train1_4_test$label)
sum(diag(cM_dt_1))/sum(cM_dt_1)
sum(diag(cM_dt_2))/sum(cM_dt_2)
sum(diag(cM_dt_3))/sum(cM_dt_3)
sum(diag(cM_dt_4))/sum(cM_dt_4)

# ====random forest====
model_rf<-randomForest(label~.,data=train1_1_train)
result<-predict(model_rf,newdata = train1_1_test[,-1],'prob')
head(result)
head(result[,1]) # % of -1
output<-result[,2]
pr<-prediction(output, train1_1_test$label)
perf<-performance(pr,"tpr","fpr")
plot(perf,colorize=TRUE)

dddd<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
ssss<-c(0.45,0.46,0.47,0.48,0.49,0.5,0.51,0.52,0.53,0.54,0.55)
print('2222')
for(i in dddd){ 
  print(i)
  cM<-table(truth=train1_2_test$label, prediction=result[,2]>i)
  print(cM)
  print(cM[1,1]+cM[2,2])
} # threshold=0.47 best
cM<-table(truth=train1_1_test$label, prediction=result[,2]>0.47)
print(cM[1,1]+cM[2,2])
print((cM[1,1]+cM[2,2])/(cM[1,1]+cM[1,2]+cM[2,1]+cM[2,2]))

result<-predict(model_rf,newdata = test1[,-1],'prob')
head(result)
id<-c(0:(length(test1[,2])-1))
head(id)
re<-c()
for(i in 1:length(test1[,2])){
  if(result[i,2]>0.47){
    re<-c(re,1)
  }else{
    re<-c(re,-1)
  }
}
head(re)
out_pred<-data.frame(id=id,label=re)
write.csv(out_pred, file='result_3.csv', row.names = F) # one best or all model?















