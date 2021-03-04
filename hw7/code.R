library(ggplot2)
library(randomForest)
library(ROCR)

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
prop.table(table(train1$label)) # no serious data imbalance -1:1=65:35
train1$label<-as.factor(train1$label)

# 4 build model
set.seed(2021)
index<-sample(1:length(train1[,2]),length(train1[,2])*0.2)
train1_test<-train1[index,]
train1_train<-train1[-index,]
model<-randomForest(
  label~.,
  data=train1_train
)
result<-predict(model,newdata = train1_test[,-1],'prob')
head(result)
head(result[,1]) # % of -1
output<-result[,2]
pr<-prediction(output, train1_test$label)
perf<-performance(pr,"tpr","fpr")
plot(perf,colorize=TRUE)

for(i in c(0.45,0.46,0.47,0.48,0.49,0.5,0.51,0.52,0.53,0.54,0.55)){ 
  print(i)
  cM<-table(truth=train1_test$label, prediction=result[,2]>i)
  print(cM[1,1]+cM[2,2])
} # threshold=0.47 best
cM<-table(truth=train1_test$label, prediction=result[,2]>0.47)
print(cM[1,1]+cM[2,2])
print((cM[1,1]+cM[2,2])/(cM[1,1]+cM[1,2]+cM[2,1]+cM[2,2]))

result<-predict(model,newdata = test1[,-1],'prob')
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
write.csv(out_pred, file='result.csv', row.names = F)
