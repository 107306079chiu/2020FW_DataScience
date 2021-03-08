# reference:
# https://www.rdocumentation.org/packages/e1071/versions/1.7-4/topics/naiveBayes
# https://www.kaggle.com/duykhanh99/lightgbm-starter-with-r-0-9493-lb

# catalog:
# 1 data
# 2 naive bayes
# 3 LGB
# 4 null model
# 5 knn

library('corrplot')
require('e1071')
require('data.table')
require('caret')
library(lightgbm)
library(class)
library(ROCR)
library(caret)

# 1 data
# reading data
traindata <- fread("train.csv")
testdata <- fread("test.csv")
sub <- data.frame(read_csv("sample_submission.csv"))

# 2 naive bayes
# Training naiveBayes model
Model1 <- naiveBayes( as.factor(isFraud) ~ .,data = traindata)

# predict training data set.
resultframe <- data.frame(truth= traindata$isFraud,
                          pred=predict(Model1, traindata))
rtab <- table(resultframe)
# confusion matrix
cm <- confusionMatrix(data = as.factor(resultframe$pred), reference = as.factor(resultframe$truth))

# predict test dataset
pred1 = predict(Model1, testdata)
# output result file
output <- data.frame(TransactionID = as.integer(testdata$TransactionID),
                     isFraud = pred1)
write.table(output, file='submit.csv', row.names = F, quote = F,sep = ',')

# draw the confusion matrix diagram
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Naivebayes', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  
draw_confusion_matrix(cm)

# 3 LGB
# data
train<-traindata
test<-testdata
y<-train$isFraud
train$isFraud<-NULL

# run LGB
tr_idx<-c(1:100000)
v_idx<-c(100001:130000)
d0 <- lgb.Dataset(data.matrix(train[tr_idx,]), label = y[tr_idx] )
dval <- lgb.Dataset(data.matrix(train[v_idx,]), label = y[v_idx] ) 
lgb_param <- list(boosting_type = 'dart',
                  objective = "binary" ,
                  metric = "AUC",
                  boost_from_average = "false",
                  tree_learner  = "serial",
                  max_depth = -1,
                  learning_rate = 0.01,
                  num_leaves = 197,
                  feature_fraction = 0.3,          
                  bagging_freq = 1,
                  bagging_fraction = 0.7,
                  min_data_in_leaf = 100,
                  bagging_seed = 11,
                  max_bin = 255,
                  verbosity = -1)
valids <- list(valid = dval)
lgb0 <- lgb.train(params = lgb_param,  data = d0, nrounds = 1000, 
                  eval_freq = 200, valids = valids, early_stopping_rounds = 400, verbose = 1, seed = 123)
oof_pred <- predict(lgb0, data.matrix(train[v_idx,]))
pr<-prediction(oof_pred, y[v_idx])
perf<-performance(pr,"tpr","fpr")
plot(perf,colorize=TRUE)

# full data
d0 <- lgb.Dataset(data.matrix(train), label = y )
lgb <- lgb.train(params = lgb_param, data = d0, nrounds = 100, verbose = -1, seed = 123) #nrounds = lgb$best_iter * 1.05
pred <- predict(lgb, data.matrix(test))

# output
sub[,2] <- pred
sub[,1] <- as.integer(sub[,1])
write.csv(sub,"submission.csv",row.names = F)

# 4 null model
null_pred<-c()
for(i in 1:length(test[,2])){
  null_pred<-c(null_pred,runif(1))
}
sub[,2]<-null_pred
write.csv(sub,"submission_null.csv",row.names = F)

# 5 knn
#with smaller dataset 
index<-sample(1:100000,10000)
trn_idx<-index[1:8000]
vali_idx<-index[8001:10000]
train1<-train[trn_idx,]
vali1<-train[vali_idx,]
y_train1<-y[trn_idx]
y_vali1<-y[vali_idx]
k2<-knn(train1,vali1,cl=y_train1,k=2,prob=TRUE) 
cm2<-table(k2,y_vali1)
sum(diag(cm2))/sum(cm2)
#plot
y_vali1<-as.numeric(y_vali1)
k2<-as.numeric(k2)
library(ROCR)
pr<-prediction(k2, y_vali1)
perf<-performance(pr,"tpr","fpr")
plot(perf,colorize=TRUE)

#full data
k<-knn(train,test,cl=y,k=2) 
out<-data.frame(TransactionID=as.integer(test[,2]),isFraud=k)
write.csv(out, file='result_knn.csv', row.names = F)
