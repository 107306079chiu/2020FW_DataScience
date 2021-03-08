library('ggplot2')
library('scales')
library('dplyr') 
library('randomForest') 
library('data.table')
library('gridExtra')
library('GGally')
library('e1071')
train_raw <- read.csv('train.csv', stringsAsFactors = FALSE)
test_raw <- read.csv('test.csv', stringsAsFactors = FALSE)
dim(train_raw)
dim(test_raw)

train <- train_raw
test <- test_raw

# Combining train and test data for quicker data prep
test$SalePrice <- NA
train$isTrain <- 1
test$isTrain <- 0
house <- rbind(train,test)

# 1. missing value 
# MasVnrArea
house$MasVnrArea[which(is.na(house$MasVnrArea))] <- mean(house$MasVnrArea,na.rm=T)

# Alley
house$Alley1 <- as.character(house$Alley)
house$Alley1[which(is.na(house$Alley))] <- "None"
table(house$Alley1)
house$Alley <- as.factor(house$Alley1)
house <- subset(house,select = -Alley1)

# MasVnrType
house$MasVnrType1 <- as.character(house$MasVnrType)
house$MasVnrType1[which(is.na(house$MasVnrType))] <- "None"
house$MasVnrType <- as.factor(house$MasVnrType1)
house <- subset(house,select = -MasVnrType1)
table(house$MasVnrType)

# LotFrontage
house$LotFrontage[which(is.na(house$LotFrontage))] <- median(house$LotFrontage,na.rm = T)

# FireplaceQu
house$FireplaceQu1 <- as.character(house$FireplaceQu)
house$FireplaceQu1[which(is.na(house$FireplaceQu))] <- "None"
house$FireplaceQu <- as.factor(house$FireplaceQu1)
house <- subset(house,select = -FireplaceQu1)

# PoolQC
house$PoolQC1 <- as.character(house$PoolQC)
house$PoolQC1[which(is.na(house$PoolQC))] <- "None"
house$PoolQC <- as.factor(house$PoolQC1)
house <- subset(house,select = -PoolQC1)

# Fence
house$Fence1 <- as.character(house$Fence)
house$Fence1[which(is.na(house$Fence))] <- "None"
house$Fence <- as.factor(house$Fence1)
house <- subset(house,select = -Fence1)

# MiscFeature
house$MiscFeature1 <- as.character(house$MiscFeature)
house$MiscFeature1[which(is.na(house$MiscFeature))] <- "None"
house$MiscFeature <- as.factor(house$MiscFeature1)
house <- subset(house,select = -MiscFeature1)

# GarageType
house$GarageType1 <- as.character(house$GarageType)
house$GarageType1[which(is.na(house$GarageType))] <- "None"
house$GarageType <- as.factor(house$GarageType1)
house <- subset(house,select = -GarageType1)

# GarageYrBlt
house$GarageYrBlt[which(is.na(house$GarageYrBlt))] <- 0

# GarageFinish
house$GarageFinish1 <- as.character(house$GarageFinish)
house$GarageFinish1[which(is.na(house$GarageFinish))] <- "None"
house$GarageFinish <- as.factor(house$GarageFinish1)
house <- subset(house,select = -GarageFinish1)

# GarageQual
house$GarageQual1 <- as.character(house$GarageQual)
house$GarageQual1[which(is.na(house$GarageQual))] <- "None"
house$GarageQual <- as.factor(house$GarageQual1)
house <- subset(house,select = -GarageQual1)

# GarageCond
house$GarageCond1 <- as.character(house$GarageCond)
house$GarageCond1[which(is.na(house$GarageCond))] <- "None"
house$GarageCond <- as.factor(house$GarageCond1)
house <- subset(house,select = -GarageCond1)

# BsmtQual
house$BsmtQual1 <- as.character(house$BsmtQual)
house$BsmtQual1[which(is.na(house$BsmtQual))] <- "None"
house$BsmtQual <- as.factor(house$BsmtQual1)
house <- subset(house,select = -BsmtQual1)

# BsmtCond
house$BsmtCond1 <- as.character(house$BsmtCond)
house$BsmtCond1[which(is.na(house$BsmtCond))] <- "None"
house$BsmtCond <- as.factor(house$BsmtCond1)
house <- subset(house,select = -BsmtCond1)

# BsmtExposure
house$BsmtExposure1 <- as.character(house$BsmtExposure)
house$BsmtExposure1[which(is.na(house$BsmtExposure))] <- "None"
house$BsmtExposure <- as.factor(house$BsmtExposure1)
house <- subset(house,select = -BsmtExposure1)

# BsmtFinType1
house$BsmtFinType11 <- as.character(house$BsmtFinType1)
house$BsmtFinType11[which(is.na(house$BsmtFinType1))] <- "None"
house$BsmtFinType1 <- as.factor(house$BsmtFinType11)
house <- subset(house,select = -BsmtFinType11)

# BsmtFinType2
house$BsmtFinType21 <- as.character(house$BsmtFinType2)
house$BsmtFinType21[which(is.na(house$BsmtFinType2))] <- "None"
house$BsmtFinType2 <- as.factor(house$BsmtFinType21)
house <- subset(house,select = -BsmtFinType21)

# Electrical
house$Electrical1 <- as.character(house$Electrical)
house$Electrical1[which(is.na(house$Electrical))] <- "None"
house$Electrical <- as.factor(house$Electrical1)
house <- subset(house,select = -Electrical1)

# 2. Factorizing
house$MSZoning<- factor(house$MSZoning)
house$Street <- factor(house$Street)
house$LotShape <-factor(house$LotShape )
house$LandContour<-factor(house$LandContour)
house$Utilities<-factor(house$Utilities)
house$LotConfig<-factor(house$LotConfig)
house$LandSlope<-factor(house$LandSlope)
house$Neighborhood<-factor(house$Neighborhood)
house$Condition1<-factor(house$Condition1)
house$Condition2<-factor(house$Condition2)
house$BldgType<-factor(house$BldgType)
house$HouseStyle<-factor(house$HouseStyle)
house$RoofStyle<-factor(house$RoofStyle)
house$RoofMatl<-factor(house$RoofMatl)
house$Exterior1st<-factor(house$Exterior1st)
house$Exterior2nd<-factor(house$Exterior2nd)
house$ExterQual<-factor(house$ExterQual)
house$ExterCond<-factor(house$ExterCond)
house$Foundation<-factor(house$Foundation)
house$Heating<-factor(house$Heating)
house$HeatingQC<-factor(house$HeatingQC)
house$CentralAir<-factor(house$CentralAir)
house$KitchenQual<-factor(house$KitchenQual)
house$Functional<-factor(house$Functional)
house$PavedDrive<-factor(house$PavedDrive)
house$SaleType<-factor(house$SaleType)
house$SaleCondition<-factor(house$SaleCondition)
str(house)

# 3.skew：Taking all the column classes in one variable so as to seperate factors from numerical variables.
Column_classes <- sapply(names(house),function(x){class(house[[x]])})
numeric_columns <-names(Column_classes[Column_classes != "factor"])

#determining skew of each numeric variable

skew <- sapply(numeric_columns,function(x){skewness(house[[x]],na.rm = T)})

# Let us determine a threshold skewness and transform all variables above the treshold.

skew <- skew[skew > 0.75]

# transform excessively skewed features with log(x + 1)

for(x in names(skew)) 
{
  house[[x]] <- log(house[[x]] + 1)
}

# 4. seperate train and test
train <- house[house$isTrain==1,]
test <- house[house$isTrain==0,]
smp_size <- floor(0.75 * nrow(train))

## setting the seed to make the partition reproducible
set.seed(123)

# 5. split train_new and validate from training set(0.75, 0.25)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train_new <- train[train_ind, ]
validate <- train[-train_ind, ]
train_new <- subset(train_new,select=-c(Id,isTrain))
validate <- subset(validate,select=-c(Id,isTrain))

#--------------build model-------------------------------------#

# 1. model by randomforest
library(randomForest)
house_model <- randomForest(SalePrice~., data = train_new)

# 2. Get importance
importance <- importance(house_model)
varImpPlot(house_model)

# 3. evaluation
# Predict using the test set

prediction <- predict(house_model,test)

# Evaluation RMSE function
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

RMSE1 <- RMSE(prediction, validate$SalePrice)
RMSE1
RMSE1 <- round(RMSE1, digits = 5)

prediction[which(is.na(prediction))] <- mean(prediction,na.rm=T)
submit <- data.frame(Id=test$Id,SalePrice=prediction)
write.csv(submit,file="House_Price_Pradeep.csv",row.names=F)
