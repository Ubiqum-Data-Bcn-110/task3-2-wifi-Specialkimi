#### library upload #### 
library(caret)
library(dplyr)
library(matrixStats)
library(apply)

#install.packages("matrixStats")
install.packages(class)
library(class)
####uploading dataset####
setwd("~/Desktop/tasks /M3T2")

validationData <- read.csv("task3-2-wifi-Specialkimi/validationData.csv",stringsAsFactors=FALSE)
#View(validationData)
trainingData <- read.csv("task3-2-wifi-Specialkimi/trainingData.csv",stringsAsFactors=FALSE)
#View(trainingData)

#### change formats training ####
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
trainingData$BUILDINGID <- factor(trainingData$BUILDINGID,levels = c(0,1,2),labels = c("0","1","2"))
trainingData$FLOOR <- factor(trainingData$FLOOR,levels = c(0,1,2,3,4),labels = c("0","1","2","3","4"))
trainingData$SPACEID <- as.factor(trainingData$SPACEID)
trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)  
trainingData$USERID <- as.factor(trainingData$USERID)
trainingData$LONGITUDE <- as.double(trainingData$LONGITUDE)
trainingData$LATITUDE <- as.double(trainingData$LATITUDE)
trainingData[,1:520] <- as.numeric(unlist(trainingData[,1:520]))
trainingData$FLOOR <- as.factor(trainingData$FLOOR)

#### change formats validation ####
validationData$PHONEID <- as.factor(validationData$PHONEID)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$SPACEID <- as.factor(validationData$SPACEID)
validationData$RELATIVEPOSITION <- as.factor(validationData$RELATIVEPOSITION)  
validationData$USERID <- as.factor(validationData$USERID)
validationData[,1:520] <- as.numeric(unlist(validationData[,1:520]))
validationData$LONGITUDE <- as.double(validationData$LONGITUDE)
validationData$LATITUDE <- as.double(validationData$LATITUDE)
validationData$FLOOR <- as.factor(validationData$FLOOR)


####means for column ####
means_per_column_training <- apply(trainingData[,1:520],2,function(x) mean(x))
col_to_del_train <- which(means_per_column_training == 100)
means_per_column_validation <- apply(validationData[,1:520],2,function(x) mean(x))
col_to_del_valid <- which(means_per_column_validation == 100)

#means for rows ####
means_per_row_training <- apply(trainingData[,1:520],1,function(x) mean(x))
row_to_del_train <- which(means_per_row_training == 100)
means_per_row_validation <- apply(validationData[,1:520],1,function(x) mean(x))
row_to_del_valid <- which (means_per_row_validation == 100)


#### removing null from columns ####
validationData <- validationData[,-col_to_del_valid]
#validationData[, c(3,4,92,93,94,95,152,158,159,160,215,217,226,227,
#                   238,239,240,241,242,243,244,245,246,247,254,293, 296,301,303,304,307,333,349,353)]=NULL

trainingData <- trainingData[,-col_to_del_train]
#trainingData[, c(3,4,92,93,94,95,152,158,159,160,215,217,226,227,
#                 238,239,240,241,242,243,244,245,246,247,254,293, 296,301,303,304,307,333,349,353)]=NULL

#### removing null from rows ####
validationData <- validationData[,]
#validationData[c(5,84,3792,4637,4640,4646,4650,4656,4667,4686,4702,4717,4722,4724,4739,4741,4746,4756,4757,4759,4778,
#                 4794,4799,4804,4813,4827,4850,4856,4857,4881,4885,4902,4907,4909,4915,4929,4930,4932,4967,4982,4983,
#                 5006,5032,5046,5047,5056,5059,5061,5066,5082,5095,5096,5098,5099,5102,5109,5110,5111,5115,5117,
#                 5128,5135,18655,18745,18746,18747,19279,19283,19370,19388,19411,19428,19431,19444,19449,19470), ]=NULL

##### removing same number of waps ####
trainingData <- trainingData[,intersect(names(validationData),names(trainingData))]
validationData <- validationData[,intersect(names(validationData),names(trainingData))]
intersect(names(validationData),names(trainingData))

#### data partition para training #####
library(caret)
muestra <- createDataPartition(trainingData$BUILDINGID, p=0.01, list=FALSE)
trainset <- createDataPartition(trainingData[muestra,]$BUILDINGID, p=0.70, list=FALSE)
df_for_train <- trainingData[trainset,]
df_for_test  <- trainingData[-trainset,]
trainControl<- factor(trainingData[trainset, "classifications"])

dim(df_for_train)
dim(df_for_test)
head(df_for_train)

#### knn for building ####
ctrl <- trainControl(method="repeatedcv",repeats = 3) 
model1knnfit <- train(y=df_for_train$BUILDINGID, x=df_for_train[,1:283], method = "knn", trcontrol = ctrl)
                      
knnPredict1 <- predict(model1knnfit, newdata = validationData )
confusionMatrix(knnPredict1, validationData$BUILDINGID)

model1 <- knn(train = df_for_train, test = df_for_test, trcontrol = ctrl )


sum(apply(trainingData[,1:283],2,function(x) is.numeric(x)))

#### SVM for building #### 
svm_Linear <- train(y=df_for_train$BUILDINGID, x=df_for_train[,1:283], method = "svmLinear",
                         trControl=ctrl,
                         preProcess = c("center", "scale"),
                        tuneLength = 10)

svm_linear_pred <- predict(svm_Linear, validationData[,1:283])

#### create new column with combined building and floor ##
trainingData <- mutate(trainingData,  BF = paste0(BUILDINGID,FLOOR)) 
trainingData$BF <- as.factor(trainingData$BF)
df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
df_for_train$BF <- as.factor(df_for_train$BF)
validationData <- mutate(validationData,BF = paste0(BUILDINGID,FLOOR))
validationData$BF <- factor(validationData$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)

df_for_train <- trainingData[trainset,]
df_for_test  <- trainingData[-trainset,]

# knn for floor
ctrl <- trainControl(method="repeatedcv",repeats = 3) 
model1knnfit_floor <- train(y=df_for_train$BF, x=df_for_train[,1:465], method = "knn", trcontrol = ctrl)
                     #preProcess = c("center","scale"))

knnPredict1_floor <- predict(model1knnfit, newdata = validationData )
confusionMatrix(knnPredict1_floor, validationData$BF)

#### svn for floor ####
svm_Linear_floor <- train(y=df_for_train$BF, x=df_for_train[,1:283], method = "svmLinear", trControl=ctrl,
preProcess = c("center", "scale"), tuneLength = 10)
                    
svm_Linear_floor_pred <-  predict(svm_Linear_floor, validationData[,1:283])
                    
svm_Linear <- train(y=df_for_train$BUILDINGID, x=df_for_train[,1:283], method = "svmLinear",
                    trControl=ctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)





               