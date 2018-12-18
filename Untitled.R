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


####100means for training ####
means_per_column_training <- apply(trainingData[,1:520],2,function(x) mean(x))
col_to_del_train <- which(means_per_column_training == 100)
means_per_column_validation <- apply(validationData[,1:520],2,function(x) mean(x))
col_to_del_valid <- which(means_per_column_validation == 100)

#means for rows
means_per_row_training <- apply(trainingData[,1:520],1,function(x) mean(x))
row_to_del_train <- which(means_per_row_training == 100)
means_per_row_validation <- apply(validationData[,1:520],1,function(x) mean(x))
row_to_del_valid <- which (means_per_row_validation == 100)


#removing null from columns
validationData <- validationData[,-col_to_del_valid]
#validationData[, c(3,4,92,93,94,95,152,158,159,160,215,217,226,227,
#                   238,239,240,241,242,243,244,245,246,247,254,293, 296,301,303,304,307,333,349,353)]=NULL

trainingData <- trainingData[,-col_to_del_train]
#trainingData[, c(3,4,92,93,94,95,152,158,159,160,215,217,226,227,
#                 238,239,240,241,242,243,244,245,246,247,254,293, 296,301,303,304,307,333,349,353)]=NULL


validationData <- validationData[,]
#validationData[c(5,84,3792,4637,4640,4646,4650,4656,4667,4686,4702,4717,4722,4724,4739,4741,4746,4756,4757,4759,4778,
#                 4794,4799,4804,4813,4827,4850,4856,4857,4881,4885,4902,4907,4909,4915,4929,4930,4932,4967,4982,4983,
#                 5006,5032,5046,5047,5056,5059,5061,5066,5082,5095,5096,5098,5099,5102,5109,5110,5111,5115,5117,
#                 5128,5135,18655,18745,18746,18747,19279,19283,19370,19388,19411,19428,19431,19444,19449,19470), ]=NULL

muestra <- createDataPartition(trainingData$BUILDINGID, p=0.2, list=FALSE)
#fuera <- setdiff(1:nrow(trainset),muestra)
df_muestra <- trainingData[muestra,]
trainset <- createDataPartition(df_muestra$BUILDINGID, p=0.70, list=FALSE)
df_for_train <- df_muestra[trainset,]
#df_for_test  <- trainingData[-trainset,]
df_for_test  <- df_muestra[-trainset,]

#### create new column with combined building and floor ##
trainingData <- mutate(trainingData,  BF = paste0(BUILDINGID,FLOOR)) 
trainingData$BF <- as.factor(trainingData$BF)
df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
df_for_train$BF <- as.factor(df_for_train$BF)
validationData <- mutate(validationData,BF = paste0(BUILDINGID,FLOOR))
validationData$BF <- factor(validationData$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)

##### intersect ####
trainingData <- trainingData[,intersect(names(validationData),names(trainingData))]
validationData <- validationData[,intersect(names(validationData),names(trainingData))]
intersect(names(validationData),names(trainingData))

#### splitting by building
Build0training <- (filter(trainingData, BUILDINGID == 0))
Build0training$BF <- factor(Build0training$BF)
Build1training<- (filter(trainingData, BUILDINGID == 1))
Build1training$BF <- factor(Build1training$BF)
Build2training <- (filter(trainingData, BUILDINGID == 2))
Build2training$BF <- factor(Build2training$BF)

####100means for B0 ####
means_per_column_training_B0 <- apply(Build0training[,1:312],2,function(x) mean(x))
col_to_del_train_B0 <- which(means_per_column_training_B0 == 100)
means_per_row_training_B0 <- apply(trainingData[,1:312],1,function(x) mean(x))
row_to_del_train_B0 <- which(means_per_row_training_B0 == 100)
Build0training <- Build0training[,-col_to_del_train_B0]
#trainingData[, c(3,4,92,93,94,95,152,158,159,160,215,217,226,227,
#                 238,239,240,241,242,243,244,245,246,247,254,293, 296,301,303,304,307,333,349,353)]=NULL

####BF B0 ##
Build0training <- mutate(Build0training,  BF = paste0(BUILDINGID,FLOOR)) 
Build0training$BF <- as.factor(Build0training$BF)
#df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
#df_for_train$BF <- as.factor(df_for_train$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)

####100means for B1 ####
means_per_column_training_B1 <- apply(Build1training[,1:312],2,function(x) mean(x))
col_to_del_train_B1 <- which(means_per_column_training_B1 == 100)
means_per_row_training_B1 <- apply(Build1training[,1:312],1,function(x) mean(x))
row_to_del_train_B1 <- which(means_per_row_training_B1 == 100)

####BF B1 ##
Build1training <- mutate(Build1training,  BF = paste0(BUILDINGID,FLOOR)) 
Build1training$BF <- as.factor(Build1training$BF)
#df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
#df_for_train$BF <- as.factor(df_for_train$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)

####100means for B2 ####
means_per_column_training_B2 <- apply(Build2training [,1:312],2,function(x) mean(x))
col_to_del_train_B2 <- which(means_per_column_training_B2 == 100)
means_per_row_training_2 <- apply(Build2training[,1:312],1,function(x) mean(x))
row_to_del_train_B2 <- which(means_per_row_training_2 == 100)

####BF B2 ##
Build2training <- mutate(Build2training,  BF = paste0(BUILDINGID,FLOOR)) 
Build2training$BF <- as.factor(Build2training$BF)
#df_for_train <- mutate(df_for_train,  BF = paste0(BUILDINGID,FLOOR)) 
#df_for_train$BF <- as.factor(df_for_train$BF)
#df_for_train$BF <- as.factor(group_indices(df_for_train, BUILDINGID, FLOOR))
#df_for_train$FLOOR <- as.factor(df_for_train$FLOOR)

dim(df_for_train)
dim(df_for_test)
head(df_for_train)



#### knn for building ####
ctrl <- trainControl(method="repeatedcv",number = 10, repeats = 1) 
#model1knnfit <- train(y=df_for_train$BUILDINGID, x=df_for_train[,1:312], method = "knn", trcontrol = ctrl)

#knnPredict1 <- predict(model1knnfit, newdata = validationData )
confusionMatrix(knnPredict1, validationData$BUILDINGID)

#model1 <- knn(train = df_for_train, test = df_for_test, trcontrol = ctrl )


#sum(apply(trainingData[,1:283],2,function(x) is.numeric(x)))

#### SVM for building #### 
svm_Linear <- train(y=df_for_train$BUILDINGID, x=df_for_train[,1:312], method = "svmLinear",
                    trControl=ctrl,
                    #preProcess = c("center", "scale"),
                    tuneLength = 10)

saveRDS(svm_Linear,file="svm_linear.rds")

svm_linear_pred <- predict(svm_Linear, validationData[,1:312])
confusionMatrix(svm_linear_pred, validationData$BUILDINGID)

#### dt for building ####
dt_training_BUILDING <- rpart(BUILDINGID~., 
             method="class", data=df_for_train[,c(1:312,316)])



df_for_train <- trainingData[trainset,]
df_for_test  <- trainingData[-trainset,]

# knn for floor
ctrl <- trainControl(method="repeatedcv", number=10 ,repeats = 3) 
model1knnfit_floor <- train(y=df_for_train$BF, x=df_for_train[,1:313], method = "knn", trcontrol = ctrl)
kk <- train(BF~.,data=df_for_train[,c(1:312,322)], method = "knn", trcontrol = ctrl)
#preProcess = c("center","scale"))

knnPredict1_floor <- predict(model1knnfit, newdata = validationData )
confusionMatrix(knnPredict1_floor, validationData$BF)

#### svn for floor ####
svm_Linear_floor <- train(y=df_for_train$BF, x=df_for_train[,1:313], method = "svmLinear", trControl=ctrl,
                          tuneLength = 10)

save(svm_Linear_floor,file="modelo_svm_linnear_floor.rds")

svm_Linear_floor_pred <-  predict(svm_Linear_floor, validationData[,1:312])
confusionMatrix(svm_Linear_floor_pred, validationData$BF)

#### svn for floor splitted by building ####
svm_Linear_floor_B0 <- train(y=Build0training$BF, x=Build0training[,1:312], method = "svmLinear", trControl=ctrl,
                          preProcess = c("center", "scale"), tuneLength = 10)

save(svm_Linear_floor)

svm_Linear_floor_B0_pred <-predict (svm_Linear_floor_B0, validationData[,c(1:312)])
confusionMatrix(svm_Linear_floor_B0_pred, validationData$BF)

svm_Linear_floor_B1 <- train(y=Build1training$BF, x=Build1training[,1:312], method = "svmLinear", trControl=ctrl,
                             preProcess = c("center", "scale"), tuneLength = 10)



svm_Linear_floor_B1_pred <-predict (svm_Linear_floor_B1, validationData[,c(1:312)])
confusionMatrix(svm_Linear_floor_B1_pred, validationData$BF)

svm_Linear_floor_B2 <- train(y=Build2training$BF, x=Build2training[,1:312], method = "svmLinear", trControl=ctrl,
                             preProcess = c("center", "scale"), tuneLength = 10)



svm_Linear_floor_B2_pred <-predict (svm_Linear_floor_B2, validationData[,c(1:312)])
confusionMatrix(svm_Linear_floor_B2_pred, validationData$BF)


#### mrls for LATITUDE #### 
LM_train_Latitude <- lm(LATITUDE ~ ., data=df_for_train[,c(1:312,314)])
LM_train_Latitude_pred <- predict(LM_train_Latitude)

postResample(LM_train_Latitude_pred, validationData$LATITUDE)

summary(LM_train_Latitude)
print(LM_train_Latitude)


#### mrls for LONGITUDE #### 
LM_train_Longitude <- lm(LONGITUDE ~ ., data=df_for_train,c(1:313))
LM_train_Longitude_pred <- predict(LM_train_Longitude)
postResample(LM_train_Longitude_pred, validationData$LONGITUDE)
summary(LM_train_Longitude)
print(LM_train_Longitude)

