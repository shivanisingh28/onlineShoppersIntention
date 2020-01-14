setwd("C:\\Users\\shiva\\OneDrive\\Desktop\\Fall 2019 - sem1\\Big Data\\project")
getwd()

###### CHUNK1: Import all libraries #############
library(dplyr)
library(tidyverse)
library(naniar)
library(caret)
library(ggplot2)
library(VIM)
library(class)
library(purrr)
library(caTools)

##### CHUNK2: Read data ################
Data1<- read.csv("online_shoppers_intention.csv")
sum(is.na(Data1))
sum(is.null(Data1))

Data2<-Data1
Data3<-Data1
Data4<-Data1
Data41<-Data1
######################## CHUNK3: EDA   ##############################
#   Total buyers!!!!!
table(Data1$Revenue)
ggplot(Data1, aes(factor(Revenue)))+
  geom_bar(fill='cyan', colour='blue')+
  xlab("REVENUE")+
  ylab("COUNT")+
  ggtitle("Revenue or Not")

#Weekend vs Revenue
ggplot(Data3, aes(Weekend, fill= Revenue))+
  geom_bar(position = "fill")+
  ylab("proportion")
#################################### Different types of Visitors###########################
Data3$VisitorType<- factor(Data3$VisitorType, levels = c("Returning_Visitor","New_Visitor","Other"))
ggplot(Data3, aes(VisitorType))+
  geom_bar(fill="red")+
  ggtitle("Different type of Visitors")

################# Visitors vs Revenue  ###############################3
ggplot(Data3, aes(VisitorType, fill = Revenue))+
  geom_bar(position = "fill")+
  ylab("Proportion")

########################### Diff Browsers #########################################
table(Data3$Browser)
#Data1$Browser<- factor(Data2$Browser, levels = c("1","2","4","5","6","10","Others"))

ggplot(Data3, aes(Browser))+
  geom_density(fill="blue",)+
  xlab("Types of Browser")+
  ggtitle("Different Browsers")

###########################   regions and traffic  #################################
table(Data3$TrafficType)
ggplot(Data3,aes(TrafficType))+
  geom_histogram(fill="green",bins = 20)+
  ggtitle("Distribution of Traffic")

######################### Traffic VS Revenue  ###################################
ggplot(Data3,aes(as.factor(TrafficType), fill= Revenue))+    
  geom_bar(position = "fill")+
  ylab("Proportion")+
  xlab("Traffic Type")
######################### Customers across different Regions  ###########################
table(Data3$Region)
ggplot(Data3,aes(Region))+
  geom_histogram(fill= "green",bins=10)+
  xlab("Different Regions")+
  ggtitle("Customers across different Regions")

ggplot(Data3,aes(TrafficType))+
  geom_histogram(fill="green",bins = 20)+
  facet_wrap(~ Region)

#####################   Regions VS REVENUE ############################
ggplot(Data3, aes(as.factor(Region), fill = Revenue))+
  geom_bar(position = "fill")+
  xlab("Regions")+
  ylab("Proportions")

#####################Product Duration VS Revenue#######################
ggplot(Data3,aes(Revenue,ProductRelated_Duration))+
  geom_jitter(width = 0.25, size=1,col="orange")

#####################Informational Duration VS Revenue#######################
ggplot(Data3,aes(Revenue,Informational_Duration))+
  geom_jitter(width = 0.25, size=1,col="orange")


#####################Admin Duration VS Revenue#######################
ggplot(Data3,aes(Revenue,Administrative_Duration))+
  geom_jitter(width = 0.25, size=1,col="orange")

##################### Exit Rates VS Revenue#######################
ggplot(Data3,aes(Revenue,ExitRates))+
  geom_jitter(width = 0.25, size=1,col="orange")

##################### Page Values VS Revenue#######################
ggplot(Data3,aes(Revenue, PageValues))+
  geom_jitter(width = 0.15, size=1, col="blue")

##################### Bounce Rates VS Revenue#######################
ggplot(Data3,aes(Revenue, BounceRates))+
  geom_jitter(width = 0.15, size=1, col="blue")

#####################Month vs PageValues w.r.t. Revenue#################
Data3$Month<- factor(Data3$Month, levels= c("Feb","Mar","May","June","Jul","Aug","Sep","Oct","Nov","Dec"))
ggplot(Data3,aes(Month, PageValues, col=Revenue))+
  geom_boxplot(outlier.size=0.5, outlier.shape = 10)+
  ggtitle("Month vs PageValue w.r.t. Revenue.")

#####################Month vs ExitRates w.r.t. Revenue#################
Data3$Month<- factor(Data3$Month, levels= c("Feb","Mar","May","June","Jul","Aug","Sep","Oct","Nov","Dec"))
ggplot(Data3,aes(Month, ExitRates, col=Revenue))+
  geom_boxplot(outlier.size=0.5, outlier.shape = 10)+
  ggtitle("Month vs ExitRates w.r.t. Revenue.")

#####################Month vs BounceRates w.r.t. Revenue#################
ggplot(Data3,aes(Month, BounceRates, col=Revenue))+
  geom_boxplot(outlier.size=0.5, outlier.shape = 10)+
  ggtitle("Month vs BounceRates w.r.t. Revenue.")

#######################Visitors Vs BounceRates wrt Revenue######################
ggplot(Data3,aes(VisitorType, BounceRates, col= Revenue))+
  geom_boxplot(outlier.size=0.5, outlier.shape = 10)+
  ggtitle("Visitors Vs BounceRates wrt Revenue")

#######################Visitors Vs ExitRates wrt Revenue######################
ggplot(Data3,aes(VisitorType, ExitRates, col= Revenue))+
  geom_boxplot(outlier.size=0.5, outlier.shape = 10)+
  ggtitle("Visitors Vs Exit Rates wrt Revenue")

#######################Visitors Vs Page Values wrt Revenue######################
ggplot(Data3,aes(VisitorType, PageValues, col= Revenue))+
  geom_boxplot(outlier.size=0.5, outlier.shape = 10)+
  ggtitle("Visitors Vs Page values wrt Revenue")

#################Region VS Exit Rates W.R.T Revenue######################
ggplot(Data3,aes(factor(Region), ExitRates, col= Revenue))+
  geom_boxplot(outlier.size=0.5, outlier.shape = 10)+
  xlab("Regions")
ggtitle("Region Vs Exit Rates wrt Revenue")

#################Region VS Page Values W.R.T Revenue######################  
ggplot(Data3,aes(factor(Region), PageValues, col= Revenue))+
  geom_boxplot(outlier.size=0.5, outlier.shape = 10)+
  xlab("Regions")
ggtitle("Region Vs Page Values wrt Revenue")

###############Region VS Bounce Rates WRT REvenue###################
ggplot(Data3,aes(factor(Region), BounceRates, col= Revenue))+
  geom_boxplot(outlier.size=0.5, outlier.shape = 10)+
  xlab("Regions")
ggtitle("Region Vs Bounce Rates wrt Revenue")



#############################################################################################
###################       PRE - PROCESS    ##################################################
#############################################################################################



######################## CHUNK 4: Imputing missing values ##################################
Data4<-Data1 %>% mutate(Revenue= ifelse(Revenue==TRUE,1,0))



Data2<-VIM::kNN(Data2, variable = c("Administrative","Administrative_Duration","Informational","Informational_Duration","ProductRelated","ProductRelated_Duration","BounceRates","ExitRates"), k=13)
Data2<-subset(Data2, select = Administrative: Revenue)

Data3<-VIM::kNN(Data3, variable = c("Administrative","Administrative_Duration","Informational","Informational_Duration","ProductRelated","ProductRelated_Duration","BounceRates","ExitRates"), k=13)
Data3<-subset(Data3, select = Administrative: Revenue)

Data4<-VIM::kNN(Data4, variable = c("Administrative","Administrative_Duration","Informational","Informational_Duration","ProductRelated","ProductRelated_Duration","BounceRates","ExitRates"), k=13)
Data4<-subset(Data4, select = Administrative: Revenue)

####################################### CHUNK 5: Encoding categorical values
Data4<-Data4 %>% mutate(Revenue= ifelse(Revenue==TRUE,1,0),Weekend= ifelse(Weekend==TRUE,1,0))
Data4<-Data4 %>% mutate(VisitorType= ifelse(VisitorType %in% "Returning_Visitor",0,ifelse(VisitorType %in% "Other",1,ifelse(VisitorType %in% "New_Visitor",2,NA))))
Data4<-Data4 %>% mutate(Month= ifelse(Month %in% "Jan",1,ifelse(Month %in% "Feb",2,ifelse(Month %in% "Mar",3,ifelse(Month %in% "Apr",4,ifelse(Month %in% "May",5,ifelse(Month %in% "Jun",6,ifelse(Month %in% "Jul",7,ifelse(Month %in% "Aug",8,ifelse(Month %in% "Sep",9,ifelse(Month %in% "Oct",10,ifelse(Month %in% "Nov",11,ifelse(Month %in% "Dec",12,0)))))))))))))

Data4<-as.data.frame(apply(Data4,2,as.numeric))
#######################Outlier detecion and removal#############################
Data41 <- Data41[sample(nrow(Data41)),]
#ExitRates column
outliers_ExitRates <- boxplot(Data41$ExitRates)$out
Data41[which(Data41$ExitRates %in% outliers_ExitRates),]
Data41 <- Data41[-which(Data41$ExitRates %in% outliers_ExitRates),]
boxplot(Data41$ExitRates)
#BounceRates column
outliers_BounceRates <- boxplot(Data41$BounceRates)$out
Data41[which(Data41$BounceRates %in% outliers_BounceRates),]
Data41 <- Data41[-which(Data41$BounceRates %in% outliers_BounceRates),]
#PageValues column
outliers_PageValues <- boxplot(Data41$PageValues)$out
Data41[which(Data41$PageValues %in% outliers_PageValues),]
Data41 <- Data41[-which(Data41$PageValues %in% outliers_PageValues),]
##########################  CHUNK6: scaling Z- score  #################


processed_vars<- preProcess(Data4 %>% select(Administrative, Administrative_Duration, Informational, Informational_Duration, ProductRelated, ProductRelated_Duration, BounceRates, ExitRates, PageValues, SpecialDay ),method = c("center","scale"))
Data4 <- predict(processed_vars, Data4)
Data_clean<-Data4
################################################################################################
####################################### CHUNK7: kNN  ##################################################
################################################################################################
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train_knn <- Data4[ind==1,]
test_knn <- Data4[ind==2,]


fitknn<-knn3(Revenue ~.,
             data = train_knn,
             k=11)

predknn<-predict(fitknn,test_knn, type = "class")
print("Performance of KNN:")
print(confusionMatrix(predknn, test_knn$Revenue, positive = "1"))


################################CHUNK8: kNN with SMOTE ############################
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]
library(DMwR)
train<-SMOTE(Revenue~., train, perc.over =500, k = 5, perc.under = 120)
revenue_class<- train$Revenue
actual_class<-test$Revenue
pred<- knn(train = train, test = test, cl= revenue_class, k=13)
table(pred, actual_class)
print("CONFUSION MATRIX FOR KNN:")
print(confusionMatrix(pred,actual_class, positive = "1"))

######################## CHUNK9: 5 FOLD Cross VALIDATION KNN  ############################################
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
trainDF1 <- Data4[ind==1,]
testDF1 <- Data4[ind==2,]

ControlParameters<- trainControl(method = "repeatedcv",
                                 repeats = 5,
                                 number = 5,
                                 savePredictions = TRUE,
)
model_knn_5fold<- train(Revenue~., data= trainDF1,
                        method="knn",
                        tuneLength=13,
                        trControl=ControlParameters,
                        metric= "Accuracy")

pred_knn_5fold<- predict(model_knn_5fold,testDF1)
print("Performance of KNN using 5 fold 5 times:")
print(confusionMatrix(pred_knn_5fold, testDF1$Revenue, positive = "1"))
str(Data4)
################################################################################################
######################## CHUNK10: KNN with 5 fold and SMOTE #############################################
################################################################################################
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
trainDF <- Data4[ind==1,]
testDF <- Data4[ind==2,]

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     search = "grid",
                     
                     returnResamp = "final",
                     savePredictions = "all",
                     sampling = "smote",
)

model_knns_5fold<- train(Revenue~., data= trainDF,
                         method="knn",
                         tuneLength=10,
                         trControl=ctrl)
pred_knns_5fold<- predict(model_knns_5fold,testDF)
print("Performance of KNN using 5 fold 5 times using SMOTE:")
print(confusionMatrix(pred_knns_5fold, testDF$Revenue, positive = "1"))
###############################################################################################################
####################################### CHUNK11: BORUTA FOR FEATURE SELECTION ##########################################
###############################################################################################################
library(Boruta)
library(mlbench)
#Data4$Revenue<-as.factor(Data4$Revenue)
#Data4<-Data_clean
#boruta<- Boruta(Revenue~., data = Data4, doTrace=2, maxRuns= 300)
impfeatures<-getConfirmedFormula(boruta)
print("important features are: ")
print(impfeatures)
getConfirmedFormula(boruta)
plot(boruta, las=2, cex.axis=0.7)

############################# CHUNK12: kNN with important Features ########################################
set.seed(123)
Data4$Revenue<-as.factor(Data4$Revenue)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train_knn <- Data4[ind==1,]
test_knn <- Data4[ind==2,]


fit_knn_imp<-knn3(Revenue ~ Administrative + Administrative_Duration + Informational +
                    Informational_Duration + ProductRelated + ProductRelated_Duration +
                    BounceRates + ExitRates + PageValues + Month + OperatingSystems +
                    Browser + TrafficType + VisitorType,
                  data = train_knn, k=11)

pred_knn_imp<-predict(fit_knn_imp,test_knn, type = "class")
print("KNN with important features from boruta:")
print(confusionMatrix(pred_knn_imp, test_knn$Revenue, positive = "1"))



################ CHUNK13: kNN with SMOTE and important features WITH 5 FOLD ##############################

Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
trainDF1 <- Data4[ind==1,]
testDF1 <- Data4[ind==2,]

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     search = "grid",
                     
                     returnResamp = "final",
                     savePredictions = "all",
                     sampling = "smote",
)

model_smote_boruta_knn<- train(Revenue~Administrative + Administrative_Duration + Informational +
                                 Informational_Duration + ProductRelated + ProductRelated_Duration +
                                 BounceRates + ExitRates + PageValues + Month + OperatingSystems +
                                 Browser + TrafficType + VisitorType,
                               data= trainDF1,
                               method="knn",
                               tuneLength=10,
                               trControl=ctrl)
pred_smote_boruta_knn<- predict(model_smote_boruta_knn,testDF1)
print("KNN with smote in 5 fold using important features:")
print(confusionMatrix(pred_smote_boruta_knn, testDF1$Revenue, positive = "1"))

############################################################################################
########################     CHUNK14: RANDOM FOREST             ####################################
####################### ####################### ####################### ####################
library(randomForest)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train_rf <- Data4[ind==1,]
test_rf <- Data4[ind==2,]  

set.seed(222)
rf<- randomForest(Revenue~., data=train_rf)
print(rf)                    
attributes(rf)

prf<-predict(rf,test_rf)
print("performance of random forest:")
print(confusionMatrix(prf,test_rf$Revenue, positive = "1"))

table(test_rf$Revenue)

########################### CHUNK15:RANDOM FOREST WITH 5 FOLD #########################################
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
trainDF1 <- Data4[ind==1,]
testDF1 <- Data4[ind==2,]

ControlParameters<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5,
                                 search = "grid",
                                 
                                 returnResamp = "final",
                                 savePredictions = "all",
)
parameters<- expand.grid(mtry=c(2,3,4))
rf_5fold<- train(Revenue~., data= trainDF1,
                 method="rf",
                 trControl=ControlParameters,
                 tuneGrid=parameters)
pred_rf_5fold<- predict(rf_5fold,testDF1)
print("Random forest with 5 folds:")
print(confusionMatrix(pred_rf_5fold, testDF1$Revenue, positive = "1"))

##################################### CHUNK16: Random forest with  5 FOLD AND SMOT ####################
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
trainDF1 <- Data4[ind==1,]
testDF1 <- Data4[ind==2,]

ControlParameters<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5,
                                 search = "grid",
                                 
                                 returnResamp = "final",
                                 savePredictions = "all",
                                 sampling = "smote",)
parameters<- expand.grid(mtry=c(2,3,4))
rf_5fold_smote<- train(Revenue~., data= trainDF1,
                       method="rf",
                       trControl=ControlParameters,
                       tuneGrid=parameters)
pred_rf_5fold_smote<- predict(rf_5fold_smote,testDF1)
print("Random forest with 5 folds and smote:")
print(confusionMatrix(pred_rf_5fold_smote, testDF1$Revenue, positive = "1"))

############################### CHUNK17:Random Forest with IMP FEATURES ####################################
library(Boruta)
library(mlbench)
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(111)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train_rf <- Data4[ind==1,]
test_rf <- Data4[ind==2,]




rf_imp<-randomForest(Revenue ~ Administrative + Administrative_Duration + Informational +
                       Informational_Duration + ProductRelated + ProductRelated_Duration +
                       BounceRates + ExitRates + PageValues + Month + OperatingSystems +
                       Browser + TrafficType + VisitorType,
                     data = train_rf,
                     ntree=300,
                     mtry=4)
pred_rf_imp<-predict(rf_imp, test_rf)
print("random forest with important features:")
print(confusionMatrix(pred_rf_imp, test_rf$Revenue, positive = "1"))

################ CHUNK18: RANDOM FOREST WITH SMOTE AND FEATURE SELECTION WITH 5 FOLD #####################

Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
trainDF <- Data4[ind==1,]
testDF <- Data4[ind==2,]

ControlParameters<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5,
                                 search = "grid",
                                 
                                 returnResamp = "final",
                                 savePredictions = "all",
                                 sampling = "smote",)
parameters<- expand.grid(mtry=c(2,3,4))
rf_all<- train(Revenue~Administrative + Administrative_Duration + Informational +
                 Informational_Duration + ProductRelated + ProductRelated_Duration +
                 BounceRates + ExitRates + PageValues + Month + OperatingSystems +
                 Browser + TrafficType + VisitorType,
               data= trainDF1,
               method="rf",
               trControl=ControlParameters,
               tuneGrid=parameters)
predALL<- predict(rf_all,testDF1)
print("Random forest with 5 folds and smote with important features:")
print(confusionMatrix(predALL, testDF1$Revenue, positive = "1"))

##########################################################################################
##################################  CHUNK19:SVM    ###########################################
##########################################################################################
library(e1071)
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]

svmmodel<- svm(Revenue~., data= train, kernel="radial")
predsvm<-predict(svmmodel, test)
print("SVM :")
print(confusionMatrix(predsvm, test$Revenue, positive = "1"))
################################### CHUNK20: SVM WITH 5 FOLD CROSS VALIDATION DONE 5 TIMES ###############
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]

ControlParameters<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5,
                                 search = "grid",
                                 
                                 returnResamp = "final",
                                 savePredictions = "all")

svm_5fold<- train(Revenue~., data= train,
                  method="svmRadial",
                  trControl=ControlParameters)
predsvm_5fold<- predict(svm_5fold,test)
print("SVM with 5 FOLDS:")
print(confusionMatrix(predsvm_5fold, test$Revenue, positive = "1"))



################################### CHUNK21: SVM WITH IMP FEATURES ##########################
library(e1071)
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]

svmmodel_imp<- svm(Revenue~Administrative + Administrative_Duration + Informational +
                     Informational_Duration + ProductRelated + ProductRelated_Duration +
                     BounceRates + ExitRates + PageValues + Month + OperatingSystems +
                     Browser + TrafficType + VisitorType,
                   data= train, kernel="radial")
predsvm_imp<-predict(svmmodel_imp, test)
print("SVM with important features:")
print(confusionMatrix(predsvm_imp, test$Revenue, positive = "1"))

################################## CHUNK22:SVM WITH 5 FOLD 5 TIMES AND SMOTE ###########################
library(e1071)
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]

ControlParameters<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5,
                                 search = "grid",
                                 
                                 returnResamp = "final",
                                 savePredictions = "all",
                                 sampling = "smote",)

svmmodel_5fold_smote<- train(Revenue~., data= train,
                             method="svmRadial",
                             trControl=ControlParameters,
)
predsvm_5fold_smote<- predict(svmmodel_5fold_smote,test)
print("SVM with 5 FOLDS and SMOTE:")
print(confusionMatrix(predsvm_5fold_smote,test$Revenue, positive = "1"))

############### CHUNK23: SVM WITH K FOLD 5 FOLD 5 TIMES WITH SMOTE AND IMPORTANT FEATURES ###############

library(e1071)
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]

ControlParameters<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5,
                                 search = "grid",
                                 
                                 returnResamp = "final",
                                 savePredictions = "all",
                                 sampling = "smote",)

svmmodel_5fold_smote_imp<- train(Revenue~Administrative + Administrative_Duration + Informational +
                                   Informational_Duration + ProductRelated + ProductRelated_Duration +
                                   BounceRates + ExitRates + PageValues + Month + OperatingSystems +
                                   Browser + TrafficType + VisitorType,
                                 data= train,
                                 method="svmRadial",
                                 trControl=ControlParameters)

predsvm_5fold_smote_imp<- predict(svmmodel_5fold_smote_imp,test)
print("SVM with 5 FOLD and SMOTE using important features")
print(confusionMatrix(predsvm_5fold_smote_imp,test$Revenue, positive = "1"))

################################################################################################
########################   CHUNK24: DECISION TREES    ##################################################
################################################################################################
library(rpart)
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]

tree1<- rpart(Revenue~.,
              data= train)
library(rpart.plot)
rpart.plot(tree1)
predtree<- predict(tree1,test, type = "class")
print("performance of desicion trees:")
print(confusionMatrix(predtree, test$Revenue, positive = "1"))

####################CHUNK25: DECISION TRESS WITH 5 FOLD OVER 5 TIMES ####################################

Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]

ControlParameters<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5 )

modeltree_5fold<- train(Revenue~., data= train,
                        method="rpart",
                        trControl=ControlParameters)
predtreecv<- predict(modeltree_5fold, test)
print("Decision Trees with 5 FOLD:")
print(confusionMatrix(predtreecv, test$Revenue, positive = "1"))

##################### CHUNK26: DECISION TREES WITH  IMPORTANT FEATURE #####################
library(rpart)
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]

tree1imp<- rpart(Revenue~Administrative + Administrative_Duration + Informational +
                   Informational_Duration + ProductRelated + ProductRelated_Duration +
                   BounceRates + ExitRates + PageValues + Month + OperatingSystems +
                   Browser + TrafficType + VisitorType,
                 data= train)
library(rpart.plot)
rpart.plot(tree1)
predtreeimp<- predict(tree1imp,test, type = "class")
print("Decision Trees with important Features:")
print(confusionMatrix(predtreeimp, test$Revenue, positive = "1"))

########################## CHUNK27: DECISION TREES WITH SMOTE AND 5 FOLD ###############################
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]

ControlParameters<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5,
                                 sampling = "smote")

modeltreesmote<- train(Revenue~., data= train,
                       method="rpart",
                       trControl=ControlParameters)
predtreesmote<- predict(modeltreesmote, test)
print("Decision Trees with Smote and 5 FOLD :")
print(confusionMatrix(predtreesmote, test$Revenue, positive = "1"))

############################ CHUNK28: DECSION TREES WITH FEATURE IMP, SMOTE AND 5 FOLD #################
Data4<-Data_clean
Data4$Revenue<-as.factor(Data4$Revenue)
set.seed(123)
ind <- sample(2, nrow(Data4), replace = TRUE, prob = c(0.7, 0.3))
train <- Data4[ind==1,]
test <- Data4[ind==2,]

ControlParameters<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5,
                                 sampling = "smote")

modeltreesmote1<- train(Revenue~Administrative + Administrative_Duration + Informational +
                          Informational_Duration + ProductRelated + ProductRelated_Duration +
                          BounceRates + ExitRates + PageValues + Month + OperatingSystems +
                          Browser + TrafficType + VisitorType,
                        data= train,
                        method="rpart",
                        trControl=ControlParameters)
predtreesmote1<- predict(modeltreesmote1, test)
print("Decision Trees 5 FOLD with smote using important features :")
print(confusionMatrix(predtreesmote1, test$Revenue, positive = "1"))

############################################# CHUNK29: CLUSTER ANALYSIS  #######################################

Data_kmeans<-Data4
str(Data_kmeans)

tot_withinss<- map_dbl(1:10, function(k){
  
  model<- kmeans(x=Data_kmeans[,c(2,7)], centers = k)
  model$tot.withinss
})

elbow_df<- data.frame(
  
  k=1:10,
  tot_withinss = tot_withinss
)

eg1<-ggplot(elbow_df, aes(x= k, y=tot_withinss))+
  geom_line()+
  scale_x_continuous(breaks = 1:10)
eg1

set.seed(42)
km<- kmeans(x= Data_kmeans[,c(2,7)], centers = 3)
clusters<- km$cluster
new_df<- mutate(Data_kmeans, cluster=clusters)
str(new_df)

plot1<-ggplot(new_df, aes(x=Administrative_Duration, y=BounceRates, color= factor(cluster)))+
  geom_point()+
  scale_x_continuous()
plot1
####################################### CHUNK 30: Informational VS Bounce##################

tot_withinss<- map_dbl(1:10, function(k){
  
  model<- kmeans(x=Data_kmeans[,c(4,7)], centers = k)
  model$tot.withinss
})

elbow_df<- data.frame(
  
  k=1:10,
  tot_withinss = tot_withinss
)

eg2<-ggplot(elbow_df, aes(x= k, y=tot_withinss))+
  geom_line()+
  scale_x_continuous(breaks = 1:10)
eg2
set.seed(42)
km<- kmeans(x= Data_kmeans[,c(4,7)], centers = 2)
clusters<- km$cluster
new_df<- mutate(Data_kmeans, cluster=clusters)
str(new_df)
plot2<-ggplot(new_df, aes(x=Informational_Duration, y=BounceRates, color= factor(cluster)))+
  geom_point()
plot2
######################### CHUNK31: Administrative Duration Vs Exit RAtes###################################


tot_withinss<- map_dbl(1:10, function(k){
  
  model<- kmeans(x=Data_kmeans[,c(2,8)], centers = k)
  model$tot.withinss
})

elbow_df<- data.frame(
  
  k=1:10,
  tot_withinss = tot_withinss
)
eg3<-ggplot(elbow_df, aes(x= k, y=tot_withinss))+
  geom_line()+
  scale_x_continuous(breaks = 1:10)
eg3

km<- kmeans(x= Data_kmeans[,c(2,8)], centers = 2)
clusters<- km$cluster
new_df<- mutate(Data_kmeans, cluster=clusters)
str(new_df)
plot3<-ggplot(new_df, aes(x=Administrative_Duration, y=ExitRates, color= factor(cluster)))+
  geom_point()
plot3

############################ CHUNK32: Region Vs traffic time ###############################################



tot_withinss<- map_dbl(1:10, function(k){
  
  model<- kmeans(x=Data_kmeans[,c(14,15)], centers = k)
  model$tot.withinss
})
elbow_df<- data.frame(
  
  k=1:10,
  tot_withinss = tot_withinss
)

eg4<-ggplot(elbow_df, aes(x= k, y=tot_withinss))+
  geom_line()+
  scale_x_continuous(breaks = 1:10)
eg4

km<- kmeans(x= Data_kmeans[,c(14,15)], centers = 2)
clusters<- km$cluster
new_df<- mutate(Data_kmeans, cluster=clusters)
str(new_df)
plot4<-ggplot(new_df, aes(x=Region, y= TrafficType, color= factor(cluster)))+
  geom_point()
plot4

####################################### CHUNK33: Administrative Duration Vs Region ###########################################


tot_withinss<- map_dbl(1:10, function(k){
  
  model<- kmeans(x=Data_kmeans[,c(2,14)], centers = k)
  model$tot.withinss
})
elbow_df<- data.frame(
  
  k=1:10,
  tot_withinss = tot_withinss
)

eg5<-ggplot(elbow_df, aes(x= k, y=tot_withinss))+
  geom_line()+
  scale_x_continuous(breaks = 1:10)
eg5

km<- kmeans(x= Data_kmeans[,c(2,14)], centers = 2)
clusters<- km$cluster
new_df<- mutate(Data_kmeans, cluster=clusters)
str(new_df)
plot5<-ggplot(new_df, aes(x=Administrative_Duration, y= Region, color= factor(cluster)))+
  geom_point()
plot5