pacman::p_load(caret,readr,ggplot2,lattice,rmarkdown,C50,corrplot,tidyr,randomForest)

#### data set ####
df.old <- read.csv("C:/Users/Arnau/Documents/Task2-3Lau/task23/data/0existing_products_cleaned.csv")
df.new <- read.csv("C:/Users/Arnau/Documents/Task2-3Lau/task23/data/0new_products_cleaned.csv")

#### Eliminació outliers ####

df.old <- df.old[which(df.old$Volume < 5999),]

#### Variable importance ####
# Elimanació de les variables numeriques per fer la correlation matrix #

df.old <- within(df.old, rm(X,ProductType,X,ProductNum,Price,x5StarReviews,x3StarReviews,x2StarReviews,x1StarReviews,NegativeServiceReview,Recommendproduct,ShippingWeight,ProductDepth,ProductWidth,ProductHeight,ProfitMargin))

#### Decision tree ####

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
Random_Forest <- train(Volume~., data = df.old,
                       method = "rf", trControl = fitControl,
                       importance = TRUE)
Random_Forest

varImp(Random_Forest, scale = FALSE)

#### correlation matrix ####

corrData <- cor(df.old)

corrplot(corrData, method = "number", type = "lower")

#### dummify the data ####

newDataFrame <- dummyVars(" ~ .", data = df.old)
readyData <- data.frame(predict(newDataFrame, newdata = df.old))

#### Correlacio amb les dummies ####

corrData <- cor(readyData)

corrplot(corrData, method = "number", type = "lower")

#### Linear Model ####

set.seed(998)
inTraining <- createDataPartition(df.old$Volume, p = .75, list = FALSE)
training <- df.old[inTraining,]
testing <- df.old[-inTraining,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
linear_model <- train(Volume~.,
                      data = training,
                      method = "lm",
                      trControl=fitControl)
linear_model

summary(linear_model)

           
Pred_training <- predict(linear_model,training)
Pred_training

summary(Pred_training)


# Comparar pred_testing con testing$Volume para saber el errror de nuestra prediccion

Pred_testing <- predict(linear_model,testing)
Pred_testing

testing$Volume

# El error 
postResample(Pred_training,testing$Volume)

                  
#### Random forest ####

set.seed(998)
inTraining <- createDataPartition(df.old$Volume,p = .75, list = FALSE)
training <- df.old[inTraining,]
testing <- df.old[-inTraining,]
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 1)
Random_Forest <- train(Volume~.,
                       data = training,
                       method = "rf",
                       trControl = fitControl,
                       preProcess = c("range"))
Random_Forest

Pred_training_RF <- predict(Random_Forest,training)

Pred_testing_RF <- predict(Random_Forest,testing)
Pred_testing_RF

testing$Volume

postResample(Pred_testing_RF,testing$Volume)


#### Gradient Boosted Tree ####

set.seed(998)
inTraining <- createDataPartition(df.old$Volume, p = .75, list = FALSE)
training <- df.old[inTraining,]
testing <- df.old[-inTraining,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
Gradient_Boosted_Tree <- train(Volume~.,
                               data = training,
                               method = "xgbTree",
                               trControl = fitControl,
                               preProcess = c("range"))
Gradient_Boosted_Tree

Pred_training_GBT <- predict(Gradient_Boosted_Tree,training)


Pred_testing_GBT <- predict(Gradient_Boosted_Tree,testing)
Pred_testing_GBT

testing$Volume

postResample(Pred_testing_GBT,testing$Volume)


#### SVM ####

set.seed(998)
inTraining <- createDataPartition(df.old$Volume, p = .75, list = FALSE)
training <- df.old[inTraining,]
testing <- df.old[-inTraining,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
SVM <- train(Volume~.,
             data = training,
             method = "svmLinear",
             trControl = fitControl,
             preProcess = (c("range")))
SVM

Pred_training_SVM <- predict(SVM,training)


Pred_testing_SVM <- predict(SVM,testing)
Pred_testing_SVM

testing$Volume

postResample(Pred_testing_SVM,testing$Volume)

#### K-NN ####

set.seed(998)
inTraining <- createDataPartition(df.old$Volume, p = 0.75, list = FALSE)
training <- df.old[inTraining,]
testing <- df.old[-inTraining,]
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 1)
KNN <- train(Volume~.,
             data = training,
             method = "knn",
             trControl = fitControl,
             preProcess = "range")
KNN

Pred_training_KNN <- predict(KNN,training)

Pred_testing_KNN <- predict(KNN, testing)
Pred_testing_KNN

testing$Volume


postResample(Pred_testing_KNN,testing$Volume)



#### Best model prediction ####
# GBM #

FinalPrediction <- predict(Gradient_Boosted_Tree,df.new)
FinalPrediction
summary(FinalPrediction)


#### Matriu errors ####


RFpred <- data.frame(Prediction.RF = Pred_testing_RF)
GBTpred <- data.frame(Prediction.GBT = Pred_testing_GBT)
SVMpred <- data.frame(Prediction.SVM = Pred_testing_SVM)
KNNpred <- data.frame(Prediction.KNN = Pred_testing_KNN)

TestingReal <- data.frame(testing$Volume)

Error.RF <- data.frame(RFpred- TestingReal)
Error.GBT <- data.frame(GBTpred - TestingReal)
Error.SVM <- data.frame(SVMpred - TestingReal)
Error.KNN <- data.frame(KNNpred - TestingReal)

names(Error.RF) <- c("Error.RF")
names(Error.GBT) <- c("Error.GBT")
names(Error.SVM) <- c("Error.SVM")
names(Error.KNN) <- c("Error.KNN")

Error <- data.frame(Error.RF,Error.GBT,Error.SVM,Error.KNN)

Matriu <- cbind(TestingReal, RFpred, GBTpred, SVMpred, KNNpred, Error)

ggplot(Matriu,aes(x = Prediction, y = Error.RF))





