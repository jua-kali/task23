##################################################
## Project: task23
## Script purpose: trial different models on data
## Date: 14 Dec 2018
## Author: Lau Stupin
##################################################

library(caret)
set.seed(42)

#### SET THESE PARAMETERS ####

#Set models to try
models <- c('lm', 'svmRadial', 'svmLinear') #svmLinear
#Set size of training set
train.size <- 0.8

#### DATA ASSUMPTIONS ####

df.old <- read.csv('../data/0existing_products_cleaned.csv')

# Drop outliers 150 & 198
df.old <- subset(df.old, ProductNum != 150 & ProductNum != 198)

## Drop all variables except 4star and PosService
dfsub <- df.old[c('x4StarReviews', 'PositiveServiceReview', 'Volume')]

#### INITIALIZE LOOP OBJECTS ####

# Initialize error dataframe
trainTest <- data.frame(model = character (),
                        set = character(),
                        RMSE = double(),
                        RSquared = double(),
                        MAE = double())

# Initialize model list
allModels <- list()

# Create training set
inTraining <- createDataPartition(dfsub$Volume, p= train.size, list = FALSE)
training <- dfsub[inTraining, ]
testing <- dfsub[-inTraining, ]

# Cross fold validation
fitControl <- trainControl(method = 'repeatedCV', number = 10, repeats = 2)

#### LOOP THROUGH MODELS ####

for (model_name in models) {
  # Run model
  model <- train(Volume~., data = training,
              method = model_name,
              preProcess = c('center', 'scale'),
              trControl = fitControl)
  
  # Add this model to the list of models
  allModels[[model_name]] <- model  

  foo <- model$results[c('RMSE', 'Rsquared', 'MAE')]
  foo$model <- model_name
  foo$set <- 'train'
  
  trainTest <- rbind(trainTest, foo)
  
  # Predict on TESTING set
  testing$predVol <- predict(model, testing) 
  
  # Calculate error and add to dataframe
  bar <- postResample(testing$predVol, testing$Volume)
  bar <- data.frame(as.list(bar))
  bar$model <- model_name
  bar$set <- 'test'
  trainTest <- rbind(trainTest, bar)

}

trainTest
trainTest$plotName <- paste(trainTest$model, trainTest$set)




# e1071, RandomForest, Gbm
