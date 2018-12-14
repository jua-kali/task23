##################################################
## Project: task23
## Script purpose: trial different models on data
## Date: 14 Dec 2018
## Author: Lau Stupin
##################################################

library(caret)
set.seed(42)

df.old <- read.csv('../data/0existing_products_cleaned.csv')

# Drop outliers 150 & 198
df.old <- subset(df.old, ProductNum != 150 & ProductNum != 198)

## Drop all variables except 4star and PosService
dfsub <- df.old[c('x4StarReviews', 'PositiveServiceReview', 'Volume')]

# Initialize error dataframe

trainTest <- data.frame(model = character (),
                        set = character(),
                        RMSE = double(),
                        RSquared = double(),
                        MAE = double())

# Create training set
inTraining <- createDataPartition(dfsub$Volume, p=.8, list = FALSE)
training <- dfsub[inTraining, ]
testing <- dfsub[-inTraining, ]

# Cross fold validation
fitControl <- trainControl(method = 'repeatedCV', number = 10, repeats = 2)

# Linear model
lm <- train(Volume~., data = training,
            method = 'lm',
            trControl = fitControl)



foo <- lm$results[c('RMSE', 'Rsquared', 'MAE')]
foo$model <- 'lm'
foo$set <- 'train'

trainTest <- rbind(trainTest, foo)

# Predict on TESTING set
testing$predVol <- predict(lm, testing) 

# Calculate error and add to dataframe
bar <- postResample(testing$predVol, testing$Volume)
bar <- data.frame(as.list(bar))
bar$model <- 'lm'
bar$set <- 'test'
trainTest <- rbind(trainTest, bar)

trainTest




# e1071, RandomForest, Gbm
