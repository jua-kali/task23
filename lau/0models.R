##################################################
## Project: task23
## Script purpose: trial different models on data
## Date: 14 Dec 2018
## Author: Lau Stupin
##################################################

library(caret)
library(plotly)
set.seed(337)

#### SET THESE PARAMETERS ####

#Set models to try
models <- c('lm', 'svmRadial', 'rf', 'gbm', 'xgbTree') #svmLinear
#Set size of training set
train.size <- 1

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
  
  # Add this model to the list of finished models
  allModels[[model_name]] <- model  

  foo <- model$results[c('RMSE', 'Rsquared', 'MAE')]
  foo$model <- model_name
  foo$set <- '1-train'
  
  trainTest <- rbind(trainTest, foo)
  
  # Predict on TESTING set
  testing$predVol <- predict(model, testing) 
  
  # Calculate error and add to dataframe
  bar <- postResample(testing$predVol, testing$Volume)
  bar <- data.frame(as.list(bar))
  bar$model <- model_name
  bar$set <- '2-test'
  trainTest <- rbind(trainTest, bar)

}

results <- resamples(list(lm=allModels$lm, 
                          #svmLinear = allModels$svmLinear,
                          svmRadial = allModels$svmRadial,
                          gbm = allModels$gbm,
                          rf = allModels$rf))
                          # gbt =allModels$xgbTree))

bwplot(results,
       scales = list(relation = 'free'),
       xlim = list(c(0, 600), c(0, 700), c(0,1)),
       layout = c(1,3)
)


summary(results)
#### PLOT ERRORS ####
prmse <- trainTest %>%
  plot_ly() %>%
  add_trace(x = ~model, y = ~RMSE, type = 'bar',
            color = ~set)

pmae <- trainTest %>%
  plot_ly() %>%
  add_trace(x = ~model, y = ~MAE, type = 'bar',
            color = ~set)

prsq <- trainTest %>%
  plot_ly() %>%
  add_trace(x = ~model, y = ~Rsquared, type = 'bar',
            color = ~set)


p <- subplot(prmse, pmae, prsq, 
             nrows = 3,
             titleY = TRUE)

p
# e1071, RandomForest, Gbm
