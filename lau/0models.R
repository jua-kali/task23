##################################################
## Project: task23
## Script purpose: trial different models on data
## Date: 14 Dec 2018
## Author: Lau Stupin
##################################################

library(caret)
library(plotly)
library(reshape2)

set.seed(46)

#### SET THESE PARAMETERS ####

#Set models to try
models <- c('svmRadial', 'rf', 'gbm', 'xgbTree', 'knn') #svmLinear
#Set size of training set
train.size <- .8

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

err.summary <- data.frame()

# Initialize model list
allModels <- list()

# Create training set
inTraining <- createDataPartition(dfsub$Volume, p= train.size, list = FALSE)
training <- dfsub[inTraining, ]
testing <- dfsub[-inTraining, ]


pa <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = training$Volume, 
                name = 'Training',
                nbinsx = 15,
                histnorm = 'percent') %>%
  add_histogram(x = testing$Volume, 
                name = 'Testing', 
                nbinsx = 15,
                histnorm = 'percent') %>%
  layout(title = 'Histogram of Volume',
         barmode = 'overlay',
         xaxis = list(title = 'Volume'),
         yaxis = list(title = 'Percent of Total Count'))
pa

# Cross fold validation
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 2)

#### LOOP THROUGH MODELS ####

for (model_name in models) {
  # Run model
  model <- train(Volume~., data = training,
              method = model_name,
              preProcess = c('center', 'scale'),
              trControl = fitControl)
  
  # Add this model to the list of finished models
  allModels[[model_name]] <- model  
  
  err.summary <- rbind(err.summary, getTrainPerf(model))

  # FOR PLOTTING TEST/TRAIN summary errors
  # foo <- model$results[c('RMSE', 'Rsquared', 'MAE')]
  # foo$model <- model_name
  # foo$set <- '1-train'
  # 
  # trainTest <- rbind(trainTest, foo)
  # 
  # # Predict on TESTING set
  # testing$predVol <- predict(model, testing) 
  # 
  # # Calculate error and add to dataframe
  # bar <- postResample(testing$predVol, testing$Volume)
  # bar <- data.frame(as.list(bar))
  # bar$model <- model_name
  # bar$set <- '2-test'
  # trainTest <- rbind(trainTest, bar)

}

err.summary

# 'svmRadial', 'rf', 'gbm', 'xgbTree', 'knn'

saveRDS(allModels, 'models/80train.mds') 
allModels80 <- readRDS('models/80train.mds')

# Resamples takes 20 random samples to show you the range of error
# It kind of makes sense to do this on the whole data set
results <- resamples(list(rf=allModels$rf,
                          gbt = allModels$xgbTree,
                          knn = allModels$knn,
                          svmRad = allModels$svmRadial,
                          gbm = allModels$gbm))

bwplot(results,
       scales = list(relation = 'free'),
       xlim = list(c(0, 600), c(0, 700), c(0,1)),
       layout = c(1,3)
)


#### BUILD RESIDUAL PLOTS ####
## I'm choosing to do this on the whole data set
dfsimp <- subset(df.old, 
                     select  = c('ProductNum', 
                               'ProductType',      
                               'Price', 
                               'x4StarReviews', 
                               'PositiveServiceReview',
                               'ProfitMargin',
                               'Volume'
                               ))


dfPred <- data.frame()

# Start counter for model names...there's probably a more elegant way to do this
i <- 1

for (model in allModels80) {
  dftemp <- dfsimp
  dftemp$model <- names(allModels80)[i]
  dftemp$VolPred <- predict(model, dftemp)
  dftemp$profit <- dftemp$VolPred * dftemp$ProfitMargin
  
  dfPred <- rbind(dfPred, dftemp)
  i <- i + 1
}

dfPred$residual <- dfPred$VolPred - dfPred$Volume

# Make a pivot table of products, profits
productSummary <- dcast(dfPred,                # Dataframe 
                        ProductType ~ model,   # Rows ~ Columns (can have layers with +)
                        value.var = "profit",  # Table values
                        fun.aggregate = sum)   # Sum the values in the table


## Full plot
p <- plot_ly(data = dfPred,
             x = ~Volume,
             y = ~residual,
             color = ~model)
p

dfRFxgb <- subset(dfPred, subset = (model == 'rf' | model == 'xgbTree'))

p <- plot_ly(data = dfRFxgb,
             x = ~Volume,
             y = ~residual,
             color = ~model)
p

#### PLOT TEST/TRAIN ERRORS ####
## works but need to activate in loop above
# prmse <- trainTest %>%
#   plot_ly() %>%
#   add_trace(x = ~model, y = ~RMSE, type = 'bar',
#             color = ~set)
# 
# pmae <- trainTest %>%
#   plot_ly() %>%
#   add_trace(x = ~model, y = ~MAE, type = 'bar',
#             color = ~set)
# 
# prsq <- trainTest %>%
#   plot_ly() %>%
#   add_trace(x = ~model, y = ~Rsquared, type = 'bar',
#             color = ~set)
# 
# 
# p <- subplot(prmse, pmae, prsq, 
#              nrows = 3,
#              titleY = TRUE)
# 
# p
# e1071, RandomForest, Gbm
