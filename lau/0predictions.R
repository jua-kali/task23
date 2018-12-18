##################################################
## Project: task23
## Script purpose: use models to make predictions
## Date: 14 Dec 2018
## Author: Lau Stupin
##################################################


library(caret)
library(plotly)
library(reshape2)

set.seed(46)


#### DATA ASSUMPTIONS ####

# Load existing products
df.old <- read.csv('../data/0existing_products_cleaned.csv')
# Drop outliers 150 & 198
df.old <- subset(df.old, ProductNum != 150 & ProductNum != 198)

# Load new products
df.new <- read.csv('/home/chief/0githubi/task23/data/0new_products_cleaned.csv')
# Select only relevant columns
dfsimp <- df.new[c('ProductNum', 
                       'ProductType',      
                       'Price', 
                       'x4StarReviews', 
                       'PositiveServiceReview',
                       'ProfitMargin'
                       )]
  
  
# Load models
allModels80 <- readRDS('models/80train.mds')


# Generate prediction for all models
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
