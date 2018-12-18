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


#### Predict Profits For New Products for all Models ####
dfPred <- data.frame()

# Start counter for model names...there's probably a more elegant way to do this
i <- 1

for (model in allModels80) {
  dftemp <- dfsimp
  dftemp$model <- names(allModels80)[i]
  dftemp$VolPred <- predict(model, dftemp)
  dftemp$profit <- dftemp$VolPred * dftemp$ProfitMargin * dftemp$Price
  
  dfPred <- rbind(dfPred, dftemp)
  i <- i + 1
}

# If negative profit is predicted, change it to zero
dfPred$profit[dfPred$profit < 0 ] <- 0

summary<- dcast(dfPred,                # Dataframe 
                ProductType ~ model,   # Rows ~ Columns (can have layers with +)
                value.var = "profit",  # Table values
                fun.aggregate = sum)   # Sum the values in the table




# Change data shape for plotting
summary.melt <- melt(summary)
summary.melt$ProductType <- as.character(summary.melt$ProductType)
# # Change it back to a factor to control plotting order
summary.melt$ProductType <- factor(summary.melt$ProductType, levels = rows)

#### Calculate current profits ####

current <- df.old[c('ProductType',      
                    'Price', 
                    'x4StarReviews', 
                    'PositiveServiceReview',
                    'ProfitMargin',
                    'Volume'
)]

current$profit <- current$Volume * current$ProfitMargin * current$Price

# Sum profit by category
sum.old <- aggregate(current$profit,  #Value to be summed
                     by = list(current$ProductType), #Category to sum them by
                     FUN = sum)

# Add current profit to summary data frame
summary$existing <- sum.old$x

# Control the order of the plot
plot.order <- c('PC', 'Laptop', 'Netbook', 'Smartphone',
                'GameConsole', 'Tablet',
                'Printer',  
                'Display', 'ExtendedWarranty', 'Software',
                'Accessories', 'PrinterSupplies')
summary$ProductType <- as.character(summary$ProductType)
# # Change it back to a factor to control plotting order
summary$ProductType <- factor(summary$ProductType, levels = plot.order)



#### Everything in condensed dataframe, plot a different trace each column####

# This method comes from: https://plot.ly/r/graphing-multiple-chart-types/
# Adding traces can also be done by a loop, but I left it unlooped for 
# troubleshooting purposes while I was figuring out traces
p <- plot_ly(summary) %>%

  add_trace(x = ~ProductType,
            y = ~gbm,
            type = 'scatter',
            mode = 'markers',
            name = 'gbm',
            marker = list(size = 10)
            )  %>%
  add_trace(x = ~ProductType,
            y = ~knn,
            type = 'scatter',
            mode = 'markers',
            name = 'knn',
            marker = list(size = 10)
            ) %>%
  add_trace(x = ~ProductType,
            y = ~rf,
            type = 'scatter',
            mode = 'markers',
            name = 'rf',
            marker = list(size = 10)
            ) %>%
  add_trace(x = ~ProductType,
            y = ~svmRadial,
            type = 'scatter',
            mode = 'markers',
            name = 'svmRad',
            marker = list(size = 10)
            ) %>%
  add_trace(x = ~ProductType,
            y = ~xgbTree,
            type = 'scatter',
            mode = 'markers',
            name = 'xgbTree',
            marker = list(size = 10)
            ) %>%
  add_trace(x = ~ProductType,
            y = ~existing,
            type = 'bar',
            name = 'existing products',
            marker = list(color = '#C9EFF9')
  ) %>%
  layout(yaxis = list(title = 'Predicted Profit'),
         xaxis = list(title = '')
         )
p


all.products <- as.vector(summary$ProductType)

p <- add_trace(data = rep(100000, 11),
               type = 'bar',
               x = all.products)

p



#### Now fiddling with bar chart ####



#Basic bar graph of current profit
q <- plot_ly(data = sum.old,
             type = 'bar',
             x = ~ Group.1,
             y = ~ x,
             color = c('yellow')
)

q



q <- plot_ly(data = summary$model,
               type = 'scatter', 
               mode = 'markers', 
               marker = list(size = 14),
               x = ~ProductType
               #y = ~value,
               #color = ~variable
)


for (model in colnames(summary)[2:6]){
  p <- add_trace(data = summary$model,
                   type = 'scatter', 
                   mode = 'markers', 
                   marker = list(size = 14),
                   x = ~ProductType
                   #y = ~value,
                   #color = ~variable
                   )
}

p


p <- plot_ly(data = sum.old,
             type = 'bar',
             x = ~ Group.1,
             y = ~ x,
             color = c('yellow')) %>%
  add_trace(data = summary,
            type = 'scatter', 
            mode = 'markers', 
            marker = list(size = 14),
            x = ~ProductType,
            y = ~value,
            color = ~variable) %>%
  layout(yaxis = list(title = 'Predicted Profit'),
         xaxis = list(title = ''))
p










# Filter to interesting products only
# rows <- c('PC', 'Laptop', 'Netbook', 'Smartphone', 
#           'Tablet', 'GameConsole')
# summary <- summary[summary$ProductType %in% rows, ]


# Filter to interesting products only
rows <- c('PC', 'Laptop', 'Netbook', 'Smartphone', 
          'Tablet', 'GameConsole')
summary <- summary[summary$ProductType %in% rows, ]
