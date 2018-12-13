# This file is for cleaning and standardizing the data


#### CLEAN DATA ####

df.old <- read.csv('data/existingproductattributes2017.csv')
df.new <- read.csv('data/newproductattributes2017.csv')

# All ints/factors seem okay 
# str(df.old)

#### EXISTING ONLY ####

# Drop extra extended warranties 
df.old <- subset(df.old, ! ProductNum %in% c(135, 136, 137, 138, 139, 140, 141))

# Change remaining warranty price to the average
df.old[df.old$ProductNum == 134, 'Price'] <- 178.92

#### BOTH EXISTING AND NEW PRODUCTS ####

# Create dummy variables for product type

# Existing products
df <- df.old
try <- dummyVars('~.', data = df)
df<- data.frame(predict(try, newdata = df))
df.old <- df

# Predicted products
df <- df.new
try <- dummyVars('~.', data = df)
df<- data.frame(predict(try, newdata = df))
df.new <- df


# Delete Best Sellers Rank

# 20% of Best Seller's rank info is missing
# This is too much to impute the values, so we will drop BestSellersRank
# sum(is.na(df.old$BestSellersRank))/length(df.old$BestSellersRank)

# Drop variable
df.old$BestSellersRank <- NULL
df.new$BestSellersRank <- NULL

# Write out csvs with clean data
write.csv(df.old, file = 'existing_products_cleaned.csv' )
write.csv(df.new, file = 'new_products_cleaned.csv' )

#### CORRELATION PLOT ####
corrData <-cor(df.old)

library(corrplot)
corrplot(corrData, tl.pos = 'n', 
         type = 'upper', 
         method = 'color',
         #addCoef.col = 'black',
         diag = FALSE)

