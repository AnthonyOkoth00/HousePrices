library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(randomForest)
train <- read.csv("./train.csv", stringsAsFactors = F)
test <- read.csv("./test.csv", stringsAsFactors = F)
dim(train)
str(train[,c(1:10, 81)]) #display first 10 variables and the response variable
#Getting rid of the ID's but keeping th test IDs in a vector. They are needed to compose the submission file
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
all <- rbind(train, test)
dim(all)

ggplot(data = all[!is.na(all$SalePrice),], aes(x = SalePrice)) + geom_histogram(fill = "blue") +
  scale_x_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)

summary(all$SalePrice)

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
cat("There are ", length(numericVars), "numeric variables")

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[, "SalePrice"], decreasing = TRUE))
# select only high correlation
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col = "black", tl.pos = "lt")
# Correlation between the feture "Overall Quality", which has the highest correlation, and the SalePrice
ggplot(data = all[!is.na(all$SalePrice), ], aes(x = factor(OverallQual), y = SalePrice)) + geom_boxplot() + labs(x = "Overall Quality") + scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)

# Second highest correlation between Grade (Ground) Living Area (square feet) and the Sale Price
ggplot(data = all[!is.na(all$SalePrice), ], aes(x = GrLivArea, y = SalePrice)) + geom_point(col = "blue") + geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
  + scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)

# Get the 2 houses with large living areas and low sale price. Make sure to get the right ones since there are 3 houses close to 4500 square feet
head(all[order(all$GrLivArea[!is.na(all$SalePrice)], decreasing = TRUE), c("SalePrice", "GrLivArea", "OverallQual")], 3)
# Get the features containing missing values
NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
cat("There are", length(NAcol), "columns with missing values")
# Assign "None" to NA's in the PoolQC feature
all$PoolQC[is.na(all$PoolQC)] <- "None"
#Label encode the values in this variable
Qualities <- c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5 )
all$PoolQC <- as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)
# Display observations with PoolArea > 0 and PoolQC == 0
all[all$PoolArea > 0 & all$PoolQC == 0, c("PoolArea", "PoolQC", "OverallQual")]
# Replace PoolQC == 0 with overal quality of the houses.
all$PoolQC[2421] <- 2
all$PoolQC[2505] <- 3
all$PoolQC[2600] <- 2
# Clean MiscFeature
all$MiscFeature[is.na(all$MiscFeature)] <- "None"
all$MiscFeature <- as.factor(all$MiscFeature)
ggplot(all[!is.na(all$SalePrice), ], aes(x = MiscFeature, y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue") + 
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
table(all$MiscFeature)
#Clean Alley Feature
all$Alley[is.na(all$Alley)] <- "None"
all$Alley <- as.factor(all$Alley)
ggplot(all[!is.na(all$SalePrice), ], aes(x = Alley, y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue") +
  scale_y_continuous(breaks = seq(0, 200000, by = 50000), labels = comma)
table(all$Alley)

# Clean Fence Quality feature
all$Fence[is.na(all$Fence)] <- "None"
table(all$Fence)
all$Fence <- as.factor(all$Fence)

# Clean Fireplace Quality feature
all$FireplaceQu[is.na(all$FireplaceQu)] <- "None"
all$FireplaceQu <- as.integer(revalue(all$FireplaceQu, Qualities))
table(all$FireplaceQu)
# Number of fireplaces
table(all$Fireplaces)
sum(table(all$Fireplaces))

# Clean Lot feature
ggplot(all[!is.na(all$LotFrontage), ], aes(x = as.factor(Neighborhood), y = LotFrontage)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
for (i in 1:nrow(all)) {
  if (is.na(all$LotFrontage[i])) { 
    all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood == all$Neighborhood[i]], na.rm = TRUE))
  }
}
#LotShape: No NA's. Values seem ordinal
all$LotShape <- as.integer(revalue(all$LotShape, c("IR3" = 0, "IR2" = 1, "IR1" = 2, "Reg" = 3)))
table(all$LotShape)
sum(table(all$LotShape))
# Lot configuration: Values seemed ordinal but visualization showed otherwise. Convert into a factor
ggplot(all[!is.na(all$SalePrice), ], aes(x = as.factor(LotConfig), y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue") +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
all$LotConfig <- as.factor(all$LotConfig)
table(all$LotConfig)
sum(table(all$LotConfig))
##Garage
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]
#check if all 157 NAs are the same observations among the variables with 157/159 NAs
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))
#find the 2 additional NA's
kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), c("GarageCars", "GarageArea", "GarageType", "GarageCond", "GarageQual", "GarageFinish")])
#Imputing modes.
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]
#display "fixed" house
kable(all[2127, c("GarageYrBlt", "GarageCars", "GarageArea", "GarageType", "GarageCond", "GarageQual", "GarageFinish")])
#fixing 3 values for house 2577
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA
#check if NA's of the character variables are now all 158
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))
# GarageType: Does not seem ordinal so to be converted into factor
all$GarageType[is.na(all$GarageType)] <- "No Garage"
all$GarageType <- as.factor(all$GarageType)
table(all$GarageType)
# GarageFinish: Ordinal values
all$GarageFinish[is.na(all$GarageFinish)] <- "None"
Finish <- c("None" = 0, "Unf" = 1, "RFn" = 2, "Fin" = 3)
all$GarageFinish <- as.integer(revalue(all$GarageFinish, Finish))
table(all$GarageFinish)
# GarageQual: Ordinal
all$GarageQual[is.na(all$GarageQual)] <- "None"
all$GarageQual <- as.integer(revalue(all$GarageQual, Qualities))
table(all$GarageQual)
#GarageCond: Can be made ordinal with the "Qualitites" vector
all$GarageCond[is.na(all$GarageCond)] <- "None"
all$GarageCond <- as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)