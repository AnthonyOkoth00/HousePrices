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
