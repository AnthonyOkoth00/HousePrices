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
## Basement variables
#check if all 79 NAs are the same observations among the variables with 80+ NAs
length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) & is.na(all$BsmtExposure) & is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))
#Find the additional NAs; BsmtFinType1 is the one with 79 NAs
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond) | is.na(all$BsmtQual) | is.na(all$BsmtExposure) | is.na(all$BsmtFinType2)), c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")]
#imputing modes
all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]
# Make BsmtQual ordinal with Qualities vector
all$BsmtQual[is.na(all$BsmtQual)] <- "None"
all$BsmtQual <- as.integer(revalue(all$BsmtQual, Qualities))
table(all$BsmtQual)
# Make BsmtCond ordinal with Qualities vector
all$BsmtCond[is.na(all$BsmtCond)] <- "None"
all$BsmtCond <- as.integer(revalue(all$BsmtCond, Qualities))
table(all$BsmtCond)
#Make BsmtExposure ordinal
all$BsmtExposure[is.na(all$BsmtExposure)] <- "None"
Exposure <- c("None" = 0, "No" = 1, "Mn" = 2, "Av" = 3, "Gd" = 4)
all$BsmtExposure <- as.integer(revalue(all$BsmtExposure, Exposure))
table(all$BsmtExposure)
# BsmtFinType1: Can be made ordinal
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- "None"
FinType <- c("None" = 0, "Unf" = 1, "LwQ" = 2, "Rec" = 3, "BLQ" = 4, "ALQ" = 5, "GLQ" = 6)
all$BsmtFinType1 <- as.integer(revalue(all$BsmtFinType1, FinType))
table(all$BsmtFinType1)
#display remaining NAs. Using BsmtQual as a reference for the 79 houses without basement agreed upon earlier
all[(is.na(all$BsmtFullBath)|is.na(all$BsmtHalfBath)|is.na(all$BsmtFinSF1)|is.na(all$BsmtFinSF2)|is.na(all$BsmtUnfSF)|is.na(all$TotalBsmtSF)), c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]
# BmstFullBatch: An integer variable
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
table(all$BsmtFullBath)
# BsmtHalfBath: Integer variable
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0
table(all$BsmtHalfBath)
# BsmtFinSF2: also integer variable
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <- 0
#Masonry veneer type, and masonry veneer area
#check if the 23 houses with veneer area NA are also NA in the veneer type
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))
#find the one that should have a MasVnrType
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c("MasVnrType", "MasVnrArea")]
#fix this veneer type by imputing the mode
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2] # taking the 2nd value because the 1st is "none"
all[2611, c("MasVnrType", "MasVnrArea")]
#Masonry veneer type
all$MasVnrType[is.na(all$MasVnrType)] <- "None"
all[!is.na(all$SalePrice), ] %>% group_by(MasVnrType) %>% dplyr::summarise(median = median(SalePrice), counts = n()) %>% arrange(median)
Masonry <- c("None" = 0, "BrkCmn" = 0, "BrkFace" = 1, "Stone" = 2)
table(all$MasVnrType)
all$MasVnrType <- as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)
#MasVnrArea: Masonry veneer area in square feet
all$MasVnrArea[is.na(all$MasVnrArea)] <- 0
#imputing the mode
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.factor(all$MSZoning)
table(all$MSZoning)
sum(table(all$MSZoning))
all$KitchenQual[is.na(all$KitchenQual)] <- 'TA' #replace with most common value
all$KitchenQual<-as.integer(revalue(all$KitchenQual, Qualities))
table(all$KitchenQual)
sum(table(all$KitchenQual))
table(all$KitchenAbvGr)
kable(all[is.na(all$Utilities) | all$Utilities=='NoSeWa', 1:9])
all$Utilities <- NULL
#impute mode for the 1 NA
all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]
all$Functional <- as.integer(revalue(all$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(all$Functional)
sum(table(all$Functional))
#imputing mode
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]
all$Exterior1st <- as.factor(all$Exterior1st)
table(all$Exterior1st)
sum(table(all$Exterior1st))
#imputing mode
all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]
all$Exterior2nd <- as.factor(all$Exterior2nd)
table(all$Exterior2nd)
sum(table(all$Exterior2nd))
all$ExterQual<-as.integer(revalue(all$ExterQual, Qualities))
table(all$ExterQual)
sum(table(all$ExterQual))
all$ExterCond<-as.integer(revalue(all$ExterCond, Qualities))
table(all$ExterCond)
sum(table(all$ExterCond))
#imputing mode
all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]
all$Electrical <- as.factor(all$Electrical)
table(all$Electrical)
sum(table(all$Electrical))
#imputing mode
all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]
all$SaleType <- as.factor(all$SaleType)
table(all$SaleType)
sum(table(all$SaleType))
all$SaleCondition <- as.factor(all$SaleCondition)
table(all$SaleCondition)
sum(table(all$SaleCondition))
####Label encoding/factorizing the remaining character variables####
Charcol <- names(all[, sapply(all, is.character)])
cat("There are", length(Charcol), "remaining columns with character values.")
#Foundation: Type of foundation
#No ordinality, so converting into factors
all$Foundation <- as.factor(all$Foundation)
table(all$Foundation)
sum(table(all$Foundation))
#Heating: Type of heating
#There are 2 heating variables, and one that indicates Airco Yes/No.
#No ordinality, so converting into factors
table(all$Heating)
sum(table(all$Heating))
#making the variable ordinal using the Qualities vector
all$HeatingQC <- as.integer(revalue(all$HeatingQC, Qualities))
table(all$HeatingQC)
sum(table(all$HeatingQC))
#CentralAir: Central air conditioning
all$CentralAir <- as.integer(revalue(all$CentralAir, c("N" = 0, "Y" = 1)))
table(all$CentralAir)
sum(table(all$CentralAir))
#RoofStyle: Type of roof
#There are 2 variables that deal with the roof of houses.
#No ordinality, so converting into factors
all$RoofStyle <- as.factor(all$RoofStyle)
table(all$RoofStyle)
sum(table(all$RoofStyle))
#RoofMatl: Roof material
##No ordinality, so converting into factors
all$RoofMatl <- as.factor(all$RoofMatl)
table(all$RoofMatl)
sum(table(all$RoofMatl))
##2 variables that specify the flatness and slope of the propoerty.
#LandContour: Flatness of the property
##No ordinality, so converting into factors
all$LandContour <- as.factor(all$LandContour)
table(all$LandContour)
sum(table(all$LandContour))
#LandSlope: Slope of property
##Ordinal, so label encoding
all$LandSlope <- as.integer(revalue(all$LandSlope, c("Sev" = 0, "Mod" = 1, "Gtl" = 2)))
table(all$LandSlope)
sum(table(all$LandSlope))
#2 variables that specify the type and style of dwelling.
#BldgType: Type of dwelling
# Seems ordinal. Check for ordinality with a visualization
ggplot(all[!is.na(all$SalePrice), ], aes(x = as.factor(BldgType), y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "median", fill ="blue") +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
#However, the visualization does not show ordinality.
#converting to factors
all$BldgType <- as.factor(all$BldgType)
table(all$BldgType)
sum(table(all$BldgType))
#HouseStyle: Style of dwelling
#No ordinality, so converting into factors
all$HouseStyle <- as.factor(all$HouseStyle)
table(all$HouseStyle)
sum(table(all$HouseStyle))
#No ordinality, so converting into factors
all$Neighborhood <- as.factor(all$Neighborhood)
table(all$Neighborhood)
sum(table(all$Neighborhood))
all$Condition1 <- as.factor(all$Condition1)
table(all$Condition1)
sum(table(all$Condition1))
all$Condition2 <- as.factor(all$Condition2)
table(all$Condition2)
sum(table(all$Condition2))
all$Street<-as.integer(revalue(all$Street, c('Grvl'=0, 'Pave'=1)))
table(all$Street)
#sum(table(all$Street))
all$PavedDrive<-as.integer(revalue(all$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(all$PavedDrive)
sum(table(all$PavedDrive))
str(all$YrSold)
str(all$MoSold)
all$MoSold <- as.factor(all$MoSold)
ys <- ggplot(all[!is.na(all$SalePrice), ], aes(x = as.factor(YrSold), y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue") +
  scale_y_continuous(breaks = seq(0, 800000, by = 25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept = 163000, linetype = "dashed", color = "red") #dashed line is median Sale Price
ms <- ggplot(all[!is.na(all$SalePrice), ], aes(x = MoSold, y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue") +
  scale_y_continuous(breaks = seq(0, 800000, by = 25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept = 163000, linetype = "dashed", color = "red") #dahsed line is median sale price
grid.arrange(ys, ms, widths = c(1, 2))
str(all$MSSubClass)
all$MSSubClass <- as.factor(all$MSSubClass)
all$MSSubClass <- revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))
str(all$MSSubClass)
numericVars <- which(sapply(all, is.numeric))  #index vector numeric variables
factorVars <- which(sapply(all, is.factor)) #index vector factor variables
cat("There are", length(numericVars), "numeric variables, and", length(factorVars), "categoric variables.")
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs") #correlations of all numeric variables
cor_sorted <- as.matrix(sort(cor_numVar[, "SalePrice"], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col = "black", tl.pos = "lt", tl.cex = 0.7, cl.cex = .7, number.cex = .7)

##Finding variable importance with a quick Random Forest
set.seed(2018)
quick_RF <- randomForest(x = all[1:1460, -79], y = all$SalePrice[1:1460], ntree = 100, importance = TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE), ]
ggplot(imp_DF[1:20, ], aes(x = reorder(Variables, MSE), y = MSE, fill = MSE)) +
  geom_bar(stat = "identity") + 
  labs(x = "Variables", y = "% increase MSE if variable is randomly permuted") +
  coord_flip() +
  theme(legend.position = "none")
s1 <- ggplot(data= all, aes(x=GrLivArea)) +
  geom_density() + labs(x='Square feet living area')
s2 <- ggplot(data=all, aes(x=as.factor(TotRmsAbvGrd))) +
  geom_histogram(stat='count') + labs(x='Rooms above Ground')
s3 <- ggplot(data= all, aes(x=X1stFlrSF)) +
  geom_density() + labs(x='Square feet first floor')
s4 <- ggplot(data= all, aes(x=X2ndFlrSF)) +
  geom_density() + labs(x='Square feet second floor')
s5 <- ggplot(data= all, aes(x=TotalBsmtSF)) +
  geom_density() + labs(x='Square feet basement')
s6 <- ggplot(data= all[all$LotArea<100000,], aes(x=LotArea)) +
  geom_density() + labs(x='Square feet lot')
s7 <- ggplot(data= all, aes(x=LotFrontage)) +
  geom_density() + labs(x='Linear feet lot frontage')
layout <- matrix(c(1,2,5,3,4,8,6,7),4,2,byrow=TRUE)
multiplot(s1, s2, s3, s4, s5, s6, s7,layout=layout)
n1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=Neighborhood, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
n2 <- ggplot(data=all, aes(x=Neighborhood)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(n1, n2)
q1 <- ggplot(data=all, aes(x=as.factor(OverallQual))) +
  geom_histogram(stat='count')
q2 <- ggplot(data=all, aes(x=as.factor(ExterQual))) +
  geom_histogram(stat='count')
q3 <- ggplot(data=all, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')
q4 <- ggplot(data=all, aes(x=as.factor(KitchenQual))) +
  geom_histogram(stat='count')
q5 <- ggplot(data=all, aes(x=as.factor(GarageQual))) +
  geom_histogram(stat='count')
q6 <- ggplot(data=all, aes(x=as.factor(FireplaceQu))) +
  geom_histogram(stat='count')
q7 <- ggplot(data=all, aes(x=as.factor(PoolQC))) +
  geom_histogram(stat='count')
layout <- matrix(c(1,2,8,3,4,8,5,6,7),3,3,byrow=TRUE)
multiplot(q1, q2, q3, q4, q5, q6, q7, layout=layout)
ms1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=MSSubClass, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
ms2 <- ggplot(data=all, aes(x=MSSubClass)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(ms1, ms2)
all$MSSubClass[all$MSSubClass=='1,5 story PUD all'] <- '2 story PUD 1946+'
#correct error
all$GarageYrBlt[2593] <- 2007 #this must have been a typo. GarageYrBlt=2207, YearBuilt=2006, YearRemodAdd=2007.
g1 <- ggplot(data=all[all$GarageCars !=0,], aes(x=GarageYrBlt)) +
  geom_histogram()
g2 <- ggplot(data=all, aes(x=as.factor(GarageCars))) +
  geom_histogram(stat='count')
g3 <- ggplot(data= all, aes(x=GarageArea)) +
  geom_density()
g4 <- ggplot(data=all, aes(x=as.factor(GarageCond))) +
  geom_histogram(stat='count')
g5 <- ggplot(data=all, aes(x=GarageType)) +
  geom_histogram(stat='count')
g6 <- ggplot(data=all, aes(x=as.factor(GarageQual))) +
  geom_histogram(stat='count')
g7 <- ggplot(data=all, aes(x=as.factor(GarageFinish))) +
  geom_histogram(stat='count')
layout <- matrix(c(1,5,5,2,3,8,6,4,7),3,3,byrow=TRUE)
multiplot(g1, g2, g3, g4, g5, g6, g7, layout=layout)
b1 <- ggplot(data=all, aes(x=BsmtFinSF1)) +
  geom_histogram() + labs(x='Type 1 finished square feet')
b2 <- ggplot(data=all, aes(x=BsmtFinSF2)) +
  geom_histogram()+ labs(x='Type 2 finished square feet')
b3 <- ggplot(data=all, aes(x=BsmtUnfSF)) +
  geom_histogram()+ labs(x='Unfinished square feet')
b4 <- ggplot(data=all, aes(x=as.factor(BsmtFinType1))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 1 finished area')
b5 <- ggplot(data=all, aes(x=as.factor(BsmtFinType2))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 2 finished area')
b6 <- ggplot(data=all, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')+ labs(x='Quality Rating')
b7 <- ggplot(data=all, aes(x=as.factor(BsmtCond))) +
  geom_histogram(stat='count')+ labs(x='Rating of general condition')
b8 <- ggplot(data=all, aes(x=as.factor(BsmtExposure))) +
  geom_histogram(stat='count')+ labs(x='Walkout or garden level walls')
layout <- matrix(c(1,2,3,4,5,9,6,7,8),3,3,byrow=TRUE)
multiplot(b1, b2, b3, b4, b5, b6, b7, b8, layout=layout)
###7 Feature engineering
##7.1 Total number of Bathrooms
all$TotBathrooms <- all$FullBath + (all$HalfBath * 0.5) + all$BsmtFullBath + (all$BsmtHalfBath * 0.5)
tb1 <- ggplot(data = all[!is.na(all$SalePrice), ], aes(x = as.factor(TotBathrooms), y = SalePrice)) +
  geom_point(col = "blue") + geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)
tb2 <- ggplot(data = all, aes(x = as.factor(TotBathrooms))) + geom_histogram(stat = "count")
grid.arrange(tb1, tb2)
##7.2 Consolidating Porch variables
all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch
por1 <- ggplot(data = all, aes(x = TotalPorchSF)) + geom_histogram() + labs(x = "Total Porch square feet")
por2 <- ggplot(data = all, aes(x = WoodDeckSF)) + geom_histogram() + labs(x = "Wood Deck square feet")
grid.arrange(por1, por2, nrow = 1)
##7.3 Consolidating Quality and Condition of External Materials
all$ExterMat <- all$ExterCond + all$ExterQual
ggplot(data = all[!is.na(all$SalePrice), ], aes(x = ExterMat, y = SalePrice)) +
  geom_point(col = "blue") + geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)
cor(all$SalePrice[!is.na(all$SalePrice)], all$ExterMat[!is.na(all$SalePrice)])
##7.4 Adding ‘House Age’ and ‘Remodeled (Yes/No)’ variables
all$Remod <- ifelse(all$YearBuilt == all$YearRemodAdd, 0, 1) #0 = no remodelling, 1 = remodelling
all$Age <- as.numeric(all$YrSold) - all$YearRemodAdd
ggplot(data = all[!is.na(all$SalePrice), ], aes(x = Age, y = SalePrice)) +
  geom_point(col = "blue") + geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)
cor(all$SalePrice[!is.na(all$SalePrice)], all$Age[!is.na(all$SalePrice)])
ggplot(all[!is.na(all$SalePrice), ], aes(x = as.factor(Remod), y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue") +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 6) +
  scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept = 163000, linetype = "dashed") #dashed line is a median sale price
all$YrSold <- as.factor(all$YrSold) #the numeric version is now not needed anymore

###7.4 Binning Neighborhood
nb1 <- ggplot(all[!is.na(all$SalePrice), ], aes(x = reorder(Neighborhood, SalePrice, FUN = median), y = SalePrice)) +
  + geom_bar(stat = "summary", fun.y = "median", fill = "blue") + labs(x = "Neighborhood", y = "Median SalePrice") +
  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = comma) +
  + geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3) +
  + geom_hline(yintercept = 163000, linetype = "dashed", color = "red") #dashed line is the median SalePrice
> nb2 <- ggplot(all[!is.na(all$SalePrice), ], aes(x = reorder(Neighborhood, SalePrice, FUN = mean), y = SalePrice)) +
  + geom_bar(stat = "summary", fun.y = "mean", fill = "blue") + labs(x = "Neighborhood", y = "Mean SalePrice") +
  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  + scale_y_continuous(breaks = seq(0, 800000, 50000), labels = comma) +
  + geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3) +
  + geom_hline(yintercept = 163000, linetype = "dashed", color = "red") 
> grid.arrange(nb1, nb2)