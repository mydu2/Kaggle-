#load relevant packages from user library
library(dplyr) 
library(tidyr)
library(ggplot2) 
library(onehot)  
library(MASS)
library(e1071) 
library(tidyverse)
library(car)
library(fastDummies)
library(reshape2)
library(tibble)
library(MASS)
library(glmnet)
library(leaps)


# Configure select() so that it works with MASS package loaded
select <- dplyr::select

#Read the training set into R
raw_train_data <-read.csv("train.csv")
raw_test_data <- read.csv("test.csv")

raw_train_data <- raw_train_data[,-1]
raw_test_data <- raw_test_data[,-1]

raw_test_data <- raw_test_data %>% cbind(SalePrice = NA)

raw_test_data <- raw_test_data %>% relocate(SalePrice, .before= MSSubClass)
raw_train_data <- raw_train_data %>% relocate(SalePrice, .before = MSSubClass)

#combine test and train sets
set.seed(123)
raw_data <- rbind(raw_train_data,raw_test_data)

#check for duplicates, there are 0
count(raw_data[duplicated(raw_data) == TRUE, ])

#check columns for large amts of NAs (besides SalePrice)
Structure_raw <- data.frame(Names = names(raw_data))
Structure_raw$Type <- sapply(raw_data,class)
Structure_raw$Nulls <- sapply(raw_data, function(x) length(which(is.na(x))))
nrow(Structure_raw[Structure_raw$Nulls >= 1,])
Structure_raw[Structure_raw$Nulls > 1,]

#make functions
SPcor <- function(column) {cor(column,raw_data$SalePrice, use= "complete.obs")}
bar_plot <- function(raw_data, column) {ggplot(raw_data, aes(column))  +  geom_bar() }
#continuous 1 var
density_plot <- function(raw_data, column) {ggplot(raw_data, aes(column))  +  geom_density() }
histo_plot <- function(raw_data, column) { ggplot(raw_data, aes(column))  +  geom_histogram()}
dot_plot <- function(raw_data, column) { ggplot(raw_data, aes(column)) +  geom_dotplot()}
#2continuous
point_plot <- function(raw_data, xcolumn, ycolumn) { ggplot(raw_data, aes(xcolumn, ycolumn)) +  geom_point()}
#1discrete/1continuous
box_plot <- function(raw_data, xcolumn, ycolumn) { ggplot(raw_data, aes(xcolumn, ycolumn)) +  geom_boxplot() }
col_plot <- function(raw_data, xcolumn, ycolumn) { ggplot(raw_data, aes(xcolumn, ycolumn)) +  geom_col()}

#clean numerics: 
raw_data <- raw_data %>% mutate_if(is.integer,as.numeric)

#IsNew (New column)
raw_data$IsNew <- ifelse(raw_data$YearBuilt == raw_data$YrSold, 1,0)

#IsRemod (New column)
raw_data$IsRemod <- ifelse(raw_data$YearBuilt == raw_data$YearRemodAdd, 0,1)

##COLUMN 1: SalePrice
sum(is.na(raw_data$SalePrice))
raw_data$SalePrice <- log10(raw_data$SalePrice)

## MSSubClass
sum(is.na(raw_data$MSSubClass))
summary(raw_data$MSSubClass)
class(raw_data$MSSubClass)
SPcor(raw_data$MSSubClass)
#OneHot Encode
OneHotMSSubClass <- dummy_cols(raw_data[,2],remove_selected_columns = T)
names(OneHotMSSubClass) <- gsub("data", "MSSubClass", names(OneHotMSSubClass))
OneHotMSSubClass <- OneHotMSSubClass[,-c(4,11)]
raw_data <- cbind(raw_data[,-2],OneHotMSSubClass)

## MSZoning
sum(is.na(raw_data$MSZoning))
summary(factor(raw_data$MSZoning))
raw_data$MSZoning[is.na(raw_data$MSZoning)] <- "RL"
#Change the MSZoning "C (all)" level to "C" to match the data description.
raw_data$MSZoning[raw_data$MSZoning == "C (all)"] <- "C"

#LotFrontage
summary(raw_data$LotFrontage)
raw_data %>%
  group_by(MSZoning) %>%
  select(MSZoning, LotFrontage) %>%
  summarize(mean = mean(LotFrontage, na.rm = TRUE))

raw_data$LotFrontage[which((raw_data$MSZoning == "C" & is.na(raw_data$LotFrontage)))] <- 65.6
raw_data$LotFrontage[which((raw_data$MSZoning == "FV" & is.na(raw_data$LotFrontage)))] <- 59.5
raw_data$LotFrontage[which((raw_data$MSZoning == "RH" & is.na(raw_data$LotFrontage)))] <- 55.4
raw_data$LotFrontage[which((raw_data$MSZoning == "RL" & is.na(raw_data$LotFrontage)))] <- 74.1
raw_data$LotFrontage[which((raw_data$MSZoning == "RM" & is.na(raw_data$LotFrontage)))] <- 52.2
raw_data[is.na(raw_data$LotFrontage),"LotFrontage"]<- mean(raw_data$LotFrontage,na.rm=T)
#impute points above 300
raw_data$LotFrontage <- ifelse(raw_data$LotFrontage > 300,mean(raw_data$LotFrontage), raw_data$LotFrontage)
skewness(raw_data$LotFrontage)
skewness(raw_data$LotFrontage^(1/3))
#transform
raw_data$LotFrontage <- raw_data$LotFrontage^(1/3)

#go back to Zoning
#OneHot, keeping FV, RH, and RM
OneHotZoning <- dummy_cols(raw_data[,2],remove_selected_columns = T)
names(OneHotZoning) <- gsub("data", "MSZoning", names(OneHotZoning))
OneHotZoning <- OneHotZoning[,-c(1,4)]
raw_data <- cbind(raw_data[,-2],OneHotZoning)

#LotArea
sum(is.na(raw_data$LotArea))
summary(raw_data$LotArea)
point_plot(raw_data, raw_data$LotArea, raw_data$SalePrice)
raw_data$LotArea <- ifelse(raw_data$LotArea > 100000,mean(raw_data$LotArea), raw_data$LotArea)
density_plot(raw_data, log10(raw_data$LotArea))
raw_data$LotArea <-log10(raw_data$LotArea)
SPcor(raw_data$LotArea)

#Street
summary(raw_data$Street)
sum(is.na(raw_data$Street))
raw_data$Street <- as.factor(raw_data$Street)


#Alley
sum(is.na(raw_data$Alley))
#1369 NAs
#According the description about Alley, NA in fact represents No alley access.
#I replace NA with "None" to avoid confusion.
raw_data$Alley[is.na(raw_data$Alley)] <- "None"
#OneHot Pave and None
OneHotAlley <- dummy_cols(raw_data[,5],remove_selected_columns = T)
names(OneHotAlley) <- gsub("data", "Alley", names(OneHotAlley))
OneHotAlley <- OneHotAlley[,-1]
raw_data <- cbind(raw_data[,-5],OneHotAlley)

#LotShape
sum(is.na(raw_data$LotShape))
summary(factor(raw_data$LotShape))
#0 mising value or typo
raw_data$LotShape<- factor(raw_data$LotShape, order = TRUE, 
                           levels = c("IR3", "IR2", "IR1", "Reg"))

#LandContour
sum(is.na(raw_data$LandContour))
summary(factor(raw_data$LandContour))
#no missing values or typos
#OneHot, keeping HLS, Low, Lvl
OneHotContour <- dummy_cols(raw_data[,6],remove_selected_columns = T)
names(OneHotContour) <- gsub("data", "Contour", names(OneHotContour))
OneHotContour <- OneHotContour[,-1]
raw_data <- cbind(raw_data[,-6],OneHotContour)

#Utilities
#remove utilities:
raw_data <- raw_data[,-6]

#LotConfig
sum(is.na(raw_data$LotConfig))
summary(factor(raw_data$LotConfig))
#OneHot, keeping CulDSac, FR2, FR3, inside 
OneHotConfig <- dummy_cols(raw_data[,6],remove_selected_columns = T)
names(OneHotConfig) <- gsub("data", "Config", names(OneHotConfig))
OneHotConfig <- OneHotConfig[,-1]
raw_data <- cbind(raw_data[,-6],OneHotConfig)

#Landslope
sum(is.na(raw_data$LandSlope))
summary(factor(raw_data$LandSlope))
raw_data$LandSlope <- as.factor(raw_data$LandSlope)
#save to combine with LotArea

#Neighborhood
sum(is.na(raw_data$Neighborhood))
summary(factor(raw_data$Neighborhood))
OneHotNeighborhood <- dummy_cols(raw_data[,7],remove_selected_columns = T)
names(OneHotNeighborhood) <- gsub("data", "Neighbor", names(OneHotNeighborhood))
OneHotNeighborhood <- OneHotNeighborhood[,-17]
raw_data <- cbind(raw_data[,-7],OneHotNeighborhood)

#Condition1
sum(is.na(raw_data$Condition1))
summary(factor(raw_data$Condition1))
OneHotCondition1 <- dummy_cols(raw_data[,7],remove_selected_columns = T)
names(OneHotCondition1) <- gsub("data", "Cond1", names(OneHotCondition1))
OneHotCondition1 <- OneHotCondition1[,-5]
raw_data <- cbind(raw_data[,-7],OneHotCondition1)

#Condition2
sum(is.na(raw_data$Condition2))
summary(factor(raw_data$Condition2))
OneHotCondition2 <- dummy_cols(raw_data[,7],remove_selected_columns = T)
names(OneHotCondition2) <- gsub("data", "Cond2", names(OneHotCondition2))
OneHotCondition2 <- OneHotCondition2[,-c(6:8)]
raw_data <- cbind(raw_data[,-7],OneHotCondition2)

#BldgType
sum(is.na(raw_data$BldgType))
summary(factor(raw_data$BldgType))
OneHotBldgType <- dummy_cols(raw_data[,7],remove_selected_columns = T)
names(OneHotBldgType) <- gsub("data", "BldgType", names(OneHotBldgType))
OneHotBldgType <- OneHotBldgType[,-c(1,3,5)]
raw_data <- cbind(raw_data[,-7],OneHotBldgType)

#HouseStyle
sum(is.na(raw_data$HouseStyle))
summary(factor(raw_data$HouseStyle))
OneHotHouseStyle <- dummy_cols(raw_data[,7],remove_selected_columns = T)
names(OneHotHouseStyle) <- gsub("data", "HouseStyle", names(OneHotHouseStyle))
OneHotHouseStyle <- OneHotHouseStyle[,-c(3,4,6)]
raw_data <- cbind(raw_data[,-7],OneHotHouseStyle)

#OverallQual
summary(raw_data$OverallQual)
raw_data$OverallQual<- as.factor(raw_data$OverallQual)
#remove due to multicollinearity in original submission
raw_data <- raw_data[,-7]

#OverallCond
summary(raw_data$OverallCond)
bar_plot(raw_data, raw_data$OverallCond)
point_plot(raw_data, raw_data$OverallCond,raw_data$SalePrice)
SPcor(raw_data$OverallCond)

#YearBuilt -> HouseAge
SPcor(raw_data$YearBuilt)
bar_plot(raw_data, raw_data$YearBuilt)
raw_data$YearBuilt <- 2011-raw_data$YearBuilt
raw_data <- rename(raw_data, HouseAge=YearBuilt)
SPcor(raw_data$HouseAge)

#YearRemodAdd _ RemodAge
summary(raw_data$YearRemodAdd)
point_plot(raw_data, raw_data$YearRemodAdd, raw_data$SalePrice)
SPcor(raw_data$YearRemodAdd)
raw_data$YearRemodAdd <- 2011-raw_data$YearRemodAdd
raw_data <- rename(raw_data, RemodAge=YearRemodAdd)
SPcor(raw_data$RemodAge)

#RoofStyle
sum(is.na(raw_data$RoofStyle))
summary(factor(raw_data$RoofStyle))
OneHotRoofStyle <- dummy_cols(raw_data[,10],remove_selected_columns = T)
names(OneHotRoofStyle) <- gsub("data", "RoofStyle", names(OneHotRoofStyle))
OneHotRoofStyle <- OneHotRoofStyle[,-c(1,2,4)]
raw_data <- cbind(raw_data[,-10],OneHotRoofStyle)

#RoofMatl
sum(is.na(raw_data$RoofMatl))
summary(factor(raw_data$RoofMatl))
OneHotRoofMatl <- dummy_cols(raw_data[,10],remove_selected_columns = T)
names(OneHotRoofMatl) <- gsub("data", "RoofMatl", names(OneHotRoofMatl))
OneHotRoofMatl <- OneHotRoofMatl[,-c(1,3,4,5)]
raw_data <- cbind(raw_data[,-10],OneHotRoofMatl)

#Exterior1st
sum(is.na(raw_data$Exterior1st))
summary(factor(raw_data$Exterior1st))
#Impute NA for VinylSd (most common)
raw_data$Exterior1st[is.na(raw_data$Exterior1st)]<- "VinylSd"
OneHotExter1st <- dummy_cols(raw_data[,10],remove_selected_columns = T)
names(OneHotExter1st) <- gsub("data", "Exter1st", names(OneHotExter1st))
OneHotExter1st <- OneHotExter1st[,-c(5,8,9,11,13)]
raw_data <- cbind(raw_data[,-10],OneHotExter1st)

#Exterior2nd
sum(is.na(raw_data$Exterior2nd))
summary(factor(raw_data$Exterior2nd))
#Impute NA for VinylSd (most common)
raw_data$Exterior2nd[is.na(raw_data$Exterior2nd)]<- "VinylSd"
OneHotExter2nd <- dummy_cols(raw_data[,10],remove_selected_columns = T)
names(OneHotExter2nd) <- gsub("data", "Exter2nd", names(OneHotExter2nd))
OneHotExter2nd <- OneHotExter2nd[,-c(5,10,14)]
raw_data <- cbind(raw_data[,-10],OneHotExter2nd)

#MasVnrType
sum(is.na(raw_data$MasVnrType))
summary(factor(raw_data$MasVnrType))
#Impute NA for VinylSd (most common)
raw_data$MasVnrType[is.na(raw_data$MasVnrType)]<- "None"
OneHotMasVnrType <- dummy_cols(raw_data[,10],remove_selected_columns = T)
names(OneHotMasVnrType) <- gsub("data", "MasVnrType", names(OneHotMasVnrType))
OneHotMasVnrType <- OneHotMasVnrType[,-c(1,3)]
raw_data <- cbind(raw_data[,-10],OneHotMasVnrType)

#MasVnrArea
summary(raw_data$MasVnrArea)
raw_data$MasVnrArea[is.na(raw_data$MasVnrArea)] <- 0
point_plot(raw_data, raw_data$MasVnrArea, raw_data$SalePrice)
#Outliers seem to occur around 1250.
#Replace outliers with mean.
raw_data$MasVnrArea <- ifelse(raw_data$MasVnrArea > 1250, 
                              mean(raw_data$MasVnrArea),raw_data$MasVnrArea)
skewness(raw_data$MasVnrArea)
skewness(log(raw_data$MasVnrArea + 1, 10))
#skewness has been greatly improved.
raw_data$MasVnrArea <- log(raw_data$MasVnrArea + 1, 10)
SPcor(raw_data$MasVnrArea)


#ExterQual
sum(is.na(raw_data$ExterQual))
summary(factor(raw_data$ExterQual))
#0 missing value or typo
raw_data$ExterQual <- factor(raw_data$ExterQual, order = TRUE, 
                             levels = c("Po", "Fa", "TA", "Gd", "Ex"))

#ExterCond
sum(is.na(raw_data$ExterCond))
summary(factor(raw_data$ExterCond))
raw_data$ExterCond <- factor(raw_data$ExterCond, order = TRUE, 
                             levels = c("Po", "Fa", "TA", "Gd", "Ex"))


#Foundation
sum(is.na(raw_data$Foundation))
summary(factor(raw_data$Foundation))
OneHotFoundation <- dummy_cols(raw_data[,13],remove_selected_columns = T)
names(OneHotFoundation) <- gsub("data", "Foundation", names(OneHotFoundation))
OneHotFoundation <- OneHotFoundation[,-2]
raw_data <- cbind(raw_data[,-13],OneHotFoundation)

#BsmtQual
sum(is.na(raw_data$BsmtQual))
summary(factor(raw_data$BsmtQual))
#Replace NA with "None" to avoid confusion.
raw_data$BsmtQual[is.na(raw_data$BsmtQual)] <- "None"
raw_data$BsmtQual <- factor(raw_data$BsmtQual, order = TRUE, 
                            levels = c("None","Po", "Fa", "TA", "Gd", "Ex"))

#BsmtCond
summary(raw_data$BsmtCond)
sum(is.na(raw_data$BsmtCond))
summary(factor(raw_data$BsmtCond))
#Replace NA with "No Basement" to avoid confusion.
raw_data$BsmtCond[is.na(raw_data$BsmtCond)] <- "None"
raw_data$BsmtCond <- factor(raw_data$BsmtCond, order = TRUE, 
                            levels = c("None","Po", "Fa", "TA", "Gd", "Ex"))

#BsmtExposure
sum(is.na(raw_data$BsmtExposure))
summary(factor(raw_data$BsmtExposure))
#replace NA with "No Basement" to avoid confusion.
raw_data$BsmtExposure[is.na(raw_data$BsmtExposure)] <- "None"
raw_data$BsmtExposure <- factor(raw_data$BsmtExposure, order = TRUE, 
                                levels = c("None","No", "Mn", "Av", "Gd"))


#BsmtFinType1
sum(is.na(raw_data$BsmtFinType1))
summary(factor(raw_data$BsmtFinType1))
#To avoid confusion, replace NA with "None".
raw_data$BsmtFinType1[is.na(raw_data$BsmtFinType1)] <- "None"
raw_data$BsmtFinType1 <- factor(raw_data$BsmtFinType1, order = TRUE, 
                                levels = c("None","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))

#BsmtFinSF1
#drop due to multicollinearity issues in first draft
raw_data <- raw_data[,-17]

#BsmtFinType2
sum(is.na(raw_data$BsmtFinType2))
summary(factor(raw_data$BsmtFinType2))
#Replace NAs with "None" to avoid confusion.
raw_data$BsmtFinType2[is.na(raw_data$BsmtFinType2)] <- "None"
raw_data$BsmtFinType2 <- factor(raw_data$BsmtFinType2, order = TRUE, 
                                levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))

#BsmtFinSF2
#Drop due to multicollinearity in earlier draft
raw_data <- raw_data[,-18]

#BsmtUnfSF
#Drop due to multicollinearity in earlier draft
raw_data <- raw_data[,-18]

#TotalBsmtSF
#Do not drop yet, use in Construct Area, but Drop for multicollinearity later
summary(raw_data$TotalBsmtSF)
raw_data$TotalBsmtSF[raw_data$TotalBsmtSF>4000] <- mean(raw_data$TotalBsmtSF<4000,na.rm = T)
raw_data$TotalBsmtSF[is.na(raw_data$TotalBsmtSF)] <- mean(raw_data$TotalBsmtSF,na.rm = T)
point_plot(raw_data, raw_data$TotalBsmtSF,raw_data$SalePrice)
skewness(raw_data$TotalBsmtSF)

#Heating
sum(is.na(raw_data$Heating))
summary(factor(raw_data$Heating))
OneHotHeating <- dummy_cols(raw_data$Heating, remove_selected_columns = T)
OneHotHeating <- OneHotHeating[,-c(1,5)]
raw_data <- cbind(raw_data[,-19], OneHotHeating)

#HeatingQC
sum(is.na(raw_data$HeatingQC))
summary(factor(raw_data$HeatingQC))
raw_data$HeatingQC <- factor(raw_data$HeatingQC, order = TRUE, 
                             levels = c("Po", "Fa", "TA", "Gd", "Ex"))

#CentralAir
sum(is.na(raw_data$CentralAir))
raw_data$CentralAir <- as.factor(raw_data$CentralAir)
summary(raw_data$CentralAir)

#Electrical
sum(is.na(raw_data$Electrical))
summary(factor(raw_data$Electrical))
#Replace the NA with SBrkr which appears the most.
raw_data$Electrical[is.na(raw_data$Electrical)] <- "SBrkr"
OneHotElectrical <- dummy_cols(raw_data[,21],remove_selected_columns = T)
names(OneHotElectrical) <- gsub("data", "Electrical", names(OneHotElectrical))
OneHotElectrical <- OneHotElectrical[,-c(4,5)]
raw_data <- cbind(raw_data[,-21],OneHotElectrical)

#X1stFlrSF
#drop due to multicollinearity in other draft
raw_data <- raw_data[,-21]

#X2ndFlrSF
#drop due to multicollinearity in other draft
raw_data <- raw_data[,-21]

#LowQualFinSF
#drop due to multicollinearity in other draft
raw_data <- raw_data[,-21]

#GrLivArea
# drop later 
summary(raw_data$GrLivArea)
point_plot(raw_data, raw_data$GrLivArea, raw_data$SalePrice)
raw_data$GrLivArea[raw_data$GrLivArea>4000] <- mean(raw_data$GrLivArea<4000,na.rm=T)
skewness(raw_data$GrLivArea^.5)
density_plot(raw_data, raw_data$GrLivArea^.5)
raw_data$GrLivArea <- raw_data$GrLivArea^.5

#BsmtFullBath
summary(raw_data$BsmtFullBath)
raw_data$BsmtFullBath[is.na(raw_data$BsmtFullBath)] <- mean(raw_data$BsmtFullBath,na.rm=T)
bar_plot(raw_data, raw_data$BsmtFullBath)
point_plot(raw_data, raw_data$BsmtFullBath, raw_data$SalePrice)
SPcor(raw_data$BsmtFullBath)

#BsmtHalfBath
summary(raw_data$BsmtHalfBath)
raw_data$BsmtHalfBath[is.na(raw_data$BsmtHalfBath)] <- mean(raw_data$BsmtHalfBath,na.rm=T)
bar_plot(raw_data, raw_data$BsmtHalfBath)
point_plot(raw_data, raw_data$BsmtHalfBath, raw_data$SalePrice)
SPcor(raw_data$BsmtHalfBath)

#FullBath
summary(raw_data$FullBath)
ggplot(data = raw_data, aes(SalePrice, FullBath)) +  geom_point()
#No obvious outliers.
skewness(raw_data$FullBath)
#The value is 0.036, approximately symmetrical.
SPcor(raw_data$FullBath)

#HalfBath
summary(raw_data$HalfBath)
bar_plot(raw_data, raw_data$HalfBath)
point_plot(raw_data, raw_data$HalfBath, raw_data$SalePrice)
SPcor(raw_data$HalfBath)

#BedroomAbvGr
summary(raw_data$BedroomAbvGr)
density_plot(raw_data, raw_data$BedroomAbvGr)
point_plot(raw_data, raw_data$BedroomAbvGr, raw_data$SalePrice)
SPcor(raw_data$BedroomAbvGr)

#KitchenAbvGr, transform for skewness
summary(raw_data$KitchenAbvGr)
bar_plot(raw_data, raw_data$KitchenAbvGr)
SPcor(raw_data$KitchenAbvGr)
skewness(raw_data$KitchenAbvGr)
density_plot(raw_data, raw_data$KitchenAbvGr^.5)
raw_data$KitchenAbvGr <- raw_data$KitchenAbvGr^.5

#KitchenQual
sum(is.na(raw_data$KitchenQual))
summary(factor(raw_data$KitchenQual))
raw_data$KitchenQual[is.na(raw_data$KitchenQual)] <- "TA"
raw_data$KitchenQual <- factor(raw_data$KitchenQual, order = TRUE, 
                               levels = c("Po", "Fa", "TA", "Gd", "Ex"))

#TotRmsAbvGrd, transform for skewness
summary(raw_data$TotRmsAbvGrd)
point_plot(raw_data, raw_data$TotRmsAbvGrd, raw_data$SalePrice)
good_TotRms <- raw_data[raw_data$TotRmsAbvGrd<12,]
mean(good_TotRms$TotRmsAbvGrd)
raw_data[raw_data$TotRmsAbvGrd>12, "TotRmsAbvGrd"]<- mean(good_TotRms$TotRmsAbvGrd)
SPcor(raw_data$TotRmsAbvGrd^.5)
raw_data$TotRmsAbvGrd <- raw_data$TotRmsAbvGrd^.5
SPcor(raw_data$TotRmsAbvGrd)
skewness(raw_data$TotRmsAbvGrd)

#Functional
sum(is.na(raw_data$Functional))
summary(factor(raw_data$Functional))
raw_data$Functional[is.na(raw_data$Functional)] <- "Typ"
raw_data$Functional <- factor(raw_data$Functional, order = TRUE, 
                              levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod","Min2","Min1","Typ"))

#Fireplaces, transform for skewness
summary(raw_data$Fireplaces)
bar_plot(raw_data, raw_data$Fireplaces^.5)
point_plot(raw_data, raw_data$Fireplaces, raw_data$SalePrice)
SPcor(raw_data$Fireplaces)
skewness(raw_data$Fireplaces^.5)
raw_data$Fireplaces <- raw_data$Fireplaces^.5
skewness(raw_data$Fireplaces)

#FireplaceQu
#Drop Later
summary(factor(raw_data$FireplaceQu))
raw_data$FireplaceQu[is.na(raw_data$FireplaceQu)] <- "None"
raw_data$FireplaceQu <- factor(raw_data$FireplaceQu, order = TRUE, 
                               levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))
summary(raw_data$FireplaceQu)

#GarageType, OneHot encode, keep 5
sum(is.na(raw_data$GarageType))
summary(factor(raw_data$GarageType))
#Replace the NA with None 
raw_data$GarageType[is.na(raw_data$GarageType)] <- "None"
OneHotGarageType <- dummy_cols(raw_data[,33],remove_selected_columns = T)
names(OneHotGarageType) <- gsub("data", "GarageType", names(OneHotGarageType))
OneHotGarageType <- OneHotGarageType[,-c(1,2)]
raw_data <- cbind(raw_data[,-33],OneHotGarageType)

#GarageYrBlt, turn into GarageAge
summary(raw_data$GarageYrBlt)

point_plot(raw_data, raw_data$GarageYrBlt, raw_data$SalePrice)
goodgarageyr <- raw_data[raw_data$GarageYrBlt<2022,]
mean(goodgarageyr$GarageYrBlt,na.rm = TRUE)
raw_data[is.na(raw_data$GarageYrBlt),"GarageYrBlt"]<- mean(goodgarageyr$GarageYrBlt,na.rm = TRUE)
raw_data[raw_data$GarageYrBlt>2022,"GarageYrBlt"]<- mean(goodgarageyr$GarageYrBlt,na.rm = TRUE)
SPcor(raw_data$GarageYrBlt)
#Turn into age
summary(raw_data$GarageYrBlt)
raw_data$GarageYrBlt <- 2011-raw_data$GarageYrBlt
raw_data <- rename(raw_data, GarageAge=GarageYrBlt)
SPcor(raw_data$GarageAge)
skewness(raw_data$GarageAge)
point_plot(raw_data, raw_data$GarageAge, raw_data$SalePrice)

#Make a new variable: HouseAge < GarageAge for Garage_newest
raw_data$Garage_Newest <- raw_data$HouseAge > raw_data$GarageAge

#GarageFinish
sum(is.na(raw_data$GarageFinish))
summary(factor(raw_data$GarageFinish))
#81 NAs but they mean "No Garage" not missing values.
#Replace NA with "No Garage".
raw_data$GarageFinish[is.na(raw_data$GarageFinish)] <- "None"
raw_data$GarageFinish <- factor(raw_data$GarageFinish, order = TRUE, 
                                levels = c("None", "Unf", "RFn", "Fin"))

#GarageCars, Combine with GarageArea
summary(raw_data$GarageCars)
raw_data$GarageCars[is.na(raw_data$GarageCars)] <- mean(raw_data$GarageCars,na.rm=T)
point_plot(raw_data, raw_data$GarageCars, raw_data$SalePrice)
SPcor(raw_data$GarageCars)

#GarageArea, Combine with GarageCars
summary(raw_data$GarageArea)
point_plot(raw_data, raw_data$GarageArea, raw_data$SalePrice)
raw_data$GarageArea <- ifelse(raw_data$GarageArea > 1250,
                              mean(raw_data$GarageArea),
                              raw_data$GarageArea)
raw_data$GarageArea[is.na(raw_data$GarageArea)] <- mean(raw_data$GarageArea,na.rm=T)
SPcor(raw_data$GarageArea)

#Combine GarageCars and GarageArea to make GarageArea_x_Cars
raw_data$GarageArea_x_cars <- raw_data$GarageCars*raw_data$GarageArea
#transform
skewness(raw_data$GarageArea_x_cars)
density_plot(raw_data,raw_data$GarageArea_x_cars)
skewness(raw_data$GarageArea_x_cars^.5)
density_plot(raw_data,raw_data$GarageArea_x_cars^.5)
raw_data$GarageArea_x_cars <- raw_data$GarageArea_x_cars^.5
skewness(raw_data$GarageArea_x_cars)

#remove garagecars 
raw_data <- raw_data[,-35]

#GarageQual, remove later due to multicollinearity in old model
summary(factor(raw_data$GarageQual))
raw_data$GarageQual[is.na(raw_data$GarageQual)] <- "None"
raw_data$GarageQual <- factor(raw_data$GarageQual, order = TRUE, 
                              levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))

#GarageCond, remove later due to multicollinearity in old model 
summary(factor(raw_data$GarageCond))
raw_data$GarageCond[is.na(raw_data$GarageCond)] <- "None"
raw_data$GarageCond <- factor(raw_data$GarageCond, order = TRUE, 
                              levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"))

#PavedDrive
sum(is.na(raw_data$PavedDrive))
summary(factor(raw_data$PavedDrive))
#No missing values or typos.
raw_data$PavedDrive <- as.factor(raw_data$PavedDrive)

#WoodDeckSF, keep for adding to ConstructArea
summary(raw_data$WoodDeckSF)
raw_data$WoodDeckSF <- ifelse(raw_data$WoodDeckSF > 750,
                              mean(raw_data$WoodDeckSF),
                              raw_data$WoodDeckSF)

ggplot(data = raw_data, aes(log10(WoodDeckSF+ 1))) +  geom_histogram(binwidth = .05)
skewness(log10(raw_data$WoodDeckSF + 1))

#Skewness has been greatly improved.
raw_data$WoodDeckSF <- log10(raw_data$WoodDeckSF + 1)
SPcor(log10(raw_data$WoodDeckSF + 1))

#OpenPorchSF, impute 523
summary(raw_data$OpenPorchSF)
SPcor(raw_data$OpenPorchSF)
ggplot(data = raw_data, aes(SalePrice, OpenPorchSF)) +  geom_point()
#Outliers seem to be around 500.
#Replace outliers with mean.
raw_data$OpenPorchSF <- ifelse(raw_data$OpenPorchSF > 500,
                               mean(raw_data$OpenPorchSF),
                               raw_data$OpenPorchSF)
#Skewness
skewness(log10(raw_data$OpenPorchSF+ 1))

#EnclosedPorch
summary(raw_data$EnclosedPorch)
point_plot(raw_data, raw_data$EnclosedPorch, raw_data$SalePrice)
raw_data[raw_data$EnclosedPorch>1000,"EnclosedPorch"] <- mean(raw_data$EnclosedPorch<1000,na.rm=T)
density_plot(raw_data, raw_data$EnclosedPorch)
skewness(log10(raw_data$EnclosedPorch+ 1))
raw_data$EnclosedPorch <- log10(raw_data$EnclosedPorch+ 1)

#X3SsnPorch, remove 
raw_data <- raw_data[,-42]

#ScreenPorch
summary(raw_data$ScreenPorch)
point_plot(raw_data, raw_data$ScreenPorch, raw_data$SalePrice)
density_plot(raw_data, raw_data$EnclosedPorch^.5)
skewness(log10(raw_data$ScreenPorch+ 1))
density_plot(raw_data, log10(raw_data$EnclosedPorch+ 1))
raw_data$ScreenPorch <- log10(raw_data$ScreenPorch+ 1)
#PoolArea, keep for Constructarea, remove afterwards
summary(raw_data$PoolArea)
density_plot(raw_data, raw_data$PoolArea)
point_plot(raw_data, raw_data$PoolArea, raw_data$SalePrice)
summary(raw_data$PoolArea)
raw_data[raw_data$PoolArea != 0, "PoolArea"] <- 1
bar_plot(raw_data, raw_data$PoolArea)
point_plot(raw_data, raw_data$PoolArea, raw_data$SalePrice)
SPcor(raw_data$PoolArea)

#PoolQC
summary(factor(raw_data$PoolQC))
raw_data$PoolQC[is.na(raw_data$PoolQC)] <- "None"
raw_data$PoolQC <- factor(raw_data$PoolQC, order = TRUE, 
                          levels = c("None", "Fa", "TA", "Gd", "Ex"))

#Fence, transform for skewness
summary(factor(raw_data$Fence))
raw_data$Fence[is.na(raw_data$Fence)] <- "None"
raw_data$Fence <- factor(raw_data$Fence, order = TRUE, 
                         levels = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"))


#MiscFeature
sum(is.na(raw_data$MiscFeature))
summary(factor(raw_data$MiscFeature))
#Replace the NA with None 
raw_data$MiscFeature[is.na(raw_data$MiscFeature)] <- "None"
OneHotMiscFeature <- dummy_cols(raw_data[,46],remove_selected_columns = T)
names(OneHotMiscFeature) <- gsub("data", "MiscFeature", names(OneHotMiscFeature))
OneHotMiscFeature <- OneHotMiscFeature[,-c(1,2,5)]
raw_data <- cbind(raw_data[,-46],OneHotMiscFeature)

#MiscVal, transform for skewness
summary(raw_data$MiscVal)
point_plot(raw_data,raw_data$MiscVal, raw_data$SalePrice)
raw_data[raw_data$MiscVal >2500, "MiscVal"] <- mean(raw_data$MiscVal<2500, na.rm = T)
point_plot(raw_data,raw_data$MiscVal, raw_data$SalePrice)
density_plot(raw_data, raw_data$MiscVal^.5)
skewness(log10(raw_data$MiscVal+ 1))
raw_data$MiscVal <- log10(raw_data$MiscVal+ 1)

#MoSold
summary(raw_data$MoSold)


#YrSold -> SoldAge
summary(raw_data$YrSold)
raw_data$YrSold <- 2011-raw_data$YrSold
raw_data <- rename(raw_data, SoldAge=YrSold)
SPcor(raw_data$SoldAge)
skewness(raw_data$SoldAge)

#SaleType
##COLUMN 79: SaleType
sum(is.na(raw_data$SaleType))
summary(factor(raw_data$SaleType))
raw_data$SaleType[is.na(raw_data$SaleType)] <- "WD"
OneHotSaleType <- dummy_cols(raw_data[,49],remove_selected_columns = T)
names(OneHotSaleType) <- gsub("data", "SaleType", names(OneHotSaleType))
OneHotSaleType <- OneHotSaleType[,-c(1,7)]
raw_data <- cbind(raw_data[,-49],OneHotSaleType)

#SaleCondition
sum(is.na(raw_data$SaleCondition))
summary(factor(raw_data$SaleCondition))
OneHotSaleCondition <- dummy_cols(raw_data[,49],remove_selected_columns = T)
names(OneHotSaleCondition) <- gsub("data", "SaleCondition", names(OneHotSaleCondition))
OneHotSaleCondition <- OneHotSaleCondition[,-c(1,5)]
raw_data <- cbind(raw_data[,-49],OneHotSaleCondition)

OneHotMoSold <- dummy_cols(raw_data[,47],remove_selected_columns = T)
names(OneHotMoSold) <- gsub("data", "MoSold", names(OneHotMoSold))
OneHotMoSold <- OneHotMoSold[,-11]
raw_data <- cbind(raw_data[,-47],OneHotSaleCondition)

###########################
#Add good variables into:

attempt1 <- raw_data[,]
attempt1 <- attempt1[,-c(18,21,22,32,36,37,39,42,43,44,183:187)]

model <- lm(SalePrice~., data = attempt1)
summary(model)


#onehot encode garagecond
garagecond <- dummy_cols(raw_data$GarageCond,remove_selected_columns = T)
raw_data <- cbind(raw_data,garagecond[,3])
raw_data <- raw_data[,-41]
model <- lm(SalePrice~., data = raw_data)
summary(model)


#remove column 58 : garagequalTA
raw_data <- raw_data[,-58]
model <- lm(SalePrice~., data = raw_data)
summary(model)

#MSZoning, Condition1, RoofStyle, MasVnrType, Foundation,  Heating, HeatingQC, TotRmsAbvGrd, Functional!, fireplaces, hometotalsf!!!!
#remove hometotalsf
raw_data <- raw_data[,-46]
model <- lm(SalePrice~., data = raw_data)
summary(model)

#change column names because bagging tree function was having problems with these names
colnames(raw_data)[which(names(raw_data) == ".RoofMatl_Tar&Grv")] <- ".RoofMatl_TarGrv"
colnames(raw_data)[which(names(raw_data) == ".Exter1st_Wd Sdng")] <- ".Exter1st_WdSdng"
colnames(raw_data)[which(names(raw_data) == ".Exter2nd_Wd Shng")] <- ".Exter2nd_WdShng"
colnames(raw_data)[which(names(raw_data) == ".Exter2nd_Brk Cmn")] <- ".Exter2nd_BrkCmn"


names(which(colSums(is.na(raw_data))>0))
raw_data <- raw_data[,-c(57,43,165,181:185)]

train <- raw_data[1:1460,]
test <- raw_data[1461:2919,]

model <- lm(SalePrice ~., data = train)
sum <- summary(model)

Test_pred <- predict(model, test)

SalePrice <- 10^Test_pred

df <- data.frame(SalePrice)

write.csv(df,"predictions.csv")
#.13828

#Deal with Aliased Coefficents

raw_data <- raw_data[,-c(57,43,165,181:185)]

train <- raw_data[1:1460,]
test <- raw_data[1461:2919,]

model <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=train)
vif(model)

Test_pred <- predict(model, test)

SalePrice <- 10^Test_pred

df <- data.frame(SalePrice)

write.csv(df,"predictions.csv")

#.13729

model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#Drop BsmtQual
raw_data <- raw_data[,-13]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#Drop FireplaceQu
raw_data <- raw_data[,-31]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#Drop Bsmt Exposure
raw_data <- raw_data[,-14]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#Drop MSSubClass 190
raw_data <- raw_data[,-53]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#GarageArea_x_cars 
raw_data <- raw_data[,-158]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.Exter2nd_CmentBd 
raw_data <- raw_data[,-130]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.MiscFeature_Shed 
raw_data <- raw_data[,-157]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.MSSubClass_50 
raw_data <- raw_data[,-46]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#HouseAge
raw_data <- raw_data[,-8]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#MasVnrArea
raw_data <- raw_data[,-9]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
summary(model1)
vif(model1)
#GasA
raw_data <- raw_data[,-142]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.Exter1st_HdBoard  
raw_data <- raw_data[,-118]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.Cond1_Norm   
raw_data <- raw_data[,-88]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#ExterQual   
raw_data <- raw_data[,-9]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.HouseStyle_SLvl    
raw_data <- raw_data[,-103]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#GarageFinish    
raw_data <- raw_data[,-28]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.RoofMatl_CompShg   
raw_data <- raw_data[,-105]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#BsmtFinType2   
raw_data <- raw_data[,-12]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.Exter2nd_Wd Sdng`  
raw_data <- raw_data[,-126]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.MSSubClass_60 
raw_data <- raw_data[,-41]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.Neighbor_Somerst 
raw_data <- raw_data[,-77]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#KitchenQual 
raw_data <- raw_data[,-22]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#GarageAge
raw_data <- raw_data[,-25]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.Neighbor_OldTown 
raw_data <- raw_data[,-72]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.SaleCondition_Partial
raw_data <- raw_data[,-150]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.Exter1st_AsbShng
raw_data <- raw_data[,-102]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
#.Cond2_Norm 
raw_data <- raw_data[,-87]
model1 <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=raw_data)
vif(model1)
summary(model1) 

#Check for non-linearity and heteroscedasticity
residualPlot(model1)
#looks fantastic!
#Check for Influential points
cooksD_cutoff <- 0.5
cooksD <- cooks.distance(model1)
cooksD_dataframe <- data.frame(obs = names(cooksD), cooks = cooksD)
cooksD_dataframe[which(abs(cooksD) > cooksD_cutoff), ]
#no influential points!


#recheck for better kaggle score (last one was .13729)
train <- raw_data[1:1460,]
test <- raw_data[1461:2919,]

plainv_modeltrain <- lm(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5 ,data=train)
vif(plainv_modeltrain)

Test_pred <- predict(plainv_modeltrain, test)

SalePrice <- 10^Test_pred

df <- data.frame(SalePrice)

write.csv(df,"predictions.csv")

#Score: 0.14893

#Perform AIC using Forward, Backward, and Hybrid 
#AIC Forward
Forward_AIC <- stepAIC(plainv_modeltrain,direction = "forward", k=2, trace = 1)
summary(Forward_AIC)

Forward_AIC_predict <- predict(Forward_AIC, test)
SalePriceForAIC <- 10^Forward_AIC_predict
dfForAIC <- data.frame(SalePriceForAIC)
write.csv(dfForAIC,"predictionsForAIC.csv")
#Score: 0.14893

#14.2 Backward selection, AIC, training MSE and test MSE.
Backward_AIC <- stepAIC(plainv_modeltrain,direction = "backward")

Backward_AIC_predict <- predict(Backward_AIC,test)
SalePriceBackAIC <- 10^Backward_AIC_predict
dfBackAIC <- data.frame(SalePriceBackAIC)
write.csv(dfBackAIC,"predictionsBackAIC.csv")
#Score: 0.14893

#14.3 Hybrid selection, AIC, training MSE and test MSE.
Hybrid_AIC <- stepAIC(plainv_modeltrain, direction = "both")
Hybrid_AIC_predict <- predict(Hybrid_AIC, test)
SalePriceHybAIC <- 10^Hybrid_AIC_predict
dfHybAIC <- data.frame(SalePriceHybAIC)
write.csv(dfHybAIC,"predictionsHybAIC.csv")
#Score: 0.14937

#Ridge

#18.1 Create an x matrix and a y vector for both training and test data sets.
set.seed(1)
x_train<- model.matrix(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5, data= raw_data)
x_train
x_train <- x_train[,-1]
y_train <- (train$SalePrice)

test$SalePrice <- 1
set.seed(1)
x_test <- model.matrix(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5, data= test)
test$SalePrice <- NA
x_test <- x_test[,-1]
y_test<- (test$SalePrice)

set.seed(1)
cv_out_ridge <- cv.glmnet(x_train, y_train, alpha = 0, 
                          type.measure = "mse", nfolds = 10)
bestlam <- cv_out_ridge$lambda.min
bestlam

#18.3 Use the best lambda to run the ridge regression model 
#on the training data set.
model_ridge <- glmnet (x_train, y_train, alpha = 0, 
                       standardize = TRUE, lambda = bestlam)

#18.4 Generate the coefficients estimates of the final Ridge regression model.
model_ridge$beta
#18.5 Training MSE
model_ridge_train_predict <- predict(model_ridge, newx = x_train)

#18.6 Test MSE
model_ridge_predict <- predict(model_ridge, newx = x_test)

SalePriceRidge <- 10^model_ridge_predict
dfRidge <- data.frame(SalePriceRidge)
write.csv(dfRidge,"predictionsRidge.csv")
#Score: 0.14607

#Lasso
set.seed(1)
x_train<- model.matrix(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5, data= raw_data)
x_train
x_train <- x_train[,-1]
y_train <- (train$SalePrice)
test$SalePrice <- 1
set.seed(1)
x_test <- model.matrix(SalePrice~. - BsmtCond^4 - BsmtFinType1^6 - GarageQual^5 - GarageCond^5, data= test)
test$SalePrice <- NA
x_test <- x_test[,-1]

set.seed(1)
cv_out_lasso <- cv.glmnet(x_train, y_train, alpha = 1, 
                          type.measure = "mse", nfolds = 5)
bestlam_lasso <- cv_out_lasso$lambda.min
model_lasso <- glmnet (x_train, y_train, alpha = 1, standardize = TRUE, lambda = bestlam_lasso)
model_lasso$beta
model_lasso_train_predict <- predict(model_lasso, newx = x_train)

model_lasso_predict <- predict(model_lasso, newx = x_test)
SalePriceLasso <- 10^model_lasso_predict
dfLasso <- data.frame(SalePriceLasso)
write.csv(dfLasso,"predictionsLasso.csv")
#Score: 0.14420

#Regression trees with Bagging
library(randomForest)
library(tree)
Simple_tree <- tree(formula = SalePrice ~ LotFrontage + LotArea + Street + LotShape + LandSlope + OverallCond +  RemodAge  +  ExterCond  +  BsmtCond  +  BsmtFinType1  +  TotalBsmtSF  +  HeatingQC  +  CentralAir  +  GrLivArea  +  BsmtFullBath  +  BsmtHalfBath  +  FullBath  +  HalfBath  +  BedroomAbvGr  +  KitchenAbvGr  +  TotRmsAbvGrd  +  Functional  +  Fireplaces  +  GarageArea  +  GarageQual  +  GarageCond  +  PavedDrive  +  WoodDeckSF  +  OpenPorchSF  +  ScreenPorch  +  PoolArea  +  MiscVal  + IsNew  +  IsRemod  +  .MSSubClass_20  +  .MSSubClass_30  +  .MSSubClass_40  +  .MSSubClass_70 + .MSSubClass_75  +  .MSSubClass_80  +  .MSSubClass_85  +  .MSSubClass_160  +  .MSZoning_FV  +  .MSZoning_RH  +  .MSZoning_RM  +  .Alley_None  +  .Alley_Pave  +  .Contour_HLS  +  .Contour_Low  +  .Contour_Lvl  +  .Config_CulDSac  +  .Config_FR2  +  .Config_FR3  +  .Config_Inside  +  .Neighbor_Blmngtn  +  .Neighbor_Blueste  +  .Neighbor_BrDale  + .Neighbor_BrkSide  + .Neighbor_ClearCr + .Neighbor_CollgCr  + .Neighbor_Crawfor  + .Neighbor_Edwards  + .Neighbor_Gilbert  + .Neighbor_IDOTRR  + .Neighbor_MeadowV  + .Neighbor_Mitchel  + .Neighbor_NAmes  + .Neighbor_NoRidge  + .Neighbor_NPkVill  + .Neighbor_NridgHt  + .Neighbor_Sawyer  + .Neighbor_SawyerW  + .Neighbor_StoneBr  + .Neighbor_SWISU + .Neighbor_Timber + .Neighbor_Veenker + .Cond1_Artery + .Cond1_Feedr + .Cond1_PosA + .Cond1_RRAe + .Cond1_RRAn + .Cond1_RRNe + .Cond1_RRNn + .Cond2_Artery + .Cond2_Feedr + .Cond2_PosA + .Cond2_PosN + .BldgType_2fmCon + .BldgType_Twnhs + .HouseStyle_1.5Fin + .HouseStyle_1.5Unf + .HouseStyle_2.5Unf + .HouseStyle_SFoyer + .RoofStyle_Gambrel + .RoofStyle_Mansard + .RoofStyle_Shed  + .RoofMatl_WdShake + .RoofMatl_WdShngl + .RoofMatl_TarGrv + .Exter1st_AsphShn + .Exter1st_BrkComm + .Exter1st_BrkFace + .Exter1st_CemntBd + .Exter1st_Plywood + .Exter1st_Stucco + .Exter1st_WdShing + .Exter1st_WdSdng + .Exter2nd_AsbShng + .Exter2nd_AsphShn+ .Exter2nd_BrkFace + .Exter2nd_HdBoard + .Exter2nd_ImStucc + .Exter2nd_MetalSd + .Exter2nd_Plywood + .Exter2nd_Stone + .Exter2nd_Stucco + .Exter2nd_WdShng + .Exter2nd_BrkCmn + .MasVnrType_BrkFace + .MasVnrType_Stone + .Foundation_BrkTil + .Foundation_PConc + .Foundation_Slab + .Foundation_Stone + .Foundation_Wood + .data_GasW + .data_Grav + .data_Wall + .Electrical_FuseA + .Electrical_FuseF + .Electrical_FuseP + .GarageType_Basment + .GarageType_BuiltIn + .GarageType_CarPort + .GarageType_Detchd + Garage_Newest + .SaleType_Con + .SaleType_ConLD + .SaleType_ConLI + .SaleType_ConLw + .SaleType_CWD + .SaleType_Oth + .SaleType_WD + .SaleCondition_AdjLand + .SaleCondition_Alloca + .SaleCondition_Family  , data = train)
summary(Simple_tree)
set.seed(1)
cv_Housing_Tree <- cv.tree(Simple_tree, K = 10)
plot(cv_Housing_Tree$size, cv_Housing_Tree$dev, type = 'b')

#set.seed because the sampling with replacement is random.
set.seed(1)
Tree_Bagging <- randomForest(SalePrice ~ ., data = train,
                             ntrees = 500, mtry = 158, replace = TRUE,
                             importance = TRUE)
Tree_Bagging
plot(Tree_Bagging)
importance(Tree_Bagging)
#23.2 Which predictors are important to predict SalePrice? 20 or higher %IncMSE
#LotArea, OverallCond, RemodAge, BsmtFinType1, TotalBsmtSF, GrLivArea, Fireplaces, GarageArea 

#23.3 Training MSE using the bagged regression tree.
MSE_Train_pred_bag <- predict(Tree_Bagging,train)


#23.4 Test MSE using the bagged regression tree.
Test_pred_bag <- predict(Tree_Bagging, test)

SalePriceBagging <- 10^Test_pred_bag
dfBagging <- data.frame(SalePriceBagging)
write.csv(dfBagging,"predictionsBagging.csv")
#Score: 0.15269

#Random Forest
##24. Random Forest (RF)
#24.1 Perform RF approach using 500 bootstraps
#Remember that the main difference between bagging and RF is that
#when constructing decision trees using bootstrap data sets,
#for each split RF considers only a subset of 
#the full set of predictors as split candidates.
Test_MSE_RF <- rep(0, 146)
for(i in 1:12){
  set.seed(1)
  Tree_RF <- randomForest(SalePrice ~ ., data = train,
                          ntrees = 500, mtry = i, replace = TRUE,
                          importance = TRUE)
  Test_pred_RF <- predict(Tree_RF, test)
  Test_MSE_RF[i] <- mean((test$SalePrice - Test_pred_RF)^2)
}
which.min(Test_MSE_RF)
##########

set.seed(1)
Tree_RF <- randomForest(SalePrice~ ., data = train, 
                        ntrees = 500, mtry= 13, replace = TRUE,
                        importance = TRUE)
plot(Tree_RF)
importance(Tree_RF)

Train_pred_rf <- predict(Tree_RF, train)
Test_pred_rf <- predict(Tree_RF, test)
SalePricerf <- 10^Test_pred_rf
dfrf <- data.frame(SalePricerf)
write.csv(dfrf,"predictionsrf.csv")
#Score: 0.15637


##25. Boosting
#25.1 Perform Boosting
library(gbm)
test <- test[,-137]
train <- train[,-137]
Tree_Boosting <- gbm(SalePrice ~., data = train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 1,
                     shrinkage = 0.01)
summary(Tree_Boosting)
Train_pred_boost <- predict(Tree_Boosting, train)
Test_pred_boost <- predict(Tree_Boosting, test)
SalePriceboost <- 10^Test_pred_boost
dfboost <- data.frame(SalePriceboost)
write.csv(dfboost,"predictionsboostdepth1.csv")
#depth 1 .13964

Tree_Boosting <- gbm(SalePrice ~., data = train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 2,
                     shrinkage = 0.01)
summary(Tree_Boosting)
Train_pred_boost <- predict(Tree_Boosting, train)
Test_pred_boost <- predict(Tree_Boosting, test)
SalePriceboost <- 10^Test_pred_boost
dfboost <- data.frame(SalePriceboost)
write.csv(dfboost,"predictionsboostdepth2.csv")
#depth 2 .13398 THE BEST

Tree_Boosting <- gbm(SalePrice ~., data = train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 3,
                     shrinkage = 0.01)
summary(Tree_Boosting)
Train_pred_boost <- predict(Tree_Boosting, train)
Test_pred_boost <- predict(Tree_Boosting, test)
SalePriceboost <- 10^Test_pred_boost
dfboost <- data.frame(SalePriceboost)
write.csv(dfboost,"predictionsboostdepth3.csv")
#depth 3 .13407

Tree_Boosting <- gbm(SalePrice ~., data = train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 4,
                     shrinkage = 0.01)
summary(Tree_Boosting)
Train_pred_boost <- predict(Tree_Boosting, train)
Test_pred_boost <- predict(Tree_Boosting, test)
SalePriceboost <- 10^Test_pred_boost
dfboost <- data.frame(SalePriceboost)
write.csv(dfboost,"predictionsboostdepth4.csv")
#depth 4 .13443

Tree_Boosting <- gbm(SalePrice ~., data = train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 5,
                     shrinkage = 0.01)
summary(Tree_Boosting)
Train_pred_boost <- predict(Tree_Boosting, train)
Test_pred_boost <- predict(Tree_Boosting, test)
SalePriceboost <- 10^Test_pred_boost
dfboost <- data.frame(SalePriceboost)
write.csv(dfboost,"predictionsboostdepth5.csv")
#depth 5 .13483

Tree_Boosting <- gbm(SalePrice ~., data = train, distribution = "gaussian",
                     n.trees = 5000, interaction.depth = 6,
                     shrinkage = 0.01)
summary(Tree_Boosting)
Train_pred_boost <- predict(Tree_Boosting, train)
Test_pred_boost <- predict(Tree_Boosting, test)
SalePriceboost <- 10^Test_pred_boost
dfboost <- data.frame(SalePriceboost)
write.csv(dfboost,"predictionsboostdepth6.csv")
#depth 6 .13655
#USE DEPTH = 2