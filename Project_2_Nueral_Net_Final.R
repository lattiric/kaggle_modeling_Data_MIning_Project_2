# Function to clean data
clean_data <- function(df) {
  # Drop columns with too many missing values
  df <- df[, !names(df) %in% c("PoolQC", "MiscFeature", "Alley", "Fence")]
  
  # Impute missing values
  df$FireplaceQu[is.na(df$FireplaceQu)] <- 'None'
  df$MasVnrArea[is.na(df$MasVnrArea)] <- 103.7 # mean val for this col
  df$MasVnrType[is.na(df$MasVnrType)] <- 2.761 # mean val for this col
  
  garage_categoricals <- c("GarageType", "GarageFinish", "GarageQual", "GarageCond")
  df[garage_categoricals] <- lapply(df[garage_categoricals], function(x) {
    x[is.na(x)] <- 'None'
    x
  })
  
  basement_categoricals <- c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")
  df[basement_categoricals] <- lapply(df[basement_categoricals], function(x) {
    x[is.na(x)] <- 'None'
    x
  })
  
  df$GarageYrBlt[is.na(df$GarageYrBlt)] <- 0
  df$GarageYrBlt <- as.integer(df$GarageYrBlt)
  
  if ("LotFrontage" %in% names(df)) {
    median_lotfrontage <- median(df$LotFrontage, na.rm = TRUE)
    df$LotFrontage[is.na(df$LotFrontage)] <- median_lotfrontage
  }
  
  if ("Electrical" %in% names(df)) {
    mode_electrical <- as.character(names(sort(table(df$Electrical), decreasing = TRUE)[1]))
    df$Electrical[is.na(df$Electrical)] <- mode_electrical
  }
  
  # Convert categorical values to factors
  categorical_vars <- c("MSZoning", "Street", "LotShape", "LandContour", "Utilities",
                        "LotConfig", "LandSlope", "Neighborhood", "Condition1", "Condition2",
                        "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st",
                        "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", "Foundation",
                        "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2",
                        "Heating", "HeatingQC", "CentralAir", "Electrical", "KitchenQual",
                        "Functional", "FireplaceQu", "GarageType", "GarageFinish", "GarageQual",
                        "GarageCond", "PavedDrive", "SaleType", "SaleCondition")
  
  for (var in categorical_vars) {
    if (var %in% names(df)) {
      df[[var]] <- as.factor(df[[var]])
      df[[var]] <- as.integer(df[[var]])
    }
  }
  
  return(df)
}

# Load data
train <- read.csv("/Users/ricklattin/Documents/SMU Year 4 Sem 1/Data_Mining/Project_2_Kaggle/house-prices-advanced-regression-techniques/train.csv", header = TRUE)
test <- read.csv("/Users/ricklattin/Documents/SMU Year 4 Sem 1/Data_Mining/Project_2_Kaggle/house-prices-advanced-regression-techniques/test.csv", header = TRUE)

# Clean data
train_clean <- clean_data(train)
test_clean <- clean_data(test)

# Check the head of the cleaned data
head(train_clean)
head(test_clean)

summary(train)

# install.packages("neuralnet")
# library(neuralnet)
# # train a nueral network but remove the activation function so it does not classify
# m <-neuralnet(SalePrice ~ MSSubClass+MSZoning+LotArea+Street+LotShape+
#     LandContour+Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+
#     HouseStyle+OverallQual+OverallCond+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+
#     Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+
#     BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinType2+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+Heating+
#     HeatingQC+CentralAir+Electrical+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+
#     BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+
#     Fireplaces+GarageType+GarageFinish+GarageCars+GarageArea+GarageQual+
#     GarageCond+PavedDrive+WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+
#     MiscVal+MoSold+YrSold+SaleType+SaleCondition,
#       train_clean,
#       hidden = c(3,4,5), #neurons in each hidden layer
#       linear.output = TRUE, # used to be FALSE for regression/classification
#       lifesign = "full", # print while training
#       rep = 1, # repitition
#       algorithm = "rprop+",
#       stepmax = 10000)
#
# plot(m)
# m$act.fct
# m$linear.output
# m$weights
# 
# output <- compute(m, train_clean)
# print(output)

#linear model
m <- lm(SalePrice ~ MSSubClass+MSZoning+LotArea+Street+LotShape+
          LandContour+Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+
          HouseStyle+OverallQual+OverallCond+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+
          Exterior2nd+MasVnrType+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+
          BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinType2+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+Heating+
          HeatingQC+CentralAir+Electrical+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+
          BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+
          Fireplaces+GarageType+GarageFinish+GarageCars+GarageArea+GarageQual+
          GarageCond+PavedDrive+WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+
          MiscVal+MoSold+YrSold+SaleType+SaleCondition,
        train_clean)

# predicting using the linear model
pred <- predict(m, test_clean)
output <- cbind(pred, test_clean$SalePrice)
output
plot(m)
test_clean

# formatting the data for submission
submission_table <- data.frame(
  Index = 1461:2919,
  Data = output
)
submission_table$pred[is.na(submission_table$pred)] <- 170000.0
summary(submission_table)

# exporting the data
file_path <- "Project_2_submission.csv"
write.csv(submission_table, file = file_path, row.names = FALSE)
