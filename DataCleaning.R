# SYS 6018: Competition 4
# Data Cleaning

# Read in data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Deal with missing values.
# For binary and categorical variables, replace with mode. If mode is -1, then replace with 0.
# For all other variables, if continuous, replace with mean; if ordinal, replace with mode.
mode <- function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
  if (uniqx[which.max(tabulate(match(x, uniqx)))] == -1){
    return(0)
  } else{
    return(uniqx[which.max(tabulate(match(x, uniqx)))])
  }
}

# Check which columns have missing values
# Training data
for (i in 3:59){
  missingvalues <- sum(train[,i] == -1)
  if (missingvalues > 0){
    columnname <- colnames(train[i])
    print(columnname)
    print(missingvalues)
  }
}
# Testing data
for (i in 2:58){
  missingvalues <- sum(test[,i] == -1)
  if (missingvalues > 0){
    columnname <- colnames(test[i])
    print(columnname)
    print(missingvalues)
  }
}

# Create copies of the raw train and test data
train_clean <- train
test_clean <- test

train_clean$ps_ind_02_cat[train_clean$ps_ind_02_cat == -1] <- mode(train_clean$ps_ind_02_cat)
train_clean$ps_ind_04_cat[train_clean$ps_ind_04_cat == -1] <- mode(train_clean$ps_ind_04_cat)
train_clean$ps_ind_05_cat[train_clean$ps_ind_05_cat == -1] <- mode(train_clean$ps_ind_05_cat)
train_clean$ps_reg_03[train_clean$ps_reg_03 == -1] <- mean(train_clean$ps_reg_03)
train_clean$ps_car_01_cat[train_clean$ps_car_01_cat == -1] <- mode(train_clean$ps_car_01_cat)
train_clean$ps_car_02_cat[train_clean$ps_car_02_cat == -1] <- mode(train_clean$ps_car_02_cat)
train_clean$ps_car_03_cat[train_clean$ps_car_03_cat == -1] <- mode(train_clean$ps_car_03_cat)
train_clean$ps_car_05_cat[train_clean$ps_car_05_cat == -1] <- mode(train_clean$ps_car_05_cat)
train_clean$ps_car_07_cat[train_clean$ps_car_07_cat == -1] <- mode(train_clean$ps_car_07_cat)
train_clean$ps_car_09_cat[train_clean$ps_car_09_cat == -1] <- mode(train_clean$ps_car_09_cat)
train_clean$ps_car_11[train_clean$ps_car_11 == -1] <- mode(train_clean$ps_car_11)
train_clean$ps_car_12[train_clean$ps_car_12 == -1] <- mean(train_clean$ps_car_12)
train_clean$ps_car_14[train_clean$ps_car_14 == -1] <- mean(train_clean$ps_car_14)

test_clean$ps_ind_02_cat[test_clean$ps_ind_02_cat == -1] <- mode(test_clean$ps_ind_02_cat)
test_clean$ps_ind_04_cat[test_clean$ps_ind_04_cat == -1] <- mode(test_clean$ps_ind_04_cat)
test_clean$ps_ind_05_cat[test_clean$ps_ind_05_cat == -1] <- mode(test_clean$ps_ind_05_cat)
test_clean$ps_reg_03[test_clean$ps_reg_03 == -1] <- mean(test_clean$ps_reg_03)
test_clean$ps_car_01_cat[test_clean$ps_car_01_cat == -1] <- mode(test_clean$ps_car_01_cat)
test_clean$ps_car_02_cat[test_clean$ps_car_02_cat == -1] <- mode(test_clean$ps_car_02_cat)
test_clean$ps_car_03_cat[test_clean$ps_car_03_cat == -1] <- mode(test_clean$ps_car_03_cat)
test_clean$ps_car_05_cat[test_clean$ps_car_05_cat == -1] <- mode(test_clean$ps_car_05_cat)
test_clean$ps_car_07_cat[test_clean$ps_car_07_cat == -1] <- mode(test_clean$ps_car_07_cat)
test_clean$ps_car_09_cat[test_clean$ps_car_09_cat == -1] <- mode(test_clean$ps_car_09_cat)
test_clean$ps_car_11[test_clean$ps_car_11 == -1] <- mode(test_clean$ps_car_11)
test_clean$ps_car_14[test_clean$ps_car_14 == -1] <- mean(test_clean$ps_car_14)

# Check to make sure all missing values have been addressed
# Training data
for (i in 3:59){
  missingvalues <- sum(train_clean[,i] == -1)
  if (missingvalues > 0){
    columnname <- colnames(train_clean[i])
    print(columnname)
    print(missingvalues)
  }
}
# Testing data
for (i in 2:58){
  missingvalues <- sum(test_clean[,i] == -1)
  if (missingvalues > 0){
    columnname <- colnames(test_clean[i])
    print(columnname)
    print(missingvalues)
  }
}