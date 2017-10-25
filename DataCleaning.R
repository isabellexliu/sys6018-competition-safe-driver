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
train$ps_ind_02_cat[train$ps_ind_02_cat == -1] <- mode(train$ps_ind_02_cat)
train$ps_ind_04_cat[train$ps_ind_04_cat == -1] <- mode(train$ps_ind_04_cat)
train$ps_ind_05_cat[train$ps_ind_05_cat == -1] <- mode(train$ps_ind_05_cat)
train$ps_reg_03[train$ps_reg_03 == -1] <- mean(train$ps_reg_03)
train$ps_car_01_cat[train$ps_car_01_cat == -1] <- mode(train$ps_car_01_cat)
train$ps_car_02_cat[train$ps_car_02_cat == -1] <- mode(train$ps_car_02_cat)
train$ps_car_03_cat[train$ps_car_03_cat == -1] <- mode(train$ps_car_03_cat)
train$ps_car_05_cat[train$ps_car_05_cat == -1] <- mode(train$ps_car_05_cat)
train$ps_car_07_cat[train$ps_car_07_cat == -1] <- mode(train$ps_car_07_cat)
train$ps_car_09_cat[train$ps_car_09_cat == -1] <- mode(train$ps_car_09_cat)
train$ps_car_11[train$ps_car_11 == -1] <- mode(train$ps_car_11)
train$ps_car_12[train$ps_car_12 == -1] <- mean(train$ps_car_12)
train$ps_car_14[train$ps_car_14 == -1] <- mean(train$ps_car_14)

test$ps_ind_02_cat[test$ps_ind_02_cat == -1] <- mode(test$ps_ind_02_cat)
test$ps_ind_04_cat[test$ps_ind_04_cat == -1] <- mode(test$ps_ind_04_cat)
test$ps_ind_05_cat[test$ps_ind_05_cat == -1] <- mode(test$ps_ind_05_cat)
test$ps_reg_03[test$ps_reg_03 == -1] <- mean(test$ps_reg_03)
test$ps_car_01_cat[test$ps_car_01_cat == -1] <- mode(test$ps_car_01_cat)
test$ps_car_02_cat[test$ps_car_02_cat == -1] <- mode(test$ps_car_02_cat)
test$ps_car_03_cat[test$ps_car_03_cat == -1] <- mode(test$ps_car_03_cat)
test$ps_car_05_cat[test$ps_car_05_cat == -1] <- mode(test$ps_car_05_cat)
test$ps_car_07_cat[test$ps_car_07_cat == -1] <- mode(test$ps_car_07_cat)
test$ps_car_09_cat[test$ps_car_09_cat == -1] <- mode(test$ps_car_09_cat)
test$ps_car_11[test$ps_car_11 == -1] <- mode(test$ps_car_11)
test$ps_car_14[test$ps_car_14 == -1] <- mean(test$ps_car_14)