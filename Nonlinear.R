# SYS 6018: Competition 4
# Data Cleaning

library(tidyverse)
library(MASS)
library(tree)
library(randomForest)
library(gbm)
library(tree)
library(boot)
library(splines)
library(gam)
source("Gini.R") # in order to use functions in Gini

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
# As we can see from the printout from above 2 for-loops,
# most values in "ps_car_03_cat" and "ps_car_05_cat" are missing,
# so we get rid of the two columns when making copies.
train_clean <- subset(train, select = -c(ps_car_03_cat, ps_car_05_cat))
test_clean <- subset(test, select = -c(ps_car_03_cat, ps_car_05_cat))

train_clean$ps_ind_02_cat[train_clean$ps_ind_02_cat == -1] <- mode(train_clean$ps_ind_02_cat)
train_clean$ps_ind_04_cat[train_clean$ps_ind_04_cat == -1] <- mode(train_clean$ps_ind_04_cat)
train_clean$ps_ind_05_cat[train_clean$ps_ind_05_cat == -1] <- mode(train_clean$ps_ind_05_cat)
train_clean$ps_reg_03[train_clean$ps_reg_03 == -1] <- mean(train_clean$ps_reg_03)
train_clean$ps_car_01_cat[train_clean$ps_car_01_cat == -1] <- mode(train_clean$ps_car_01_cat)
train_clean$ps_car_02_cat[train_clean$ps_car_02_cat == -1] <- mode(train_clean$ps_car_02_cat)
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
test_clean$ps_car_07_cat[test_clean$ps_car_07_cat == -1] <- mode(test_clean$ps_car_07_cat)
test_clean$ps_car_09_cat[test_clean$ps_car_09_cat == -1] <- mode(test_clean$ps_car_09_cat)
test_clean$ps_car_11[test_clean$ps_car_11 == -1] <- mode(test_clean$ps_car_11)
test_clean$ps_car_14[test_clean$ps_car_14 == -1] <- mean(test_clean$ps_car_14)

# Check to make sure all missing values have been addressed
# Training data
for (i in 3:57){
  missingvalues <- sum(train_clean[,i] == -1)
  if (missingvalues > 0){
    columnname <- colnames(train_clean[i])
    print(columnname)
    print(missingvalues)
  }
}
# Testing data
for (i in 2:56){
  missingvalues <- sum(test_clean[,i] == -1)
  if (missingvalues > 0){
    columnname <- colnames(test_clean[i])
    print(columnname)
    print(missingvalues)
  }
}

# Deal with categorical variable.
train_clean$ps_ind_02_cat <- as.factor(train_clean$ps_ind_02_cat)
train_clean$ps_ind_04_cat <- as.factor(train_clean$ps_ind_04_cat)
train_clean$ps_ind_05_cat <- as.factor(train_clean$ps_ind_05_cat)
train_clean$ps_car_01_cat <- as.factor(train_clean$ps_car_01_cat)
train_clean$ps_car_02_cat <- as.factor(train_clean$ps_car_02_cat)
train_clean$ps_car_04_cat <- as.factor(train_clean$ps_car_04_cat)
train_clean$ps_car_06_cat <- as.factor(train_clean$ps_car_06_cat)
train_clean$ps_car_07_cat <- as.factor(train_clean$ps_car_07_cat)
train_clean$ps_car_08_cat <- as.factor(train_clean$ps_car_08_cat)
train_clean$ps_car_09_cat <- as.factor(train_clean$ps_car_09_cat)
train_clean$ps_car_10_cat <- as.factor(train_clean$ps_car_10_cat)
train_clean$ps_car_11_cat <- as.factor(train_clean$ps_car_11_cat)

test_clean$ps_ind_02_cat <- as.factor(test_clean$ps_ind_02_cat)
test_clean$ps_ind_04_cat <- as.factor(test_clean$ps_ind_04_cat)
test_clean$ps_ind_05_cat <- as.factor(test_clean$ps_ind_05_cat)
test_clean$ps_car_01_cat <- as.factor(test_clean$ps_car_01_cat)
test_clean$ps_car_02_cat <- as.factor(test_clean$ps_car_02_cat)
test_clean$ps_car_04_cat <- as.factor(test_clean$ps_car_04_cat)
test_clean$ps_car_06_cat <- as.factor(test_clean$ps_car_06_cat)
test_clean$ps_car_07_cat <- as.factor(test_clean$ps_car_07_cat)
test_clean$ps_car_08_cat <- as.factor(test_clean$ps_car_08_cat)
test_clean$ps_car_09_cat <- as.factor(test_clean$ps_car_09_cat)
test_clean$ps_car_10_cat <- as.factor(test_clean$ps_car_10_cat)
test_clean$ps_car_11_cat <- as.factor(test_clean$ps_car_11_cat)

### Nonlinear methods ###

# Smoothing splines and GAMs #

# First check what smoothness level is optimal for individual
# variables' smoothing splines through cross-validation
set.seed(6)
ss1=smooth.spline(train_clean$ps_ind_01,train_clean$target,cv=TRUE)
ss1$df  # 2.277957

ss2=smooth.spline(train_clean$ps_ind_02_cat,train_clean$target,cv=TRUE)
ss2$df  # 2.000001

ss3=smooth.spline(train_clean$ps_ind_03,train_clean$target,cv=TRUE)
ss3$df  # 12

ss4=smooth.spline(train_clean$ps_ind_04_cat,train_clean$target,cv=TRUE)
ss4$df  # Error: need at least four unique 'x' values

ss5=smooth.spline(train_clean$ps_ind_05_cat,train_clean$target,cv=TRUE)
ss5$df  # Error: 'tol' must be strictly positive and finite

ss6=smooth.spline(train_clean$ps_ind_06_bin,train_clean$target,cv=TRUE)
ss6$df  # Error: need at least four unique 'x' values

ss7=smooth.spline(train_clean$ps_ind_07_bin,train_clean$target,cv=TRUE)
ss7$df  # Error: need at least four unique 'x' values

ss8=smooth.spline(train_clean$ps_ind_08_bin,train_clean$target,cv=TRUE)
ss8$df  # Error: 'tol' must be strictly positive and finite

ss9=smooth.spline(train_clean$ps_ind_09_bin,train_clean$target,cv=TRUE)
ss9$df  # Error: 'tol' must be strictly positive and finite

ss10=smooth.spline(train_clean$ps_ind_10_bin,train_clean$target,cv=TRUE)
ss10$df  # Error: 'tol' must be strictly positive and finite

ss11=smooth.spline(train_clean$ps_ind_11_bin,train_clean$target,cv=TRUE)
ss11$df  # Error: 'tol' must be strictly positive and finite

ss12=smooth.spline(train_clean$ps_ind_12_bin,train_clean$target,cv=TRUE)
ss12$df  # Error: 'tol' must be strictly positive and finite

ss13=smooth.spline(train_clean$ps_ind_13_bin,train_clean$target,cv=TRUE)
ss13$df  # Error: 'tol' must be strictly positive and finite

ss14=smooth.spline(train_clean$ps_ind_14,train_clean$target,cv=TRUE)
ss14$df  # Error: 'tol' must be strictly positive and finite

ss15=smooth.spline(train_clean$ps_ind_15,train_clean$target,cv=TRUE)
ss15$df   # 14.00001

ss16=smooth.spline(train_clean$ps_ind_16_bin,train_clean$target,cv=TRUE)
ss16$df  # Error: need at least four unique 'x' values

ss17=smooth.spline(train_clean$ps_ind_17_bin,train_clean$target,cv=TRUE)
ss17$df  # Error: 'tol' must be strictly positive and finite

ss18=smooth.spline(train_clean$ps_ind_18_bin,train_clean$target,cv=TRUE)
ss18$df  # Error: 'tol' must be strictly positive and finite

ss19=smooth.spline(train_clean$ps_reg_01,train_clean$target,cv=TRUE)
ss19$df   # 9.999988

ss20=smooth.spline(train_clean$ps_reg_02,train_clean$target,cv=TRUE)
ss20$df   # 2.000005

ss21=smooth.spline(train_clean$ps_reg_03,train_clean$target,cv=TRUE)
ss21$df   # 7.441016

ss22=smooth.spline(train_clean$ps_car_01_cat,train_clean$target,cv=TRUE)
ss22$df   # 4.37158

ss23=smooth.spline(train_clean$ps_car_02_cat,train_clean$target,cv=TRUE)
ss23$df  # Error: 'tol' must be strictly positive and finite

ss24=smooth.spline(train_clean$ps_car_04_cat,train_clean$target,cv=TRUE)
ss24$df  # Error: 'tol' must be strictly positive and finite

ss25=smooth.spline(train_clean$ps_car_06_cat,train_clean$target,cv=TRUE)
ss25$df   # 2.000006

ss26=smooth.spline(train_clean$ps_car_07_cat,train_clean$target,cv=TRUE)
ss26$df  # Error: 'tol' must be strictly positive and finite

ss27=smooth.spline(train_clean$ps_car_08_cat,train_clean$target,cv=TRUE)
ss27$df  # Error: 'tol' must be strictly positive and finite

ss28=smooth.spline(train_clean$ps_car_09_cat,train_clean$target,cv=TRUE)
ss28$df   # 2

ss29=smooth.spline(train_clean$ps_car_10_cat,train_clean$target,cv=TRUE)
ss29$df  # Error: 'tol' must be strictly positive and finite

ss30=smooth.spline(train_clean$ps_car_11_cat,train_clean$target,cv=TRUE)
ss30$df   # 2.001177

ss31=smooth.spline(train_clean$ps_car_11,train_clean$target,cv=TRUE)
ss31$df   # 2

ss32=smooth.spline(train_clean$ps_car_12,train_clean$target,cv=TRUE)
ss32$df   # 22.96648

ss33=smooth.spline(train_clean$ps_car_13,train_clean$target,cv=TRUE)
ss33$df   # 8.768609

ss34=smooth.spline(train_clean$ps_car_14,train_clean$target,cv=TRUE)
ss34$df   # 18.90449

ss35=smooth.spline(train_clean$ps_car_15,train_clean$target,cv=TRUE)
ss35$df   # 4.086362

ss36=smooth.spline(train_clean$ps_calc_01,train_clean$target,cv=TRUE)
ss36$df   # 2.000001

ss37=smooth.spline(train_clean$ps_calc_02,train_clean$target,cv=TRUE)
ss37$df   # 3.460283

ss38=smooth.spline(train_clean$ps_calc_03,train_clean$target,cv=TRUE)
ss38$df   # 3.973857

ss39=smooth.spline(train_clean$ps_calc_04,train_clean$target,cv=TRUE)
ss39$df   # 6

ss40=smooth.spline(train_clean$ps_calc_05,train_clean$target,cv=TRUE)
ss40$df   # 5.025339

ss41=smooth.spline(train_clean$ps_calc_06,train_clean$target,cv=TRUE)
ss41$df   # 2.000001

ss42=smooth.spline(train_clean$ps_calc_07,train_clean$target,cv=TRUE)
ss42$df   # 2.206999

ss43=smooth.spline(train_clean$ps_calc_08,train_clean$target,cv=TRUE)
ss43$df   # 2.000001

ss44=smooth.spline(train_clean$ps_calc_09,train_clean$target,cv=TRUE)
ss44$df   # 2.000003

ss45=smooth.spline(train_clean$ps_calc_10,train_clean$target,cv=TRUE)
ss45$df   # 2.000004

ss46=smooth.spline(train_clean$ps_calc_11,train_clean$target,cv=TRUE)
ss46$df   # 2.744885

ss47=smooth.spline(train_clean$ps_calc_12,train_clean$target,cv=TRUE)
ss47$df   # 2.000002

ss48=smooth.spline(train_clean$ps_calc_13,train_clean$target,cv=TRUE)
ss48$df   # 2.000001

ss49=smooth.spline(train_clean$ps_calc_14,train_clean$target,cv=TRUE)
ss49$df   # 2.000005

ss50=smooth.spline(train_clean$ps_calc_15_bin,train_clean$target,cv=TRUE)
ss50$df   # Error: 'tol' must be strictly positive and finite

ss51=smooth.spline(train_clean$ps_calc_16_bin,train_clean$target,cv=TRUE)
ss51$df   # Error: need at least four unique 'x' values

ss52=smooth.spline(train_clean$ps_calc_17_bin,train_clean$target,cv=TRUE)
ss52$df   # Error: need at least four unique 'x' values

ss53=smooth.spline(train_clean$ps_calc_18_bin,train_clean$target,cv=TRUE)
ss53$df   # Error: need at least four unique 'x' values

ss54=smooth.spline(train_clean$ps_calc_19_bin,train_clean$target,cv=TRUE)
ss54$df   # Error: need at least four unique 'x' values

ss55=smooth.spline(train_clean$ps_calc_20_bin,train_clean$target,cv=TRUE)
ss55$df   # Error: 'tol' must be strictly positive and finite

# Generate a GAM using all 55 variables, using smoothing splines with degrees of freedom from above code
# for quantitative variables when applicable, and just including the categorical and binary variables as they are
gam.m1=gam(target~s(ps_ind_01,2)+ps_ind_02_cat+s(ps_ind_03,12)+ps_ind_04_cat+
             ps_ind_05_cat+ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+
             ps_ind_10_bin+ps_ind_11_bin+ps_ind_12_bin+ps_ind_13_bin+ps_ind_14+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+ps_ind_18_bin+
             s(ps_reg_01,10)+s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+
             ps_car_02_cat+ps_car_04_cat+ps_car_06_cat+ps_car_07_cat+
             ps_car_08_cat+ps_car_09_cat+ps_car_10_cat+ps_car_11_cat+
             s(ps_car_11,2)+s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+
             s(ps_car_15,4)+s(ps_calc_01,2)+s(ps_calc_02,3)+s(ps_calc_03,4)+
             s(ps_calc_04,6)+s(ps_calc_05,5)+s(ps_calc_06,2)+s(ps_calc_07,2)+
             s(ps_calc_08,2)+s(ps_calc_09,2)+s(ps_calc_10,2)+s(ps_calc_11,3)+
             s(ps_calc_12,2)+s(ps_calc_13,2)+s(ps_calc_14,2)+ps_calc_15_bin+
             ps_calc_16_bin+ps_calc_17_bin+ps_calc_18_bin+ps_calc_19_bin+
             ps_calc_20_bin,data=train_clean)   # GAM with smoothing splines
summary(gam.m1)

# 5-fold cross validation of above GAM model
set.seed(5)
fold <- sample(1:nrow(train_clean), size = (nrow(train_clean) / 5), replace = FALSE)
fold1 <- train_clean[fold,]
other <- train_clean[-fold,]
fold <- sample(1:nrow(other), size = (nrow(other) / 4), replace = FALSE)
fold2 <- other[fold,]
other <- other[-fold,]
fold <- sample(1:nrow(other), size = (nrow(other) / 3), replace = FALSE)
fold3 <- other[fold,]
other <- other[-fold,]
fold <- sample(1:nrow(other), size = (nrow(other) / 2), replace = FALSE)
fold4 <- other[fold,]
fold5 <- other[-fold,]

rm(gam.m1,other)  # Free up memory

# Testing set = 1st fold
trainset <- rbind(fold2,fold3,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+ps_ind_02_cat+s(ps_ind_03,12)+ps_ind_04_cat+
             ps_ind_05_cat+ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+
             ps_ind_10_bin+ps_ind_11_bin+ps_ind_12_bin+ps_ind_13_bin+ps_ind_14+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+ps_ind_18_bin+
             s(ps_reg_01,10)+s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+
             ps_car_02_cat+ps_car_04_cat+ps_car_06_cat+ps_car_07_cat+
             ps_car_08_cat+ps_car_09_cat+ps_car_10_cat+ps_car_11_cat+
             s(ps_car_11,2)+s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+
             s(ps_car_15,4)+s(ps_calc_01,2)+s(ps_calc_02,3)+s(ps_calc_03,4)+
             s(ps_calc_04,6)+s(ps_calc_05,5)+s(ps_calc_06,2)+s(ps_calc_07,2)+
             s(ps_calc_08,2)+s(ps_calc_09,2)+s(ps_calc_10,2)+s(ps_calc_11,3)+
             s(ps_calc_12,2)+s(ps_calc_13,2)+s(ps_calc_14,2)+ps_calc_15_bin+
             ps_calc_16_bin+ps_calc_17_bin+ps_calc_18_bin+ps_calc_19_bin+
             ps_calc_20_bin,data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini  # 0.2831961
summary(gam.training)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03305  0.02294  0.03399  0.03645  0.04748  0.26448 
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold1)
gam.gini <- normalized.gini.index(fold1$target, gam.predictions)
gam.gini  # 0.2734444
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.01805  0.02289  0.03408  0.03641  0.04746  0.20798 
rm(gam.m1)  # Free up memory
# Testing set = 2nd fold
trainset <- rbind(fold1,fold3,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+ps_ind_02_cat+s(ps_ind_03,12)+ps_ind_04_cat+
             ps_ind_05_cat+ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+
             ps_ind_10_bin+ps_ind_11_bin+ps_ind_12_bin+ps_ind_13_bin+ps_ind_14+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+ps_ind_18_bin+
             s(ps_reg_01,10)+s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+
             ps_car_02_cat+ps_car_04_cat+ps_car_06_cat+ps_car_07_cat+
             ps_car_08_cat+ps_car_09_cat+ps_car_10_cat+ps_car_11_cat+
             s(ps_car_11,2)+s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+
             s(ps_car_15,4)+s(ps_calc_01,2)+s(ps_calc_02,3)+s(ps_calc_03,4)+
             s(ps_calc_04,6)+s(ps_calc_05,5)+s(ps_calc_06,2)+s(ps_calc_07,2)+
             s(ps_calc_08,2)+s(ps_calc_09,2)+s(ps_calc_10,2)+s(ps_calc_11,3)+
             s(ps_calc_12,2)+s(ps_calc_13,2)+s(ps_calc_14,2)+ps_calc_15_bin+
             ps_calc_16_bin+ps_calc_17_bin+ps_calc_18_bin+ps_calc_19_bin+
             ps_calc_20_bin,data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini2 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini2  # 0.2852569
summary(gam.training)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02288  0.02293  0.03413  0.03642  0.04751  0.19207 
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold2)
gam.gini2 <- normalized.gini.index(fold2$target, gam.predictions)
gam.gini2  # 0.2681727
summary(gam.predictions)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.01814  0.02296  0.03415  0.03643  0.04746  0.18887 
rm(gam.m1)  # Free up memory
# Testing set = 3rd fold
trainset <- rbind(fold1,fold2,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+ps_ind_02_cat+s(ps_ind_03,12)+ps_ind_04_cat+
             ps_ind_05_cat+ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+
             ps_ind_10_bin+ps_ind_11_bin+ps_ind_12_bin+ps_ind_13_bin+ps_ind_14+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+ps_ind_18_bin+
             s(ps_reg_01,10)+s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+
             ps_car_02_cat+ps_car_04_cat+ps_car_06_cat+ps_car_07_cat+
             ps_car_08_cat+ps_car_09_cat+ps_car_10_cat+ps_car_11_cat+
             s(ps_car_11,2)+s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+
             s(ps_car_15,4)+s(ps_calc_01,2)+s(ps_calc_02,3)+s(ps_calc_03,4)+
             s(ps_calc_04,6)+s(ps_calc_05,5)+s(ps_calc_06,2)+s(ps_calc_07,2)+
             s(ps_calc_08,2)+s(ps_calc_09,2)+s(ps_calc_10,2)+s(ps_calc_11,3)+
             s(ps_calc_12,2)+s(ps_calc_13,2)+s(ps_calc_14,2)+ps_calc_15_bin+
             ps_calc_16_bin+ps_calc_17_bin+ps_calc_18_bin+ps_calc_19_bin+
             ps_calc_20_bin,data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini3 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini3  # 0.2881266
summary(gam.training)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03353  0.02275  0.03397  0.03629  0.04744  0.26191 
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold3)
gam.gini3 <- normalized.gini.index(fold3$target, gam.predictions)
gam.gini3  # 0.2568223
summary(gam.predictions)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.04220  0.02281  0.03398  0.03633  0.04748  0.20221 
rm(gam.m1)  # Free up memory
# Testing set = 4th fold
trainset <- rbind(fold1,fold2,fold3,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+ps_ind_02_cat+s(ps_ind_03,12)+ps_ind_04_cat+
             ps_ind_05_cat+ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+
             ps_ind_10_bin+ps_ind_11_bin+ps_ind_12_bin+ps_ind_13_bin+ps_ind_14+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+ps_ind_18_bin+
             s(ps_reg_01,10)+s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+
             ps_car_02_cat+ps_car_04_cat+ps_car_06_cat+ps_car_07_cat+
             ps_car_08_cat+ps_car_09_cat+ps_car_10_cat+ps_car_11_cat+
             s(ps_car_11,2)+s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+
             s(ps_car_15,4)+s(ps_calc_01,2)+s(ps_calc_02,3)+s(ps_calc_03,4)+
             s(ps_calc_04,6)+s(ps_calc_05,5)+s(ps_calc_06,2)+s(ps_calc_07,2)+
             s(ps_calc_08,2)+s(ps_calc_09,2)+s(ps_calc_10,2)+s(ps_calc_11,3)+
             s(ps_calc_12,2)+s(ps_calc_13,2)+s(ps_calc_14,2)+ps_calc_15_bin+
             ps_calc_16_bin+ps_calc_17_bin+ps_calc_18_bin+ps_calc_19_bin+
             ps_calc_20_bin,data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini4 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini4  # 0.2870786
summary(gam.training)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02000  0.02291  0.03429  0.03662  0.04793  0.28095 
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold4)
gam.gini4 <- normalized.gini.index(fold4$target, gam.predictions)
gam.gini4  # 0.261353
summary(gam.predictions)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02014  0.02291  0.03425  0.03660  0.04786  0.23678 
rm(gam.m1)  # Free up memory
# Testing set = 5th fold
trainset <- rbind(fold1,fold2,fold3,fold4)
gam.m1=gam(target~s(ps_ind_01,2)+ps_ind_02_cat+s(ps_ind_03,12)+ps_ind_04_cat+
             ps_ind_05_cat+ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+
             ps_ind_10_bin+ps_ind_11_bin+ps_ind_12_bin+ps_ind_13_bin+ps_ind_14+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+ps_ind_18_bin+
             s(ps_reg_01,10)+s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+
             ps_car_02_cat+ps_car_04_cat+ps_car_06_cat+ps_car_07_cat+
             ps_car_08_cat+ps_car_09_cat+ps_car_10_cat+ps_car_11_cat+
             s(ps_car_11,2)+s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+
             s(ps_car_15,4)+s(ps_calc_01,2)+s(ps_calc_02,3)+s(ps_calc_03,4)+
             s(ps_calc_04,6)+s(ps_calc_05,5)+s(ps_calc_06,2)+s(ps_calc_07,2)+
             s(ps_calc_08,2)+s(ps_calc_09,2)+s(ps_calc_10,2)+s(ps_calc_11,3)+
             s(ps_calc_12,2)+s(ps_calc_13,2)+s(ps_calc_14,2)+ps_calc_15_bin+
             ps_calc_16_bin+ps_calc_17_bin+ps_calc_18_bin+ps_calc_19_bin+
             ps_calc_20_bin,data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini5 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini5  # 0.2822196
summary(gam.training)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02883  0.02318  0.03424  0.03646  0.04736  0.29495
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold5)
gam.gini5 <- normalized.gini.index(fold5$target, gam.predictions)
gam.gini5  # 0.2785013
summary(gam.predictions)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02329  0.02321  0.03417  0.03645  0.04736  0.23299 

# Average Ginis from the 5 folds
gam.traininggini <- (gam.traininggini+gam.traininggini2+gam.traininggini3+gam.traininggini4+gam.traininggini5) / 5
gam.testgini <- (gam.gini+gam.gini2+gam.gini3+gam.gini4+gam.gini5) / 5
gam.traininggin   # 0.2851756
gam.testgini  # 0.2676587

rm(gam.m1)  # Free up memory

# Repeat the entire process above for a GAM where all insignificant variables (no stars in the Anova for
# Parametric Effects table in the model summary) are removed
set.seed(20)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             ps_ind_14+s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=train_clean)   # GAM with smoothing splines
summary(gam.m1)
rm(gam.m1)  # Free up memory

# Testing set = 1st fold
trainset <- rbind(fold2,fold3,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             ps_ind_14+s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini  # 0.2817575
summary(gam.training)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03382  0.02297  0.03398  0.03645  0.04741  0.26437 
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold1)
gam.gini <- normalized.gini.index(fold1$target, gam.predictions)
gam.gini  # 0.2758478
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02148  0.02293  0.03403  0.03642  0.04738  0.20650 
rm(gam.m1)  # Free up memory
# Testing set = 2nd fold
trainset <- rbind(fold1,fold3,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             ps_ind_14+s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini2 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini2  # 0.2839286
summary(gam.training)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02120  0.02299  0.03411  0.03642  0.04743  0.19211 
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold2)
gam.gini2 <- normalized.gini.index(fold2$target, gam.predictions)
gam.gini2  # 0.2705189
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.01689  0.02296  0.03414  0.03642  0.04732  0.19081 
rm(gam.m1)  # Free up memory
# Testing set = 3rd fold
trainset <- rbind(fold1,fold2,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             ps_ind_14+s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini3 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini3  # 0.2867864
summary(gam.training)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03232  0.02279  0.03395  0.03629  0.04736  0.26083 
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold3)
gam.gini3 <- normalized.gini.index(fold3$target, gam.predictions)
gam.gini3  # 0.2580018
summary(gam.predictions)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03772  0.02285  0.03399  0.03633  0.04743  0.20020 
rm(gam.m1)  # Free up memory
# Testing set = 4th fold
trainset <- rbind(fold1,fold2,fold3,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             ps_ind_14+s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini4 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini4  # 0.2859786
summary(gam.training)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02084  0.02294  0.03428  0.03662  0.04787  0.28121
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold4)
gam.gini4 <- normalized.gini.index(fold4$target, gam.predictions)
gam.gini4  # 0.2624255
summary(gam.predictions)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02334  0.02293  0.03424  0.03660  0.04781  0.23638
rm(gam.m1)  # Free up memory
# Testing set = 5th fold
trainset <- rbind(fold1,fold2,fold3,fold4)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             ps_ind_14+s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini5 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini5  # 0.2809997
summary(gam.training)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02985  0.02322  0.03421  0.03646  0.04730  0.29390
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold5)
gam.gini5 <- normalized.gini.index(fold5$target, gam.predictions)
gam.gini5  # 0.280513
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02293  0.02324  0.03414  0.03645  0.04727  0.23312

# Average Ginis from the 5 folds
gam.traininggini <- (gam.traininggini+gam.traininggini2+gam.traininggini3+gam.traininggini4+gam.traininggini5) / 5
gam.testgini <- (gam.gini+gam.gini2+gam.gini3+gam.gini4+gam.gini5) / 5
gam.traininggini   # 0.2838902
gam.testgini  # 0.2694614

rm(gam.m1)  # Free up memory



# Set aside a 20% validation set from the training data to use with boosting trees
set.seed(5)
valid <- sample(1:nrow(train_clean), size = (nrow(train_clean) / 5), replace = FALSE)
valid

trainingset <- train_clean[-valid,]
validationset <- train_clean[valid,]

# # Cross-validated classification tree
# classtree <- tree(target~.-id-ps_car_11_cat,trainingset)
# cv_classtree <- cv.tree(classtree,FUN=prune.misclass)
# 
# # The variable ps_car_11_cat was removed because it is a categorical variable with over 100 different categories,
# # and the random forest commands give errors that they cannot handle categorical predictors with more than 53 categories.
# 
# # Bagging and Random Forests
# set.seed(55)
# bag.tree <- randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=57,importance=TRUE)  # Bagging
# rf1=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=1,importance=TRUE)   # Random forest, m = 1 predictor
# rf2=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=2,importance=TRUE)   # Random forest, m = 2 predictors
# rf3=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=3,importance=TRUE)   # Random forest, m = 3 predictors
# rf4=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=4,importance=TRUE)   # Random forest, m = 4 predictors
# rf5=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=5,importance=TRUE)   # Random forest, m = 5 predictors
# rf6=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=6,importance=TRUE)   # Random forest, m = 6 predictors
# rf7=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=7,importance=TRUE)   # Random forest, m = 7 predictors
# rf8=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=8,importance=TRUE)   # Random forest, m = 8 predictors
# rf9=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=9,importance=TRUE)   # Random forest, m = 9 predictors
# rf10=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=10,importance=TRUE)   # Random forest, m = 10 predictors
# rf11=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=11,importance=TRUE)   # Random forest, m = 11 predictors
# rf12=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=12,importance=TRUE)   # Random forest, m = 12 predictors
# rf13=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=13,importance=TRUE)   # Random forest, m = 13 predictors
# rf14=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=14,importance=TRUE)   # Random forest, m = 14 predictors
# rf15=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=15,importance=TRUE)   # Random forest, m = 15 predictors
# rf16=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=16,importance=TRUE)   # Random forest, m = 16 predictors
# rf17=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=17,importance=TRUE)   # Random forest, m = 17 predictors
# rf18=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=18,importance=TRUE)   # Random forest, m = 18 predictors
# rf19=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=19,importance=TRUE)   # Random forest, m = 19 predictors
# rf20=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=20,importance=TRUE)   # Random forest, m = 20 predictors
# rf21=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=21,importance=TRUE)   # Random forest, m = 21 predictors
# rf22=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=22,importance=TRUE)   # Random forest, m = 22 predictors
# rf23=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=23,importance=TRUE)   # Random forest, m = 23 predictors
# rf24=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=24,importance=TRUE)   # Random forest, m = 24 predictors
# rf25=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=25,importance=TRUE)   # Random forest, m = 25 predictors
# rf26=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=26,importance=TRUE)   # Random forest, m = 26 predictors
# rf27=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=27,importance=TRUE)   # Random forest, m = 27 predictors
# rf28=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=28,importance=TRUE)   # Random forest, m = 28 predictors
# rf29=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=29,importance=TRUE)   # Random forest, m = 29 predictors
# rf30=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=30,importance=TRUE)   # Random forest, m = 30 predictors
# rf31=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=31,importance=TRUE)   # Random forest, m = 31 predictors
# rf32=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=32,importance=TRUE)   # Random forest, m = 32 predictors
# rf33=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=33,importance=TRUE)   # Random forest, m = 33 predictors
# rf34=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=34,importance=TRUE)   # Random forest, m = 34 predictors
# rf35=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=35,importance=TRUE)   # Random forest, m = 35 predictors
# rf36=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=36,importance=TRUE)   # Random forest, m = 36 predictors
# rf37=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=37,importance=TRUE)   # Random forest, m = 37 predictors
# rf38=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=38,importance=TRUE)   # Random forest, m = 38 predictors
# rf39=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=39,importance=TRUE)   # Random forest, m = 39 predictors
# rf40=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=40,importance=TRUE)   # Random forest, m = 40 predictors
# rf41=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=41,importance=TRUE)   # Random forest, m = 41 predictors
# rf42=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=42,importance=TRUE)   # Random forest, m = 42 predictors
# rf43=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=43,importance=TRUE)   # Random forest, m = 43 predictors
# rf44=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=44,importance=TRUE)   # Random forest, m = 44 predictors
# rf45=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=45,importance=TRUE)   # Random forest, m = 45 predictors
# rf46=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=46,importance=TRUE)   # Random forest, m = 46 predictors
# rf47=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=47,importance=TRUE)   # Random forest, m = 47 predictors
# rf48=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=48,importance=TRUE)   # Random forest, m = 48 predictors
# rf49=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=49,importance=TRUE)   # Random forest, m = 49 predictors
# rf50=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=50,importance=TRUE)   # Random forest, m = 50 predictors
# rf51=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=51,importance=TRUE)   # Random forest, m = 51 predictors
# rf52=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=52,importance=TRUE)   # Random forest, m = 52 predictors
# rf53=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=53,importance=TRUE)   # Random forest, m = 53 predictors
# rf54=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=54,importance=TRUE)   # Random forest, m = 54 predictors
# rf55=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=55,importance=TRUE)   # Random forest, m = 55 predictors
# rf56=randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=56,importance=TRUE)   # Random forest, m = 56 predictors

# Boosting
set.seed(55)
boost.tree=gbm(target~.-id-ps_car_11_cat,data=trainingset,distribution="bernoulli",n.trees=5000)  # Default shrinkage/lambda parameter of 0.001
summary(boost.tree)
#                           var      rel.inf
# ps_car_13           ps_car_13 29.963429422
# ps_ind_05_cat   ps_ind_05_cat 16.029823136
# ps_ind_17_bin   ps_ind_17_bin 14.786642057
# ps_reg_03           ps_reg_03  8.545529894
# ps_ind_03           ps_ind_03  4.980239672
# ps_car_01_cat   ps_car_01_cat  4.620651767
# ps_car_03_cat   ps_car_03_cat  4.093595412
# ps_ind_06_bin   ps_ind_06_bin  2.938558500
# ps_ind_07_bin   ps_ind_07_bin  2.800738620
# ps_car_06_cat   ps_car_06_cat  2.449859677
# ps_car_09_cat   ps_car_09_cat  2.097789745
# ps_car_04_cat   ps_car_04_cat  1.789118887
# ps_car_07_cat   ps_car_07_cat  1.442725970
# ps_ind_15           ps_ind_15  1.361205700
# ps_ind_16_bin   ps_ind_16_bin  1.319131343
# ps_reg_02           ps_reg_02  0.765218263
# ps_reg_01           ps_reg_01  0.008954954
# ps_ind_01           ps_ind_01  0.006786982
# ps_ind_02_cat   ps_ind_02_cat  0.000000000
# ps_ind_04_cat   ps_ind_04_cat  0.000000000
# ps_ind_08_bin   ps_ind_08_bin  0.000000000
# ps_ind_09_bin   ps_ind_09_bin  0.000000000
# ps_ind_10_bin   ps_ind_10_bin  0.000000000
# ps_ind_11_bin   ps_ind_11_bin  0.000000000
# ps_ind_12_bin   ps_ind_12_bin  0.000000000
# ps_ind_13_bin   ps_ind_13_bin  0.000000000
# ps_ind_14           ps_ind_14  0.000000000
# ps_ind_18_bin   ps_ind_18_bin  0.000000000
# ps_car_02_cat   ps_car_02_cat  0.000000000
# ps_car_05_cat   ps_car_05_cat  0.000000000
# ps_car_08_cat   ps_car_08_cat  0.000000000
# ps_car_10_cat   ps_car_10_cat  0.000000000
# ps_car_11           ps_car_11  0.000000000
# ps_car_12           ps_car_12  0.000000000
# ps_car_14           ps_car_14  0.000000000
# ps_car_15           ps_car_15  0.000000000
# ps_calc_01         ps_calc_01  0.000000000
# ps_calc_02         ps_calc_02  0.000000000
# ps_calc_03         ps_calc_03  0.000000000
# ps_calc_04         ps_calc_04  0.000000000
# ps_calc_05         ps_calc_05  0.000000000
# ps_calc_06         ps_calc_06  0.000000000
# ps_calc_07         ps_calc_07  0.000000000
# ps_calc_08         ps_calc_08  0.000000000
# ps_calc_09         ps_calc_09  0.000000000
# ps_calc_10         ps_calc_10  0.000000000
# ps_calc_11         ps_calc_11  0.000000000
# ps_calc_12         ps_calc_12  0.000000000
# ps_calc_13         ps_calc_13  0.000000000
# ps_calc_14         ps_calc_14  0.000000000
# ps_calc_15_bin ps_calc_15_bin  0.000000000
# ps_calc_16_bin ps_calc_16_bin  0.000000000
# ps_calc_17_bin ps_calc_17_bin  0.000000000
# ps_calc_18_bin ps_calc_18_bin  0.000000000
# ps_calc_19_bin ps_calc_19_bin  0.000000000
# ps_calc_20_bin ps_calc_20_bin  0.000000000

# Re-predict on training set
boost.training <- predict(boost.tree, newdata = trainingset, n.trees = 5000)
boost.traininggini <- normalized.gini.index(trainingset$target, boost.training)
boost.traininggini  # 0.2448071
boost.training <- as.data.frame(boost.training)
summary(boost.training)
# boost.training  
# Min.   :-3.670  
# 1st Qu.:-3.497  
# Median :-3.351  
# Mean   :-3.304  
# 3rd Qu.:-3.148  
# Max.   :-2.021  


# Use boosted tree to make predictions on validation set
boost.predictions <- predict(boost.tree, newdata = validationset, n.trees = 5000)
boost.gini <- normalized.gini.index(validationset$target, boost.predictions)
boost.gini  # 0.2480922
boost.predictions <- as.data.frame(boost.predictions)
summary(boost.predictions)
# boost.predictions
# Min.   :-3.670   
# 1st Qu.:-3.497   
# Median :-3.351   
# Mean   :-3.304   
# 3rd Qu.:-3.146   
# Max.   :-2.005   

# Repeat above procedure with boosting, this time removing all 
# predictors with a rel.inf value of 0.000000000 (unimportant variables)
set.seed(65)
boost.tree=gbm(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
                 ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
                 ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
                 ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,distribution="bernoulli",n.trees=5000)  # Default shrinkage/lambda parameter of 0.001
summary(boost.tree)
#                         var     rel.inf
# ps_car_13         ps_car_13 29.81943892
# ps_ind_05_cat ps_ind_05_cat 15.86696613
# ps_ind_17_bin ps_ind_17_bin 14.86374867
# ps_reg_03         ps_reg_03  8.67479996
# ps_ind_03         ps_ind_03  4.96614317
# ps_car_01_cat ps_car_01_cat  4.65390524
# ps_car_03_cat ps_car_03_cat  4.15121322
# ps_ind_06_bin ps_ind_06_bin  2.86249695
# ps_ind_07_bin ps_ind_07_bin  2.85644818
# ps_car_06_cat ps_car_06_cat  2.50072325
# ps_car_09_cat ps_car_09_cat  2.01197215
# ps_car_04_cat ps_car_04_cat  1.90080283
# ps_car_07_cat ps_car_07_cat  1.44531095
# ps_ind_15         ps_ind_15  1.38308816
# ps_ind_16_bin ps_ind_16_bin  1.34148918
# ps_reg_02         ps_reg_02  0.66480683
# ps_reg_01         ps_reg_01  0.03664622
# ps_ind_01         ps_ind_01  0.00000000

# Re-predict on training set
boost.training <- predict(boost.tree, newdata = trainingset, n.trees = 5000)
boost.traininggini <- normalized.gini.index(trainingset$target, boost.training)
boost.traininggini  # 0.2449439
boost.training <- as.data.frame(boost.training)
summary(boost.training)
# boost.training  
# Min.   :-3.672  
# 1st Qu.:-3.497  
# Median :-3.349  
# Mean   :-3.304  
# 3rd Qu.:-3.148  
# Max.   :-2.027  
# Use boosted tree to make predictions on validation set
boost.predictions <- predict(boost.tree, newdata = validationset, n.trees = 5000)
boost.gini <- normalized.gini.index(validationset$target, boost.predictions)
boost.gini  # 0.2483251
boost.predictions <- as.data.frame(boost.predictions)
summary(boost.predictions)
# boost.predictions
# Min.   :-3.672   
# 1st Qu.:-3.497   
# Median :-3.350   
# Mean   :-3.304   
# 3rd Qu.:-3.146   
# Max.   :-2.006   

# # Bagging with the same set of 18 predictors
# set.seed(75)
# bag.tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                            ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                            ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                            ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=18,importance=TRUE,ntree=100)  # Bagging
# summary(bag.tree)
# # Re-predict on training set
# bag.training <- predict(bag.tree, newdata = trainingset)
# bag.traininggini <- normalized.gini.index(trainingset$target, bag.training)
# bag.traininggini  # 0.2449439
# bag.training <- as.data.frame(bag.training)
# summary(bag.training)
# # Use bagged tree to make predictions on validation set
# bag.predictions <- predict(bag.tree, newdata = validationset)
# bag.gini <- normalized.gini.index(validationset$target, bag.predictions)
# bag.gini  # 0.2483251
# bag.predictions <- as.data.frame(bag.predictions)
# summary(bag.predictions)
# 
# rm(bag.predictions, bag.training, bag.tree)
# gc()
# 
# # Random Forests
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                            ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                            ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                            ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=17,importance=TRUE,ntree=100)  # Random forest m = 17
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=16,importance=TRUE,ntree=100)  # Random forest m = 16
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=15,importance=TRUE,ntree=100)  # Random forest m = 15
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=14,importance=TRUE,ntree=100)  # Random forest m = 14
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=13,importance=TRUE,ntree=100)  # Random forest m = 13
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=12,importance=TRUE,ntree=100)  # Random forest m = 12
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=11,importance=TRUE,ntree=100)  # Random forest m = 11
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=10,importance=TRUE,ntree=100)  # Random forest m = 10
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=9,importance=TRUE,ntree=100)  # Random forest m = 9
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=8,importance=TRUE,ntree=100)  # Random forest m = 8
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=7,importance=TRUE,ntree=100)  # Random forest m = 7
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=6,importance=TRUE,ntree=100)  # Random forest m = 6
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=5,importance=TRUE,ntree=100)  # Random forest m = 5
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=4,importance=TRUE,ntree=100)  # Random forest m = 4
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=3,importance=TRUE,ntree=100)  # Random forest m = 3
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=2,importance=TRUE,ntree=100)  # Random forest m = 2
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_car_03_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=1,importance=TRUE,ntree=100)  # Random forest m = 1
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 0.2449439
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 0.2483251
# predictions <- as.data.frame(predictions)
# summary(predictions)

















# Make predictions of test set from boosted tree
boost.testpredictions <- as.data.frame(predict(boost.tree, newdata=test_clean, n.trees = 5000))
names(boost.testpredictions) <- "target"
summary(boost.testpredictions)
# target      
# Min.   :-3.670  
# 1st Qu.:-3.496  
# Median :-3.351  
# Mean   :-3.304  
# 3rd Qu.:-3.147  
# Max.   :-2.063  

boost.submission <- cbind(test_clean$id, boost.testpredictions)
names(boost.submission) <- c("id","target")

# Re-scale to a [0,1] scale
boost.submission$target <- ((boost.submission$target - min(boost.submission$target)) / (max(boost.submission$target) - min(boost.submission$target)))

# Write out predictions to a CSV file
write.csv(boost.submission, "Boosted_Tree_Predictions.csv", row.names=FALSE)
