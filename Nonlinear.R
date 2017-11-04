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

#ss4=smooth.spline(train_clean$ps_ind_04_cat,train_clean$target,cv=TRUE)
#ss4$df  # Error: need at least four unique 'x' values

#ss5=smooth.spline(train_clean$ps_ind_05_cat,train_clean$target,cv=TRUE)
#ss5$df  # Error: 'tol' must be strictly positive and finite

#ss6=smooth.spline(train_clean$ps_ind_06_bin,train_clean$target,cv=TRUE)
#ss6$df  # Error: need at least four unique 'x' values

#ss7=smooth.spline(train_clean$ps_ind_07_bin,train_clean$target,cv=TRUE)
#ss7$df  # Error: need at least four unique 'x' values

#ss8=smooth.spline(train_clean$ps_ind_08_bin,train_clean$target,cv=TRUE)
#ss8$df  # Error: 'tol' must be strictly positive and finite

#ss9=smooth.spline(train_clean$ps_ind_09_bin,train_clean$target,cv=TRUE)
#ss9$df  # Error: 'tol' must be strictly positive and finite

#ss10=smooth.spline(train_clean$ps_ind_10_bin,train_clean$target,cv=TRUE)
#ss10$df  # Error: 'tol' must be strictly positive and finite

#ss11=smooth.spline(train_clean$ps_ind_11_bin,train_clean$target,cv=TRUE)
#ss11$df  # Error: 'tol' must be strictly positive and finite

#ss12=smooth.spline(train_clean$ps_ind_12_bin,train_clean$target,cv=TRUE)
#ss12$df  # Error: 'tol' must be strictly positive and finite

#ss13=smooth.spline(train_clean$ps_ind_13_bin,train_clean$target,cv=TRUE)
#ss13$df  # Error: 'tol' must be strictly positive and finite

#ss14=smooth.spline(train_clean$ps_ind_14,train_clean$target,cv=TRUE)
#ss14$df  # Error: 'tol' must be strictly positive and finite

ss15=smooth.spline(train_clean$ps_ind_15,train_clean$target,cv=TRUE)
ss15$df   # 14.00001

#ss16=smooth.spline(train_clean$ps_ind_16_bin,train_clean$target,cv=TRUE)
#ss16$df  # Error: need at least four unique 'x' values

#ss17=smooth.spline(train_clean$ps_ind_17_bin,train_clean$target,cv=TRUE)
#ss17$df  # Error: 'tol' must be strictly positive and finite

#ss18=smooth.spline(train_clean$ps_ind_18_bin,train_clean$target,cv=TRUE)
#ss18$df  # Error: 'tol' must be strictly positive and finite

ss19=smooth.spline(train_clean$ps_reg_01,train_clean$target,cv=TRUE)
ss19$df   # 9.999988

ss20=smooth.spline(train_clean$ps_reg_02,train_clean$target,cv=TRUE)
ss20$df   # 2.000005

ss21=smooth.spline(train_clean$ps_reg_03,train_clean$target,cv=TRUE)
ss21$df   # 7.441016

ss22=smooth.spline(train_clean$ps_car_01_cat,train_clean$target,cv=TRUE)
ss22$df   # 4.37158

#ss23=smooth.spline(train_clean$ps_car_02_cat,train_clean$target,cv=TRUE)
#ss23$df  # Error: 'tol' must be strictly positive and finite

#ss24=smooth.spline(train_clean$ps_car_04_cat,train_clean$target,cv=TRUE)
#ss24$df  # Error: 'tol' must be strictly positive and finite

ss25=smooth.spline(train_clean$ps_car_06_cat,train_clean$target,cv=TRUE)
ss25$df   # 2.000006

#ss26=smooth.spline(train_clean$ps_car_07_cat,train_clean$target,cv=TRUE)
#ss26$df  # Error: 'tol' must be strictly positive and finite

#ss27=smooth.spline(train_clean$ps_car_08_cat,train_clean$target,cv=TRUE)
#ss27$df  # Error: 'tol' must be strictly positive and finite

ss28=smooth.spline(train_clean$ps_car_09_cat,train_clean$target,cv=TRUE)
ss28$df   # 2

#ss29=smooth.spline(train_clean$ps_car_10_cat,train_clean$target,cv=TRUE)
#ss29$df  # Error: 'tol' must be strictly positive and finite

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

#ss50=smooth.spline(train_clean$ps_calc_15_bin,train_clean$target,cv=TRUE)
#ss50$df   # Error: 'tol' must be strictly positive and finite

#ss51=smooth.spline(train_clean$ps_calc_16_bin,train_clean$target,cv=TRUE)
#ss51$df   # Error: need at least four unique 'x' values

#ss52=smooth.spline(train_clean$ps_calc_17_bin,train_clean$target,cv=TRUE)
#ss52$df   # Error: need at least four unique 'x' values

#ss53=smooth.spline(train_clean$ps_calc_18_bin,train_clean$target,cv=TRUE)
#ss53$df   # Error: need at least four unique 'x' values

#ss54=smooth.spline(train_clean$ps_calc_19_bin,train_clean$target,cv=TRUE)
#ss54$df   # Error: need at least four unique 'x' values

#ss55=smooth.spline(train_clean$ps_calc_20_bin,train_clean$target,cv=TRUE)
#ss55$df   # Error: 'tol' must be strictly positive and finite

# The 55 smoothing spline commands corresponding to the 55 potential predictors give the 
# cross-validated optimal degrees of freedom for the individual smoothing splines for each 
# variable. These optimal degrees of freedom will be used for the quantitative variables 
# in the following GAMs because the GAMs will include smoothing spline components for 
# these variables. However, many of the predictors returned errors when attempting to
# create smoothing splines with them, either because there were not enough unique values
# (smoothing splines require at least four unique 'x' values) or because the values
# were not strictly positive and finite. For these problematic variables, they will still
# be included in the following GAMs, but just in their raw forms. Most of the variables
# that returned these errors are binary or categorical variables.

# GAM Model #1
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
gam.traininggini   # 0.2851756
gam.testgini  # 0.2676587

rm(gam.m1)  # Free up memory

# This first attempt at a 5-fold cross-validated GAM using all the variables seems promising, looking at
# the resulting average Gini coefficients. The average testing Gini coefficient for this GAM was 0.2676587
# while the average training Gini coefficient was 0.2851756. This suggests that overfitting may be
# occurring, which is not surprising because this GAM is an extremely flexible model. So in the next
# GAM, we try to address this by removing insignificant variables from the model.



# GAM Model #2
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

# When cross-validating this GAM, every single training Gini coefficient was lower than the corresponding
# coefficient in the previous GAM, but every single testing Gini coefficient was higher than the
# corresponding coefficient in the previous GAM. This shows that removing the insignificant variables
# helped to reduce the overfitting issue. The average testing Gini coefficient increased from the
# previous GAM to 0.2694614, while the average training Gini coefficient decreased to 0.2838902.
# The training coefficient is still quite a bit higher than the testing coefficient, so in the following
# GAM we try to prune the model even further.



# GAM Model #3
# Repeat the entire process above for a GAM using the same variables as above, except for one
# variable that is now insignificant in the Anova for parametric effects (i.e. removing ps_ind_14)
set.seed(20)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=train_clean)   # GAM with smoothing splines
summary(gam.m1)
rm(gam.m1)  # Free up memory

# Testing set = 1st fold
trainset <- rbind(fold2,fold3,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini  # 0.281753
summary(gam.training)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03377  0.02297  0.03398  0.03645  0.04741  0.26431  
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold1)
gam.gini <- normalized.gini.index(fold1$target, gam.predictions)
gam.gini  # 0.2758913
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02148  0.02293  0.03403  0.03642  0.04738  0.20557 
rm(gam.m1)  # Free up memory
# Testing set = 2nd fold
trainset <- rbind(fold1,fold3,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini2 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini2  # 0.283897
summary(gam.training)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02117  0.02299  0.03411  0.03642  0.04743  0.19207
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold2)
gam.gini2 <- normalized.gini.index(fold2$target, gam.predictions)
gam.gini2  # 0.2705858
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.01688  0.02296  0.03414  0.03642  0.04732  0.19071
rm(gam.m1)  # Free up memory
# Testing set = 3rd fold
trainset <- rbind(fold1,fold2,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini3 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini3  # 0.2867908
summary(gam.training)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03230  0.02279  0.03396  0.03629  0.04736  0.26089 
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold3)
gam.gini3 <- normalized.gini.index(fold3$target, gam.predictions)
gam.gini3  # 0.2580928
summary(gam.predictions)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03788  0.02285  0.03398  0.03634  0.04743  0.20006
rm(gam.m1)  # Free up memory
# Testing set = 4th fold
trainset <- rbind(fold1,fold2,fold3,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini4 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini4  # 0.2859676
summary(gam.training)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02083  0.02294  0.03428  0.03662  0.04787  0.28127
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold4)
gam.gini4 <- normalized.gini.index(fold4$target, gam.predictions)
gam.gini4  # 0.2624213
summary(gam.predictions)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02333  0.02293  0.03425  0.03660  0.04781  0.23641
rm(gam.m1)  # Free up memory
# Testing set = 5th fold
trainset <- rbind(fold1,fold2,fold3,fold4)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini5 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini5  # 0.2809992
summary(gam.training)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02986  0.02323  0.03421  0.03646  0.04730  0.29390 
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold5)
gam.gini5 <- normalized.gini.index(fold5$target, gam.predictions)
gam.gini5  # 0.2805207
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02293  0.02324  0.03414  0.03645  0.04727  0.23312

# Average Ginis from the 5 folds
gam.traininggini <- (gam.traininggini+gam.traininggini2+gam.traininggini3+gam.traininggini4+gam.traininggini5) / 5
gam.testgini <- (gam.gini+gam.gini2+gam.gini3+gam.gini4+gam.gini5) / 5
gam.traininggini   # 0.2838815
gam.testgini  # 0.2695024

rm(gam.m1)  # Free up memory

# This GAM has a very marginal improvement over the previous GAM as a result of removing one
# variable. The average testing Gini coefficient improved from 0.2694614 to 0.2695024, while
# the average training Gini coefficient decreased from 0.2838902 to 0.2838815. Currently this
# is the best performing model, but we want to see if we can adjust the GAM a little bit
# more to squeeze another minor improvement in its performance.



# GAM Model #4
# Repeat the entire process above for a GAM using the same variables as above, except now
# replace the variable s(ps_reg_02,2) with just its basic form ps_reg_02, because in the
# prior GAM Anova for nonparametric effects table the nonparametric effect of s(ps_reg_02,2)
# was statistically insignificant
set.seed(20)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             ps_reg_02+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=train_clean)   # GAM with smoothing splines
summary(gam.m1)
rm(gam.m1)  # Free up memory

# Testing set = 1st fold
trainset <- rbind(fold2,fold3,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             ps_reg_02+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini  # 0.2817254
summary(gam.training)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03380  0.02297  0.03398  0.03645  0.04741  0.26434  
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold1)
gam.gini <- normalized.gini.index(fold1$target, gam.predictions)
gam.gini  # 0.2758605
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02149  0.02293  0.03403  0.03642  0.04737  0.20562  
rm(gam.m1)  # Free up memory
# Testing set = 2nd fold
trainset <- rbind(fold1,fold3,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             ps_reg_02+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini2 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini2  # 0.2838711
summary(gam.training)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02118  0.02298  0.03411  0.03642  0.04743  0.19214
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold2)
gam.gini2 <- normalized.gini.index(fold2$target, gam.predictions)
gam.gini2  # 0.2705513
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.01684  0.02296  0.03414  0.03642  0.04732  0.19072 
rm(gam.m1)  # Free up memory
# Testing set = 3rd fold
trainset <- rbind(fold1,fold2,fold4,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             ps_reg_02+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini3 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini3  # 0.2867537
summary(gam.training)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03231  0.02279  0.03396  0.03629  0.04737  0.26087 
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold3)
gam.gini3 <- normalized.gini.index(fold3$target, gam.predictions)
gam.gini3  # 0.258078
summary(gam.predictions)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.03789  0.02285  0.03399  0.03634  0.04744  0.20013 
rm(gam.m1)  # Free up memory
# Testing set = 4th fold
trainset <- rbind(fold1,fold2,fold3,fold5)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             ps_reg_02+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini4 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini4  # 0.2858938
summary(gam.training)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02083  0.02294  0.03427  0.03662  0.04786  0.28135
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold4)
gam.gini4 <- normalized.gini.index(fold4$target, gam.predictions)
gam.gini4  # 0.2624513
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02341  0.02293  0.03422  0.03660  0.04781  0.23619 
rm(gam.m1)  # Free up memory
# Testing set = 5th fold
trainset <- rbind(fold1,fold2,fold3,fold4)
gam.m1=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             ps_reg_02+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=trainset)   # GAM with smoothing splines
# Re-predict on training set
gam.training <- predict(gam.m1, newdata = trainset)
gam.traininggini5 <- normalized.gini.index(trainset$target, gam.training)
gam.traininggini5  # 0.2809792
summary(gam.training)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02987  0.02322  0.03421  0.03646  0.04730  0.29386
# Use GAM to make predictions on held-out test set
gam.predictions <- predict(gam.m1, newdata = fold5)
gam.gini5 <- normalized.gini.index(fold5$target, gam.predictions)
gam.gini5  # 0.2805102
summary(gam.predictions)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02293  0.02323  0.03414  0.03645  0.04728  0.23314 

# Average Ginis from the 5 folds
gam.traininggini <- (gam.traininggini+gam.traininggini2+gam.traininggini3+gam.traininggini4+gam.traininggini5) / 5
gam.testgini <- (gam.gini+gam.gini2+gam.gini3+gam.gini4+gam.gini5) / 5
gam.traininggini   # 0.2838446
gam.testgini  # 0.2694903

rm(gam.m1)  # Free up memory

# This GAM performed marginally worse than the previous one. The average testing Gini coefficient
# decreased from 0.2695024 to 0.2694903. Also, the average training Gini coefficient decreased
# from 0.2838815 to 0.2838446.

# Based on these results from the various GAM models, we believe that GAM model #3 is the best
# one of its class, and we move on to examining other nonlinear methods.



# Boosted trees (and why no other forests were created) #

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
# bag.tree <- randomForest(target~.-id-ps_car_11_cat,data=trainingset,mtry=54,importance=TRUE)  # Bagging
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

# Boosting
# Boost model #1
set.seed(55)
boost.tree=gbm(target~.-id-ps_car_11_cat,data=trainingset,distribution="bernoulli",n.trees=5000)  # Default shrinkage/lambda parameter of 0.001
summary(boost.tree)
#                           var      rel.inf
# ps_car_13           ps_car_13 31.314381682
# ps_ind_05_cat   ps_ind_05_cat 16.406249561
# ps_ind_17_bin   ps_ind_17_bin 15.456259730
# ps_reg_03           ps_reg_03  8.757439952
# ps_car_01_cat   ps_car_01_cat  5.387697113
# ps_ind_03           ps_ind_03  5.124425028
# ps_ind_06_bin   ps_ind_06_bin  3.068154854
# ps_ind_07_bin   ps_ind_07_bin  2.978783483
# ps_car_06_cat   ps_car_06_cat  2.539833321
# ps_car_09_cat   ps_car_09_cat  2.205315052
# ps_car_04_cat   ps_car_04_cat  1.755671615
# ps_car_07_cat   ps_car_07_cat  1.454435758
# ps_ind_15           ps_ind_15  1.314057990
# ps_ind_16_bin   ps_ind_16_bin  1.307024923
# ps_reg_02           ps_reg_02  0.904717088
# ps_reg_01           ps_reg_01  0.018569661
# ps_ind_01           ps_ind_01  0.006983189
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
boost.traininggini  # 0.2423589
boost.training <- as.data.frame(boost.training)
summary(boost.training)
# boost.training  
# Min.   :-3.674  
# 1st Qu.:-3.499  
# Median :-3.353  
# Mean   :-3.304  
# 3rd Qu.:-3.146  
# Max.   :-1.975  


# Use boosted tree to make predictions on validation set
boost.predictions <- predict(boost.tree, newdata = validationset, n.trees = 5000)
boost.gini <- normalized.gini.index(validationset$target, boost.predictions)
boost.gini  # 0.2470356
boost.predictions <- as.data.frame(boost.predictions)
summary(boost.predictions)
# boost.predictions
# Min.   :-3.674   
# 1st Qu.:-3.499   
# Median :-3.354   
# Mean   :-3.305   
# 3rd Qu.:-3.145   
# Max.   :-1.955

# In constructing the boosted trees, a 20% validation set was used, with the remaining 80% of the data
# as the training set. The reason why a validation set was used instead of a cross-validation procedure
# is because of computational limitations; it takes a very long time to run one boosted model, so running
# a model 5 times is infeasible.

# We attempted to build a cross-validated classification tree, bagged forests, and random forests using
# the commented-out code above. Every time, we got an error saying R could not store a vector with
# a size of several gigabytes. Boosted models were the only forest models that would run.

# The first boosted model used all predictors except for the variable ps_car_11_cat because the forest
# models would not accept categorical predictors with more than 53 categories, and this particular variable
# is a categorical variable with over 100 different categories. This boosted model resulted in a Gini
# coefficient of 0.2423589 for the training set, and a Gini coefficient of 0.2470356 for the held-out
# validation set. These results are worse than the results for all the GAMs. In looking at the summary
# of the boosted model, we noticed that a lot of variables seemed to be unimportant, with rel.inf values
# of 0.000000000. So, we decided to run a trimmed-down version of the boosted model below to see if there
# would be any performance improvements.


# Boost model #2
# Repeat above procedure with boosting, this time removing all 
# predictors with a rel.inf value of 0.000000000 (unimportant variables)
set.seed(65)
boost.tree=gbm(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
                 ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
                 ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
                 ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,distribution="bernoulli",n.trees=5000)  # Default shrinkage/lambda parameter of 0.001
summary(boost.tree)
#                         var     rel.inf
# ps_car_13         ps_car_13 31.18646506
# ps_ind_05_cat ps_ind_05_cat 16.25041889
# ps_ind_17_bin ps_ind_17_bin 15.52057459
# ps_reg_03         ps_reg_03  8.88932514
# ps_car_01_cat ps_car_01_cat  5.44923273
# ps_ind_03         ps_ind_03  5.08783749
# ps_ind_07_bin ps_ind_07_bin  3.06762302
# ps_ind_06_bin ps_ind_06_bin  2.99806068
# ps_car_06_cat ps_car_06_cat  2.59770946
# ps_car_09_cat ps_car_09_cat  2.12382836
# ps_car_04_cat ps_car_04_cat  1.89167579
# ps_car_07_cat ps_car_07_cat  1.42637814
# ps_ind_16_bin ps_ind_16_bin  1.35728098
# ps_ind_15         ps_ind_15  1.30991347
# ps_reg_02         ps_reg_02  0.80702077
# ps_reg_01         ps_reg_01  0.03665543
# ps_ind_01         ps_ind_01  0.00000000

# Re-predict on training set
boost.training <- predict(boost.tree, newdata = trainingset, n.trees = 5000)
boost.traininggini <- normalized.gini.index(trainingset$target, boost.training)
boost.traininggini  # 0.242442
boost.training <- as.data.frame(boost.training)
summary(boost.training)
# boost.training  
# Min.   :-3.676  
# 1st Qu.:-3.499  
# Median :-3.352  
# Mean   :-3.304  
# 3rd Qu.:-3.145  
# Max.   :-1.978
# Use boosted tree to make predictions on validation set
boost.predictions <- predict(boost.tree, newdata = validationset, n.trees = 5000)
boost.gini <- normalized.gini.index(validationset$target, boost.predictions)
boost.gini  # 0.247282
boost.predictions <- as.data.frame(boost.predictions)
summary(boost.predictions)
# boost.predictions
# Min.   :-3.676   
# 1st Qu.:-3.499   
# Median :-3.352   
# Mean   :-3.304   
# 3rd Qu.:-3.144   
# Max.   :-1.954     

# The boosted model with fewer variables performed marginally better than the first boosted model.
# The training set Gini coefficient increased to 0.242442, and the validation set Gini coefficient
# increased to 0.247282. However, both of these are still worse than the results from the GAMs.

# We tried to run similar models using bagging and random forest procedures with the same reduced
# set of 17 predictors and n.trees parameter of 5000, but again received errors that the vectors'
# size was too big and could not be stored. So we reduced the n.trees parameter to 100, and ran the
# code below. No errors showed up, but the process did not finish, even when left to run overnight.



# # Bagging with the same set of 17 predictors
# set.seed(75)
# bag.tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                            ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                            ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                            ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=17,importance=TRUE,ntree=100)  # Bagging
# summary(bag.tree)
# # Re-predict on training set
# bag.training <- predict(bag.tree, newdata = trainingset)
# bag.traininggini <- normalized.gini.index(trainingset$target, bag.training)
# bag.traininggini  # 
# bag.training <- as.data.frame(bag.training)
# summary(bag.training)
# # Use bagged tree to make predictions on validation set
# bag.predictions <- predict(bag.tree, newdata = validationset)
# bag.gini <- normalized.gini.index(validationset$target, bag.predictions)
# bag.gini  # 
# bag.predictions <- as.data.frame(bag.predictions)
# summary(bag.predictions)
# 
# rm(bag.predictions, bag.training, bag.tree)
# gc()
# 
# # Random Forests
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                            ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                            ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                            ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=16,importance=TRUE,ntree=100)  # Random forest m = 17
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=15,importance=TRUE,ntree=100)  # Random forest m = 16
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=14,importance=TRUE,ntree=100)  # Random forest m = 14
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=13,importance=TRUE,ntree=100)  # Random forest m = 13
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=12,importance=TRUE,ntree=100)  # Random forest m = 12
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=11,importance=TRUE,ntree=100)  # Random forest m = 11
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=10,importance=TRUE,ntree=100)  # Random forest m = 10
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=9,importance=TRUE,ntree=100)  # Random forest m = 9
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=8,importance=TRUE,ntree=100)  # Random forest m = 8
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=7,importance=TRUE,ntree=100)  # Random forest m = 7
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=6,importance=TRUE,ntree=100)  # Random forest m = 6
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=5,importance=TRUE,ntree=100)  # Random forest m = 5
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=4,importance=TRUE,ntree=100)  # Random forest m = 4
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=3,importance=TRUE,ntree=100)  # Random forest m = 3
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=2,importance=TRUE,ntree=100)  # Random forest m = 2
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)
# 
# set.seed(75)
# tree <- randomForest(target~ps_car_13+ps_ind_05_cat+ps_ind_17_bin+ps_reg_03+ps_ind_03+
#                        ps_car_01_cat+ps_ind_06_bin+ps_ind_07_bin+ps_car_06_cat+
#                        ps_car_09_cat+ps_car_04_cat+ps_car_07_cat+ps_ind_15+ps_ind_16_bin+
#                        ps_reg_02+ps_reg_01+ps_ind_01,data=trainingset,mtry=1,importance=TRUE,ntree=100)  # Random forest m = 1
# summary(tree)
# # Re-predict on training set
# training <- predict(tree, newdata = trainingset)
# traininggini <- normalized.gini.index(trainingset$target, training)
# traininggini  # 
# training <- as.data.frame(training)
# summary(training)
# # Use random forest tree to make predictions on validation set
# predictions <- predict(tree, newdata = validationset)
# gini <- normalized.gini.index(validationset$target, predictions)
# gini  # 
# predictions <- as.data.frame(predictions)
# summary(predictions)



# In the end, we decided to use GAM model #3 as our primary nonlinear model to make predictions on
# the test dataset with. This model has the highest average test Gini coefficient out of all the
# models examined. Unfortunately, bagging, random forest, and cross-validated trees could not
# be examined due to computational power reasons.



### Submitting predictions on the cleaned test dataset ###

# Re-create GAM Model #3 using the entire training dataset
set.seed(20)
gam.model=gam(target~s(ps_ind_01,2)+s(ps_ind_03,12)+ps_ind_04_cat+ps_ind_05_cat+
             ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_12_bin+
             s(ps_ind_15,14)+ps_ind_16_bin+ps_ind_17_bin+s(ps_reg_01,10)+
             s(ps_reg_02,2)+s(ps_reg_03,7)+ps_car_01_cat+ps_car_02_cat+ps_car_04_cat+
             ps_car_06_cat+ps_car_07_cat+ps_car_08_cat+ps_car_09_cat+ps_car_11_cat+
             s(ps_car_12,23)+s(ps_car_13,9)+s(ps_car_14,19)+s(ps_car_15,4),data=train_clean)   # GAM with smoothing splines

# Make predictions of cleaned test set from GAM Model #3
gam.testpredictions <- as.data.frame(predict(gam.model, newdata=test_clean))
names(gam.testpredictions) <- "target"
summary(gam.testpredictions)
#     target        
# Min.   :-0.02139  
# 1st Qu.: 0.02308  
# Median : 0.03413  
# Mean   : 0.03646  
# 3rd Qu.: 0.04745  
# Max.   : 0.26232 

gam.submission <- cbind(test_clean$id, gam.testpredictions)
names(gam.submission) <- c("id","target")

# Re-scale to a [0,1] scale
gam.submission$target <- ((gam.submission$target - min(gam.submission$target)) / (max(gam.submission$target) - min(gam.submission$target)))

# From the summary of the test predictions, we see that the range of the predictions goes from
# -0.02139 to 0.26232. However, the predictions we submit must be between 0 to 1. Re-scaling our
# predictions using the above command is appropriate in this situation because the evaluation
# metric is the Gini coefficient, which is concerned more about the relative ordering of the
# predictions instead of the actual prediction values, and this re-scaling procedure preserves 
# the ordering of the predictions.

# Write out predictions to a CSV file
write.csv(gam.submission, "GAM_Predictions.csv", row.names=FALSE)

# This submission file received a score of 0.267 on the public leaderboard.
