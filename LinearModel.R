# SYS 6018: Competition 4
# Xinyang Liu
# xl9qw

library(boot)
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

# Create a logistic regression model using all variables (full model)
lm <- lm(target ~.-id, data = train_clean)
summary(lm) # Adjusted R-squared:  0.00847
anova(lm) # MSE: 0.0348

# K-fold cross validation
library(DAAG)
cvResults <- cv.lm(train_clean, lm, m = 5)
attr(cvResults, 'ms') # 0.0348

# Gini
probs <- as.vector(predict(lm, type = "response"))
normalized.gini.index(train_clean$target, abs(probs)) # 0.264

# Create another model using significant variables shown in the summary
lm1 <- lm(target ~ ps_ind_01 + ps_ind_02_cat + ps_ind_03 + ps_ind_04_cat + ps_ind_05_cat +
            ps_ind_07_bin + ps_ind_08_bin + ps_ind_15 + ps_ind_16_bin + ps_ind_17_bin + ps_ind_18_bin +
            ps_reg_01 + ps_reg_02 + ps_reg_03 + ps_car_01_cat + ps_car_02_cat + ps_car_04_cat +
            ps_car_07_cat + ps_car_09_cat + ps_car_11_cat + ps_car_12 + ps_car_13 + ps_car_14, data = train_clean)
summary(lm1) # Adjusted R-squared:  0.00846

# K-fold cross validation
cvResults1 <- cv.lm(train_clean, lm1, m = 5)
attr(cvResults1, 'ms') # 0.0348

# Gini
probs1 <- as.vector(predict(lm1, type = "response"))
normalized.gini.index(train_clean$target, abs(probs1)) # 0.263

# Create a third model using significant variables shown in the anova
lm2 <- lm(target ~ ps_ind_01 + ps_ind_02_cat + ps_ind_03 + ps_ind_04_cat + ps_ind_05_cat +
            ps_ind_06_bin + ps_ind_07_bin + ps_ind_08_bin + ps_ind_12_bin + ps_ind_15 +
            ps_ind_16_bin + ps_ind_17_bin + ps_ind_18_bin + ps_reg_01 + ps_reg_02 + ps_reg_03 +
            ps_car_01_cat + ps_car_02_cat + ps_car_04_cat + ps_car_06_cat + ps_car_07_cat +
            ps_car_08_cat + ps_car_09_cat + ps_car_11_cat + ps_car_12 + ps_car_13 + ps_car_14, data = train_clean)
summary(lm2) # Adjusted R-squared:  0.00849

# K-fold cross validation
cvResults2 <- cv.lm(train_clean, lm2, m = 5)
attr(cvResults2, 'ms') # 0.0348

# Gini
probs2 <- as.vector(predict(lm2, type = "response"))
normalized.gini.index(train_clean$target, abs(probs2)) # 0.263

# Use stepwise function to find the best model that minimizes AIC
library(MASS)
fit <- lm(target ~.-id, data = train_clean)
step <- stepAIC(fit, direction="both")
step$anova

# Final model from stepwise function
lm3 <- lm(target ~ ps_ind_01 + ps_ind_02_cat + ps_ind_03 + ps_ind_04_cat + 
             ps_ind_05_cat + ps_ind_07_bin + ps_ind_08_bin + ps_ind_15 + 
             ps_ind_16_bin + ps_ind_17_bin + ps_ind_18_bin + ps_reg_01 + 
             ps_reg_02 + ps_reg_03 + ps_car_01_cat + ps_car_02_cat + ps_car_04_cat +
             ps_car_06_cat + ps_car_07_cat + ps_car_09_cat + ps_car_11_cat +
             ps_car_12 + ps_car_13 + ps_car_14 + ps_calc_03, data = train_clean)
summary(lm3) # Adjusted R-squared:  0.0085

# K-fold cross validation
cvResults3 <- cv.lm(train_clean, lm3, m = 5)
attr(cvResults3, 'ms') # 0.0348

# Gini
probs3 <- as.vector(predict(lm3, type = "response"))
normalized.gini.index(train_clean$target, abs(probs3)) # 0.263

# The last model has the highest R^2, small MSE, and highest gini except for the full model
# Make predictions on the test data using lm3
probs <- as.vector(predict(lm3, newdata = test_clean, type = "response"))
table <- data.frame(test_clean$id, abs(probs)) # a dataframe with id and predictions
colSums(is.na(table))  # no weird predictions
write.table(table, file = "Predictions.csv", row.names = F, col.names = c('id', 'target'), sep = ",")
