require(caret)

train_Cat <- copy(train)
train_Cat$Default <- ifelse(train_Cat$Default == 0, "No", "Yes") # Need to make this a factor for xgb
test_Cat <- copy(test)
test_Cat$Default <- ifelse(test_Cat$Default == 0, "No", "Yes") # Need to make this a factor for xgb

train_Cat$trx_type_mode = as.factor(train_Cat$trx_type_mode)
train_Cat$trx_cat_mode = as.factor(train_Cat$trx_cat_mode)
train_Cat$reversedPayment = as.logical(train_Cat$reversedPayment)
train_Cat$noPayments = as.logical(train_Cat$noPayments)
train_Cat$credit_change = as.logical(train_Cat$credit_change)



test_Cat$trx_type_mode = as.factor(test_Cat$trx_type_mode)
test_Cat$trx_cat_mode = as.factor(test_Cat$trx_cat_mode)
test_Cat$reversedPayment = as.logical(test_Cat$reversedPayment)
test_Cat$noPayments = as.logical(test_Cat$noPayments)
test_Cat$credit_change = as.logical(test_Cat$credit_change)


flds <- createFolds(train$ID_CPTE, k = 10, list = TRUE, returnTrain = FALSE)

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1, number = 3, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel = T)

cv.ctrl <- trainControl(method = "none",  
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel = T)

folds = 10

xgbVector <- vector("list",length = folds)
mannVector <- vector("list",length = folds)
glmVector <- vector("list",length = folds)

predictionVector <- vector("list",length = folds)

mann.grid <- expand.grid(size = 2, decay = 0.33, bag = TRUE)
xgb.grid <- expand.grid(nrounds = 500, eta = 0.06,
                        max_depth = 9, gamma = 0.1,
                        colsample_bytree = 0.8, min_child_weight = 100, 
                        subsample = 0.8)




for ( i in 1:folds) {
testFolds = train[flds[[i]],]
trainFolds = train[-flds[[i]],]
  
testCatFolds = train_Cat[flds[[i]],]
trainCatFolds = train_Cat[-flds[[i]],]

#Need to reduce

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)


##====================================
## XGBoost
##====================================


## Run the model
xgbVector[[i]] <-train(Default ~ mean_payment + number_payments + max_payment + min_payment + 
                   median_payment + reversedPayment + noPayments + mean_balance + 
                   mean_cash_balance + max_balance + max_cash_balance + max_num_cmp + 
                   count_num_cmp + credit_change + TotalBalanceOL_perc_max + 
                   TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind + 
                   CB_limit_perc_max + CB_limit_perc_mean + Spending_mean + 
                   SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind,
                 data=trainCatFolds,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 metric = "ROC",
                 #na.action = "na.pass",
                 verbose = T,
                 nthread = 3,
                 allowParallel = TRUE)


stopCluster(cl)
registerDoSEQ()


cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

##====================================
## Model Averaged Single Hidden Layer Neural Network
##====================================

## Run the model
mannVector[[i]] <-train(Default ~ mean_payment + number_payments  + reversedPayment + noPayments + mean_balance + 
                    mean_cash_balance + max_num_cmp + 
                    count_num_cmp + credit_change  + 
                    TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind + 
                    CB_limit_perc_mean + Spending_mean + 
                    SpendingOL_perc_max + SpendingOL_ind,
                  data=trainCatFolds,
                  method="avNNet",
                  trControl = cv.ctrl,
                  tuneGrid = mann.grid,
                  metric = "auc",
                  #na.action = "na.pass",
                  verbose = T,
                  nthread = 3)


stopCluster(cl)
registerDoSEQ()



##====================================
## Logistic Regression 
##====================================

glmVector[[i]] <- glm(Default ~ mean_payment + number_payments + max_payment + min_payment + 
                           median_payment + reversedPayment + noPayments + mean_balance*max_balance + 
                           mean_cash_balance*max_cash_balance + max_num_cmp + 
                           count_num_cmp + credit_change + TotalBalanceOL_perc_max + 
                           TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind + 
                           CB_limit_perc_max + CB_limit_perc_mean  + 
                           SpendingOL_perc_max*SpendingOL_ind,
                         family = binomial, data = trainFolds)




predictionVector[[i]] = data.frame(testFolds$ID_CPTE,
                                   predict(xgbVector[[i]], newdata = testCatFolds, type = "prob")$Yes,
                                   predict(mannVector[[i]], newdata = testCatFolds, type = "prob")$Yes,
                                   predict(glmVector[[i]], newdata = testFolds, type = "response"),
                                   testFolds$Default)
colnames(predictionVector[[i]] ) = c("ID_CPTE", "XGBoost", "MANnet", "GLM", "Default")
}
