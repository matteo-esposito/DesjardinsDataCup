

## Cross-validation
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 2, number = 5, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel = T)

## Formula for 
formula = as.formula("Default ~ mean_payment + number_payments + max_payment + 
                           +                  min_payment + median_payment + reversedPayment + noPayments + 
                           +                  mean_balance + mean_cash_balance + max_balance + max_cash_balance + 
                           +                  max_num_cmp + count_num_cmp + credit_change + TotalBalanceOL_perc_max + 
                           +                  TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind + 
                           +                  CB_limit_perc_max + CB_limit_perc_mean + Spending_mean + 
                           +                  SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind")

## 
train_boost <- copy(train)
train_boost$Default <- ifelse(train_boost$Default == 0, "No", "Yes") # Need to make this a factor for xgb


##====================================
## LEVEL 1 MODELS
##====================================


##====================================
## Logistic Regression
##====================================

logreg_classifier <- glm(Default ~ mean_payment + number_payments + max_payment + 
                           +                  min_payment + median_payment + reversedPayment + noPayments + 
                           +                  mean_balance + mean_cash_balance + max_balance + max_cash_balance + 
                           +                  max_num_cmp + count_num_cmp + credit_change + TotalBalanceOL_perc_max + 
                           +                  TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind + 
                           +                  CB_limit_perc_max + CB_limit_perc_mean + 
                           +                  SpendingOL_perc_max  + SpendingOL_ind, family = binomial, data = train)

pred_logreg <- predict(logreg_classifier, newdata = test, type = "response")
pred_rounded_logreg <- ifelse(pred_logreg >= 0.257,1,0)
table(pred_rounded_logreg,model_test$Default)

model_test$pred1 = pred_rounded_logreg
model_test$pred2 = as.numeric(pred_rpart)-1

##====================================
## GLM Step AIC Regression
##====================================

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

StepAIC.tune = train(formula, data = train_boost, 
                method = 'glmStepAIC', 
                trControl = cv.ctrl, family = "binomial")
stopCluster(cl)
registerDoSEQ()

##====================================
## Random Forest
##====================================

#Choosing mtry between 2 and 10 for analysis
rf.Grid <-  expand.grid( 
  mtry  = c(2:10)
)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

rf.tune = train(formula, data = train_boost, 
                      method = 'rf', 
                      trControl = cv.ctrl,
                      tuneGrid = cv.ctrl)
stopCluster(cl)
registerDoSEQ()

##====================================
## XGBoost
##====================================

## Grid searching
xgb.grid <- expand.grid(nrounds = 300, eta = c(0.01,0.05,0.1),
                        max_depth = c(5,7,9), gamma = 0,
                        colsample_bytree = 0.8, min_child_weight = 100, 
                        subsample = 0.8)



# Modifying formula after seeing the output of the first run.
# predictors_xgb <- c("TotalBalanceOL_perc_max", "TotalBalanceOL_perc_mean", "count_num_cmp", "credit_change")
# formula_xgb <- as.formula(paste0("Default ~", paste(predictors_xgb, collapse = "+")))

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

## Run the model
xgb.tune <-train(formula,
                 data=train_boost,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 #na.action = "na.pass",
                 verbose = T,
                 nthread = 3)


stopCluster(cl)
registerDoSEQ()
##====================================
## AdaBoost Classification Trees
##====================================

# adaboost.grid <- expand.grid(mfinal = )



# Modifying formula after seeing the output of the first run.
# predictors_xgb <- c("TotalBalanceOL_perc_max", "TotalBalanceOL_perc_mean", "count_num_cmp", "credit_change")
# formula_xgb <- as.formula(paste0("Default ~", paste(predictors_xgb, collapse = "+")))

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

## Run the model
adaboost.tune <-train(formula,
                 data=train_boost,
                 method="adaboost",
                 trControl=cv.ctrl,
                 tuneGrid=ada.grid,
                 #na.action = "na.pass",
                 verbose = T,
                 nthread = 3)


stopCluster(cl)
registerDoSEQ()
##====================================
## Bagged AdaBoost
##====================================

ada.grid <- expand.grid(mfinal = )



# Modifying formula after seeing the output of the first run.
# predictors_xgb <- c("TotalBalanceOL_perc_max", "TotalBalanceOL_perc_mean", "count_num_cmp", "credit_change")
# formula_xgb <- as.formula(paste0("Default ~", paste(predictors_xgb, collapse = "+")))

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

## Run the model
ada.tune <-train(formula,
                 data=train_boost,
                 method="AdaBag",
                 trControl=cv.ctrl,
                 tuneGrid=ada.grid,
                 #na.action = "na.pass",
                 verbose = T,
                 nthread = 3)


stopCluster(cl)
registerDoSEQ()

##====================================
## Model Averaged Neural Network
##====================================

mann.grid <- expand.grid(size = c(3,5,7), decay = c(0.1,0.15,0.2), bag = TRUE)


cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

## Run the model
mann.tune <-train(Default ~ mean_payment + number_payments  + 
                    +                 reversedPayment + noPayments + 
                    +                  mean_balance + mean_cash_balance + count_num_cmp + credit_change + 
                    +                  TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind + 
                    +                  CB_limit_perc_mean + SpendingOL_perc_max  + SpendingOL_ind,
                 data=train_boost,
                 method="avNNet",
                 trControl = cv.ctrl,
                 tuneGrid = mann.grid,
                 #na.action = "na.pass",
                 verbose = T,
                 nthread = 3)


stopCluster(cl)
registerDoSEQ()

##====================================
## Model Averaged Naive Bayes 
##====================================
#
# manb.grid <- expand.grid(mfinal = )


cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

## Run the model
manb_tune <-train(Default ~ mean_payment + number_payments  + 
                    +                 reversedPayment + noPayments + 
                    +                  mean_balance + mean_cash_balance + count_num_cmp + credit_change + 
                    +                  TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind + 
                    +                  CB_limit_perc_mean + SpendingOL_perc_max  + SpendingOL_ind,
                  data=train_boost,
                  method="manb",
                  trControl = cv.ctrl,
                  #tuneGrid = mann.grid,
                  #na.action = "na.pass",
                  verbose = T,
                  nthread = 3)


stopCluster(cl)
registerDoSEQ()



##====================================
## Support Vector Machines Linear Kernel
##====================================

# svm.grid <- expand.grid(mfinal = )


cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

## Run the model
svm.tune <-train(formula,
                  data=train_boost,
                  method="svmLinear2",
                  trControl = cv.ctrl,
                  #tuneGrid = mann.grid,
                  #na.action = "na.pass",
                  verbose = T,
                  nthread = 3)


stopCluster(cl)
registerDoSEQ()

##====================================
## K Nearest Neighbour
##====================================

# kknn.grid <- expand.grid(mfinal = )


cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

## Run the model
kknn.tune <-train(formula,
                 data=train_boost,
                 method="kknn",
                 trControl = cv.ctrl,
                 #tuneGrid = mann.grid,
                 #na.action = "na.pass",
                 verbose = T,
                 nthread = 3)


stopCluster(cl)
registerDoSEQ()
##====================================
## Support Vector Machines Radial Kernel
##====================================

# svm.grid <- expand.grid(mfinal = )


cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

## Run the model
svm.tune <-train(formula,
                 data=train_boost,
                 method="svmLinear2",
                 trControl = cv.ctrl,
                 #tuneGrid = mann.grid,
                 #na.action = "na.pass",
                 verbose = T,
                 nthread = 3)


stopCluster(cl)
registerDoSEQ()

##====================================
## LEVEL 2 MODEL
##====================================
 
# trainlevel2 = data.frame(predict(xgb.tune, newdata = train, type = "prob")$Yes,
#                          predict(logreg_classifier, newdata = train),
#                          predict(rf.tune, newdata = train, type = "prob")$Yes,
#                          predict(ada.tune, newdata = train, type = "prob")$Yes,
#                          predict(mann.tune, newdata = train, type = "prob")$Yes,
#                          train$Default)
# 
# colnames(trainlevel2) = c("v1", "v2", "v3", "v4", "v5", "Default")
# glm.fit.level2 <- glm(Default ~ v1*v3 + v2 + v4 + v5, family = binomial, data = trainlevel2)


testlevel2 = data.frame(test$ID_CPTE, 
                         predict(xgb.tune, newdata = test),
                         ifelse(predict(logreg_classifier, newdata = test)>0.4,1,0),
                         predict(rf.tune, newdata = test),
                         predict(ada.tune, newdata = test),
                         predict(kknn.tune, newdata = test),
                         predict(svm.tune, newdata = test),
                         predict(mann.tune, newdata = test))
colnames(testlevel2) = c( "ID_CPTE", "v1", "v2", "v3", "v4", "v5", "v6", "v7")

testlevel2$v1 = ifelse(testlevel2$v1 == "No", 0 , 1)
testlevel2$v3 = ifelse(testlevel2$v3 == "No", 0 , 1)
testlevel2$v4 = ifelse(testlevel2$v4 == "No", 0 , 1)
testlevel2$v5 = ifelse(testlevel2$v5 == "No", 0 , 1)
testlevel2$v6 = ifelse(testlevel2$v6 == "No", 0 , 1)
testlevel2$v7 = ifelse(testlevel2$v7 == "No", 0 , 1)

write.csv(testlevel2, paste0(getwd(),"/Data/level2Submission.csv"))
