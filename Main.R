

# Desjardins Lab DataCup
# June/July 2018
# Matteo Esposito, Haiqi Liang, Fred Siino, Tony Yuan

#--------------------------------------------------------#
# 0. Preliminary                                         #
# ______________________________________________________ #
#                                                        #
#   - Load in packages & data                            #
#   - Set working directories                            #
#--------------------------------------------------------#

## WD
setwd("~/Github/DesjardinsDataCup/")

## Package loading
packages <-
  c(
    "data.table",
    "rpart",
    "caret",
    "gbm",
    "mgcv",
    "ggplot2",
    "dplyr",
    "Hmisc",
    "xgboost",
    "corrplot",
    "e1071",
    "plotly",
    "ROSE",
    "sqldf",
    "rJava",
    "ranger",
    "gam",
    "kernlab",
    "kknn",
    "naivebayes"
  )
#sapply(packages, install.packages, character.only = T)
sapply(packages, require, character.only = T)

## Load data
billing_train <-
  fread(paste0(getwd(), "/Data/facturation_train.csv")) # SUMMARY/eSTATEMENT OF WHAT YOU OWE ON CREDIT CARD by MONTH
billing_test <- fread(paste0(getwd(), "/Data/facturation_test.csv"))
payments_train <-
  fread(paste0(getwd(), "/Data/paiements_train.csv")) # PAYING OFF CREDIT CARD
payments_test <- fread(paste0(getwd(), "/Data/paiements_test.csv"))
performance_train <-
  fread(paste0(getwd(), "/Data/performance_train.csv")) # DEFAULT/NOT DEFAULT
performance_test <-
  fread(paste0(getwd(), "/Data/performance_test.csv"))
transactions_train <-
  fread(paste0(getwd(), "/Data/transactions_train.csv")) # DAILY CREDIT CARD PURCHASES
transactions_test <-
  fread(paste0(getwd(), "/Data/transactions_test.csv"))
joined_table_train <-
  fread(paste0(getwd(), "/joined_table_sql_test.csv"))
transactions_add_train <-
  read.csv(paste0(getwd(), "/Data/additional_transactions_train.csv")) # ++DAILY CREDIT CARD PURCHASES
transactions_add_test <-
  read.csv(paste0(getwd(), "/Data/additional_transactions_test.csv"))

sampSol <- fread(paste0(getwd(), "/Data/sample_solution.csv"))

#--------------------------------------------------------#
# 1. Preprocessing
# ______________________________________________________
#
#   - NA/bad formatting checks & replacements
#   - Feature engineering
#   - Table merges
#--------------------------------------------------------

# Inspect data

## NA counter function
NAcount <- function(df) {
  sapply(df, function(x)
    table(is.na(x)))
}

NAcount(billing_train)
NAcount(payments_train)
NAcount(performance_train)
NAcount(transactions_train)

NAcount(billing_test)
NAcount(payments_test)
NAcount(performance_test)
NAcount(transactions_test)

## Naive global mean imputation right now, since group avg substitution not working
payments_train$TRANSACTION_AMT <-
  ifelse(
    is.na(payments_train$TRANSACTION_AMT),
    mean(payments_train$TRANSACTION_AMT, na.rm = T),
    payments_train$TRANSACTION_AMT
  )
sum(is.na(payments_train$TRANSACTION_AMT))

# Impute with mean by group (ID)
#paiements_train_2 <- paiements_train %>%
#                    group_by(ID_CPTE) %>%
#                    mutate(TRANSACTION_AMT= ifelse(is.na(TRANSACTION_AMT), mean(TRANSACTION_AMT, na.rm=TRUE), TRANSACTION_AMT))

##====================================
## Billing features (OL = Over limit)
##====================================

## TRAIN
## Perc/Ind multicollinearity??

#combining train and test so only need to do data prep once

billing_full = rbind(billing_train, billing_test)

billing_full$TotalBalanceOL_perc = billing_full$CurrentTotalBalance / billing_full$CreditLimit
billing_full$TotalBalanceOL_ind = as.numeric(billing_full$CurrentTotalBalance > billing_full$CreditLimit)

billing_full$CB_limit_perc = billing_full$CashBalance / billing_full$CreditLimit
billing_full$CB_limit_ind = as.numeric(billing_full$CashBalance > billing_full$CreditLimit)
billing_full$CB_ind = as.numeric(billing_full$CashBalance > 0)

billing_full$Spending = billing_full$CurrentTotalBalance - billing_full$CashBalance
billing_full$SpendingOL_perc = billing_full$Spending / billing_full$CreditLimit
billing_full$SpendingOL_ind = as.numeric(billing_full$Spending > billing_full$CreditLimit)

# CMP = Consecutive missing payments
billing_full$Num_CMP = billing_full$DelqCycle

## New table to store summaritive variables (useful for modeling in the future)
billing_full_grouped <-
  setNames(data.table(matrix(
    nrow = uniqueN(billing_full$ID_CPTE), ncol = 1
  )), c("ID_CPTE"))
billing_full_grouped$ID_CPTE = unique(billing_full$ID_CPTE)

billing_full$DelqCycle_ind <-
  ifelse(billing_full$DelqCycle != 0, 1, 0)

billing_full_grouped = billing_full %>%
  group_by(ID_CPTE) %>%
  summarise(
    mean_balance = mean(CurrentTotalBalance),
    mean_cash_balance = mean(CashBalance),
    max_balance = max(CurrentTotalBalance),
    max_cash_balance = max(CashBalance),
    max_num_cmp = max(DelqCycle),
    count_num_cmp = sum(DelqCycle_ind),
    credit_change = (max(CreditLimit) != min(CreditLimit)),
    TotalBalanceOL_perc_max = max(TotalBalanceOL_perc),
    TotalBalanceOL_perc_mean = mean(TotalBalanceOL_perc),
    TotalBalanceOL_ind = max(TotalBalanceOL_ind),
    CB_ind = max(CB_ind),
    CB_limit_perc_max = max(CB_limit_perc),
    CB_limit_perc_mean = mean(CB_limit_perc),
    Spending_mean = mean(Spending),
    SpendingOL_perc_max = max(SpendingOL_perc),
    SpendingOL_perc_mean = mean(SpendingOL_perc),
    SpendingOL_ind = max(SpendingOL_ind)
  )

#split train and test

billing_train = billing_full[billing_full$ID_CPTE %in% performance_train$ID_CPTE, ]
billing_train_grouped = billing_full_grouped[billing_full_grouped$ID_CPTE %in% performance_train$ID_CPTE, ]

billing_test = billing_full[billing_full$ID_CPTE %in% performance_test$ID_CPTE, ]
billing_test_grouped = billing_full_grouped[billing_full_grouped$ID_CPTE %in% performance_test$ID_CPTE, ]



##====================================
## Transaction features
##====================================

## COMBINING TRAIN AND TEST

transactions_full = rbind(
  transactions_train,
  transactions_test,
  transactions_add_train,
  transactions_add_test
)

transactions_full$isLocal = as.numeric(transactions_full$MERCHANT_COUNTRY_XCD == "DP")

transactions_full_grouped <-
  setNames(data.table(matrix(
    nrow = uniqueN(transactions_full$ID_CPTE), ncol = 1
  )), c("ID_CPTE"))
transactions_full_grouped$ID_CPTE = unique(transactions_full$ID_CPTE)

transactions_full$trx_type_A = as.numeric(transactions_full$TRANSACTION_TYPE_XCD == "A")
transactions_full$trx_type_B = as.numeric(transactions_full$TRANSACTION_TYPE_XCD == "B")
transactions_full$trx_type_C = as.numeric(transactions_full$TRANSACTION_TYPE_XCD == "C")
transactions_full$trx_type_D = as.numeric(transactions_full$TRANSACTION_TYPE_XCD == "D")
transactions_full$trx_type_E = as.numeric(transactions_full$TRANSACTION_TYPE_XCD == "E")
transactions_full$trx_type_F = as.numeric(transactions_full$TRANSACTION_TYPE_XCD == "F")
transactions_full$trx_type_G = as.numeric(transactions_full$TRANSACTION_TYPE_XCD == "G")
transactions_full$trx_cat_A = as.numeric(transactions_full$TRANSACTION_CATEGORY_XCD == "A")
transactions_full$trx_cat_B = as.numeric(transactions_full$TRANSACTION_CATEGORY_XCD == "B")
transactions_full$trx_cat_C = as.numeric(transactions_full$TRANSACTION_CATEGORY_XCD == "C")
transactions_full$trx_cat_D = as.numeric(transactions_full$TRANSACTION_CATEGORY_XCD == "D")
transactions_full$trx_cat_E = as.numeric(transactions_full$TRANSACTION_CATEGORY_XCD == "E")
transactions_full$trxpctused = ifelse(
  transactions_full$PRIOR_CREDIT_LIMIT_AMT == 0 ,
  0,
  transactions_full$TRANSACTION_AMT / transactions_full$PRIOR_CREDIT_LIMIT_AMT
)
transactions_full_grouped = transactions_full %>%
  group_by(ID_CPTE) %>%
  summarise(
    number_transactions = n(),
    traveller_ind = ifelse(number_transactions != sum(isLocal), 0, 1),
    mean_trx_amt = mean(TRANSACTION_AMT),
    mean_trx_pct = mean(trxpctused),
    trx_type_mode = names(table(TRANSACTION_TYPE_XCD))[which.max(table(TRANSACTION_TYPE_XCD))],
    trx_cat_mode = names(table(TRANSACTION_CATEGORY_XCD))[which.max(table(TRANSACTION_CATEGORY_XCD))],
    trx_type_A_perc = mean(trx_type_A),
    trx_type_B_perc = mean(trx_type_B),
    trx_type_C_perc = mean(trx_type_C),
    trx_type_D_perc = mean(trx_type_D),
    trx_type_E_perc = mean(trx_type_E),
    trx_type_F_perc = mean(trx_type_F),
    trx_type_G_perc = mean(trx_type_G),
    trx_cat_A_perc = mean(trx_cat_A),
    trx_cat_B_perc = mean(trx_cat_B),
    trx_cat_C_perc = mean(trx_cat_C),
    trx_cat_D_perc = mean(trx_cat_D),
    trx_cat_E_perc = mean(trx_cat_E)
  )

#split train and test

transactions_train = transactions_full[transactions_full$ID_CPTE %in% performance_train$ID_CPTE, ]
transactions_train_grouped = transactions_full_grouped[transactions_full_grouped$ID_CPTE %in% performance_train$ID_CPTE, ]

transactions_test = transactions_full[transactions_full$ID_CPTE %in% performance_test$ID_CPTE, ]
transactions_test_grouped = transactions_full_grouped[transactions_full_grouped$ID_CPTE %in% performance_test$ID_CPTE, ]

## Observe the number of occurrences of each pairing (modify variables if you want to try some combinations out yourself)


## NOTE: Might need to re-run this section...
dt_inspection <-
  transactions_train[, .SD, .SDcols = c("ID_CPTE",
                                        "TRANSACTION_CATEGORY_XCD",
                                        "TRANSACTION_TYPE_XCD",
                                        "TRANSACTION_AMT")]

dt_inspection %>%
  group_by(ID_CPTE) %>%
  summarise(mean_trx_by_cat <- mean(TRANSACTION_AMT))

dt_inspection <-
  transactions_train[, .SD, .SDcols = c(
    "ID_CPTE",
    "TRANSACTION_CATEGORY_XCD",
    "TRANSACTION_TYPE_XCD",
    "TRANSACTION_AMT",
    "mean_trx_by_cat"
  )]

dt_pair_count <-
  merge(dt_inspection, train, by = "ID_CPTE", all.x = TRUE)
dt_pair_count$pair_count <- 1
aggregate(
  pair_count ~ TRANSACTION_CATEGORY_XCD + TRANSACTION_TYPE_XCD + Default +
    mean_trx_by_cat,
  dt_pair_count,
  FUN = sum
)

##====================================
## Payment features
##====================================

## TRAIN
payments_train_grouped <-
  setNames(data.table(matrix(
    nrow = uniqueN(payments_train$ID_CPTE), ncol = 1
  )), c("ID_CPTE"))
payments_train_grouped$ID_CPTE = unique(payments_train$ID_CPTE)

payments_train_grouped = payments_train %>%
  group_by(ID_CPTE) %>%
  summarise(
    mean_payment = mean(TRANSACTION_AMT),
    number_payments = n(),
    max_payment = max(TRANSACTION_AMT),
    min_payment = min(TRANSACTION_AMT),
    median_payment = median(TRANSACTION_AMT),
    reversedPayment = sum(PAYMENT_REVERSAL_XFLG == "N") >= 1,
    noPayments = sum(PAYMENT_REVERSAL_XFLG == "") >= 1
  )

payments_train_grouped[is.na(payments_train_grouped)] <- 0

## TEST
payments_test_grouped <-
  setNames(data.table(matrix(
    nrow = uniqueN(payments_test$ID_CPTE), ncol = 1
  )), c("ID_CPTE"))
payments_test_grouped$ID_CPTE = unique(payments_test$ID_CPTE)

payments_test_grouped = payments_test %>%
  group_by(ID_CPTE) %>%
  summarise(
    mean_payment = mean(TRANSACTION_AMT),
    number_payments = n(),
    max_payment = max(TRANSACTION_AMT),
    min_payment = min(TRANSACTION_AMT),
    median_payment = median(TRANSACTION_AMT),
    reversedPayment = sum(PAYMENT_REVERSAL_XFLG == "N") >= 1,
    noPayments = sum(PAYMENT_REVERSAL_XFLG == "") >= 1
  )

payments_test_grouped[is.na(payments_test_grouped)] <- 0

##====================================
## Summary table (For models)
##====================================

temp1_train = merge(payments_train_grouped, billing_train_grouped, by = "ID_CPTE")
temp2_train = merge(temp1_train, performance_train, by = "ID_CPTE")
train = merge(temp2_train,
              transactions_train_grouped,
              by = "ID_CPTE",
              all.x = TRUE)
rm(temp1_train, temp2_train)

temp1_test = merge(payments_test_grouped, billing_test_grouped, by = "ID_CPTE")
temp2_test = merge(temp1_test, performance_test, by = "ID_CPTE")
test = merge(temp2_test,
             transactions_test_grouped,
             by = "ID_CPTE",
             all.x = TRUE)
rm(temp1_test, temp2_test)

logi_vars <- c("credit_change", "reversedPayment", "noPayments")

for (cn in logi_vars) {
  train[[cn]] <- as.numeric(train[[cn]])
  test[[cn]] <- as.numeric(test[[cn]])
}

## ===================
## Fixing NAs in train and test

for (i in 1:nrow(train)) {
  if (is.na(train[i, "mean_trx_amt"])) {
    train[i, "mean_trx_amt"] = mean(transactions_full_grouped$mean_trx_amt)
  }
  if (is.na(train[i, "mean_trx_pct"])) {
    train[i, "mean_trx_pct"] = mean(transactions_full_grouped$mean_trx_pct)
  }
  if (is.na(train[i, "number_transactions"])) {
    train[i, "number_transactions"] = mean(transactions_full_grouped$number_transactions)
  }
}

for (i in 1:nrow(test)) {
  if (is.na(test[i, "mean_trx_amt"])) {
    train[i, "mean_trx_amt"] = mean(transactions_full_grouped$mean_trx_amt)
  }
  if (is.na(test[i, "mean_trx_pct"])) {
    test[i, "mean_trx_pct"] = mean(transactions_full_grouped$mean_trx_pct)
  }
  if (is.na(test[i, "number_transactions"])) {
    test[i, "number_transactions"] = mean(transactions_full_grouped$number_transactions)
  }
}


## ===================
## SQL join
## ===================

## Creating lag variable
billing_train$lag_PERIODID_MY <- Lag(billing_train$PERIODID_MY)

# Sort by vector name [z] then [x]
billing_train <- billing_train[with(billing_train, order(ID_CPTE, PERIODID_MY)),]

billing_train_forjoin <- as.data.frame(billing_train[-1])

join_pmts <- "SELECT billing_train_forjoin.*,
payments_train.TRANSACTION_AMT
FROM billing_train_forjoin a LEFT JOIN payments_train b
WHERE a.ID_CPTE = b.ID_CPTE AND b.TRANSACTION_DTTM >= a.lag_PERIODID_MY AND "

billing_join_payments <- sqldf(join_pmts, stringsAsFactors = FALSE)

## ===================
## Changing to categorical Response for certain Models
## ===================


train_Cat <- copy(train)
train_Cat$Default <-
  ifelse(train_Cat$Default == 0, "No", "Yes") # Need to make this a factor for xgb
test_Cat <- copy(test)
test_Cat$Default <-
  ifelse(test_Cat$Default == 0, "No", "Yes") # Need to make this a factor for xgb

##====================================
## Formula
##====================================

## Wanted predictors
## Since we're doing classification we don't care THAT much about multicollinearity
predictors_base <-
  c(
    "count_num_cmp",
    "mean_payment",
    "credit_change",
    "TotalBalanceOL_perc_max",
    "TotalBalanceOL_perc_mean",
    "TotalBalanceOL_ind",
    "SpendingOL_perc_max",
    "SpendingOL_perc_mean",
    "SpendingOL_ind"
  )

predictors_1 <-
  c(
    "count_num_cmp",
    "mean_payment",
    "credit_change",
    "TotalBalanceOL_perc_max",
    "TotalBalanceOL_perc_mean",
    "SpendingOL_perc_max",
    "SpendingOL_perc_mean"
  )

xgb_vars <-  c(
  "mean_payment",
  "number_payments",
  "max_payment",
  "
               min_payment",
  "median_payment",
  "reversedPayment",
  "noPayments",
  "
               mean_balance",
  "mean_cash_balance",
  "max_balance",
  "max_cash_balance",
  "
               max_num_cmp",
  "count_num_cmp",
  "credit_change",
  "TotalBalanceOL_perc_max",
  "
               TotalBalanceOL_perc_mean",
  "TotalBalanceOL_ind",
  "CB_ind",
  "
               CB_limit_perc_max",
  "CB_limit_perc_mean",
  "Spending_mean",
  "
               SpendingOL_perc_max",
  "SpendingOL_perc_mean",
  "SpendingOL_ind"
)

revised_vars <-
  c(
    "TotalBalanceOL_perc_max",
    "count_num_cmp",
    "CB_limit_perc_mean",
    "median_payment",
    "number_payments",
    "max_balance",
    "max_cash_balance",
    "credit_change",
    "pred_logreg"
  )

## Creating formula for models post correlation analysis
formula <-
  as.formula(paste("Default ~", paste(revised_vars, collapse = "+")))

##====================================
## Data Visualization
##====================================

## Use Tony's Shiny app

## Correlation Plot
dt_corr <-
  na.omit(train[, !colnames(train) %in% c(
    "PERIODID_MY",
    "number_transactions",
    "traveller_ind",
    "trx_type_mode",
    "trx_cat_mode"
  )])

dt_corr2 <- na.omit(train[, colnames(train) %in% xgb_vars])
dt_corr3 <- na.omit(train[, colnames(train) %in% revised_vars])

## Output corrplot
corrplot(cor(dt_corr3),
         method = "circle",
         order = "alphabet",
         type = "lower")

#--------------------------------------------------------#
# 2. Modeling
# ______________________________________________________
#
#   - Finding Parameters for Different level 1 models
#   - Decision trees/rpart
#   - Logistic Regression (GLM)
#   - XGBoost
#   - SVM
#--------------------------------------------------------#

## Spliting Data for models
library(caTools)
set.seed(8)
split = sample.split(train$Default, SplitRatio = 0.70)
model_train = subset(train, split == TRUE)
model_test = subset(train, split == FALSE)

##====================================
## Optimal Model Parameters
##====================================

cv.ctrl <-
  trainControl(
    method = "repeatedcv",
    repeats = 1,
    number = 4,
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    allowParallel = T
  )
##====================================
## XGBoost Model Parameters
##====================================

train$Default = as.factor(train$Default)

xgb.grid <- expand.grid(
  nrounds = c(100, 300, 500),
  eta = c(0, 0),
  max_depth = c(5, 7, 9),
  gamma = c(0, 0.1),
  colsample_bytree = 0.8,
  min_child_weight = c(1, 50, 100),
  subsample = 0.8
)

xgb.grid <- expand.grid(
  nrounds = c(100, 300),
  eta = c(0),
  max_depth = c(1, 5, 9),
  gamma = c(0, 0.1),
  colsample_bytree = 0.8,
  min_child_weight = c(1, 10, 100),
  subsample = 0.8
)

xgb.tune = train(
  Default ~ mean_payment + number_payments + max_payment + min_payment +
    median_payment + reversedPayment + noPayments + mean_balance +
    mean_cash_balance + max_balance + max_cash_balance + max_num_cmp +
    count_num_cmp + credit_change + TotalBalanceOL_perc_max +
    TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind +
    CB_limit_perc_max + CB_limit_perc_mean + Spending_mean +
    SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind +
    mean_trx_amt + mean_trx_pct + number_transactions,
  data = train_Cat,
  method = "xgbTree",
  trControl = cv.ctrl,
  tuneGrid = xgb.grid,
  metric = "ROC"
)



##====================================
## Recursive Partitioning (ROC = 0.681)
##====================================

rpart_classifier <-
  rpart(
    formula,
    data = model_train,
    method = "class",
    control = rpart.control(
      minsplit = 100,
      maxdepth = 10,
      cp = 0.001
    )
  )

pred_rpart <- predict(rpart_classifier, model_test, type = "class")
table(model_test$Default, pred_rpart)

# ROC = 0.681
roc.curve(model_test$Default, pred_rpart)

##====================================
## Logistic Regression (ROC = 0.798)
##====================================
logi_vars <- c("credit_change", "reversedPayment", "noPayments")

for (cn in logi_vars) {
  model_train[[cn]] <- as.numeric(model_train[[cn]])
  model_test[[cn]] <- as.numeric(model_test[[cn]])
}

formula_logreg <-
  as.formula(paste("Default ~", paste(revised_vars, collapse = "+")))

logreg_classifier <-
  glm(formula, family = binomial, data = model_train)

pred_logreg <-
  predict(logreg_classifier, newdata = model_test, type = "response")

## Calculate ROC
roc.curve(model_test$Default, pred_logreg)

## Final prediction for submission

pred_logreg_test <-
  predict(logreg_classifier, newdata = test, type = "response")

##====================================
## XGBoost (ROC = 0.885)
##====================================

## Formatting tables for xgboost
revised_vars <-
  c(
    "TotalBalanceOL_perc_max",
    "count_num_cmp",
    "CB_limit_perc_mean",
    "median_payment",
    "number_payments",
    "max_balance",
    "max_cash_balance",
    "credit_change",
    "pred_logreg_test"
  )

logi_vars <- c("credit_change", "reversedPayment", "noPayments")

for (cn in logi_vars) {
  model_train[[cn]] <- as.numeric(model_train[[cn]])
  model_test[[cn]] <- as.numeric(model_test[[cn]])
}

set.seed(100)

train_label <- as.numeric(as.factor(model_train$Default)) - 1
test_label <- as.numeric(as.factor(model_test$Default)) - 1

train_for_xgb <-
  as.matrix(copy(model_train[, colnames(model_train) %in% c(revised_vars, "Default")]))
test_for_xgb <-
  as.matrix(copy(model_test[, colnames(model_test) %in% c(revised_vars, "Default")]))


dtrain <- xgb.DMatrix(data = train_for_xgb,
                      label = train_label)

dtest <- xgb.DMatrix(data = test_for_xgb,
                     label = test_label)

## Grid search

## READ-ME ********
##
## When the output csv is written, re-visit the console and check to see which iteration for each version (V1,V2,..,VN)
## is the best. What is written in the first 2 rows of the output csv are the train and test auc.
##
## Below each training and testing auc are the combination of hyperparameters used to get those metrics.
## Good luck :P

searchGridSubCol <- expand.grid(
  subsample = 0.8,
  colsample_bytree = c(0.8),
  max_depth = 3,
  min_child = c(1, 20, 50),
  eta = c(0.05, 0.1, 0.2),
  gamma = c(0, 1, 5)
)

cv_function <- function(parameterList) {
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentDepth <- parameterList[["max_depth"]]
  currentEta <- parameterList[["eta"]]
  currentMinChild <- parameterList[["min_child"]]
  currentGamma <- parameterList[["gamma"]]
  
  xgboostModelCV <-
    xgb.cv(
      data =  dtrain,
      nrounds = 500,
      nfold = 3,
      showsd = TRUE,
      metrics = "auc",
      verbose = TRUE,
      "eval_metric" = "auc",
      "gamma" = currentGamma,
      "objective" = "binary:logistic",
      "max.depth" = currentDepth,
      "eta" = currentEta,
      "subsample" = currentSubsampleRate,
      "colsample_bytree" = currentColsampleRate
      ,
      print_every_n = 5,
      "min_child_weight" = currentMinChild,
      booster = "gbtree",
      early_stopping_rounds = 10
    )
  
  #print(xgboostModelCV$evaluation_log)
  xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
  auc <- tail(xvalidationScores$train_auc_mean, 1)
  tauc <- tail(xvalidationScores$test_auc_mean, 1)
  output <-
    return(
      c(
        auc,
        tauc,
        currentSubsampleRate,
        currentColsampleRate,
        currentDepth,
        currentEta,
        currentMinChild,
        currentGamma
      )
    )
}

system.time(aucErrorsHyperparameters <-
              apply(searchGridSubCol, 1, cv_function))

output <- as.data.frame((aucErrorsHyperparameters))
output
write.csv(output, paste0(getwd(), "/xgb_gridsearch.csv"))

## Keeping the model under control
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eta = 0.1,
    gamma = 5,
    max_depth = 3,
    min_child_weight = 1,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

# xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 300, nfold = 10, showsd = T, stratified = T,
#                 print_every_n = 5, early_stopping_rounds = 10)

## Model with optimal parameters

xgb1 <- xgb.train(
  params = params
  ,
  data = dtrain
  ,
  nrounds = 500
  ,
  watchlist = list(val = dtest, train = dtrain)
  ,
  print_every_n = 2
  ,
  early_stopping_round = 5
  #,maximize = F
  ,
  eval_metric = "auc"
)

xgbpred <- predict(xgb1, dtest)
# xgbpred <- ifelse(xgbpred > 0.5,1,0)
# confusionMatrix(xgbpred, test_label)

## Calculate ROC
roc.curve(model_test$Default, xgbpred)

## Variable Importance Graph
mat <- xgb.importance(feature_names = colnames(dtrain), model = xgb1)
xgb.plot.importance(mat)

## Final prediction for submission
test_final_label <- as.numeric(as.factor(test$Default)) - 1
test_final_xgb <-
  as.matrix(copy(test[, colnames(test) %in% c(revised_vars, "Default")]))
dtest_final <- xgb.DMatrix(data = test_final_xgb,
                           label = test_final_label)


xgb_submission <- predict(xgb1, dtest_final)

##====================================
## Out Of Sample Prediction + Measure
##====================================

OutSamplePred = function(data = train,
                         folds = 10,
                         FUN ,
                         caretUsed = TRUE) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  flds <-
    createFolds(data$ID_CPTE,
                k = folds,
                list = TRUE,
                returnTrain = FALSE)
  responseType = ""
  if (caretUsed) {
    responseType = "prob"
  } else {
    responseType = "response"
  }
  
  
  modelVector = vector("list", length = folds)
  predictionVector = vector("list", length = folds)
  
  for (i in 1:folds) {
    testFolds = data[flds[[i]], ]
    trainFolds = data[-flds[[i]], ]
    
    
    
    ## Run the model
    modelVector[[i]] <- FUN(trainFolds)
    
    
    
    predictionVector[[i]] = data.frame(
      testFolds$ID_CPTE,
      predict(modelVector[[i]], newdata = testFolds, type = responseType),
      testFolds$Default
    )
    
  }
  
  
  df = rbind(predictionVector[[1]], predictionVector[[2]])
  for (i in 3:folds) {
    df = rbind(df, predictionVector[[i]])
  }
  return(df)
}

level1ModelList = list(
  "svmRadial",
  "gam",
  "xgboost",
  "var1xgboost",
  "var2xgboost",
  "ranger",
  "mann",
  "knn2",
  "knn4",
  "knn8",
  "knn16",
  "nb"
)
level1DataList =  vector("list", length = length(level1ModelList))


svmRadialFunction = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  svm.grid = expand.grid(sigma = 0.05, C = 1)
  
  
  return(
    train(
      Default ~ mean_payment + number_payments + max_payment + min_payment +
        median_payment + reversedPayment + noPayments + mean_balance +
        mean_cash_balance + max_balance + max_cash_balance + max_num_cmp +
        count_num_cmp + credit_change + TotalBalanceOL_perc_max +
        TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind +
        CB_limit_perc_max + CB_limit_perc_mean + Spending_mean +
        SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind,
      data = data,
      method = "svmRadial",
      trControl = cv.ctrl,
      tuneGrid = svm.grid,
      #na.action = "na.pass",
      verbose = T,
      nthread = 3
    )
  )
}
gamFunction = function(data = trainFolds) {
  return(
    gam(
      Default ~ s(mean_payment, 7) + s(number_payments, 4) + max_payment + min_payment *
        median_payment +
        reversedPayment + noPayments + mean_balance * max_balance +
        mean_cash_balance +  max_num_cmp +
        count_num_cmp + credit_change + s(TotalBalanceOL_perc_max, 4) *
        s(TotalBalanceOL_perc_mean, 3) +
        TotalBalanceOL_ind + CB_ind +
        CB_limit_perc_max * CB_limit_perc_mean  +
        SpendingOL_perc_max * SpendingOL_ind,
      family = binomial,
      data = data
    )
  )
}
xgboostFunction = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  xgb.grid <- expand.grid(
    nrounds = 300,
    eta = 0.05,
    max_depth = 9,
    gamma = 0.1,
    colsample_bytree = 0.8,
    min_child_weight = 100,
    subsample = 0.8
  )
  return(
    train(
      Default ~ mean_payment + number_payments + max_payment + min_payment +
        median_payment + reversedPayment + noPayments + mean_balance +
        mean_cash_balance + max_balance + max_cash_balance + max_num_cmp +
        count_num_cmp + credit_change + TotalBalanceOL_perc_max +
        TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind +
        CB_limit_perc_max + CB_limit_perc_mean + Spending_mean +
        SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind,
      data = data,
      method = "xgbTree",
      trControl = cv.ctrl,
      tuneGrid = xgb.grid,
      #na.action = "na.pass",
      verbose = T,
      nthread = 3
    )
  )
}
var1xgboostFunction = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  var1xgb.grid <- expand.grid(
    nrounds = 100,
    eta = 0.3,
    max_depth = 2,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.5
  )
  return(
    train(
      Default ~ max_num_cmp +
        count_num_cmp + credit_change + TotalBalanceOL_perc_max +
        TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind +
        CB_limit_perc_max + CB_limit_perc_mean + Spending_mean +
        SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind,
      data = data,
      method = "xgbTree",
      trControl = cv.ctrl,
      tuneGrid = var1xgb.grid,
      #na.action = "na.pass",
      verbose = T,
      nthread = 3
    )
  )
}
var2xgboostFunction = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  var2xgb.grid <- expand.grid(
    nrounds = 100,
    eta = 0.3,
    max_depth = 3,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.5
  )
  return(
    train(
      Default ~ mean_payment + number_payments + max_payment + min_payment +
        median_payment + reversedPayment + noPayments + mean_balance +
        mean_cash_balance + max_balance + max_cash_balance,
      data = data,
      method = "xgbTree",
      trControl = cv.ctrl,
      tuneGrid = var2xgb.grid,
      #na.action = "na.pass",
      verbose = T,
      nthread = 3
    )
  )
}
rangerFunction = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  ranger.grid = expand.grid(mtry = 2,
                            splitrule = "gini",
                            min.node.size = 2)
  return(
    train(
      Default ~ mean_payment + number_payments + max_payment + min_payment +
        median_payment + reversedPayment + noPayments + mean_balance +
        mean_cash_balance + max_balance + max_cash_balance + max_num_cmp +
        count_num_cmp + credit_change + TotalBalanceOL_perc_max +
        TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind +
        CB_limit_perc_max + CB_limit_perc_mean + Spending_mean +
        SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind,
      data = data,
      method = "ranger",
      trControl = cv.ctrl,
      tuneGrid = ranger.grid,
      #na.action = "na.pass",
      verbose = T
    )
  )
}
mannFunction = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  
  mann.grid <- expand.grid(size = 2,
                           decay = 0.33,
                           bag = TRUE)
  
  
  train(
    Default ~ mean_payment + number_payments  + reversedPayment + noPayments + mean_balance +
      mean_cash_balance  +  max_num_cmp +
      count_num_cmp + credit_change  +
      TotalBalanceOL_ind + TotalBalanceOL_perc_max + CB_ind +
      CB_limit_perc_mean + Spending_mean +
      SpendingOL_perc_max + SpendingOL_ind,
    data = data,
    method = "avNNet",
    trControl = cv.ctrl,
    tuneGrid = mann.grid,
    #na.action = "na.pass",
    verbose = T,
    nthread = 3
  )
}
knn2Function = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  knn.grid <- expand.grid(k = 2)
  
  return(
    train(
      Default ~ mean_payment + number_payments + max_payment + min_payment +
        median_payment + reversedPayment + noPayments + mean_balance +
        mean_cash_balance + max_balance + max_cash_balance + max_num_cmp +
        count_num_cmp + credit_change + TotalBalanceOL_perc_max +
        TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind +
        CB_limit_perc_max + CB_limit_perc_mean + Spending_mean +
        SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind,
      data = data,
      method = "knn",
      trControl = cv.ctrl,
      tuneGrid = knn.grid
    )
  )
}
knn4Function = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  knn.grid <- expand.grid(k = 4)
  
  return(
    train(
      Default ~ mean_payment + number_payments + max_payment + min_payment +
        median_payment + reversedPayment + noPayments + mean_balance +
        mean_cash_balance + max_balance + max_cash_balance + max_num_cmp +
        count_num_cmp + credit_change + TotalBalanceOL_perc_max +
        TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind +
        CB_limit_perc_max + CB_limit_perc_mean + Spending_mean +
        SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind,
      data = data,
      method = "knn",
      trControl = cv.ctrl,
      tuneGrid = knn.grid
    )
  )
}
knn8Function = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  knn.grid <- expand.grid(k = 8)
  
  return(
    train(
      Default ~ mean_payment + number_payments + max_payment + min_payment +
        median_payment + reversedPayment + noPayments + mean_balance +
        mean_cash_balance + max_balance + max_cash_balance + max_num_cmp +
        count_num_cmp + credit_change + TotalBalanceOL_perc_max +
        TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind +
        CB_limit_perc_max + CB_limit_perc_mean + Spending_mean +
        SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind,
      data = data,
      method = "knn",
      trControl = cv.ctrl,
      tuneGrid = knn.grid
    )
  )
}
knn16Function = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  knn.grid <- expand.grid(k = 16)
  
  return(
    train(
      Default ~ mean_payment + number_payments + max_payment + min_payment +
        median_payment + reversedPayment + noPayments + mean_balance +
        mean_cash_balance + max_balance + max_cash_balance + max_num_cmp +
        count_num_cmp + credit_change + TotalBalanceOL_perc_max +
        TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind +
        CB_limit_perc_max + CB_limit_perc_mean + Spending_mean +
        SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind,
      data = data,
      method = "knn",
      trControl = cv.ctrl,
      tuneGrid = knn.grid
    )
  )
}
nbFunction = function(data = trainFolds) {
  cv.ctrl <- trainControl(method = "none",
                          #summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = T)
  nb.grid = expand.grid(laplace = 0,
                        adjust = 0,
                        usekernel = TRUE)
  
  nb.tune <-
    train(
      Default ~ mean_payment + number_payments + max_payment + min_payment +
        median_payment + reversedPayment + noPayments + mean_balance +
        mean_cash_balance + max_balance + max_cash_balance + max_num_cmp +
        count_num_cmp + credit_change + TotalBalanceOL_perc_max +
        TotalBalanceOL_perc_mean + TotalBalanceOL_ind + CB_ind +
        CB_limit_perc_max + CB_limit_perc_mean + Spending_mean +
        SpendingOL_perc_max + SpendingOL_perc_mean + SpendingOL_ind,
      data = train_Cat,
      method = "naive_bayes",
      trControl = cv.ctrl,
      tuneGrid = nb.grid,
      #na.action = "na.pass",
      verbose = T,
      nthread = 3
    )
  
}

svmRadialtest = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = svmRadialFunction,
  caretUsed = TRUE
)
gamtest = OutSamplePred(
  data = train,
  folds = 10,
  FUN = gamFunction,
  caretUsed = FALSE
)
xgboosttest = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = xgboostFunction,
  caretUsed = TRUE
)
var1xgboosttest = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = var1xgboostFunction,
  caretUsed = TRUE
)
var2xgboosttest = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = var2xgboostFunction,
  caretUsed = TRUE
)
rangertest = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = rangerFunction,
  caretUsed = TRUE
)
manntest = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = mannFunction,
  caretUsed = TRUE
)
knn2test = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = knn2Function,
  caretUsed = TRUE
)
knn4test = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = knn4Function,
  caretUsed = TRUE
)
knn8test = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = knn8Function,
  caretUsed = TRUE
)
knn16test = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = knn16Function,
  caretUsed = TRUE
)
nbtest = OutSamplePred(
  data = train_Cat,
  folds = 10,
  FUN = nbFunction,
  caretUsed = TRUE
)

colnames(svmRadialtest) = c("ID_CPTE", "drop", "svmRadial", "Default")
colnames(gamtest) = c("ID_CPTE", "gam", "Default")
colnames(xgboosttest) = c("ID_CPTE", "drop", "xgboost", "Default")
colnames(var1xgboosttest) = c("ID_CPTE", "drop", "var1xgboost", "Default")
colnames(var2xgboosttest) = c("ID_CPTE", "drop", "var2xgboost", "Default")
colnames(rangertest) = c("ID_CPTE", "drop", "ranger", "Default")
colnames(manntest) = c("ID_CPTE", "drop", "mann", "Default")
colnames(knn2test) = c("ID_CPTE", "drop", "knn2", "Default")
colnames(knn4test) = c("ID_CPTE", "drop", "knn4", "Default")
colnames(knn8test) = c("ID_CPTE", "drop", "knn8", "Default")
colnames(knn16test) = c("ID_CPTE", "drop", "knn16", "Default")
colnames(nbtest) = c("ID_CPTE", "drop", "nb", "Default")


#--------------------------------------------------------#
# 3. Submission
# ______________________________________________________
#
#   - Create submission .csv
#--------------------------------------------------------#

test$final_pred <- xgb_submission

submission <- data.frame(test$ID_CPTE, test$final_pred)
colnames(submission) = c("ID_CPTE", "Default")

#submission_final = merge(submission,performance_test, by = "ID_CPTE")

write.csv(submission,
          paste0(getwd(), "/Submissions/submission_1_JUL12.csv"))

## Command for excel - lol
# =VLOOKUP(E2,$B$2:$C$5101,2,$C$2:$C$5101)
