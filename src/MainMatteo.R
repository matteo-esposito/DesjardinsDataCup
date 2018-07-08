
# Desjardins Lab DataCup
# June/July 2018
# Matteo Esposito, Haiqi Liang, Fred Siino, Tony Yuan (alphabetical bros)

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
packages <- c("data.table", "rpart", "caret", "gbm", "mgcv", "ggplot2", "dplyr", 
              "Hmisc", "xgboost", "corrplot", "e1071", "plotly")
sapply(packages, require, character.only = T)

## Load data
billing_train <- as.data.table(read.csv(paste0(getwd(),"/Data/facturation_train.csv")))
billing_test <- as.data.table(read.csv(paste0(getwd(),"/Data/facturation_test.csv"))) 
payments_train <- as.data.table(read.csv(paste0(getwd(),"/Data/paiements_train.csv"))) 
payments_test <- as.data.table(read.csv(paste0(getwd(),"/Data/paiements_test.csv"))) 
performance_train <- as.data.table(read.csv(paste0(getwd(),"/Data/performance_train.csv")))
performance_test <- as.data.table(read.csv(paste0(getwd(),"/Data/performance_test.csv")))
transactions_train <- as.data.table(read.csv(paste0(getwd(),"/Data/transactions_train.csv"))) 
transactions_test <- as.data.table(read.csv(paste0(getwd(),"/Data/transactions_test.csv")))

sampSol <- as.data.table(read.csv(paste0(getwd(),"/Data/sample_solution.csv")))

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
NAcount <- function(df){
  sapply(df,function(x) table(is.na(x)))
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
payments_train$TRANSACTION_AMT <- ifelse(is.na(payments_train$TRANSACTION_AMT), mean(payments_train$TRANSACTION_AMT, na.rm=T), payments_train$TRANSACTION_AMT)
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
billing_train$TotalBalanceOL_perc = billing_train$CurrentTotalBalance/billing_train$CreditLimit
billing_train$TotalBalanceOL_ind = as.numeric(billing_train$CurrentTotalBalance > billing_train$CreditLimit)

billing_train$CB_limit_perc = billing_train$CashBalance/billing_train$CreditLimit
billing_train$CB_limit_ind = as.numeric(billing_train$CashBalance > billing_train$CreditLimit)
billing_train$CB_ind = as.numeric(billing_train$CashBalance > 0 )

billing_train$Spending = billing_train$CurrentTotalBalance - billing_train$CashBalance
billing_train$SpendingOL_perc = billing_train$Spending/billing_train$CreditLimit
billing_train$SpendingOL_ind = as.numeric(billing_train$Spending > billing_train$CreditLimit)

# CMP = Consecutive missing payments
billing_train$Num_CMP = billing_train$DelqCycle

## New table to store summaritive variables (useful for modeling in the future)
billing_train_grouped <- setNames(data.table(matrix(nrow = uniqueN(billing_train$ID_CPTE), ncol = 1)), c("ID_CPTE"))
billing_train_grouped$ID_CPTE = unique(billing_train$ID_CPTE)

billing_train$DelqCycle_ind <- ifelse(billing_train$DelqCycle != 0, 1, 0)

billing_train_grouped = billing_train %>%
  group_by(ID_CPTE) %>%
  summarise(
    mean_balance = mean(CurrentTotalBalance),
    mean_cash_balance = mean(CashBalance),
    max_balance = max(CurrentTotalBalance),
    max_cash_balance = max(CashBalance),
    max_num_cmp = max(DelqCycle),
    count_num_cmp = sum(DelqCycle_ind),
    credit_change = (max(CreditLimit) != min(CreditLimit))
  )

## TEST
billing_test$TotalBalanceOL_perc = billing_test$CurrentTotalBalance/billing_test$CreditLimit
billing_test$TotalBalanceOL_ind = as.numeric(billing_test$CurrentTotalBalance > billing_test$CreditLimit)

billing_test$CB_limit_perc = billing_test$CashBalance/billing_test$CreditLimit
billing_test$CB_limit_ind = as.numeric(billing_test$CashBalance > billing_test$CreditLimit)
billing_test$CB_ind = as.numeric(billing_test$CashBalance > 0 )

billing_test$Spending = billing_test$CurrentTotalBalance - billing_test$CashBalance
billing_test$SpendingOL_perc = billing_test$Spending/billing_test$CreditLimit
billing_test$SpendingOL_ind = as.numeric(billing_test$Spending > billing_test$CreditLimit)

# CMP = Consecutive missing payments
billing_test$Num_CMP = billing_test$DelqCycle

## New table to store summaritive variables (useful for modeling in the future)
billing_test_grouped <- setNames(data.table(matrix(nrow = uniqueN(billing_test$ID_CPTE), ncol = 1)), c("ID_CPTE"))
billing_test_grouped$ID_CPTE = unique(billing_test$ID_CPTE)

billing_test$DelqCycle_ind <- ifelse(billing_test$DelqCycle != 0, 1, 0)

billing_test_grouped = billing_test %>%
  group_by(ID_CPTE) %>%
  summarise(
    mean_balance = mean(CurrentTotalBalance),
    mean_cash_balance = mean(CashBalance),
    max_balance = max(CurrentTotalBalance),
    max_cash_balance = max(CashBalance),
    max_num_cmp = max(DelqCycle),
    count_num_cmp = sum(DelqCycle_ind),
    credit_change = (max(CreditLimit) != min(CreditLimit))
  )

##====================================
## Transaction features
##====================================

## TRAIN
transactions_train$isLocal = as.numeric(transactions_train$MERCHANT_COUNTRY_XCD == "DP")

transactions_train_grouped <- setNames(data.table(matrix(nrow = uniqueN(transactions_train$ID_CPTE), ncol = 1)), c("ID_CPTE"))
transactions_train_grouped$ID_CPTE = unique(transactions_train$ID_CPTE)

transactions_train_grouped = transactions_train %>%
  group_by(ID_CPTE) %>%
  summarise(
    number_transactions = n(), 
    traveller_ind = ifelse(number_transactions != sum(isLocal),1,0)
  )

## TEST
transactions_test$isLocal = as.numeric(transactions_test$MERCHANT_COUNTRY_XCD == "DP")

transactions_test_grouped <- setNames(data.table(matrix(nrow = uniqueN(transactions_test$ID_CPTE), ncol = 1)), c("ID_CPTE"))
transactions_test_grouped$ID_CPTE = unique(transactions_test$ID_CPTE)

transactions_test_grouped = transactions_test %>%
  group_by(ID_CPTE) %>%
  summarise(
    number_transactions = n(), 
    traveller_ind = ifelse(number_transactions != sum(isLocal),1,0)
  )

##====================================
## Payment features
##====================================

## TRAIN
payments_train_grouped <- setNames(data.table(matrix(nrow = uniqueN(payments_train$ID_CPTE), ncol = 1)), c("ID_CPTE"))
payments_train_grouped$ID_CPTE = unique(payments_train$ID_CPTE)

payments_train_grouped = payments_train %>%
  group_by(ID_CPTE) %>%
  summarise(mean_payment = mean(TRANSACTION_AMT),
            number_payments = n(),
            max_payment = max(TRANSACTION_AMT),
            min_payment = min(TRANSACTION_AMT),
            median_payment = median(TRANSACTION_AMT),
            reversedPayment = sum(PAYMENT_REVERSAL_XFLG == "N")>=1,
            noPayments = sum(PAYMENT_REVERSAL_XFLG == "")>=1)

payments_train_grouped[is.na(payments_train_grouped)] <- 0

## TEST
payments_test_grouped <- setNames(data.table(matrix(nrow = uniqueN(payments_test$ID_CPTE), ncol = 1)), c("ID_CPTE"))
payments_test_grouped$ID_CPTE = unique(payments_test$ID_CPTE)

payments_test_grouped = payments_test %>%
  group_by(ID_CPTE) %>%
  summarise(mean_payment = mean(TRANSACTION_AMT),
            number_payments = n(),
            max_payment = max(TRANSACTION_AMT),
            min_payment = min(TRANSACTION_AMT),
            median_payment = median(TRANSACTION_AMT),
            reversedPayment = sum(PAYMENT_REVERSAL_XFLG == "N")>=1,
            noPayments = sum(PAYMENT_REVERSAL_XFLG == "")>=1)

payments_test_grouped[is.na(payments_test_grouped)] <- 0

##====================================
## Summary table (For models)
##====================================

temp1_train = merge(payments_train_grouped,billing_train_grouped, by = "ID_CPTE") 
temp2_train = merge(temp1_train,performance_train, by = "ID_CPTE") 
train = merge(temp2_train, transactions_train_grouped, by = "ID_CPTE", all.x = TRUE)
rm(temp1_train,temp2_train)

temp1_test = merge(payments_test_grouped,transactions_test_grouped, by = "ID_CPTE") 
temp2_test = merge(temp1_test,performance_test, by = "ID_CPTE") 
test = merge(temp2_test, transactions_test_grouped, by = "ID_CPTE", all.x = TRUE)
rm(temp1_test,temp2_test)

#--------------------------------------------------------#
# 2. Modeling                                            
# ______________________________________________________ 
#                                                        
#   - Decision trees/rpart
#   - Logistic Regression (GLM)                          
#   - XGBoost
#   - SVM
#--------------------------------------------------------#

set.seed(8) # Patrice bring us luck

## RPART
predictors <- c("number_transactions") # For testing
formula <- as.formula(paste("Default ~", paste(predictors, collapse = "+")))

rpart_classifier <- rpart(Default ~ ., data = train, method = "class",
                     control = rpart.control(minsplit = 100, maxdepth = 10, cp=0.001))

pred_rpart <- predict(rpart_classifier, train, type = "class")

confusionMatrix(pred_rpart, train$Default)

## LOGISTIC



## XGB




## SVM




#--------------------------------------------------------#
# 3. Submission                                          
# ______________________________________________________ 
#                                                        
#   - Create submission .csv
#--------------------------------------------------------#
