
# Desjardins Lab DataCup
# June/July 2018

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
              "Hmisc", "xgboost", "corrplot", "e1071")
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
#--------------------------------------------------------#

# Inspect data

## NA counter function
NAcount <- function(df){
  sapply(df,function(x) table(is.na(x)))
}

NAcount(billing_train)
NAcount(payments_train)
NAcount(performance_train)
NAcount(transactions_train)

## Naive global mean imputation right now, since group avg substitution not working
payments_train$TRANSACTION_AMT <- ifelse(is.na(payments_train$TRANSACTION_AMT), mean(payments_train$TRANSACTION_AMT, na.rm=T), payments_train$TRANSACTION_AMT)
sum(is.na(payments_train$TRANSACTION_AMT)) 

# Impute with mean by group (ID)
#paiements_train_2 <- paiements_train %>% 
#                    group_by(ID_CPTE) %>% 
#                    mutate(TRANSACTION_AMT= ifelse(is.na(TRANSACTION_AMT), mean(TRANSACTION_AMT, na.rm=TRUE), TRANSACTION_AMT))

## Billing features (OL = Over limit)
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
billing_train_grouped <- setNames(data.table(matrix(nrow = 11900, ncol = 1)), c("ID_CPTE"))
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
    count_num_cmp = sum(DelqCycle_ind)
  )

## Transaction features
transactions_train$isLocal = as.numeric(transactions_train$MERCHANT_COUNTRY_XCD == "DP")




#--------------------------------------------------------#
# 2. Modeling                                            
# ______________________________________________________ 
#                                                        
#   - Logistic Regression (GLM)                          
#   - XGBoost
#   - SVM
#--------------------------------------------------------#


#--------------------------------------------------------#
# 3. Submission                                          
# ______________________________________________________ 
#                                                        
#   - Create submission .csv
#--------------------------------------------------------#
