
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
setwd("~/Github/DesjardinsDataCup")

## Package loading
packages <- c("data.table", "rpart", "caret", "gbm", "mgcv", "ggplot2", "plyr", "dplyr", "Hmisc", "xgboost", "corrplot")
sapply(packages, require, character.only = T)

## Load data
facturation_train <- as.data.table(read.csv("~/Github/DesjardinsDataCup/Data/facturation_train.csv")) 
facturation_test <- as.data.table(read.csv("~/Github/DesjardinsDataCup/Data/facturation_test.csv")) 
paiements_train <- as.data.table(read.csv("~/Github/DesjardinsDataCup/Data/paiements_train.csv")) 
paiements_test <- as.data.table(read.csv("~/Github/DesjardinsDataCup/Data/paiements_test.csv")) 
performance_train <- as.data.table(read.csv("~/Github/DesjardinsDataCup/Data/performance_train.csv")) 
performance_test <- as.data.table(read.csv("~/Github/DesjardinsDataCup/Data/performance_test.csv")) 
transactions_train <- as.data.table(read.csv("~/Github/DesjardinsDataCup/Data/transactions_train.csv")) 
transactions_test <- as.data.table(read.csv("~/Github/DesjardinsDataCup/Data/transactions_test.csv")) 

sampSol <- as.data.table(read.csv("~/Github/DesjardinsDataCup/Data/sample_solution.csv")) 

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

NAcount(facturation_train)
NAcount(paiements_train)
NAcount(performance_train)
NAcount(transactions_train)

## Naive imputation right now, since group avg substitution not working
paiements_train$TRANSACTION_AMT <- ifelse(is.na(paiements_train$TRANSACTION_AMT), mean(paiements_train$TRANSACTION_AMT, na.rm=T), paiements_train$TRANSACTION_AMT)
sum(is.na(paiements_train$TRANSACTION_AMT)) 

# Impute with mean by group (ID)
#paiements_train_2 <- paiements_train %>% 
#                    group_by(ID_CPTE) %>% 
#                    mutate(TRANSACTION_AMT= ifelse(is.na(TRANSACTION_AMT), mean(TRANSACTION_AMT, na.rm=TRUE), TRANSACTION_AMT))

#--------------------------------------------------------#
# 2. Data Exploration                                    #
# ______________________________________________________ #
#                                                        #
#   - detail 1                                           #
#   - detail 2                                           #
#--------------------------------------------------------#

## Thanks Tony ;)
payments_train_grouped = paiements_train %>%
  group_by(ID_CPTE) %>%
  summarize(mean_payment = mean(TRANSACTION_AMT),
            #number_payments = n(),
            max_payment = max(TRANSACTION_AMT),
            min_payment = min(TRANSACTION_AMT),
            median_payment = median(TRANSACTION_AMT),
            reversedPayment = sum(PAYMENT_REVERSAL_XFLG == "N")>=1,
            noPayments = sum(PAYMENT_REVERSAL_XFLG == "")>=1)

payments_train_grouped[is.na(payments_train_grouped)] <- 0

billing_train_grouped = facturation_train %>%
  group_by(ID_CPTE) %>%
  summarize(
    mean_balance = mean(CurrentTotalBalance),
    mean_cash_balance = mean(CashBalance),
    max_balance = max(CurrentTotalBalance),
    max_cash_balance = max(CashBalance)
  )

transactions_train_grouped = transactions_train %>%
  group_by(ID_CPTE) %>%
  summarize(
    number_transactions = n()
  )

#--------------------------------------------------------#
# 3. Modeling                                            #
# ______________________________________________________ #
#                                                        #
#   - detail 1                                           #
#   - detail 2                                           #
#--------------------------------------------------------#


#--------------------------------------------------------#
# 4. Submission                                          #
# ______________________________________________________ #
#                                                        #
#   - detail 1                                           #
#   - detail 2                                           #
#--------------------------------------------------------#
