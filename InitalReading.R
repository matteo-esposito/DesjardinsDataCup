library(dplyr)  
library(caret)
library(gbm)
library(randomForest)
library(doParallel)
library(nnet)
library(MASS)
library(olsrr)
library(plyr)
library(pROC)
library(plotly)
library(dplyr)
library(corrplot)
library(cvTools)

path = "C:\\Users\\Tony\\OneDrive\\Actuary\\Data Science\\DataCup"

transactions_train = read.csv(paste0(path,"\\transactions_train.csv"))
payments_train = read.csv(paste0(path,"\\paiements_train.csv"))
performance_train = read.csv(paste0(path,"\\performance_train.csv"))
billing_train = read.csv(paste0(path,"\\facturation_train.csv"))

summary(performance_train)
summary(transactions_train)
summary(payments_train)
summary(billing_train)


payments_train_grouped = payments_train %>%
  group_by(ID_CPTE) %>%
  summarize(mean_payment = mean(TRANSACTION_AMT),
            number_payments = n(),
            max_payment = max(TRANSACTION_AMT),
            min_payment = min(TRANSACTION_AMT),
            median_payment = median(TRANSACTION_AMT),
            reversedPayment = sum(PAYMENT_REVERSAL_XFLG == "N"),
            noPayments = sum(PAYMENT_REVERSAL_XFLG == ""))


fullDf = merge(payments_train_grouped,performance_train, by = "ID_CPTE") 

