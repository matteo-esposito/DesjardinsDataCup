library(dplyr)  
library(plotly)
library(corrplot)


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
            reversedPayment = sum(PAYMENT_REVERSAL_XFLG == "N")>=1,
            noPayments = sum(PAYMENT_REVERSAL_XFLG == "")>=1)

payments_train_grouped[is.na(payments_train_grouped)] <- 0

billing_train_grouped = billing_train %>%
  group_by(ID_CPTE) %>%
  summarize(
    mean_balance = mean(CurrentTotalBalance),
    mean_cash_balance = mean(CashBalance),
    max_balance = max(CurrentTotalBalance),
    max_cash_balance = max(CashBalance),
    # mostRecentStatement = max(StatementDate), 
    # mostRecentBalance = CurrentTotalBalance[which.max(StatementDate)],
    delq = sum(DelqCycle)>=1
  )

transactions_train_grouped = payments_transactions %>%
  group_by(ID_CPTE) %>%
  summarize(
    number_transactions = n()
  )




train = merge(payments_train_grouped,performance_train, by = "ID_CPTE") 


