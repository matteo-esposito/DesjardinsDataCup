#global settings

path <- "~/Desktop/DesjardinsDataCup/data/"


# loading packages 

library(dplyr)
library(psych)



#loading data (train)

transactions_train = read.csv(paste0(path,"transactions_train.csv"))
payments_train = read.csv(paste0(path,"paiements_train.csv"))
performance_train = read.csv(paste0(path,"performance_train.csv"))
billing_train = read.csv(paste0(path,"facturation_train.csv"))

#loading data (test)

transactions_test = read.csv(paste0(path,"transactions_test.csv"))
payments_test = read.csv(paste0(path,"paiements_test.csv"))
performance_test = read.csv(paste0(path,"performance_test.csv"))
billing_test = read.csv(paste0(path,"facturation_test.csv"))


############################
#  DATA EXPLORATION
############################

summary(performance_train)
performance_train %>% group_by(PERIODID_MY) %>% summarize (mean_default=mean(Default))
describe.by(performance_train$Default,performance_train$PERIODID_MY)

#avg default rate ~.2 and stable across years, 2015 slighty higher..


summary(transactions_train)
transactions_train %>% group_by(MERCHANT_CATEGORY_XCD) %>% summarize (count=n()) %>% arrange(desc(count))
transactions_train %>% group_by(MERCHANT_CITY_NAME) %>% summarize (count=n()) %>% arrange(desc(count))

# could do something with % transaction in category x

summary(payments_train)

#could look at payments vs amount due 

summary(billing_train)


############################
#  DATA PREPARATION
############################

#performance
performance_train$PERIODID_MY_year = substr(performance_train$PERIODID_MY,1,4)

#billing
billing_train$TB_limit_perc = billing_train$CurrentTotalBalance/billing_train$CreditLimit
billing_train$TB_limit_ind = as.numeric(billing_train$CurrentTotalBalance > billing_train$CreditLimit)

billing_train$CB_limit_perc = billing_train$CashBalance/billing_train$CreditLimit
billing_train$CB_limit_ind = as.numeric(billing_train$CashBalance > billing_train$CreditLimit)
billing_train$CB_ind = as.numeric(billing_train$CashBalance > 0 )

billing_train$B_wo_cash = billing_train$CurrentTotalBalance - billing_train$CashBalance
billing_train$B_limit_perc = billing_train$B_wo_cash/billing_train$CreditLimit
billing_train$B_limit_ind = as.numeric(billing_train$B_wo_cash > billing_train$CreditLimit)


