
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
billing_train <- as.data.table(read.csv(paste0(getwd(),"/Data/facturation_train.csv"))) # SUMMARY/eSTATEMENT OF WHAT YOU OWE ON CREDIT CARD by MONTH
billing_test <- as.data.table(read.csv(paste0(getwd(),"/Data/facturation_test.csv"))) 
payments_train <- as.data.table(read.csv(paste0(getwd(),"/Data/paiements_train.csv"))) # PAYING OFF CREDIT CARD
payments_test <- as.data.table(read.csv(paste0(getwd(),"/Data/paiements_test.csv"))) 
performance_train <- as.data.table(read.csv(paste0(getwd(),"/Data/performance_train.csv"))) # DEFAULT/NOT DEFAULT
performance_test <- as.data.table(read.csv(paste0(getwd(),"/Data/performance_test.csv"))) 
transactions_train <- as.data.table(read.csv(paste0(getwd(),"/Data/transactions_train.csv"))) # DAILY CREDIT CARD PURCHASES
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
    traveller_ind = ifelse(number_transactions != sum(isLocal),1,0),
    trx_type_mode = names(table(TRANSACTION_TYPE_XCD))[which.max(table(TRANSACTION_TYPE_XCD))],
    trx_cat_mode = names(table(TRANSACTION_CATEGORY_XCD))[which.max(table(TRANSACTION_CATEGORY_XCD))]
  )

## TEST
transactions_test$isLocal = as.numeric(transactions_test$MERCHANT_COUNTRY_XCD == "DP")

transactions_test_grouped <- setNames(data.table(matrix(nrow = uniqueN(transactions_test$ID_CPTE), ncol = 1)), c("ID_CPTE"))
transactions_test_grouped$ID_CPTE = unique(transactions_test$ID_CPTE)

transactions_test_grouped = transactions_test %>%
  group_by(ID_CPTE) %>%
  summarise(
    number_transactions = n(), 
    traveller_ind = ifelse(number_transactions != sum(isLocal),1,0),  
    trx_type_mode = names(table(TRANSACTION_TYPE_XCD))[which.max(table(TRANSACTION_TYPE_XCD))],
    trx_cat_mode = names(table(TRANSACTION_CATEGORY_XCD))[which.max(table(TRANSACTION_CATEGORY_XCD))]
  )

## Observe the number of occurrences of each pairing (modify variables if you want to try some combinations out yourself)
dt_inspection <- transactions_train[,.SD,.SDcols=c("ID_CPTE","TRANSACTION_CATEGORY_XCD", "TRANSACTION_TYPE_XCD")]
dt_pair_count <- merge(dt_inspection, train, by = "ID_CPTE", all.x = TRUE)
dt_pair_count$pair_count <- 1
aggregate(pair_count ~ TRANSACTION_CATEGORY_XCD+TRANSACTION_TYPE_XCD+Default,dt_pair_count, FUN = sum)

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

temp1_test = merge(payments_test_grouped,billing_test_grouped, by = "ID_CPTE") 
temp2_test = merge(temp1_test,performance_test, by = "ID_CPTE") 
test = merge(temp2_test, transactions_test_grouped, by = "ID_CPTE", all.x = TRUE)
rm(temp1_test,temp2_test)

##====================================
## Data Visualization
##====================================

## Use Tony's Shiny app

## Correlation Plot
dt_corr <- train[,!colnames(train) %in% c("PERIODID_MY", "number_transactions", "traveller_ind")]

## Convert all logical to integer
for (cn in colnames(dt_corr)){
  if (class(dt_corr[[cn]]) == "logical"){
    dt_corr[[cn]] <- as.numeric(dt_corr[[cn]])
  }
}

## Output corrplot
corrplot(cor(dt_corr), method = "circle", order = "alphabet", type = "lower")

##====================================
## Formula
##====================================

## Wanted predictors
## Since we're doing classification we don't care THAT much about multicollinearity
predictors_base <- c("count_num_cmp", "mean_payment", "credit_change", "TotalBalanceOL_perc_max", 
                "TotalBalanceOL_perc_mean", "TotalBalanceOL_ind", "SpendingOL_perc_max", 
                "SpendingOL_perc_mean", "SpendingOL_ind")

predictors_1 <- c("count_num_cmp", "mean_payment", "credit_change", "TotalBalanceOL_perc_max", 
                "TotalBalanceOL_perc_mean", "SpendingOL_perc_max", "SpendingOL_perc_mean")

## Creating formula for models post correlation analysis
formula <- as.formula(paste("Default ~", paste(predictors_base, collapse = "+")))

#--------------------------------------------------------#
# 2. Modeling                                            
# ______________________________________________________ 
#                    
#   - Splitting data for modeling
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
## Recursive Partitioning
##====================================

rpart_classifier <- rpart(formula, data = model_train, method = "class",
                     control = rpart.control(minsplit = 100, maxdepth = 10, cp=0.001))

pred_rpart <- predict(rpart_classifier, model_test, type = "class")

table(pred_rpart,model_test$Default)

##====================================
## Logistic Regression
##====================================

logreg_classifier <- glm(formula, family = binomial, data = model_train)

pred_logreg <- predict(logreg_classifier, newdata = model_test, type = "response")
pred_rounded_logreg <- ifelse(pred_logreg >= 0.5,1,0)
table(pred_rounded_logreg,model_test$Default)

model_test$pred1 = pred_rounded_logreg
model_test$pred2 = as.numeric(pred_rpart)-1

# For test
write.csv(model_test,paste0(getwd(),"/Submissions/dummy.csv"))

##====================================
## XGBoost
##====================================

train_for_xgb <- copy(train)
train_for_xgb$Default <- ifelse(train_for_xgb$Default == 0, "No", "Yes") # Need to make this a factor for xgb

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = 200, eta = c(0.01,0.05,0.1),
                        max_depth = c(2,4,6),gamma = 0,
                        colsample_bytree = 0.8,min_child_weight = 100, 
                        subsample = 0.8)

## Modifying formula after seeing the output of the first run.
predictors_xgb <- c("TotalBalanceOL_perc_max", "TotalBalanceOL_perc_mean", "count_num_cmp", "credit_change")
formula_xgb <- as.formula(paste0("Default ~", paste(predictors_xgb, collapse = "+")))

xgb_tune <-train(formula,
                 data=train_for_xgb,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose = T,
                 metric="merror",
                 nthread = 2)

## Visualize results
print(xgb_tune)
plot(xgb_tune)

##====================================
## Support Vector Machines
##====================================


#--------------------------------------------------------#
# 3. Submission                                          
# ______________________________________________________ 
#                                                        
#   - Create submission .csv
#--------------------------------------------------------#

final_pred <- predict(logreg_classifier, newdata = test, type = "response")
final_pred_rounded <- ifelse(final_pred >= 0.5,1,0)

submission <- data.frame(test$ID_CPTE, final_pred_rounded)
colnames(submission) = c("ID_CPTE", "Default")

write.csv(submission,paste0(getwd(),"/Submissions/Submission.csv"))








