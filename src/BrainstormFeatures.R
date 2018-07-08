## Brainstorm
## Features

## Case:  75780289, 59776762

#################
## Facturation ##
#################

Spending = CurrentTotalBalance - CashBalance
SpendingOL = Spending/CreditLimit

OverLimitIndicator = (CurrentTotalBalance > CreditLimit)
TotalBalanceOL = (CurrentTotalBalance/CreditLimit)
CashBalanceIndictor = CashBalance > 0
CashBalanceOL = CashBalance/CreditLimit

# CashBalance ~ Cash advance
# Checking for credit limit change (pseudocode)
CreditLimitSign = if (CreditLimit[i] > CreditLimit[i-1]) {
    sign = 1
  } else if (CreditLimit[i] < CreditLimit[i-1]) {
    sign = -1
  } else {
    sign = 0
  }

Num_CMP <- DelqCycle # basic descriptive statistics (max, count)
Max_Num_CMP <- billing_train[ , max(DelqCycle), by = ID_CPTE]
Count_Num_CMP <- billing_trin[, count(DelqCycle), by = I_CPTE]
ConsMPSign <- if (NumConsecutiveMissedPayments[i] > NumConsecutiveMissedPayments[i-1]) {
  sign = 1
} else if (NumConsecutiveMissedPayments[i] < NumConsecutiveMissedPayments[i-1]) {
  sign = -1
} else {
  sign = 0
}

##################
## Transactions ##
##################

IsLocal = (MERCHANT_COUNTRY_XCD = "DP")

# TravellerIndicator = count(MERCHANT_COUNTRY_XCD = "DP") by ID over 
# Percentage of each level for all categorical variables by ID


#####################
## Facturation/PMT ##
#####################

# Link (SAS-style) the payments to the currenttotalbalance and observe whether 
# or not there is any remaining balance in the future months.






























