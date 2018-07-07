## Brainstorm
## Features

## Case:  75780289, 59776762

## Facturation >>
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

Spending = CurrentTotalBalance - CashBalance
SpendingOL = Spending/CreditLimit

NumConsecutiveMissedPayments <- DelqCycle # basic descriptive statistics (max, count)
ConsMPSign <- if (NumConsecutiveMissedPayments[i] > NumConsecutiveMissedPayments[i-1]) {
  sign = 1
} else if (NumConsecutiveMissedPayments[i] < NumConsecutiveMissedPayments[i-1]) {
  sign = -1
} else {
  sign = 0
}

