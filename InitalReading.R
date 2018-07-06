path = "C:\\Users\\Tony\\OneDrive\\Actuary\\Data Science\\DataCup"

transactions_train = read.csv(paste0(path,"\\transactions_train.csv"))
payments_train = read.csv(paste0(path,"\\paiements_train.csv"))
performance_train = read.csv(paste0(path,"\\performance_train.csv"))
billing_train = read.csv(paste0(path,"\\facturation_train.csv"))

summary(performance_train)
