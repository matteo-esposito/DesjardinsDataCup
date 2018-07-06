
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
packages <- c("data.table", "rpart", "caret", "gbm", "mgcv", "ggplot2", "dplyr","Hmisc", "xgboost", "corrplot")
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
#   - NA/bad formatting checks                           
#   - Feature engineering
#   - Table merges
#--------------------------------------------------------#



#--------------------------------------------------------#
# 2. Variable Selection                                  #
# ______________________________________________________ #
#                                                        #
#   - detail 1                                           #
#   - detail 2                                           #
#--------------------------------------------------------#


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
