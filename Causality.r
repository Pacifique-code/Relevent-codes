setwd("~/Documents/PhD_Research/phd_research/donnees/CSV_Data")

## Monthly rainfall
Monthly_kirundo = read.csv("Monthly_Kir.csv")
Monthly_muyinga = read.csv("Monthly_Muy.csv")
 # having the data with different variables that we need to test the Granger causality
Muyinga_data = read.csv("Combined_data_Muy.csv")


# Test for Granger Causality using grangertest  function of lmtest R-package 
# then, the library "lmtest" has to be loaded


grangertest(Muyinga_data[, 5], Muyinga_data[, 10], order = 3, na.action = na.omit)

