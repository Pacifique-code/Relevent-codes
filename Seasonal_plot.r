setwd("~/Documents/PhD_Research/phd_research/donnees/CSV_Data/Usefull data")

Data = read.csv("Monthly_Data_Obs_impute.csv")

# loading  the libreary to plot seasonal data
data = as.ts(Data[,4])
library(forecast)
ggseasonplot(as.ts(data), year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")



library(xts)
to.yearly(data,drop.time=TRUE)
mean(data)
data

gzs <- getCHIRPS("africa", tres = "monthly"
                 , begin = as.Date("1982-01-01"), end = as.Date("1983-12-31")
                 , dsn = file.path(getwd(), "data"))
