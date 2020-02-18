setwd("~/Bureau")

Data = read.csv("DAILY_CHIRP_KIR_MUY_AGREG2.csv", na = "empty")
#data2 = as.numeric(paste(data))
mat = as.matrix((data[,2:7]))


library("ggpubr")

library("phonTools")
library("basicTrendline")
library("corrplot")
library("na.tools")
library("dplyr")
library("R.utils")



trendline(Data$KIR_PREC, Data$OBS_KIR, model = "line2P", plot = TRUE, linecolor = "red", lty = 1, lwd = 1, summary = TRUE)
trendline(Data$ERA5_KIR, Data$OBS_KIR, model = "line2P", plot = TRUE, linecolor = "red", lty = 1, lwd = 1, summary = TRUE)
trendline(Data$MUY_PREC, Data$OBS_MUY, model = "line2P", plot = TRUE, linecolor = "red", lty = 1, lwd = 1, summary = TRUE)
trendline(Data$ERA5_MUY, Data$OBS_MUY, model = "line2P", plot = TRUE, linecolor = "red", lty = 1, lwd = 1, summary = TRUE)
trendline(Data$KIR_PREC, Data$ERA5_KIR, model = "line2P", plot = TRUE, linecolor = "red", lty = 1, lwd = 1, summary = TRUE)
