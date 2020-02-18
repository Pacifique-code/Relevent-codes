setwd("~/Documents/PhD_Research/phd_research/donnees/CSV_Data")
Data = read.csv("Kirundo_Precipitation.csv")
rai(as.daily(Data,na.value = NA),granularity = "m")
rain = read.csv("Rainfalltotal.csv")
                                          ##Test for homogeneity
                                          
                                          ## Gamma distribution
                                          
library(fitdistrplus)
fit.gamma <- fitdist(rain$Muyinga, distr = "gamma", method = "mle")
plot(fit.gamma)
summary(fit.gamma)
                                          
                                          
                                          #### Trend analysis and Homogeneity
                                          
                                          #Converting data to ts format
                                          
Kirundo_ts = ts(rain[,2],frequency=1,start=c(1986,1))
Muyinga_ts = ts(rain[,3],frequency=1,start=c(1986,1))


all_dat = rain
yr_frst <- 1986
yr_last <- 2017
wave <- all_dat[all_dat[,"Year"]>=yr_frst & all_dat[,"Year"]<=yr_last,]
Kirundo_ts = ts(wave[,2],frequency=1,start=c(1992,1))
d = dwt(Kirundo_ts, wf="la8", n.levels=4, boundary="periodic")
dwt.nondyadic(Kirundo_ts)

                                          
                                          ## Mann Kendal test for trend analysis
library(trend)
mk.test(Kirundo_ts)
mk.test(Muyinga_ts)
                                          
                                          ## Sen’s slope for Magnitude of trend
                                          
sens.slope(Kirundo_ts)
sens.slope(Muyinga_ts)
                                                      ##  Change-point detection
                                          # Pettitt’s test
                                          
pettitt.test(Kirundo_ts)
                                          
                                          
                                          #Buishand Range Test
br.test(Kirundo_ts)
                                          
                                          # Buishand U Test
bu.test(Kirundo_ts)
plot(bu.test(Kirundo_ts))
                                          
                                          
                                          ## Standard Normal Homogeinity Test
snh.test(Kirundo_ts)
                                          
                                          
                                                           ### Ploting time series data
                                          
temps=time(rain$Year)
                                          
reglin=lm(rain[,2]~temps)
resi.mco=residuals(reglin)
                                          
ychap=fitted(reglin)
                                         
v12=c(var(rain[,2]),var(resi.mco))
                                          
plot.ts(rain[,2],las=1,ylab="precipitation",xlab="Time")
                                          
abline(coef=coef(reglin))
                                          
summary(reglin)
                                          
                                          ###### Monthly data Kirundo
   #-----------------------------------                                         
### Setting the directory
   # ----------------------------------
setwd("~/Documents/PhD_Research/phd_research/donnees/CSV_Data")
                                          
par(mfrow=c(1,1))
                                          
Monthly_kirundo = read.csv("Monthly_Kir.csv")
                                         
   ##############################


#######################
Monthly_kirundo
                                         
 Kirundo=NULL
                                          
 for (i in 1:32)
                                          
   {
                                            
   k=Monthly_kirundo[i,]
                                            
   Kirundo =c(Kirundo,as.numeric(k) )
                                          }
Kirundo
Monthly_kir=ts(Kirundo, start=c(1986,1), frequency=12)
Monthly_kir

monthplot(Monthly_kir,ylab="Rainfall (mm)",xlab = "Months",main="",cex.main=2)

library(spi)
spi(2,"Rainfalltotal.csv", 1986,2017)
                                          
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          # Monthly data Muyinga
                                          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Monthly_muyinga = read.csv("Monthly_Muy.csv")

##############################


#######################
Monthly_muyinga

Muyinga=NULL

for (i in 1:32)
   
{
   
   k=Monthly_muyinga[i,]
   
   Muyinga =c(Muyinga,as.numeric(k) )
}
Muyinga
Monthly_Muy=ts(Muyinga, start=c(1986,1), frequency=12)                                          
             












                              ##########
                                          
 A = read.csv("Seasonal.csv")
#lm = lm(A[,2]~A[,3], data=A)
ts = ts(A[,2],frequency=1,start=c(1986,1))
ts2 = ts(A[,3],frequency=1,start=c(1986,1))
temps=time(ts)
reglin=lm(ts~temps)
reglin2=lm(ts2~temps)
resi.mco=residuals(reglin)
ychap=fitted(reglin)
v12=c(var(ts),var(resi.mco))
plot.ts(ts, type="o", col="black", pch="o", lty=2,las=1,ylab="Rainfall (mm)",xlab="Year",main="Muyinga")
abline(coef=coef(reglin))
points(ts2,type="o", col="black", pch="o", lty=1,las=1,ylab="Rainfall (mm)",xlab="Year")
abline(coef=coef(reglin2))
legend("topright",inset=c(0.02,0.04), legend = c("A", "B"),lty=c(2,1), lwd=c(1,2),
      cex=0.4,col=c("black","black"))
                                          ##################
ts4 = ts(A[,4],frequency=1,start=c(1986,1))
ts5 = ts(A[,5],frequency=1,start=c(1986,1))
temps=time(ts)
reglin4=lm(ts4~temps)
reglin5=lm(ts5~temps)
resi.mco=residuals(reglin3)
ychap=fitted(reglin4)
v12=c(var(ts),var(resi.mco))
plot.ts(ts4, type="o", col="black", pch="o", lty=2,las=1,ylim = c(50, 750),ylab="Rainfall (mm)",xlab="Year",main="Kirundo")
abline(coef=coef(reglin4))
points(ts5,type="o", col="black", pch="o", lty=1,las=1,ylab="Rainfall (mm)",xlab="Year")
abline(coef=coef(reglin5))
legend("topright",inset=c(0.02,0.04), legend = c("A", "B"),lty=c(2,1), lwd=c(1,2),
            cex=0.8,col=c("black","black"))
                                          
                                          ########
ts6 = ts(A[,6],frequency=1,start=c(1986,1))
ts7 = ts(A[,7],frequency=1,start=c(1986,1))
temps=time(ts)
reglin6=lm(ts6~temps)
reglin7=lm(ts7~temps)
plot.ts(ts2, type="o", col="black", pch="o", lty=2,las=1,ylim = c(100, 1200),ylab="Rainfall (mm)",xlab="Year",main="Kirundo")
abline(coef=coef(reglin2))
points(ts4,type="o", col="black", pch="o", lty=1,las=1,ylab="Rainfall (mm)",xlab="Year")
abline(coef=coef(reglin4))
points(ts5,type="o", col="black", pch="o", lty=1,las=1,ylab="Rainfall (mm)",xlab="Year")
abline(coef=coef(reglin4))
legend("topright",inset=c(0.02,0.04), legend = c("A", "B"),lty=c(2,1), lwd=c(1,2),
                          cex=0.8,col=c("black","black"))
                                          ##### Plots##
                                          ## Muyinga, season A and B nad annual
par(mar=c(8,6,2,4))
cex <- 2
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
shapes = c(16, 4, 23) 
plot.ts(ts, type="o",col="black", pch = 16,lty=2,las=1,ylim = c(100, 1600),
         ylab="Rainfall (mm)",xlab=" ",main="Muyinga",bty='l',las = 0,lwd = 3)
abline(coef=coef(reglin),lty=2,lwd = 3)
points(ts2,type="o", col="black", lty=1,las=1,lwd = 3, pch = 4,ylab="Rainfall (mm)",xlab="Year")
abline(coef=coef(reglin2),lwd = 3)
points(ts7,type="o", col="black", lty=2,las=2 ,lwd = 3,pch = 23,ylab="Rainfall (mm)",xlab="Year")
abline(coef=coef(reglin7),lty=2,lwd = 3)
#legend("top",inset=c(-1,-1), legend = c("Season A", "Season B", "Annual"),lty=c(2,1,2), lwd=c(1,1,2),
# cex=1.2,col=c("black","black","black"),xpd=F, horiz=T, bty="n",pch=c(1,2))
                                          
legend(x = 2004, y = -75, xpd = TRUE,xjust = .5, yjust = 1, horiz = TRUE,
legend = c("Season A", "Season B", "Annual"),lty=c(2,1,2), lwd=c(3,3,3),
      merge = TRUE,  box.lty = 0,cex=1.5,pch = shapes)
             ## Kirundo, season A and B nad annual
par(las=0)
cex <- 2
par(mar=c(8,6,2,4))
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
plot.ts(ts4, type="o", col="black", pch= 16, lty=2,lwd = 3,ylim = c(100, 1600),ylab="Rainfall (mm)",
            xlab=" ",main="Kirundo",bty='l',las = 0)
abline(coef=coef(reglin4),lty=2,lwd = 3)
points(ts5,type="o", col="black", pch=4, lty=1,las=1,lwd = 3,ylab="Rainfall (mm)",xlab="Year")
abline(coef=coef(reglin5),lwd = 2)
points(ts6,type="o", col="black", pch = 23, lty=2,las=2 ,lwd = 3,ylab="Rainfall (mm)",xlab="Year")
abline(coef=coef(reglin6),lty=2,lwd = 3)
legend(x = 2004, y = -75, xpd = TRUE,xjust = .5, yjust = 1, horiz = TRUE,
legend = c("Season A", "Season B", "Annual"),lty=c(2,1,2), lwd=c(3,3,3),
      merge = TRUE,  box.lty = 0,cex=1.5,pch = shapes)
                                          ######
reglin8=lm(A[,2]~A[,9])
plot(A[,2]~A[,9])
abline(reglin8,col="red",lwd=3)
summary(reglin8)

#################################################""
# Computing standardised anomaly index SAI
###########################################
# Kirundo
library(hydroTSM)
library(tsbox)
Monthly_kirundo.xts <- ts_xts(Monthly_kirundo)
monthly2annual(Monthly_kirundo.xts, FUN=sum, na.rm=TRUE)

mySum <- function(Monthly_kir, mean(Monthly_kir)) {
   output <- ((Monthly_kir- mean(Monthly_kir))/sd(Monthly_kir))
   return(output)
}
output
plot(output)
dotchart(output)
plot(output,type="h",xlim=c(1986,2017),lwd=3,
     tck=0.02, ylim=c(-1.5,4), #tck>0 makes ticks inside the plot
     ylab="SAI",
     xlab="Time (Year)",col="red",
     main="Kirundo")
lines(output,type="h",
      lwd=3, tck=-0.02, col="blue")
####  Seasonal plot
library(seas)
library(MASS)
Monthly_kirundo = read.csv("Monthly_Kir1.csv")
seas.temp.plot(Monthly_kirundo)

library("lattice")
levelplot(Monthly_kir  ,xlab="X",
          main="")








                                          #####################################################################################################
                                          #### Variability of crop productivity 
                                          
                                          
B = read.csv("Crop_Variability.csv")
View(B)
                                          
                                          
                                          # Muyinga rainfaill
Muy_A = ts(B[,2],frequency=1,start=c(1996,1))
 Muy_B = ts(B[,3],frequency=1,start=c(1996,1))
                                          
                                          # Kirundo rainfall
                                          
Kirundo_A = ts(B[,4],frequency=1,start=c(1996,1))
Kirundo_B = ts(B[,5],frequency=1,start=c(1996,1))
                                          
                                          
                                          # Kirundo beans production
                                          
Kir_beans_A = ts(B[,8],frequency=1,start=c(1996,1))
mean(Kir_beans_A)
Kir_beans_B = ts(B[,9],frequency=1,start=c(1996,1))
mean(Kir_beans_B)
                                          
temps=time(Kir_beans_A)
reglin=lm(Kir_beans_A~temps)
reglin2=lm(Kir_beans_B~temps)
resi.mco=residuals(reglin)
ychap=fitted(reglin)
v12=c(var(ts),var(resi.mco))
plot.ts(Kir_beans_A, type="o", col="black", pch="o", lty=2,las=1,ylab="Rainfall (mm)",xlab="Year",main="Muyinga")
abline(coef=coef(reglin))
                                          
                                          
                                          # Kirundo maize production
                                          
 Kir_maize_A = ts(B[,12],frequency=1,start=c(1996,1))
 mean(Kir_maize_A)
Kir_maize_B = ts(B[,13],frequency=1,start=c(1996,1))
mean(Kir_maize_B)
                                          
temps=time(Kir_maize_A)
reglin3=lm(Kir_maize_A~temps)
reglin4=lm(Kir_maize_B~temps)
resi.mco=residuals(reglin3)
 ychap=fitted(reglin3)
                                          #v12=c(var(ts),var(resi.mco))
plot.ts(Kir_maize_A, type="o", col="black", pch="o", lty=2,las=1,ylab="Rainfall (mm)",xlab="Year",main="Kirundo")
abline(coef=coef(reglin3))
                                          
                                          # Muyinga production
                                          
 Muy_beans_A = ts(B[,10],frequency=1,start=c(1996,1))
 mean(Muy_beans_A)
Muy_beans_B = ts(B[,11],frequency=1,start=c(1996,1))
                                          
                                          
                                          Muy_maize_A = ts(B[,14],frequency=1,start=c(1996,1))
                                          mean(Muy_maize_A)
                                          Muy_maize_B = ts(B[,15],frequency=1,start=c(1996,1))
                                          mean(Muy_maize_B)
                                          
                                          
                                          panel.cor <- function(x, y, ...)
                                          {
                                            par(usr = c(0, 1, 0, 1))
                                            txt <- as.character(format(cor(x, y), digits=2))
                                            text(0.5, 0.5, txt, cex = 6* abs(cor(x, y)))
                                          }
                                          
                                          pairs(B[2:11], upper.panel=panel.cor)
                                          
                                          
                                          lm.out = lm(B[,2]~B[,14])
                                          
                                          plot(B[,2]~B[,14],main="",
                                               xlab="",
                                               lwd=3,cex.axis=1.2, ylab="")
                                          # Adding regression line to plot
                                          abline(lm.out,col="red",lwd=3)
                                          summary(lm.out)
                                          
                                          lm.out = lm(Muy_A~Muy_beans_A)
                                          plot(Muy_A~Muy_beans_A,main="",
                                               xlab="",     lwd=3,cex.axis=1.2, ylab="")
                                          
                                          
                                          ############################
                                          
                                          ############################
                                          
                                          library(readxl)
onset_cessation_data <- read_excel("~/Documents/PhD_Research/phd_research/Donnees/onset_cessation_data.xlsx", 
                                                                             na = "empty")
#View(onset_cessation_data)
                                          
Data = onset_cessation_data
Data
plot(Data)
                                          
                                          #### Kirundo
                                          
panel.cor <- function(x, y, ...)
{
par(usr = c(0, 1, 0, 1))
txt <- as.character(format(cor(x, y), digits=2))
text(0.5, 0.5, txt, cex = 6* abs(cor(x, y)))
}
                                          
                                          pairs(Data[1:8], upper.panel=panel.cor)
                                          
                                          
                                          lm.out = lm(Data$Kir_onset~Data$Kir_duration)
                                          
                                          plot(Data$Kir_onset~Data$Kir_duration,main="",
                                               xlab="",
                                               lwd=3,cex.axis=1.2, ylab="")
                                          abline(lm.out,col="red",lwd=3)
                                          summary(lm.out)
                                          ## Onset~annual total
                                          lm.out = lm(Data$Kir_Annual~Data$Kir_onset)
                                          
                                          plot(Data$Kir_Annual~Data$Kir_onset,main="",
                                               xlab="Onset date (days from 1st July)",
                                               lwd=1.5,cex.axis=1.2, ylab="Annual precipitation (mm)")
                                          abline(lm.out,col="black",lwd=2)
                                          text(x=140, y= 1200, labels = expression('R'^2*'='*'0.1052'))
                                          text(x=75, y= 800, labels = expression('Kirundo'),cex = 1.5)
                                          summary(lm.out)
                                          
                                          # Onset~duration
                                          lm.out2 = lm(Data$Kir_duration~Data$Kir_onset)
                                          
                                          plot(Data$Kir_duration~Data$Kir_onset,main="",
                                               xlab="Onset date (days from 1st July)",ylab = "Duration of rainy season (days)",
                                               lwd=1.5,cex.axis=1.2)
                                          abline(lm.out2,col="black",lwd=2)
                                          text(x=140, y= 260, labels = expression('R'^2*'='*'0.6709'))
                                          text(x=80, y= 180, labels = expression('Kirundo'),cex = 1.5)
                                          summary(lm.out2)
                                          
                                          
                                          ### Muyinga
                                          
                                          # Onset~duration
                                          lm.out = lm(Data$Muy_duration~Data$Muy_onset)
                                          
                                          plot(Data$Muy_duration~Data$Muy_onset,main="",
                                               xlab="Onset date (days from 1st July)",ylab = "Duration of rainy season (days)",
                                               lwd=1.5,cex.axis=1.2)
                                          text(x=140, y= 280, labels = expression('R'^2*'='*'0.80'))
                                          text(x=80, y= 180, labels = expression('Muyinga'),cex = 1.5)    
                                          abline(lm.out,col="black",lwd=2)
                                          
summary(lm.out)



                                                # onset~annual  
lm.out2 = lm(Data$Muy_Annual~Data$Muy_onset)
                                          
plot(Data$Muy_Annual~Data$Muy_onset,main="",
xlab="Onset date (days from 1st July)",
lwd=1.5,cex.axis=1.2, ylab="Annual precipitation (mm)",
pch=20,ylim=c(500,1600))

text(x=140, y= 1500, labels = expression('R'^2*'='*'0.009'))
text(x=80, y= 600, labels = expression('Muyinga'),cex = 1.5) 
abline(lm.out2,col="black",lwd=2)
summary(lm.out2)

###################""  Onset and Duration analysis
setwd("~/Bureau")
Data = read.csv("onset_cessation_data.csv")
Data
##################################""" CV 
mean(Data$Kir_onset)
sd(Data$Kir_onset)
CV = sd(Data$Kir_onset)/sd(Data$Kir_onset)

a = mean(Data$Kir_cessation)
b = sd(Data$Kir_cessation)
CV = b/a

a = mean(Data$Kir_duration)
b = sd(Data$Kir_duration)
b/a
#######
mean(Data$Muy_onset)
sd(Data$Muy_onset)
CV = sd(Data$Muy_onset)/sd(Data$Muy_onset)

a = mean(Data$Muy_cessation)
b = sd(Data$Muy_cessation)
CV = b/a

a = mean(Data$Muy_duration)
b = sd(Data$Muy_duration)
b/a



²#######################################"

library("ggcorrplot")
my_data <- Data
corr <- round(cor(my_data), 2)
summary(corr)
ggcorrplot(corr, p.mat = cor_pmat(my_data),
           hc.order = TRUE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)
library(GGally)
library(ggplot2)
ggpairs(Data)+ theme_bw()
                            

# load a time series of NDVI (normalized difference vegetation index)
data(ndvi)
plot(ndvi)

# calculate trend (default method: trend calculated based on annual aggregated data)
trd <- Trend(ndvi)
trd
plot(trd)

# an important parameter is mosum.pval: if the p-value is changed to 1, 
# breakpoints can be detected in the time series regardless if 
# there is significant structural change
trd <- Trend(ndvi, mosum.pval=1)
trd
plot(trd)

# calculate trend based on modelling the seasonal cycle
trd <- Trend(ndvi, method="STM")
trd
plot(trd)

# calculate trend based on removal of the seasonal cycle
trd <- Trend(ndvi, method="SeasonalAdjusted", funSeasonalCycle=MeanSeasonalCycle)
plot(trd)
lines(trd$adjusted, col="green")
trd

# modify maximal number of breakpoints
trd <- Trend(ndvi, method="SeasonalAdjusted", breaks=1)
plot(trd)
trd

# use quantile regression
trd <- Trend(ndvi, method="RQ")
plot(trd)
trd


#####   PLOTTING THE PCI

# LOADING DATA KIRUNO
library(precintcon)
monthly_pci = read.csv("PCI_monthly_Kir.csv")
monthly_pci =read.data("PCI_monthly_Kir.csv", sep = ",", dec = ".", header = TRUE, na.value = NA)
par(mfrow=c(2,2))
#View(monthly_pci)
PCI1 = pci(monthly_pci)
PCI1 = rai(monthly_pci)


pplot.rai(PCI1$rai)
pplot.pci(PCI1, ylab = "Precipitation Concentration Index (Kirundo)",cex = 3)
pplot.pci(PCI1, ylab = "Precipitation Concentration Index (Kirundo)",cex = 3)


pplot.pci(PCI1, xlab = "Years", ylab = "Precipitation Concentration Index (Kirundo)", legend = NULL, 
          fontsize = 16, axis.text.color = "black", export = FALSE, 
          export.name = "pci_plot.png", width = 20, height = 20, units = "cm")

pci.seasonal(PCI1)

# LOADING DATA for Muyinga
Muy_monthly_pci =read.data("PCI_monthly_Muy).csv", sep = ",", dec = ".", header = TRUE, na.value = NA)

PCI2 = pci(Muy_monthly_pci)

pplot.pci(PCI2, xlab = "Years", ylab = "Precipitation Concentration Index (Muyinga)", legend = NULL, 
          fontsize = 18, axis.text.color = "black", export = FALSE, 
          export.name = "pci_plot.png", width = 20, height = 20, units = "cm")
p = pplot.pci(PCI2, ylab = "Precipitation Concentration Index (Muyinga)")

#summary(PCI)
mean(PCI$pci)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
min(PCI$pci) 
max(PCI$pci) 

# Decadal variations
monthly_pci2 =read.data("PCI_monthly_Muy).csv", sep = ",", dec = ".", header = TRUE, na.value = NA)
attach(monthly_pci2)
monthly_pci[1:120,]
PCI1 = pci(monthly_pci[1:120,])

dec1 = monthly_pci2[1:120,]
dec2 = monthly_pci2[121:240,]
dec3 = monthly_pci2[241:384,]
PCI1 = pci(dec1)
PCI2 = pci(dec2)
PCI3 = pci(dec3)

''' > mean(PCI1$pci)
[1] 13.09099
> mean(PCI2$pci)
[1] 12.92184
> mean(PCI3$pci)
[1] 13.06684
> min(PCI1$pci)
[1] 9.631839
> max(PCI1$pci)
[1] 15.4102
> min(PCI2$pci)
[1] 10.59421
> max(PCI2$pci)
[1] 14.63653
> min(PCI3$pci)
[1] 11.20732
> max(PCI3$pci)
[1] 16.21613
'''


'''> mean(PCI1$pci)
[1] 13.19766
> mean(PCI2$pci)
[1] 13.93096
> mean(PCI3$pci)
[1] 12.98003
> min(PCI1$pci)
[1] 11.34831
> max(PCI1$pci)
[1] 15.02953
> min(PCI2$pci)
[1] 11.94145
> max(PCI2$pci)
[1] 17.16802
> min(PCI3$pci)
[1] 10.8198
> max(PCI3$pci)
[1] 15.60229
 
'''
