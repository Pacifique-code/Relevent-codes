# Download netCDF file
# Library
#install.packages("ncdf")
library(ncdf4)
# 4 dimensions: lon,lat,level,time
nc_precip=ncdf4::nc_open("~/Downloads/precip.mon.mean.nc")
nc_precip
nc_precip$dim$lon$vals #output lon values 0.0->357.5
nc_precip$dim$lat$vals #output lat values 90->-90
nc_precip$dim$time$vals #output time values in GMT hours: 1297320, 1298064
nc_precip$dim$time$units
#[1] "hours since 1800-01-01 00:00:0.0"
#nc$dim$level$vals
Lon <- ncvar_get(nc_precip, "lon")
Lat <- ncvar_get(nc_precip, "lat")
Time<- ncvar_get(nc_precip, "time")
#Time is the same as nc$dim$time$vals
head(Time)
#[1] 1297320 1298064 1298760 1299504 1300224 1300968
library(chron)
Tymd<-month.day.year(Time[1]/24,c(month = 1, day = 1, year = 1800))
#c(month = 1, day = 1, year = 1800) is the reference time
Tymd
#$month
#[1] 1
#$day
#[1] 1
#$year
#[1] 1948
#1948-01-01
precnc_prec<- ncvar_get(nc_precip, "precip")
dim(precnc_prec)
#[1] 144 73 826, i.e., 826 months=1948-01 to 2016-10, 68 years 10 mons
#plot a 90S-90N temp along a meridional line at 180E
plot(seq(-90,90,length=72),precnc_prec[71,,1], type="l",
     xlab="Lat", ylab="Temp [oC]",
     main="90S-90N temperature [mm/day] along a meridional line at 35E:
     Jan 1948")

#Write the data as space-time matrix with a header
precst=matrix(0,nrow=10512,ncol=486)
temp=as.vector(precnc_prec[,,1])
head(temp)

for (i in 1:486) {precst[,i]=as.vector(precnc_prec[ , , i])}
dim(precst)
#[1] 10512
826
#Build lat and lon for 10512 spatial positions using rep
LAT=rep(Lat, 144)
LON=rep(Lon[1],73)
for (i in 2:144){LON=c(LON,rep(Lon[i],73))}
gpcpst=cbind(LAT, LON, precst)
dim(gpcpst)
#[1] 10512
828
#The first two columns are lat and lon. 826 mons: 1948.01-2016.10
#Convert the Julian day and hour into calendar mons for time
tm=month.day.year(Time/24, c(month = 1, day = 1, year = 1800))
tm1=paste(tm$year,"-",tm$month)
#tm1=data.frame(tm1)
tm2=c("Lat","Lon",tm1)
colnames(gpcpst) <- tm2
setwd("/media/pacifique/74C241CEC24194F0/PhD_Research/EOFs")
#setwd routes the desired csv file to a given directory
write.csv(gpcpst,file="ncepJan1948_Oct2016.csv")


monJ=seq(1,486,12) #Select each January
gpcpdat=gpcpst[,3:486]
gpcpJ=gpcpdat[,monJ]
climJ<-rowMeans(gpcpJ)
library(matrixStats)# rowSds command is in the matrixStats package
sdJ<-rowSds(gpcpJ)
#Plot Jan climatology
library(maps)
Lat1=seq(90,-90, len=73)
Lat=-Lat1
mapmat=matrix(climJ,nrow=144)
mapmat= mapmat[,seq(length(mapmat[1,]),1)]
plot.new()
int=seq(-50,50,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', "darkgreen","green",
                               "white","yellow","pink","red","maroon"),interpolate="spline")
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NCEP Jan SAT RA 1948-2015 climatology [deg C]",
                                xlab="Longitude", ylab="Latitude"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"))
#---------------------
#Plot Jan Standard Deviation
Lat=-Lat1
mapmat=matrix(sdJ,nrow=144)
mapmat= mapmat[,seq(length(mapmat[1,]),1)]
plot.new()
mapmat=pmin(mapmat,5) #Compress the values >5 to 5
int=seq(0,5,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 'green',
                               'yellow','pink','red','maroon'),interpolate='spline')
filled.contour(Lon, Lat, mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="NCEP Jan SAT RA 1948-2015 Standard Deviation [deg C]",
                                xlab="Longitude", ylab="Latitude"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="[oC]"))



#Compute the Jan EOFs
monJ=seq(1,486,12)
gpcpdat=gpcpst[,3:486]
gpcpJ=gpcpdat[,monJ]
climJ<-rowMeans(gpcpJ)
library(matrixStats)
sdJ<-rowSds(gpcpJ)
anomJ=(gpcpdat[,monJ]-climJ)/sdJ #standardized anomalies
anomAW=sqrt(cos(gpcpst[,1]*pi/180))*anomJ #Area weighted anormalies
svdJ=svd(anomAW) #execute SVD
#Compute the Jan EOFs

#plot eigenvalues
par(mar=c(3,4,2,4))
plot(100*(svdJ$d)^2/sum((svdJ$d)^2), type="o", ylab="Percentage of variance [%]",
     xlab="Mode number", main="Eigenvalues of covariance matrix")
legend(20,5, col=c("black"),lty=1,lwd=2.0,
       legend=c("Percentange variance"),bty="n",
       text.font=2,cex=1.0, text.col="black")
par(new=TRUE)
plot(cumsum(100*(svdJ$d)^2/sum((svdJ$d)^2)),type="o",
     col="blue",lwd=1.5,axes=FALSE,xlab="",ylab="")
legend(20,50, col=c("blue"),lty=1,lwd=2.0,
       legend=c("Cumulative variance"),bty="n",
       text.font=2,cex=1.0, text.col="blue")
axis(4)
mtext("Cumulative variance [%]",side=4,line=2)                             
###############################                              
#plot EOF1: The physical EOF= eigenvector divided by area factor
mapmat=matrix(svdJ$u[,1]/sqrt(cos(gpcpst[,1]*pi/180)),nrow=144)
rgb.palette=colorRampPalette(c('blue','green','white','yellow','red'),interpolate='spline')
int=seq(-0.04,0.04,length.out=61)
mapmat=mapmat[, seq(length(mapmat[1,]),1)]
filled.contour(Lon, Lat, -mapmat, color.palette=rgb.palette, levels=int,
               plot.title=title(main="January EOF1 from 1948-2016 NCEP Temp Data"),
               plot.axes={axis(1); axis(2);map('world2', add=TRUE);grid()},
               key.title=title(main="Scale"))
#
#plot PC1
pcdat<-svdJ$v[,1]
Time<-seq(1948,2015)
plot(Time, -pcdat, type="o", main="PC1 of NCEP RA Jan SAT: 1948-2015",
     xlab="Year",ylab="PC values",
     lwd=2, ylim=c(-0.3,0.3))


