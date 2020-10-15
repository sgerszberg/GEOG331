library(lubridate)
datH <- read.csv("/Users/Simon/Documents/GitHub/Data for class/data/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)       

datP <- read.csv("/Users/Simon/Documents/GitHub/Data for class/data/2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365)) 


#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))


#QUESION 3

#number of oberservations in streamflow data
nrow(datD)

#number of observations in precepitation data
nrow(datP)



sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")



#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle




#Qustion 5

#aggreagtes by day and year
doy_2017_line <- aggregate(datD$discharge, by=list(datD$doy,datD$year), FUN="mean")
doy_2017_line<-doy_2017_line[doy_2017_line$Group.2==2017,]



#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Months", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
lines(doy_2017_line$Group.1,doy_2017_line$x,col = "green", type = "l")
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border


# #question 5
# datD$month<-day(dmy(datD$date))
# 
# aveY <- aggregate(datD$discharge, by=list(datD$month), FUN="mean")
# colnames(aveY) <- c("month","monthAve")
# 
# 
# sdY <- aggregate(datD$discharge, by=list(datD$month), FUN="sd")
# colnames(sdY) <- c("month","monthSD")
# 
# 
# #make plot
# plot(aveY$month,aveY$monthAve, 
#      type="l", 
#      xlab="months", 
#      ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
#      lwd=2,
#      ylim=c(0,90),
#      xaxs="i", yaxs ="i",#remove gaps from axes
#      axes=FALSE)#no axes
# # polygon(2017,#x coordinates
# #         11.9,#ycoord
# #         col="blue", 
# #         border=NA#no border
# # )
# axis(1, seq(1,12, by=1), #tick intervals
#      lab=seq(1,12, by=1)) #tick labels
# axis(2, seq(0,80, by=20),
#      seq(0,80, by=20),
#      las = 2)#show ticks at 90 degree angle
# # legend("topright", c("mean","1 standard deviation"), #legend items
# #        lwd=c(2,NA),#lines
# #        col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
# #        pch=c(NA,15),#symbols
# #        bty="n")#no legend border

#question 7

datP$check<-"no"

for (i in 1:nrow(datP)){
        if(datP$hour[i]==1 & datP$hour[i:i+22]==1:23)
                datP$check[i]<-"yes" 
        
}

full_24P<-datP[datP$check=="yes",]

for (i in 1:nrow(datP)){
        if(datP$hour[i]==1 & datP$hour[i:i+22]==1:23)
                datP$check[i]<-"yes" 
        
}



discharg_graph <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")


plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="DOY", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
lines(full_24P$doy,y=NULL,col = "red", type = "l")
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","days with full preciptation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black","red"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border


#question 8


#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#new day
hydroD <- datD[datD$doy >= 128 & datD$doy < 130 & datD$year == 2010,]
hydroP <- datP[datP$doy >= 128 & datP$doy < 130 & datP$year == 2010,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#QUESTION 9
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_boxplot()


ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_violin()

datD$day<-month(dmy(datD$date))

datD$season<-ifelse(datD$month==12 & datD$day>21 |datD$month==1|datD$month==2|datD$month==3 & datD$day<20, "Winter",ifelse(
        datD$month==3 & datD$day>21|datD$month==4|datD$month==5|datD$month==6 & datD$day<21,"Spring",ifelse(
        datD$month==6 & datD$day>21|datD$month==7|datD$month==8|datD$month==9 & datD$day<22,"Summer","Fall")
))


datD$color<-ifelse(datD$season=="Winter","blue",ifelse(datD$season=="Spring","purple",
        ifelse(datD$season=="Summer","green",ifelse(datD$season=="Fall","red","purple"))))
violin_graph_2016<-datD[datD$year==2016 & !is.na(datD$season),]

violin_graph_2017<-datD[datD$year==2017 & !is.na(datD$season),]



#2016 by season
ggplot(data= violin_graph_2016, aes(season,discharge)) + 
        geom_violin(aes(colour=color))+
        labs(title ="Season's discharge 2016 by season")

#2017 by season
ggplot(data= violin_graph_2017, aes(season,discharge)) + 
        geom_violin(aes(colour=color))+
        labs(title ="Season's discharge 2017 by season")

