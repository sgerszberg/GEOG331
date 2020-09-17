# heights <- c(30,41,20,22)
# heights_cm <- heights*100
# heights[1]
# heights[2:3]
# 
# Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
# Mat[2,]
# 
# datW <- read.csv("/Users/Simon/Documents/GitHub/Data for class/noaa_weather (1)/2011124.csv")
# str(datW)
# datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
# datW$year <- as.numeric(format(datW$dateF,"%Y"))
# 
# levels(datW$NAME)
# 
# mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
# datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
# 
# averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
# colnames(averageTemp) <- c("NAME","MAAT")
# 
# datW$siteN <- as.numeric(as.factor(datW$NAME))
# 
# hist(datW$TAVE[datW$siteN == 1],
#      freq=FALSE, 
#      main = paste(levels(datW$NAME)[1]),
#      xlab = "Average daily temperature (degrees C)", 
#      ylab="Relative frequency",
#      col="grey50",
#      border="white")


#QUESTION 1
rows<-nrow(datW)
columns<-ncol(datW)

#Question 2
num<-c(11.2,2.3,4.4,5,6)
char<-c("e","b","c","d","a")
int<-c(1,2,3,4,5,6)
fact<-as.factor(c("male","male","female","female","male","female","male"))



#Question 3
help(hist)
help(paste)

#Question 4

for (i in 2:4){
        hist(datW$TAVE[datW$siteN == i],
             freq=FALSE, 
             main = paste(levels(datW$NAME)[i]),
             xlab = "Average daily temperature (degrees C)", 
             ylab="Relative frequency",
             col="grey50",
             border="white")
        
        abline(v = mean(datW$TAVE[datW$siteN == i],na.rm=TRUE), 
               col = "tomato3",
               lwd = 3)
        #add standard deviation line below the mean with red (tomato3) color
        #and thickness of 3
        abline(v = mean(datW$TAVE[datW$siteN == i],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == i],na.rm=TRUE), 
               col = "tomato3", 
               lty = 3,
               lwd = 3)
        #add standard deviation line above the mean with red (tomato3) color
        #and thickness of 3
        abline(v = mean(datW$TAVE[datW$siteN == i],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == i],na.rm=TRUE), 
               col = "tomato3", 
               lty = 3,
               lwd = 3)
}

par(mfrow=c(2,2))

#question 6
extra_mean<-mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4
# pnorm(5,
#       extra_mean,
#       sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
#                                                         mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
#                                                         sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

qnorm(.95,
      extra_mean,
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Question 7

hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily precipitation", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#gamma distribution

#Question 8

sum_precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum",na.rm=TRUE)

hist(sum_precip$x[sum_precip$Group.1=="ABERDEEN, WA US"],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Annual Precipitation", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

# sum_precip<-datW %>%
#         group_by(PRCP, year) %>% 
#         summarise_each(funs(sum))


#question 9

averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)


averagePRECIP <- aggregate(datW$PRCP, by=list(datW$NAME), FUN="mean",na.rm=TRUE)





