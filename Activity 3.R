#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

assert(1 == 1, "error: unequal values")

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("/Users/Simon/Documents/GitHub/Data for class/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("/Users/Simon/Documents/GitHub/Data for class/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

colnames(datW) <-   colnames(sensorInfo)
#use install.packages to install lubridate
# install.packages(c("lubridate"))
#it is helpful to comment this line after you run this line of code on the computer
#and the package installs. You really don't want to do this over and over again.

library(lubridate)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")


#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  


#question 5:


lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

assert(datW$DD[lightscale > 0] == lightscale[lightscale > 0], "error: unequal values")


#question 6:
datW$wind.speed2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

assert(datW$wind.speed2== datW$wind.speed , "error: unequal values")


plot(subset$Time[!is.na(datW$wind.speed2)],subset$A[!is.na(subset$A)],type="l")  

plot(datW$wind.speed2[!is.na(datW$wind.speed2)], datW$wind.speed[!is.na(datW$wind.speed)], xlab = "wind without extreme", ylab = "wind with extreme",
     type="l")


#question 8
sum((datW$precipitation))


averages<-data.frame(avg_air_temp=mean(datW$air.temperature,na.rm=TRUE),
                     avg_wind_speed=mean(datW$wind.speed,na.rm=TRUE),
                     avg_soil_temp=mean(datW$soil.temp,na.rm=TRUE),
                     avg_preicipitation=mean(datW$precipitation,na.rm=TRUE))

averages<-round(averages,2)

#question 9

# par(mfrow=c(2,2))


plot(datW$DD , datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture",
     type="l")
plot(datW$DD , datW$air.temperature, xlab = "Day of Year", ylab = "Air temperature",
     type="l")
plot(datW$DD , datW$soil.temp, xlab = "Day of Year", ylab = "Soil temperature",
     type="l")
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation",
     type="l")




  
  