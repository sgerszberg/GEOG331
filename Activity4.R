#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length


x<-c(1,3,1)
y<-c(2,4,3)

for (i in (1:3)){
  fit<-lm(iris[,x[i]]~iris[,y[i]])
  print(summary(fit))
}



#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))


iris_3<-left_join(height,iris)


  



#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  #creates points
  geom_point()


#3b. make a scatter plot with ggplot and get rid of busy grid lines

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  #creates points
  geom_point()+
  #makes grid lines blank
  theme(panel.grid = element_blank())

#3c.make a scatter plot with ggplot and get rid of grid lines,
#show species by color, and increase the point size
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  #increases point size and changes color
  geom_point(aes(color=Species),size=4)+
  #makes grid lines blank
  theme(panel.grid = element_blank())
  


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		
