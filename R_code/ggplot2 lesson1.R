
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
ggplot(mtcars, aes(x=wt, y=mpg))+geom_point()

qplot(mtcars$wt, mtcars$mpg, data = mtcars,
      colour = cyl, size = cyl)
qplot(mpg, wt, data = mtcars, facets = vs ~ am)
qplot(mpg, wt, data = mtcars, geom = "path")
qplot(factor(cyl), wt, data = mtcars, 
      geom = c("boxplot", "jitter"))
qplot(mpg, data = mtcars, geom = "dotplot")

############  Creating a Line Graph #################
data("pressure")
#### line graph with base graphics by plot() function
plot(pressure$temperature, pressure$pressure,
     type = "l")
#### with points and another line
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, 
      col = "red")
points(pressure$temperature, pressure$pressure/2,
       col = "red")
#### line graph with qplot() from ggplot2
library(ggplot2)
qplot(pressure$temperature, pressure$pressure,
      geom = "line")
qplot(temperature, pressure, data = pressure,
      geom = c("line","point"))
ggplot(pressure,aes(x= temperature, y = pressure)
       ) + geom_line() + geom_point()

############# Creating a Bar Graph ##################
##### bar graph of values
barplot(height = BOD$demand,names.arg = BOD$Time)
#####  bar graph of counts
barplot(table(mtcars$cyl))
library(ggplot2)
# qplot(factor(BOD$Time), BOD$demand, geom = "bar", stat = "identity")
# qplot(Time, demand,data = BOD, geom = "bar", stat = "identity")
qplot(mtcars$cyl)      
qplot(factor(mtcars$cyl))      
######## equal to 
ggplot(mtcars, aes(x = factor(cyl))) + geom_bar()
ggplot(BOD, aes(x = Time, y = demand)) + 
  geom_bar(stat = "identity")

############ creating a Histogram #######################
hist(mtcars$mpg)
##########specify number of bins with breaks
hist(mtcars$mpg, breaks = 10)
qplot(mtcars$mpg)
library(ggplot2)
qplot(mpg, data = mtcars, binwidth = 4)
####### This is equivalent to
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 4)

############## creating a Boxplot ######################
plot(ToothGrowth$supp, ToothGrowth$len)
####### This is equivalent to
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len)+ geom_boxplot()
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
ggplot(ToothGrowth, aes(x = supp, y = len)) + geom_boxplot()
####### Using three separate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose),
      ToothGrowth$len, geom = "boxplot")
####### This is equivalent to
ggplot(ToothGrowth, aes(x = interaction(supp, dose),
                        y = len))+
  geom_boxplot()

############## plotting a Function Curve ################
curve(x^3-5*x, from = -4, to = 4)
#### plot a user-defined function
myfun <- function(xvar){
  1/(1+exp(-xvar+10))
}
curve(myfun(x), from = 0, to = 20)
curve(1-myfun(x), add = TRUE, col = "red")


