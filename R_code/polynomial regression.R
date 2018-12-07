########################## genarate data #########################
x1 <- rnorm(100, mean = 3, sd = 3)
x2 <- rnorm(100)
y1 <- 3*x1^2+5*x1+rnorm(100, mean = 100, sd =10 )
y2 <- 5*x2^3+3*x2^2+rnorm(100, mean = 10, sd =5 )
x3 <- rnorm(100)
x4 <- rnorm(100)
y3 <- 2*x3^4+5*x3^3+rnorm(100, mean=20, sd= 10)
y4 <- 3*x4^5+20*x4^3+rnorm(100,mean = 200, sd = 10)
y5 <- 4.5*x4^6+8*x4^5+rnorm(100,mean = 300, sd = 20)
polynomialdata <- data.frame(x1,y1,x2,y2,x3,y3,x4,y4,y5)
write.csv(polynomialdata,file = "polynomialdata.csv", sep ="")
############################# read data ##########################
polynomialdata <- read.csv("polynomialdata.csv")
################################################################
#####                  二次多项式                          #####
################################################################
######################## polynomial regression ##################
poly1 <- lm(y1 ~ x1+I(x1^2), data = polynomialdata )
summary(poly1)  
######################### model diagnosis #####################
library(car)
ncvTest(poly1)####同方差性检验
#durbinWatsonTest(poly1)##误差的独立性检验
#vif(poly1)###多重共线性检验【大于4存在共线性】
influencePlot(poly1)######异常分析
plot(poly1)
library(lmtest)
dwtest(poly1, alternative = "two.side")##误差的独立性检验
########################## Forecast ############################
x1 <- runif(10, min = 0, max = 20)
x2 <- rnorm(10, mean = 5, sd = 5)
x3 <- rnorm(10, mean = 10, sd = 8)
x4 <- rnorm(10, mean = 2, sd = 2)
x <- x4^2
polytestdata <- data.frame(x1, x2, x3, x4, x)
write.csv(polytestdata, file = "polytestdata.csv", sep = '')

polytestdata <- read.csv("polytestdata.csv")
poly1testdata <- data.frame(polytestdata$x1,polytestdata$x1^2)
poly1testdata <- as.matrix(poly1testdata)
coef <- poly1$coefficients[-1]
coef <- as.matrix(coef)
prediction<- poly1testdata%*%coef+poly1$coefficients[1]
########## genarate quantile of Gaussian distribution ###########
qnorm(ppoints(length(poly1$residuals)))
y <- poly1$residuals/sd(poly1$residuals)
as.vector(y[order(y)])
########################################################
###                  三次多项式                      ###
########################################################
polynomialdata <- read.csv("polynomialdata.csv")
######################## polynomial regression ##################
poly2 <- lm(y2~x2+I(x2^2)+I(x2^3), data = polynomialdata)
summary(poly2)
######################### model diagnosis #####################
library(lmtest)
dwtest(poly2, alternative = "two.side")##误差的独立性检验
library(car)
ncvTest(poly2)####同方差性检验
influencePlot(poly2)######异常分析
plot(poly2)
########## genarate quantile of Gaussian distribution ###########
qnorm(ppoints(length(poly2$residuals)))
y <- poly2$residuals/sd(poly2$residuals)
as.vector(y[order(y)])
########################## Forecast ############################
polytestdata <- read.csv("polytestdata.csv")
poly2testdata <- data.frame(polytestdata$x2,polytestdata$x2^2,polytestdata$x2^3)
poly2testdata <- as.matrix(poly2testdata)
coef <- poly2$coefficients[-1]
coef <- as.matrix(coef)
prediction<- poly2testdata%*%coef+poly2$coefficients[1]
################################################################
#####                  四次多项式                          #####
################################################################
polynomialdata <- read.csv("polynomialdata.csv")
######################## polynomial regression ##################
poly3 <- lm(y3~x3+I(x3^2)+I(x3^3)+I(x3^4), data = polynomialdata)
summary(poly3)
######################### model diagnosis #####################
library(lmtest)
dwtest(poly3, alternative = "two.side")##误差的独立性检验
library(car)
ncvTest(poly3)####同方差性检验
influencePlot(poly3)######异常分析
plot(poly3)
########## genarate quantile of Gaussian distribution ###########
qnorm(ppoints(length(poly3$residuals)))
y <- poly3$residuals/sd(poly3$residuals)
as.vector(y[order(y)])
########################## Forecast ############################
polytestdata <- read.csv("polytestdata.csv")
poly3testdata <- data.frame(polytestdata$x3,polytestdata$x3^2,
                            polytestdata$x3^3,polytestdata$x3^4)
poly3testdata <- as.matrix(poly3testdata)
coef <- poly3$coefficients[-1]
coef <- as.matrix(coef)
prediction<- poly3testdata%*%coef+poly3$coefficients[1]
################################################################
#####                  五次多项式                          #####
################################################################
polynomialdata <- read.csv("polynomialdata.csv")
######################## polynomial regression ##################
poly4 <- lm(y4~x4+I(x4^2)+I(x4^3)+I(x4^4)+I(x4^5), data = polynomialdata)
summary(poly4)
######################### model diagnosis #####################
library(lmtest)
dwtest(poly4, alternative = "two.side")##误差的独立性检验
library(car)
ncvTest(poly4)####同方差性检验
influencePlot(poly4)######异常分析
plot(poly4)
########## genarate quantile of Gaussian distribution ###########
qnorm(ppoints(length(poly4$residuals)))
y <- poly4$residuals/sd(poly4$residuals)
as.vector(y[order(y)])
########################## Forecast ############################
polytestdata <- read.csv("polytestdata.csv")
poly4testdata <- data.frame(polytestdata$x4,polytestdata$x4^2,
                            polytestdata$x4^3,polytestdata$x4^4,
                            polytestdata$x4^5)
poly4testdata <- as.matrix(poly4testdata)
coef <- poly4$coefficients[-1]
coef <- as.matrix(coef)
prediction<- poly4testdata%*%coef+poly4$coefficients[1]
################################################################
#####                  六次多项式                          #####
################################################################
polynomialdata <- read.csv("polynomialdata.csv")
######################## polynomial regression ##################
poly5 <- lm(y5~x4+I(x4^2)+I(x4^3)+I(x4^4)+I(x4^5)+I(x4^6), data = polynomialdata)
summary(poly5)
######################### model diagnosis #####################
library(lmtest)
dwtest(poly5, alternative = "two.side")##误差的独立性检验
library(car)
ncvTest(poly5)####同方差性检验
influencePlot(poly5)######异常分析
plot(poly5)
########## genarate quantile of Gaussian distribution ###########
qnorm(ppoints(length(poly5$residuals)))
y <- poly5$residuals/sd(poly5$residuals)
as.vector(y[order(y)])
########################## Forecast ############################
polytestdata <- read.csv("polytestdata.csv")
poly5testdata <- data.frame(polytestdata$x4,polytestdata$x4^2,
                            polytestdata$x4^3,polytestdata$x4^4,
                            polytestdata$x4^5,polytestdata$x4^6)
poly5testdata <- as.matrix(poly5testdata)
coef <- poly5$coefficients[-1]
coef <- as.matrix(coef)
prediction<- poly5testdata%*%coef+poly5$coefficients[1]

