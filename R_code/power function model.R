####################### genarate data ##################
x1 <- runif(100, min = 4, max = 80)
y1 <- 2*x1^3
x2 <- rnorm(100, mean = 20, sd = 10)
y2 <- 3*x2-8
ep <- rnorm(100)
y <- y1+y2+ep
plot(x1,y)
powerdata <- data.frame(x1,x2,y)
write.csv(powerdata,file = "powerdata.csv", sep ="")
###################### read data ###########################
library(car)
powerdata <- read.csv("powerdata.csv")
#################### fit a power function model ############
powerfunmodel <- lm(log(y)~log(x), data = powerdata)
summary(powerfunmodel)
####################### model diagnosis ###################
ncvTest(powerfunmodel)####同方差性检验
#durbinWatsonTest(powerfunmodel)##误差的独立性检验   
#vif(powerfunmodel)###多重共线性检验【大于4存在共线性】
influencePlot(powerfunmodel, id.method = "identify")######异常分析
plot(powerfunmodel)
library(lmtest)
dwtest(powerfunmodel, alternative = "two.side")##误差的独立性检验
###################### Forecast ##############################
x <- rnorm(10, mean = 10, sd = 2)
powertestdata <- data.frame(x)
write.csv(powertestdata,file = "powertestdata.csv", sep ="")
powertestdata <- read.csv("powertestdata.csv")
powertestdata <- as.matrix(powertestdata)
coef <- powerfunmodel$coefficients[-1]
coef <- as.vector(coef)
prediction <- powertestdata^coef%*%exp(powerfunmodel$coefficients[1])

##############################################################
qnorm(ppoints(length(powerfunmodel$residuals)))
y <- powerfunmodel$residuals/sd(powerfunmodel$residuals)
as.vector(y[order(y)])
