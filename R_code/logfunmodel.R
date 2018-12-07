####################### genarate data #######################
x <- runif(100, min = 10, max = 50)
y <- 5-4*log(x)
ep <- rnorm(100)
y <- y+ep
logfundata <- data.frame(x,y)
write.csv(logfundata,file = "logfundata.csv", sep ="")
####################### read data ############################
library(car)
logfundata <- read.csv("logfundata.csv")
###################### fit a log function model####################
logmodel <- lm(y~log(x), data = logfundata)
summary(logmodel)
####################### model diagnosis ###################
ncvTest(logmodel)####ͬ�����Լ���
#vif(logmodel)###���ع����Լ��顾����4���ڹ����ԡ�
influencePlot(logmodel, id.method = "identify")######�쳣����
library(lmtest)
dwtest(logmodel, alternative = "two.side")##���Ķ����Լ���   
plot(logmodel)
######################## Forecast ######################### 
x1 <- runif(10, min = 1, max = 25)
x2 <- runif(10, min = 20, max = 40)
x3 <- rnorm(10, mean = 5, sd = 2) 
logtestdata <- data.frame(x1,x2,x3)
write.csv(logtestdata,file = "logtestdata.csv", sep ="")
logtestdata <- read.csv("logtestdata.csv")
logtestdata <- as.matrix(logtestdata$x1)
#logtestdata <- cbind(log(logtestdata[,1]),
#                     log(logtestdata[,2]),logtestdata[,3])
coef <- logmodel$coefficients[-1]
coef <- as.matrix(coef)

prediction <- log(logtestdata)%*%coef+logmodel$coefficients[1]

##################################################
qnorm(ppoints(length(logmodel$residuals))) 
y <- logmodel$residuals/sd(logmodel$residuals)
as.vector(y[order(y)])