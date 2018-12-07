####################### genarate data ##################
x1 <- 1:40
y1 <- 3*exp(0.2*x1)
x2 <- rnorm(40, mean = 10, sd = 6)
y2 <- 3+5*x2
ep <- rnorm(40)
y <- y1+y2+ep
plot(x1,y)
expdata <- data.frame(x1,x2,y)
write.csv(expdata,file = "expdata.csv", sep ="")
#################### read data #############################
library(car)
expdata <- read.csv("expdata.csv")
#################### fit a exp function model ############
expfunmodel <- lm(log(y)~x1, data = expdata)
summary(expfunmodel)
####################### model diagnosis ##################
ncvTest(expfunmodel)####同方差性检验
#durbinWatsonTest(expfunmodel)##误差的独立性检验
###vif(expfunmodel)###多重共线性检验【大于4存在共线性】
influencePlot(expfunmodel, id.method = "identify")######异常分析
plot(expfunmodel)  

library(lmtest)
dwtest(expfunmodel, alternative = "two.side")#误差的独立性检验

###################### Forecast ###########################
#x1 <- rnorm(15,mean=5, sd = 2)
#exptestdata <- data.frame(x1)
#write.csv(exptestdata,file = "exptestdata.csv", sep ="")
exptestdata <- read.csv("exptestdata.csv")
exptestdata <- as.matrix(exptestdata)
coef <- expfunmodel$coefficients[-1]
coef <- as.matrix(coef)
logy_prediction <- exptestdata%*%coef+expfunmodel$coefficients[1]
y_prediction <- exp(logy_prediction)
############################################################
qnorm(ppoints(length(expfunmodel$residuals)))
y <- expfunmodel$residuals/sd(expfunmodel$residuals)
as.vector(y[order(y)])