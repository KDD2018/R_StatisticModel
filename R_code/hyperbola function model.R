####################### genarate data ##################
x <- seq(from = 2, to = 22 ,by = 0.4)
y <- 10+2/x
ep <- rnorm(51, mean = 0, sd = 0.1)
y <- y+ep
plot(x,y) 
z <- 1/x
hyperboladata <- data.frame(x,y,z)
write.csv(hyperboladata,file = "hyperboladata.csv", sep ="")
#################### fit a hyperbola function model ############
library(car)
hyperboladata <- read.csv("hyperboladata.csv")
hyperbolafunmodel <- lm(y~z, data = hyperboladata)
summary(hyperbolafunmodel)
####################### model diagnosis ###################
ncvTest(hyperbolafunmodel)####同方差性检验
#durbinWatsonTest(hyperbolafunmodel)##误差的独立性检验   
#vif(hyperbolafunmodel)###多重共线性检验【大于4存在共线性】
influencePlot(hyperbolafunmodel, id.method = "identify")######异常分析
plot(hyperbolafunmodel)
library(lmtest)
dwtest(hyperbolafunmodel, alternative = "two.side")##误差的独立性检验
######################## Forecast #########################
x <- runif(10, min = 1, max = 5)
hyperbolatestdata <- data.frame(x)
write.csv(hyperbolatestdata,file = "hyperbolatestdata.csv", sep ="")
hyperbolatestdata <- read.csv("hyperbolatestdata.csv")
hyperbolatestdata <- as.matrix(hyperbolatestdata)

coef <- hyperbolafunmodel$coefficients[-1]
coef <- as.vector(coef)
prediction <- (1/hyperbolatestdata)%*%hyperbolafunmodel$coefficients[-1]+ hyperbolafunmodel$coefficients[1]
  
##############################################################
qnorm(ppoints(length(hyperbolafunmodel$residuals)))
y <- hyperbolafunmodel$residuals/sd(hyperbolafunmodel$residuals)
as.vector(y[order(y)])

