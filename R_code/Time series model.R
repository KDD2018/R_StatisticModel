################################################
#####       Exponential smoothing           ####
################################################
data("AirPassengers")
####时序走势
plot(AirPassengers)
#########时间序列分解
ap_dp <-decompose(AirPassengers, type = "mult")
plot(ap_dp)
######## Holt-Winter 乘法模型 ############
ap.hw <- HoltWinters(AirPassengers, seasonal = "mult")
plot(ap.hw)
plot(forecast(ap.hw))
#################### ets ########
library(forecast)
ap_e <- ets(AirPassengers, model = "AAM")
plot(forecast(ap_e))

####### 预测 ############
ap.p <- predict(ap.hw, n.ahead = 4*12)
ts.plot(AirPassengers, ap.p, 
        gpars=list(xlab="time", ylab="AirPassengers", 
                   lty=c(1:2), main = "Holt-Winter Forecast"))

#######################################################
#####              ARIMA Model                    #####
#######################################################
####### 载入数据 #########
data("AirPassengers")
plot(AirPassengers)
#plot(aggregate(AirPassengers))
#boxplot(ap~cycle(AirPassengers))
######### 对数变换、差分变换后的趋势特征 #######
ln <- log(AirPassengers)
plot(ln)
ap <- diff(ln, lag = 12)
plot(ap) #######消除季节变动
as <- diff(ap)
plot(as)#######消除趋势
#########  平稳性检验  ###################
library(tseries)
adf.test(as)
######### 自相关图、偏自相关图 ###########
a <- acf(as)
p <- pacf(as)
########### 拟合模型 ############
ap_am <- arima(ln, order = c(0,1,1), 
      seasonal = list(order = c(0,1,1),period = 12))
plot(ap_am)
############## 残差白噪声检验 #################
r <- acf(ap_am$residuals)
############# 预测 ############################
ap_fore <- predict(ap_am, n.ahead = 4*12)
ap_pt <- exp(ap_fore$pred)#####指数变换为原序列
ts.plot(AirPassengers, ap_pt,
        gpars=list(xlab="time", ylab="AirPassengers", 
                   lty=c(1:2), main = "ARIMA Forecast"))





##################################################
data("UKDriverDeaths")
par(mfrow = c(1, 2))
plot(UKDriverDeaths)
########
lines(filter(UKDriverDeaths, c(1/2, rep(1, 11), 1/2)/12),
      col = 2) 
plot(rollapply(UKDriverDeaths, 12, sd))

### Season-trend decomposition by loess smoothing ###
dd_stl <- stl(log(UKDriverDeaths), s.window = 13)
plot(dd_stl)
plot(dd_dec$trend, ylab = "trend") 
lines(dd_stl$time.series[,"trend"], lty = 2, lwd = 2)

####################################################
a <- data.frame(time = c(time(Nile)),Nile =c(Nile))
p <- ggplot(a, aes(x=time, y=Nile))
p + geom_line(color = "skyblue") + xlab("time") + ylab("Nile")
library(ggfortify)
autoplot(Nile, ts.colour = "tomato1" )+ xlab("time") + ylab("Nile")
autoplot(stl(AirPassengers, s.window = 'periodic'),ts.colour = "red")
library(changepoint)
autoplot(cpt.meanvar(Nile),colour='green') + ggtitle("The Time Series of Nile")

############################################################
set.seed(1)
x <- w <- rnorm(1000)
for(i in 3:1000)
  x[i] <- 0.5*x[i-1]+x[i-1]-0.5*x[i-2]+w[i]+0.3*w[i-1]
plot(x)
acf(x)
dx <- diff(x)
plot(dx,type = "l")
acf(dx)
pacf(dx)
library(tseries)
adf.test(dx)

arima(x,order = c(1,1,1))
y <- arima.sim(model = list(order = c(1,1,1),ar = 0.5,
                            ma = 0.3),n = 1000)
arima(y,order = c(1,1,1))
