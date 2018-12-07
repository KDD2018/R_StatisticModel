###大样本T检验

hospital <- read.csv("hospital.csv")
#hospital$PRICE <- rnorm(10832, mean = 15, sd = 10)##构建数据
#hospital$SEX <- sample(c('M','F'),10832, replace = TRUE,
                       #prob = c(0.5,0.5))
#write.csv(hospital,file = "hospital.csv", sep = "")
###单样本T检验
hist(hospital$PRICE)
library(nortest)
lillie.test(hospital$PRICE)##正态性检验
t.test(hospital$PRICE, mu = 14.8)
#####独立样本T检验
library(car)
lillie.test(hospital$PRICE)##正态性检验
leveneTest(PRICE~CHARGE_TYPE, data = hospital)##方差齐性检验
t.test(PRICE~CHARGE_TYPE, data = hospital, var.equal = TRUE)
####匹配样本T检验
library(car)
lillie.test(hospital$PRICE)##正态性检验
t.test(PRICE~SEX, data = hospital, paired = TRUE)

