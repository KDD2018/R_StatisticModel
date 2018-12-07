###小样本
####单样本T检验
library(MASS)
#write.csv(UScrime,file = "UScrime.csv", sep = "")
library(car)
qqnorm(UScrime$M)
qqline(UScrime$M)
shapiro.test(UScrime$M)####正态性检验
t.test(UScrime$M, mu=137 )
####独立样本T检验
library(car)
qqnorm(UScrime$Prob)
qqline(UScrime$Prob)
shapiro.test(UScrime$Prob)####正态性检验
leveneTest(Prob~factor(So), data = UScrime)##方差齐性检验
t.test(Prob~So,data = UScrime)
####匹配样本T检验
library(car)
qqnorm(CO2$uptake)
qqline(CO2$uptake)
shapiro.test(CO2$uptake)####正态性检验
leveneTest(uptake~Type, data = CO2)##方差齐向检验
t.test(uptake~Type, data = CO2, paired = TRUE)

###独立性检验
library(vcd)
mytable <- xtabs(~Treatment+Improved, data = Arthritis)
chisq.test(mytable)###分类型
library(coin)
spearman_test(CONT ~ INTG, data = USJudgeRatings)#####数值型
write.csv(USJudgeRatings,file = "USJudgeRatings", sep = "")
