####单因素方差分析
library("multcomp")
library("car")
fit <- aov(response~trt, data = cholesterol)
summary(fit)
TukeyHSD(fit, "trt", ordered = FALSE)##多重比较
###par(mar=c(5,4,6,2))
###tuk <- glht(fit, linfct=mcp(trt="Tukey"))
###plot(cld(tuk, level=.05), col="lightgrey")
#bartlett.test(response~trt, data = cholesterol)#异方差检验
leveneTest(response~trt,data = cholesterol)#异方差检验
qqnorm(cholesterol$response)
qqline(cholesterol$response)###Q-Q图 正态性检验
####多因素方差分析
#attach(ToothGrowth)
#table(supp,dose)
#aggregate(len, by=list(supp,dose),FUN=mean)
#aggregate(len, by=list(supp,dose),FUN=sd)
#fit <- aov(len~supp*dose)
#summary(fit)
library(car)
fit <- aov(uptake~conc*Type+Treatment+Plant,CO2)
summary(fit)
leveneTest(uptake~Type,data = CO2)#异方差检验
qqnorm(CO2$uptake)
qqline(CO2$uptake)###Q-Q图 正态性检验
attach(CO2)
write.csv(CO2,file = "CO2.csv", sep = "")