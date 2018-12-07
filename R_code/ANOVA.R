###############　ＡＮＯＶＡ　##############
write.csv(ToothGrowth, file = "tooth.csv", sep = '')
tooth <- read.csv("tooth.csv")
######## 单因素 #########
library(car)
library(multcomp)
attach(tooth)
fit1 <- aov(len ~ dose, data = tooth)
summary(fit1)
shapiro.test(tooth$len[dose==0.5])
shapiro.test(tooth$len[dose==1])
shapiro.test(tooth$len[dose==2])###正态性检验
leveneTest(len, dose, data = tooth)##方差齐性检验
tooth$dose <- factor(tooth$dose, levels = c("0.5","1","2"),
                     labels = c("0.5", "1", "2"))
tuk <- multcomp::glht(fit1, linfct = mcp(dose = "Tukey"))#多重比较
summary(tuk)
plot(cld(tuk, level = 0.05), col = "red")#多重比较图形展示

################  多因素 ################
fit2 <- aov(len ~ dose + supp, data = tooth)
summary(fit2)###无交互作用
fit3 <- aov(len ~ dose + supp + dose:supp, data = tooth)
summary(fit3)##有交互作用
shapiro.test(tooth$len[dose==0.5])
shapiro.test(tooth$len[dose==1])
shapiro.test(tooth$len[dose==2])
shapiro.test(tooth$len[supp=="VC"])
shapiro.test(tooth$len[supp=="OJ"])###正态性检验 
leveneTest(len, dose, data = tooth)
leveneTest(len, supp, data = tooth)##方差齐性检验

