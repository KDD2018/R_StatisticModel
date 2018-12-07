##################### logit模型 ############################
library(car)
library(aod)
library(sjstats)
candidate <- read.csv("candidate.csv")####读取数据
candidate$rank <- factor(candidate$rank)
mylogit <- glm(admit ~ gre + gpa + rank, 
               data = candidate, family = "binomial")
summary(mylogit)#######################建立logit模型
#################### model diagnosis ##########################
hoslem_gof(mylogit)#############################拟合优度检验
vif(mylogit)#############################共线性检验
influencePlot(mylogit)######异常分析
######################### Forecast #################################
logittestdata <- with(candidate, data.frame(
  gre = rep(seq(from = 200, to = 800, length.out = 100), 4),
  gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))###测试集

prediction <- predict(mylogit, newdata = logittestdata, 
                               type = "response", se = FALSE)
####################################################################
write.csv(logittestdata,file = "logittestdata.csv", sep = "")

