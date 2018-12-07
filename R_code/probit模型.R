#####probit模型
library(car)
library(sjstats)
candidate <- read.csv("candidate.csv")#####读取数据
candidate$rank <- factor(candidate$rank)
############################# fit a probit model####################
probit <- glm(admit ~ gre + gpa + rank, 
                family = binomial(link = "probit"), data = candidate)
summary(probit)###########################构建probit模型
##################### model diagnosis #########################
hoslem_gof(probit)######################拟合优度检验
vif(probit)#############################共线性检验
influencePlot(probit)##################影响分析
##################### Forecast ######################################
newprobittestdata <- data.frame(gre = rep(seq(from = 200, to = 800, 
                                    length.out = 100), 4 * 4), 
                      gpa = rep(c(2.5, 3, 3.5, 4), each = 100 * 4), 
                      rank = factor(rep(rep(1:4,  each = 100), 4)))
newprobittestdata[, c("prediction", "se")] <- predict(probit, newprobittestdata, 
                                   type = "response", se.fit = TRUE)[-3]
write.csv(newprobittestdata,file = "newprobittestdata.csv", sep = "")
