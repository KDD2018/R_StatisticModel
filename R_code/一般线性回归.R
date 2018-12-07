############### 一般线性回归 #####################
library(car)
states <- as.data.frame(state.x77[, c("Murder","Population",
                                      "Illiteracy","Income","Frost")])
############################# fit a linear model ############################
fit <- lm(Murder~Population+Illiteracy+Income+Frost,
          data = states)##拟合方程
summary(fit)
########################### model diagnosis ##############################
qqPlot(fit,labels = row.names(states), id.method = "identify",
       simulate = TRUE, main = "Q-Q Plot")###正态性检验
plot(fit)
ncvTest(fit)####同方差性检验
durbinWatsonTest(fit)##误差的独立性检验
vif(fit)###多重共线性检验【大于4存在共线性】
influencePlot(fit, id.method = 'identify')######异常分析
####################### Forecast  ####################################
newstate <- read.csv("newstate.csv")
prediction <- predict(fit, newstate)
newstate <- data.frame(newstate, prediction)
write.csv(newstate,file = "newstate.csv", sep ="")
