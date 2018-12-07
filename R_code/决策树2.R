library("rpart")
library("rpart.plot")
library("survival")
###构建决策树
fit <- rpart(Surv(pgtime, pgstat)~age+eet+g2+grade+gleason+ploidy,
             stagec, method = "exp")
####查看决策树
print(fit)
printcp(fit)
###绘制构建完的决策树图
plot(fit, uniform = T, branch = 0.6, compress = T)
text(fit, use.n = T)
####剪枝
fit2 <- prune(fit, cp = 0.016)
####绘制剪枝后的决策树图
plot(fit2, uniform = T, branch = 0.6, compress =T)
text(fit2, use.n =T)
