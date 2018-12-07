####tobit模型
require(ggplot2)
require(GGally)
require(AER)
library(car)
dat <- read.csv("tobit.csv")
###绘制分布图
f <- function(x, var, bw = 15){
  dnorm(x, mean = mean(var), sd(var))*length(var)*bw
}
p <- ggplot(dat, aes(x = apt, fill = prog))
p + stat_bin(binwidth = 15) + 
  stat_function(fun = f, size = 1, args = list(var = dat$apt))
p + stat_bin(binwidth = 1) + 
  stat_function(fun = f, size = 1, args = list(var = dat$apt,bw = 1))
ggpairs(dat[,c('read','math', 'apt')])
###模拟tobit模型
#summary(m <- vglm(apt ~ read + math + prog,
                # tobit(Upper = 800), data = dat))
m <- tobit(apt ~ read + math + prog, right = 800, data = dat)
summary(m)
#m2 <- tobit(apt ~ read + math, right = 800, data = dat)
#summary(m2)

#(p <- pchisq(2 * (logLik(m) - logLik(m2)), df = 2, 
            # lower.tail = FALSE))

vif(m)#############################共线性检验
influencePlot(m)##################影响分析
####残差分析 
dat$yhat <- fitted(m)
dat$rr <- resid(m, type = "response")
par(mfcol = c(1, 2))

with(dat, {
  plot(yhat, rr, main = "Fitted vs Residuals")
  qqnorm(rr)
  qqline(rr)
})

