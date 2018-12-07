library(C50)
data(churn)
treemodel <- C5.0(x = churnTrain[,-20],
                  y = churnTrain$churn)
summary(treemodel)
ruleModel <- C5.0(churn ~ ., data = churnTrain, rules = TRUE)
ruleModel
summary(ruleModel)
plot(treemodel)
#####¾ö²ßÊ÷C5.0
mod1 <- C5.0(Species ~ ., data = iris)
plot(mod1)
plot(mod1, subtree = 3)
data(butterfly)
plot(butterfly,type='n')
text(butterfly[,1],butterfly[,2],labels=rownames(butterfly),cex=0.7,lwd=2)
library(cluster)
x <- rbind(cbind(rnorm(10, 0, 0.5), rnorm(10, 0, 0.5)),
           cbind(rnorm(15, 5, 0.5), rnorm(15, 5, 0.5)),
           cbind(rnorm( 3,3.2,0.5), rnorm(3,3.2,0.5)))
fannyx <- fanny(x, 2)
## Note that observations 26:28 are "fuzzy" (closer to # 2):
fannyx
summary(fannyx)
fan.x.15 <- fanny(x, 2, memb.exp = 1.5)