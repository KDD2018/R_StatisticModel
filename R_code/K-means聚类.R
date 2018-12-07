###################################################
###########     K-均值聚类          ###############
###################################################
############# 读入数据
consumer <- read.csv("全国各省消费支出.csv",header = TRUE, 
                     sep = ",", row.names = "省份")
################## K-均值聚类
km <- kmeans(scale(consumer), centers = 5, nstart = 20)
#################################################
library(fpc)
data("iris")
#############　标准化数据
normfun <- function(x){
  (x-min(x))/(max(x)-min(x))
}
raw.data <- iris[,1:4]
norm.data <- data.frame(sl = normfun(raw.data[,1]),
                        sw = normfun(raw.data[,2]),
                        pl = normfun(raw.data[,3]),
                        pw = normfun(raw.data[,4]))
#################### 通过轮廓系数寻找最优聚类数
K <- 2:8
round <- 50
rst <- sapply(K, function(i){
  print(paste("K=", i))
  mean(sapply(1:round, function(r){
    print(paste("Round=", r))
    result <- kmeans(norm.data, i)
    stats <- cluster.stats(dist(norm.data), result$cluster)
    stats$avg.silwidth
  }))
})
plot(K, rst, type = "l", main = "轮廓系数走势",
     xlab = "聚类个数", ylab = "轮廓系数")
##################聚为两类##############################
opar <- par(mfrow = c(1,2))
km <- kmeans(norm.data, 2)
############# 多维缩放
cmd <- cmdscale(dist(norm.data, method = "euclidean"))
plot(cmd, col = km$cluster, main = "K=2", pch = 19)
plot(cmd, col = iris$Species, main = "原始分类", pch = 19)
par(opar)







