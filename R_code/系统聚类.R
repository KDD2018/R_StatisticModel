#####################################################
##########   Hierarchical Clustering Cluster Analysis        ###########
#####################################################
###########   系统聚类  ##################
###### 输入数据 ，生成距离 #########
x <- c(1, 2, 6, 8, 11)
dim(x) <- c(5, 1)
d <- dist(x,method = "euclidean")
######## 生成系统聚类 #############
hc1 <- hclust(d, method = "single") ###### 最短距离
plot(hc1, hang = -1)
hc2 <- hclust(d, method = "complete")##### 最长距离
plot(hc2, hang = -1)
hc3 <- hclust(d, method = "median") ######  中间距离
plot(hc3, hang = -1)
hc4 <- hclust(d, method = "centroid")#####  重心法
plot(hc4, hang = -1)
hc5 <- hclust(d, method = "average")######  类平均法
plot(hc5, hang = -1)
hc6 <- hclust(d, method = "mcquitty")#####  Mcquitty 相似法
plot(hc6, hang = -1)
hc7 <- hclust(d, method = "ward.D")########  离差平方和
plot(hc7, hang = -1)
############ 谱系图参数设置 ###############
dend1 <- as.dendrogram(hc1)
opar <- par(mfrow = c(2, 2), mar = c(4,3,1,2))
plot(dend1)
plot(dend1, nodePar = list(pch = c(3, NA), cex = 1.2, 
                           lab.cex = 0.8),
     type = "r", center = FALSE)
plot(dend1, edgePar=list(col = 2:3, lty = 1:2),
     dLeaf = 1.5, edge.root = TRUE)
plot(dend1, nodePar = list(pch = 2:1, cex = .4*2:1),
     horiz = TRUE)
par(opar)
####################################################
############## 输入相关阵即相似系数
x<-c(1.000, 0.846, 0.805, 0.859, 0.473, 0.398, 0.301, 0.382, 
     0.846, 1.000, 0.881, 0.826, 0.376, 0.326, 0.277, 0.277,
     0.805, 0.881, 1.000, 0.801, 0.380, 0.319, 0.237, 0.345, 
     0.859, 0.826, 0.801, 1.000, 0.436, 0.329, 0.327, 0.365,
     0.473, 0.376, 0.380, 0.436, 1.000, 0.762, 0.730, 0.629,
     0.398, 0.326, 0.319, 0.329, 0.762, 1.000, 0.583, 0.577,
     0.301, 0.277, 0.237, 0.327, 0.730, 0.583, 1.000, 0.539,
     0.382, 0.415, 0.345, 0.365, 0.629, 0.577, 0.539, 1.000)
names <- c('身高','手臂长','上肢长','下肢长',
           '体重','颈围','胸围','胸宽')
r <- matrix(data = x, nrow = 8, dimnames = list(names, names))
################# 距离
d <- as.dist(1-r)
#################### 生成聚类
hc <- hclust(d)
dend <- as.dendrogram(hc)####生成树状结构
################### 设置图形参数
np <- list(col = 3:2, cex = c(2, 0.75), pch = 21:22,
           bg = c("light blue", "pink"),
           lab.cex = 1.0, lab.cex = 'tomato')

adde <- function(n){
  if(!is.leaf(n)){
    attr(n, "edgePar") <- list(p.col = "plum")
    attr(n, "edgetext") <- paste(attr(n, "members"), "members")
  }
  n
}
de <- dendrapply(dend, adde)####### 节点上应用自定义adde函数
plot(de, nodePar = np)######绘制谱系图
plclust(hc, hang = -1)#####绘制谱系图
re <- rect.hclust(hc, k=3)

###################################################
write.csv(x = X, file = "全国各省消费支出.csv")
############# 读入数据
consumer <- read.csv("全国各省消费支出.csv",header = TRUE, 
                     sep = ",", row.names = "省份")
################# 生成距离结构
d <- dist(scale(consumer))
#################  系统聚类
h1 <- hclust(d, method = "complete")
h2 <- hclust(d, method = "average")
h3 <- hclust(d, method = "centroid")
h4 <- hclust(d, method = "ward.D")
############### 生成谱系图
opar <- par(mfrow = c(2,2), mar = c(4,3,1,2))
plclust(h1, hang = -1)
re1 <- rect.hclust(h1, k = 5, border = "red")
plclust(h2, hang = -1)
re1 <- rect.hclust(h2, k = 5, border = "red")
plclust(h3, hang = -1)
re1 <- rect.hclust(h3, k = 5, border = "blue")
plclust(h4, hang = -1)
re1 <- rect.hclust(h4, k = 5, border = "blue")
par(opar)

##################################################
#######载入鸢尾花数据
data("iris")
########## 生成距离结构
d <- dist(x = iris[, 1:4], method = "euclidean", diag = TRUE)
####################  生成聚类
hr1 <- hclust(d, method = "single") ###### 最短距离
de <- as.dendrogram(hr1)
plot(de, nodePar = list(cex = 0, lab.cex = 0.5))
re <- rect.hclust(hr1, k = 3, border = 1:3)
plclust(hr1, hang = -1)
re1 <- rect.hclust(hr1, k=3)
iris_id <- cutree(hr1, k = 3)
table(iris_id, iris$Species)


hr2 <- hclust(d, method = "complete")##### 最长距离
plot(hr2, hang = -1)
hr3 <- hclust(d, method = "median") ######  中间距离
plot(hr3, hang = -1)
hr4 <- hclust(d, method = "centroid")#####  重心法
plot(hr4, hang = -1)
hr5 <- hclust(d, method = "average")######  类平均法
plot(hr5, hang = -1)
hr6 <- hclust(d, method = "mcquitty")#####  Mcquitty 相似法
plot(hr6, hang = -1)
hr7 <- hclust(d, method = "ward.D")########  离差平方和
plot(hr7, hang = -1)






