###相关分析
#x <- cor(state.x77[,3], state.x77[,4])#双变量间相关系数
#cor.test(state.x77[,3], state.x77[,4])#双变量相关系数显著性检验
#cor(state.x77)####计算多变量间相关系数
library(car)
scatterplotMatrix(state.x77)
library(psych)
corr.test(state.x77, adjust = "none", use = "complete")##显著性检验

write.csv(state.x77,file = "state.x77.csv", sep = "")
####偏相关分析
library(psych)
library(ggm)
state <- state.x77[,1:6]
jsbl <- c(1,5)
tjbl <- c(2,3,6)
u <- c(jsbl,tjbl)
s <- cov(state)#各变量间的协方差
pcor(u,s)#相关系数
q <- length(tjbl)
n <- dim(state)[1]
pcor.test(pcor(u,s),q,n)#显著性检验
#####典型相关分析
###典型相关系数的显著性检验
cancorcoef.test <- function(r,n,p,q,alpha = 0.1) {
  m <- length(r);Q <- rep(0,m);lambda <- 1;
  for(k in m:1){
    lambda <- lambda*(1-r[k]^2);
    Q[k] <- log(lambda)
  }
  s<-0; i<-m;
  for(k in 1:m){
    Q[k] <- (n-k+1-1/2*(p+q+3)+s)*Q[k]
    chi <- 1-pchisq(Q[k],(p-k+1)*(q-k+1))
    if(chi>alpha){
      i<- k-1;break
    }
    s<- s+1/r[k]^2
  }
  i
}
cancorcoef.test(r=ca$cor, n=20, p=3, q=3)
##########################
##典型相关分析
library(CCA)
data("nutrimouse")
X <- as.matrix(nutrimouse$gene[,1:5])
Y <- as.matrix(nutrimouse$lipid[,1:5])
res.cc <- cc(X,Y)##典型相关系数
plot(res.cc$cor, type = "b")
plt.cc(res.cc)
######################
######################
####典型相关分析

require(CCA)
mm <- read.csv("mmreg.csv")
psych <- mm[,1:3]
acad <- mm[,4:8]
matcor(psych, acad)###原始变量间的相关系数
ccl <- cc(psych, acad)###计算典型相关系数及典型载荷
ccl$cor##查看典型相关系数
ccl[3:4]##查看典型系数
ccl$scores####查看典型载荷
####典型相关显著性检验
ev <- (1-ccl$cor^2)
n <- dim(psych)[1]
p <- length(psych)
q <- length(acad)
k <- min(p,q)
m <- n-3/2-(p+q)/2
w <- rev(cumprod(rev(ev)))
d1 <- d2 <- f<- vector("numeric", k)
for(i in 1:k){
  s <- sqrt((p^2*q^2-4)/(p^2+q^2-5))
  si <- 1/s
  d1[i] <- p*q
  d2[i] <- m*s-p*q/2+1
  r <- (1-w[i]^si)/w[i]^si
  f[i] <- r*d2[i]/d1[i]
  p <- p-1
  q <- q-1
}###F统计量
pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(Wilk = w, F = f, df1 = d1, df2 = d2, p =pv))##F检验

###标准化典型系数
s1 <- diag(sqrt(diag(cov(psych))))
s1 %*% ccl$xcoef
s2 <- diag(sqrt(diag(cov(acad))))
s2 %*% ccl$ycoef

write.csv(nutrimouse, file = "nutrimouse.csv", sep = "")



