###样本量、均值、标准差、峰度、偏度
library("dprep")
a <- mtcars[,c(-2,-8:-11)]
mystats <- function(x, na.omit = FALSE) {
  if (na.omit) 
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  cv <- s/m
  sum<-sum(x)
  ad <- sum(abs(x-m))/n
  mod <- moda(x)
  skew <- sum((x - m)^3/s^3)/n
  kurt <- sum((x - m)^4/s^4)/n - 3
  return(c(sum= sum, cv =cv,ad=ad, skew=skew,kurt=kurt)) 
}
s <- sapply(a, mystats)
###描述性分析
#write.csv(mtcars,file = "mtcars.csv", sep = "")
library("psych")
p <- describe(mtcars[,c(-2,-8:-11)])
z <- hist(mtcars$disp)
library("Hmisc")
describe(mtcars[,c(2,8:11)])
############计算四分位差
s <- quantile(mtcars[,c(-2,-8:-11)][,6], probs = 0.75)
t <- quantile(mtcars[,c(-2,-8:-11)][,6], probs = 0.25)
s-t


for(i in 1:6) 
{
  qd <- vector(mode = "numeric",length = 6)
  u <- vector(mode = "numeric",length = 6)
  v <- vector(mode = "numeric",length = 6)
  u[i]<-quantile(x[,i], probs = 0.75)
  v[i]<-quantile(x[,i], probs = 0.25)
  qd[i]<- u[i]-v[i]
  return(qd)
  
}