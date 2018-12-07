 ###########################################################
################## Factor Analysis ########################
#facdata <- read.csv("principaldata.csv",header = T )
#facdata <- facdata[,1:11]
#write.csv(facdata, file = "facdata.csv", sep = '')
########################################################

library(psych)
library(GPArotation)
facdata <- read.csv("facdata.csv")
R <- cor(facdata)
fa.parallel(facdata, fa = "fa", 
            show.legend = TRUE, main = "screen plot")
################# 主轴因子法+最大方差旋转 ##################
factor.analy <- fa(facdata, nfactors =2, 
                   fm = "pa", rotate = "varimax")
###########################################################
factor.analy1 <- fa(facdata, nfactors =2, 
                   fm = "ml", rotate = "varimax")

###################### 自定义因子分析函数 ###############################
###################### 执行主成分法 ################################
source("factor.analy1.R") 
(fa<-factor.analy1(R, m=2))
###################### 执行主轴因子法 ##############################
d<-c(0.123, 0.112, 0.155, 0.116, 0.073, 0.045, 0.033, 0.095)
source("factor.analy2.R") 
(fa<-factor.analy2(R, m=2, d))
###################### 执行极大似然法 ##############################
source("factor.analy3.R") 
(fa<-factor.analy3(R, m=2, d))

##################### 因子分析 ###############################
factor.analy<-function(S, m=0, d=1/diag(solve(S)), 
                       method=method){
  if (m==0){
    p<-nrow(S)
    eig<-eigen(S) 
    sum_eig<-sum(diag(S))
    for (i in 1:p){ 
      if (sum(eig$values[1:i])/sum_eig>0.80){
        m<-i 
        break
      }
    }
  }
  source("factor.analy1.R")
  source("factor.analy2.R")
  source("factor.analy3.R")
  switch(method, princomp=factor.analy1(S, m),
         factor=factor.analy2(S, m, d),
         likelihood=factor.analy3(S, m, d)
  )
} 



fa <- factor.analy(R,method = "princomp")

f <- varimax(fa$loadings%*%diag(c(-1,1)),normalize = TRUE)
f <- psych::Promax(fa$loadings%*%diag(c(-1,1)))
weight <- solve(R)%*%f$loadings
score <- scale(facdata)%*%weight
