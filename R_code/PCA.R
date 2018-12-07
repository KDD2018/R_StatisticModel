############################# PCA ###################################
principaldata <- USJudgeRatings
write.csv(principaldata, file = "principaldata.csv", sep = '')


########################## parallel analysis ##########################
principaldata <- read.csv("principaldata.csv", header = T)
library(psych)
R <- cor(principaldata[,1:11])
fa.parallel(principaldata[,1:11], fa = "pc", 
            show.legend = FALSE, main = "screen plot")
######################## extract components #####################
pc <- principal(principaldata[,1:11], nfactors = 2, rotate = "none")
p <- princomp(principaldata, cor = TRUE, scores = TRUE)

######################## component loading #######################
pc$loadings #####correlation of variable and pc

####################### weight of component scores ################
pc$weights

###################### components scores ##########################
pc$scores

#########################################################
principaldata <- data.frame(principaldata, pc$scores)
write.csv(principaldata, file = "principaldata.csv", sep = '')


scale(principaldata[,1:11])%*%p















