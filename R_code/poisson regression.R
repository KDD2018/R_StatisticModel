####poisson regression
require(msm)
library(sjstats)
library(car)
p <- read.csv("poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, 
                 labels=c("General", "Academic","Vocational"))
  id <- factor(id)
})
summary(p)
#################################fit a poisson model##############
summary(m1 <- glm(num_awards ~ prog + math, 
                  family="poisson", data=p))
####################################模型诊断###############
hoslem_gof(m1)#############################拟合优度检验
vif(m1)#############################共线性检验
influencePlot(m1)######影响分析
#################################预  测##########################
prog <- as.factor(sample(c("General", "Academic","Vocational"),20,replace = TRUE))
newpoissontestdata <- data.frame(math = sample(seq(40,90),20),prog)
prediction <- predict(m1, newdata = newpoissontestdata, 
                      type = "response", se = FALSE)
newpoissontestdata <- cbind(newpoissontestdata,prediction)
######################################################################
write.csv(newpoissontestdata,file = "newpoissontestdata.csv", sep = "")

