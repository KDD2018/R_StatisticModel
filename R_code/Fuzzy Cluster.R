######################################################
###########         Fuzzy  Cluster         ###########
######################################################
# Function tools for fuzzy cluster included:
# Fclust and FKM of package "fclust" & fanny of package "cluster"
# & cmeans and cshell of package "e1071"

###################  Import data
consumer <- read.csv("全国各省消费支出.csv", header = TRUE,
                     row.names = "省份")
############## fanny of package "cluster"
library(cluster)
f <- fanny(consumer, k = 5, diss = inherits(consumer, "dist"))
summary(f)
plot(f)
################ cmeans and cshell of package "e1071"
library(e1071)
cm <- cmeans(consumer, 5)


################ Iteractive Fclust of package "fclust"
library(fclust)
#fuzzy <- Fclust(consumer)
fuzzy <- FKM(consumer, 5)
summary(fuzzy)
plot(fuzzy)


