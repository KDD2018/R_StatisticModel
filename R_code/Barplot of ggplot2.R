############# Bar Graphs ##########################
library(ggplot2)
library(gcookbook)
ggplot(pg_mean, aes(x = group, y = weight)) + 
  geom_bar(stat = "identity")
ggplot(pg_mean, aes(x = group)) + geom_bar()
ggplot(pg_mean,aes(x = group, y  = weight)) +
  geom_bar(stat = "identity", fill = "lightblue", 
           color = "black")

ggplot(BOD, aes(x = Time, y = demand)) + 
  geom_bar(stat = "identity")
ggplot(BOD, aes(x = factor(Time), y = demand)) + 
  geom_bar(stat = "identity")

############## Grouping Bars Together
############  grouped bar plot
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(position = "dodge", stat = "identity")
### another color
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Pastel1")

###########  stacked bar plot
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity")
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Pastel2")
ce <- cabbage_exp[1:5,]
ggplot(ce, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_fill_brewer(palette = "Pastel1")

########### Making a bar graph of counts 
ggplot(diamonds, aes(x = cut)) + geom_bar(fill = "purple")
ggplot(diamonds, aes(x = carat)) + geom_bar(color = "blue")
ggplot(diamonds, aes(x = carat)) + geom_histogram(fill = "green") 

############## Using color in a bar plot 
library(gcookbook)
upc <- subset(uspopchange, rank(Change)>40)
ggplot(upc, aes(x = Abb, y = Change, fill = Region)) +
  geom_bar(stat = "identity")
ggplot(upc, aes(x = reorder(Abb, Change), y = Change, fill = Region)) +
  geom_bar(stat = "identity", color = "black") + 
  scale_fill_manual(values = c("#669933", "#FFCC66")) +
  xlab("State") 

csub <- subset(climate, Source =="Berkeley" & Year >= 1900) 
csub$pos <- csub$Anomaly10y >= 0
ggplot(csub, aes(x = Year, y = Anomaly10y, fill = pos)) +
  geom_bar(stat = "identity", position = "dodge")
ggplot(csub, aes(x = Year, y = Anomaly10y, fill = pos)) +
  geom_bar(stat = "identity", position = "identity", 
           color = "black", size = 0.15) +
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = FALSE)

############### adjusting bar width and spacing
library(gcookbook)
ggplot(pg_mean, aes(x = group, y = weight)) +
  geom_bar(stat = "identity")
ggplot(pg_mean, aes(x = group, y = weight)) +
  geom_bar(stat = "identity", width = 0.5)

ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge")
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", width = 0.7, 
           position = position_dodge(width = 0.5))

############## making a stacked bar graph
library(ggplot2)
library(gcookbook)
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity") +
  guides(fill = guide_legend(reverse = TRUE))

library(plyr)
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar),
       order = desc(Cultivar)) +
  geom_bar(stat = "identity")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_brewer(palette="Pastel1")

############## making a proportional stacked bar graph
library(gcookbook)
library(plyr)
ce <- ddply(cabbage_exp, "Date", transform, 
            percent_weight = Weight/sum(Weight)*100)
ggplot(ce, aes(x = Date, y = percent_weight, fill = Cultivar)) +
  geom_bar(stat = "identity")

ggplot(ce, aes(x = Date, y = percent_weight, fill = Cultivar)) +
  geom_bar(stat = "identity", color = "blue") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "Pastel1")

############# adding labs to a bar graph
library(ggplot2)
library(gcookbook)
############ adjust the position of labels by vjust()
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), 
                        y = Weight)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Weight), vjust = 10, color = "white")

ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar),
                        y = Weight)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Weight),vjust = -0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)

############ map y position above bar top
ggplot(data = cabbage_exp, 
       mapping = aes(x = interaction(Date, Cultivar),y = Weight)) +
  geom_bar(stat = "identity") +
  geom_text(mapping = aes(y = Weight + 0.5 , label = Weight))

######### add labels on grouped bars
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Weight), vjust = 3, color = "white",
            position = position_dodge(0.9), size = 4)

######### add labels on stacked bars
library(plyr)
ce <- arrange(cabbage_exp, Date, Cultivar)
ce <- ddply(ce, "Date", transform, label_y = cumsum(Weight))
ggplot(ce, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_y, label = Weight),
            vjust = 9, color = "white")

##### calculate y position, placing it in the middle
ce <- ddply(ce, "Date", transform, 
            label_y = cumsum(Weight) - 0.5*Weight)
ggplot(ce, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_y, label = Weight), color = "white")

ggplot(ce, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(y = label_y, 
                label = paste(format(Weight, nsmall = 2), "kg")),
            size = 4) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "Pastel1")

##############  making a dot plot
library(ggplot2)
library(gcookbook)
tophit <- tophitters2001[1:25,]
ggplot(tophit, aes(x = avg, y = name)) + geom_point()
ggplot(tophit, aes(x = avg, y = reorder(name, avg))) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "blue",
                                          linetype = "dashed"),
        panel.grid.major.y = element_line(color = "red", 
                                          linetype = "twodash"),
        panel.grid.minor.y = element_blank()                                   
        )

ggplot(tophit, aes(x = reorder(name, avg), y = avg)) +
  geom_point(size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_line(color = "green",
                                          linetype = "dotdash"),
        panel.grid.major.x = element_line(color = "red", 
                                          linetype = "dashed")
        )

nameorder <- tophit$name[order(tophit$lg,tophit$avg)]
tophit$name <- factor(tophit$name, levels = nameorder)

ggplot(tophit, aes(x = avg, y = name)) +
  geom_segment(aes(yend = name), xend = 0, color = "grey50") +
  geom_point(size = 3, aes(color = lg)) +
  scale_fill_brewer(palette = "Set1", limits = c("NL","AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(1, 0.55),
        legend.justification = c(1, 0.5))

ggplot(tophit, aes(x = avg, y = name)) +
  geom_segment(aes(yend = name), xend = 0, color = "grey50") +
  geom_point(size = 3, aes(color = lg)) +
  scale_color_brewer(palette = "Set1", 
                     limits = c("NL", "AL"), guide = FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales ="free_y", space = "free_y")


