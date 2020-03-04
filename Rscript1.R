read.csv("C:/users/Nathan/Documents/datamining/machine_dataset.csv")->processors
str(processors)
summary(processors)



#Min-Max Normalization
processors$MYCTNORM = (processors[,3] - 17) / 1600
processors$CACHENORM = (processors[,6] - 0) / 256
processors$CHNORM = processors[,7] / 55
processors$MNORM = processors[,4] / 180
processors[,11] <- NULL
View(processors)

#binning
b <- c(-Inf, 400, 800, Inf)
names <- c("Low","Medium", "High")
processors$MYCT.cat <- cut(processors$MYCT, breaks = b, labels = names)
View(processors)

#data visualization
plot(processors)


library(ggplot2)
ggplot(data = processors, aes(x=PRP))+geom_histogram(bins=40)
ggplot(processors, aes(x=MMAX, y=PRP))+geom_point()+geom_smooth(method="lm", se=F)
ggplot(processors, aes(x=MMIN, y=PRP))+geom_point()+geom_smooth(method="lm", se=F)
ggplot(processors, aes(x=CACHE, y=PRP))+geom_point()+geom_smooth(method="lm", se=F)
ggplot(processors, aes(x=CHMIN, y=PRP))+geom_point()+geom_smooth(method="lm", se=F)
ggplot(processors, aes(x=CHMAX, y=PRP))+geom_point()+geom_smooth(method="lm", se=F)
ggplot(processors, aes(x=MMAX, y=PRP, col=factor(CACHE)))+geom_point()+geom_smooth(method="lm", se=F)


#splitting data
set.seed(2)
library(caTools)
split <- sample.split(processors, SplitRatio = 0.7)
split
train <- subset(processors, split = "TRUE")
test <- subset(processors, split = "FALSE")
nrow(train)
nrow(test)