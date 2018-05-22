library(ggplot2)
library(gridExtra)
library(GGally)
library(plyr)
library(corrplot)

setwd("C:/Users/HS/Documents/GitHub/wine_multivariate_team")
redwine <- read.csv("./data/redwine.csv")
whitewine <-read.csv("./data/whitewine.csv")

redwine$TYPE<-c("Red")
whitewine$TYPE<-c("White")

wine<-rbind(redwine, whitewine)
wine
attach(wine)

head(wine)
tail(wine)
class(wine)
str(wine)
names(wine)
dim(wine)
as.factor(wine$TYPE)

boxplot(X~quality, data=wine)


#summarising the data set
head(wine$TYPE)
#Fixed Acidit, Count

    ggplot(wine, aes(x = fixed.acidity)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = seq(4, 16, by = 1)) +
  ggtitle("Fixed Acidity distribution") +
  xlab("Fixed Acidity") +
  ylab("Count")

#Volatile Acidity
(pp1 <- ggplot(wine, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.02) +
  scale_x_continuous(breaks = seq(0, 1.6, by = 0.1)) +
  ggtitle("Volatile Acidity distribution") +
  xlab("Volatile Acidity") +
  ylab("Count"))

pp2 <- ggplot(wine, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.02) +
  scale_x_log10(breaks = seq(0, 1.6, by = 0.5)) +
  ggtitle("Volatile Acidity distribution") +
  xlab("log(Volatile Acidity)") +
  ylab("Count")

grid.arrange(pp1, pp2)

###



(p1 <- ggplot(wine, aes(x = pH)) +
  geom_histogram(binwidth = 0.02) +
  ggtitle("pH distribution") +
  xlab("pH") +
  ylab("Count"))


(p2 <- ggplot(wine, aes(x = free.sulfur.dioxide)) +
  geom_histogram(binwidth = 1) +
  ggtitle("free.sulfur.dioxide distribution") +
  xlab("free.sulfur.dioxide") +
  ylab("Count"))


(p3 <- ggplot(wine, aes(x =total.sulfur.dioxide)) +
  geom_histogram(binwidth = 3) +
  ggtitle("Total.sulfur.dioxide distribution") +
  xlab("Total.sulfur.dioxide") +
  ylab("Count"))


(p4 <- ggplot(wine, aes(x = alcohol)) +
  geom_histogram(binwidth = 0.1) +
  ggtitle("Alcohol distribution") +
  xlab("Alcohol") +
  ylab("Count"))

grid.arrange(p1, p2, p3, p4, ncol = 2)

ggplot(wine, aes(x = residual.sugar)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 15, by = 1)) +
  ggtitle("Residual Sugar Distributions") +
  xlab("Residual Sugar") +
  ylab("Count")

ggplot(wine, aes(x = quality)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(3, 8, by = 1)) +
  ggtitle("Quality Distributions") +
  xlab("Quality") +
  ylab("Count")

ggplot(wine, aes(x = quality.cut)) +
  geom_bar() +
  ggtitle("Quality Cut Distributions") +
  xlab("Quality Cut") +
  ylab("Count")

col2 <- colorRampPalette(c("red", "white", "blue"))
M <- cor(wine[,seq(1,12)])
corrplot.mixed(M, lower = "number", upper = "circle", col = col2(10))

ggplot(wine, aes(x = alcohol)) +
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Alcohol VS Quality") +
  xlab("Alcohol") +
  ylab("Quality")


ggplot(wine, aes(x=volatile.acidity))+
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Volatile Acidity VS Quality") +
  xlab("Volatile Acidity") +
  ylab("Quality")

ddply(wine,
      .(quality),
      summarize,
      Mean_Volatile_Acidity = mean(volatile.acidity),
      Variance_Volatile_Acidity = var(volatile.acidity),
      Standard_Deviation_Volatile_Acidity = sd(volatile.acidity))

ggplot(wine, aes(x = Free_SO2, y = Total_SO2)) +
  geom_jitter(alpha = 1/5) +
  ggtitle("Free S02 vs Total SO2") +
  xlab("Free SO2") +
  ylab("Total SO2")

ggplot(wine, aes(x = residual.sugar)) +
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Residual Sugar VS Quality") +
  xlab("Residual Sugar") +
  ylab("Quality")

ggplot(wine, aes(x = fixed.acidity, y = citric.acid)) +
  geom_jitter(alpha = 1/4) +
  ggtitle("Fixed Acidity VS Citric Acid") +
  xlab("Fixed Acidity") +
  ylab("Citric Acid")

ggplot(wine, aes(x = fixed.acidity, y = density)) +
  geom_line(stat = 'summary', fun.y = 'mean') +
  scale_x_continuous(breaks = seq(4, 16, 1)) +
  ggtitle("Fixed Acidity VS Density") +
  xlab("Fixed Acidity") +
  ylab("Density")



