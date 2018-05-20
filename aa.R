install.packages("gridExtra")
install.packages("GGally")
install.packages("corrplot")


library(ggplot2)
library(gridExtra)
library(GGally)
library(plyr)
library(corrplot)

setwd("C:/Users/HS/Documents/GitHub/wine_multivariate_team")
ds <- read.csv("./data/redwine.csv")

# removing the serial number column
ds$X <- NULL

#changing the some of column names
colnames(ds)[which(names(ds) == "free.sulfur.dioxide")] <- "Free_SO2"
colnames(ds)[which(names(ds) == "total.sulfur.dioxide")] <- "Total_SO2"

# adding column to divide quality into 3 bins
ds$quality.cut <- cut(ds$quality, breaks = c(0,4,6,10))

#printing the dimension of the dataset
dim(ds)

# printing structure of the dataset
str(ds)

#summarising the data set
summary(ds)

ggplot(ds, aes(x = fixed.acidity)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = seq(4, 16, by = 1)) +
  ggtitle("Fixed Acidity distribution") +
  xlab("Fixed Acidity") +
  ylab("Count")
#Fixed Acidit, Count

pp1 <- ggplot(ds, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.02) +
  scale_x_continuous(breaks = seq(0, 1.6, by = 0.1)) +
  ggtitle("Volatile Acidity distribution") +
  xlab("Volatile Acidity") +
  ylab("Count")

pp2 <- ggplot(ds, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.02) +
  scale_x_log10(breaks = seq(0, 1.6, by = 0.5)) +
  ggtitle("Volatile Acidity distribution") +
  xlab("log(Volatile Acidity)") +
  ylab("Count")

grid.arrange(pp1, pp2)

p1 <- ggplot(ds, aes(x = pH)) +
  geom_histogram(binwidth = 0.02) +
  ggtitle("pH distribution") +
  xlab("pH") +
  ylab("Count")


p2 <- ggplot(ds, aes(x = Free_SO2)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Free SO2 distribution") +
  xlab("Free SO2") +
  ylab("Count")


p3 <- ggplot(ds, aes(x = Total_SO2)) +
  geom_histogram(binwidth = 3) +
  ggtitle("Total SO2 distribution") +
  xlab("Total SO2") +
  ylab("Count")


p4 <- ggplot(ds, aes(x = alcohol)) +
  geom_histogram(binwidth = 0.1) +
  ggtitle("Alcohol distribution") +
  xlab("Alcohol") +
  ylab("Count")

grid.arrange(p1, p2, p3, p4, ncol = 2)

ggplot(ds, aes(x = residual.sugar)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 15, by = 1)) +
  ggtitle("Residual Sugar Distributions") +
  xlab("Residual Sugar") +
  ylab("Count")

ggplot(ds, aes(x = quality)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(3, 8, by = 1)) +
  ggtitle("Quality Distributions") +
  xlab("Quality") +
  ylab("Count")

ggplot(ds, aes(x = quality.cut)) +
  geom_bar() +
  ggtitle("Quality Cut Distributions") +
  xlab("Quality Cut") +
  ylab("Count")

col2 <- colorRampPalette(c("red", "white", "blue"))
M <- cor(ds[,seq(1,12)])
corrplot.mixed(M, lower = "number", upper = "circle", col = col2(10))

ggplot(ds, aes(x = alcohol)) +
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Alcohol VS Quality") +
  xlab("Alcohol") +
  ylab("Quality")


ggplot(ds, aes(x=volatile.acidity))+
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Volatile Acidity VS Quality") +
  xlab("Volatile Acidity") +
  ylab("Quality")

ddply(ds,
      .(quality),
      summarize,
      Mean_Volatile_Acidity = mean(volatile.acidity),
      Variance_Volatile_Acidity = var(volatile.acidity),
      Standard_Deviation_Volatile_Acidity = sd(volatile.acidity))

ggplot(ds, aes(x = Free_SO2, y = Total_SO2)) +
  geom_jitter(alpha = 1/5) +
  ggtitle("Free S02 vs Total SO2") +
  xlab("Free SO2") +
  ylab("Total SO2")

ggplot(ds, aes(x = residual.sugar)) +
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Residual Sugar VS Quality") +
  xlab("Residual Sugar") +
  ylab("Quality")

ggplot(ds, aes(x = fixed.acidity, y = citric.acid)) +
  geom_jitter(alpha = 1/4) +
  ggtitle("Fixed Acidity VS Citric Acid") +
  xlab("Fixed Acidity") +
  ylab("Citric Acid")

ggplot(ds, aes(x = fixed.acidity, y = density)) +
  geom_line(stat = 'summary', fun.y = 'mean') +
  scale_x_continuous(breaks = seq(4, 16, 1)) +
  ggtitle("Fixed Acidity VS Density") +
  xlab("Fixed Acidity") +
  ylab("Density")



