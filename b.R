setwd("C:/Users/HS/Documents/GitHub/wine_multivariate_team")
redwine<-read.csv("./data/redwine.csv")
library(tidyverse)
summary(redwine)


library(ggplot2)
wqa <- read.csv("./data/redwine.csv")
names(wqa)
str(wqa)

qplot(x = quality, data = wqa)
summary(wqa$quality)

#Transforming Quality from an Integer to a Factor
wqa$quality <- factor(wqa$quality, ordered = T)

#Creating a new Factored Variable called 'Rating'

wqa$rating <- ifelse(wqa$quality < 5, 'bad', ifelse(
  wqa$quality < 8, 'average', 'good'))

wqa$rating <- ordered(wqa$rating,
                      levels = c('bad', 'average', 'good'))

summary(wqa$rating)


install.packages("gridExtra")
library(gridExtra)

p1 <- qplot(x = pH, data = wqa)
p2 <- qplot(x = density, data = wqa)
p3 <- qplot(x = citric.acid, data = wqa)
p4 <- qplot(x = fixed.acidity, data = wqa)
p5 <- qplot(x = total.sulfur.dioxide, data = wqa)
p6 <- qplot(x = citric.acid, data = wqa)
p7 <- qplot(x = free.sulfur.dioxide, data = wqa)
p8 <- qplot(x = volatile.acidity, data = wqa)
p9 <- qplot(x = sulphates, data = wqa)
p10 <- qplot(x = residual.sugar, data = wqa)

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2)


