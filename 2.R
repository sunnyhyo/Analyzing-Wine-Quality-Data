library(ggplot2)
library(gridExtra)
library(GGally)
library(plyr)
library(corrplot)

setwd("C:/Users/HS/Documents/GitHub/Analyzing-Wine-Quality-Data")
rw <- read.csv("./data/redwine.csv")
getwd()

#PCA 분석# 
summary(rw)
rw<-rw[,-1]
names(rw)
rww<-scale(rw,center=T,scale=T)
rww.PC <- princomp(rww)
summary(rww.PC)
plot(rww.PC)

# 주성분 5개까지 포함하면 설명력 77%이 된다.
loadings(rww.PC)  # 여기서 주성분 의미분석해야함. 변수 공부 필요....



F1<-factanal(rww,factors=5) # 역시 변수 공부해서 의미 분석해야함..
