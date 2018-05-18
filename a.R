setwd("C:/Users/HS/Documents/GitHub/wine_multivariate_team")
redwine<-read.csv("./data/redwine.csv")

x<-redwine[,1:11]
x
quality<-redwine$quality 
pairs(x, col=as.numeric(quality)+2,pch=16)
