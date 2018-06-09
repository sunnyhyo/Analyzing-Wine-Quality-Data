library(ggplot2)
library(gridExtra)
library(GGally)
library(plyr)
library(corrplot)
install.packages("nnet")
library(nnet)

setwd("C:/Users/HS/Documents/GitHub/Analyzing-Wine-Quality-Data")
rw <- read.csv("./data/redwine.csv")
getwd()
names(rw)
rw<-rw[,2:13]
#PCA 분석

rwq<-rw
rwq[rwq$quality==3|rwq$quality==4,13]<-c("low")
rwq[rwq$quality==5|rwq$quality==6|rwq$quality==7,13]<-c("middle")
rwq[rwq$quality==8,13]<-c("high")

rw.pc<-princomp(rw[,1:11],cor=T)
loadings(rw.PC)  # 여기서 주성분 의미분석해야함. 변수 공부 필요....
# 주성분 5개까지 포함하면 설명력 77%이 된다.

#PC1  
#PC2 
#PC3 
#PC4 
#PC5 


Y1<-predict(rw.pc)[,1]
Y2<-predict(rw.pc)[,2]
Y3<-predict(rw.pc)[,3]
Y4<-predict(rw.pc)[,4]
Y5<-predict(rw.pc)[,5]

PC5<-cbind(Y1,Y2,Y3,Y4,Y5)
PC5<-as.data.frame(PC5)

PC5.q<-cbind(PC5,rw$quality,rwq$V13)
colnames(PC5.q)[6]<-c("quality")

colnames(PC5.q)[7]<-c("grade")
rw.logis<-multinom(grade~.,PC5.q)

rw.logis.pred<-predict(rw.logis,data=PC5.q)
table(ifelse(rw.logis.pred==PC5.q$grade,1,0))







#Loading the csv file
wine <- read.csv("./data/redwine.csv")

#Transforming Quality from an Integer to a Factor
wine$quality <- factor(wine$quality, ordered = T)

#Creating a new Factored Variable called 'Rating'

wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(
  wine$quality < 7, 'average', 'good'))

wine$rating <- ordered(wine$rating,
                       levels = c('bad', 'average', 'good'))

head(wine)
ggplot(wine, aes(x=rating)) + geom_bar(fill="dark blue")
ggplot(data=wine, aes(x=alcohol, y=quality)) +
  geom_point(aes(color=rating, shape=rating), size=5)
ggplot(data=wine, aes(x=alcohol, y=pH)) +
  geom_point(aes(color=rating, shape=rating))
ggplot(data=wine, aes(x=alcohol, y=volatile.acidity)) +
  geom_point(aes(color=rating, shape=rating))

#당도
plot(wine$residual.sugar)

wine$sweetness <- ifelse(wine$residual.sugar < 3, 'dry', ifelse(
  wine$residual.sugar < 8, 'medium', 'sweet'))
table(wine$sweetness)
ggplot(wine, aes(x=sweetness)) + geom_bar(fill="dark blue")
ggplot(data=wine, aes(x=residual.sugar, y=sweetness)) +
  geom_point(aes(color=rating, shape=rating))
ggplot(data=wine, aes(x=residual.sugar, y=alcohol)) +
  geom_point(aes(color=rating, shape=rating))

#풍미
wine$body <- ifelse(wine$density < 3, 'light-bodied', ifelse(
  wine$density < 8, 'medium-bodied', 'full-bodied'))

light bodied, medium bodied, full- bodied