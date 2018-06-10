#경로 설정
setwd("C:/Users/HS/Documents/GitHub/Analyzing-Wine-Quality-Data")
#데이터 읽기
rw<- read.csv("winequality-red.csv", header = T, sep=";")


rw.scale<-scale(wine[1:11])

rw.pc<-princomp(rw,cor=T)
rw.pc.data<-predict(rw.pc,rw)[,1:5]
wine<-as.data.frame(rw.pc.data)
head(wine)
wine$quality<- as.numeric(rw$quality)

wine <- wine %>%
  mutate(quality1 = factor(ifelse(quality >= 7, "2", "1"))) %>%
  #quality1 은 2개 범주로 구분  
  #1: Bad, 2:Good()
  mutate(quality2 = factor(ifelse(quality >= 7, "3", 
                                  ifelse(quality <=4, "1", "2" )))) %>% 
  #quality2는 3개 범주로 구분
  #1: Bad, 2:Medium, 3: Good
  mutate(quality3= factor(ifelse(quality <=4, "1", "2")))

table(wine$quality);table(wine$quality1);table(wine$quality2)

quality<-wine$quality
quality1<-wine$quality1
quality2<-wine$quality2
quality3<-wine$quality3
head(wine)
wine<-wine[,1:8]
wss<-(nrow(wine)-1)*sum(apply(wine,2,var))
for(i in 1:15) wss[i]<-sum(kmeans(wine,centers=i)$withinss)
plot(1:15,wss,type='b',xlab="Number of Clusters",ylab='Within groups sum of squares')

#I choose 6 and 8 to see which one is going to be more appropriate for our analysis.

fit1 <- kmeans(wine,2)
fit2 <- kmeans(wine,3)
fit3 <- kmeans(wine,4)
fit4 <- kmeans(wine,5)
fit4 <- kmeans(wine,6)

class(fit1)
fit1$cluster
table(fit1$cluster)
plotcluster(wine, fit1$cluster)
aggregate(wine,by=list(fit1$cluster),FUN=mean)
mydata <- data.frame(wine, fit1$cluster)


table(fit2$cluster)
plotcluster(wine, fit2$cluster)
aggregate(wine,by=list(fit2$cluster),FUN=mean)
mydata <- data.frame(wine, fit2$cluster)

table(fit3$cluster)
plotcluster(wine, fit3$cluster)
aggregate(wine,by=list(fit3$cluster),FUN=mean)
mydata <- data.frame(wine, fit3$cluster)


table(fit4$cluster)
plotcluster(wine, fit4$cluster)
aggregate(wine,by=list(fit4$cluster),FUN=mean)
mydata <- data.frame(wine, fit4$cluster)


clusplot(wine, fit1$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
clusplot(wine, fit2$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
clusplot(wine, fit3$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
clusplot(wine, fit4$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

plot(wine, col=fit1$cluster)
plot(wine, col=fit2$cluster)
plot(wine, col=fit3$cluster)
plot(wine, col=fit4$cluster)

#오분유률
error.list<-which(wine$quality1 != fit1$cluster)
wine[error.list,]
#head(error.list)
LDA.data<-data.frame(wine.lda.values$x, predict=fit1$cluster, original=quality1)
LDA.data$error<-LDA.data$predict != LDA.data$original
head(LDA.data)
table(LDA.data$error)
ggplot(LDA.data,aes(LD1,LD2)) + geom_point(aes(color=original, shape=predict, size=error))
cluster1<-as.numeric(fit1$cluster)
cluster2<-as.numeric(fit2$cluster)
cluster3<-as.numeric(fit3$cluster)

names(wine)
mean(ifelse(wine$quality1==cluster1,1,0))
mean(ifelse(wine$quality2==cluster2,1,0))
mean(ifelse(wine$quality1==cluster3,1,0))
table(wine$quality1);table(cluster1);/
  mean(ifelse(wine$quality1==cluster1,1,0))
table(wine$quality2);table(cluster2);/
  mean(ifelse(wine$quality2==cluster2,1,0))




