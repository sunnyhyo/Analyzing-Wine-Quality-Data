#경로 설정
setwd("C:/Users/HS/Documents/GitHub/Analyzing-Wine-Quality-Data")
#데이터 읽기
wine<- read.csv("winequality-red.csv", header = T, sep=";")


rw.scale<-scale(wine[1:11])
#rw<- cbind(rw.scale, wine[12])

#factor
#################
rw.fa<-factanal(rw.scale,factors=5,rotation="varimax",scores="regression")
rw.fa
#summary(rw.fa)

rw.fa.data<- data.fa$scores
class(rw.fa.data)
class(rw$quality)
rw.fa.data<-as.data.frame(rw.fa.data)
rw.fa.data$quality <- wine[12] <- as.numeric(wine$quality)
head(rw.fa.data)
class(rw.fa.data)
class(wine$quality)



wine<-rw.fa.data
class(wine$quality)

wine <- wine %>%
  mutate(quality1 = factor(ifelse(quality >= 7, "2", "1"))) %>%
  #quality1 은 2개 범주로 구분  
  #1: Bad, 2:Good()
  mutate(quality2 = factor(ifelse(quality >= 7, "3", 
                                  ifelse(quality <=4, "1", "2" )))) %>% 
  #quality2는 3개 범주로 구분
  #1: Bad, 2:Medium, 3: Good
  mutate(quality3= factor(ifelse(quality <=4, "1", "2")))
head(wine)

#####

ggpairs(wine,aes(color=quality1))
ggpairs(wine,aes(color=quality2))
ggpairs(wine,aes(color=quality3))

names(wine)
head(wine)
########
#LDA
#그냥 quality

quality<-wine$quality
quality1<-wine$quality1
quality2<-wine$quality2
quality3<-wine$quality3

Factor1<-wine$Factor1
Factor2<-wine$Factor2
Factor3<-wine$Factor3
Factor4<-wine$Factor4
Factor5<-wine$Factor5

wine.lda <- lda(quality ~ Factor1+Factor2+Factor3+Factor4+Factor5  , data=wine)
wine.lda
predict(wine.lda)
summary(predict(wine.lda))

wine.lda.values <- predict(wine.lda)
ldahist(data = wine.lda.values$x[,1], g=quality)
ldahist(data = wine.lda.values$x[,2], g=quality)  #설명력 없음
plot(wine.lda.values$x[,1],wine.lda.values$x[,2] ) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],
     quality,cex=0.7,pos=4,col="red") # add labels

names(wine)
# good, bad -> 1차원 축소
wine.lda <- lda(quality1 ~ Factor1+Factor2+Factor3+Factor4+Factor5  , data=wine)
head(wine.lda)
str(predict(wine.lda))
wine.lda.values <- predict(wine.lda)
head(wine.lda.values$x)
wine.lda.values$class
ldahist(data = wine.lda.values$x[,1], g=wine.lda.values$class)
#ldahist(data = wine.lda.values$x[,2], g=wine.lda.values$class)  #설명력 없음
plot(wine.lda.values$x[,1]) # make a scatterplot
text(1:1599,wine.lda.values$x[,1],
     wine.lda.values$class,cex=0.7,pos=4,col="red") # add labels

table(wine$quality2)
# good, bad, middle -> 2차원 축소
wine.lda <- lda(quality2 ~ Factor1+Factor2+Factor3+Factor4+Factor5  , data=wine)
#head(wine.lda)
#str(predict(wine.lda))
wine.lda.values <- predict(wine.lda)
head(wine.lda.values$x)
wine.lda.values$class
error.list<-which(wine$quality2 != wine.lda.values$class)
wine[error.list,]
#head(error.list)
LDA.data<-data.frame(wine.lda.values$x, predict=wine.lda.values$class, original=quality2)
LDA.data$error<-LDA.data$predict != LDA.data$original
head(LDA.data)
table(LDA.data$error)
ggplot(LDA.data,aes(LD1,LD2)) + geom_point(aes(color=original, shape=predict, size=error))


ldahist(data = wine.lda.values$x[,1], g=wine.lda.values$class)
ldahist(data = wine.lda.values$x[,2], g=wine.lda.values$class)  #설명력 없음
plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],
     wine.lda.values$class,cex=0.7,pos=4,col="red") # add labels
# factor loadings plotting
#points(wine.lda.values$loadings, pch=19, col = "blue")


# bad, good -> 1차원 축소
wine.lda <- lda(quality3 ~ Factor1+Factor2+Factor3+Factor4+Factor5  , data=wine)
#head(wine.lda)
#str(predict(wine.lda))
wine.lda.values <- predict(wine.lda)
head(wine.lda.values$x)
wine.lda.values$class
error.list<-which(wine$quality3 != wine.lda.values$class)
wine[error.list,]
#head(error.list)
LDA.data<-data.frame(wine.lda.values$x, predict=wine.lda.values$class, original=quality3)
LDA.data$error<-LDA.data$predict != LDA.data$original
head(LDA.data)
table(LDA.data$error)
ggplot(LDA.data,aes(LD1,LD2)) + geom_point(aes(color=original, shape=predict, size=error))

table(wine$quality3)
ldahist(data = wine.lda.values$x[,1], g=wine.lda.values$class)
ldahist(data = wine.lda.values$x[,2], g=wine.lda.values$class)  #설명력 없음
plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],
     wine.lda.values$class,cex=0.7,pos=4,col="red") # add labels

######
#logistic

#good, bad
set.seed(1802)
names(wine)
wine.glm <- glm(quality1 ~  Factor1+Factor2+Factor3+Factor4+Factor5, 
                data   = wine,
                family = "binomial")
#모형을 생성함에 있어서 유의한 변수들에는 어떤 것들이 있는지와 특히 유의한 변수들은 무엇이 있는지 확인 해보는 과정입니다.
summary(wine.glm)

#total.sulfur.dioxide, volatile.acidity , sulphates, alcohol 

y_obs<- wine$quality1
yhat_glm <- predict(wine.glm)
binomial_deviance(y_obs, yhat_glm)

# ROC Curve
library(ROCR)
pred_glm <- prediction(yhat_glm, y_obs)
perf_glm <- performance(pred_glm,
                        measure   = "tpr",
                        x.measure = "fpr") 

plot(perf_glm,
     col='black',
     main="ROC Curve of glm")

abline(0,1)
table(wine$quality1)
#bad 1382, good 217
1-(217)/(1387+217)
#0.864617
performance(pred_glm, "auc")@y.values[[1]]
#0.860973


#good, bad
set.seed(1802)
names(wine)
wine.glm <- glm(quality2 ~  Factor1+Factor2+Factor3+Factor4+Factor5, 
                data   = wine,
                family = "binomial")
#모형을 생성함에 있어서 유의한 변수들에는 어떤 것들이 있는지와 특히 유의한 변수들은 무엇이 있는지 확인 해보는 과정입니다.
summary(wine.glm)

#total.sulfur.dioxide, volatile.acidity , sulphates, alcohol 

y_obs<- wine$quality2
yhat_glm <- predict(wine.glm)
binomial_deviance(y_obs, yhat_glm)

# ROC Curve
library(ROCR)
pred_glm <- prediction(yhat_glm, y_obs)
perf_glm <- performance(pred_glm,
                        measure   = "tpr",
                        x.measure = "fpr") 

plot(perf_glm,
     col='black',
     main="ROC Curve of glm")

abline(0,1)
table(wine$quality)
#bad 1382, good 217
1-(217)/(1387+217)
#0.864617
performance(pred_glm, "auc")@y.values[[1]]
#0.860973


##good, bad, middle  
set.seed(1802)
#names(wine)
wine.glm <- multinom(quality2 ~  Factor1+Factor2+Factor3+Factor4+Factor5, 
                     data   = wine)
wine.glm

#모형을 생성함에 있어서 유의한 변수들에는 어떤 것들이 있는지와 특히 유의한 변수들은 무엇이 있는지 확인 해보는 과정입니다.
summary(wine.glm)

#volatile.acidity , alcohol , pH, residual.sugar

y_obs<-wine$quality
yhat_glm <-predict(wine.glm)
summary(predict(wine.glm))
plot(predict(wine.glm))
binomial_deviance(y_obs, yhat_glm)

# ROC Curve
pred_glm <- prediction(yhat_glm, y_obs)
perf_glm <- performance(pred_glm,
                        measure   = "tpr",
                        x.measure = "fpr") 

plot(perf_glm,
     col='black',
     main="ROC Curve of glm")

abline(0,1)
table(wine$quality2)
#bad 63, good 217, medium 1319
(1319)/(1319+63+217) #0.824
performance(pred_glm, "auc")@y.values[[1]]
#0.86019





#good, bad
set.seed(1802)
names(wine)
wine.glm <- glm(quality3 ~  Factor1+Factor2+Factor3+Factor4+Factor5, 
                data   = wine,
                family = "binomial")
#모형을 생성함에 있어서 유의한 변수들에는 어떤 것들이 있는지와 특히 유의한 변수들은 무엇이 있는지 확인 해보는 과정입니다.
summary(wine.glm)

#total.sulfur.dioxide, volatile.acidity , sulphates, alcohol 

y_obs<- wine$quality3
yhat_glm <- predict(wine.glm)
summary(yhat_glm)
yhat_glm<-ifelse(yhat_glm <=4, 1,2)

binomial_deviance(y_obs, yhat_glm)

# ROC Curve

library(ROCR)
pred_glm <- prediction(yhat_glm, y_obs)
perf_glm <- performance(pred_glm,
                        measure   = "tpr",
                        x.measure = "fpr") 

plot(perf_glm,
     col='black',
     main="ROC Curve of glm")

abline(0,1)
table(wine$quality3)
#bad 63, good 1536
1-63/(1387+217)
#0.964617
performance(pred_glm, "auc")@y.values[[1]]
#0.64







summary(wine)
wine.scale<-scale(wine[1:11])
wine<- wine.scale
library(dplyr)
wine<-rw.fa.data
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

###########
utility<- read.csv("winequality-red.csv", header = T, sep=";")

util.data<-utility[,1:11]
dist.util<-dist(util.data,method="euclidean")
h.complete<-hclust(dist.util,method="complete")
plot(h.complete,hang=-1,main="complete linkage")


h.complete<-hclust(dist.util,method="single")
plot(h.complete,hang=-1,main="single linkage")

h.complete<-hclust(dist.util,method="average")
plot(h.complete,hang=-1,main="average linkage")


