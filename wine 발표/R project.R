### wine quality classification project


관측치 4898개와 12개 변수로 구성 되어있는 데이터
11가지 화학 성분들을 독립변수로 사용하여 와인의 품질(quality)을 예측하는 것이 목적
변수에 대한 자세한 설명은 아래와 같습니다.

11개 변수들 중 와인의 품질(quality)와 가장 연관이 높은 것은 알코올 (alcohol)임을 알 수 있다.

와인의 품질(quality)을 0과 10 사이의 수로 예측하는 회귀분석이 아닌,
품질의 좋고 나쁨을 구별하는 이항 분류분석
따라서 품질을 기존 integer 변수에서 7 이상이면 좋음(Good), 그 외에는 나쁨(Bad)인 factor 변수로 변환


rw<- read.csv("winequality-red.csv", header = T, sep=";")
names(rw)
rw.scale<-scale(rw[1:11])
rw<- cbind(rw.scale, wine[12])

rw.pc<-princomp(rw.scale,cor=T)
summary(rw.pc)
rw.pc$loadings
rw.pc.pd<-predict(rw.pc,rw)[,1:5]


#################3
rw.fa<-factanal(rw.scale,factors=5,rotation="varimax",scores="regression")
rw.fa
#summary(rw.fa)

rw.fa.data<- data.fa$scores
class(rw.fa.data)
class(rw$quality)
rw.fa.data<-as.data.frame(rw.fa.data)
rw.fa.data$quality<-rw$quality

head(rw.fa.data)
class(rw.fa.data)
class(rw$quality)
rw$quality<-as.numeric(rw$quality)
rw$quality
rw.fa.data<-as.matrix(rw.fa.data)
rw.fa.data$quality<-rw$quality 
rw.fa.data


  
````{r}
EDA
1. 데이터 전처리
2. 데이터 전처리 후 탐색적 데이터 분석

변수 확인
1. PCA
2. FA

classification 
1. logistic
2. LDA
3. K means

````
#####
#패키지 설치
install.packages("fpc")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("GGally")
install.packages("MASS")
install.packages("cluster")
install.packages("ROCR")
install.packages("nnet")
library(fpc)
library(cluster)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(GGally)
library(MASS)
library(ROCR)
library(nnet)
#####
#경로 설정
setwd("C:/Users/HS/Documents/GitHub/Analyzing-Wine-Quality-Data")
#데이터 읽기
wine<- read.csv("winequality-red.csv", header = T, sep=";")
head(wine)
names(wine)

#####
#EDA
p1 <- wine %>%
  ggplot(aes(quality)) +
  geom_bar() +
  ggtitle("와인 품질 막대 그래프")

p2 <- wine %>%
  ggplot(aes(alcohol, factor(quality))) +
  geom_jitter(col = "gray") +
  geom_boxplot(alpha = .5) +
  ggtitle("품질별 알코올 상자 그래프")

p3 <- wine %>%
  ggplot(aes(alcohol, density)) +
  geom_point(alpha=.1) +
  geom_smooth() +
  ggtitle("알코올, 밀도 산점도 & 추세선")

p4 <- wine %>%
  ggplot(aes(factor(quality), density)) +
  geom_jitter(color = "gray") +
  geom_boxplot(alpha = .5) +
  ggtitle("품질별 밀도 상자 그래프")
p4
grid.arrange(p1, p2, p3, p4, ncol=2)


품질이 5부터 6이 가장 많고, 1부터4,  7부터 10까지는 별로 없음을 알 수 있다.
table(wine$quality)
위에서 산점도를 통해 봤을 때는 알코올과 품질이 어느 정도 연관성이 있으며, 두 변수는 비선형 관계임을 알 수 있다.
산점도와 추세선을 봤을 때 알코올과 밀도는 음의 상관관계가 있다.
품질별 밀도 상자그림을 보면 와인의 품질이 좋을수록 평균 밀도가 낮음을 알 수 있다.

#####
#데이터 전처리 후 분ㅅ
wine <- wine %>%
  mutate(quality1 = factor(ifelse(quality >= 6, "Good", "Bad"))) %>%
  #quality1 은 2개 범주로 구분
  mutate(quality2 = factor(ifelse(quality >= 7, "Good", 
                                 ifelse(quality <=4, "Bad", "Medium" ))))
  #quality2 는 3개 범주로 구분

quality1<-ifelse(wine$quality1=="Good", 2, 1 )
quality2<-ifelse(wine$quality2=="Good", 3, 
                 ifelse(wine$quality2=="Bad", 1, 2))

head(wine)
names(wine)
table(wine$quality) ; table(wine$quality1) ; table(wine$quality2)
class(wine$quality) ; class(wine$quality1) ; class(wine$quality2)



sp1 <- wine %>%
  ggplot(aes(quality1)) + 
  geom_bar() +
  ggtitle("품질 분포 막대 그래프")

sp2 <- wine %>%
  ggplot(aes(alcohol, fill = quality1)) +
  geom_density(alpha = .5) +
  ggtitle("알코올, 품질 밀도 그래프")

sp3 <- wine %>%
  ggplot(aes(alcohol, density, col = quality1)) +
  geom_point() +
  ggtitle("알코올, 밀도 산점도")

sp4 <- wine %>%
  ggplot(aes(residual.sugar, fill = quality1)) +
  geom_density(alpha = .5) +
  xlim(0, 25) +
  ggtitle("잔당, 밀도에 대한 밀도 그래프")

grid.arrange(sp1, sp2, sp3, sp4, ncol = 2)


sp1 <- wine %>%
  ggplot(aes(quality2)) + 
  geom_bar() +
  ggtitle("품질 분포 막대 그래프")
sp1
sp2 <- wine %>%
  ggplot(aes(alcohol, fill = quality2)) +
  geom_density(alpha = .5) +
  ggtitle("알코올, 품질 밀도 그래프")
sp2
sp3 <- wine %>%
  ggplot(aes(alcohol, density, col = quality2)) +
  geom_point() +
  ggtitle("알코올, 밀도 산점도")
sp3
sp4 <- wine %>%
  ggplot(aes(residual.sugar, fill = quality2)) +
  geom_density(alpha = .5) +
  xlim(0, 25) +
  ggtitle("잔당, 밀도에 대한 밀도 그래프")
sp4
grid.arrange(sp1, sp2, sp3, sp4, ncol = 2)


시각화를 통해 다음 사실들을 알 수 있다.
품질이 나쁜 게 좋은 것보다 많음을 알 수 있다.
품질이 안 좋은 와인이 좋은 품질의 와인보다 알콜 농도가 낮은 것과 
두 변수는 비선형 관계임을 알 수 있다.
품질에 상관 없이 알코올과 밀도는 음의 상관관계 이다.
품질에 따른 잔당은 큰 차이가 없다. 즉, 잔당이 품질에 큰 영향력을 주지는 못 한다.


#####
#logistic regression 

#good, bad
set.seed(1802)
names(wine)
wine.glm <- glm(quality1 ~  fixed.acidity + volatile.acidity + citric.acid +
                  residual.sugar + chlorides + free.sulfur.dioxide + 
                  total.sulfur.dioxide + density + pH+
                  sulphates +alcohol, 
                data   = wine,
                family = "binomial")

#모형을 생성함에 있어서 유의한 변수들에는 어떤 것들이 있는지와 특히 유의한 변수들은 무엇이 있는지 확인 해보는 과정입니다.
summary(wine.glm)

#total.sulfur.dioxide, volatile.acidity , sulphates, alcohol 

y_obs<- ifelse(wine$quality1 =="Good", 1, 0)
yhat_glm <- predict(wine.glm)
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
table(wine$quality1)
#bad 744, good 855
performance(pred_glm, "auc")@y.values[[1]]
#0.82219

##good, bad, middle  

set.seed(1802)
#names(wine)
wine.glm <- multinom(quality2 ~  fixed.acidity + volatile.acidity +
                       citric.acid +
                  residual.sugar + chlorides + free.sulfur.dioxide + 
                  total.sulfur.dioxide + density + pH+
                  sulphates +alcohol, 
                data   = wine)
wine.glm

#모형을 생성함에 있어서 유의한 변수들에는 어떤 것들이 있는지와 특히 유의한 변수들은 무엇이 있는지 확인 해보는 과정입니다.
summary(wine.glm)

#volatile.acidity , alcohol , pH, residual.sugar

y_obs<- ifelse(wine$quality2 =="Good", 3, 
               ifelse(wine$quality2=="Bad", 1, 0))
yhat_glm <- predict(wine.glm)
yhat_glm<-ifelse(yhat_glm =="Good", 3, 
       ifelse(yhat_glm=="Bad", 1, 0))
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
#0.7108545

#########
#LDA
# good, bad -> 1차원 축소
wine.lda <- lda(quality1 ~ total.sulfur.dioxide + volatile.acidity +
                  sulphates+ alcohol , data=wine)
wine.lda
predict(wine.lda)
wine.lda.values <- predict(wine.lda)
ldahist(data = wine.lda.values$x[,1], g=quality2)
ldahist(data = wine.lda.values$x[,2], g=quality2)  #설명력 없음
plot(wine.lda.values$x[,1]) # make a scatterplot
text(wine.lda.values$x[,1],
     quality1,cex=0.7,pos=4,col="red") # add labels


# good, bad, middle -> 2차원 축소
wine.lda <- lda(quality2 ~ volatile.acidity + alcohol + pH+ residual.sugar , data=wine)
wine.lda
predict(wine.lda)
wine.lda.values <- predict(wine.lda)
ldahist(data = wine.lda.values$x[,1], g=quality2)
ldahist(data = wine.lda.values$x[,2], g=quality2)
plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],
     quality2,cex=0.7,pos=4,col="red") # add labels




```
########
#k means cluster

품질 예측은 아니고 그냥 분류 

Step1: Scale the data
As the measurement of free sulfur dioxide is from 1 to 72 while citric acidity is scaled from 0 to 1. We need to scale the data in order to perform accuracy of distance of each clusters. 
Step2: Find the ideal number of clusters
Step3: Plot the clusters
Step 4: Validate if number of cluster equal to 6 is more accurate than 8.
Step 5: Get the mean of the each attribute of each group
```


summary(wine)
wine.scale<-scale(wine[1:11])
wine<- cbind(wine.scale, wine[12])

wine<-rw.fa
wss<-(nrow(wine)-1)*sum(apply(wine,2,var))
for(i in 1:15) wss[i]<-sum(kmeans(wine,centers=i)$withinss)
plot(1:15,wss,type='b',xlab="Number of Clusters",ylab='Within groups sum of squares')

#I choose 6 and 8 to see which one is going to be more appropriate for our analysis.

fit1 <- kmeans(wine,2)
fit2 <- kmeans(wine,3)
fit3 <- kmeans(wine,4)
fit4 <- kmeans(wine,5)
fit4 <- kmeans(wine,6)


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

cluster.stats(?, fit1$cluster, fit2$cluster)
?cluster.stats



