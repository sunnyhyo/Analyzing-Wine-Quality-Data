library(ggplot2)
library(gridExtra)
library(GGally)
library(plyr)
library(corrplot)

setwd("C:/Users/HS/Documents/GitHub/wine_multivariate_team")
rw <- read.csv("./data/redwine.csv")
ww <-read.csv("./data/whitewine.csv")

wine<-rbind(redwine, whitewine)

#
summary(rw)
table(rw$quality)
#
ggplot(rw)+geom_bar(aes(quality))+
  ggtitle("quality") +
  xlab("quality") +
  ylab("Count")

p1<- ggplot(rw)+geom_histogram(aes(fixed.acidity),binwidth=0.5)+
  ggtitle("Fixed Acidity distribution") +
  xlab("Fixed Acidity") +
  ylab("Count")
p2<- ggplot(rw)+geom_histogram(aes(volatile.acidity))+
  ggtitle("Volatile acidity") +
  xlab("Volatile acidity") +
  ylab("Count")

p3<- ggplot(rw)+geom_histogram(aes(citric.acid))+
  ggtitle("citric acid") +
  xlab("citric acid") +
  ylab("Count")
p4<- ggplot(rw)+geom_histogram(aes(residual.sugar))+
  #  scale_x_log10(breaks = seq(0, 1.6, by = 0.5)) +
  #  ggtitle("log(Residual sugar)") +
  ggtitle("Residual sugar") +
  xlab("Residual sugar") +
  ylab("Count")
p5<- ggplot(rw)+geom_histogram(aes(chlorides),binwidth=0.02)+
  ggtitle("Chlorides") +
  xlab("Chlorides") +
  ylab("Count")+ coord_cartesian(xlim = c(0,0.3))
p6<- ggplot(rw)+geom_histogram(aes(free.sulfur.dioxide))+
  ggtitle("Free sulfur dioxide") +
  xlab("Free sulfur dioxide") +
  ylab("Count")
p7<- ggplot(rw)+geom_histogram(aes(fixed.acidity))+
  ggtitle("density") +
  xlab("density") +
  ylab("Count")
p8<- ggplot(rw)+geom_histogram(aes(density))+
  ggtitle("density") +
  xlab("density") +
  ylab("Count")
p9<- ggplot(rw)+geom_histogram(aes(pH),binwidth=0.05)+
  ggtitle("pH") +
  xlab("pH") +
  ylab("Count")
p10<- ggplot(rw)+geom_histogram(aes(sulphates))+
  ggtitle("sulphates") +
  xlab("sulphates") +
  ylab("Count")
p11<- ggplot(rw)+geom_histogram(aes(total.sulfur.dioxide))+
  ggtitle("Total sulfur dioxide") +
  xlab("Total sulfur dioxide") +
  ylab("Count")
p12<- ggplot(rw)+geom_histogram(aes(alcohol))+
  ggtitle("alcohol") +
  xlab("alcohol") +
  ylab("Count")

grid.arrange(p1,p2,p3,p4, ncol=2)
grid.arrange(p5,p6,p7,p8, ncol=2)
grid.arrange(p9,p10,p11,p12, ncol=2)

  

#ggplot(rw) + geom_histogram(aes(residual.sugar)) + coord_cartesian(ylim = c(0, 5))

#correlation 
install.packages("corrplot")
library(corrplot)
M<-cor(rw)
head(round(M,2))
corrplot(M, method="number")


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
          sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=TRUE 
)
#상관관계 높은 변수끼리 scatter plot
a1<-ggplot(rw)+geom_point(aes(x=fixed.acidity,y=citric.acid)) # 0.67 
a2<-ggplot(rw)+geom_point(aes(x=fixed.acidity,y=density)) # 0.66
a3<-ggplot(rw)+geom_point(aes(x=free.sulfur.dioxide,y=total.sulfur.dioxide)) # 0.66 이상점 두어개 있음


a4<-ggplot(rw)+geom_point(aes(x=fixed.acidity,y=pH)) # -0.68
a5<-ggplot(rw)+geom_point(aes(x=volatile.acidity,y=pH)) # -0.55
a6<-ggplot(rw)+geom_point(aes(x=citric.acid,y=pH)) # -0.54
a7<-ggplot(rw)+geom_point(aes(x=density,y=alcohol)) # -0.49

grid.arrange(a1,a2,a3,a4,a5,a6,a7,ncol=2)

#
boxplot(alcohol~quality, data=rw,
        main= "Alcohol & Quality Data"  ,
        xlab="Quality",
        ylab="Alchohol")
ggplot(rw, aes(x = alcohol,y=..density..)) +
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Alcohol & Quality") +
  xlab("Alcohol") +
  ylab("Distribution Density")
#
boxplot(volatile.acidity~quality, data=rw,
        main= "Volatile acidity & Quality Data"  ,
        xlab="Quality",
        ylab="volatile.acidity")

ggplot(rw, aes(x=volatile.acidity))+
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("Volatile Acidity & Quality") +
  xlab("Volatile Acidity") +
  ylab("Distribution Density")

#
boxplot(volatile.acidity~quality, data=rw,
        main= "sulphates & Quality Data"  ,
        xlab="Quality",
        ylab="sulphates")

ggplot(rw, aes(x=volatile.acidity))+
  geom_density(aes(fill = "red", color = "red")) +
  facet_wrap(~quality) +
  theme(legend.position = "none") +
  ggtitle("sulphates & Quality") +
  xlab("sulphates") +
  ylab("Distribution Density")

#
