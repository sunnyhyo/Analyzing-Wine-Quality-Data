#####
#Factor
#경로 설정
setwd("C:/Users/HS/Documents/GitHub/Analyzing-Wine-Quality-Data")
#데이터 읽기
wine<- read.csv("winequality-red.csv", header = T, sep=";")
head(wine)
names(wine.scale)
wine.scale
wine.scale<-scale(wine[1:11])
wines<- cbind(wine.scale, wine[12])

FF<-factanal(wine.scale,factors=5)
FF

F1<-cbind(wine.scale[1],wine.scale[9] )
F2<-cbind(wine.scale[6],wine.scale[7] )
F3<-cbind(wine.scale[2],wine.scale[3], wine.scale[5], wine.scale[10] )
F4<-cbind(wine.scale[11] )
F5<-cbind(wine.scale[4],wine.scale[8] )


apply(wine.scale,2,mean)
apply(wine.scale,2,sd)
pairs(wine.scale)
s.cor<-var(wine.scale)
s.cor
s.pc1<-princomp(wine,cor=TRUE)
s.pc1
s.pc2<-prcomp(wine,scale=T,center=T)
s.pc2

s.pc2$rotation # loadings(s.pc1)
head(s.pc2$x) #s.pc1[[6]]

s<-var(s.pc2$x)
s

pvar<-round(diag(s)/sum(diag(s), 3))
pvar
plot(s.pc1$x[,1], .pc2$x[,2],xlab="PC1",ylab="PC2")
m<-2
p<-ncol(stocks)
L<-s.pc2$rotation[,1:m]* matrix(rep(sqrt(eigen(s.cor)$values[1:m]),each=p), ncol=2)
L
Psi<-diag(diag(s.cor-L%*%t(L)))
Psi
fitted.model<-L%*%t(L)+Psi
fitted.model
resid<-s.cor-fitted.model
resid
s.cor