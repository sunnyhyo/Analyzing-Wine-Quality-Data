

#factor -> regression 


rw.pc<-princomp(rw,cor=T)
rw.pc.pd<-predict(rw.pc,rw)[,1:5]
# 이게 PC1~PC5 data


factanal(데이터,factors=팩터수,rotation="varimax",scores="regression")

data.fa<-factanal(데이터명,factors=팩터수,rotation="varimax",scores="regression")

data.fa$scores
lda, kmeans, multinom(grade 3개 이상일 때), glm(grade 2개일 때)

