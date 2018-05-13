setwd("C:/Users/HS/Documents/GitHub/wine_multivariate_team")
redWine<-read.csv("./data/winequality-red.csv", sep=";")
whiteWine<-read.csv("./data/winequality-white.csv", sep=";")

write.csv(redWine,file="redwine.csv",row.names=TRUE)
write.csv(whiteWine,file="whitewine.csv",row.names=TRUE)

head(redWine)
