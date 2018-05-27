DigitalServics<-read.csv(file.choose())
head(DigitalServics)
tail(DigitalServics)
summary(DigitalServics)

str(DigitalServics)
summary(DigitalServics)

DSNonFactors<-DigitalServics[,-c(1,2,5,7,10,12,14,16,18,20,22,24,26,28,30,32,38)]
str(DSNonFactors)

library(corrplot)
#X<-as.matrix(DSNonFactors)
cor(DSNonFactors)
corrplot(cor(DSNonFactors))
warnings()

PCADS <- princomp(~X, scores = TRUE, cor=TRUE)
summary(PCADS)
loadings(PCADS)
Factors <- loadings(PCADS)
Factors
print(Factors, digits = 2, cutoff = 0.4, sort=TRUE)
