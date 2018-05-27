library(Boruta)
library(rFerns)
library(ranger)

library(randomForest)

library(corrplot)
DigitalServics<-read.csv(file.choose())
head(DigitalServics)
tail(DigitalServics)
summary(DigitalServics)
corrplot(cor(DSNonFactors))

str(DigitalServics)
summary(DigitalServics)

DSRf<-DigitalServics[,-c(1,2,5,7,10,12,14,16,18,20,22,24,26,28,30,32,39)]
str(DSRf)

attach(DSRf)

Boruta(Churn~.,data=DSRf,doTrace=2)->Boruta.DigiServRF
print(Boruta.DigiServRF)

Boruta.ferns.DigiServ<-Boruta(Churn~.,data=DSRf,getImp=getImpFerns)
print(Boruta.ferns.Titanic)

bestmtry<-tuneRF(DSRf,Churn,stepFactor = 1.2,improve=0.01,trace = T,plot = T)
str(bestmtry)

print(randomForest(Churn~.-GenderBinary-PhoneServiceBinary,data=DSRf,importance=TRUE,proximity=TRUE))

attach(DigitalServics)