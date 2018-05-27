DigitalServics<-read.csv(file.choose())
head(DigitalServics)
tail(DigitalServics)
summary(DigitalServics)

str(DigitalServics)
summary(DigitalServics)

DSNonFactors<-DigitalServics[,-c(1,2,5,7,10,12,14,16,18,20,22,24,26,28,30,32,38)]
str(DSNonFactors)

attach(DigitalServics)

#Checking Correlation
library(corrplot)
#X<-as.matrix(DSNonFactors)
cor(DSNonFactors)
corrplot(cor(DSNonFactors))
warnings()

#Dividing set into two
set.seed(444)
ind <- sample(2, nrow(DSNonFactors),
              replace = TRUE,
              prob = c(0.7, 0.3))
Training<-DSNonFactors[ind==1,]
Testing<-DSNonFactors[ind==2,]
attach(Training)

#plottingGraphs
#pairs(DSNonFactors)
#Not a Good Idea on this computer

#Running Logistic Regression on only Insignificant variables reduced by using reverse elimination
ChurnModel<-glm(Training$ChurnBinary~.,data=Training,family = binomial)
summary(ChurnModel)


#Running Logistic Regression on only Insignificant variables reduced by using reverse elimination
ChurnModelSig<-glm(Training$ChurnBinary~.-PMCreditCardAuto-PMMailedCheck-DeviceProtetionBinary-OnlineBackupBinary-PhoneServiceBinary-DependentBinary-PartnerBinary-SeniorCitizen-GenderBinary,data=Training,family = binomial)
summary(ChurnModelSig)

#Predicting values based on model
ChurnPredictModel <- predict(ChurnModel,Testing,type="response")
ChurnPredictModel

ChurnValuesPredict<-rep(0,1491)
ChurnValuesPredict[ChurnPredictModel>0.5]<-1

#Checking for Misclassification rate
table(ChurnValuesPredict,Testing$ChurnBinary)



