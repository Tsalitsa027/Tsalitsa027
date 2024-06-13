library(readxl)
UAS_ANDAT <- read_excel("datacategorical.xlsx")
View(UAS_ANDAT)
str(UAS_ANDAT)

Y=as.factor(UAS_ANDAT$Gender)
x1=UAS_ANDAT$Weightinpounds
x2=UAS_ANDAT$Heightininches
x3=UAS_ANDAT$Waistininches
x4=UAS_ANDAT$Hipsininches
x5=UAS_ANDAT$Chestininches
x6=UAS_ANDAT$Handininches
x7=UAS_ANDAT$Shoe

UAS=data.frame(Y, x1, x2, x3,x4, x5, x6, x7)
model1 = glm(Y~x1+x2+x3+x4+x5+x6+x7, data=UAS, family = binomial(link ="logit" ))

#Uji Signifikansi
library(pscl)
pR2(Model1)
qchisq(0.95,(7-1))

#Uji Parsial
summary(model1)
model2 = glm(Y~x2+x3+x4+x5+x6+x7, data=UAS, family = binomial(link ="logit" ))
summary(model2)
model3 = glm(Y~x2+x3+x4+x6+x7, data=UAS, family = binomial(link ="logit" ))
summary(model3)
model4 = glm(Y~x3+x4+x6+x7, data=UAS, family = binomial(link ="logit" ))
summary(model4)
model5 = glm(Y~x3+x6+x7, data=UAS, family = binomial(link ="logit" ))
summary(model5)
model6 = glm(Y~x6+x7, data=UAS, family = binomial(link ="logit" ))
summary(model6)

model = c("Model 1","Model 2","Model 3", "Model 4", "Model 5", "Model 6")
AIC = c(model1$aic,model2$aic,model3$aic, model4$aic, model5$aic, model6$aic)
tabel = data.frame(model,AIC)
tabel

plot(model4)
