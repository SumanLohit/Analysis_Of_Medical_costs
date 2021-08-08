lm5 <- lm(logcharges ~ . -age -age.2 -charges-bmi-children +bminew*smoker + smoker*factorChildren +agenew*smoker , data = medCost2)
summary(lm5) #78.88

#taking age and age square
lm6<- lm(logcharges~ age+age.2+bminew+factorChildren+smoker+region +bminew*smoker + smoker*factorChildren +age*smoker, data = medCost2)
summary(lm6) #0.8521
plot(lm6, which=1)

lm7<- lm(logcharges~ age+age.2+bminew+factorChildren+smoker+region+bminew*smoker + smoker*factorChildren +age*smoker+ age*children, data = medCost2)
summary(lm7) #0.8573
plot(lm7, which=1)

#taking agenew
lm8<-lm(logcharges~ agenew+bminew+factorChildren+smoker+region+bminew*smoker + smoker*factorChildren +agenew*smoker, data = medCost2)
summary(lm8) #0.79

x<-c(20:60)
k1<-lm6$coefficients["age"]
k2<-lm6$coefficients["age.2"]
k3<-(lm6$coefficients["age:smokeryes"])

kConf1 <- confint(lm6,2)
kConf2 <- confint(lm6,3)
kConf3 <- confint(lm6,18)

plot(x=x,y= exp(lm6$coefficients["age"]+lm6$coefficients["age.2"]+lm6$coefficients["age:smokeryes"]+2*lm6$coefficients["age.2"]*x)-1)

(k1+k2+k3)/(-2*k2)
(k1+k2)/(-2*k2)

ages<- c(20,40,60) 

100*(exp(k1+k2+k3+2*k2*ages)-1) #smoker %increase  1.82,  -0.05%
100*(exp(k1+k2+2*k2*ages)-1 )#non-smoker %increase  5.28, 3.33%



100*(exp(kConf1+kConf2+kConf3+2*kConf2*20)-1) #smoker %increase
100*(exp(kConf1+kConf2+2*kConf2*20)-1 )#non-smoker %increase
100*(exp(kConf1+kConf2+kConf3+2*kConf2*60)-1) #smoker %increase
100*(exp(kConf1+kConf2+2*kConf2*60)-1 )#non-smoker %increase







#smoker
k4<-lm6$coefficients["smokeryes"]
k5<-lm6$coefficients["bminew:smokeryes"]
k6<-(lm6$coefficients["age:smokeryes"])
k7 <- lm6$coefficients["factorChildren1:smokeryes"]
k8 <- lm6$coefficients["factorChildren2:smokeryes"]
k9 <- lm6$coefficients["factorChildren3:smokeryes"]
k10 <- lm6$coefficients["factorChildrenOther:smokeryes"]

#at 0, bmi=1,

bmis <-c(0,1)
childrenV <- c(0,k7,k8,k9,k10)


for (childValue in childrenV)
{
      for(bmiValue in bmis)
      {
           cat("\n \n bmi Value :", bmiValue)
           cat(" ; children :", match(childValue,childrenV) -1)
          cat(" ; Ratio:",(round(exp(k4+k5*bmiValue+k6*50+childValue),2)))
      }
}




 test_medCost<- medCost2[c(1,1,1,1),]
 test_medCost$age = c(60,61,60,61)
 test_medCost$smoker=c("yes","yes","no","no")
 test_medCost$age.2 = test_medCost$age^2
 exp(predict(newdata =test_medCost,lm6))
 
 test_medCost$age = c(20,21,20,21)
 test_medCost$smoker=c("yes","yes","no","no")
 test_medCost$age.2 = test_medCost$age^2
 exp(predict(newdata =test_medCost,lm6))
 
 test_medCost$age = c(40,41,40,41)
 test_medCost$smoker=c("yes","yes","no","no")
 test_medCost$age.2 = test_medCost$age^2
 exp(predict(newdata =test_medCost,lm6))
 