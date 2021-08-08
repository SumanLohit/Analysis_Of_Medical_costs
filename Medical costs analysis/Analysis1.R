myTable= matrix(c(19, 32, 42, 30), nrow=2, ncol=2)
colnames(myTable) <- c("Yes","No")
rownames(myTable) <- c("Preschool","Control")

medCost <- read.csv("insurance.csv")
str(medCost)
factor_vars <- c('sex','smoker','region')
medCost[factor_vars] <- lapply(medCost[factor_vars], function(x) as.factor(x))
library(mosaic)
favstats(charges~sex,data=medCost)
favstats(charges~region, data=medCost)
summary(medCost)
#summaryBy(PctChange ~ SpeedLimit, data = ex0223,   FUN = list(mean, max, min, median, sd)) 


cor.matrix<-cor(medCost[-c(2,5,6)], method = "pearson")
library(corrplot)
library(RColorBrewer)
corrplot(cor.matrix, type = "lower",method = "square", order= "hclust",
         tl.col = "black", tl.s= 45,  col = brewer.pal(n=5, name = "RdYlBu"),
         addCoefasPercent = TRUE)

#this is better
ggcorrplot(cor.matrix, hc.order=TRUE,lab=TRUE,type = "lower",
           outline.color = "white", ggtheme = ggplot2::theme_gray,
           colors = brewer.pal(n = 3, name = "RdYlBu"), tl.cex = 8, lab_size = 3)

library(PerformanceAnalytics)
cor_data<-medCost[, c(1,3,4,7)]
chart.Correlation(cor_data, histogram=TRUE, pch=19)
cor_logData <- cor_data
cor_logData$charges <- log(cor_logData$charges)
chart.Correlation(cor_logData, histogram=TRUE, pch=19)

attach(medCost)
pairs(cor_data, pch = 19)

library(moments)
skewness(log(charges))   #log transformation works
kurtosis(log(charges))

skewness(children)
skewness(log(children))   #not possible
kurtosis(children)
sum(is.na(medCost)) #not working

sum(is.na(charges))
sum(is.na(age))
sum(is.na(bmi))
sum(is.na(children))

shapiro.test(charges)

#F test
var.test(log(charges) ~ sex, data= medCost, alternative = "two.sided") #unequal var
t.test(log(charges) ~ sex, data= medCost)   
#true means not 0. female < male

var.test(charges ~ smoker, data= medCost, alternative = "two.sided") #unequal var
t.test(charges ~ smoker, data= medCost) #welch 2 sample   
#group no < group yes

#Anova- northeast, northwest, southeast, southwest
anova(lm(charges ~ region, data= medCost))
#group means different

#tukey kramer
myAov<-aov(log(charges) ~ region) #gives same answer as anova
plot(myAov, which=1) #3 outliers, no pattern
plot(myAov, which=2)  #model is not good
plot(myAov, which=4) #544, 578, 1231 outliers
summary(myAov)

require(multcomp)
tukey<- glht(myAov, linfct= mcp(region= "Tukey"))
summary(tukey)
#means of southwest and southeast are different. Others are equal

#regression
charges_lm <- lm(charges~., data=medCost)
plot(charges_lm, which=1) #not good

medCost$logcharges <- log(medCost$charges)
charges_lm1 <- lm(logcharges~age+sex+bmi+children+smoker+region+sex*smoker)
summary(charges_lm1)  #all significant
plot(charges_lm1, which=1)
plot(charges_lm1, which=2)
plot(charges_lm1, which=4)

smallest <- logcharges ~ 1 #no variables
biggest <- logcharges ~ age+sex+bmi+children+smoker+region+
           children*sex+ sex*smoker +
  age*smoker +age*children +age*sex + age.2+ age*smoker*children + 
  region*age +region*smoker*age+ age*sex*children*smoker*region
m <- lm(logcharges ~ age, data=medCost1)
stats::step(m, scope=list(lower=smallest, upper=biggest)) 

aic<- lm(logcharges ~ age + smoker + children + bmi + region + 
           sex + age.2 + age:smoker + age:children + smoker:children + 
           smoker:region + smoker:sex + age:region + age:sex + age:smoker:children + 
           age:smoker:region + age:smoker:sex, data = medCost1)
summary(aic)
library(faraway)
m_aic<-step(aic, direction = "backward")


ggplot(medCost,aes(x=age, y= log(charges)))+geom_point()
ggplot(medCost,aes(x=bmi, y= log(charges)))+geom_point()

medCost$age.2 <- medCost$age^2
charges_lm_age2 <- lm(logcharges~age+sex+bmi+children+smoker+region+sex*smoker +
                    age*smoker +age*children +age*sex + age*smoker*children, 
                      data = medCost)
summary(charges_lm_age2) #81.6
plot(charges_lm_age2, which=1)
plot(charges_lm_age2, which=4)

#431,517, 1028 outliers. #2-term, square, 3-term interaction
#this is giving better result than step aic
medCost1<- medCost[-c(431,517,1028, 103),]
charges_lm2 <- lm(logcharges~age+sex+bmi+children+smoker+region+sex*smoker +
                    age*smoker +age*children +age*sex + age.2+ age*smoker*children + 
                    region*age +region*smoker*age, data = medCost1)
summary(charges_lm2) #84.15
plot(charges_lm2, which=1)
plot(charges_lm2, which=4)




#Divide bmi into 2 levels
medCost2= medCost1
medCost2$bminew<- ifelse(medCost2$bmi > 30, 1, 0)
charges_lm3 <- lm(logcharges~age+sex+bminew+children+smoker+region+sex*smoker +
                    age*smoker +age*children +age*sex + age.2+ age*smoker*children + 
                    region*age +region*smoker*age, data = medCost2)  
summary(charges_lm3)
plot(charges_lm3, which=1)

smallest3 <- logcharges ~ 1 #no variables
biggest3 <- logcharges ~ age+sex+bminew+children+smoker+region+
  children*sex+ sex*smoker +
  age*smoker +age*children +age*sex + age.2+ age*smoker*children + 
  region*age +region*smoker*age+ age*sex*children*smoker*region+ smoker*bminew
m3 <- lm(logcharges ~ age, data=medCost2)
stats::step(m3, scope=list(lower=smallest3, upper=biggest3))

charges_lm4 <- lm(logcharges ~ age + smoker + children + bminew + 
                region + sex + age.2 + age:smoker + age:children + smoker:bminew + 
                smoker:children + age:sex + age:region + smoker:region + 
                smoker:sex + age:smoker:children + age:smoker:sex + age:smoker:region, 
                data = medCost2)  #86.24
summary(charges_lm4)
plot(charges_lm4, which=2)

attach(medCost2)
medCost2$factorChildren <- ifelse(children==0, "0",ifelse(children ==1, "1",ifelse(children == 2,"2", ifelse(children==3, "3","Other"))))

lm<- lm(logcharges~ age+ age.2+smoker+age*smoker+bminew+ bminew*smoker+children+region+sex+smoker*children +age*sex +region*smoker, data=medCost2)
summary(lm) #85.49
#this is better
lm<- lm(logcharges~ age+ age.2+smoker+age*smoker+bminew+ bminew*smoker+factorChildren+region+sex+smoker*factorChildren +age*sex +region*smoker +age*factorChildren , data= medCost2)
summary(lm) #85.82


plot(lm, which=1)
round(lm$coefficients, 3)
options(round= 3)

charges_lm4 <- lm(logcharges ~ age + smoker + factorChildren + bminew + 
                    region + sex + age.2 + age:smoker + age:factorChildren + smoker:bminew + 
                    smoker:factorChildren + age:sex + age:region + smoker:region + 
                    smoker:sex + age:smoker:factorChildren + age:smoker:sex, 
                  data = medCost2)
summary(charges_lm4) #86.44



#age to factor
medCost2$agenew<- ifelse(medCost2$age > 55, 1, 0)
agelm<-lm(logcharges~age+age.2+age*smoker+age*factorChildren +age*sex+age*region+age*bminew, data=medCost2)
summary(agelm)
plot(agelm, which=1)

age_lm<- lm(logcharges ~age)
age_lm1<- lm(logcharges~factor(age))
anova(age_lm,age_lm1)
summary(agelm)
agelm1<- lm(logcharges ~agenew+agenew*smoker+agenew*factorChildren+agenew*sex+agenew*region+agenew*bmi)
summary(agelm1)
anova(agelm,agelm1) #age regression is not a good representation



exp(0.0291)
exp(0.0621)
exp(0.0621-0.00468)
exp(0.0621-0.01375)
exp(0.0621-0.0083)
exp(0.0621-0.01903)
exp(0.0621-0.00468-0.033)
exp(0.0621-0.0083-0.0083)
exp(0.0621-0.01375-0.033)
exp(0.0621-0.01903-0.033)
exp(0.0621-0.0083-0.033)

lm1<- lm(logcharges~ age+ age.2+smoker+age*smoker+bminew+ bminew*smoker+
           factorChildren+region+sex +age*sex +region*smoker , data= medCost2)
summary(lm1) #11 variables, 85.04
plot(lm2, which=1)

medCost2$agenew <- ifelse(age<40,"Young",ifelse(age<55,"Middle","Old"))
lm2 <- lm(logcharges ~ . -charges-bmi-children +bminew*smoker + smoker*factorChildren +agenew*smoker -agenew,data = medCost2)
summary(lm2)
#Middle age
  # bmi low , children 0
exp(lm2$coefficients["smokeryes"])
# bmi low , children 1
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren1"])

# bmi low , children 2
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren2"])

# bmi low , children 3
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren3"])

# bmi low , children >3
exp(lm2$coefficients["smokeryes"]++lm2$coefficients["smokeryes:factorChildrenOther"])

# bmi 1 , children 0
exp(lm2$coefficients["smokeryes"] + lm2$coefficients["bminew"])
# bmi 1 , children 1
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren1"]+ lm2$coefficients["bminew"])

# bmi 1 , children 2
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren2"]+ lm2$coefficients["bminew"])

# bmi 1 , children 3
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren3"]+ lm2$coefficients["bminew"])

# bmi 1 , children >3
exp(lm2$coefficients["smokeryes"]++lm2$coefficients["smokeryes:factorChildrenOther"]+ lm2$coefficients["bminew"])


#young age
#bmi low
exp(lm2$coefficients["smokeryes"]+ lm2$coefficients["smokeryes:agenewYoung"]) 
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren1"]+ lm2$coefficients["smokeryes:agenewYoung"])
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren2"]+ lm2$coefficients["smokeryes:agenewYoung"])
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren3"]+ lm2$coefficients["smokeryes:agenewYoung"])
exp(lm2$coefficients["smokeryes"]++lm2$coefficients["smokeryes:factorChildrenOther"]+ lm2$coefficients["smokeryes:agenewYoung"])

#bmi high
exp(lm2$coefficients["smokeryes"] + lm2$coefficients["bminew"]+ lm2$coefficients["smokeryes:agenewYoung"])
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren1"]+ lm2$coefficients["bminew"]+ lm2$coefficients["smokeryes:agenewYoung"])
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren2"]+ lm2$coefficients["bminew"]+ lm2$coefficients["smokeryes:agenewYoung"])
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren3"]+ lm2$coefficients["bminew"]+ lm2$coefficients["smokeryes:agenewYoung"])
exp(lm2$coefficients["smokeryes"]++lm2$coefficients["smokeryes:factorChildrenOther"]+ lm2$coefficients["bminew"]+ lm2$coefficients["smokeryes:agenewYoung"])

#old age
#bmi low
exp(lm2$coefficients["smokeryes"]+ lm2$coefficients["smokeryes:agenewOld"]) 
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren1"]+ lm2$coefficients["smokeryes:agenewOld"])
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren2"]+ lm2$coefficients["smokeryes:agenewOld"])
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren3"]+ lm2$coefficients["smokeryes:agenewOld"])
exp(lm2$coefficients["smokeryes"]++lm2$coefficients["smokeryes:factorChildrenOther"]+ lm2$coefficients["smokeryes:agenewOld"])

#bmi high
exp(lm2$coefficients["smokeryes"] + lm2$coefficients["bminew"]+ lm2$coefficients["smokeryes:agenewOld"])
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren1"]+ lm2$coefficients["bminew"]+ lm2$coefficients["smokeryes:agenewOld"])
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren2"]+ lm2$coefficients["bminew"]+ lm2$coefficients["smokeryes:agenewOld"])
exp(lm2$coefficients["smokeryes"]+lm2$coefficients["smokeryes:factorChildren3"]+ lm2$coefficients["bminew"]+ lm2$coefficients["smokeryes:agenewOld"])
exp(lm2$coefficients["smokeryes"]++lm2$coefficients["smokeryes:factorChildrenOther"]+ lm2$coefficients["bminew"]+ lm2$coefficients["smokeryes:agenewOld"])
