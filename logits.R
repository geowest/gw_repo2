
## WEEK 9

############################################

## Class Examples

setwd("/Users/Geoboo/Desktop/")
data1<-read.csv("vaccine.csv",header=TRUE)
attach(data1)
data2<-na.omit(data.frame(knowl2,education,income,age,gender,ideology,party))
detach(data1)
attach(data2)
library(car)
sideeff<-knowl2
f.gender<-factor(gender,levels=c(0,1),labels=c("Female","Male"))

## Binomial logit analysis
model1<-glm(sideeff~education+income+age+f.gender,family=binomial)
summary(model1)

# Model1 pseudo R-squared
1-(1393.2/1418.1)

# Recode "party"
table(party)
r.party<-recode(party,"1=0;2=1;else=2")
table(r.party)
f.party<-factor(r.party,levels=c(0,1,2),labels=c("Democrats","Republicans","Others"))
model2<-glm(sideeff~education+income+age+f.gender+ideology+f.party,family=binomial)
summary(model2)

# Model2 pseudo R-squared
1-(1391.0/1418.1)

# Model comparisons
anova(model1,model2,test='Chisq')

# Reversing logits
model3<-glm(sideeff~education+age+f.gender,family=binomial)
summary(model3)

# Mean of control variables (age and f.gender)
summary(data.frame(age,gender)) #mean(age)=45.10 and mean(gender)=0.48
# Note: We need to get the mean of "gender", not the mean of "f.gender" here. 

# Calculate L
L<-0.023636-0.165529*education-0.011041*45.10+0.348582*0.48

# Calculate the anti-log of L
P<-1/(1+exp(-L))
C<-cbind(education,L,P)
C[1:10,]

# plot it
plot(education,P,ylab="Fitted Probability of Y=1")

# Easier way to do the same thing
library(effects)
plot(effect("education",model3))

# All effects
plot(allEffects(model3),ask=FALSE)

# Predicting probability of a specific type of case
# Type1
p1<-predict(model3,expand.grid(education=7,age=55,f.gender="Male"), type="response")
p1

# Type2
p2<-predict(model3,expand.grid(education=1,age=18,f.gender="Female"), type="response")
p2

# People of different ages
p3<-predict(model3,expand.grid(education=1,age=c(30,40,50),f.gender="Female"), type="response")
p3

###########################################

## Multinomial logistic regression

# Cleaning up
rm(list=ls())
detach(data2)
data1<-na.omit(read.csv("vaccine.csv",header=TRUE))
attach(data1)

# Recode/relevel "party"
table(party)
r.party<-recode(party,"4='NA'")
table(r.party)
f.party<-factor(r.party,levels=c(1,2,3),labels=c("Democrats","Republicans","Independents"))
table(f.party)
f.party1<-relevel(f.party,ref="Independents")
table(f.party1)

# Run the regression
install.packages("nnet")
library("nnet")
model1<-multinom(f.party1~education+income+age) 
summary(model1,Wald=TRUE)
Anova(model1)

model2<-multinom(f.party1~ideology+education+income+age)
summary(model2,Wald=TRUE)
Anova(model2)

# Model comparison
anova(model1,model2)

# Effect plots
plot(effect("ideology",model2))
plot(allEffects(model2),ask=FALSE)
