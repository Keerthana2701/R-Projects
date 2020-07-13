setwd("D:/ShakthiSai -Keerthana BABI and Learnings/PGP-BABI-Great Lakes/Chapter 3 Advanced Statistics/AS Project")
getwd()

install.packages("nFactors")
library(readr)
library(psych)
library(ggplot2)
library(reshape2)
library(corrplot)
library(caTools)
library(nFactors)
library(caTools)

mydataset=read.csv("Factor-Hair-Revised.csv",header=TRUE)
View(mydataset)
attach(mydataset)
summary(mydataset)
names(mydataset)

str(mydataset)



##charts

plot(mydataset$Satisfaction,type="l")
barplot(mydataset$ProdQual,col ="Red")


## to see the outliers using boxplot

boxplot(mydataset$ProdQual,horizontal = T)
boxplot(mydataset$Ecom,horizontal = T)
boxplot(mydataset$TechSup,horizontal = T)
boxplot(mydataset$CompRes,horizontal = T)
boxplot(mydataset$Advertising,horizontal = T)
boxplot(mydataset$ProdLine,horizontal = T)
boxplot(mydataset$SalesFImage,horizontal = T)
boxplot(mydataset$ComPricing,horizontal = T)
boxplot(mydataset$WartyClaim,horizontal = T)
boxplot(mydataset$OrdBilling,horizontal = T)
boxplot(mydataset$DelSpeed,horizontal = T)
boxplot(mydataset$Satisfaction,horizontal = T)



##to see the value of outliers
boxplot(mydataset$ProdQual)$out
boxplot(mydataset$Ecom)$out
boxplot(mydataset$TechSup)$out
boxplot(mydataset$CompRes)$out
boxplot(mydataset$Advertising)$out
boxplot(mydataset$ProdLine)$out
boxplot(mydataset$SalesFImage)$out
boxplot(mydataset$ComPricing)$out
boxplot(mydataset$WartyClaim)$out
boxplot(mydataset$OrdBilling)$out
boxplot(mydataset$DelSpeed)$out
boxplot(mydataset$Satisfaction)$out


## the variables Ecom,Delspeed,OrdBilling,SalesFimage has outliers

##Summary of the dataset


summary(mydataset)

##summary of independent variables 

summary(mydataset$ProdQual)
summary(mydataset$Ecom)
summary(mydataset$TechSup)
summary(mydataset$CompRes)
summary(mydataset$Advertising)
summary(mydataset$ProdLine)
summary(mydataset$SalesFImage)
summary(mydataset$ComPricing)
summary(mydataset$WartyClaim)
summary(mydataset$OrdBilling)
summary(mydataset$DelSpeed)
summary(mydataset$Satisfaction)


## finding missing values

missing.values=sum(is.na(mydataset))
missing.values


## Multicollinearity



## corelation in dataset -checking for pair wise correlation -evidence of multicolliniarity

mydataset.correlation=cor(mydataset[,2:13])
mydataset.correlation
print(mydataset.correlation,digits=4)
corrplot(mydataset.correlation)



## Simple linear regression with customer satisfaction as dependent var with other indep var

r1=lm(Satisfaction~ProdQual)
summary(r1)

r2=lm(Satisfaction~Ecom)
summary(r2)

r3=lm(Satisfaction~TechSup)
summary(r3)

r4=lm(Satisfaction~CompRes)
summary(r4)


r5=lm(Satisfaction~Advertising)
summary(r5)

r6=lm(Satisfaction~ProdLine)
summary(r6)

r7=lm(Satisfaction~SalesFImage)
summary(r7)

r8=lm(Satisfaction~ComPricing)
summary(r8)

r9=lm(Satisfaction~WartyClaim)
summary(r9)

r10=lm(Satisfaction~OrdBilling)
summary(r10)


r11=lm(Satisfaction~DelSpeed)
summary(r11)


## Analysis of fit:

pred=predict(r11)
act=Satisfaction
b=data.frame(pred,act)
b
plot(act,col="Red",xlab="Actual vs predicted")
lines(act,col="Red")
lines(pred,col="Blue")



## are these statistically significant when we run individually? forthis lets check the multiple R 
## squared results

##  Multiple Regression

multiple.regression=lm(Satisfaction~ProdQual+Ecom+TechSup+CompRes+Advertising+ProdLine+SalesFImage+ComPricing+WartyClaim+OrdBilling+DelSpeed)
summary(multiple.regression)


## PCA /FA with 4 facors and naming the factors

View(mydataset)
mydataset.correlation
ev=eigen(mydataset.correlation)
print(ev,digits=5)
Eigen.value=ev$values
Eigen.value

## here , we get 4 eigen values >1 .so no of factors =4

factors=c(1,2,3,4)
scree.plot=data.frame(factors,Eigen.value)
scree.plot
plot(scree.plot,main="screen plot factors",col="blue")
lines(scree.plot,col="Red")
 

##fa

fa1<- fa(r=mydataset, nfactors = 4, rotate="varimax",fm="pa")
print(fa1)
fa.diagram(fa1)

## to get PC factors

unrotate=principal(mydataset,nfactors=4,rotate="none")
print(unrotate,digits=4)


unrotate.plotting=plot(unrotate,row.names(unrotate$loadings))


## since they are significantly not different,perform rotation

rotate=principal(mydataset,nfactors=4,rotate="varimax")
print(rotate,digits=4)

rotate.plotting=plot(rotate,row.names(rotate$loadings))

## from this, PC1 has Complaint Resolution, Delivery speed and order billing
##  grouped as factor 1
##PC2 has Salesforceimage,E-commerce,advertisment grouped as factor 2
## PC3 has technical support,Warranty claims as factor 3
## PC4 has product quality as factor4

## naming factors:
# Factor 1: Good Product service factor
## Factor 2:internet  sales factor
## Factor 3: technical assistance factor
## Factor 4: Product quality factor

## 4 factors obtained

## take rotated score


##Create a data frame with a minimum of 5 columns, 4 of which are different factors 
##and the 5th column is Customer Satisfaction

##using PCA
mydf=data.frame(mydataset[13],rotate$scores)
mydf

##using FA
mydf1=data.frame(mydataset[13],fa1$scores)
mydf1


##Multiple linear regression with customer satisfaction as dependent variables 
##and the four factors as independent variables. 

multiple.linear.regression=lm(Satisfaction~.,mydf)
summary(multiple.linear.regression)
anova(multiple.linear.regression)













