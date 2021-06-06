
#Regression and ANOVA

#MUA MUAYENE EDÝLEN  sokak hayvanlarý verisi
#MUD MUDAHALE EDÝLEN  sokak hayvanlarý verisi

mua<- read_csv("C:/Users/winds/Desktop/G/1- CENG 3516 Statistical Computing 2020-2021 Bahar/çalýþma/sokakhayvanlarý/Muayene Edilen Sokak Hayvanlarý Verisi.csv")

mud<- read_csv("C:/Users/winds/Desktop/G/1- CENG 3516 Statistical Computing 2020-2021 Bahar/çalýþma/sokakhayvanlarý/Müdahale Edilen Sokak Hayvanlarý Verisi.csv")



colnames(mua)<-c("animal_id","year","month","condition","total")
colnames(mud)<-c("animal_id","year","month","condition","total")
mean(mua$total)
mean(mud$total)
sd(mua$animal_id)











#use set.seed to set the random number generation seed

set.seed(100)
x <- rnorm(mua$total)
e <- rnorm(mua$month, mean=0, sd=5)
y <- 5 + 15 * x + e
y

#The lm function performs a linear regression and reports the coefficients.

lm(mua$total ~ mud$total)
lm(month ~ total, data = mua)
lm(month ~ total, data = mud)



muad <- data.frame(mua$total , mud$total)
head(muad)
View(muad)





m<-lm(mua$total ~ mud$total)
## anova
anova(m)

##Model coefficients
coefficients(m)
coef(m)

##Confidence intervals for the regression coefficients

confint(m)


#Residual sum of squares

deviance(m)


##Key statistics, such as R2, the F statistic, and the residual standard error (??)
summary(m)




##Variance–covariance matrix of the main parameters
vcov(m)

anova(m)



###This shows how lm was called when it created the model
summary(m)$call


















