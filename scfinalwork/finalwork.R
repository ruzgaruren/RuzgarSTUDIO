library(readr)

# a -Data visualization and descriptive stats


install.packages("quantmod")
library(quantmod)
install.packages("patchwork")
install.packages("psych")
install.packages("table1")
install.packages("gglop2")
install.packages("reshape2")
library(reshape2)





req <- substitute(require(x, character.only = TRUE))
libs<-c("psych", "ggplot2", "table1", "patchwork")
sapply(libs, function(x) eval(req) || {install.packages(x); eval(req)})
library(ggplot2)
library(psych)
library(table1)
library(patchwork)
library(ggplot2)

#Loading required packages are at under installing packages UsingR and MASS
#install.packages("UsingR")
#install.packages("MASS")
library(MASS)
library(HistData)
library(UsingR)
library(Formula)
library(survival)
library(lattice)
library(Hmisc)
# Require ggplot2 and UsingR
require(UsingR)
require(ggplot2)



#load data 



library(readr)
dagitilan_miktar <- read_csv("C:/Users/winds/Desktop/scfinalwork/dagitilan mama miktari verisi.csv")
View(dagitilan_miktar)


pandemide_dagitilan_miktar <- read_csv("C:/Users/winds/Desktop/scfinalwork/pandemi-surecinde-sokak-hayvanlarina-dagitilan-mama-miktari.csv")


muayene_veri <- read_csv("C:/Users/winds/Desktop/scfinalwork/Muayene Edilen Sokak Hayvanlari  Verisi.csv")


mudahale_veri<- read_csv("C:/Users/winds/Desktop/scfinalwork/Mudahale Edilen Sokak Hayvanlari Verisi.csv")


sahiplendirilen_veri <- read_csv("C:/Users/winds/Desktop/scfinalwork/Sahiplendirilen Sokak Hayvanlari  Verisi.csv")

View(dagitilan_miktar)
View(pandemide_dagitilan_miktar)
View(muayene_veri)
View(mudahale_veri)
View(sahiplendirilen_veri)



#dagýtýlan mama mýktarý STEP1: CHANGE THE COLS NAME


colnames(dagitilan_miktar)<-c("animal_id","year","month","district","town","location","quantity","unit")
ls()
View(dagitilan_miktar)

mean(dagitilan_miktar$quantity)
colnames(pandemide_dagitilan_miktar)<-c("animal_id","year","month","district","town","location","quantity","unit")
ls()
mean(pandemide_dagitilan_miktar$quantity)






# The first 10 observation of our dataset using the print(head(data, n = 10)) function
print(head(dagitilan_miktar, n = 7))
print(tail(pandemide_dagitilan_miktar, n = 10))


# Checking the class of the data set using class(dataset) function
class(dagitilan_miktar)
# Checking the structure of the data set using str(dataset) function
str(pandemide_dagitilan_miktar)

#Summary statistics of data set
summary(pandemide_dagitilan_miktar)

summary(dagitilan_miktar)




# Histogram of quantity 
ggplot(data = dagitilan_miktar, mapping = aes(x = quantity)) +
  geom_histogram(bins = 100, fill = "blue") +
  ggtitle("quantity histogram") +
  theme(plot.title = element_text(hjust = 0.5)) 



# Histogram of Total quantity With Lines
ggplot(data = pandemide_dagitilan_miktar, mapping = aes(x = quantity)) +
  geom_freqpoly(bins = 10) +
  ggtitle("Histogram of Total quantity With Lines") +
  theme(plot.title = element_text(hjust = 0.5))


# Histogram of the Total by using the ggplot2 package
ggplot(data = pandemide_dagitilan_miktar, mapping = aes(x = quantity)) +
  geom_histogram(bins = 35, fill = "green") +
  ggtitle(expression(atop("Histogram of quantity", atop(italic("Total of animals( data from pandemide_dagitilan_miktar)", ""))))) +
  theme(plot.title = element_text(hjust = 0.9)) +
  theme(plot.subtitle = element_text(hjust = 0.5))





# Using  geom_smooth () function
ggplot(data = dagitilan_miktar, mapping = aes(x = month, y = quantity)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Month", y = "quantity") +
  ggtitle(expression(atop("Scatterplot of Months and Animals", atop(italic("count By Ruzgar Üren", ""))))) +
  theme(plot.title = element_text(hjust = 0.4)) +
  theme(plot.subtitle = element_text(hjust = 0.5))





#Use ggplot(), geom_bar() and geom_error bar() to construct the plots

ggplot(dagitilan_miktar, aes(x=month, y=quantity, fill=animal_id)) + 
  geom_bar( position="stack", stat="identity") +
  geom_errorbar(aes(ymin=0, ymax=50000), 
                width=0.7,                    # Width of the error bars
                position=position_dodge(0.1))


x<- rnorm(dagitilan_miktar, mean = 20, sd=5)
par(mfrow=c(2,3))
fiveavarages <- function(x){
  c(avarage=mean(x),trimmed=mean(x, trim = 0.10),median= median(x),
    geometricmean= prod(x)^(1/length(x)),harmonicmean= 1/mean(1/x))
}


hist(x)
qqnorm(x)
qqline(x)
boxplot(x)
hist(x)
qqnorm(x)
qqline(x)
fiveavarages(x)



#pandemide_dagitilan_miktar verisi inceleme

par(mfrow=c(1,1))
plot1<- qplot(x= quantity, data = pandemide_dagitilan_miktar, geom="bar")
plot1

base.plot<- ggplot(pandemide_dagitilan_miktar,aes(x= quantity))+
  xlab("quantity")
base.plot + geom_histogram()
base.plot + geom_histogram(aes(fill=month))


base.plot + geom_density()
base.plot + geom_density(aes(fill=animal_id))
base.plot + geom_density(aes(fill=month), alpha= 0.5)
base.plot + geom_density(aes(fill=quantity), alpha= 0.5)


base.plot1 <- ggplot(pandemide_dagitilan_miktar, aes(x= as.factor(month),y = quantity))+
  xlab("Months")+
  ylab("Quantity- KG")

base.plot1 + geom_boxplot()
base.plot1 + geom_violin()



#---------sahiplendirilen_veri------- PLOTS-------
par(mfrow=c(1,1))
plot2<- qplot(x= ADET, data = sahiplendirilen_veri, geom="bar")
plot2
s2<- rnorm(sahiplendirilen_veri, mean = 10, sd=5)
par(mfrow=c(2,3))
avaragesofsahiplendirilenveri <- function(x){
  c(avarage=mean(x),trimmed=mean(x, trim = 0.10),median= median(x),
    geometricmean= prod(x)^(1/length(x)),harmonicmean= 1/mean(1/x))
}


hist(s2)
qqnorm(s2)
qqline(s2)
boxplot(s2)
hist(s2)
qqnorm(s2)
qqline(s2)
avaragesofsahiplendirilenveri(s2)













# Calculate Linear regression using lm() function
(height.lm <- lm(month ~ quantity, data = dagitilan_miktar))

##CALCULATE LINEAR REGRESSION USING THE lm()FUNCTION
lm(formula = month ~ quantity, data = dagitilan_miktar)


# Complete regression results using summary() function
(summary(height.lm))




#---------------ANOVA AS AN ALTERNATIVE REGRESSION-----------

# Pass tips and package reshape2 to the data() function




View(pandemide_dagitilan_miktar)

tips1<-data.frame(dagitilan_miktar$quantity , pandemide_dagitilan_miktar$quantity)
tips1





str(tips1)

# Pass arguments to aov() function for an ANOVA test
(tips1.anova <- aov(month ~ quantity - 1, data = pandemide_dagitilan_miktar))
View(tips1)
aov(formula = month ~ quantity - 1, data = pandemide_dagitilan_miktar)
# Pass the model tips.anova to summary()function
summary(tips1.anova)
# Linear model using the lm() function
(tips1.lm <- lm(month ~ quantity - 1, data = pandemide_dagitilan_miktar))

# The complete regression output
summary(tips1.lm)
View(tips1)





# Extract them from the summary for tips1.lm
(tips1.Info <- summary(tips1.lm))

lm(formula = month ~ quantity, data = pandemide_dagitilan_miktar)
colnames(tips1)<-c("tmonth","tquantity")
# Extract point estimates for the mean and standard errors of tips per day
(tips1.Coef <- as.data.frame(tips1.Info$coefficients[, 1:2]))












#----------------------------anova another example with other data---------

m<-lm( mudahale_veri$ADET ~ sahiplendirilen_veri$ADET )
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












#--------------------------------------   t-test()  -------------------------

#One sample t-test via t.test() Test whether quantity mean is different than 1000 grams Ho:mu=100 Ha:mu is not 1000 or

#Ho:mu=1000 Ha:mu < 1000

dagitilan_miktar.ci <- t.test(dagitilan_miktar$quantity)$conf.int
dagitilan_miktar.ci


dagitilan_miktar.t.test<- t.test(dagitilan_miktar$quantity, alternative = c("less"), mu = 1000, conf.level = 0.95)
dagitilan_miktar.t.test
names(dagitilan_miktar.t.test)


dagitilan_miktar.t.test$p.value   # p-value  0.7515369
dagitilan_miktar.t.test$null.value   # null value mean 1000 
dagitilan_miktar.t.test$stderr   #stderr  184.4158
dagitilan_miktar.t.test$estimate  # group means
dagitilan_miktar.t.test$data.name   # data name
attr(dagitilan_miktar.t.test$conf.int, "conf.level")  # confidence level




#Let’s compute a summary table


aggregate(month ~ quantity, 
          data = dagitilan_miktar, 
          FUN = function(x) {c(mean = mean(x), sd = sd(x))})


#Two sample t-test via t.test()

aggregate(month ~ quantity, data = dagitilan_miktar, 
          FUN = function(x) {c(mean = mean(x), 
                               se = sd(x) / sqrt(length(x)))})


















#rmarkdown::render("analysis.R")
#rmarkdown::render("analysis.R", "pdf_document")

