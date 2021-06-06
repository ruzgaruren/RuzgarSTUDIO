library(readr)

# a -Data visualization and descriptive stats


install.packages("quantmod")
library(quantmod)
#check 6 row of the data 
head(SahiplendirilenSHV)


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





#load data 



dagitilan_miktar <- read_csv("C:/Users/winds/Desktop/scfinalwork/dagitilan mama miktari verisi.csv")



pandemide_dagitilan_miktar <- read_csv("C:/Users/winds/Desktop/scfinalwork/pandemi-surecinde-sokak-hayvanlarina-dagitilan-mama-miktari.csv")


muayene_veri <- read_csv("C:/Users/winds/Desktop/scfinalwork/Muayene Edilen Sokak Hayvanlari  Verisi.csv")


mudahale_veri<- read_csv("C:/Users/winds/Desktop/scfinalwork/Mudahale Edilen Sokak Hayvanlari Verisi.csv")


sahiplendirilen_veri <- read_csv("C:/Users/winds/Desktop/scfinalwork/Sahiplendirilen Sokak Hayvanlari  Verisi.csv")

View(dagitilan_miktar)
View(pandemide_dagitilan_miktar)
View(muayene_veri)
View(mudahale_veri)
View(sahiplendirilen_veri)




#dagýtýlan mama mýktarý 


colnames(dagitilan_miktar)<-c("animal_id","year","month","district","town","location","quantity","unit")
ls()
View(dagitilan_miktar)

mean(dagitilan_miktar$quantity)



colnames(pandemide_dagitilan_miktar)<-c("animal_id","year","month","district","town","location","quantity","unit")
ls()
mean(pandemide_dagitilan_miktar$quantity)






# The first 10 observation of our dataset using the print(head(data, n = 10)) function
print(head(dagitilan_miktar, n = 10))
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
  geom_histogram(bins = 10, fill = "blue") +
  ggtitle("quantity histogram") +
  theme(plot.title = element_text(hjust = 0.9)) 



# Histogram of total animals - counts with lines
ggplot(data = pandemide_dagitilan_miktar, mapping = aes(x = quantity)) +
  geom_freqpoly(bins = 10) +
  ggtitle("Histogram of Total quantity With Lines") +
  theme(plot.title = element_text(hjust = 0.5))


# Histogram of the Total by using the ggplot2 package
ggplot(data = pandemide_dagitilan_miktar, mapping = aes(x = quantity)) +
  geom_histogram(bins = 30, fill = "seagreen") +
  ggtitle(expression(atop("Histogram of quantity", atop(italic("Total of animals( data from pandemide_dagitilan_miktar)", ""))))) +
  theme(plot.title = element_text(hjust = 0.9)) +
  theme(plot.subtitle = element_text(hjust = 0.5))





# Using  geom_smooth () function
ggplot(data = dagitilan_miktar, mapping = aes(x = month, y = quantity)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Month", y = "quantity") +
  ggtitle(expression(atop("Scatterplot of Months and Animals", atop(italic("count By Ruzgar Üren", ""))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))






# Calculate Linear regression using lm() function
(height.lm <- lm(month ~ quantity, data = dagitilan_miktar))

##CALCULATE LINEAR REGRESSION USING THE lm()FUNCTION
lm(formula = month ~ quantity, data = dagitilan_miktar)


# Complete regression results using summary() function
(summary(height.lm))




#ANOVA AS AN ALTERNATIVE REGRESSION

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














