muayene<- read_csv("C:/Users/winds/Desktop/G/1- CENG 3516 Statistical Computing 2020-2021 Bahar/çalışma/sokakhayvanları/Muayene Edilen Sokak Hayvanları Verisi.csv")

mudahale<- read_csv("C:/Users/winds/Desktop/G/1- CENG 3516 Statistical Computing 2020-2021 Bahar/çalışma/sokakhayvanları/Müdahale Edilen Sokak Hayvanları Verisi.csv")



colnames(muayene)<-c("animal_id","year","month","condition","total")
colnames(mudahale)<-c("animal_id","year","month","condition","total")
mean(muayene$total)
mean(mudahale$total)
sd(muayene$animal_id)

#create boxplot to show month-total of animals


qplot(x=month, y= total,
      geom="boxplot", data = muayene,
      xlab=" Month",
      ylab="Total number of  animals",
      fill= I("lightblue"))



qplot(aes(x=muayene$total, y= mudahale$total),
      geom="boxplot", data = muayene,
      xlab=" muayene-total",
      ylab="mudahale-total",
      fill= I("red"))

#Loading required packages are at under installing packages UsingR and MASS
#install.packages("UsingR")
#install.packages("MASS")
library(MASS)
library(HistData)
library(UsingR)
library(ggplot2)
library(Formula)
library(survival)
library(lattice)
library(Hmisc)
# Require ggplot2 and UsingR
require(UsingR)
require(ggplot2)

# The first 10 observation of our dataset using the print(head(data, n = 10)) function
print(head(muayene, n = 10))
print(tail(mudahale, n = 10))




# Checking the class of the data set using class(dataset) function
class(mudahale)
# Checking the structure of the data set using str(dataset) function
str(muayene)

#Summary statistics of father.son data set
summary(muayene)



# Histogram of total animals
ggplot(data = muayene, mapping = aes(x = total)) +
  geom_histogram(bins = 10, fill = "blue") +
  ggtitle("Total Animals") +
  theme(plot.title = element_text(hjust = 0.9)) 





# Histogram of total animals - counts with lines
ggplot(data = mudahale, mapping = aes(x = total)) +
  geom_freqpoly(bins = 10) +
  ggtitle("Histogram of Total Counts With Lines") +
  theme(plot.title = element_text(hjust = 0.5))




# Histogram of the Total by using the ggplot2 package
ggplot(data = muayene, mapping = aes(x = total)) +
  geom_histogram(bins = 30, fill = "seagreen") +
  ggtitle(expression(atop("Histogram of Total", atop(italic("Total of animals( data from muayene)", ""))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))



# Using  geom_smooth () function
ggplot(data = muayene, mapping = aes(x = month, y = total)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Month", y = "Total Animals") +
  ggtitle(expression(atop("Scatterplot of Months and Animals", atop(italic("count By Ruzgar Üren", ""))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))










# Calculate Linear regression using lm() function
(height.lm <- lm(month ~ total, data = muayene))

##CALCULATE LINEAR REGRESSION USING THE lm()FUNCTION
lm(formula = month ~ total, data = muayene)


# Complete regression results using summary() function
(summary(height.lm))






#ANOVA AS AN ALTERNATIVE REGRESSION

# Pass tips and package reshape2 to the data() function

#install.packages("reshape2")
library(reshape2)
View(muayene)

tips1<-data.frame(muayene$month , mudahale$total)
tips1
str(tips1)

# Pass arguments to aov() function for an ANOVA test
(tips1.anova <- aov(month ~ total - 1, data = muayene))
View(tips1)
aov(formula = month ~ total - 1, data = muayene)
# Pass the model tips.anova to summary()function
summary(tips1.anova)
# Linear model using the lm() function
(tips1.lm <- lm(month ~ total - 1, data = muayene))

# The complete regression output
summary(tips1.lm)

















# Install the plyr package : install it and then load it
library(plyr)
require(plyr)












