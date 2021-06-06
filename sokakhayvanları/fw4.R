mua<- read_csv("C:/Users/winds/Desktop/G/1- CENG 3516 Statistical Computing 2020-2021 Bahar/çalýþma/sokakhayvanlarý/Muayene Edilen Sokak Hayvanlarý Verisi.csv")

mud<- read_csv("C:/Users/winds/Desktop/G/1- CENG 3516 Statistical Computing 2020-2021 Bahar/çalýþma/sokakhayvanlarý/Müdahale Edilen Sokak Hayvanlarý Verisi.csv")



colnames(mua)<-c("animal_id","year","month","condition","total")
colnames(mud)<-c("animal_id","year","month","condition","total")
mean(mua$total)
mean(mud$total)
sd(mua$animal_id)

#create boxplot to show month-total of animals


qplot(x=month, y= total,
       geom="boxplot", data = mua,
       xlab=" Month",
       ylab="Total number of  animals",
       fill= I("lightblue"))



qplot(aes(x=mua$total, y= mud$total),
      geom="boxplot", data = mua,
      xlab=" mua-total",
      ylab="mud-total",
      fill= I("lightblue"))

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
print(head(mua, n = 10))
print(tail(mud, n = 10))




# Checking the class of the data set using class(dataset) function
class(mud)
# Checking the structure of the data set using str(dataset) function
str(mua)

#Summary statistics of father.son data set
summary(mua)



# Histogram of total animals
ggplot(data = mua, mapping = aes(x = total)) +
  geom_histogram(bins = 10, fill = "blue") +
  ggtitle("Total Animals") +
  theme(plot.title = element_text(hjust = 0.9)) 





# Histogram of total animals - counts with lines
ggplot(data = mud, mapping = aes(x = total)) +
  geom_freqpoly(bins = 10) +
  ggtitle("Histogram of Total Counts With Lines") +
  theme(plot.title = element_text(hjust = 0.5))




# Histogram of the Total by using the ggplot2 package
ggplot(data = mua, mapping = aes(x = total)) +
  geom_histogram(bins = 30, fill = "seagreen") +
  ggtitle(expression(atop("Histogram of Total", atop(italic("Total of animals( data from mua)", ""))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))



# Using  geom_smooth () function
ggplot(data = mua, mapping = aes(x = month, y = total)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Month", y = "Total Animals") +
  ggtitle(expression(atop("Scatterplot of Months and Animals", atop(italic("count By Ruzgar Üren", ""))))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))










# Calculate Linear regression using lm() function
(height.lm <- lm(month ~ total, data = mua))

##CALCULATE LINEAR REGRESSION USING THE lm()FUNCTION
lm(formula = month ~ total, data = mua)


# Complete regression results using summary() function
(summary(height.lm))






#ANOVA AS AN ALTERNATIVE REGRESSION

# Pass tips and package reshape2 to the data() function

#install.packages("reshape2")
library(reshape2)
View(mua)

tips1<-data.frame(mua$month , mud$total)
tips1
str(tips1)

# Pass arguments to aov() function for an ANOVA test
(tips1.anova <- aov(month ~ total - 1, data = mua))
View(tips1)
aov(formula = month ~ total - 1, data = mua)
# Pass the model tips.anova to summary()function
summary(tips1.anova)
# Linear model using the lm() function
(tips1.lm <- lm(month ~ total - 1, data = mua))

# The complete regression output
summary(tips1.lm)

















# Install the plyr package : install it andd then load it
library(plyr)
require(plyr)












