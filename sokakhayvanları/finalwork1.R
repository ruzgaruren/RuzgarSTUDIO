#Select a theme. My example is about animals

#. Find relevant datasets from the portal
      #DagitilanMamaMiktari
      #MuayeneEdilenSokakHayvanlariVerisi
      #SahiplendirilenSokakHayvanlariVerisi
                      #I choose the titles to interest in... 

# Lets Import data 
# SHV is "sokak hayvanlari verisi"

DagitilanMamaSHV <- read.table("C:/Users/winds/Desktop/G/1- CENG 3516 Statistical Computing 2020-2021 Bahar/çalisma/sokakhayvanlari/Dagitilan Mama Miktari.csv", 
                 header = FALSE,
                 sep = ",")



MuayeneEdilenSHV <- read.table("C:/Users/winds/Desktop/G/1- CENG 3516 Statistical Computing 2020-2021 Bahar/çalisma/sokakhayvanlari/Muayene Edilen Sokak Hayvanlari Verisi.csv", 
                                   header = FALSE,
                                   sep = ",")


SahiplendirilenSHV <- read.table("C:/Users/winds/Desktop/G/1- CENG 3516 Statistical Computing 2020-2021 Bahar/çalisma/sokakhayvanlari/Sahiplendirilen Sokak Hayvanlari Verisi.csv", 
                                                 header = FALSE,
                                                 sep = ",")
View(DagitilanMamaSHV)
View(MuayeneEdilenSHV)
View(SahiplendirilenSHV)






colnames(DagitilanMamaSHV)<-c("animal_id","year","mounth","district","town","location","quantity","unit")
ls()
View(DagitilanMamaSHV)

mean(DagitilanMamaSHV$quantity)



ggplot(DagitilanMamaSHV, )



# a -Data visualization and descriptive stats


install.packages("quantmod")
library(quantmod)
#check 6 row of the data 
head(SahiplendirilenSHV)


install.packages("patchwork")
install.packages("psych")
install.packages("table1")
install.packages("gglop2")









req <- substitute(require(x, character.only = TRUE))
libs<-c("psych", "ggplot2", "table1", "patchwork")
sapply(libs, function(x) eval(req) || {install.packages(x); eval(req)})
library(ggplot2)
library(psych)
library(table1)
library(patchwork)
library(ggplot2)








 # b. Confidence intervals like: compare confidence intervals of traffic fines for Region A and
 #Region B.



























# c. Hypothesis testing Generate at least 3 hypothesis and test them. Assume normality for each group if normality check fails







































 # d. ANOVA or Regression Example: Are there significant differences between mean waiting times of different Subway locations?










































