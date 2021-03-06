


#dmmshv dag�t�lan mama miktari sokak hayvanlar� verisi

dmmshv<- read_csv("C:/Users/winds/Desktop/G/1- CENG 3516 Statistical Computing 2020-2021 Bahar/�al��ma/sokakhayvanlar�/Da��t�lan Mama Miktar�.csv")
colnames(dmmshv)<-c("animal_id","year","mounth","district","town","location","quantity","unit")
ls()
View(dmmshv)
mean(dmmshv$quantity)
mean(dmmshv$mounth)
sd(dmmshv$quantity)



dmmshv.ci <- t.test(dmmshv$quantity)$conf.int
dmmshv.ci


#--------------------------------------   t-test()  -------------------------

#One sample t-test via t.test() Test whether quantity mean is different than 1000 grams Ho:mu=100 Ha:mu is not 1000 or

#Ho:mu=1000 Ha:mu < 1000
dmmshv.t.test<- t.test(dmmshv$quantity, alternative = c("less"), mu = 1000, conf.level = 0.95)
dmmshv.t.test
names(dmmshv.t.test)


dmmshv.t.test$p.value   # p-value

dmmshv.t.test$estimate  # group means

attr(dmmshv.t.test$conf.int, "conf.level")  # confidence level






# Create boxplot showing - quantity - Mounths
qplot(x = mounth, y = quantity,
      geom = "boxplot", data = dmmshv,
      xlab = "Mounth", 
      ylab = "Quantity (grams)",
      fill = I("lightblue"))



#Let�s compute a summary table


aggregate(mounth ~ quantity, 
          data = dmmshv, 
          FUN = function(x) {c(mean = mean(x), sd = sd(x))})


#Two sample t-test via t.test()

aggregate(mounth ~ quantity, data = dmmshv, 
          FUN = function(x) {c(mean = mean(x), 
                               se = sd(x) / sqrt(length(x)))})






#Use ggplot(), geom_bar() and geom_error bar() to construct the plots

ggplot(dmmshv, aes(x=mounth, y=quantity, fill=animal_id)) + 
  geom_bar( position="stack", stat="identity") +
  geom_errorbar(aes(ymin=0, ymax=50000), 
                width=0.7,                    # Width of the error bars
                position=position_dodge(0.1))


x<- rnorm(dmmshv, mean = 20, sd=5)
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





par(mfrow=c(1,1))
plot1<- qplot(x= quantity, data = dmmshv, geom="bar")
plot1

base.plot<- ggplot(dmmshv,aes(x= quantity))+
  xlab("quantity")
base.plot + geom_histogram()
base.plot + geom_histogram(aes(fill=mounth))


base.plot + geom_density()
base.plot + geom_density(aes(fill=animal_id))
base.plot + geom_density(aes(fill=mounth), alpha= 0.5)
base.plot + geom_density(aes(fill=quantity), alpha= 10.5)


base.plot1 <- ggplot(dmmshv, aes(x= as.factor(mounth),y = quantity))+
  xlab("Months")+
  ylab("Quantity- grams")

base.plot1 + geom_boxplot()
base.plot1 + geom_violin()

