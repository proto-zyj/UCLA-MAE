#
setwd("C:/Users/zyj37/Desktop/MAE/ECON 430")
car= read.csv("Project/car.csv")

# 
library(tidyverse)
library(car)
library(GGally)
library(corrplot)
library(mice)
library(lmtest)
library(leaps)
library(MASS)
library(boot) 
library(DAAG)
library(tseries)
#
str(car)
head(car)

#Car_ID			Unique id of each observation (Interger)		
#Symboling 			Its assigned insurance risk rating, A value of +3 
#indicates that the auto is risky, -3 that it is probably pretty 
#safe.(Categorical) 		
#carCompany			Name of car company (Categorical)		
#fueltype			Car fuel type i.e gas or diesel (Categorical)		
#aspiration			Aspiration used in a car (Categorical)		
#doornumber			Number of doors in a car (Categorical)		
#carbody			body of car (Categorical)		
#drivewheel			type of drive wheel (Categorical)		
#enginelocation			Location of car engine (Categorical)		
#wheelbase			Wheelbase of car (Numeric) front wheel to back wheel		
#carlength			Length of car (Numeric)		
#carwidth			Width of car (Numeric)		
#carheight			height of car (Numeric)		
#curbweight			The weight of a car without occupants or baggage. (Numeric)		
#enginetype			Type of engine. (Categorical)		
#cylindernumber			cylinder placed in the car (Categorical)		
#enginesize			Size of car (Numeric)		
#fuelsystem			Fuel system of car (Categorical)		
#boreratio			Boreratio of car (Numeric)		
#stroke			Stroke or volume inside the engine (Numeric)		
#compressionratio			compression ratio of car (Numeric)		
#horsepower			Horsepower (Numeric)		
#peakrpm			car peak rpm (Numeric)		
#citympg			Mileage in city (Numeric)		
#highwaympg			Mileage on highway (Numeric)		
#price(Dependent variable)			Price of car (Numeric)		


#
car = subset(car,select=-car_ID)
md.pattern(car)

name <- strsplit(as.character(car$CarName)," ")
brands <- sapply(name,"[",1)
brands[which(brands=="maxda")]= "mazda"
brands[which(brands=="porcshce")]= "porsche"
brands[which(brands=="toyouta")]= "toyota"
brands[which(brands=="vw"|brands=="vokswagen")]= "volkswagen"
brands[which(brands=="Nissan")]="nissan"
car = cbind(car,brands)

group = group_by(car,brands)
mean_price = summarise(group,price_mean = mean(price))
mean_price$brandlevel = cut(mean_price$price_mean,
                            c(0,10000,20000,40000)
  ,labels=c("low-grade","medium-grade","top-grade"))
car = merge(car,mean_price,by="brands")

car$symboling=factor(car$symboling)

summary(car)

# EDA

# brands popularity ?
car$brands <- factor(car$brands,levels=names(sort(table(car$brands),decreasing = T)))
ggplot(car,aes(x=brands))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("CarName")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90,size=12,vjust=0.5))

# PRICE compare base and log
hist_PRICE = hist((car$price),freq=F,breaks=20,main="Histogram of PRICE",col="purple")
hist_PRICE = hist(log(car$price),freq=F,breaks=20,main="Histogram of PRICE",col="purple")
densityPlot(~price,data=car)
densityPlot(~log(price),data=car)
qq_PRICE=qqPlot(~price,data=car,main="Quantile Plot of PRICE",id=F)
qq_PRICE=qqPlot(~log(price),data=car,main="Quantile Plot of PRICE",id=F)
box_PRICE=Boxplot(~price,data=car,main="Boxplot of PRICE",col="purple")
box_PRICE=Boxplot(~log(price),data=car,main="Boxplot of PRICE",col="purple")
# it seems log is better

car$price=log(car$price)
# scatterplot matrix
ggpairs(car,c("wheelbase","carlength","price")) # both
ggpairs(car,c("carwidth","carheight","price"))  # width
ggpairs(car,c("curbweight","enginesize","price")) # both
ggpairs(car,c("boreratio","stroke","price")) 
ggpairs(car,c("compressionratio","horsepower","price")) #horsepower 
ggpairs(car,c("peakrpm","citympg","price")) #citympg
ggpairs(car,c("highwaympg","price")) # highwaympg

# Boxplot of categorical variables
Boxplot(price~symboling,data=car,col="purple")
Boxplot(price~fueltype,data=car,col="purple")
Boxplot(price~aspiration,data=car,col="purple")
Boxplot(price~doornumber,data=car,col="purple")
Boxplot(price~carbody,data=car,col="purple")
Boxplot(price~drivewheel,data=car,col="purple")
Boxplot(price~enginelocation,data=car,col="purple")
Boxplot(price~enginetype,data=car,col="purple")
Boxplot(price~cylindernumber,data=car,col="purple")
Boxplot(price~fuelsystem,data=car,col="purple")

# Barplot of Categorical variables
ggplot(car,aes(x=symboling))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Symboling")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(car,aes(x=fueltype))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Fueltype")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(car,aes(x=aspiration))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Aspiration")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(car,aes(x=doornumber))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Doornumber")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(car,aes(x=carbody))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Carbody")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(car,aes(x=drivewheel))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Drivewheel")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(car,aes(x=enginelocation))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Enginelocation")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(car,aes(x=enginetype))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Enginetype")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(car,aes(x=cylindernumber))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Cylindernumber")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(car,aes(x=cylindernumber))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Wheelbase")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(car,aes(x=fuelsystem))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Fuesystem")+
  theme(plot.title = element_text(hjust = 0.5))

# brands vs price
ggplot(mean_price,aes(x=reorder(brands,price_mean),y=price_mean))+
  geom_bar(stat="identity",fill="purple")+
  theme(axis.text.x=element_text(angle=90,size=12,vjust=0.5))

##############################################################
# 1 (a)
car = subset(car,select=-c(CarName,price_mean,brands))
# = subset(car,select=-enginelocation)
summary(car)


#symboling
ggplot(car,aes(x=symboling))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Symboling")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~symboling,data=car,col="purple")

#fueltype
ggplot(car,aes(x=fueltype))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Fueltype")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~symboling,data=car,col="purple")

#aspiration
ggplot(car,aes(x=aspiration))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Aspiration")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~aspiration,data=car,col="purple")

#doornumber
ggplot(car,aes(x=doornumber))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Doornumber")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~doornumber,data=car,col="purple")

#carbody
ggplot(car,aes(x=carbody))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Carbody")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~carbody,data=car,col="purple")

#drivewheel
ggplot(car,aes(x=drivewheel))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Drivewheel")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~drivewheel,data=car,col="purple")

# enginelocation
ggplot(car,aes(x=enginelocation))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Enginelocation")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~enginelocation,data=car,col="purple")

# wheelbase
hist(car$wheelbase,breaks="FD",col="purple")
qqPlot(car$wheelbase)
Boxplot(car$wheelbase)

#carlength
hist(car$carlength,breaks="FD",col="purple")
qqPlot(car$carlength)
Boxplot(car$carlength)


#carwidth
hist(car$carwidth,breaks="FD",col="purple")
qqPlot(car$carwidth)
Boxplot(car$carwidth)

#carheight
hist(car$carheight,breaks="FD",col="purple")
qqPlot(car$carheight)
Boxplot(car$carheight)

#curbweight
hist(car$curbweight,breaks="FD",col="purple")
qqPlot(car$curbweight)
Boxplot(car$curbweight)

# enginetype
ggplot(car,aes(x=enginetype))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Enginetype")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~enginetype,data=car,col="purple")

# cylindernumber
ggplot(car,aes(x=cylindernumber))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Cylindernumber")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~cylindernumber,data=car,col="purple")

# enginesize
hist(car$enginesize,breaks="FD",col="purple")
qqPlot(car$enginesize)
Boxplot(car$curbweight)

#fuelsystem
ggplot(car,aes(x=fuelsystem))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Fuesystem")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~fuelsystem,data=car,col="purple")

#boreratio
hist(car$boreratio,breaks="FD",col="purple")
qqPlot(car$boreratio)
Boxplot(car$boreratio)


#stroke
hist(car$stroke,breaks="FD",col="purple")
qqPlot(car$stroke)
Boxplot(car$stroke)

#compressionratio
hist(car$compressionratio,breaks="FD",col="purple")
qqPlot(car$compressionratio)
Boxplot(car$compressionratio)

#horsepower
hist(car$horsepower,breaks="FD",col="purple")
qqPlot(car$horsepower)
Boxplot(car$horsepower)

#peakrpm
hist(car$peakrpm,breaks="FD",col="purple")
qqPlot(car$peakrpm)
Boxplot(car$peakrpm)

#citympg
hist(car$citympg,breaks="FD",col="purple")
qqPlot(car$citympg)
Boxplot(car$citympg)

#highwaympg
hist(car$highwaympg,breaks="FD",col="purple")
qqPlot(car$highwaympg)
Boxplot(car$highwaympg)

#price
hist(car$price,breaks="FD",col="purple")
qqPlot(car$price)

#brandlevel
ggplot(car,aes(x=brandlevel))+
  geom_bar(fill="purple")+
  geom_text(stat ='count',aes(label=..count..),vjust=-0.5)+
  theme_classic()+
  ggtitle("Brandlevel")+
  theme(plot.title = element_text(hjust = 0.5))
Boxplot(price~brandlevel,data=car,col="purple")

colnames(car)
continuous = c(8,9,10,11,12,15,17,18,19,20,21,22,23,24)
factor = c(1,2,3,4,5,6,7,13,14,16,25)
corrplot(cor(car[,continuous]))

##############################################################
#(b)
colnames(car)[continuous]
densityPlot(~wheelbase,data=car)
densityPlot(~carlength,data=car)
densityPlot(~carwidth,data=car)
densityPlot(~carheight,data=car)
densityPlot(~curbweight,data=car)
densityPlot(~enginesize,data=car)
densityPlot(~boreratio,data=car)
densityPlot(~stroke,data=car)
densityPlot(~compressionratio,data=car)
densityPlot(~horsepower,data=car)
densityPlot(~peakrpm,data=car)
densityPlot(~citympg,data=car)
densityPlot(~highwaympg,data=car)
densityPlot(~price,data=car)

#(c)
car_t = car
car_t_half = car
colnames(car_t)[continuous]

summary(t)
#trans = bcPower(with(car_t,cbind(wheelbase,carlength,carwidth,carheight,
                      #curbweight,enginesize,boreratio,stroke,
                      #compressionratio,horsepower,
                      #peakrpm,citympg,highwaympg)),coef(t,round=TRUE))
scatterplot(price~wheelbase,data=car_t)
summary(t <- powerTransform(wheelbase~price,data=car_t)) #-2
testTransform(t,lambda=-2)
scatterplot(price~log(wheelbase),data=car_t)
plot(car_t$wheelbase^(-2),car_t$price)

scatterplot(price~carlength,data=car_t)
summary(t <- powerTransform(carlength~price,data=car_t)) #1

scatterplot(price~carwidth,data=car_t)
summary(t <- powerTransform(carwidth~price,data=car_t)) # -5
testTransform(t,lambda=-5)
plot(car_t$carwidth^(-5),car_t$price)


scatterplot(price~carheight,data=car_t)
summary(powerTransform(carheight~price,data=car_t)) #1
#scatterplot(price~log(carheight),data=car)
cor(car_t$price,car_t$carheight) #<0.2

scatterplot(price~curbweight,data=car_t)
summary(t <- powerTransform(curbweight~price,data=car_t))#0
scatterplot(price~log(curbweight),data=car_t)

scatterplot(price~enginesize,data=car_t)
summary(t <- powerTransform(enginesize~price,data=car_t)) #-0.5
testTransform(t,-0.5)
plot(car_t$enginesize^(-0.5),car_t$price)

scatterplot(price~boreratio,data=car_t)
summary(t <- powerTransform(boreratio~price,data=car_t)) #1


scatterplot(price~stroke,data=car_t)
summary(t <- powerTransform(stroke~price,data=car_t))#2
testTransform(t,2)
plot(car_t$stroke^(2),car_t$price)
cor(car_t$price,car_t$stroke) #<0.2

scatterplot(price~compressionratio,data=car_t)
scatterplot(jitter(price)~jitter(compressionratio,amount=5),data=car_t)
summary(t <- powerTransform(compressionratio~price,data=car_t))#-3
testTransform(t,-3)
plot(car_t$compressionratio^(-3),car_t$price)
cor(car_t$price,car_t$compressionratio) #<0.2



scatterplot(price~horsepower,data=car_t)
summary(t <- powerTransform(horsepower~price,data=car_t)) #0
scatterplot(price~log(horsepower),data=car_t)

summary(car_t)

scatterplot(price~peakrpm,data=car_t)
summary(t <- powerTransform(peakrpm~price,data=car_t))#0
testTransform(t,0)
scatterplot(price~log(peakrpm),data=car)
cor(car_t$price,car_t$peakrpm) #<abs(0.2)


scatterplot(price~citympg,data=car_t)
summary(t <- powerTransform(citympg~price,data=car_t)) # -0.5
testTransform(t,-0.5)
plot(car_t$citympg^(-0.5),car_t$price)
scatterplot(price~log(citympg),data=car_t)

scatterplot(price~highwaympg,data=car_t)
summary(t <- powerTransform(highwaympg~price,data=car_t))#0
scatterplot(price~log(highwaympg),data=car_t)
car_t = car_t %>% mutate(wheelbase=wheelbase^(-2),
                         carlength=carlength,
                         carwidth=carwidth^(-5),
                         carheight=carheight,
                         curbweight=log(curbweight),
                         enginesize=enginesize^(-0.5),
                         boreratio=boreratio,
                         stroke=stroke^2,
                         compressionratio=compressionratio^(-3),
                         horsepower=log(horsepower),
                         peakrpm=log(peakrpm),
                         citympg=citympg^(-0.5),
                         highwaympg=log(highwaympg)
                         )
car_t_half = car_t_half %>% mutate(
                         
                         curbweight=log(curbweight),
                         horsepower=log(horsepower),
                         peakrpm=log(peakrpm),
                         highwaympg=log(highwaympg)
)
###############################################################
# 2
summary(car)
summary(car_t)
lm_basal = lm(price~.-compressionratio+stroke+carheight+peakrpm,data=car)
summary(lm_basal) # remove less-correlated variables
lm_basal = update(lm_basal,.~.-fuelsystem-cylindernumber,data=car)
summary(lm_basal) # remove no-converge variables
car::vif(lm_basal)
lm_basal = update(lm_basal,.~.-highwaympg,data=car)
summary(lm_basal) # romove VIF>6
car::vif(lm_basal)
lm_basal = update(lm_basal,.~.-curbweight,data=car)
summary(lm_basal)# remove VIF>5
car::vif(lm_basal)
lm_basal = update(lm_basal,.~.-enginesize,data=car)
summary(lm_basal)
car::vif(lm_basal)

lm_subset_b = regsubsets(formula(lm_basal),method="backward",nbest=1000,nvmax=100,data=car) 
lm_subset_b_s=summary(lm_subset_b)
which.min(lm_subset_b_s$cp)
coef(lm_subset_b,223)
lm_subset_b_s$cp[223]

lm_subset_f = regsubsets(formula(lm_basal),method="forward",nbest=1000,nvmax=100,data=car)
lm_subset_f_s=summary(lm_subset_f)
which.min(lm_subset_f_s$cp)
coef(lm_subset_f,441)
lm_subset_f_s$cp[441]

lm_subset_e = regsubsets(formula(lm_basal),method="exhaustive",nbest=40,nvmax=100,data=car)
lm_subset_e_s=summary(lm_subset_e)
which.min(lm_subset_e_s$cp)
coef(lm_subset_e,633)
lm_subset_e_s$cp[633]
###################################################################

lm_basal_t = lm(price~.-compressionratio+stroke+carheight+peakrpm,data=car_t)
summary(lm_basal_t) # remove less-correlated variables
lm_basal_t = update(lm_basal_t,.~.-fuelsystem-cylindernumber,data=car_t)
summary(lm_basal_t) # remove no-converge variables
car::vif(lm_basal_t)
lm_basal_t = update(lm_basal_t,.~.-citympg,data=car_t)
summary(lm_basal_t) # romove VIF>6
car::vif(lm_basal_t)
lm_basal_t = update(lm_basal_t,.~.-curbweight,data=car_t)
summary(lm_basal_t)# remove VIF>5
car::vif(lm_basal_t)
lm_basal_t = update(lm_basal_t,.~.-enginesize,data=car_t)
summary(lm_basal_t)
car::vif(lm_basal_t)

lm_subset_t_b = regsubsets(formula(lm_basal_t),method="backward",nbest=1000,nvmax=100,data=car_t) 
lm_subset_t_b_s=summary(lm_subset_t_b)
which.min(lm_subset_t_b_s$cp)
coef(lm_subset_t_b,200)
lm_subset_t_b_s$cp[200]

lm_subset_t_f = regsubsets(formula(lm_basal_t),method="forward",nbest=1000,nvmax=100,data=car_t)
lm_subset_t_f_s=summary(lm_subset_t_f)
which.min(lm_subset_t_f_s$cp)
coef(lm_subset_t_f,408)
lm_subset_t_f_s$cp[408]

lm_subset_t_e = regsubsets(formula(lm_basal_t),method="exhaustive",nbest=40,nvmax=40,data=car_t)
lm_subset_t_e_s=summary(lm_subset_t_e)
which.min(lm_subset_t_e_s$cp)
coef(lm_subset_t_e,32)
lm_subset_t_e_s$cp[32]

summary(car_t$fueltype)
car2_t = car_t
car2_t$symboling1 = ifelse(car2_t$symboling==1,1,0)
car2_t$symboling3 = ifelse(car2_t$symboling==3,1,0)
car2_t$fueltypegas = ifelse(car2_t$fueltype=="gas",1,0)
car2_t$drivewheelfwd = ifelse(car2_t$drivewheel=="fwd",1,0)
car2_t$enginelocationrear = ifelse(car2_t$enginelocation=="rear",1,0)
lm_subset = lm(price~symboling1+symboling3+fueltypegas+carbody+
                 drivewheelfwd+enginelocationrear+wheelbase+
                 carlength+carwidth+boreratio+horsepower+highwaympg+
             brandlevel,data=car2_t)
summary(lm_subset)
lm_subset = update(lm_subset,.~.-symboling3,data=car2_t)
summary(lm_subset)
lm_subset = update(lm_subset,.~.-wheelbase,data=car2_t)
summary(lm_subset)

#lm_step = stepAIC(lm_subset,direction="both",trace=0,data=car2_t)
#summary(lm_step)

car::vif(lm_subset)

crPlots(lm_subset)
boxTidwell(price~carwidth+carlength+boreratio,~symboling1+symboling3+fueltypegas+carbody+
             drivewheelfwd+enginelocationrear+wheelbase+
             horsepower+highwaympg+
             brandlevel,data=car2_t)
car2_t$carwidth2 = car2_t$carwidth^(-2)
lm_subset_box = update(lm_subset,.~.-carwidth+carwidth2,data=car2_t)
crPlots(lm_subset_box)
summary(lm_subset_box)
lm_subset_box = update(lm_subset_box,.~.-highwaympg,data=car2_t)

resettest(lm_subset_box,2,type="regressor")
resettest(lm_subset_box,3,type="regressor")

qqPlot(lm_subset_box,data=car2_t,id=list(n=3))
outlierTest(lm_subset_box)
influenceIndexPlot(lm_subset_box,var="cook")
influenceIndexPlot(lm_subset_box,var="hat")
influencePlot(lm_subset_box)

ncvTest(lm_subset_box_nounusal)
bptest(lm_subset_box_nounusal)
gqtest(lm_subset_box_nounusal)


lm_subset_box_nounusal = update(lm_subset_box,subset = -c(75,26,53,127,129,130),data=car2_t)
summary(lm_subset_box_nounusal)
AIC(lm_subset_box,lm_subset_box_nounusal)

car_t_half$symboling1 = ifelse(car_t_half$symboling==1,1,0)
car_t_half$symboling3 = ifelse(car_t_half$symboling==3,1,0)
car_t_half$fueltypegas = ifelse(car_t_half$fueltype=="gas",1,0)
car_t_half$drivewheelfwd = ifelse(car_t_half$drivewheel=="fwd",1,0)
car_t_half$enginelocationrear = ifelse(car_t_half$enginelocation=="rear",1,0)
car_t_half$carwidth2 = car_t_half$carwidth^(-2)

test = lm(price~symboling1+fueltypegas+carbody+
                    drivewheelfwd+enginelocationrear+
                    carlength+boreratio+horsepower+brandlevel+
                    carwidth2,data=car_t_half)
summary(test)
ncvTest(test)
bptest(test)
gqtest(test)
car::vif(test)
jarque.bera.test(test$residuals)



residualPlots(lm_subset,type="rstudent")
#######################################################
coef_boot = Boot(lm_subset_box_nounusal, R=999)
summary(coef_boot)
hist(coef_boot)

#####################################################
par(mar=c(1, 0, 2, 0))
cv = CVlm(data=car2_t,form.lm = lm_subset_box_nounusal,
                      m=5,printit=T,plotit=T)
# 0.03 0.02 0.02 0.02 0.02
attr(cv,"ms")

