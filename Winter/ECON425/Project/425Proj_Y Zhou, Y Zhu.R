#### data wrangling
# packages
library(tidyverse)
library(fastDummies)
library(ggpubr)
library(gridExtra)

# read data
data = read.csv("File/425 Project/car.csv")
summary(data)

# clean data
data <- data %>%
  select(-c(CarName))
data2 <- data # back up

data <- data %>%
  mutate(fueltype = ifelse(fueltype == "diesel",0,1))

data <- data %>%
  mutate(aspiration = ifelse(aspiration == "turbo",1,0))

data <- data %>%
  mutate(doornumber = ifelse(doornumber == "four",1,0))

carbody = dummy_cols(data$carbody)
colnames(carbody) = c("data","convertible","hardtop","hatchback","sedan","wagon")
data = cbind(data, carbody)
data <- data %>%
  select(-c(carbody,data,convertible))
rm(carbody)

drivewheel = dummy_cols(data$drivewheel)
colnames(drivewheel) = c("data","fourwd","fwd","rwd")
data = cbind(data, drivewheel)
data <- data %>%
  select(-c(drivewheel,data,fourwd))
rm(drivewheel)

data <- data %>%
  mutate(enginelocation = ifelse(enginelocation == "front",1,0))

data <- data %>%
  mutate(enginetype = ifelse(enginetype %in% c("dohc","dohcv"),1,ifelse(enginetype %in% c("ohc","ohcf","ohcv"),0,2)))
enginetype = dummy_cols(data$enginetype)
colnames(enginetype) = c("data","ohc","dohc","other")
data = cbind(data, enginetype)
data <- data %>%
  select(-c(enginetype,data,dohc))
rm(enginetype)

data <- data %>%
  mutate(cylindernumber = ifelse(cylindernumber %in% c("two","three","four"), 0, 1))



#### data visualization
cleandata <- read.csv("File/425 Project/cleandata.csv")


# variables
## continuous variables
normal = function(x) {
  normal = (x - min(x)) / (max(x) - min(x))
  return(normal)
}

cleandata_normal <- cleandata %>%
  mutate(
    wheelbase = normal(wheelbase),
    carlength = normal(carlength),
    carwidth = normal(carwidth),
    carheight = normal(carheight),
    curbweight = normal(curbweight),
    enginesize = normal(enginesize),
    boreratio = normal(boreratio),
    stroke = normal(stroke),
    compressionratio = normal(compressionratio),
    horsepower = normal(horsepower),
    peakrpm = normal(peakrpm),
    citympg = normal(citympg),
    highwaympg = normal(highwaympg),
    price = normal(price)
  )

histplot = function(data){
  g1 = ggplot(data, aes(x = wheelbase)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g2 = ggplot(data, aes(x = carlength)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g3 = ggplot(data, aes(x = carwidth)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g4 = ggplot(data, aes(x = carheight)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g5 = ggplot(data, aes(x = curbweight)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g6 = ggplot(data, aes(x = enginesize)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g7 = ggplot(data, aes(x = boreratio)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g8 = ggplot(data, aes(x = stroke)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g9 = ggplot(data, aes(x = compressionratio)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g10 = ggplot(data, aes(x = horsepower)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g11 = ggplot(data, aes(x = peakrpm)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g12 = ggplot(data, aes(x = citympg)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g13 = ggplot(data, aes(x = highwaympg)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  g14 = ggplot(data, aes(x = price)) +
    geom_histogram(aes(y = ..density..),fill = "#99CCFF")+
    theme_minimal()
  gtotal = grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14)
  return(gtotal)
}

### comparison
raw = histplot(cleandata)
normal = histplot(cleandata_normal)
ggsave("con_ori.jpg",raw)
ggsave("con_nor.jpg",normal)


## factor variables
### label
g1 = ggplot(data2,aes(x=symboling))+
  geom_histogram(alpha = 0.3, color = "#9999ff",fill = "#99CCFF")+
  theme_minimal()
ggsave("label.jpg",g1)

### features
discrete_plot = function(data){
  data_f = as_factor(data)
  data_df = as.data.frame(table(data_f))
  g = ggplot(data_df,aes(x = "",y = Freq,fill=data_f)) +
    geom_bar(position = "stack",stat = "identity",width = 1)+
    coord_polar("y", start=0)+
    labs(x = '', y = '', title =substitute(data))+
    theme(
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold",hjust = 0.5))
  return(g)
}
attach(data2)
g2 = discrete_plot(fueltype)
g3 = discrete_plot(aspiration)
g4 = discrete_plot(doornumber)
g5 = discrete_plot(enginelocation)
g6 = discrete_plot(carbody)
g7 = discrete_plot(drivewheel)
g8 = discrete_plot(enginetype)
g9 = discrete_plot(cylindernumber)
g10 = discrete_plot(fuelsystem)
d_plot = grid.arrange(g2,g3,g4,g5,g6,g7,g8,g9,g10)
ggsave("File/425 Project/discrete.jpg",d_plot)

# metrics

## load evaluation metrics 
mdres = read.csv('File/425 Project/mdres.csv')
mdres[is.na(mdres)] <- 0
colnames(mdres) = c("Model","Accuracy","Precision_negative",
                    "Precision_neutral","Precision_positive",
                    "Recall_negative","Recall_neutral","Recall_positive")

## Plotting

graphmetric <- function(rdata, mfamily){
  g1 = ggplot(data = rdata, mapping = aes(x=mfamily, y=Accuracy, fill=Model)) + 
    geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.7)) +
    theme(legend.position='bottom',axis.title.x=element_blank())
  
  g2= ggplot(data = rdata, mapping = aes(x=mfamily, y=Precision_negative, fill=Model)) + 
    geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.7)) +
    theme(legend.position='None',axis.title.x=element_blank())
  
  g3= ggplot(data = rdata, mapping = aes(x=mfamily, y=Precision_neutral, fill=Model)) + 
    geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.7)) +
    theme(legend.position='None',axis.title.x=element_blank())
  
  g4= ggplot(data = rdata, mapping = aes(x=mfamily, y=Precision_positive, fill=Model)) + 
    geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.7)) +
    theme(legend.position='None',axis.title.x=element_blank())
  
  g5= ggplot(data = rdata, mapping = aes(x=mfamily, y=Recall_negative, fill=Model)) + 
    geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.7)) +
    theme(legend.position='None',axis.title.x=element_blank())
  
  g6= ggplot(data = rdata, mapping = aes(x=mfamily, y=Recall_neutral, fill=Model)) + 
    geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.7)) +
    theme(legend.position='None',axis.title.x=element_blank())
  
  g7= ggplot(data = rdata, mapping = aes(x=mfamily, y=Recall_positive, fill=Model)) + 
    geom_bar(stat = 'identity', width = 0.5, position = position_dodge(0.7)) +
    theme(legend.position='None',axis.title.x=element_blank())
  
  ggarrange(g1,
            ggarrange(g2,g3,g4,g5,g6,g7,nrow=2,ncol=3),
            nrow=2,widths = 10, heights = 20
            )
  
}



g1 = graphmetric(mdres[1:24,], 'Logistic Regression')
ggsave("D://File/425 Project/LR.jpg",g1)
g2 = graphmetric(mdres[25:34,], 'KNN')
g3 = graphmetric(mdres[35:46,], 'MLP')
g4 = graphmetric(mdres[47:57,], 'SVM')
g5 = graphmetric(mdres[58:66,], 'Decision tree')


