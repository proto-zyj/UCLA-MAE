# package
library(tidyverse)
library(plm)
library(quantreg)
library(stargazer)
library(rqpd)
library(rqpd)
# wd
wd <- "C://Users//zyj37//Desktop//MAE//Winter//ECON 424//Project"
setwd(wd)
# read
data <-  read_csv("data2.csv")
glimpse(data)
gdpdef <- read_csv("gdpdef.csv")
data <- merge(data,gdpdef,by = "y")
data <- data %>%
  mutate(INCCITY = INCCITY /GDPDEF,
         INVCVILLI = INVCVILLI / GDPDEF,
         GDP = INCCITY / GDPDEF,
         SERVICEGDP = SERVICEGDP / GDPDEF,
         PERGDP = PERGDP/ GDPDEF,
         SAVING = SAVING / GDPDEF,
         LOAN = LOAN / GDPDEF)
data <- data %>%
  mutate(inequality = INCCITY / INVCVILLI,
         gaprate = (INCCITY - INVCVILLI) / (INCCITY + INVCVILLI),
         resource = WMINE / WORKER,
         urban = 1 - HHVILLI / HOUSEHOLD,
         finance = (SAVING + LOAN) / GDP,
         service = SERVICEGDP / GDP,
         manu = WMANU / WORKER,
         educ = COLLEGE / POP)

data_pool <- pdata.frame(data,index=c("CODE","y"))


phtest(inequality~resource+urban+finance+service+manu+educ+PERGDP,data=data)


pool1 = lm(inequality~resource+urban+finance+service+manu+educ+PERGDP, data=data)
pool1 = lm(inequality~resource+urban+finance+service+manu+educ+PERGDP_01, data=data)

summary(pool1)

pool2 = lm(gaprate~resource+urban+finance+service+manu+educ+PERGDP, data=data)
pool2 = lm(gaprate~resource+urban+finance+service+manu+educ+PERGDP_01, data=data)

summary(pool2)

fe1 <- plm(inequality~resource+urban+finance+service+manu+educ+PERGDP_01,data=data)
summary(fe1)

fe2 <- plm(gaprate~resource+urban+finance+service+manu+educ+PERGDP_01, data=data)
summary(fe2)


summary(rq1)
summary(rq2)
summary(rq3)


rq2 = rq(gaprate~resource+urban+finance+service+manu+educ+PERGDP_01, tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), data = data)      
summary(rq2)

stargazer(pool1,fe1,pool2,fe2,align = T,no.space = T,omit.stat = c("aic","bic","f"))

data$PERGDP_01 = (data$PERGDP - min(data$PERGDP)) / (max(data$PERGDP) - min(data$PERGDP))

stargazer(rq1,rq2,rq3,align = T,no.space = T,omit.stat = c("aic","bic","f"))


