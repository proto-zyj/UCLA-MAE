####Note####
# Our group rewrite the wholes code using the "tidyverse" package and rename variables 
# for better understanding.
# We use the function "fread" to import the raw data much faster.
# We find there is one error with the sample cpde:
# cps_06_keep_educ_indices <- (is.na(HIGRADE)|is.na(EDUC)) | (HIGRADE != 999 & HIGRADE != 0 & EDUC >1 & EDUC!=999)
# Here The logistic expression "EDUC >1 & EDUC!=999" will drop observations with a irregular EDUC value
# But when we do the union calculation(is.na(HIGRADE) | (HIGRADE!= 999 & HIGRADE !=0),
# It will keep all the observations that HIGRADE equals NA, which is all the observations after 1991
# The data cleaning will lose it effect.

# code for check
#cps_06_keep_educ_indices <- (is.na(HIGRADE)|is.na(EDUC)) | (HIGRADE != 999 & HIGRADE != 0 & EDUC >1 & EDUC!=999)
#test = cps[cps_06_keep_educ_indices,]
#sum(test[YEAR>=1991,]$EDUC == 1)

####Setting####
#packages#
library(tidyverse) 
library(data.table) 
library(fastDummies)
library(stargazer)
library(AER)
library(gridExtra)
# clean up
rm(list = ls())

# setwd
wd = "C:/Users/zyj37/Desktop/MAE/Winter/ECON 424/Homework" # change to your own
setwd(wd)


####Load Data###
# for running ,please either change the name here or the file name
# GDP Deflator
def = fread("deflator.csv") %>% 
  mutate(YEAR = as.numeric(substr(DATE, 0, 4))) %>% 
  select(YEAR, deflator) %>%
  rename(DEFLATOR = deflator)

# CPS data
cps = fread("cps_00001.csv") 


####Variable Cleaning(Demographic)####
attach(cps)

# drop out of work force (keep between 16 and 64 last year, that means 17 and 65 this year)
sum(is.na(AGE)) # there is no NA in the data 
age_keep <- AGE >= 17 & AGE <= 65 

# drop weight<0
sum(is.na(ASECWT)) #there is no NA in the data
asecwt_keep  <-  ASECWT >= 0

# drop education missing or blanks
 # 0 & 1 means not in universe, 999 means missing,
 # HIGRADE is only for before 1991, for after 1991, it is NA

## examine the variable
sum(is.na(HIGRADE[YEAR <= 1991]))
sum(is.na(EDUC))

## my try (to find the irregular observations)
sum(EDUC[YEAR <= 1991] == 999)
sum(EDUC[YEAR <= 1991] == 1)
sum(HIGRADE[YEAR <= 1991] == 999)
sum(HIGRADE[YEAR <= 1991] == 0)
sum(EDUC[YEAR > 1991] == 999)
sum(EDUC[YEAR > 1991] <= 1)

educ_keep <- EDUC >1 & EDUC != 999

## double check
test  <-  cps[educ_keep,]
sum(test$EDUC == 999 | test$EDUC <= 1)
sum(test$HIGRADE == 999 | test$HIGRADE == 0, na.rm = T)
rm(test)

# drop 3/8 redesign in the 2014 ASEC sample: 3/8ths of the total sample was randomly selected to receive the redesigned income questions
 # 1 mens redesign samples and we do not need them
hflag_keep <- is.na(HFLAG) | (HFLAG != 1)

# drop hispanic non responses of 901 and 902
sum(is.na(HISPAN))
hispan_keep <- HISPAN != 901 & HISPAN != 902

# gather all drops
keep <- age_keep & hflag_keep & asecwt_keep & educ_keep & hispan_keep
cps <- cps[keep,]
rm(age_keep, asecwt_keep, educ_keep, hflag_keep, hispan_keep, keep)
####Create Bins####
detach(cps)
attach(cps)

# education dummies
make_edu5 <-  function(educ,higrade,year){
  edu5  <-  0
  if(higrade <= 141 && year <= 1991){
    edu5 <- 1 # HSD 
  } 
  if(higrade == 150 && year <= 1991){
    edu5 <- 2 # HSG 
  } 
  if(higrade >= 151  &&  higrade <= 181 && year <= 1991){
    edu5 <- 3 # SMC 
  } 
  if(higrade == 190 && year <= 1991){
    edu5 <- 4 # CLG 
  } 
  if(higrade >= 191 && year <= 1991){
    edu5 <- 5 # GTC 
  }
  
  if(educ <= 71 && year >= 1992){
    edu5 <- 1
  } 
  if(educ == 73 && year >= 1992){
    edu5 <- 2
  }
  if(educ >= 80 && educ <= 92 && year >= 1992){
    edu5 <- 3
  } 
  if(educ == 111 && year >= 1992){
    edu5 <- 4
  } 
  if(educ >= 120 && year >= 1992){
    edu5 <- 5
  }
  return(edu5)
}
cps$edu5 <- mapply(make_edu5, EDUC, HIGRADE, YEAR) 
sum(is.na(cps$edu5)) # double check 

# gender and race dummies
sum(SEX == 9)
sum(is.na(SEX))
cps$female <-as.numeric(cps$SEX == 2)

sum(RACE == 999)
sum(is.na(RACE))
cps$white <- as.numeric(cps$RACE == 100)


# hispanic dummies
sum(is.na(HISPAN))
make_origin <- function(HISPAN){
  origin = 0
  if(HISPAN >= 100 && HISPAN <= 109){
    origin <- 1
  } 
  if (HISPAN >= 200){
    origin <- 2
  }
  return(origin)
}
cps$origin <- mapply(make_origin,cps$HISPAN)

# create bins
make_bin <- function(a, f, r, e, o){paste0(a, f, r, e, o)}
detach(cps)
cps$lbin <- mapply(make_bin, cps$AGE, cps$female, cps$white, cps$edu5, cps$origin)

####Create Instrument: Relative population####
attach(cps)
# Create hispanic dummy
cps$hispan_dum <- as.numeric(HISPAN != 0) 

# weighting by respective weight
cps$hisp_asecwt <- cps$hispan_dum * cps$ASECWT # for all hispanic
cps$nonhisp_asecwt <- (1 - cps$hispan_dum) * cps$ASECWT # for all non-hispanic

# aggregate population
ratio_h_nh <- cps %>%
  group_by(YEAR) %>%
  summarize(hisp_population = sum(hisp_asecwt),
            nonhisp_population = sum(nonhisp_asecwt)) %>%
  mutate(z = hisp_population / nonhisp_population)
detach(cps)
cps <- merge(cps, ratio_h_nh, by = "YEAR")
rm(ratio_h_nh)
####Variable Cleaning 2(INCOME)####
# drop missing or N.I.U or negative in INCWAGE
attach(cps)
incwage_drop <- (INCWAGE == 99999999 | INCWAGE == 99999998 | INCWAGE < 0)

# drop missing or N.I.U or negative in OINCWAGE, INCLONGJ
oincwage_drop <- !is.na(OINCWAGE) & (OINCWAGE == 99999999 | OINCWAGE < 0)
inclongj_drop <- !is.na(INCLONGJ) & (INCLONGJ == 99999999 | INCLONGJ < 0)

# Keep if wage worker
worktype_keep <- (CLASSWLY >= 20 & CLASSWLY <= 28)

#Drop missing weeks worked 
missing_wkwrk_drop <- (WKSWORK2==9 & YEAR<1976)
sum(missing_wkwrk_drop)

cps <- cps[(!incwage_drop) & (!oincwage_drop) & (!inclongj_drop) & worktype_keep,]
rm(inclongj_drop, incwage_drop, missing_wkwrk_drop, oincwage_drop, worktype_keep)
detach(cps)

#Deal with Income Topcoding
attach(cps)
## replace incwage topcodes by 1.5*topcodes
incwage_top <- 50000 * 1.5 * (INCWAGE >= 50000 & YEAR >= 1968 & YEAR<=1981) +
  75000 * 1.5 * (INCWAGE >= 75000 & YEAR >= 1982 & YEAR <= 1984) + 
  99999 * 1.5 * (INCWAGE >= 99999 & YEAR >= 1985 & YEAR <= 1987)
cps$INCWAGE <- ifelse(incwage_top == 0, INCWAGE, incwage_top)
sum(is.na(INCWAGE))

inclongj_top <- 99997 * 1.5 * (INCLONGJ >= 99997 & YEAR >= 1988 & YEAR<=1995)  +
  150000*1.5 * (INCLONGJ >= 150000 & YEAR >= 1996 & YEAR <= 2002) +
  200000*1.5 * (INCLONGJ >= 200000 & YEAR >= 2003 & YEAR <= 2010) +
  250000*1.5 * (INCLONGJ >= 250000 & YEAR >= 2011 & YEAR <= 2014) +
  280000*1.5 * (INCLONGJ >= 280000 & YEAR == 2015) +
  300000*1.5 * (INCLONGJ >= 300000 & YEAR >= 2016 & YEAR <= 2018)
cps$INCLONGJ <- ifelse(inclongj_top == 0, INCLONGJ, inclongj_top)
sum(is.na(INCLONGJ[YEAR >= 1988]))

oincwage_top <- 99997 * 1.5 * (OINCWAGE>=99997 & YEAR >= 1988 & YEAR <= 1995) +
  25000 * 1.5 * (OINCWAGE >= 25000 & YEAR >= 1996 & YEAR <= 2002) +
  35000 * 1.5 * (OINCWAGE >= 35000 & YEAR >= 2003 & YEAR <= 2010) +
  47000 * 1.5 * (OINCWAGE >= 47000 & YEAR >= 2011 & YEAR <= 2014) +
  56000 * 1.5 * (OINCWAGE >= 56000 & YEAR == 2015) +
  55000 * 1.5 * (OINCWAGE >= 55000 & YEAR >= 2016 & YEAR <= 2017) +
  56000 * 1.5 * (OINCWAGE >= 56000 & YEAR == 2018)
cps$OINCWAGE <- ifelse(oincwage_top == 0, OINCWAGE, oincwage_top)
sum(is.na(OINCWAGE[YEAR >= 1988]))

## after 1988 INCWAGE is a combination of the INCLONGJ and OINCWAGE
cps$INCWAGE <- ifelse(YEAR >= 1988, INCLONGJ+OINCWAGE, INCWAGE)
rm(inclongj_top, incwage_top, oincwage_top)

# Calculate missing working week with median of WKSWORK2
cps$wkswork1_cal <- 0 * (WKSWORK2==0 & YEAR<1976)+
  7    * (WKSWORK2 == 1 & YEAR < 1976)+
  20   * (WKSWORK2 == 2 & YEAR < 1976)+
  33   * (WKSWORK2 == 3 & YEAR < 1976)+
  43.5 * (WKSWORK2 == 4 & YEAR < 1976)+
  48.5 * (WKSWORK2 == 5 & YEAR < 1976)+
  51   * (WKSWORK2 == 6 & YEAR < 1976)
cps$WKSWORK1 <- ifelse(cps$wkswork1_cal == 0, cps$WKSWORK1, cps$wkswork1_cal) 

# Estimate working hours per week before 1976 using data btw 1976 and 1978
cps_year_1976_1978 <- cps[(cps$YEAR >= 1976 & cps$YEAR <= 1978),]

## run the model 
model_hours_week_1976_78 <- lm(UHRSWORKLY ~ FULLPART + factor(edu5) + female + white + 
                             factor(origin), cps_year_1976_1978)
dum_edu5 <- dummy_cols(cps$edu5)
dum_origin <- dummy_cols(cps$origin)

cps$pred_hours_week <- model_hours_week_1976_78$coefficients[1]+
  model_hours_week_1976_78$coefficients[2] * cps$FULLPART + 
  model_hours_week_1976_78$coefficients[3] * dum_edu5$.data_2 + 
  model_hours_week_1976_78$coefficients[4] * dum_edu5$.data_3 +
  model_hours_week_1976_78$coefficients[5] * dum_edu5$.data_4 + 
  model_hours_week_1976_78$coefficients[6] * dum_edu5$.data_5 + 
  model_hours_week_1976_78$coefficients[7] * cps$female +
  model_hours_week_1976_78$coefficients[8] * cps$white +
  model_hours_week_1976_78$coefficients[9] * dum_origin$.data_1 +
  model_hours_week_1976_78$coefficients[10] * dum_origin$.data_2

cps$UHRSWORKLY = ifelse(cps$YEAR < 1976, cps$pred_hours_week, cps$UHRSWORKLY)

#total hours worked last year
cps$hours_week <- cps$UHRSWORKLY * cps$WKSWORK1
rm(model_hours_week_1976_78, dum_origin, dum_edu5, cps_year_1976_1978)

####Variable Selection####
cps$YEAR <- cps$YEAR-1

# Keeping variables I need for Canonical model and shift-share analysis
ss <- cps[,c("YEAR", "ASECWT", "IND90LY", "hours_week", "INCWAGE", "hispan_dum", "z", "lbin")]
write_csv(ss,"ss.csv")
#ss <- fread("ss.csv")
####Data in final form for Shift-Share analysis####
#checking missing
rm(cps)
summary(ss$IND90LY)
# Weight incwage
ss$incwage_individual <-  ss$INCWAGE * ss$ASECWT
ss$INCWAGE <- NULL
# Create Data for Shift Share Analysis. Collapse weighted income by hispan_dym year and industry
shift_share <- aggregate(cbind(incwage_ind_y = ss$incwage_individual),
                         by = list(YEAR = ss$YEAR, hispan_dum = ss$hispan_dum, IND90LY = ss$IND90LY
                                   ), FUN = sum)
####Data in final form for Canonical Model: Table####
ss$IND90LY <- NULL

#check missing
sum(ss$hours_week == 0)

#weight hours, to collapse by bin
ss$hours_individual <- ss$hours_week * ss$ASECWT
ss$hours_week <- NULL

#Collapse hours and incwage by year and lbin
hours_inc_bin_y <- ss %>%
  group_by(YEAR, lbin) %>%
  summarize(hours_bin_y = sum(hours_individual),
            inc_bin_y = sum(incwage_individual)) 

#need hispan_dum, and instrument in the table as well. Take means.
h_z <- ss %>%
  group_by(YEAR, lbin) %>%
  summarize(h_bin_y = mean(hispan_dum),
            z_y = mean(z))
cm <- merge(hours_inc_bin_y, h_z, by = c("YEAR", "lbin"))

#need deflator to generate real income: inc and later real wages
cm <- merge(cm, def, by= "YEAR")
cm$inc_bin_y <-  cm$inc_bin_y / cm$DEFLATOR
cm$DEFLATOR = NULL

# additionally , saving instrument as time variable for later regressions
instrument <- cm %>%
  group_by(YEAR) %>%
  summarize(z = mean(z_y))

####Compositon Adjustment####

#a) To pin down CA hours, calculate aggregated real income of hisp  and non hisp
inc_h_y <- cm %>%
  group_by(YEAR, h_bin_y) %>%
  summarize(inc_h_y = sum(inc_bin_y)) %>%
  mutate(lninc_h_y = log(inc_h_y))

cm = merge(cm, inc_h_y, by = c("YEAR", "h_bin_y"))

#b) 
## Total hours per bin (time invariant)-->numerator
hours_bin <- cm %>%
  group_by(lbin) %>%
  summarize(hours_bin = sum(hours_bin_y))
cm <- merge(cm, hours_bin, by = "lbin")

## Total hours per hispan - non hispan  (time invariant) --> denominator
hours_h <- cm %>%
  group_by(h_bin_y) %>%
  summarize(hours_h = sum(hours_bin_y))
cm <- merge(cm, hours_h, by = "h_bin_y")

cm$weight <- cm$hours_bin / cm$hours_h

# real wages per bin
cm$lnwage_bin_y <- log(cm$inc_bin_y / cm$hours_bin_y)
#weighting real wages
cm$lnwage_bin_y <- cm$lnwage_bin_y * cm$weight

# delete missing values
cm <- cm[!(is.nan(cm$lnwage_bin_y)|is.infinite(cm$lnwage_bin_y)),]

#CA Wages (per origin and year)
lnwage_h_y <- cm %>%
  group_by(YEAR,h_bin_y) %>%
  summarize(lnwage_h_y = sum(lnwage_bin_y))
cm <- merge(cm, lnwage_h_y, by=c("YEAR", "h_bin_y"))

#c) CA hours
cm$lnhours_h_y <- cm$lninc_h_y - cm$lnwage_h_y

rm(def, h_z, hours_bin, hours_h, hours_inc_bin_y, inc_h_y, lnwage_h_y, ss)

####Canonical Model analysis####
#Now just keep time series for relative prices, rel. supply, hisp_dum
cm_fin <-  aggregate(cbind(lnw = cm$lnwage_h_y, 
                           lhours = cm$lnhours_h_y),
                   by = list(hispan_dum = cm$h_bin_y, 
                             YEAR = cm$YEAR), FUN = mean)
cm_wide <- reshape(cm_fin, timevar = c("hispan_dum"),
          idvar = c("YEAR"),
          direction = "wide")
rm(cm_fin, cm)

# calculating wage premiums
cm_wide$w <- cm_wide$lnw.1 - cm_wide$lnw.0

# relative supply
cm_wide$s <- cm_wide$lhours.1 - cm_wide$lhours.0

# merge with instrument
cm_wide <- merge(cm_wide, instrument, by=c("YEAR"))
rm(instrument)

# normalize wrt 1970
initial_year <- cm_wide[cm_wide$YEAR == min(cm_wide$YEAR),]
cm_wide$s = cm_wide$s - initial_year$s
cm_wide$w = cm_wide$w - initial_year$w
cm_wide$z = cm_wide$z - initial_year$z

# time trend  
cm_wide$t = cm_wide$YEAR - initial_year$YEAR
rm(initial_year)

#regression
## detrending relative wages
reg_w_t = lm(w ~ t, cm_wide)
cm_wide$w_detrend = reg_w_t$residuals

## detrending relative supply
reg_s_t = lm(s ~ t, cm_wide)
cm_wide$s_detrend = reg_s_t$residuals

## predicting detrending relative wages using detrending relative supply.
reg_w_detrend_s_detrend  = lm(w_detrend ~ s_detrend, cm_wide)
cm_wide$w_detrend_pred = reg_w_detrend_s_detrend$fitted.values

# predicting relative wages using relative supply
reg_w_t_s = lm(w ~ t + s, cm_wide)
cm_wide$w_pred = reg_w_t_s$fitted.values

## iv
reg_z_s = lm(z ~ s, cm_wide)
cm_wide$z_hat = reg_z_s$fitted.values
reg_z_hat_t = lm(z_hat ~ t, cm_wide)
cm_wide$z_hat_detrend = reg_z_hat_t$residuals
reg_w_detrend_z_hat_detrend = lm(w_detrend ~ z_hat_detrend, cm_wide)
cm_wide$w_detrend_pred_iv = reg_w_detrend_z_hat_detrend$fitted.values
reg_w_t_z = ivreg(w ~ t + s | t + z, data = cm_wide)
cm_wide$w_pred_iv = reg_w_t_z$fitted.values

####Figures####

rel_wage = ggplot(data = cm_wide, aes(x = YEAR, y = w)) +
  geom_line(col = "#064375", size = 1)+
  geom_point(col = "#064375")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.4, size = 20),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_text(face="bold", size = 10))+
  labs(title = "Evolution of Hispanic Wage Premium", x = "Year Income Earned", 
       y = "Hispanic Wage Premium")+
  scale_x_continuous(breaks = pretty(cm_wide$YEAR, n = 10))+
  scale_y_continuous(breaks = pretty(cm_wide$w, n = 10))

rel_s = ggplot(data = cm_wide, aes(x = YEAR, y = s, group = 1)) +
  geom_line(col = "#064375", size = 1)+
  geom_point(col = "#064375")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.4, size = 20),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_text(face="bold", size = 10))+
  labs(title = "Evolution of Relative Hispanic Supply ", x = "Year Income Earned", 
       y = "Relative Hours Worked")+
  scale_x_continuous(breaks = pretty(cm_wide$YEAR, n = 10))+
  scale_y_continuous(breaks = pretty(cm_wide$s, n = 10))

w_fit = ggplot(data = cm_wide) +
  geom_line(aes(x = YEAR, y = w, col = "Wage Premium"), size = 1) +
  geom_line(aes(x = YEAR, y = w_pred, col = "Predicted Wage Premium"), size = 1) + 
  theme_minimal()+
  scale_color_manual("", values = c("#064375", "#BF7112"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_text(face="bold", size = 10),
        legend.position = c(0.7, 0.9))+
  labs(title = "Fit of Hispanic Wage Premium", x = "Year Income Earned", 
       y = "Hispanic Wage Premium") +
  scale_x_continuous(breaks = pretty(cm_wide$YEAR, n = 10))+
  scale_y_continuous(breaks = pretty(cm_wide$w_pred_iv, n = 10))

w_detrend_fit = ggplot(data = cm_wide) +
  geom_line(aes(x = YEAR, y = w_detrend, color = "Detrened Wage Premium"), size =1) +
  geom_line(aes(x = YEAR, y = w_detrend_pred, color = "Predicted Detrended Wage Premium"), size = 1) + 
  theme_minimal() +
  scale_color_manual("", values = c("#064375", "#BF7112")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_text(face="bold", size = 10),
        legend.position = c(0.7, 0.9))+
  labs(title = "Fit of Detrended Hispanic Premium", x = "Year Income Earned", 
       y = "Detrended Hispanic Wage Premium") +
  scale_x_continuous(breaks = pretty(cm_wide$YEAR, n = 10))+
  scale_y_continuous(breaks = pretty(cm_wide$w_detrend_pred_iv, n = 10))

second_scale <- 1.1
w_d_s_d = ggplot(data = cm_wide) +
  geom_line(aes(x = YEAR, y = s_detrend, color = "Detrended Hispanic Relative Supply"), size =1) +
  geom_line(aes(x = YEAR, y = w_detrend * second_scale, color = "Detrended Hispanic Wage Premium"), size = 1) + 
  theme_minimal() +
  scale_color_manual("", values = c("#064375", "#BF7112")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(face="bold", size = 14),
        axis.text.y = element_text(face="bold", size = 14),
        legend.position = c(0.5, 0.1),
        legend.text=element_text(size = 14 ))+
  labs(title = "Relationship between Detrend Series", x = "Year Income Earned", 
       y = "Detrended Hispanic Relative Supply") +
  scale_x_continuous(breaks = pretty(cm_wide$YEAR, n = 10))+
  scale_y_continuous(sec.axis = sec_axis(~.*1 / second_scale ,name = "Detrended Hispanic Wage Premium"))


# use grid.arrange to put two graphs together
g1 = grid.arrange(rel_s, rel_wage, ncol = 2)
ggsave("g1.jpg", width = 12, height = 5)
g2 = grid.arrange(w_fit, w_detrend_fit, ncol = 2)
ggsave("g2.jpg", width = 12, height = 5)
ggsave("relation btw detrend series.jpg", w_d_s_d, width = 12, height = 7)


### iv figures
rel_s_iv = ggplot(data = cm_wide, aes(x = YEAR, y = z_hat, group = 1)) +
  geom_line(col = "#064375", size = 1)+
  geom_point(col = "#064375")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.4, size = 20),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_text(face="bold", size = 10),
        legend.position = c(0.7, 0.9))+
  labs(title = "Evolution of Relative Hispanic Supply(IV) ", x = "Year Income Earned", 
       y = "Relative Hours Worked")+
  scale_x_continuous(breaks = pretty(cm_wide$YEAR, n = 10))+
  scale_y_continuous(breaks = pretty(cm_wide$z_hat, n = 10))

w_fit_iv = ggplot(data = cm_wide) +
  geom_line(aes(x = YEAR, y = w, col = "Wage Premium"), size = 1) +
  geom_line(aes(x = YEAR, y = w_pred_iv, col = "Predicted Wage Premium"), size = 1) + 
  theme_minimal()+
  scale_color_manual("", values = c("#064375", "#BF7112"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_text(face="bold", size = 10),
        legend.position = c(0.7, 0.9))+
  labs(title = "Fit of Hispanic Wage Premium (IV)", x = "Year Income Earned", 
       y = "Hispanic Wage Premium") +
  scale_x_continuous(breaks = pretty(cm_wide$YEAR, n = 10))+
  scale_y_continuous(breaks = pretty(cm_wide$w, n = 10))

w_detrend_fit_iv = ggplot(data = cm_wide) +
  geom_line(aes(x = YEAR, y = w_detrend, color = "Detrened Wage Premium"), size =1) +
  geom_line(aes(x = YEAR, y = w_detrend_pred_iv, color = "Predicted Detrended Wage Premium"), size = 1) + 
  theme_minimal() +
  scale_color_manual("", values = c("#064375", "#BF7112")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_text(face="bold", size = 10),
        legend.position = c(0.7, 0.9))+
  labs(title = "Fit of Detrended Hispanic Premium (IV)", x = "Year Income Earned", 
       y = "Detrended Hispanic Wage Premium") +
  scale_x_continuous(breaks = pretty(cm_wide$YEAR, n = 10))+
  scale_y_continuous(breaks = pretty(cm_wide$w_detrend, n = 10))

second_scale_iv <- 0.1
w_d_s_d_iv = ggplot(data = cm_wide) +
  geom_line(aes(x = YEAR, y = z_hat_detrend, color = "Detrended Hispanic Relative Supply"), size =1) +
  geom_line(aes(x = YEAR, y = w_detrend * second_scale_iv, color = "Detrended Hispanic Wage Premium"), size = 1) + 
  theme_minimal() +
  scale_color_manual("", values = c("#064375", "#BF7112")) +
  theme(plot.title = element_text(hjust = 0, size = 20),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_text(face="bold", size = 10),
        legend.position = c(0.5, 0.1),
        legend.text=element_text(size = 14 ))+
  labs(title = "Relationship between Detrend Series(IV)", x = "Year Income Earned", 
       y = "Detrended Hispanic Relative Supply") +
  scale_x_continuous(breaks = pretty(cm_wide$YEAR, n = 10))+
  scale_y_continuous(sec.axis = sec_axis(~.*1 / second_scale_iv ,name = "Detrended Hispanic Wage Premium"))

g3 = grid.arrange(rel_s_iv,w_fit_iv,w_detrend_fit_iv,w_d_s_d_iv)
ggsave("g3.jpg", g3, width = 12, height = 10)

####Shift Share Analysis####
ss <- shift_share %>% 
  filter(YEAR %in% c(1971, 2017)) %>%
  arrange(YEAR)

ss <- reshape(ss,
              timevar = "hispan_dum",
              idvar = c("YEAR", "IND90LY"),
              direction = "wide")
ss <- ss %>% 
  mutate(inc_ind = incwage_ind_y.0 + incwage_ind_y.1) %>%
  filter(!is.na(inc_ind)) %>%
  rename(inc_ind_1 = incwage_ind_y.1) %>%
  select(YEAR, IND90LY, inc_ind_1, inc_ind)

ss_year_agg <- ss %>%
  group_by(YEAR) %>%
  summarize(inc_total_1 = sum(inc_ind_1),
            inc_total = sum(inc_ind))

ss <- ss %>% merge(ss_year_agg, by ="YEAR")
ss <- mutate(ss, p_1_total = inc_total_1 / inc_total)

ss <- ss %>% mutate(
  p_1_ind = inc_ind_1 / inc_ind,
  p_ind_total = inc_ind / inc_total)

ss <- ss[,c("YEAR","IND90LY","p_1_total","p_1_ind","p_ind_total")]

full_ind_year <- expand.grid(unique(ss$YEAR), unique(ss$IND90LY))
names(full_ind_year) <- c("YEAR", "IND90LY")

ss <- left_join(full_ind_year, ss, by = c("YEAR", "IND90LY"))
clean <- function(x){ifelse(is.na(x), 0, x)}
ss$p_1_ind <- mapply(clean, ss$p_1_ind)
ss$p_ind_total <- mapply(clean, ss$p_ind_total)
ss$p_1_total <- mapply(clean, ss$p_1_total)

ss_year_diff <- reshape(ss, 
                        timevar = "YEAR",
                        idvar = "IND90LY",
                        direction = "wide")

# diff
ss_year_diff <- ss_year_diff %>%
  mutate(diff_p_1_ind = p_1_ind.2017 - p_1_ind.1971,
         mean_p_1_ind = 0.5 * (p_1_ind.2017 + p_1_ind.1971),
         diff_p_ind_total = p_ind_total.2017 - p_ind_total.1971,
         mean_p_ind_total = 0.5 * (p_ind_total.2017 + p_ind_total.1971),
         diff_p_1_total = p_1_total.2017 - p_1_total.1971) %>%
  select(IND90LY,diff_p_1_ind,mean_p_1_ind,diff_p_ind_total,mean_p_ind_total,diff_p_1_total)

shift_share_complete <- ss_year_diff %>%
  transmute(btws = mean_p_1_ind * diff_p_ind_total,
            withins = mean_p_ind_total * diff_p_1_ind,
            totals = diff_p_1_total) %>%
  summarize(btw = sum(btws),
            within = sum(withins),
            total = mean(totals))


