####Setting####
#packages#
library(tidyverse)
library(data.table)
library(haven)
library(reshape2)

# clean up
rm(list = ls())

# setwd
wd = "C:/Users/zyj37/Desktop/MAE/Winter/ECON 424/Homework/HW2" # change to your own
setwd(wd)
rm(wd)


####Load Data####
usa <- fread("usa.csv")
usa <- usa %>%
  select(-RACESINGD, -EMPSTATD, -EDUCD, -BPLD)


####Data Cleaning(demographic)####
attach(usa)

# 1 age drop out of work force (btw 16 and 65 last year)
sum(is.na(AGE)) # there is no NA in the data
age_keep <- AGE >= 17 & AGE <= 65

# 2 drop weight<0
sum(is.na(PERWT)) #there is no NA in the data
perwt_keep <- PERWT >= 0

# 3 drop group quarters
sum(is.na(GQ))
gq_keep <- GQ < 3

# 4,5,6 drop missing values in educ, employment status and birthplace
sum(is.na(EDUC))
sum(is.na(EMPSTAT))
sum(is.na(BPL))
educ_keep <- EDUC > 0
empstat_keep <- EMPSTAT > 0
bpl_keep <- BPL < 999

usa_keep <- age_keep & perwt_keep & gq_keep & educ_keep & empstat_keep & bpl_keep
usa <- usa[usa_keep, ]
detach(usa)
rm(age_keep, perwt_keep, gq_keep, educ_keep, empstat_keep, bpl_keep, usa_keep)


#### labor bins ####
attach(usa)

# education dummies
##(edu5=1: Grade 8 and under Grade 8; edu5=2: Grade between 9 and 11; edu5=3: Grade 12; edu5=4: 1,2 and 3 years of college; edu5=5: 4 and 5+ years of college)
make_edu5 <-  function(EDUC) {
  edu5  <-  0
  if (EDUC <= 2) {
    edu5 <- 1
  }
  if (EDUC >= 3 && EDUC <= 5) {
    edu5 <- 2
  }
  if (EDUC == 6) {
    edu5 <- 3
  }
  if (EDUC >= 7 && EDUC <= 9) {
    edu5 <- 4
  }
  if (EDUC >= 10) {
    edu5 <- 5
  }
  return(edu5)
}


usa$edu5 <- mapply(make_edu5, EDUC) 
sum(is.na(usa$edu5)) # double check
rm(make_edu5)

# gender and race dummies
sum(is.na(SEX))
sum(is.na(RACESING))
usa <- usa %>%
  mutate(female = as.numeric(SEX == 2),
         white = as.numeric(RACESING == 1))

usa$lbin <-
  mapply(function(a, e, f, w) {
    str_c(a, e, f, w)
  }, usa$AGE, usa$edu5, usa$female, usa$white)

#### other dummies ####
sum(is.na(usa$CITIZEN))
usa <- usa %>%
  mutate(
    lb = ifelse(EMPSTAT < 3, 1, 0), # labor force
    col = as.numeric(edu5 == 4 | edu5 == 5), # college
    native = as.numeric(CITIZEN == 1 | BPL < 100) # born in US or born in foreign with US parents
)
detach(usa)

#### data cleaning(income) ####
# missing, na in income and working weeks
attach(usa)
drop_inc <-  INCWAGE == 999999 | INCWAGE == 999998 # na and missing
drop_wkswork1 <- WKSWORK1 == 0
usa <- usa[(!drop_inc) & (!drop_wkswork1), ]
detach(usa)
rm(drop_inc, drop_wkswork1)

# top coding
attach(usa)
incwage_top <-
  (75000 * 1.5) * (INCWAGE >= 75000 & YEAR == 1980) + 
  (200000 * 1.5) * (INCWAGE >= 200000 & YEAR == 2007)
usa <- usa %>%
  mutate(
    INCWAGE = ifelse(incwage_top == 0, INCWAGE, incwage_top)
)
rm(incwage_top)

# total hours
usa <- usa %>%
  mutate(hours = UHRSWORK * WKSWORK1)
detach(usa)
#### construct dataset CZ ####

# merge with DD's cz

## change format
usa <- usa %>%
  mutate(CNTYGP98_f = str_pad(CNTYGP98, 3, side = "left", pad = "0"),
         PUMA_f = str_pad(PUMA, 4, side = "left", pad = "0")
         )

## generate key for merge
usa <- usa %>%
  mutate(
    COUNTY = ifelse(YEAR == 1980, CNTYGP98_f, PUMA_f),
    key = mapply(function(s,c){str_c(s,c)}, STATEFIP, COUNTY)
  )

## uploading CZ from DD
cz_1980 <- read_dta("1980_czone.dta")
cz_1980 <- as.data.frame(cz_1980)
cz_1980 <- cz_1980 %>%
  rename(key = ctygrp1980)
cz_2007 <- read_dta("2007_czone.dta")
cz_2007 <- as.data.frame(cz_2007)
cz_2007 <- cz_2007 %>%
  rename(key = puma2000)
cz_dorn <- rbind(cz_1980, cz_2007)

## select variables for cz
usa <- usa %>%
  select(
    YEAR,
    key,
    PERWT,
    EMPSTAT,
    INCWAGE,
    BPL,
    IND,
    lb,
    col,
    lbin,
    hours,
    native,
    female,
    white
  ) %>%
  arrange(YEAR)

## merge usa with DD's cz
rm(cz_1980, cz_2007)
cz <- merge(usa, cz_dorn, by = "key")
rm(usa, cz_dorn)
# adjustment

## person weights needs to be multipled by a factor
cz <- cz %>%
  mutate(PERWT = PERWT * afactor)

## adjusted variables by person weights
cz <- cz %>%
  mutate(
    hours = hours * PERWT,
    INCWAGE = INCWAGE * PERWT,
    unemp_w = ifelse(EMPSTAT == 2, 1, 0) * PERWT,
    emp_w = ifelse(EMPSTAT == 1, 1, 0) * PERWT,
    lb_w = lb * PERWT,
    native_w = native * PERWT,
    immigrant_w = (1 - native) * PERWT,
    col_w = col * PERWT,
    female_w = female * PERWT
)

## generate dummies for manufacturies
cz <- cz %>%
  mutate(
    manu = ifelse(((IND >= 100 & IND <= 392 & YEAR ==1980)|
                      (IND >= 1070 & IND <= 3990 & YEAR == 2007)),
                      1,0),
    manu_w = manu * PERWT)

#### constructing independent variable:  immigration inflow ####
rm(immigrants_cz_y, immigrants_cz_y_wide, immigrants_inflow_ratio, immigrants_inflow_ratio_cal, immigrants_w,pop_cz_1980,pop_cz_total_1980, pop_total_1980)
## total pop in 1980 grouped by cz
pop_cz_1980 <- cz %>%
  filter(YEAR == 1980) %>%
  group_by(czone) %>%
  summarize(pop_cz_1980 = sum(PERWT))
pop_total_1980 <- cz %>%
  filter(YEAR == 1980) %>%
  group_by(YEAR) %>%
  summarize(pop_total_1980 = sum(PERWT))

## adjust population in 1980 for Card instrument
pop_cz_1980$YEAR = 1980
pop_cz_total_1980 <- merge(pop_cz_1980, pop_total_1980, by = "YEAR")
pop_cz_total_1980$pop_total_1980_net <- pop_cz_total_1980$pop_total_1980 - pop_cz_total_1980$pop_cz_1980

## individuals of immigrants
immigrants_w <- cz %>%
  filter(native == 0) %>%
  select(YEAR, BPL, czone, immigrant_w)


## individuals of immigrants grouped by cz in 1980 and 2007
immigrants_cz_y <- immigrants_w %>% 
  group_by(czone, YEAR) %>%
  summarize(immigrants_cz_y = sum(immigrant_w))

## immigration inflow  
immigrants_cz_y_wide <- dcast(immigrants_cz_y, czone ~ YEAR, value.var = "immigrants_cz_y")
colnames(immigrants_cz_y_wide) = c("czone", "i_1980", "i_2007")

immigrants_inflow_ratio_cal <- merge(immigrants_cz_y_wide, pop_cz_total_1980, by = "czone")
immigrants_inflow_ratio <- immigrants_inflow_ratio_cal %>%
  mutate(immigration_inflow_ratio = (i_2007 - i_1980) / pop_cz_1980)
rm(immigrants_cz_y_wide, immigrants_inflow_ratio_cal)
rm(pop_cz_1980, pop_total_1980)

#### card instrument ####
# construct source region
attach(immigrants_w)
make_source_regions <- 1 * (BPL == 600) + # AFRICA
  2 * (BPL == 150) + # CANADA
  3 * (BPL >= 210 & BPL <= 300) + # CENTRAL/SOUTH AMERICA
  4 * (BPL == 500) + # CHINA
  5 * (BPL >= 450 & BPL <= 465) + # CENTRAL/EAST EU
  6 * (BPL == 521) + # INIDA
  7 * (BPL == 200) + # MEXICO
  8 * (BPL >= 501 & BPL <= 509) + # EAST ASIA
  9 * ((BPL >= 510 & BPL <= 520) | (BPL >= 522 & BPL <= 599)) + # SOUTHEAST/SOUTHWEST ASIA
  10 * (BPL >= 700 & BPL <= 710) + # OCEANIA
  11 * (BPL >= 420 & BPL <= 429)# WEST EU
detach(immigrants_w)

immigrants_w = immigrants_w %>%
  mutate(source_regions = make_source_regions)
rm(make_source_regions)


# construct fcs(fraction of source region over czone in 1980) 
## immigrants in 1980
immigrants_w_1980 <- immigrants_w %>%
  filter(YEAR == 1980)

## immigrants in 1980 grouped by cz sr
immigrants_cz_sr_1980 <- immigrants_w_1980 %>%
  group_by(czone, source_regions) %>%
  summarize(immigrants_cz_sr_1980 = sum(immigrant_w))

## immigrants in 1980 grouped by cz
immigrants_cz_1980 <- immigrants_w_1980 %>%
  group_by(czone) %>%
  summarize(immigrants_cz_1980 = sum(immigrant_w))

# get ratio
immigrants_czsr_cz_1980 <- merge(immigrants_cz_1980, immigrants_cz_sr_1980, by = "czone")
immigrants_fcs_cal <- immigrants_czsr_cz_1980 %>%
  mutate(fcs = immigrants_cz_sr_1980 / immigrants_cz_1980)

# full cz sr
full_cz_sr<-expand.grid(unique(immigrants_w$source_regions),unique(immigrants_w$czone))
names(full_cz_sr) <- c("source_regions","czone")
immigrants_cz_sr_full <- left_join(full_cz_sr, immigrants_fcs_cal, by=c("source_regions", "czone"))
immigrants_fcs <- immigrants_fcs_cal %>%
  mutate(fcs = ifelse(is.na(fcs), 0, fcs))

# Construct migration flow by region substracting the corresponding value in the community zone(remove effect of cz demand)

#### immigrants grouped by year sr
immigrants_sr_y <- immigrants_w %>%
  group_by(source_regions, YEAR) %>%
  summarize(immigrants_sr_y = sum(immigrant_w))

#### immigrants grouped by year sr cz
immigrants_cz_sr_y <- immigrants_w %>%
  group_by(czone, source_regions, YEAR) %>%
  summarize(immigrants_cz_sr_y = sum(immigrant_w))

## net immigrants by cz sr and y, equals immigrants by sr y minus immigrants by sr cz and y means excluding the immigrants in this cz
immigrants_czsr_y_sr_y <- merge(immigrants_cz_sr_y, immigrants_sr_y, by = c("source_regions", "YEAR"))
immigrants_cz_sr_y_net <- immigrants_czsr_y_sr_y %>%
  mutate(immigrants_cz_sr_y_net = immigrants_sr_y - immigrants_cz_sr_y)

net_migration <-immigrants_cz_sr_y_net[, c("YEAR", "source_regions", "czone", "immigrants_cz_sr_y_net") ] 

net_migration_wide <- reshape(net_migration, 
                              timevar = "YEAR",
                              idvar = c("source_regions","czone"),
                              direction = "wide")

## I2007 -I1980 by cz sr
net_migration_change <- net_migration_wide %>%
  mutate(
    immigrants_cz_sr_net.2007 = ifelse(is.na(immigrants_cz_sr_y_net.2007), 0, immigrants_cz_sr_y_net.2007),
    immigrants_cz_sr_net.1980 = ifelse(is.na(immigrants_cz_sr_y_net.1980), 0, immigrants_cz_sr_y_net.1980),
    immigrants_net_change = immigrants_cz_sr_net.2007 - immigrants_cz_sr_net.1980,
    immigrants_cz_sr_net.2007 = NULL,
    immigrants_cz_sr_y_net.2007 = NULL,
    immigrants_cz_sr_net.1980 = NULL,
    immigrants_cz_sr_y_net.1980 = NULL)

# card instrument
# merge fcs with I2007-I1980
card_iv_part <- merge(immigrants_fcs, net_migration_change, by=c("source_regions","czone"))
card_iv_part <- card_iv_part %>%
  mutate(product = fcs * immigrants_net_change)

# multiple fcs with I2007 - I1980 and sum up s
card_iv_part = card_iv_part %>%
  group_by(czone) %>%
  summarize(product = sum(product))

indep <- merge(immigrants_inflow_ratio, card_iv_part, by = "czone")

x <- indep %>%
  select(czone, immigration_inflow_ratio, product, pop_total_1980_net) %>%
  mutate(card_iv = product / pop_total_1980_net) %>%
  select(czone, immigration_inflow_ratio, card_iv)
rm(card_iv_part, full_cz_sr, immigrants_cz_1980, immigrants_cz_sr_1980, immigrants_cz_sr_full, immigrants_cz_sr_y, immigrants_cz_sr_y_net, immigrants_cz_y, immigrants_czsr_cz_1980, immigrants_czsr_y_sr_y, immigrants_fcs, immigrants_fcs_cal, immigrants_inflow_ratio, immigrants_sr_y, immigrants_w_1980, net_migration, net_migration_change, net_migration_wide, pop_cz_total_1980)
rm(indep)

#### construct dependent variables 1: native wage####
natives_w <- cz %>%
  filter(native == 1)

# generate time-invariant weight
hours_lbin_cz <- natives_w %>%
  group_by(lbin, czone) %>%
  summarize(hours_lbin_cz = sum(hours))

hours_cz <- natives_w %>%
  group_by(czone) %>%
  summarize(hours_cz = sum(hours))

wage_weight <- merge(hours_lbin_cz, hours_cz, by = "czone")  
wage_weight <- wage_weight %>%
  mutate(wage_weight = hours_lbin_cz / hours_cz,
         hours_cz = NULL,
         hours_lbin_cz = NULL)

# generating Real wages by year, CZ, and lbin
hours_income_lbin_cz_y <- natives_w %>%
  group_by(lbin, czone, YEAR) %>%
  summarize(hours_lbin_cz_y = sum(hours),
            income_lbin_cz_y = sum(INCWAGE))

## gdp deflator
GDPDEF <- fread("deflator.csv")
GDPDEF$YEAR = as.numeric(substr(GDPDEF$DATE, 0, 4) )
GDPDEF$DATE<-NULL

## deflation
hours_income_lbin_cz_y <- merge(hours_income_lbin_cz_y, GDPDEF,by="YEAR")
income_wage_lbin_cz_y <- hours_income_lbin_cz_y %>%
  mutate(
    income_lbin_cz_y_real =  income_lbin_cz_y / deflator,
    wage_lbin_cz_y_real = income_lbin_cz_y_real / hours_lbin_cz_y
  )

# use weight
wage_lbin_cz_y_ca <- merge(income_wage_lbin_cz_y, wage_weight, by=c("czone","lbin"))
wage_lbin_cz_y_ca <- wage_lbin_cz_y_ca %>%
  mutate(product = wage_lbin_cz_y_real * wage_weight)

# outcome 1 average wage
y1 <- wage_lbin_cz_y_ca %>%
  group_by(YEAR, czone) %>%
  summarize(native_wage_cz_y = sum(product))

y1 <- as.data.frame(y1)
y1 <- reshape(y1,
              timevar = "YEAR",
              idvar = "czone",
              direction = "wide")

y1 <- y1 %>%
  mutate(
    native_wage_change = log(native_wage_cz_y.2007 / native_wage_cz_y.1980),
    native_wage_cz_y.1980 = NULL,
    native_wage_cz_y.2007 = NULL
)
rm(GDPDEF, hours_income_lbin_cz_y, income_wage_lbin_cz_y, wage_lbin_cz_y_ca, hours_cz, hours_lbin_cz)

#### construct dependent variables 2: unemployment ####

# weight by pop
pop_lbin_cz <- natives_w %>%
  group_by(lbin,czone) %>%
  summarize(pop_lbin_cz = sum(PERWT))

pop_cz <- natives_w %>%
  group_by(czone) %>%
  summarize(pop_cz = sum(PERWT))

unemploy_weight <- merge(pop_lbin_cz, pop_cz, by = "czone")
unemploy_weight <- unemploy_weight %>%
  mutate(unemploy_weight = pop_lbin_cz / pop_cz,
         pop_cz = NULL,
         pop_lbin_cz = NULL)

# Generating Native unemployment rate by year, CZ, and lbin
unemp_lb_lbin_cz_y <- natives_w %>%
  group_by(YEAR, czone, lbin) %>%
  summarize(unemploy_lbin_cz_y = sum(unemp_w),
            lb_lbin_cz_y = sum(lb_w)) %>%
  mutate(unemploy_rate_lbin_cz_y = unemploy_lbin_cz_y / lb_lbin_cz_y)

unemp_lb_lbin_cz_y$unemploy_rate_lbin_cz_y <- ifelse(is.nan(unemp_lb_lbin_cz_y$unemploy_rate_lbin_cz_y), 0, unemp_lb_lbin_cz_y$unemploy_rate_lbin_cz_y)

# use weight
unemp_rate_lbin_cz_y_ca <- merge(unemp_lb_lbin_cz_y, unemploy_weight, by = c("czone", "lbin"))
unemp_rate_lbin_cz_y_ca <- unemp_rate_lbin_cz_y_ca %>%
  mutate(product = unemploy_rate_lbin_cz_y * unemploy_weight)

# outcome 2 unemploy rate
y2 <- unemp_rate_lbin_cz_y_ca %>%
  group_by(YEAR, czone) %>%
  summarize(native_unemploy_rate_cz_y = sum(product))

y2 <- as.data.frame(y2)
y2 <- reshape(y2,
              timevar = "YEAR",
              idvar = "czone",
              direction = "wide")

y2 <- y2 %>%
  mutate(
    native_unemploy_rate_change = log(native_unemploy_rate_cz_y.2007 / native_unemploy_rate_cz_y.1980),
    native_unemploy_rate_cz_y.2007 = NULL,
    native_unemploy_rate_cz_y.1980 = NULL
  )
rm(pop_cz, pop_lbin_cz, unemp_lb_lbin_cz_y, unemp_rate_lbin_cz_y_ca, unemploy_weight)


#### construct dependent variables 3: labor participation rate ####
# use some weight in variable 1 

# Generating Native labor force participation rate by year, CZ, and lbin
pop_lb_lbin_cz_y <- natives_w %>%
  group_by(YEAR, czone, lbin) %>%
  summarize(pop_lbin_cz_y = sum(PERWT),
            lb_lbin_cz_y = sum(lb_w)) %>%
  mutate(parti_rate_lbin_cz_y = lb_lbin_cz_y / pop_lbin_cz_y)

# use weight
parti_rate_lbin_cz_y_ca <- merge(pop_lb_lbin_cz_y, wage_weight, by = c("czone", "lbin"))
parti_rate_lbin_cz_y_ca <- parti_rate_lbin_cz_y_ca %>%
  mutate(product = parti_rate_lbin_cz_y * wage_weight)

# outcome 3 participation rate
y3 <- parti_rate_lbin_cz_y_ca %>%
  group_by(YEAR, czone) %>%
  summarize(native_parti_rate_cz_y = sum(product))

y3 <- as.data.frame(y3)
y3 <- reshape(y3,
              timevar = "YEAR",
              idvar = "czone",
              direction = "wide")

y3 <- y3 %>%
  mutate(
    native_parti_rate_change = log(native_parti_rate_cz_y.2007 / native_parti_rate_cz_y.1980),
    native_parti_rate_cz_y.1980 = NULL,
    native_parti_rate_cz_y.2007 = NULL
  )


#### construct dependent variables 4: employ share  change  in manu ####
manuf <- cz %>%
  filter(manu == 1)

# weight by pop
pop_lbin_cz_manuf <- manuf %>%
  group_by(lbin,czone) %>%
  summarize(pop_lbin_cz = sum(PERWT))

pop_lbin_manuf <- manuf %>%
  group_by(czone) %>%
  summarize(pop_cz = sum(PERWT))

employ_weight_manu <- merge(pop_lbin_cz_manuf, pop_lbin_manuf, by = "czone")
employ_weight_manu <- employ_weight_manu %>%
  mutate(employ_weight_m = pop_lbin_cz / pop_cz,
         pop_cz = NULL,
         pop_lbin_cz = NULL)

# Generating Native employment rate in manuf by year, CZ, and lbin
manuf$emp_w_native <- manuf$emp_w * manuf$native

emp_share_native_total_lbin_cz_y <- manuf %>%
  group_by(YEAR, czone, lbin) %>%
  summarize(employ_lbin_cz_y = sum(emp_w),
            employ_native_lbin_cz_y = sum(emp_w_native)) %>%
  mutate(employ_share_lbin_cz_y = employ_native_lbin_cz_y / employ_lbin_cz_y)

emp_share_native_total_lbin_cz_y$employ_share_lbin_cz_y <- ifelse(is.nan(emp_share_native_total_lbin_cz_y$employ_share_lbin_cz_y), 0, emp_share_native_total_lbin_cz_y$employ_share_lbin_cz_y)

# use weight
emp_share_cz_y_ca <- merge(emp_share_native_total_lbin_cz_y, employ_weight_manu, by = c("czone", "lbin"))
emp_share_cz_y_ca <- emp_share_cz_y_ca %>%
  mutate(product = employ_share_lbin_cz_y * employ_weight_m)

# outcome 4 employ share
y4 <- emp_share_cz_y_ca %>%
  group_by(YEAR, czone) %>%
  summarize(emp_share_cz_y_ca = sum(product))

y4 <- as.data.frame(y4)
y4 <- reshape(y4,
              timevar = "YEAR",
              idvar = "czone",
              direction = "wide")

y4 <- y4 %>%
  mutate(
    employ_share_m_change = log(emp_share_cz_y_ca.2007 / emp_share_cz_y_ca.1980),
    emp_share_cz_y_ca.2007 = NULL,
    emp_share_cz_y_ca.1980 = NULL
  )

y = merge(y1, y2, by = "czone")
y = merge(y, y3, by = "czone")
y = merge(y, y4, by = "czone")
rm(emp_share_cz_y_ca, emp_share_native_total_lbin_cz_y, employ_weight_manu, parti_rate_lbin_cz_y_ca, pop_lb_lbin_cz_y, pop_lbin_cz_manuf, pop_lbin_manuf, wage_weight)


#### covariates ####
#You can use, for example:
rm(immigrants_w, manuf, natives_w)
#Share of college educated in a CZ lf in 1980
lf_1980 <- cz %>%
  filter(YEAR == 1980, lb == 1)

x2 <- lf_1980 %>%
  group_by(czone) %>%
  summarize(lf_1980 = sum(PERWT),
            college = sum(col_w)) %>%
  mutate(col_share = college / lf_1980,
         lf_1980 = NULL,
         college = NULL)
  
#Share of manuf in CZ labor force in 1980

x3 <- lf_1980 %>%
  group_by(czone) %>%
  summarize(lf_1980 = sum(PERWT),
            manu = sum(manu_w)) %>%
  mutate(manu_share = manu / lf_1980,
         lf_1980 = NULL,
         manu = NULL)

# Share of women in CZ labor force in 1980
x4 <- lf_1980 %>%
  group_by(czone) %>%
  summarize(lf_1980 = sum(PERWT),
            female = sum(female_w)) %>%
  mutate(female_share = female / lf_1980,
         lf_1980 = NULL,
         female = NULL)

covariates = merge(x2, x3, by = "czone")
covariates = merge(covariates, x4, by = "czone")

#### 2SLS ####

# merging outcome variables and covariates
data <-merge(y, x, by="czone") 
data <-merge(data, covariates, by="czone") 
write.csv(data, "data.csv")
rm(x, x2, x3, x4, y, y1, y2, y3, y4, covariates, lf_1980)
# First stage
reg_immigration_inflow_ratio_card_iv <- lm(immigration_inflow_ratio ~ card_iv + col_share + manu_share + female_share, data = data)
summary(reg_immigration_inflow_ratio_card_iv)
x_c.hat <- fitted.values(reg_immigration_inflow_ratio_card_iv)
data = cbind(data, x_c.hat)
# Second stage
data = read.csv("data.csv")
# outcome 1
reg_native_wage_change_x.c_1 <- lm(native_wage_change ~ x_c.hat, data = data)
summary(reg_native_wage_change_x.c_1)


g1 = ggplot(data = data, aes(x = x_c.hat, y = native_wage_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_wage_change_x.c_1)[1],
    slope = coef(reg_native_wage_change_x.c_1)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Native Wage Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))+
  labs(x = "Immigration Shock(IV)", y = "Native Wage Change")

reg_native_wage_change_x.c_2 <- lm(native_wage_change ~ x_c.hat + col_share, data = data)
summary(reg_native_wage_change_x.c_2)
g2 = ggplot(data = data, aes(x = x_c.hat, y = native_wage_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_wage_change_x.c_2)[1],
    slope = coef(reg_native_wage_change_x.c_2)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration shock on Native Wage Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  labs(x = "Immigration Shock(IV)", y = "Native Wage Change")



reg_native_wage_change_x.c_3 <- lm(native_wage_change ~ x_c.hat + col_share + manu_share, data = data)
summary(reg_native_wage_change_x.c_3)

g3 = ggplot(data = data, aes(x = x_c.hat, y = native_wage_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_wage_change_x.c_3)[1],
    slope = coef(reg_native_wage_change_x.c_3)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration shock on Native Wage Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Native Wage Change" )



reg_native_wage_change_x.c_4 <- lm(native_wage_change ~ x_c.hat + col_share + manu_share + female_share, data = data)
summary(reg_native_wage_change_x.c_4)

g4 = ggplot(data = data, aes(x = x_c.hat, y = native_wage_change)) +
  geom_abline(
    intercept = coef(reg_native_wage_change_x.c_4)[1],
    slope = coef(reg_native_wage_change_x.c_4)[2],
    color = '#064375'
  ) + geom_point() + 
  ggtitle("Effect of Immigration shock on Native Wage Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +   
  labs(x = "Immigration Shock(IV)", y = "Native Wage Change")


# outcome 2
reg_native_unemploy_rate_change_x.c_1 <- lm(native_unemploy_rate_change ~ x_c.hat, data = data)
summary(reg_native_unemploy_rate_change_x.c_1)

g5 = ggplot(data = data, aes(x = x_c.hat, y = native_unemploy_rate_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_unemploy_rate_change_x.c_1)[1],
    slope = coef(reg_native_unemploy_rate_change_x.c_1)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Native Unemployment Rate Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Native Unemployment Rate Change")


reg_native_unemploy_rate_change_x.c_2 <- lm(native_unemploy_rate_change ~ x_c.hat + col_share, data = data)
summary(reg_native_unemploy_rate_change_x.c_2)

g6 = ggplot(data = data, aes(x = x_c.hat, y = native_unemploy_rate_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_unemploy_rate_change_x.c_2)[1],
    slope = coef(reg_native_unemploy_rate_change_x.c_2)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Native Unemployment Rate Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Native Unemployment Rate Change")


reg_native_unemploy_rate_change_x.c_3 <- lm(native_unemploy_rate_change ~ x_c.hat + col_share + manu_share, data = data)
summary(reg_native_unemploy_rate_change_x.c_3)

g7 = ggplot(data = data, aes(x = x_c.hat, y = native_unemploy_rate_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_unemploy_rate_change_x.c_3)[1],
    slope = coef(reg_native_unemploy_rate_change_x.c_3)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Native Unemployment Rate Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14 )) + 
  labs(x = "Immigration Shock(IV)", y = "Native Unemployment Rate Change")


reg_native_unemploy_rate_change_x.c_4 <- lm(native_unemploy_rate_change ~ x_c.hat + col_share + manu_share + female_share, data = data)
summary(reg_native_unemploy_rate_change_x.c_4)

g8 = ggplot(data = data, aes(x = x_c.hat, y = native_unemploy_rate_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_unemploy_rate_change_x.c_4)[1],
    slope = coef(reg_native_unemploy_rate_change_x.c_4)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Native Unemployment Rate Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Native Unemployment Rate Change")




# outcome 3
reg_native_parti_rate_change_x.c_1 <- lm(native_parti_rate_change ~ x_c.hat, data = data)
summary(reg_native_parti_rate_change_x.c_1)

g9 = ggplot(data = data, aes(x = x_c.hat, y = native_parti_rate_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_parti_rate_change_x.c_1)[1],
    slope = coef(reg_native_parti_rate_change_x.c_1)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Native Labor Participation Rate Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Native Labor Participation Rate Change")


reg_native_parti_rate_change_x.c_2 <- lm(native_parti_rate_change ~ x_c.hat + col_share, data = data)
summary(reg_native_parti_rate_change_x.c_2)

g10 = ggplot(data = data, aes(x = x_c.hat, y = native_parti_rate_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_parti_rate_change_x.c_2)[1],
    slope = coef(reg_native_parti_rate_change_x.c_2)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Native Labor Participation Rate Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Native Labor Participation Rate Change")


reg_native_parti_rate_change_x.c_3 <- lm(native_parti_rate_change ~ x_c.hat + col_share + manu_share, data = data)
summary(reg_native_parti_rate_change_x.c_3)


g11 = ggplot(data = data, aes(x = x_c.hat, y = native_parti_rate_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_parti_rate_change_x.c_3)[1],
    slope = coef(reg_native_parti_rate_change_x.c_3)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Native Labor Participation Rate Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Native Labor Participation Rate Change")


reg_native_parti_rate_change_x.c_4 <- lm(native_parti_rate_change ~ x_c.hat + col_share + manu_share + female_share, data = data)
summary(reg_native_parti_rate_change_x.c_4)


g12 = ggplot(data = data, aes(x = x_c.hat, y = native_parti_rate_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_native_parti_rate_change_x.c_4)[1],
    slope = coef(reg_native_parti_rate_change_x.c_4)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Native Labor Participation Rate Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Native Labor Participation Rate Change")

# outcome 4
reg_employ_share_m_change_x.c_1 <- lm(employ_share_m_change ~ x_c.hat, data = data)
summary(reg_employ_share_m_change_x.c_1 )

g13 = ggplot(data = data, aes(x = x_c.hat, y = employ_share_m_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_employ_share_m_change_x.c_1)[1],
    slope = coef(reg_employ_share_m_change_x.c_1)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Labor Share Change of the Native in Manufacture Industry") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Labor Share Change of the Native in Manufacture Industry")


reg_employ_share_m_change_x.c_2 <- lm(employ_share_m_change ~ x_c.hat + col_share, data = data)
summary(reg_employ_share_m_change_x.c_2 )

g14 = ggplot(data = data, aes(x = x_c.hat, y = employ_share_m_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_employ_share_m_change_x.c_2)[1],
    slope = coef(reg_employ_share_m_change_x.c_2)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Labor Share Change of the Native in Manufacture Industry") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Labor Share Change of the Native in Manufacture Industry")


reg_employ_share_m_change_x.c_3 <- lm(employ_share_m_change ~ x_c.hat + col_share + manu_share, data = data)
summary(reg_employ_share_m_change_x.c_3 )

g15 = ggplot(data = data, aes(x = x_c.hat, y = employ_share_m_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_employ_share_m_change_x.c_3)[1],
    slope = coef(reg_employ_share_m_change_x.c_3)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Labor Share Change of the Native in Manufacture Industry") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Labor Share Change of the Native in Manufacture Industry")




reg_employ_share_m_change_x.c_4 <- lm(employ_share_m_change ~ x_c.hat + col_share + manu_share + female_share, data = data)
reg_employ_share_m_change_x.c_4$model
summary(reg_employ_share_m_change_x.c_4 )

g16 = ggplot(data = data, aes(x = x_c.hat, y = employ_share_m_change)) +
  geom_point() +
  geom_abline(
    intercept = coef(reg_employ_share_m_change_x.c_4)[1],
    slope = coef(reg_employ_share_m_change_x.c_4)[2],
    color = '#064375'
  ) +
  ggtitle("Effect of Immigration Shock on Labor Share Change of the Native in Manufacture Industry") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  labs(x = "Immigration Shock(IV)", y = "Labor Share Change of the Native in Manu-Industry")



#### output ####
library(gridExtra)
outcome1 = grid.arrange(g1, g2, g3, g4)
outcome2 = grid.arrange(g5, g6, g7, g8)
outcome3 = grid.arrange(g9, g10, g11, g12)
outcome4 = grid.arrange(g13, g14, g15, g16)
ggsave("outcome1.jpg", outcome1, width = 12,height = 10)
ggsave("outcome2.jpg", outcome2, width = 12, height = 10)
ggsave("outcome3.jpg", outcome3, width = 12, height = 10)
ggsave("outcome4.jpg", outcome4,  width = 12, height = 10)

library(stargazer)
stargazer(reg_immigration_inflow_ratio_card_iv, align = T, no.space = T, omit.stat=c("LL","ser","f"))
stargazer(reg_native_wage_change_x.c_1, reg_native_wage_change_x.c_2, reg_native_wage_change_x.c_3, reg_native_wage_change_x.c_4, align = T, no.space = T, omit.stat=c("LL","ser","f"))
stargazer(reg_native_unemploy_rate_change_x.c_1, reg_native_unemploy_rate_change_x.c_2, reg_native_unemploy_rate_change_x.c_3, reg_native_unemploy_rate_change_x.c_4, align = T, no.space = T, omit.stat=c("LL","ser","f"))
stargazer(reg_native_parti_rate_change_x.c_1, reg_native_parti_rate_change_x.c_2, reg_native_parti_rate_change_x.c_3, reg_native_parti_rate_change_x.c_4, align = T, no.space = T, omit.stat=c("LL","ser","f"))
stargazer(reg_employ_share_m_change_x.c_1, reg_employ_share_m_change_x.c_2, reg_employ_share_m_change_x.c_3, reg_employ_share_m_change_x.c_4, align = T, no.space = T, omit.stat=c("LL","ser","f"))
