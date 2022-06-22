#load the data
rm(list=ls())
setwd("E:/2Sem_all_study_material/SDM/week9/assign8")
library(readxl)

#feature engineering 
df_trans <- read_excel("SnackChain.xlsx", sheet= "transactions")
df_stores <- read_excel("SnackChain.xlsx", sheet= "stores")
df_products <- read_excel("SnackChain.xlsx", sheet= "products")

df_products<- df_products[!( df_products$CATEGORY == "ORAL HYGIENE PRODUCTS"),]

df_trans = merge(x=df_trans,y=df_stores,by.x = "STORE_NUM", by.y = "STORE_ID", all.x = TRUE)
df_trans = merge(x=df_trans,y=df_products,by.x ="UPC", by.y = "UPC", all.x = TRUE)
colnames(df_trans)=tolower(make.names(colnames(df_trans)))    #convert to lowercase variable name
str(df_trans)

#install.packages("lubridate")
install.packages("lubridate")
library(lubridate)
df_trans$month <- lubridate::month(df_trans$week_end_date)
df_trans$year <- lubridate::year(df_trans$week_end_date)
df_trans$week_end_date <- NULL 

df_trans$msa <- as.factor(df_trans$msa)
df_trans$state <- as.factor(df_trans$state)
df_trans$segment <- as.factor(df_trans$segment)
df_trans$manufacturer <- as.factor(df_trans$manufacturer)
#df_tran$category <- as.factor(df_trans$category)
df_trans$store_num <- as.factor(df_trans$store_num)
df_trans$month <- as.factor(df_trans$month)
df_trans$year <- as.factor(df_trans$year)

#exploratory analysis 
table(df_trans$spend)
table(df_trans$spend)
table(df_trans$spend)

#visualization
hist(df_trans$hhs)
hist(log(df_trans$hhs))
hist(df_trans$spend)
hist(log(df_trans$spend))
hist(df_trans$units)
hist(log(df_trans$units))
attach(df_trans)
colSums(is.na(df_trans)) 
df_trans$parking <- NULL
df_trans$description <- NULL

# Correlation Test

library("PerformanceAnalytics")
df_temp3 <- df_trans[, c(4,7,8,17,18)]
chart.Correlation(df_temp3)   

#question1
test1 <- glm(spend~display+feature+tpr_only+msa, family = poisson(link = log), data = df_trans )
summary(test1)

test2 <- glm(units~display+feature+tpr_only+msa, family = poisson(link = log), data = df_trans )
summary(test2)

test3 <- glm(hhs~display+feature+tpr_only+msa, family = poisson(link = log), data = df_trans )
summary(test3)

#dispersion test
library(AER)
dispersiontest(test1)
dispersiontest(test2)
dispersiontest(test3)       #Since all data is overdispersed here, so I will use quasipoisson

#quasipossion 
test4 <- glm(spend~display+feature+tpr_only+msa, family = quasipoisson(link = log), data = df_trans )
summary(test4)

test5 <- glm(units~display+feature+tpr_only+msa, family = quasipoisson(link = log), data = df_trans )
summary(test5)

test6 <- glm(hhs~display+feature+tpr_only+msa, family = quasipoisson(link = log), data = df_trans )
summary(test6)

library(stargazer)
stargazer(test4, test5, test6, type="text")

#assumption test
library("car")                                # Test of multicollinearity
vif(test4)
vif(test5)
vif(test6)
library(lmtest)
dwtest(test4) 
dwtest(test5)
dwtest(test6)

#question2
library(MASS)
test7 <- glm.nb(spend~display+feature+tpr_only+month+msa+category+segment, data= df_trans )
summary(test7)
stargazer(test1, test4, test7, type="text")

#assumption test
library("car")                                # Test of multicollinearity
vif(test7)

library(lmtest)
dwtest(test7) 

#test10 <- glm.nb(spend~display+feature+tpr_only+month+msa+category+segment, data= df_trans )
#summary(test10)


#question3
library(lme4)
test12 <- lmer(units~display+feature+tpr_only+month+msa+category+segment+price + (1|upc), data = df_trans)

summary(test12)
AIC(test12)
ranef(test12)$upc

