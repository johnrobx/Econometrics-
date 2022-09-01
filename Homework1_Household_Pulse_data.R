#R Basics for Lecture 1
#Econ B2000, Statistics and Introduction to Econometrics
#John Robison 
#8/31/2022
#Group Members: Safinaz Ali, Suguru Iwashiro




install.packages("tidyverse")
install.packages("plyr")

library(tidyverse)
library(plyr)

setwd("/Users/Owner/Desktop/Econometrics")
getwd()

x <- 1:50
w <- 1 + sqrt(x)/2
example1 <- data.frame(x=x, y= x + rnorm(x)*w)
attach(example1)

fm <- lm(y ~ x)
summary(fm)

lrf <- lowess(x, y)
plot(x, y)
lines(x, lrf$y)
abline(0, 1, lty=3)
abline(coef(fm))

detach()

setwd("/Users/Owner/Desktop/Econometrics/ecob2000_lecture1")


load("Household_Pulse_data.RData")

Household_Pulse_data[1:10,1:7]

attach(Household_Pulse_data)

summary(Household_Pulse_data)


summary(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "transgender"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "other"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "NA"])

mean(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
mean(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "male"])


######Start of "R Basics for Lecture 1 A"################################


hist(TBIRTH_YEAR[(TBIRTH_YEAR < 1950)])


mean(TBIRTH_YEAR[ (GENID_DESCRIBE == "female") & (TBIRTH_YEAR > 1933) ]) 

summary(EEDUC)

summary(EST_ST)

summary(INCOME)

ddply(Household_Pulse_data, .(EST_ST), summarize, mean = 
        round(mean(2021 - TBIRTH_YEAR), 2), sd = round(sd(2021 - TBIRTH_YEAR), 2), 
      n_obsv = length(EST_ST))

ddply(Household_Pulse_data, .(EST_ST), summarize, 
      age90th = quantile((2021 - TBIRTH_YEAR),probs = 0.9), 
      age10th = quantile((2021 - TBIRTH_YEAR),probs = 0.1), 
      n_obs = length(TBIRTH_YEAR))

table(EEDUC,GENID_DESCRIBE)

xtabs(~EEDUC + GENID_DESCRIBE)

prop.table(table(EEDUC,GENID_DESCRIBE))

#Method 1
mean(TBIRTH_YEAR[(REGION == "Northeast")])

#Method 2 
restrict1 <- as.logical((REGION == "Northeast"))
dat_northeast <- subset(Household_Pulse_data, restrict1)

detach()
attach(dat_northeast)

mean(TBIRTH_YEAR)

detach()

####################################################################
#######END OF PROVIDED CODE#########################################
####################################################################

#gender distribution by state
table(EST_ST,GENID_DESCRIBE)

# interesting in every state the majority of survey respondents were women 

table1 = table(EST_ST,GENID_DESCRIBE)
size(table1)


df <- data.frame(table(EST_ST,GENID_DESCRIBE))

df[new]

