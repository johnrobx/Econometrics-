#Homework 5 Lab 4 
#Group Names Safinaz Ali and 

#Set up working environment

setwd("/Users/johnrobison/Desktop/CUNY MA Economics/Econometrics")
load("acs2017_ny_data.RData") 



#consider what subgroup to use
attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb) # 
detach()
attach(dat_use)

#before running a regression, consider what variables should be in your model

#a linear regression, with wage as the dependent variable, a dummy for major, and other variable

# During Lab my group discussed the effect of Vetern status on level of income 

model_temp5 <- lm(INCWAGE ~ AGE + VETSTAT, data = dat_use)
summary(model_temp5)
plot(model_temp5)

# Our groups initial assumption would be that veteran status would cause an increase in income because many people that were veterans and now work at a normal job do get the benefits 
#of getting paid more then a non veteran. 
# We ended up finding that VETSTAT income was lower than the average in the chosen subset by 4740. this was not what we were expecting 
# potentially veterans getting benefits or because the subset is of a NYC population is why there income was lower. 
# A quick search will show you that on the national average Veterans status actually results in higher icome but our studying ahs found in the NYC Area this does not appear to be the case 

model_temp6 <- lm(INCWAGE ~ AGE + educ_hs + educ_somecoll + educ_college + educ_advdeg + VETSTAT, data = dat_use)
summary(model_temp6)
plot(model_temp6)

#Our group wanted to further look at how beinga veteran compared to other forms of education:

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -26546.19    3153.91  -8.417  < 2e-16 ***
#  AGE             1330.81      40.21  33.094  < 2e-16 ***
#  educ_hs        11670.34    1811.56   6.442 1.19e-10 ***
#  educ_somecoll  21769.29    1843.29  11.810  < 2e-16 ***
#  educ_college   57107.19    1805.30  31.633  < 2e-16 ***
#  educ_advdeg    81394.82    1843.94  44.142  < 2e-16 ***
#  VETSTAT         4644.45    2168.43   2.142   0.0322 * 

# When compared to all other forms of education the VETSTAT actually results in an additional 4644.45 in income 
# This is different than when it is the only variable tested above. A follow-up question would be why is this occurring?
# These results depict our projected outcome where veterans do infact make more than the avearge person in the subset choosen 
# They also show how any other form of education (educ_hs, educ_somecoll, educ_college, educ_advdeg) all net more income 


#Testing other variables: 
model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp1)
plot(model_temp1)

model_temp2 <- lm(INCWAGE ~ EDUC)
summary(model_temp2)
plot(model_temp2)

#Adding in female increases some wages decreases other wages across EDUC
model_temp3 <- lm(INCWAGE ~ EDUC + female)
summary(model_temp2)
plot(model_temp2)

model_temp4 <- lm(INCWAGE ~ DEGFIELD)
summary(model_temp3)
plot(model_temp3)



# maybe get fancy
require(stargazer)
stargazer(model_temp3, type = "text")
# play with stargazer instead of summary, it can look nicer!


require(AER)


#plot of regression line with data points


NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)
# ^^ that looks like crap since Wages are soooooooo skew!  So try to find some sensible ylim = c(0, ??)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
# discus what you see in this plot

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

#Try both lm(INCWAGE ~ … and lm(log(INCWAGE) ~


#finally, always, always, ALWAYS !! remember this part

detach()

#Try some more linear regressions. Explain if there is a plausible causal link from X variables to Y and not the reverse. 
#Explain what additional restrictions to put on the dataset (eg just prime age, just females, just college degree, whatever).

#Explain your results, giving details about the estimation and providing any relevant graphics. What are the changes from 
#what you’d previously found (with k-nn or averages) and why might this be so? How do changes in specification (e.g. logs) 
#change the estimated coefficients? What are some relevant predicted values? Do those seem sensible? What additional information 
#would be useful? Impress me.

