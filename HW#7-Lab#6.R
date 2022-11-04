Household_Pulse_data_w48 = load("/Users/johnrobison/Desktop/CUNY MA Economics/Econometrics/Household_Pulse_data_w48.RData")
load("/Users/johnrobison/Desktop/CUNY MA Economics/Econometrics/acs2017_ny_data.RData")

data1 = Household_Pulse_data

model_logit1 <- glm(vaxx ~ EEDUC,
                    family = binomial, data = Household_Pulse_data)

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
Household_Pulse_data_w48 <- loadRData("/Users/johnrobison/Desktop/CUNY MA Economics/Econometrics/Household_Pulse_data_w48.RData")

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 



table(Household_Pulse_data$vaxx, Household_Pulse_data$EEDUC)


pick_use1 <- (Household_Pulse_data$TBIRTH_YEAR < 2000) 
dat_use1 <- subset(Household_Pulse_data, pick_use1)

# and to be finicky, might want to use this for factors after subsetting in case some get lost
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 



model_logit1 <- glm(vaxx ~ TBIRTH_YEAR + EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)

model_logit_REGION <- glm(vaxx ~ REGION,
                    family = binomial, data = dat_use1)

summary(model_logit_REGION)

# Here we compared vaccination by region and found that people in the south, midwest and west regions all had 
# a less likelihood of being vaccinated than those in the East. Midwest was slightly less than the south and the west was 
# more likely than both the south and midwest. Estimates RELATIVE TO EAST: MIDWEST = -.77958, SOUTH = -77958, WEST = -.54108

model_logit_REGION_TWDAYS <- glm(vaxx ~ REGION + TWDAYS,
                          family = binomial, data = dat_use1)

summary(model_logit_REGION_TWDAYS)

model_logit_REGION_TWDAYS_EEDUC <- glm(vaxx ~ REGION + TWDAYS + EEDUC,
                                 family = binomial, data = dat_use1)

summary(model_logit_REGION_TWDAYS_EEDUC)

# I then proceeded to add days spent teleworking and education level to the model and found that the south actual overtook
# the midwest as being the most unlikely region for a person to be vaccinated in. Also that people having not teleworked at 
# all are the least likely to be vaccinated relative to people having done some amount of teleworking people teleworking 3-4 days 
# were the most likely vaccinated group Which could make sense because that means they are working in person at least one day of the week
# this could mean there is a greater need for this group to be vaccinated compared to the group that teleworked for the whole week
# Lastly, Education was added to the mix and as expected the more education someone has the more likely they are to be vaccinated 
# these factors are what I will use in the prediction below 

YoungPerson_South_NoTelework_SomeHS <- data.frame(TBIRTH_YEAR = 2000,
                                       REGION = factor("South", levels = levels(dat_use1$REGION)),
                                       TWDAYS = factor("had no telework days in past week", levels = levels(dat_use1$TWDAYS)),
                                       EEDUC = factor("some hs", levels = levels(dat_use1$EEDUC))
                                      
)

OldPerson_Northeast_Telework3to4days_AdvDeg <- data.frame(TBIRTH_YEAR = 1950,
                                                  REGION = factor("Northeast", levels = levels(dat_use1$REGION)),
                                                  TWDAYS = factor("had 3-4 telework days in past week", levels = levels(dat_use1$TWDAYS)),
                                                  EEDUC = factor("adv deg", levels = levels(dat_use1$EEDUC))
                                                  
)

predict(model_logit_REGION_TWDAYS_EEDUC,South_NoTelework_SomeHS)
predict(model_logit_REGION_TWDAYS_EEDUC,South_NoTelework_SomeHS,type = "response")
# Prediction Value = .7634
predict(model_logit_REGION_TWDAYS_EEDUC,OldPerson_Northeast_Telework3to4days_AdvDeg)
predict(model_logit_REGION_TWDAYS_EEDUC,OldPerson_Northeast_Telework3to4days_AdvDeg,type = "response")
# Prediction Value = 4.030065

# The goal here was to find the prediction value at each extreme. Based on the estimates I found previously it was clear someone 
# from the south who did not telework and had minimal post HS education was the most likely not to be vaccinated this combination resulted in a 
# low prediction value wheras the combination of all the factors that resulted in vacination resulted in a high prediction value 
# By using type = "reponse" we can see the probability of the chosen factors resulting in a correct identification of vaccination 

#Example Prediction Provided:
new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1990,
                                       EEDUC = factor("bach deg", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("never",levels = levels(dat_use1$MS)),
                                       RRACE = factor("Black",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("male", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted,type = "response")



