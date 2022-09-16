#R Basics for Lecture 1
#Econ B2000, Statistics and Introduction to Econometrics
#John Robison 
#9/15/2022
#Group Members: none

ls()
rm(list = ls())

setwd("/Users/Owner/Desktop/Econometrics/ecob2000_lecture1")
load("Household_Pulse_data.RData")
attach("Household_Pulse_data.RData")

df <- "Household_Pulse_data.RData"
attach(df)

summary(Household_Pulse_data$EEDUC)
summary(Household_Pulse_data$EEDUC[Household_Pulse_data$RECVDVACC == 'yes got vaxx'])


# I was curious about looking at education, region and vaccination status 
# I was expecting that the Northeast and west would have a higher proportion of educated and vaccinated people
# This was true in that the Northeast had significantly higher % vaccinated and holding advanced degrees 
# but the south was actually the second most followed by the west and then Midwest last 

#Northeast = .283
#South = .246
#West = .233
#Midwest = .21807

restrict1 <- (Household_Pulse_data$EEDUC == "adv deg") & (Household_Pulse_data$RECVDVACC == "yes got vaxx")
data_new <- subset(Household_Pulse_data,restrict1)

summary(data_new$REGION)/summary(Household_Pulse_data$REGION)


# Now I am interested in checking the difference when the household income is greater than 200,000

restrict0 <- (Household_Pulse_data$EEDUC == "adv deg") & (Household_Pulse_data$INCOME == "HH income $200k +")
restrict2 <- (Household_Pulse_data$EEDUC == "adv deg") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") & (Household_Pulse_data$INCOME == "HH income $200k +")
restrict3 <- (Household_Pulse_data$EEDUC == "adv deg") & (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$INCOME == "HH income $200k +")
data_new0 <- subset(Household_Pulse_data,restrict0)
data_new2 <- subset(Household_Pulse_data,restrict2)
data_new3 <- subset(Household_Pulse_data,restrict3)

#Whole Population (Advanced Degree & Income > 200K)
summary(data_new0$REGION)

#Population proportion of the original sample  
summary(data_new0$REGION)/summary(Household_Pulse_data$REGION)

summary(data_new2$REGION)/summary(Household_Pulse_data$REGION)


