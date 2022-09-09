#Homework 2
#John Robison 
#Course: Econometrics 
#Group members: Suguru Iwashiro and Safinaz Ali
#Date:September 9th, 2022


setwd("/Users/johnrobison/Desktop/CUNY MA Economics/Econometrics")


#Possible Protocol 1 (PP1): roll once; if get 6 then conclude the dice is not fair; 
#if roll any other number then conclude it is fair. Analyze PP1: if the dice were fair, 
#what is the probability it would be judged to be unfair? Oppositely, if the dice were unfair, 
#what is the probability that it would be judged to be fair?
  
#On a fair dice there is an equal chance of rolling each number

#We rolled our dice and received a 6 so we concluded our dice was fair. The probability of 
#having a fair dice judged fair is 1 in 6 the probability of having an unfair fair dice judged 
#unfair is 5 in 6 

#PP2: roll the dice 20 times. (Each person should have done this beforehand.) Group can 
#specify a decision rule to judge that dice is fair or unfair. Consider the stats question: 
#if fair dice are rolled 20 times, what is likely number of 6 resulting? How unusual is it, 
#to get 1 more or less than that? How unusual is it, to get 2 more or less? 3? 
#Analyze PP2 including the question: if the dice were fair, what is the chance it could be 
#judged as unfair?

#my group rolled the dice 20 times and received two 6 rolls. 

#We had the idea that you could create a rule based on a fair die's probability of landing  
#any given number (16.67%)

#We tried to create an experiment to say that the mean probability of receiving any number is 16.67
#µ = 16.67e
#our idea was that you could make a rule that allows you to determine if a dice is fair by determining 
#a standard error 

#this can be done using a t test, What we decided was if the null hypothesis of µ = 16.67 (A fair die is judged fair)
#alternatively a not fair dice is judged fair would occur 83.33% of the time 
#then if in your sample if you decide to accept the null 
# When the dice is rolled 20 times the expected amount of 6s rolled would be .1667 * 20 = 3.33
# 3.33 = np
#the rest can be determined in R using a t test 

how_many_rolls <- 20
sim_rolls <- sample(1:6, how_many_rolls, replace = TRUE)
if_come_up_6 <- as.numeric(lots_of_sim_rolls == 6)

mean(if_come_up_6)
sd(sim_rolls,na.rm = TRUE)
np <- how_many_rolls*0.167 #1/6 = 0.167 
(sum(if_come_up_6)-np)/sqrt(np*(1-0.167))
t.test(sim_rolls,var.equal = TRUE) 

#Based on the standard error I rule that the the null hypothesis stand correct because we received a 6
# 10% of the time which does not fall in the acceptable standard error 
  
#PP3: roll 100 times and specify decision rules. Some cases are easy: if every single roll 
#comes to 6 then might quickly conclude. But what about the edge cases? Is it fair to say that 
#every conclusion has some level of confidence attached? Where do you set boundaries for decisions? 
#Analyze PP3. What is the chance that fair dice could be judged to be unfair?

#by running the same expierement as above and setting the number of rolls to 100 you can gain confidence in
# your determination 
how_many_rolls <- 100

# Our rule is applicable for any ample size but the larger your sample size the greater the confidence you can have in you abilty 
# to judge if the die is fair or not.


