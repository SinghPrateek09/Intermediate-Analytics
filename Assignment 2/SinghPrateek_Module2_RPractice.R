# ALY6015 Module 2 R Practice: Singh Prateek ------------------------------------------------
#----------------- Author: Prateek Singh
#----------------- Submission Date: 25th Jan, 2022
#----------------- Tutor: Jiyoung Yun


# Step: Importing Libraries ------------------------------------------------

library(psych)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tibble)



# Question 1: ------------------------------------------------

# A medical researcher wishes to see if hospital patients in a large hospital have the same blood type distribution as those in the general population.
# The distribution for the general population is as follows: type A, 20%; type B, 28%; type O, 36%; and type AB = 16%.
# He selects a random sample of 50 patients and finds the following: 12 have type A blood, 8 have type B, 24 have type O, and 6 have type AB blood. 
# At α = 0.10, can it be concluded that the distribution is the same as that of the general population?

# Blood Type                Expected | Observed
# Type A                      20%    |  12
# Type B                      28%    |  8
# Type O                      36%    |  24
# Type AB                     16%    |  6

# Stating the Hypothesis:
# H0: Type A = 0.20, Type B = 0.28, Type O = 0.36, Type AB = 0.16
# H1: The distribution is not the same as stated in Null Hypothesis

# df = (number of rows - 1) * (number of columns -1)
# Critical Value = qchisq(alpha, df, lower.tail = F)

cvalue_BloodType <- qchisq(0.10, 3, lower.tail = F)
cvalue_BloodType

alpha_1 <- 0.10

observed_1 <- c(12, 8, 24, 6)

exp_prob_1 <- c(0.20, 0.28, 0.36, 0.16)

ChisqTest_Result_BloodType <- chisq.test(x = observed_1, p = exp_prob_1)

ChisqTest_Result_BloodType$statistic
ChisqTest_Result_BloodType$p.value
ChisqTest_Result_BloodType$parameter

ChisqTest_Result_BloodType


ifelse(ChisqTest_Result_BloodType$p.value > alpha_1, "Fail to reject null hypothesis", "Reject the null hypothesis")

# Fail to reject null Hypothesis: The distribution of population is correct and same as stated in the Null Hypothesis as p-value comes out to be greater than alpha.




# Question 2: ------------------------------------------------

# According to the Bureau of Transportation Statistics, on-time performance by the airlines is described as follows:
# Action                                                |	% of Time
# On time                                               |	70.8
# National Aviation System delay                        |	8.2
# Aircraft arriving late                                |	9.0
# Other (because of weather and other conditions)       |	12.0

# Records of 200 randomly selected flights for a major airline company showed that 125 planes were on time; 
# 40 were delayed because of weather, 10 because of a National Aviation System delay, and the rest because of arriving late.
# At α = 0.05, do these results differ from the government’s statistics?

# Stating the Hypothesis:
# H0: The results from the government statistic is correct and valid
# H1: The results differ from the government statistic

# df = (number of rows - 1) * (number of columns -1)
# Critical Value = qchisq(alpha, df, lower.tail = F)

cvalue_AirlinePerformace <- qchisq(0.05, 3, lower.tail = F)
cvalue_AirlinePerformace

alpha_2 <- 0.05

observed_2 <- c(125, 40, 10, 25)

exp_prob_2 <- c(0.708, 0.12, 0.082, 0.09)

ChisqTest_Result_AirlinePerformace <- chisq.test(x = observed_2, p = exp_prob_2)

ChisqTest_Result_AirlinePerformace$statistic
ChisqTest_Result_AirlinePerformace$p.value
ChisqTest_Result_AirlinePerformace$parameter

ChisqTest_Result_AirlinePerformace


ifelse(ChisqTest_Result_AirlinePerformace$p.value > alpha_2, "Fail to reject null hypothesis", "Reject the null hypothesis")

# Reject the null Hypothesis: The results differ from the government statistic as p-value comes out to be lesser than alpha.




# Question 3: ------------------------------------------------

# Are movie admissions related to ethnicity? A 2014 study indicated the following numbers of admissions (in thousands) for two different years. 
# At the 0.05 level of significance, can it be concluded that movie attendance by year was dependent upon ethnicity?
  
#         Caucasian	|  Hispanic	  |   African American	  |    Other
# 2013	      724	  |     335	    |         174	          |     107
# 2014	      370	  |     292	    |         152	          |     140

# Stating the Hypothesis:
# H0: The movie attendance by year is independent of ethnicity
# H1: The movie attendance by year is dependent of ethnicity

# df = (number of rows - 1) * (number of columns -1)
# Critical Value = qchisq(alpha, df, lower.tail = F)

cvalue_EthnicityAttendance <- qchisq(0.05, 3, lower.tail = F)
cvalue_EthnicityAttendance

alpha_3 <- 0.05

Yr2013 <- c(724, 335, 174, 107)
Yr2014 <- c(370, 292, 152, 140)

MovieAttendence_Matrix <- matrix(c(Yr2013, Yr2014), nrow = 2, byrow = TRUE,
                 dimnames = list(c("2013", "2014"),  c("Caucasian", "Hispanic", "African American", "Other")))

ChisqTest_Result_EthnicityAttendance <- chisq.test(MovieAttendence_Matrix)

ChisqTest_Result_EthnicityAttendance$statistic
ChisqTest_Result_EthnicityAttendance$p.value
ChisqTest_Result_EthnicityAttendance$parameter

ChisqTest_Result_EthnicityAttendance


ifelse(ChisqTest_Result_EthnicityAttendance$p.value > alpha_3, "Fail to reject null hypothesis", "Reject the null hypothesis")

# Reject the null Hypothesis: The movie attendance by year is dependent of ethnicity as p-value comes out to be lesser than alpha.




# Question 4: ------------------------------------------------

# This table lists the numbers of officers and enlisted personnel for women in the military. 
# At α = 0.05, is there sufficient evidence to conclude that a relationship exists between rank and branch of the Armed Forces? 

# Action                Officers	|  Enlisted
# Army	                 10,791	  |   62,491
# Navy	                 7,816	  |   42,750
# Marine Corps           932      |   9,525
# Air Force              11,819   |   54,344

# Stating the Hypothesis:
# H0: The relationship exists between rank and branch of the Armed Forces
# H1: The relationship doesn't exist between rank and branch of the Armed Forces

# df = (number of rows - 1) * (number of columns -1)
# Critical Value = qchisq(alpha, df, lower.tail = F)

cvalue_ArmedForces <- qchisq(0.05, 3, lower.tail = F)
cvalue_ArmedForces

alpha_4 <- 0.05

Army <- c(10791, 62491)
Navy <- c(7816, 42750)
MarineCorps <- c(932, 9525)
AirForce <- c(11819, 54344)

ArmedForces_Matrix <- matrix(c(Army, Navy, MarineCorps, AirForce), nrow = 4, byrow = TRUE,
                                 dimnames = list(c("Army", "Navy", "Marine Corps", "Air Force"),  c("Officers", "Enlisted")))

ChisqTest_Result_ArmedForces <- chisq.test(ArmedForces_Matrix)

ChisqTest_Result_ArmedForces$statistic
ChisqTest_Result_ArmedForces$p.value
ChisqTest_Result_ArmedForces$parameter

ChisqTest_Result_ArmedForces


ifelse(ChisqTest_Result_ArmedForces$p.value > alpha_4, "Fail to reject null hypothesis", "Reject the null hypothesis")

# Reject the null Hypothesis: The relationship doesn't exist between rank and branch of the Armed Forces as p-value comes out to be lesser than alpha.




# One-Way ANOVA

# Question 5: ------------------------------------------------

# The amount of sodium (in milligrams) in one serving for a random sample of three different kinds of foods is listed. 
# At the 0.05 level of significance, is there sufficient evidence to conclude that a difference in mean sodium amounts exists among condiments, cereals, and desserts? 

# Condiments  |   Cereals	|  Desserts
# 270         |     260	  |   100
# 130         |     220	  |   180
# 230         |     290	  |   250
# 180         |     290	  |   250
# 80          |     200	  |   300
# 70          |     320	  |   360
# 200         |     140	  |   300
#             |       	  |   160

# Stating the Hypothesis:
# H0: µ1 = µ2 = µ3
# H1: At least one mean out of three is different

alpha_5 <- 0.05

Condiments <- data.frame('sodium' = c(270, 130, 230, 180, 80, 70, 200), 'food' = rep('Condiments', 7), stringsAsFactors = FALSE)

Cereals <- data.frame('sodium' = c(260, 220, 290, 290, 200, 320, 140), 'food' = rep('Cereals', 7), stringsAsFactors = FALSE)

Desserts <- data.frame('sodium' = c(100, 180, 250, 250, 300, 360, 300, 160), 'food' = rep('Desserts', 8), stringsAsFactors = FALSE)

Sodium <- rbind(Condiments, Cereals, Desserts)

Sodium$food <- as.factor(Sodium$food)

Anova_Food <- aov(sodium ~ food, data = Sodium)

summary(Anova_Food)

Anova_Food_Summary <- summary(Anova_Food)                                       # Save summary to an object

# Degrees of freedom

# K-1: between group variance: Numerator
df.numerator <- Anova_Food_Summary[[1]][1, "Df"]
df.numerator

# N-K: within group variance: Denominator
df.denominator <- Anova_Food_Summary[[1]][2, "Df"]
df.denominator

F.value <- Anova_Food_Summary[[1]][1, "F value"]                                # Extract F test value
F.value

p.value <- Anova_Food_Summary[[1]][1, "Pr(>F)"]                                 # Extract p-value
p.value

ifelse(p.value > alpha_5, "Fail to reject null hypothesis", "Reject the null hypothesis")

# Fail to reject the null Hypothesis: The mean of Sodium amount in listed food types(Condiments, Cereals & Desserts) is equal as p-value comes out to be greater than alpha.

# However, if we were to reject Null Hypothesis if the sodium mean for even one food type was different, we will run TukeyHSD() function to check the difference in mean

TukeyHSD(Anova_Food)




# Question 6: ------------------------------------------------

# The sales in millions of dollars for a year of a sample of leading companies are shown. At α = 0.01, is there a significant difference in the means?

#   Cereal	|   Chocolate |   Candy	Coffee
#     578   |   	311     |	      261
#     320   |   	106     |	      185
#     264   |   	109     |	      302
#     249   |   	125     |	      689
#     237   |   	173	

# Stating the Hypothesis:
# H0: µ1 = µ2 = µ3
# H1: At least one mean out of three is different

alpha_6 <- 0.01

Cereal <- data.frame('Sales' = c(578, 320, 264, 249, 237), 'companies' = rep('Cereal', 5), stringsAsFactors = FALSE)

Chocolate <- data.frame('Sales' = c(311, 106, 109, 125, 173), 'companies' = rep('Chocolate', 5), stringsAsFactors = FALSE)

Candy_Coffee <- data.frame('Sales' = c(261, 185, 302, 689), 'companies' = rep('Candy_Coffee', 4), stringsAsFactors = FALSE)

Sales <- rbind(Cereal, Chocolate, Candy_Coffee)

Sales$companies <- as.factor(Sales$companies)

Anova_Companies <- aov(Sales ~ companies, data = Sales)

summary(Anova_Companies)

Anova_Companies_Summary <- summary(Anova_Companies)                             # Save summary to an object

# Degrees of freedom

# K-1: between group variance: Numerator
df.numerator_Sales <- Anova_Companies_Summary[[1]][1, "Df"]
df.numerator_Sales

# N-K: within group variance: Denominator
df.denominator_Sales <- Anova_Companies_Summary[[1]][2, "Df"]
df.denominator_Sales

F.value_Sales <- Anova_Companies_Summary[[1]][1, "F value"]                     # Extract F test value
F.value_Sales

p.value_Sales <- Anova_Companies_Summary[[1]][1, "Pr(>F)"]                      # Extract p-value
p.value_Sales

ifelse(p.value_Sales > alpha_6, "Fail to reject null hypothesis", "Reject the null hypothesis")

# Fail to reject the null Hypothesis: The sales mean in millions of dollars for a year of a sample of leading companies are same as p-value comes out to be greater than alpha.

# However, if we were to reject Null Hypothesis if the Sales mean for even one company sample was different, we will run TukeyHSD() function to check the difference in mean

TukeyHSD(Anova_Companies)




# Question 7: ------------------------------------------------

# The expenditures (in dollars) per pupil for states in three sections of the country are listed. 
# Using α = 0.05, can you conclude that there is a difference in means?

# Eastern third	|   Middle third |   Western third
#     4946      |   	 6149      |	      5282
#     5953      |   	 7451      |	      8605
#     6202      |   	 6000      |	      6528
#     7243      |   	 6479      |	      6911
#     6113      |   	

# Stating the Hypothesis:
# H0: µ1 = µ2 = µ3
# H1: At least one mean out of three is different

alpha_7 <- 0.05

Eastern_Third <- data.frame('Expenditure' = c(4946, 5953, 6202, 7243, 6113), 'Sections' = rep('Eastern Third', 5), stringsAsFactors = FALSE)

Middle_Third <- data.frame('Expenditure' = c(6149, 7451, 6000, 6479), 'Sections' = rep('Middle Third', 4), stringsAsFactors = FALSE)

Western_Third <- data.frame('Expenditure' = c(5282, 8605, 6528, 6911), 'Sections' = rep('Western Third', 4), stringsAsFactors = FALSE)

Expenditure <- rbind(Eastern_Third, Middle_Third, Western_Third)

Expenditure$Sections <- as.factor(Expenditure$Sections)

Anova_Sections <- aov(Expenditure ~ Sections, data = Expenditure)

summary(Anova_Sections)

Anova_Sections_Summary <- summary(Anova_Sections)                               # Save summary to an object

# Degrees of freedom

# K-1: between group variance: Numerator
df.numerator_Exp <- Anova_Sections_Summary[[1]][1, "Df"]
df.numerator_Exp

# N-K: within group variance: Denominator
df.denominator_Exp <- Anova_Sections_Summary[[1]][2, "Df"]
df.denominator_Exp

F.value_Exp <- Anova_Sections_Summary[[1]][1, "F value"]                        # Extract F test value
F.value_Exp

p.value_Exp <- Anova_Sections_Summary[[1]][1, "Pr(>F)"]                         # Extract p-value
p.value_Exp

ifelse(p.value_Exp > alpha_7, "Fail to reject null hypothesis", "Reject the null hypothesis")

# Fail to reject the null Hypothesis: The expenditure mean in dollars per pupil for states in three sections of the country are same as p-value comes out to be greater than alpha.

# However, if we were to reject Null Hypothesis if the expenditure mean for even one section of the country was different, we will run TukeyHSD() function to check the difference in mean

TukeyHSD(Anova_Sections)




# Question 8: ------------------------------------------------

# A gardening company is testing new ways to improve plant growth. 
# Twelve plants are randomly selected and exposed to a combination of two factors, a “Grow-light” in two different strengths and a plant food supplement with different mineral supplements. 
# After a number of days, the plants are measured for growth, and the results (in inches) are put into the appropriate boxes.

#             	|   Grow-light 1 |   Grow-light 2
# Plant Food A  |  9.2, 9.4, 8.9 |	8.5, 9.2, 8.9
# Plant Food B  |  7.1, 7.2, 8.5 |	5.5, 5.8, 7.6

# Can an interaction between the two factors be concluded? Is there a difference in mean growth with respect to light? With respect to plant food? 
# Use α = 0.05.

# Stating the Hypothesis:
# H1: The mean of observations grouped by Grow-Light are same
# H2: The mean of observations grouped by Plant Food are same
# H3: There is no interaction between the Grow-Light and Plant Food

alpha_8 <- 0.05

Plant_Measure = data.frame(PlantFood = rep(c("A", "B"), each=6), 
                           GrowLight=rep(c("1", "2"), each=3, times=2), 
                           Growth=c(9.2,9.4,8.9,8.5,9.2,8.9,7.1,7.2,8.5,5.5,5.8,7.6))

Plant_Measure

TwoWayAnova_PlantGrowth <- aov(Growth ~ GrowLight * factor(PlantFood), data = Plant_Measure)

anova(TwoWayAnova_PlantGrowth)

plot(Growth ~ GrowLight + factor(PlantFood), data = Plant_Measure, xlab = "Plant Food")


TukeyHSD(TwoWayAnova_PlantGrowth)




# Question 9: ------------------------------------------------

Baseball <- read.csv('baseball.csv')

describe(Baseball)

Baseball <- as.data.frame(unclass(Baseball), stringsAsFactors = TRUE)

ggplot(Baseball, aes(x = reorder(Team, RS, FUN = median), y = RS, fill = Team)) + 
  geom_boxplot(notch = FALSE, alpha = 0.9) + 
  scale_y_continuous(name = "Run Scored", breaks = c(0, Baseball$RS), guide = guide_axis(check.overlap = TRUE)) +
  coord_flip() + 
  stat_boxplot(geom = 'errorbar', width = 0.5) +
  scale_x_discrete(name = "Team") +
  labs(title = "Teams Runs Scored") +
  theme(plot.title = element_text(hjust = 0.5, color = "red", face = "bold"), 
        axis.title.x = element_text(color = "Orange"), 
        axis.title.y = element_text(color = "orange"),
        axis.text.x = element_text(color = "cyan4", angle = 90),
        axis.text.y = element_text(color = "cyan4"),
        axis.line = element_line(color = "cyan4")
  )

wins <- Baseball %>% 
  group_by(Team) %>% 
  summarize(wins = sum(W)) %>% 
  as_tibble() 

hist(wins$wins, main = "Distribution of Team's Wins", xlab = "Number of Wins Per Team", col = "Pink")

Baseball$Decade <- Baseball$Year - (Baseball$Year %% 10) 

Decade_Win <- Baseball %>% 
  group_by(Decade) %>% 
  summarize(wins = sum(W)) %>% 
  as_tibble() 

Decade_Win

hist(Decade_Win$wins, main = "Distribution of Wins per Decade", xlab = "Number of Wins Per Decade", xaxt = "n") 
axis(1, at = seq(5000, 25000, by = 2000), labels = seq(5000, 25000, by = 2000))

mean(Decade_Win$wins)
median(Decade_Win$wins)

# Assuming the expected frequencies are equal for wins by decade
# Stating the Hypothesis:
# H0: There is no difference in the number of wins by decade
# H1: There is a difference in the number of wins by decade

# df = (number of rows - 1) * (number of columns -1)
# Critical Value = qchisq(alpha, df, lower.tail = F)

cvalue_WinsByDecade <- qchisq(0.05, 5, lower.tail = F)
cvalue_WinsByDecade

alpha_9 <- 0.05

ChisqTest_Result_WinsByDecade <- chisq.test (Decade_Win)

ChisqTest_Result_WinsByDecade$statistic
ChisqTest_Result_WinsByDecade$p.value
ChisqTest_Result_WinsByDecade$parameter

ChisqTest_Result_WinsByDecade


ifelse(ChisqTest_Result_WinsByDecade$p.value > alpha_9, "Fail to reject null hypothesis", "Reject the null hypothesis")

# Reject null Hypothesis: There is a difference in the number of wins by decade as p-value comes out to be greater than alpha.