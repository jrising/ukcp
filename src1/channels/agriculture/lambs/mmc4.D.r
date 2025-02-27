#Parameter estimation script

#Author: Hannah Rose
#Contact: hannah.rose@bris.ac.uk
#Software: R - http://www.r-project.org/index.html
#Required packages: N/A

#Abbreviations:
##D50 = time to 50% development in days
##L50 = time to 50% mortality in days
##temp = temperature in degrees centigrade

#Notes:
#Data were transformed as required to fit linear models
#See main text for full methodology and references


###################
#DEVELOPMENT RATES#
###################

#Haemonchus contortus#
######################

#Data source: Hsu and Levine 1976, Rose 1963
dat = data.frame(D50 = c(5.75, 3.1, 3, 2.7, 4, 10, 14,22.5), temp = c(20,25,30,35,25.5,20.5,15.5,10.5))

#Calculate the instantaneous rate
instantaneous.dev = -log(0.5)/dat$D50

#Fit a linear model
mod = lm(instantaneous.dev~dat$temp)
summary(mod)

#Find minimum temperature threshold for development
dev.threshold = -(mod$coeff[1])/mod$coeff[2]

#Teladorsagia circumcincta#
###########################

#Data source: Pandey et al. 1989
dat16 = data.frame(percentageL3yield = c(1,17,26,53,55,60,67,68), days = c(6,7,8,9,10,11,12,13))
#L3 are then expressed as a proportion of max L3 yield for analysis to preclude mortality
dat16$correctedL3 = dat16$percentageL3yield/max(dat16$percentageL3yield)
#Repeat for 25 degrees. Too few data were available for other temperatures.
dat25 = data.frame(percentageL3yield = c(3,21,27,32,33,41), days = c(4,5,6,7,8,9))
dat25$correctedL3 = dat25$percentageL3yield/max(dat25$percentageL3yield)

#Transform as described by Azam et al. 2012 and remove infinite values (resulting from transformation of proportions of 0 or 1)
t16 = log10(dat16$correctedL3/(1-dat16$correctedL3))
t16[which(t16==-Inf)] = NA
t16[which(t16==Inf)] = NA
t25 = log10(dat25$correctedL3/(1-dat25$correctedL3))
t25[which(t25==-Inf)] = NA
t25[which(t25==Inf)] = NA

#Fit linear models to the proportions of L3 over time. Find D50 (where the fitted line intercepts y = 0)
mod = lm(t16~dat16$days)
summary(mod)
D50_16 = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t25~log(dat25$days))
summary(mod)
D50_25 = exp((0 - mod$coef[1]) / mod$coef[2])

#Data source: Salih and Grainger 1982
#Note: Salih and Grainger report mean duration of egg and larval stages. These are combined to give total time to L3
#Note: 35 degrees removed - it is assumed that the absence of development to L3 is due to mortality, not an upper development threshold
tempSG = c(10,15,20,25,30)
eggduration= c(4.016, 2.058, 1.495, 0.9916, 1)
larvalduration = c(19.06, 13.14, 10.41, 8.11, 4.89)
totalduration = eggduration+larvalduration

#Create data frame containing estimated D50 and temperatures for above two studies
#Include also - 
#Young et al. 1980a - eggs hatched at 3 degrees in saline
#Crofton 1965 - minimum hatching temperature of 4 degrees
#Crofton and Whitlock 1965a, b
dat = data.frame(D50 = c(0, 0, 0, 0, D50_16, D50_25, totalduration), temp = c(3, 4, 4, 10, 16, 25, tempSG))

#Calculate the instantaneous rate
instantaneous.dev = -log(0.5)/dat$D50
instantaneous.dev[c(1:4, 7)]=0#replace Inf in 1:4 with 0 as 0 development rate at these temperatures

#Fit a linear model
mod = lm(instantaneous.dev~dat$temp)
summary(mod)

#Find minimum temperature threshold for development
dev.threshold = -(mod$coeff[1])/mod$coeff[2]

#Ostertagia ostertagi#
######################

#Data source: Pandey 1972a
#Notes: analysis is as for Pandey et al. 1989 above
dat35 = data.frame(proportionL3yield = c(0.2,0.35,0.37), days = c(3,4,5))
dat35$correctedL3 = dat35$proportionL3yield/max(dat35$proportionL3yield)

dat30 = data.frame(proportionL3yield = c(0.13,0.27,0.5,0.5), days = c(3,4,5,6))
dat30$correctedL3 = dat30$proportionL3yield/max(dat30$proportionL3yield)

dat25 = data.frame(proportionL3yield = c(0.05,0.15,0.25,0.49,0.67), days = c(3,4,5,6,7))
dat25$correctedL3 = dat25$proportionL3yield/max(dat25$proportionL3yield)

dat20 = data.frame(proportionL3yield = c(0,0.08,0.17,0.4,0.55,0.6), days = c(3,4,5,6,7,8))
dat20$correctedL3 = dat20$proportionL3yield/max(dat20$proportionL3yield)

dat15 = data.frame(proportionL3yield = c(0,0,0,0,0,0.06,0.19,0.35), days = c(3,4,5,6,7,8,9, 10))
dat15$correctedL3 = dat15$proportionL3yield/max(dat15$proportionL3yield)

dat10 = data.frame(proportionL3yield = c(0,0,0,0,0,0,0,0,0.05,0.1,0.15,0.27), days = c(3,4,5,6,7,8,9,10,19,20,21,22))
dat10$correctedL3 = dat10$proportionL3yield/max(dat10$proportionL3yield)

#Transform as described by Azam et al. 2012
t35 = log10(dat35$correctedL3/(1-dat35$correctedL3))
t35[which(t35==-Inf)] = NA
t35[which(t35==Inf)] = NA
t30 = log10(dat30$correctedL3/(1-dat30$correctedL3))
t30[which(t30==-Inf)] = NA
t30[which(t30==Inf)] = NA
t25 = log10(dat25$correctedL3/(1-dat25$correctedL3))
t25[which(t25==-Inf)] = NA
t25[which(t25==Inf)] = NA
t20 = log10(dat20$correctedL3/(1-dat20$correctedL3))
t20[which(t20==-Inf)] = NA
t20[which(t20==Inf)] = NA
t15 = log10(dat15$correctedL3/(1-dat15$correctedL3))
t15[which(t15==-Inf)] = NA
t15[which(t15==Inf)] = NA
t10 = log10(dat10$correctedL3/(1-dat10$correctedL3))
t10[which(t10==-Inf)] = NA
t10[which(t10==Inf)] = NA

#Too few data for 35 degrees, 30 degrees and 15 degrees
#Fit linear models to find D50
mod = lm(t25~dat25$days)
summary(mod)
D50_25 = ((0 - mod$coef[1]) / mod$coef[2])
mod = lm(t20~dat20$days)
summary(mod)
D50_20 = ((0 - mod$coef[1]) / mod$coef[2])
mod = lm(t10~dat10$days)
summary(mod)
D50_10 = ((0 - mod$coef[1]) / mod$coef[2])

#Create data frame with estimated D50
#Include data source: Young et al 1980b, Rose 1961
dat = data.frame(D50 = c(D50_25, D50_20, D50_10, (72/24), (130/24), (245/24), (510/24), mean(c(3,7)), mean(c(7,16)), mean(c(18,28))), temp = c(25, 20, 10, 27, 20, 15, 10, 22.5, 15, 10.5)) 

#Calculate the instantaneous rate
instantaneous.dev = -log(0.5)/dat$D50

#Fit a linear model
mod = lm(instantaneous.dev~dat$temp)
summary(mod)

#Find minimum temperature threshold for development
dev.threshold = -(mod$coeff[1])/mod$coeff[2]

######################################
#MORTALITY RATE - PREINFECTIVE STAGES#
######################################

#Haemonchus contortus#
######################

#Data source: Todd et al. 1976a
dat4 = data.frame(unembryonated.eggs = c(1,0.81,0.69,0.77,0.74,0.8,0.84,0.35,0.17,0,0,0,0,0,0), embryonated.eggs = c(1,0.89,0.66,0.6,0.62,0.6,0.54,0.55,0.56,0.47,0.31,0.29,0.06,0,0), L1 = c(0.62,0.77,0.67,0.86,1,0.66,0.88,0.94,0.98,0.99,0.41,0.06,0,0,0), L2 = c(0.91,1,0.97,0.89,1,0.96,1,0.99,0.86,0.06,0.04,0,0,0,0), days = c(0.02,0.04,0.08,0.17,0.33,0.5,1,2,4,8,16,32,64,128,256))
dat45 = data.frame(unembryonated.eggs = c(0.36,0.01,0,0,0,0,0,0,0,0,0,0,0,0,0), embryonated.eggs = c(1,0.92,0.95,0.01,0.04,0.04,0,0,0,0,0,0,0,0,0), L1 = c(1,0.95,0.9,0.92,0.11,0,0,0,0,0,0,0,0,0,0), L2 = c(0.65,0.07,0,0,0,0,0,0,0,0,0,0,0,0,0), days = c(0.02,0.04,0.08,0.17,0.33,0.5,1,2,4,8,16,32,64,128,256))

#Transform as described by Azam et al. 2012
t4ue = log10(dat4$unembryonated.eggs/(1-dat4$unembryonated.eggs))
t4ue[which(t4ue==-Inf)] = NA
t4ue[which(t4ue==Inf)] = NA
t4ee = log10(dat4$embryonated.eggs/(1-dat4$embryonated.eggs))
t4ee[which(t4ee==-Inf)] = NA
t4ee[which(t4ee==Inf)] = NA
t4L1 = log10(dat4$L1/(1-dat4$L1))
t4L1[which(t4L1==-Inf)] = NA
t4L1[which(t4L1==Inf)] = NA
t4L2 = log10(dat4$L2/(1-dat4$L2))
t4L2[which(t4L2==-Inf)] = NA
t4L2[which(t4L2==Inf)] = NA

t45ue = log10(dat45$unembryonated.eggs/(1-dat45$unembryonated.eggs))
t45ue[which(t45ue==-Inf)] = NA
t45ue[which(t45ue==Inf)] = NA
t45ee = log10(dat45$embryonated.eggs/(1-dat45$embryonated.eggs))
t45ee[which(t45ee==-Inf)] = NA
t45ee[which(t45ee==Inf)] = NA
t45L1 = log10(dat45$L1/(1-dat45$L1))
t45L1[which(t45L1==-Inf)] = NA
t45L1[which(t45L1==Inf)] = NA
t45L2 = log10(dat45$L2/(1-dat45$L2))
t45L2[which(t45L2==-Inf)] = NA
t45L2[which(t45L2==Inf)] = NA

#Fit linear models to find D50
mod = lm(t4ue~dat4$days)
summary(mod)
L50_4ue = ((0 - mod$coef[1]) / mod$coef[2])
mod = lm(t4ee~dat4$days)
summary(mod)
L50_4ee = ((0 - mod$coef[1]) / mod$coef[2])
mod = lm(t4L2~dat4$days)
summary(mod)
L50_4L2 = ((0 - mod$coef[1]) / mod$coef[2])

##Create data frame with estimated L50
#L1 incubated at 4 degrees - data not analysed due to significant variability and an increase in numbers of L1 between day 0 and 8
##All stages assigned an instantaneous mortality rate of 1 at 45 degrees and -10 degrees as there was 100% mortality within 24 hours 

#Eggs
dat = data.frame(L50 = c(L50_4ue, L50_4ee, 0, 0, 0, 0), temp = c(4, 4, 45, 45, -10, -10)) 
instantaneous.mort = -log(0.5)/dat$L50
instantaneous.mort[3:6] = 1

#Fit a linear model
#Quadratic model fitted as higher mortality at lowest and highest temperatures
#Log transform y variable to prevent negative mortality rates
x = dat$temp
x2 = x^2
mod = lm(log(instantaneous.mort)~x+x2)
summary(mod)

#Larvae
dat = data.frame(L50 = c(L50_4L2, 0, 0, 0, 0), temp = c(4, 45, 45, -10, -10)) 
instantaneous.mort = -log(0.5)/dat$L50
instantaneous.mort[2:5] = 1
x = dat$temp
x2 = x^2
mod = lm(log(instantaneous.mort)~x+x2)
summary(mod)

#Teladorsagia circumcincta#
###########################

#Data source: Pandey et al 1993
#Eggs surviving after incubation at 4 degrees
dat4 = data.frame(days = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22), eggs.surviving = c(0.95, 0.9, 0.85, 0.8, 0.65, 0.5, 0.36, 0.22, 0.21, 0.19, 0.18, 0.17, 0.16, 0.15, 0.14, 0.14, 0.13,0.13,0.12,0.12, 0.11, 0.11))

#Transform as described by Azam et al. 2012
t4 = log10(dat4$eggs.surviving/(1-dat4$eggs.surviving))
t4[which(t4==-Inf)] = NA
t4[which(t4==Inf)] = NA

#Fit a linear model to find L50
mod = lm(t4~log(dat4$days))
summary(mod)
L50_4 = exp((0 - mod$coef[1]) / mod$coef[2])

#Include data source: Pandey et al. 1989
#As the survival of L3 is generally a decreasing sigmoid function of time, I have assumed that very little L3 mortality is observed soon after development, and that any mortality is due to mortality of pre-infective stages
dat = data.frame(temp = c(4, 16, 25, 35), max.eggs = c(NA, 98, 100, 100), max.L3 = c(NA, 68, 41, 10))
dat$difference.eggs = dat$max.eggs-dat$max.L3
dat$difference.eggs[1] = 50
time.max.eggs = c(NA, 4, 4, 1)
time.max.L3 = c(NA, 13,9,4)
dat$days = time.max.L3-time.max.eggs
dat$days[1] = L50_4
instantaneous.mort = -log(1-(dat$difference.eggs/100))/dat$days

#fit linear model to the data from the two studies
x = dat$temp
x2 = x^2
mod = lm(log(instantaneous.mort)~x+x2)
summary(mod)

#Ostertagia ostertagi#
######################

#Data source: Pandey 1972
#Note: only observations on the mortality of eggs in faeces were used. Observations on the mortality of L1 in water not used as substrate may affect mortality rates
dat = data.frame(days = c(1, 2, 4, 7, 14, 28, (6*7), (8*7), (10*7), (12*7), (16*7), (20*7), (24*7), (28*7), (32*7), (36*7), (40*7), (44*7), (46*7), (50*7), (52*7)),
                 minus10 = c(1,1,0.74,0.58,0.36,0.20,0.03,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                 one = c(1,1,1,1,0.95,0.90,0.66,0.25,0.22,0.17,0.12,0.11,0.11,0.04,0.05,0.03,0.02,0.02,0.02,0,0),
                 four = c(1,1,1,1,1,1,1,0.9,0.6,0.48,0.28,0.16,0.12,0.12,0.08,0.1,0.06,0.03,0.03,0.01,0)),

#Transform as described by Azam et al. 2012
tm10 = log10(dat$minus10/(1-dat$minus10))
tm10[which(tm10==-Inf)] = NA
tm10[which(tm10==Inf)] = NA
t1 = log10(dat$one/(1-dat$one))
t1[which(t1==-Inf)] = NA
t1[which(t1==Inf)] = NA
t4 = log10(dat$four/(1-dat$four))
t4[which(t4==-Inf)] = NA
t4[which(t4==Inf)] = NA

#Fit linear models to find L50
mod = lm(tm10~dat$days)
summary(mod)
L50_m10 = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t1~log10(dat$days))
summary(mod)
L50_1 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t4~log10(dat$days))
summary(mod)
L50_4 = 10^((0 - mod$coef[1]) / mod$coef[2])

#Apply an instantaneous mortality rate of 1 to 40 degrees as >99% mortality within 24 hours
dat = data.frame(L50 = c(L50_m10, L50_1, L50_4, 0), temp = c(-10, 1, 4, 40)) 
instantaneous.mort = -log(0.5)/dat$L50
instantaneous.mort[4] = 1

#Fit quadratic regression to log transformed y variable
x = dat$temp
x2 = x^2
mod = lm(log(instantaneous.mort)~x+x2)
summary(mod)

###############################
#MORTALITY RATE - L3 IN FAECES#
###############################

#Haemonchus contortus#
######################

#Data source: Todd et al. 1976a
#100% mortality in less than 24 hours at -28 and -10 degrees. Apply mortality rate of 1
#Data source: Todd et al. 1976b
#100% mortality in less than 24 hours at -28 and -10 degrees. Apply mortality rate of 1

dat = data.frame(days = c(0.02,0.04,0.08,0.17,0.33,0.5,1,2,4,8,16,32,64,128,256),
                 four = c(1,1,1,1,0.78,0.71,0.6,0.75,0.74,0.6,0.66,0.49,0.63,0.47,0.2),
                 twenty = c(1,1,1,1,0.81,0.89,0.7,0.63,0.59,0.71,0.42,0.23,0.08,0.04,0),
                 twentyfive = c(1,0.91,0.79,0.83,0.69,0.62,0.6,0.67,0.6,0.5,0.54,0.39,0.07,0.02,0),
                 thirtyfive = c(1,1,0.98,1,0.89,0.76,0.61,0.6,0.62,0.5,0.58,0.39,0.07,0,0),
                 fourtyfive = c(0.8,0.71,0.7,0.65,0.72,0.16,0.34,0.07,0.01,NA,NA,NA,NA,NA,NA),
                 four_b = c(1,1,0.89,0.68,0.71,0.72,0.69,0.71,0.71,0.6,0.64,0.59,0.56,0.52,0.21),
                 twenty_b = c(1,0.99,0.89,0.8,0.84,0.78,0.77,0.72,0.69,0.59,0.64,0.48,0.24,0.07,0.01),
                 twentyfive_b = c(0.91,0.8,0.8,0.8,0.66,0.62,0.64,0.62,0.61,0.5,0.5,0.37,0.12,0.02,0),
                 thirtyfive_b = c(1,0.96,0.97,1,0.87,0.7,0.64,0.63,0.59,0.56,0.52,0.33,0.03,0,0),
                 fourtyfive_b = c(0.72,0.7,0.67,0.61,0.4,0.21,0.32,0.11,0.01,NA,NA,NA,NA,NA,NA))

#Transform as described by Azam et al. 2012
t4a = log10(dat$four/(1-dat$four))
t4a[which(t4a==-Inf)] = NA
t4a[which(t4a==Inf)] = NA
t20a = log10(dat$twenty/(1-dat$twenty))
t20a[which(t20a==-Inf)] = NA
t20a[which(t20a==Inf)] = NA
t25a = log10(dat$twentyfive/(1-dat$twentyfive))
t25a[which(t25a==-Inf)] = NA
t25a[which(t25a==Inf)] = NA
t35a = log10(dat$thirtyfive/(1-dat$thirtyfive))
t35a[which(t35a==-Inf)] = NA
t35a[which(t35a==Inf)] = NA
t45a = log10(dat$fourtyfive/(1-dat$fourtyfive))
t45a[which(t45a==-Inf)] = NA
t45a[which(t45a==Inf)] = NA

t4b = log10(dat$four_b/(1-dat$four_b))
t4b[which(t4b==-Inf)] = NA
t4b[which(t4b==Inf)] = NA
t20b = log10(dat$twenty_b/(1-dat$twenty_b))
t20b[which(t20b==-Inf)] = NA
t20b[which(t20b==Inf)] = NA
t25b = log10(dat$twentyfive_b/(1-dat$twentyfive_b))
t25b[which(t25b==-Inf)] = NA
t25b[which(t25b==Inf)] = NA
t35b = log10(dat$thirtyfive_b/(1-dat$thirtyfive_b))
t35b[which(t35b==-Inf)] = NA
t35b[which(t35b==Inf)] = NA
t45b = log10(dat$fourtyfive_b/(1-dat$fourtyfive_b))
t45b[which(t45b==-Inf)] = NA
t45b[which(t45b==Inf)] = NA          

#Fit linear models to find L50
mod = lm(t4a~dat$days)
summary(mod)
L50_4a = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t20a~log10(dat$days))
summary(mod)
L50_20a = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t25a~dat$days)
summary(mod)
L50_25a = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t35a~log10(dat$days))
summary(mod)
L50_35a = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t45a~dat$days)
summary(mod)
L50_45a = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t4b~dat$days)
summary(mod)
L50_4b = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t20b~log10(dat$days))
summary(mod)
L50_20b = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t25b~dat$days)
summary(mod)
L50_25b = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t35b~log10(dat$days))
summary(mod)
L50_35b = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t45b~dat$days)
summary(mod)
L50_45b = ((0 - mod$coef[1]) / mod$coef[2])

#Create a data frame and calculate instantaneous mortality rates
dat = data.frame(temp = c(4, 20, 25, 35, 45, 4, 20, 25, 35, 45, -10, -10), L50 = c(L50_4a, L50_20a, L50_25a, L50_35a, L50_45a, L50_4b, L50_20b, L50_25b, L50_35b, L50_45b, 0, 0))

instantaneous.mort = -log(0.5)/dat$L50
instantaneous.mort[c(5, 10:12)] = 1 #apply an instantaneous rate of 1 to -10 and any rates >1

#Fit linear model
x = dat$temp
x2 = x^2
mod = lm(log(instantaneous.mort)~x+x2)
summary(mod)

#Teladorsagia circumcincta & Ostertagia ostertagi#
##################################################

#No data - see main text and section on mortality of O. ostertagi in soil below

#############################
#MORTALITY RATE - L3 IN SOIL#
#############################

#Haemonchus contortus#
######################

#Data source@ Todd et al. 1976b (free control replicates only)
#-10 and 45 degrees have an instantaneous rate >1 as 100% mortality within 24 hours


dat = data.frame(four = c(0.99, 0.99, 1, 0.99, 0.96, 0.93, 0.75, 0.58, 0.42), 
                 twenty = c(0.99, 0.98, 0.99, 0.99, 0.98, 0.95, 0.93, 0.81, 0.36),
                 twentyfive = c(0.98,0.99,0.98,0.99,0.97,0.87,0.80,0.53,0.04),
                 thirtyfive = c(0.96,0.95,0.96,0.94,0.77,0.68,0.18,0.01,0),
                 fortyfive = c(0.04,0.07,0.04,0,0,0,0,0,0),
                 days = c(1,2,4,8,16,32,64,128,256))

#Transform as described by Azam et al. 2012
t4 = log10(dat$four/(1-dat$four))
t4[which(t4==-Inf)] = NA
t4[which(t4==Inf)] = NA
t20 = log10(dat$twenty/(1-dat$twenty))
t20[which(t20==-Inf)] = NA
t20[which(t20==Inf)] = NA
t25 = log10(dat$twentyfive/(1-dat$twentyfive))
t25[which(t25==-Inf)] = NA
t25[which(t25==Inf)] = NA
t35 = log10(dat$thirtyfive/(1-dat$thirtyfive))
t35[which(t35==-Inf)] = NA
t35[which(t35==Inf)] = NA
t45 = log10(dat$fourtyfive/(1-dat$fourtyfive))
t45[which(t45==-Inf)] = NA
t45[which(t45==Inf)] = NA

#As there is an extended period of no change in some of the above data, linear models are fitted to the period of decrease in numbers of L3
#Fit linear models to find L50
mod = lm(t4[3:9]~log10(dat$days[3:9]))
summary(mod)
L50_4 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t20[3:9]~log10(dat$days[3:9]))
summary(mod)
L50_20 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t25[3:9]~log10(dat$days[3:9]))
summary(mod)
L50_25 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t35[3:9]~dat$days[3:9])
summary(mod)
L50_35 = ((0 - mod$coef[1]) / mod$coef[2])

#Include data source: Jehan and Gupta 1974 and calculate instantaneous rates
dat = data.frame(temp = c(-10, 4, 20, 25, 35, 45, 25,30,34,37,40), L50 = c(0, L50_4, L50_20, L50_25, L50_35, 0, 60,55,28,12,7))

instantaneous.mort = -log(0.5)/dat$L50
instantaneous.mort[which(instantaneous.mort==Inf)] = 1 #apply an instantaneous rate of 1 to -10 and 45 degrees frm Todd et al. as 100% mortality within 24 hours

#Fit linear model
x = dat$temp
x2 = x^2

mod = lm(log(instantaneous.mort)~x+x2)
summary(mod)

#Teladorsagia circumcincta#
###########################

#Data source: Pandey et al. 1993
dat = data.frame(days = c(0,1*7,2*7,3*7,4*7,5*7,6*7,7*7,8*7,9*7,10*7,11*7,12*7,13*7,14*7,15*7,16*7),
                 minus10= c(1,0.889830509,0.86440678,0.813559322,0.728813559,0.762711864,0.737288136,0.703389831,0.516949153,0.322033898,0.127118644,0.059322034,0.059322034,0.059322034,0,0,0),
                 four= c(1,0.991596639,0.941176471,0.932773109,0.941176471,0.957983193,0.957983193,0.957983193,0.924369748,0.915966387,0.924369748,0.924369748,0.915966387,0.798319328,0.764705882,0.764705882,0.731092437),
                 sixteen= c(1,1,1,1,1,0.983193277,1,1,1,0.983193277,0.983193277,1,0.924369748,0.907563025,0.87394958,0.739495798,0.756302521),
                 twentyfive= c(1,1,0.983193277,0.974789916,0.924369748,0.924369748,0.722689076,0.68907563,0.579831933,0.596638656,0.579831933,0.546218487,0.546218487,0.361344538,0.134453782,0.176470588,0.226890756),
                 thirtyfive= c(1,0.907563025,0.907563025,0.890756303,0.890756303,0.789915966,0.12605042,0.025210084,0,0,0,0,0,0,0,0,0))

#Transform as described by Azam et al. 2012
tm10 = log10(dat$minus10/(1-dat$minus10))
tm10[which(tm10==-Inf)] = NA
tm10[which(tm10==Inf)] = NA
t16 = log10(dat$sixteen/(1-dat$sixteen))
t16[which(t16==-Inf)] = NA
t16[which(t16==Inf)] = NA
t4 = log10(dat$four/(1-dat$four))
t4[which(t4==-Inf)] = NA
t4[which(t4==Inf)] = NA
t25 = log10(dat$twentyfive/(1-dat$twentyfive))
t25[which(t25==-Inf)] = NA
t25[which(t25==Inf)] = NA
t35 = log10(dat$thirtyfive/(1-dat$thirtyfive))
t35[which(t35==-Inf)] = NA
t35[which(t35==Inf)] = NA

#Fit linear models to find L50
mod = lm(tm10~dat$days)
summary(mod)
L50_m10 = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t4[c(2, 6:17)]~dat$days[c(2, 6:17)]) #decrease in L3 on days 2-4 removed
summary(mod)
L50_4 = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t16[10:17]~dat$days[10:17]) #as before, multiple intervals with no change are removed
summary(mod)
L50_16 = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t25~log10(dat$days))
summary(mod)
L50_25 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t35[5:8]~dat$days[5:8])  #as before, multiple intervals with no change are removed
summary(mod)
L50_35 = ((0 - mod$coef[1]) / mod$coef[2])

#Data source: Gruner and Suryahadi 1993 - 20 degrees
dat = data.frame(days = c(30, 60, 90, 120, 150, 180, 210),L3 = c(0.92, 0.83, 0.77, 0.7, 0.61, 0.5, 0.4))

#Transform as described by Azam et al. 2012
t20 = log10(dat$L3/(1-dat$L3))

#Fit linear model to find L50
mod = lm(t20~dat$days)
summary(mod)
L50_20 = ((0-mod$coef[1])/mod$coef[2])

#Create data frame and include...
#Data source: Rossanigo and Gruner 1996 - 20 degrees
#Data source: Jasmer et al. 1987
#Append Pandey et al. 1993 and Gruner&Suryahadi papers
dat = data.frame(days = c(0.625,0.625,0.625, 195,191,205,192,180,261, L50_20, L50_m10, L50_4, L50_16, L50_25, L50_35), proportion.L3.surviving = c(0.85,0.75,0.8, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), temp = c(-10, -15, -18, 20, 20, 20, 20, 20, 20, 20, -10, 4, 16, 25, 35))

dat$instantaneous.mort = -log(dat$proportion.L3.surviving)/dat$days

#Fit linear model
x = dat$temp
x2 = x^2
mod = lm(log(dat$instantaneous.mort)~x+x2)
summary(mod)

#Ostertagia ostertagi#
######################

#Data source: Pandey 1972

dat = data.frame(days = c(0.020833333,0.041666667,0.083333333,0.166666667,0.333333333,0.5,1,2,4,7,14,28,42,56,70,98,126,154,182,210,238,266,294,322,364),
                 minus10 = c(NA,NA,NA,NA,NA,NA,NA,0.8,0.5,0.25,0.13,0.06,0.01,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                 one = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0.99,0.96,0.94,0.95,0.93,0.9,0.85,0.82,0.75),
                 four = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0.99,0.99,0.98,0.99,0.99,0.97,0.95,0.93,0.94,0.9),
                 twenty = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0.99,0.98,0.98,0.99,0.95,0.9,0.8,0.8,0.74,0.5,0.33,0.13,0.05,NA,NA),
                 twentyfive = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0.98,0.9,0.92,0.73,0.51,0.21,0.18,0.05,NA,NA,NA,NA),
                 thirty = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0.96,0.89,0.78,0.37,0.4,0.15,0.04,NA,NA,NA,NA,NA,NA,NA,NA),
                 thirtyfive = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,0.98,0.88,0.6,0.2,0.1,0.02,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                 fourty = c(0.97,0.94,0.83,0.85,0.38,0.1,0.05,0.03,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

#Transform as described by Azam et al. 2012
tm10 = log10(dat$minus10/(1-dat$minus10))
tm10[which(tm10==-Inf)] = NA
tm10[which(tm10==Inf)] = NA
t1 = log10(dat$one/(1-dat$one))
t1[which(t1==-Inf)] = NA
t1[which(t1==Inf)] = NA
t4 = log10(dat$four/(1-dat$four))
t4[which(t4==-Inf)] = NA
t4[which(t4==Inf)] = NA
t20 = log10(dat$twenty/(1-dat$twenty))
t20[which(t20==-Inf)] = NA
t20[which(t20==Inf)] = NA
t25 = log10(dat$twentyfive/(1-dat$twentyfive))
t25[which(t25==-Inf)] = NA
t25[which(t25==Inf)] = NA
t30 = log10(dat$thirty/(1-dat$thirty))
t30[which(t30==-Inf)] = NA
t30[which(t30==Inf)] = NA
t35 = log10(dat$thirtyfive/(1-dat$thirtyfive))
t35[which(t35==-Inf)] = NA
t35[which(t35==Inf)] = NA
t40 = log10(dat$fourty/(1-dat$fourty))
t40[which(t40==-Inf)] = NA
t40[which(t40==Inf)] = NA

#Fit linear models to find L50
mod = lm(tm10~log10(dat$days))
summary(mod)
L50_m10 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t1~log10(dat$days))
summary(mod)
L50_1 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t4~log10(dat$days))
summary(mod)
L50_4 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t20~dat$days)
summary(mod)
L50_20 = ((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t25~log10(dat$days))
summary(mod)
L50_25 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t30~log10(dat$days))
summary(mod)
L50_30 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t35~log10(dat$days))
summary(mod)
L50_35 = 10^((0 - mod$coef[1]) / mod$coef[2])

mod = lm(t40~log10(dat$days))
summary(mod)
L50_40 = 10^((0 - mod$coef[1]) / mod$coef[2])

#Create data frame and calculate instantaneous rates
dat = data.frame(temp = c(-10, 1, 4, 20, 25, 30, 35, 40, 45), L50 = c(L50_m10, L50_1, L50_4, L50_20, L50_25, L50_30, L50_35, L50_40, 0))

instantaneous.mort = -log(0.5)/dat$L50
instantaneous.mort[8:9] = 1 #apply an instantaneous rate of 1 to values >1

#Fit linear model
x = dat$temp
x2 = x^2
x3= x^3
mod = lm(log(instantaneous.mort)~x+x2+x3)
modb = lm(log(instantaneous.mort)~x+x2)
AIC(mod, modb) # cubic model minimises AIC and is therefore used for parameter estimate
summary(mod)

#Comparison of mortality in water with point estimates of mortality of Ostertagia and Cooperia in faeces
#Data source: Perrson 1974

#Predict mortality rates at 3 and 20 degrees using the above regression
three_water = exp(predict(mod, list(x=3, x2=3^2, x3=3^3)))
twenty_water = exp(predict(mod, list(x=20, x2=20^2, x3=20^3)))

#Predict mortality rate of L3 in faeces based on Perrson's data
dat = data.frame(days = c(0,27,62,98,132,160,196,226,258,281,318,338,378,406),
                   three = c(1,1,1,1,1,1,1,0.5,0.47,0.27,0.57,0.05,0.13,0.28),
                   twenty = c(1,0.2,0.12,0.08,0.04,0.02,0.01,0.02,0.01,0,0,0,NA,NA))

#Due to variability in the data of Persson, it is not possible to fit linear models...
#Estimate the instantaneous mortality rate for each time point and take the mean...
inst.3 = -log(dat$three)/dat$days
inst.20 = -log(dat$twenty)/dat$days
inst.3[inst.3==0] = NA
inst.20[inst.20==Inf] = NA
three_faeces = mean(inst.3, na.rm = TRUE)
twenty_faeces = mean(inst.20, na.rm = TRUE)

#Scaling factor to estimate mortaity in faeces from mortality in soil
scale_three = three_faeces/three_water
scale_twenty = twenty_faeces/twenty_water

####################
#VERTICAL MIGRATION#
####################

#Data source: Callinan and Westcott 1986

dat = data.frame(temp = c(10, 15, 20, 25, 30), L3.herbage = c(3,4.49,1.44,1.37,0.72), L3.soil = c(24.28,14.93,27.82,8.58, 15.54))
prop_L3_herbage = (dat$L3.herbage/(dat$L3.herbage+dat$L3.soil))

#20 degrees anomalous - see main text

#Fit linear model
x = dat$temp[c(1,2,4,5)]
x2 = x^2
mod = lm(log(prop_L3_herbage[c(1,2,4,5)])~x+x2)
summary(mod)
