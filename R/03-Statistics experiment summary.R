libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'effects', 'arm', 'rsq')
lapply(libs, require, character.only = TRUE)

###Statistical Analyses
DTpiles<-readRDS("Input/pile_format.rds")
DTtrials<-readRDS("Input/trial_format.rds")

#Summary info for cafeteria experiments
length(unique(DTtrials$Eartag)) #summary of individuals
DTtrials[Sex=="M", length(unique(Eartag))] #number of males
DTtrials[Sex=="F", length(unique(Eartag))] #number of females
summary(DTtrials$Trial) 
max(DTtrials$Low_temp) 
min(DTtrials$Low_temp) 
median(DTtrials$Low_temp)
max(DTtrials$White)   
median(DTtrials$White)
mean(DTtrials$N)
sd(DTtrials$N)
mean(DTtrials$P)
sd(DTtrials$P)

#correlations to check for overlap between hypothesis testing
cor(DTtrials$White, DTtrials$N)
cor(DTtrials$Low_temp, DTtrials$N)
cor(DTtrials$White, DTtrials$P)
cor(DTtrials$Low_temp, DTtrials$P)
cor(DTtrials$White, DTtrials$Low_temp)
cor(DTtrials$Low_temp, DTtrials$Start_mass)

#summary of feeding rates
DTtrials[, mean(IR)]  #mean total intake rate (g/kg/day)
DTtrials[, sd(IR)]    #standard error for total intake rate (g/kg/day)
DTtrials[, mean(TotalEaten)]  #mean total consumption (g)
DTtrials[, sd(TotalEaten)]    #standard error for toal consumption (g)
DTtrials[, mean(Diff_IR)]    #mean preference (g/kg/day)
DTtrials[, sd(Diff_IR)]      #mean preference (g/kg/day)
DTpiles[, mean(IR), by=Treatment]   #mean intake rates from each pile (g/kg/day)
DTpiles[, sd(IR), by=Treatment]     #standard error for intake rates from each pile (g/kg/day)

#Testing if side affects feeding
summary(lm(DTpiles$IR~DTpiles$Side))
df(lm(DTpiles$IR~DTpiles$Side))

#Weightloss summary stats
DTtrials[, mean(Mass_change)] #mean % body mass lost
DTtrials[, sd(Mass_change)]   #sd %body mass lost


    ###Habituation trends
#Intake rates
DTtrials[, mean(IR), by=Trial]   #means for total eaten by trial
DTtrials[, sd(IR), by=Trial]     #standard error for total eaten by trial
summary(lm(DTtrials$IR~DTtrials$Habituation))

#Preference
summary(lm(DTtrials$Diff_IR~DTtrials$Habituation))  

#weightloss
summary(lm(DTtrials$Mass_change~DTtrials$Habituation))

