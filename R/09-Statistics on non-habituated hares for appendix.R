libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'ggeffects', 'arm', 'rsq', 'dplyr')
lapply(libs, require, character.only = TRUE)

#Import caf experiments in pile format
DTpiles<-readRDS("Input/pile_format.rds")

#Import caf experiments in trial format
DTtrials<-readRDS("Input/trial_format.rds")
#elminate two experiments where bunnies escaped
DTtrials<-DTtrials[!is.na(Mass_change)]

#Subset data to only include first trials, or non-habituated hares
DTpiles<-DTpiles[Trial==1]
DTtrials<-DTtrials[Trial==1]


#Feeding response AIC----

Null <- lmer(IR ~ (1|sampleID) , REML=F, data=DTpiles)
Base <- lmer(IR ~ Treatment + (1|sampleID), REML=F, data=DTpiles)
Temp <- lmer(IR ~ Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Coat <- lmer(IR ~ White*Treatment + (1|sampleID), REML=F, data=DTpiles)
Energetic <- lmer(IR ~ White*Treatment + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nitrogen <- lmer(IR ~ N_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Phosphorus <- lmer(IR ~ P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nutrient <- lmer(IR ~ N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Full <- lmer(IR ~ White*Treatment + Low_temp*Treatment + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)

#List and AIC
Mods<-list(Null, Base, Temp, Coat, Energetic, Nitrogen, Phosphorus, Nutrient, Full)
Names<-c('Null', 'Base', 'Temp', 'Coat', 'Energetic', 'Nitrogen', 'Phosphorus', 'Nutrient', 'Full')
AICfeed<-as.data.table(aictab(REML=F, cand.set = Mods, modnames = Names, sort = TRUE))
AICfeed[,ModelLik:=NULL]
AICfeed[,Cum.Wt:=NULL]
#round whole table to 3 dec places
AICfeed<-AICfeed %>% mutate_if(is.numeric, round, digits=3)


#Body condition responses ----

Null2 <- lm(Mass_change ~ IR, data = DTtrials)
Base2 <- lm(Mass_change ~ IR + Diff_IR, data = DTtrials)
Temp2 <- lm(Mass_change ~ IR + Low_temp*Diff_IR, data = DTtrials)
Coat2 <- lm(Mass_change ~ IR + White*Diff_IR, data = DTtrials)
Energetic2 <- lm(Mass_change ~ IR + White*Diff_IR + Low_temp*Diff_IR, data = DTtrials)
Nitrogen2 <- lm(Mass_change ~ IR + N_mean*Diff_IR, data = DTtrials)
Phosphorus2 <- lm(Mass_change ~ IR + P_mean*Diff_IR, data = DTtrials)
Nutrient2 <- lm(Mass_change ~ IR + N_mean*Diff_IR + P_mean*Diff_IR, data = DTtrials)
Full2 <- lm(Mass_change ~ IR + White*Diff_IR + Low_temp*Diff_IR + N_mean*Diff_IR + P_mean*Diff_IR, data = DTtrials)

#List and AIC
Mods2<-list(Null2, Base2, Temp2, Coat2, Energetic2, Nitrogen2, Phosphorus2, Nutrient2, Full2)
AICbody<-as.data.table(aictab(REML=F, cand.set = Mods2, modnames = Names, sort = TRUE))
AICbody[,ModelLik:=NULL]
AICbody[,Cum.Wt:=NULL]
#round whole table to 3 dec places
AICbody<-AICbody %>% mutate_if(is.numeric, round, digits=3)


#Save appendix AIC tables
fwrite(AICfeed, "Findings/Table8.csv")
fwrite(AICbody, "Findings/Table9.csv")
