libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'effects', 'arm', 'rsq')
lapply(libs, require, character.only = TRUE)

#Import caf experiments in pile format
DTpiles<-readRDS("Input/pile_format.rds")


            #### Feeding AIC ####

Choice.Mod<-list()

#1 = Null model
Choice.Mod[[1]]<-lmer(IR ~ Habituation + (1|sampleID) , REML=F, data=DTpiles)
#2 = Base model
Choice.Mod[[2]]<-lmer(IR ~ Habituation + Treatment + (1|sampleID), REML=F, data=DTpiles)
#3 = Temperature model
Choice.Mod[[3]]<-lmer(IR ~ Habituation + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
#4 = Coat Color model
Choice.Mod[[4]]<-lmer(IR ~ Habituation + White*Treatment + (1|sampleID), REML=F, data=DTpiles)
#5 = Energetic model 
Choice.Mod[[5]]<-lmer(IR ~ Habituation + White*Treatment + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
#6 = Nitrogen model
Choice.Mod[[6]]<-lmer(IR ~ Habituation + N_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
#7 = Phosphorus model
Choice.Mod[[7]]<-lmer(IR ~ Habituation + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
#8 = Nutrient model
Choice.Mod[[8]]<-lmer(IR ~ Habituation + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
#9 = Full model
Choice.Mod[[9]]<-lmer(IR ~ Habituation + White*Treatment + Low_temp*Treatment + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)

#create a vector of names to trace back models in set 
Modnames <- paste("mod", 1:length(Choice.Mod), sep = " ")
#produce AIC table that uses AICc, will be table1 in the paper
AIC<-aictab(REML=F, cand.set = Choice.Mod, modnames = Modnames, sort = TRUE)
AIC<-as.data.table(AIC)
AIC[Modnames=="mod 1", Model:="Null"][Modnames=="mod 2", Model:="Base"][Modnames=="mod 3", Model:="Temperature"]
AIC[Modnames=="mod 4", Model:="Coat Colour"][Modnames=="mod 5", Model:="Energetic"][Modnames=="mod 6", Model:="Nitrogen"]
AIC[Modnames=="mod 7", Model:="Phosphorus"][Modnames=="mod 8", Model:="Nutrient"][Modnames=="mod 9", Model:="Full"]
AIC[, Modnames:=NULL]


#individual model summary outputs
summary(Choice.Mod[[1]]) #Null
summary(Choice.Mod[[2]]) #Base
summary(Choice.Mod[[3]]) #Temp
summary(Choice.Mod[[4]]) #Coat
summary(Choice.Mod[[5]]) #Energetic
summary(Choice.Mod[[6]]) #Nitrogen
summary(Choice.Mod[[7]]) #Phosphorus
summary(Choice.Mod[[8]]) #Nutrient
summary(Choice.Mod[[9]]) #Full

#individual model R2 outputs
r.squaredGLMM(Choice.Mod[[1]]) #Null
r.squaredGLMM(Choice.Mod[[2]]) #Base
r.squaredGLMM(Choice.Mod[[3]]) #Temp
r.squaredGLMM(Choice.Mod[[4]]) #Coat
r.squaredGLMM(Choice.Mod[[5]]) #Energetic
r.squaredGLMM(Choice.Mod[[6]]) #Nitrogen
r.squaredGLMM(Choice.Mod[[7]]) #Phosphorus
r.squaredGLMM(Choice.Mod[[8]]) #Nutrient
r.squaredGLMM(Choice.Mod[[9]]) #Full


#to get effects for the coat colour in the energetics model
effsC <- as.data.table(effect(c("White"), xlevels=15, Choice.Mod[[5]]))
#to get the effects for the temperature in the energetic model
effsT <- as.data.table(effect(c("Low_temp"), xlevels=15, Choice.Mod[[5]]))


#Same models but diff function
Null2<-lme(IR ~ 1, random=~1|sampleID, data=DTpiles)
Base2<-lme(IR ~ Habituation + Treatment, random=~1|sampleID,  data=DTpiles)
Temp2<-lme(IR ~ Habituation + Low_temp*Treatment, random=~1|sampleID,  data=DTpiles)
Coat2<-lme(IR ~ Habituation + White*Treatment, random=~1|sampleID,  data=DTpiles)
Energetic2<-lme(IR ~ Habituation + Low_temp*Treatment + White*Treatment, random=~1|sampleID,  data=DTpiles)
Nitrogen2<-lme(IR ~ Habituation + N_mean*Treatment, random=~1|sampleID,  data=DTpiles)
Phosphorus2<-lme(IR ~ Habituation + P_mean*Treatment, random=~1|sampleID,  data=DTpiles)
Nutrient2<-lme(IR ~ Habituation + N_mean*Treatment + P_mean*Treatment, random=~1|sampleID,  data=DTpiles)
Full2<-lme(IR ~ Habituation + White*Treatment + Low_temp*Treatment + N_mean*Treatment + P_mean*Treatment, random=~1|sampleID,  data=DTpiles)

summary(Base2)
summary(Temp2)
summary(Coat2)
summary(Energetic2)
summary(Nitrogen2)
summary(Phosphorus2)
summary(Nutrient2)
summary(Full2)


#Saving effects into input folder
saveRDS(effsC, "Input/effects_coat.rds")
saveRDS(effsT, "Input/effects_temp.rds")

#### Saving tables

#table 3
fwrite(AIC, "Findings/Table3.csv")     ###Saving the AIC table

#table 4 information
summary(Energetic2)                  


#table 5: all models
stargazer(Choice.Mod,
          type="html",
          out="Findings/table5.html",
          digits = 2,
          column.labels = c("Null", "Base", "Temp", "Coat", "Energetic", "N", "P", "Nutrient", "Full")
          # covariate.labels = c("Temp", "White", "N", "P", "Rank", "Temp*Rank", "White*Rank", "Temp*Rank", "N*Rank", "P*Rank", "N*Rank", "P*Rank"),
          # dep.var.labels = "Grams of Spruce Pile Consumed"
)
