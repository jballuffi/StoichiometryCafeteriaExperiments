libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'effects', 'arm', 'rsq')
lapply(libs, require, character.only = TRUE)

      ###Statistical Analyses
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


#model to run separately, don't show p-values, but can get R2s
Null<-lmer(IR ~ 1 + (1|sampleID) , REML=F, data=DTpiles)
Base<-lmer(IR ~ Habituation + Treatment + (1|sampleID), REML=F, data=DTpiles)
Temp<-lmer(IR ~ Habituation + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Coat<-lmer(IR ~ Habituation + White*Treatment + (1|sampleID), REML=F, data=DTpiles)
Energetic<-lmer(IR ~ Habituation + White*Treatment + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nitrogen<-lmer(IR ~ Habituation + N_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Phosphorus<-lmer(IR ~ Habituation + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nutrient<-lmer(IR ~ Habituation + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Full<-lmer(IR ~ Habituation + White*Treatment + Low_temp*Treatment + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)

r.squaredGLMM(Null)
r.squaredGLMM(Base)
r.squaredGLMM(Temp)
r.squaredGLMM(Coat)
r.squaredGLMM(Energetic)
r.squaredGLMM(Nitrogen)
r.squaredGLMM(Phosphorus)
r.squaredGLMM(Nutrient)
r.squaredGLMM(Full)

#to get effects for the coat colour in the energetics model
effsC <- as.data.table(effect(c("White"), xlevels=15, Energetic))
#to get the effects for the temperature in the energetic model
effsT <- as.data.table(effect(c("Low_temp"), xlevels=15, Energetic))


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

