libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'effects', 'arm', 'rsq')
lapply(libs, require, character.only = TRUE)

      ###Statistical Analyses
DTpiles<-readRDS("Input/pile_format.rds")
DTtrials<-readRDS("Input/trial_format.rds")

DTpiles<-DTpiles[Trial==1]
DTtrials<-DTtrials[Trial==1]


#Summary info for cafeteria experiments
unique(DTpiles$Date) #summary of experiment dates
unique(DTtrials$Year) #summary of years
unique(DTpiles$Eartag) #summary of individuals
max(DTpiles$Low_temp) #max lowest temp
min(DTpiles$Low_temp) #min lowest temp
max(DTpiles$White)  
median(DTpiles$White)
summary(DTtrials)

DTtrials[, mean(TotalEaten)]  #mean total eaten from both piles
DTtrials[, sd(TotalEaten)]    #standard error for toal eaten from both piles
DTpiles[, mean(Total), by=Treatment]   #means for total eaten from each pile
DTpiles[, sd(Total), by=Treatment]     #standard error for total eaten from each pile

#correlations to check for overlap between hypothesis testing
cor(DTtrials$White, DTtrials$N)
cor(DTtrials$Low_temp, DTtrials$N)
cor(DTtrials$White, DTtrials$P)
cor(DTtrials$Low_temp, DTtrials$P)
cor(DTtrials$White, DTtrials$Low_temp)
cor(DTtrials$Low_temp, DTtrials$Start_mass)


                      #### Feeding AIC ####

Choice.Mod<-list()

#1 = Null model
Choice.Mod[[1]]<-lmer(IR ~ 1 + (1|sampleID) , REML=F, data=DTpiles)
#2 = Base model
Choice.Mod[[2]]<-lmer(IR ~ Treatment + (1|sampleID), REML=F, data=DTpiles)
#3 = Temperature model
Choice.Mod[[3]]<-lmer(IR ~ Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
#4 = Coat Color model
Choice.Mod[[4]]<-lmer(IR ~ White*Treatment + (1|sampleID), REML=F, data=DTpiles)
#5 = Energetic model 
Choice.Mod[[5]]<-lmer(IR ~ White*Treatment + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
#6 = Nitrogen model
Choice.Mod[[6]]<-lmer(IR ~ N_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
#7 = Phosphorus model
Choice.Mod[[7]]<-lmer(IR ~ P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
#8 = Nutrient model
Choice.Mod[[8]]<-lmer(IR ~ N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
#9 = Full model
Choice.Mod[[9]]<-lmer(IR ~ White*Treatment + Low_temp*Treatment + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)

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
Base<-lmer(IR ~ Treatment + (1|sampleID), REML=F, data=DTpiles)
Temp<-lmer(IR ~ Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Coat<-lmer(IR ~ White*Treatment + (1|sampleID), REML=F, data=DTpiles)
Energetic<-lmer(IR ~ White*Treatment + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nitrogen<-lmer(IR ~ N_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Phosphorus<-lmer(IR ~ P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nutrient<-lmer(IR ~ N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Full<-lmer(IR ~ White*Treatment + Low_temp*Treatment + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)

r.squaredGLMM(Energetic) #information for caption of table 3
summary(Energetic)


#to get effects for the coat colour in the energetics model
effsC <- as.data.table(effect(c("White"), xlevels=15, Energetic))
#to get the effects for the temperature in the energetic model
effsT <- as.data.table(effect(c("Low_temp"), xlevels=15, Energetic))


#Same models but diff function
Null2<-lme(IR ~ 1, random=~1|sampleID, data=DTpiles)
Base2<-lme(IR ~ Treatment, random=~1|sampleID,  data=DTpiles)
Temp2<-lme(IR ~ Low_temp*Treatment, random=~1|sampleID,  data=DTpiles)
Coat2<-lme(IR ~ White*Treatment, random=~1|sampleID,  data=DTpiles)
Energetic2<-lme(IR ~ Low_temp*Treatment + White*Treatment, random=~1|sampleID,  data=DTpiles)
Nitrogen2<-lme(IR ~ N_mean*Treatment, random=~1|sampleID,  data=DTpiles)
Phosphorus2<-lme(IR ~ P_mean*Treatment, random=~1|sampleID,  data=DTpiles)
Nutrient2<-lme(IR ~ N_mean*Treatment + P_mean*Treatment, random=~1|sampleID,  data=DTpiles)
Full2<-lme(IR ~ White*Treatment + Low_temp*Treatment + N_mean*Treatment + P_mean*Treatment, random=~1|sampleID,  data=DTpiles)


saveRDS(effsC, "Input/effects_coat.rds")
saveRDS(effsT, "Input/effects_temp.rds")

        #### Saving tables

#table 2
fwrite(AIC, "Findings/Table2.csv")     ###Saving the AIC table

#table 3 information
summary(Energetic2)                  


#table 4: all models
stargazer(Null, Base, Temp, Coat, Energetic, Nitrogen, Phosphorus, Nutrient, Full,
          type="html",
          out="Findings/table4.html",
          digits = 2,
          column.labels = c("Null", "Base", "Temp", "Coat", "Energetic", "N", "P", "Nutrient", "Full"),
          covariate.labels = c("Temp", "White", "N", "P", "Rank", "Temp*Rank", "White*Rank", "Temp*Rank", "N*Rank", "P*Rank", "N*Rank", "P*Rank"),
         dep.var.labels = "Grams of Spruce Pile Consumed"
)



