libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'effects', 'arm', 'rsq')
lapply(libs, require, character.only = TRUE)

#import trial format
DTtrials<-readRDS("Input/trial_format.rds")
DTtrials<-DTtrials[!is.na(Mass_change)]


summary(lm(Mass_change~Habituation, data=DTtrials))

#### Weight Change AIC ####

Choice.Mod<-list()

#1 = Null model
Choice.Mod[[1]]<-lm(Mass_change ~ 1, data = DTtrials)
#2 = Base model
Choice.Mod[[2]]<-lm(Mass_change ~ Diff_IR, data = DTtrials)
#3 = Temperature model
Choice.Mod[[3]]<- lm(Mass_change ~ Diff_IR + Low_temp, data = DTtrials)
#4 = Coat Color model
Choice.Mod[[4]]<- lm(Mass_change ~ Diff_IR + White, data = DTtrials)
#5 = Energetic model 
Choice.Mod[[5]]<- lm(Mass_change ~ Diff_IR + White*Low_temp, data = DTtrials)
#6 = Nitrogen model
Choice.Mod[[6]]<- lm(Mass_change ~ Diff_IR + N_mean, data = DTtrials)
#7 = Phosphorus model
Choice.Mod[[7]]<- lm(Mass_change ~ Diff_IR + P_mean, data = DTtrials)
#8 = Nutrient model
Choice.Mod[[8]]<- lm(Mass_change ~ Diff_IR + N_mean*P_mean, data = DTtrials)
#9 = Full model
Choice.Mod[[9]]<- lm(Mass_change ~ Diff_IR + White + Low_temp + N_mean + P_mean, data = DTtrials)

#create a vector of names to trace back models in set 
Modnames <- paste("mod", 1:length(Choice.Mod), sep = " ")
#produce AIC table that uses AICc, will be table1 in the paper
AIC<-aictab(REML=F, cand.set = Choice.Mod, modnames = Modnames, sort = TRUE)
AIC<-as.data.table(AIC)
AIC[Modnames=="mod 1", Model:="Null"][Modnames=="mod 2", Model:="Base"][Modnames=="mod 3", Model:="Temperature"]
AIC[Modnames=="mod 4", Model:="Coat Colour"][Modnames=="mod 5", Model:="Energetic"][Modnames=="mod 6", Model:="Nitrogen"]
AIC[Modnames=="mod 7", Model:="Phosphorus"][Modnames=="mod 8", Model:="Nutrient"][Modnames=="mod 9", Model:="Full"]

AIC[, Modnames:=NULL]

Base<-lm(Mass_change ~ Diff_IR, data = DTtrials)
Temp<-lm(Mass_change ~ Diff_IR + Low_temp, data = DTtrials)
Coat<-lm(Mass_change ~ Diff_IR + White, data = DTtrials)
Energetic<-lm(Mass_change ~ Diff_IR + White*Low_temp, data = DTtrials)
Nitrogen<-lm(Mass_change ~ Diff_IR + N_mean, data = DTtrials)
Phosphorus<-lm(Mass_change ~ Diff_IR + P_mean, data = DTtrials)
Nutrient<-lm(Mass_change ~ Diff_IR + N_mean*P_mean, data = DTtrials)
Full<-lm(Mass_change ~ Diff_IR + White + Low_temp + N_mean + P_mean, data = DTtrials)

summary(Base)
summary(Temp)
summary(Coat)
summary(Energetic)
summary(Nitrogen)
summary(Phosphorus)
summary(Nutrient)
summary(Full)
