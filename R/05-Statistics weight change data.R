libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'ggeffects', 'arm', 'rsq')
lapply(libs, require, character.only = TRUE)

#import trial format
DTtrials<-readRDS("Input/trial_format.rds")
#elminate two experiments where bunnies escaped
DTtrials<-DTtrials[!is.na(Mass_change)]


#### Weight Change AIC ####

Choice.Mod<-list()

#1 = Null model
Choice.Mod[[1]]<-lm(Mass_change ~ 1, data = DTtrials)
#2 = Base model
Choice.Mod[[2]]<-lm(Mass_change ~ IR + Diff_IR, data = DTtrials)
#3 = Temperature model
Choice.Mod[[3]]<- lm(Mass_change ~ IR + Low_temp*Diff_IR, data = DTtrials)
#4 = Coat Color model
Choice.Mod[[4]]<- lm(Mass_change ~ IR + White*Diff_IR, data = DTtrials)
#5 = Energetic model 
Choice.Mod[[5]]<- lm(Mass_change ~ IR + White*Diff_IR + Low_temp*Diff_IR, data = DTtrials)
#6 = Nitrogen model
Choice.Mod[[6]]<- lm(Mass_change ~ IR + N_mean*Diff_IR, data = DTtrials)
#7 = Phosphorus model
Choice.Mod[[7]]<- lm(Mass_change ~ IR + P_mean*Diff_IR, data = DTtrials)
#8 = Nutrient model
Choice.Mod[[8]]<- lm(Mass_change ~ IR + N_mean*Diff_IR + P_mean*Diff_IR, data = DTtrials)
#9 = Full model
Choice.Mod[[9]]<- lm(Mass_change ~ IR + White*Diff_IR + Low_temp*Diff_IR + N_mean*Diff_IR + P_mean*Diff_IR, data = DTtrials)

#create a vector of names to trace back models in set 
Modnames <- paste("mod", 1:length(Choice.Mod), sep = " ")
#produce AIC table that uses AICc, will be table1 in the paper
AIC<-aictab(REML=F, cand.set = Choice.Mod, modnames = Modnames, sort = TRUE)
AIC<-as.data.table(AIC)
AIC[Modnames=="mod 1", Model:="Null"][Modnames=="mod 2", Model:="Base"][Modnames=="mod 3", Model:="Temperature"]
AIC[Modnames=="mod 4", Model:="Coat Colour"][Modnames=="mod 5", Model:="Energetic"][Modnames=="mod 6", Model:="Nitrogen"]
AIC[Modnames=="mod 7", Model:="Phosphorus"][Modnames=="mod 8", Model:="Nutrient"][Modnames=="mod 9", Model:="Full"]
AIC[, Modnames:=NULL]


#individual model outputs
summary(Choice.Mod[[1]]) #Null
summary(Choice.Mod[[2]]) #Base
summary(Choice.Mod[[3]]) #Temp
summary(Choice.Mod[[4]]) #Coat
summary(Choice.Mod[[5]]) #Energetic
summary(Choice.Mod[[6]]) #Nitrogen
summary(Choice.Mod[[7]]) #Phosphorus
summary(Choice.Mod[[8]]) #Nutrient
summary(Choice.Mod[[9]]) #Full

#to get effects for weightloss~preference
effsP <- ggpredict(Choice.Mod[[2]], terms = c("Diff_IR"))

#Function to collect coefficients, standard errors, and R2s for every model
outputfun <- function(model) {
  #collect coef values
  coefOut <- data.table(t(coef(model)))
  coefOut<-round(coefOut,3)
  #collect standard errors
  seOut <- data.table(t(se.coef(model)))
  seOut<-round(seOut,3)
  #Paste coef and standard errors together, rename cols
  coefse<-data.table(t(paste(coefOut, seOut, sep="±")))
  setnames(coefse, paste0(colnames(coefOut)))
  #collect R2s and change column name
  rsqOut <- data.table(rsq(model))
  names(rsqOut)<-c("rsq")
  #return each datatable binded together by row
  return(data.table(coefse, rsqOut))
}

#apply to same list of models as in AIC
OutAll<-lapply(Choice.Mod, outputfun)
OutAll<-rbindlist(OutAll, fill = TRUE)
#make srting of model names for model column
ModelNames<-c("Null", "Base", "Temp", "Coat", "Energetic", "N", "P", "Nutrient", "Full")
OutAll$Model<-ModelNames


#saving the effects
saveRDS(effsP, "Input/effects_pref.rds")

#### Saving tables

#table 6
fwrite(AIC, "Findings/Table6.csv")     ###Saving the AIC table

#table 7
fwrite(OutAll, "Findings/Table7.csv")
                

#Another option for table 7
stargazer(Choice.Mod,
          type="html",
          out="Findings/table7.html",
          digits = 2,
          column.labels = c("Null", "Base", "Temp", "Coat", "Energetic", "N", "P", "Nutrient", "Full"),
          float.env = "sidewaystable"
          # dep.var.labels = "Grams of Spruce Pile Consumed"
)
