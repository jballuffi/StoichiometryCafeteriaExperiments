libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'ggeffects', 'arm', 'rsq')
lapply(libs, require, character.only = TRUE)

#import trial format
DTtrials<-readRDS("Input/trial_format.rds")
#elminate two experiments where bunnies escaped
DTtrials<-DTtrials[!is.na(Mass_change)]


#### Weight Change AIC ####

Null <- lm(Mass_change ~ IR, data = DTtrials)

Base <- lm(Mass_change ~ IR + Diff_IR, data = DTtrials)

Temp <- lm(Mass_change ~ IR + Low_temp*Diff_IR, data = DTtrials)

Coat <- lm(Mass_change ~ IR + White*Diff_IR, data = DTtrials)

Energetic <- lm(Mass_change ~ IR + White*Diff_IR + Low_temp*Diff_IR, data = DTtrials)

Nitrogen <- lm(Mass_change ~ IR + N_mean*Diff_IR, data = DTtrials)

Phosphorus <- lm(Mass_change ~ IR + P_mean*Diff_IR, data = DTtrials)

Nutrient <- lm(Mass_change ~ IR + N_mean*Diff_IR + P_mean*Diff_IR, data = DTtrials)

Full <- lm(Mass_change ~ IR + White*Diff_IR + Low_temp*Diff_IR + N_mean*Diff_IR + P_mean*Diff_IR, data = DTtrials)

#List and AIC
Mods<-list(Null, Base, Temp, Coat, Energetic, Nitrogen, Phosphorus, Nutrient, Full)
Names<-c('Null', 'Base', 'Temp', 'Coat', 'Energetic', 'Nitrogen', 'Phosphorus', 'Nutrient', 'Full')
AIC<-as.data.table(aictab(REML=F, cand.set = Mods, modnames = Names, sort = TRUE))
AIC[,ModelLik:=NULL]



#to get effects for weightloss~preference
effsP <- ggpredict(Base, terms = c("Diff_IR"))

#Function to collect coefficients, standard errors, and R2s for every model
outputfun <- function(model) {
  #collect coef values
  coefOut <- data.table(t(coef(model)))
  coefOut<-round(coefOut, 2)
  #collect standard errors
  seOut <- data.table(t(se.coef(model)))
  seOut<-round(seOut, 2)
  #Paste coef and standard errors together, rename cols
  coefse<-data.table(t(paste(coefOut, seOut, sep=" ± ")))
  setnames(coefse, paste0(colnames(coefOut)))
  #collect R2s and change column name
  rsqOut <- data.table(rsq(model))
  names(rsqOut)<-c("rsq")
  rsqOut <- round(rsqOut, 2)
  #return each datatable binded together by row
  return(data.table(coefse, rsqOut))
}

#apply to same list of models as in AIC
OutAll<-lapply(Mods, outputfun)
OutAll<-rbindlist(OutAll, fill = TRUE)
OutAll$Model<-Names


#saving the effects
saveRDS(effsP, "Input/effects_pref.rds")

#### Saving tables

#table 6
fwrite(AIC, "Findings/Table6.csv")     ###Saving the AIC table

#table 7
fwrite(OutAll, "Findings/Table7.csv")
                

#Another option for table 7
stargazer(Mods,
          type="html",
          out="Findings/table7.html",
          digits = 2,
          column.labels = c("Null", "Base", "Temp", "Coat", "Energetic", "N", "P", "Nutrient", "Full"),
          float.env = "sidewaystable"
          # dep.var.labels = "Grams of Spruce Pile Consumed"
)
