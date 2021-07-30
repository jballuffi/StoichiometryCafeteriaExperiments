libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'ggeffects', 'arm', 'rsq', 'dplyr')
lapply(libs, require, character.only = TRUE)

#Import caf experiments in pile format
DTpiles<-readRDS("Input/pile_format.rds")


#### Feeding AIC ####

Null <- lmer(IR ~ Habituation + (1|sampleID) , REML=F, data=DTpiles)
Base <- lmer(IR ~ Habituation + Treatment + (1|sampleID), REML=F, data=DTpiles)
Temp <- lmer(IR ~ Habituation + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Coat <- lmer(IR ~ Habituation + White*Treatment + (1|sampleID), REML=F, data=DTpiles)
Energetic <- lmer(IR ~ Habituation + White*Treatment + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nitrogen <- lmer(IR ~ Habituation + N_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Phosphorus <- lmer(IR ~ Habituation + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nutrient <- lmer(IR ~ Habituation + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Full <- lmer(IR ~ Habituation + White*Treatment + Low_temp*Treatment + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)

#List and AIC
Mods<-list(Null, Base, Temp, Coat, Energetic, Nitrogen, Phosphorus, Nutrient, Full)
Names<-c('Null', 'Base', 'Temp', 'Coat', 'Energetic', 'Nitrogen', 'Phosphorus', 'Nutrient', 'Full')
AIC<-as.data.table(aictab(REML=F, cand.set = Mods, modnames = Names, sort = TRUE))
AIC[,ModelLik:=NULL]
AIC[,Cum.Wt:=NULL]
#round whole table to 3 dec places
AIC<-AIC %>% mutate_if(is.numeric, round, digits=3)

#to get effects for the coat colour in the energetics model
effsC<-ggpredict(Energetic, terms = c("White", "Treatment"))
#to get the effects for the temperature in the energetic model
effsT<-ggpredict(Energetic, terms = c("Low_temp", "Treatment"))

#Function to collect coefficients, standard errors, and R2s for every model
outputfun <- function(model) {
  #collect coef values
  coefOut <- data.table(t(fixef(model)))
  coefOut<-round(coefOut,2)
  #collect standard errors
  seOut <- data.table(t(se.fixef(model)))
  seOut<-round(seOut,2)
  #Paste coef and standard errors together, rename cols
  coefse<-data.table(t(paste(coefOut, seOut, sep=" ± ")))
  setnames(coefse, paste0(colnames(coefOut)))
  #collect R2s
  rsqOut <- data.table(r.squaredGLMM(model))
  rsqOut<- round(rsqOut, 2)
  #return each datatable binded together by row
  return(data.table(coefse, rsqOut))
}

#apply to same list of models as in AIC
OutAll<-lapply(Mods, outputfun)
OutAll<-rbindlist(OutAll, fill = TRUE)
OutAll$Model<-Names
#chaning some columns in the output
OutAll[,`(Intercept)`:=NULL]
setnames(OutAll, "HabituationHabituated", "Habituation")

#function to swap out specific words in column names for new ones
nameswap <- function(old, new, Data) {
  older<-colnames(Data)[grep(old, colnames(Data))]
  newer <- gsub(old, new, older)
  setnames(Data, older, newer)
}

nameswap(old='TreatmentHigh', new='Quality', Data=OutAll)
nameswap(old='White', new='Coat', Data=OutAll)
nameswap(old='Low_temp', new='Temp', Data=OutAll)
nameswap(old='N_mean', new='N', Data=OutAll)
nameswap(old='P_mean', new='P', Data=OutAll)

####Combine interaction outputs whoe column names were reversed

#grabbinging column names with a ":" i.e. interaction outputs
oldcols <- colnames(OutAll)[grepl(':', colnames(OutAll))]
#re-order the column names
newcols <- vapply(strsplit(oldcols, ':'), function(x) paste0(sort(x), collapse = ':'), 
                  'potato')

#function to merge two columns of the same interaction coef
fix <- function(version1, version2, data) {
  coal <- coalesce(data[[version1]], data[[version2]])
  data[, (version1) := coal]
  data[, (version2) := NULL]
}

#apply fix function to the grabbed columns
lapply(seq.int(oldcols), function(i){
  v1 <- newcols[[i]]
  v2 <- oldcols[[i]]
  if(v1 != v2){
    fix(v1, v2, OutAll)
  }
})

#swaping out ":" for "*"
nameswap(old=':', new='*', Data=OutAll)
#one last name change
setnames(OutAll, "Quality*Temp", "Temp*Quality")

#now reorder the cols
setcolorder(OutAll, c("Model", 
                      "Habituation", 
                      "Quality", 
                      "Temp", "Temp*Quality", 
                      "Coat", "Coat*Quality",
                      "N", "N*Quality",
                      "P", "P*Quality",
                      "R2m","R2c"))


#Saving effects into input folder
saveRDS(effsC, "Input/effects_coat.rds")
saveRDS(effsT, "Input/effects_temp.rds")

#### Saving tables

#table 4
fwrite(AIC, "Output/TableA2.csv")     ###Saving the AIC table

#table 5
fwrite(OutAll, "Output/Table2.csv")


#table 5: all models
stargazer(Mods,
          type="html",
          out="Output/Table2.html",
          digits = 2,
          column.labels = c("Null", "Base", "Temp", "Coat", "Energetic", "N", "P", "Nutrient", "Full")
          # covariate.labels = c("Temp", "White", "N", "P", "Rank", "Temp*Rank", "White*Rank", "Temp*Rank", "N*Rank", "P*Rank", "N*Rank", "P*Rank"),
          # dep.var.labels = "Grams of Spruce Pile Consumed"
)

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

