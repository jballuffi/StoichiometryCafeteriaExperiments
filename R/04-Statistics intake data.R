libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'ggeffects', 'arm', 'rsq', 'dplyr')
lapply(libs, require, character.only = TRUE)

#Import caf experiments in pile format
DTpiles<-readRDS("Input/pile_format.rds")
#Import table that shows how models are constructed
models<- fread("Input/model_construction.csv")
models$Body <- NULL #delete column with body condition models


#### Feeding AIC ####

#create models
Null <- lmer(IR ~ Habituation + (1|sampleID) , REML=F, data=DTpiles)
Base <- lmer(IR ~ Habituation + Treatment + (1|sampleID), REML=F, data=DTpiles)
Temp <- lmer(IR ~ Habituation + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Coat <- lmer(IR ~ Habituation + White*Treatment + (1|sampleID), REML=F, data=DTpiles)
Energetic <- lmer(IR ~ Habituation + White*Treatment + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nitrogen <- lmer(IR ~ Habituation + N_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Phosphorus <- lmer(IR ~ Habituation + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nutrient <- lmer(IR ~ Habituation + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Full <- lmer(IR ~ Habituation + White*Treatment + Low_temp*Treatment + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)

#List and build AIC table
Mods<-list(Null, Base, Temp, Coat, Energetic, Nitrogen, Phosphorus, Nutrient, Full)
Names<-c('Null', 'Base', 'Temp', 'Coat', 'Energetic', 'Nitrogen', 'Phosphorus', 'Nutrient', 'Full')
AIC<-as.data.table(aictab(REML=F, cand.set = Mods, modnames = Names, sort = TRUE))
AIC[,ModelLik:=NULL]
AIC[,Cum.Wt:=NULL]
#round whole table to 3 dec places
AIC<-AIC %>% mutate_if(is.numeric, round, digits=3)

#merge model information with AIC output
AICMS<- merge(AIC, models, by.x="Modnames", by.y="Model_name")

#Function to collect R2s for every model
collectR2 <- function(model) {
  #collect R2s
  rsqOut <- data.table(r.squaredGLMM(model))
  rsqOut<- round(rsqOut, 2)
  #return each datatable binded together by row
  return(data.table(rsqOut))
}

#run function and get R2s for all models
R2s<-lapply(Mods, collectR2)
R2s<-rbindlist(R2s, fill = TRUE)
R2s$Modnames<-Names

#merge R2s with AIC table
AICMS<- merge(AICMS, R2s, by="Modnames")

#creating final format for table 2
Tab2<- AICMS[, .(Modnames, Feeding, AICc, Delta_AICc, R2m, R2c)]
setnames(Tab2, "Feeding", "Design")

#to get effects for the coat colour in the energetics model
effsC<-ggpredict(Energetic, terms = c("White", "Treatment"))
#to get the effects for the temperature in the energetic model
effsT<-ggpredict(Energetic, terms = c("Low_temp", "Treatment"))





#####Collect all coefficients and SEs for models####

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

#Combine interaction outputs whose column names were reversed

#grabbing column names with a ":" i.e. interaction outputs
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




####Test AIC results with transformed coat color####

DTpiles[, White_asin := asin(White)]

Null2 <- lmer(IR ~ Habituation + (1|sampleID) , REML=F, data=DTpiles)
Base2 <- lmer(IR ~ Habituation + Treatment + (1|sampleID), REML=F, data=DTpiles)
Temp2 <- lmer(IR ~ Habituation + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Coat2 <- lmer(IR ~ Habituation + White_asin*Treatment + (1|sampleID), REML=F, data=DTpiles)
Energetic2 <- lmer(IR ~ Habituation + White_asin*Treatment + Low_temp*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nitrogen2 <- lmer(IR ~ Habituation + N_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Phosphorus2 <- lmer(IR ~ Habituation + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Nutrient2 <- lmer(IR ~ Habituation + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)
Full2 <- lmer(IR ~ Habituation + White_asin*Treatment + Low_temp*Treatment + N_mean*Treatment + P_mean*Treatment + (1|sampleID), REML=F, data=DTpiles)

#List and AIC
Mods2<-list(Null2, Base2, Temp2, Coat2, Energetic2, Nitrogen2, Phosphorus2, Nutrient2, Full2)
AIC2<-as.data.table(aictab(REML=F, cand.set = Mods2, modnames = Names, sort = TRUE))
AIC2[,ModelLik:=NULL]
AIC2[,Cum.Wt:=NULL]
#round whole table to 3 dec places
AIC2<-AIC2 %>% mutate_if(is.numeric, round, digits=3)


#summarize energetic model with transformed coat color
summary(Energetic2)
#results of non-transformed energetic model 
summary(Energetic)

#checking residuals for non-tranformed energetic model
res<- residuals(Energetic)
hist(res)
fit<- fitted(Energetic)
plot(res~fit)
lag.plot(res, diag = FALSE, do.lines = FALSE)

#check residuals for transformed energetic model
res2<-residuals(Energetic2)
hist(res2)
fit2<-fitted(Energetic2)
plot(res2~fit2)
lag.plot(res2, diag = FALSE, do.lines = FALSE)






#####Saving results####

#Saving effects into input folder
saveRDS(effsC, "Input/effects_coat.rds")
saveRDS(effsT, "Input/effects_temp.rds")

#### Saving tables

#table 2
fwrite(Tab2, "Output/Table2.csv")     ###Saving the AIC table

#table A1
fwrite(OutAll, "Output/TableA1.csv")


#table A1, in stargazer format
stargazer(Mods,
          type="html",
          out="Output/TableA1.html",
          digits = 2,
          column.labels = c("Null", "Base", "Temp", "Coat", "Energetic", "N", "P", "Nutrient", "Full")
          # covariate.labels = c("Temp", "White", "N", "P", "Rank", "Temp*Rank", "White*Rank", "Temp*Rank", "N*Rank", "P*Rank", "N*Rank", "P*Rank"),
          # dep.var.labels = "Grams of Spruce Pile Consumed"
)

