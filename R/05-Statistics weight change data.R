libs<-c("data.table",'stargazer', 'AICcmodavg', 'MuMIn', 'ggeffects', 'arm', 'rsq', 'dplyr')
lapply(libs, require, character.only = TRUE)

#import trial format
DTtrials<-readRDS("Input/trial_format.rds")
#elminate two experiments where bunnies escaped
DTtrials<-DTtrials[!is.na(Mass_change)]
#Import table that shows how models are constructed
models<- fread("Input/model_construction.csv")
models$Feeding <- NULL #delete column with feeding response models



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
Tab3<- AICMS[, .(Modnames, Body, AICc, Delta_AICc, R2m, R2c)]
setnames(Tab3, "Body", "Design")


#to get effects for weightloss~preference
effsP <- ggpredict(Base, terms = c("Diff_IR"))


####Collect model output information####

#Function to collect coefficients, standard errors, and R2s for every model
outputfun <- function(model) {
  #collect coef values
  coefOut <- data.table(t(coef(model)))
  coefOut<-round(coefOut, 3)
  #collect standard errors
  seOut <- data.table(t(se.coef(model)))
  seOut<-round(seOut, 3)
  #Paste coef and standard errors together, rename cols
  coefse<-data.table(t(paste(coefOut, seOut, sep=" ± ")))
  setnames(coefse, paste0(colnames(coefOut)))
  #collect R2s and change column name
  rsqOut <- data.table(rsq(model))
  names(rsqOut)<-c("rsq")
  rsqOut <- round(rsqOut, 3)
  #return each datatable binded together by row
  return(data.table(coefse, rsqOut))
}

#apply to same list of models as in AIC
OutAll<-lapply(Mods, outputfun)
OutAll<-rbindlist(OutAll, fill = TRUE)
OutAll$Model<-Names
OutAll[,`(Intercept)`:=NULL]

#function to swap out specific words in column names for new ones
nameswap <- function(old, new, Data) {
  older<-colnames(Data)[grep(old, colnames(Data))]
  newer <- gsub(old, new, older)
  setnames(Data, older, newer)
}
nameswap(old='Diff_IR', new='Pref', Data=OutAll)
nameswap(old='White', new='Coat', Data=OutAll)
nameswap(old='Low_temp', new='Temp', Data=OutAll)
nameswap(old='N_mean', new='N', Data=OutAll)
nameswap(old='P_mean', new='P', Data=OutAll)

#Combine interaction outputs whoe column names were reversed

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

#swapping out ":" for "*"
nameswap(old=':', new='*', Data=OutAll)
#one last name change
setnames(OutAll, "Pref*Temp", "Temp*Pref")

#now reorder the cols
setcolorder(OutAll, c("Model", 
                      "IR", 
                      "Pref", 
                      "Temp", "Temp*Pref", 
                      "Coat", "Coat*Pref",
                      "N", "N*Pref",
                      "P", "P*Pref",
                      "rsq"))


####Test AIC results with transformed coat color data####

DTtrials[, White_asin:= asin(White)]

Null2 <- lm(Mass_change ~ IR, data = DTtrials)
Base2 <- lm(Mass_change ~ IR + Diff_IR, data = DTtrials)
Temp2 <- lm(Mass_change ~ IR + Low_temp*Diff_IR, data = DTtrials)
Coat2 <- lm(Mass_change ~ IR + White_asin*Diff_IR, data = DTtrials)
Energetic2 <- lm(Mass_change ~ IR + White_asin*Diff_IR + Low_temp*Diff_IR, data = DTtrials)
Nitrogen2 <- lm(Mass_change ~ IR + N_mean*Diff_IR, data = DTtrials)
Phosphorus2 <- lm(Mass_change ~ IR + P_mean*Diff_IR, data = DTtrials)
Nutrient2 <- lm(Mass_change ~ IR + N_mean*Diff_IR + P_mean*Diff_IR, data = DTtrials)
Full2 <- lm(Mass_change ~ IR + White_asin*Diff_IR + Low_temp*Diff_IR + N_mean*Diff_IR + P_mean*Diff_IR, data = DTtrials)

#List and AIC
Mods2<-list(Null2, Base2, Temp2, Coat2, Energetic2, Nitrogen2, Phosphorus2, Nutrient2, Full2)
AIC2<-as.data.table(aictab(REML=F, cand.set = Mods2, modnames = Names, sort = TRUE))
AIC2[,ModelLik:=NULL]
AIC2[,Cum.Wt:=NULL]
#round whole table to 3 dec places
AIC2<-AIC2 %>% mutate_if(is.numeric, round, digits=3)




####Save everything####


#saving the effects
saveRDS(effsP, "Input/effects_pref.rds")


#table 3
fwrite(Tab3, "Output/Table3.csv")     ###Saving the AIC table

#table A2
fwrite(OutAll, "Output/TableA2.csv")
                

#Another option for table A2
stargazer(Mods,
          type="html",
          out="Output/TableA2.html",
          digits = 2,
          column.labels = c("Null", "Base", "Temp", "Coat", "Energetic", "N", "P", "Nutrient", "Full"),
          float.env = "sidewaystable"
          # dep.var.labels = "Grams of Spruce Pile Consumed"
)

#table A4
fwrite(AIC2, "Output/TableA4_2.csv")
