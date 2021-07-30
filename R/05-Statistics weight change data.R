libs<-c("data.table",'stargazer', 'AICcmodavg', 'MuMIn', 'ggeffects', 'arm', 'rsq', 'dplyr')
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
AIC[,Cum.Wt:=NULL]
#round whole table to 3 dec places
AIC<-AIC %>% mutate_if(is.numeric, round, digits=3)



#to get effects for weightloss~preference
effsP <- ggpredict(Base, terms = c("Diff_IR"))

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



#saving the effects
saveRDS(effsP, "Input/effects_pref.rds")

#### Saving tables

#table 6
fwrite(AIC, "Output/TableA2_2.csv")     ###Saving the AIC table

#table 7
fwrite(OutAll, "Output/Table3.csv")
                

#Another option for table 7
stargazer(Mods,
          type="html",
          out="Output/Table3.html",
          digits = 2,
          column.labels = c("Null", "Base", "Temp", "Coat", "Energetic", "N", "P", "Nutrient", "Full"),
          float.env = "sidewaystable"
          # dep.var.labels = "Grams of Spruce Pile Consumed"
)
