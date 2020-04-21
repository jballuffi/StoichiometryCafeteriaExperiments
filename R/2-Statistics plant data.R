libs<-c("data.table",'stargazer', 'AICcmodavg', 'ggplot2','RColorBrewer','lme4','pwr','MuMIn','nlme', 'effects', 'arm', 'rsq')
lapply(libs, require, character.only = TRUE)

      ###Stats for Black Spruce Variation on grid

#Import trap/grid dataframe
DTtraps<-readRDS("Input/all_trap_locs.rds")
#clean DTtraps for summary stats
DTtraps<-DTtraps[!Sampling=="Interpolated"]

#summary stats for the trapping grid
#for nitrogen
DTtraps[,mean(N)]
DTtraps[,max(N)]
DTtraps[,min(N)]
DTtraps[,sd(N)]
DTtraps[Rank=="High", median(N)]
DTtraps[Rank=="Low", median(N)]

#for phosphorus
DTtraps[,mean(P)]
DTtraps[,max(P)]
DTtraps[,min(P)]
DTtraps[,sd(P)]
DTtraps[Rank=="High", median(P)]
DTtraps[Rank=="Low", median(P)]

#for secondary compounds
DTtraps[,mean(PSC)]
DTtraps[,max(PSC)]
DTtraps[,min(PSC)]
DTtraps[,sd(PSC)]
DTtraps[Rank=="High", median(PSC)]
DTtraps[Rank=="Low", median(PSC)]


#for canopy cover
DTtraps[,mean(CanopyClosure)]
DTtraps[,max(CanopyClosure)]
DTtraps[,min(CanopyClosure)]
DTtraps[,sd(CanopyClosure)]

#for DBH
DTtraps[,mean(AvgDBH)]
DTtraps[,max(AvgDBH)]
DTtraps[,min(AvgDBH)]
DTtraps[,sd(AvgDBH)]


#correlation between N and P across the grid
cor(DTtraps$N, DTtraps$P)
summary(lm(DTtraps$P ~ DTtraps$N))
#correlation between N and PSCs across the grid
cor(DTtraps$PSC, DTtraps$N)
summary(lm(DTtraps$PSC ~ DTtraps$N))
#correlation between P and PSCs across the grid
cor(DTtraps$PSC, DTtraps$P)
summary(lm(DTtraps$PSC ~ DTtraps$P))

#Habitat covariate analysis for Nitrogen
HabModN<-lm(DTtraps$N~DTtraps$AvgDBH+DTtraps$CanopyClosure)
summary(HabModN)
resN<-residuals(HabModN)
hist(resN)
fitN<-fitted(HabModN)
plot(resN~fitN)
lag.plot(resN,diag = FALSE, do.lines = FALSE)


#Habitat covariate analysis for Phosphorus
HabModP<-lm(DTtraps$P~DTtraps$AvgDBH+DTtraps$CanopyClosure)
summary(HabModP)
resP<-residuals(HabModP)
hist(resP)
fitP<-fitted(HabModP)
plot(resP~fitP)
lag.plot(resP,diag = FALSE, do.lines = FALSE)

#Habitat covariate analysis for PSCs
HabModPSC<-lm(DTtraps$PSC~DTtraps$AvgDBH+DTtraps$CanopyClosure)
summary(HabModPSC)
resPSC<-residuals(HabModPSC)
hist(resPSC)
fitPSC<-fitted(HabModPSC)
plot(resPSC~fitPSC)
lag.plot(resPSC,diag = FALSE, do.lines = FALSE)
