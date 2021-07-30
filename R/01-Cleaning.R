libs<-c('data.table','sf', 'rgdal','raster','sp')
lapply(libs, require, character.only = TRUE)

####Data prep and cleaning

## Nutritional Mapping and extraction

#Setting up a dataframe for the original nutrient sampling
Spruce<-read.csv("Input/PIMA_2018Oct6.csv")
Spruce<-as.data.table(Spruce)
Spruce<-Spruce[Site=="BL"]
Spruce$N<-Spruce$Total.N
Spruce[, Total.N:=NULL] [, SampleLabel:=NULL] [, Site:=NULL] [, Species:=NULL]
Spruce[,Sampling:="Sampled"]

#Identifying the trap locations we clipped from
Spruce[SampleLoc%in%c("I2","I4","J1","L6","L4","N5"), Sampling:="Offered"]
Spruce[SampleLoc%in%c("I2","I4","J1"), Rank:="High"]
Spruce[SampleLoc%in%c("L4","L6","N5"), Rank:="Low"]
Spruce[is.na(Rank), Rank:="None"]

#Setting up the raster layers of interpolated nutrient maps
utm21N <- '+proj=longlat +zone=21 ellps=WGS84'
#import raster layers for bloomfield N, P, and secondary compounds and project correctly
bloomN<-raster("Input/PIMA_N.tif")
bloomP<-raster("Input/PIMA_P.tif")
bloomPSC<-raster("Input/terpene_idw.tif")
bloomN<-projectRaster(bloomN,crs=utm21N)
bloomP<-projectRaster(bloomP,crs=utm21N)
bloomPSC<-projectRaster(bloomPSC,crs=utm21N)

#convering all rasters into dataframes/datatables for future figures
#not to used further in this script, only just saved at end
bloomN_points <- rasterToPoints(bloomN)
gridN <- data.table(bloomN_points)
setnames(gridN, "PIMA_N", "N")

bloomP_points <- rasterToPoints(bloomP)
gridP <- data.table(bloomP_points)
setnames(gridP, "PIMA_P", "P")

bloomPSC_points <- rasterToPoints(bloomPSC)
gridPSC <- data.table(bloomPSC_points)
setnames(gridPSC, "terpene_idw", "PSC")


#import the grid shapefile, get rid of other 3 grid locations
grid<-as.data.table(st_read("Input/SamplePoints_AllGrids.shp"))
grid<-grid[SiteName=="Bloomfield"]
grid[, geometry:=NULL]
grid[, SiteName:=NULL]
#create a trap column from column keeping grid and trap
grid[, Trap := tstrsplit(PlotName, "+", fixed=TRUE, keep=c(2))]
grid[, PlotName:=NULL]

#import habitat covariates for trap locations
Cov<-data.table(read.csv("Input/PlotCovariates.csv"))
Cov<-Cov[SiteName=="Bloomfield"]
Cov[, SampleLoc := tstrsplit(PlotName, "+", fixed=TRUE, keep=c(2))]
Cov[, PlotName:=NULL]
Cov[, SiteName:=NULL]

#take out just the coordinate columns for raster extraction
grid.point<-grid[,.(POINT_X,POINT_Y)]
coordinates(grid.point) <- c("POINT_X", "POINT_Y")
#extract grid Nitrogen and phosphorus at each trap location
trapN<-extract(bloomN, grid.point)
trapP<-extract(bloomP, grid.point)
trapPSC<-extract(bloomPSC, grid.point)
#add in the extracted values into the grid datatable
grid[, N := trapN]
grid[, P := trapP]
grid[, PSC := trapPSC]
trap.locs<-as.data.table(grid.point)
#create a datatable with just traps and nutrient measures
trap.nutrients<-grid[,.(Trap, N, P, PSC)]
#create a datatable with the grid locs and PSCs
grid.PSC<-grid[,.(POINT_X, POINT_Y, Trap, PSC)]

#Merge grid dataframe with spruce nutrient sampling with new "Sampling" column
DTtraps<-merge(Spruce, grid.PSC, by.x = c('SampleLoc'), by.y = c('Trap'), all=TRUE)
DTtraps[is.na(Sampling), Sampling:="Interpolated"]
#clear out PSM data where interpolated
DTtraps[is.na(P), PSC:=NA]
#Merge stoich data with habitat data
DTtraps<-merge(DTtraps, Cov, by=c('SampleLoc'), all=TRUE)
#create a new column after that which specifically addresses if spruce was present or not
DTtraps[Sampling=="Sampled", Spruce:="Present"]
DTtraps[Sampling=="Offered", Spruce:="Present"]
DTtraps[Sampling=="Interpolated", Spruce:="Absent"]


#importing trapping data to get all locations each individual were trapped at
trapping<-fread("Input/Trapping_2018_2019.csv")
trapping$Date <- as.Date(trapping$Date, format = "%m/%d/%Y")
trapping[, Year := tstrsplit(Date, "-", fixed=TRUE, keep=c(1))]

#get the number of individuals on the trapping grid per year
trapping[, length(unique(Ear_tag)), by=Year]

trapping.nutrients<-merge(trapping, trap.nutrients, by=c("Trap"), all.x=TRUE)





#now taking the mean N, P, and PSC for each individual across all locs they were trapped
ind.N<-trapping.nutrients[,.(mean(N)), by=Ear_tag]
names(ind.N)<-c("Eartag", "N_mean")
ind.P<-trapping.nutrients[,.(mean(P)), by=Ear_tag]
names(ind.P)<-c("Eartag","P_mean")
ind.PSC<-trapping.nutrients[,. (mean(PSC)), by=Ear_tag]
names(ind.PSC)<-c("Eartag", "PSC_mean")
ind.nut<-merge(ind.N, ind.P, by=c("Eartag"))
ind.nut<-merge(ind.nut, ind.PSC, by=c("Eartag"))  #the datasheet for individual origin nutrients




### Import experiment data

##each row represents one cafeteria experiment
trials<-fread("Input/feeding_trials_2018_2019.csv")
trials[, Notes:=NULL]
#create an ID unique to each experiment
trials[, sampleID:=paste(Eartag, Date, sep="_")]
#making a column for year
trials[, Year := tstrsplit(Date, "/", fixed=TRUE, keep=c(3))]
trials[, ID_Year := paste(Eartag, Year, sep="_")]
trials$Year <-factor(trials$Year)

#make trial number a factor with levels
trials$Trial <- factor(trials$Trial, levels = c("1", "2", "3"), ordered = TRUE)
#create another option for trials, either habituated (never experimented on) or not (been through at least one experiment)
trials[Trial==1, Habituation:="Non-habituated"]
trials[Trial==2, Habituation:="Habituated"]
trials[Trial==3, Habituation:="Habituated"]
trials$Habituation<-factor(trials$Habituation, levels = c("Non-habituated", "Habituated"))
#make the trap letters and numbers easier to work with later in ggplot
trials$LetterVis <- factor(trials$Letter, levels = c("A", "C","E", "F", "I", "L", "M", "N"), ordered=TRUE)
trials$NumberVis<-as.character(trials$Number)
#turn % white into a percent
trials$White<-trials$White/100
#making a "bin" of coat colour, turns into a factor with 5 levels
trials$White_bin <- cut(trials$White, breaks = 5)
#merging the nutritional mapping and trap locs with trials dataframe
trials<- merge(ind.nut, trials, by = c('Eartag'), all.y=TRUE)
trials<-merge(trap.nutrients, trials, by = c('Trap'), all.y = TRUE)
#calculate the amounts eaten for each rank
trials[, Diff_high := (Start_high-End_high)]     
trials[, Diff_low := (Start_low-End_low)]       
#mass change times 100
trials[, Mass_change := (Mass_change*100)]


# reorganize raw data into a pile format, each row represents one pile in an experiment
piles<-melt(trials, measure.vars = c("Diff_high", "Diff_low"), variable.name = "Treatment", value.name = "Total" )
piles[Treatment=="Diff_high", Treatment:="High"]
piles[Treatment=="Diff_low", Treatment:="Low"]
piles[, Start_high:=NULL][, Start_low:=NULL][, End_high:=NULL][, End_low:=NULL]
piles[Treatment=="High", Side:=Side_high]
piles[Treatment=="Low", Side:=Side_low]
piles[, Side_high:=NULL][,Side_low:=NULL]
#making all sides lowercase
piles[, Side:=tolower(piles$Side)]
piles$Side<-factor(piles$Side)
#making variables correct categories ('Vis'= for visual)
piles$Treatment<-factor(piles$Treatment, levels=c("Low", "High"))
#for the piles format, calculate the rate of consumption unit = g/kg/day
piles[, IR:=(Total/(Start_mass/1000))]


#for the trials format, make some last calculations
trials[, TotalEaten := Diff_high+Diff_low]    #Calculate how many grams were eaten total; unit = g/day
trials[, Diff := (Diff_high-Diff_low)]        #Calculate the "preference" or the diff between what was eaten from the high and low
trials[, IR:=(TotalEaten/(Start_mass/1000))]  #Calculate the rate of consumption unit = g/kg/day
trials[, IR_high:=(Diff_high/(Start_mass/1000))] #Calculate intake rate of high rank
trials[, IR_low:=(Diff_low/(Start_mass/1000))] #Calculate intake rate of low rank
trials[, Diff_IR := (IR_high-IR_low)] #Calculate the "preference" in intake rate form


###Save the cleaned and organized datasheets in RDS files

saveRDS(piles,"Input/pile_format.rds")   #by pile
saveRDS(DTtraps, "Input/all_trap_locs.rds") #trap locs, names, sampling status, and stoich results
saveRDS(trials, "Input/trial_format.rds")  #by trial
saveRDS(gridN, "Input/bloomfield_N.rds")   #datatable of bloomfield N raster
saveRDS(gridP, "Input/bloomfield_P.rds")  #datatable of bloomfield P raster
saveRDS(gridPSC, "Input/bloomfield_PSC.rds")  #datatable of bloomdield PSC raster

