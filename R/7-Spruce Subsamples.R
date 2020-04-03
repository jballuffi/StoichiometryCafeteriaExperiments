library(ggplot2)
library(data.table)
library(ggpubr)

Spruce<-read.csv("Input/Spruce_Stoich_Samples.csv")
DTtraps<-readRDS("Input/all_trap_locs.rds")
DTtrials<-readRDS("Input/trial_format.rds")


#set up spruce datatable
Spruce<-as.data.table(Spruce)
Spruce<-Spruce[!Age_Class=="J"]
Spruce$Pred_Rank<-factor(Spruce$Pred_Rank, levels=c("Low", "High"))
Spruce$Info<-NULL
Spruce[,Sampling:="Fall Offering"]

#set up trap datatable
DTtraps<-DTtraps[!Sampling=="Interpolated"]
DTtraps$Spruce<-NULL
colnames(DTtraps)[6]<-"C"
DTtraps[,Sampling:="Summer Sample"]
DTtraps[SampleLoc%in%c("I2","I4","I6","J1"), Pred_Rank:="High"]
DTtraps[SampleLoc%in%c("L4","L6","N5"), Pred_Rank:="Low"]

#Combine the spruce and trap datatables for future figure S3
Full<- rbindlist(list(DTtraps, Spruce), fill = TRUE, use.names = TRUE)

#Summary Statistics for subsampling
Spruce[, median(C), by=Pred_Rank]
Spruce[, sd(C), by=Pred_Rank]
Spruce[, median(N), by= Pred_Rank]
Spruce[, sd(N), by= Pred_Rank]
Spruce[, median(P), by=Pred_Rank]
Spruce[, sd(P), by=Pred_Rank]

summary(lm(N~Pred_Rank, data=Spruce))
summary(lm(P~Pred_Rank, data=Spruce))
summary(lm(C~Pred_Rank, data=Spruce))
cor(Spruce$P, Spruce$N)


RandomNFun<-function(Data, Var1, Var2){
  SampleTest<-Data[, sample(Var1, 100, replace=TRUE), by=Var2]
  names(SampleTest)<-c("Pred_Rank","N")
  SampleTest[, ID:=1:100, by=Pred_Rank]
  
  SampleTestLow<-SampleTest[Pred_Rank=="Low"]
  SampleTestLow[,Pred_Rank:=NULL]
  SampleTestHigh<-SampleTest[Pred_Rank=="High"]
  SampleTestHigh[,Pred_Rank:=NULL]
  names(SampleTestLow)<-c("N_Low", "ID")
  names(SampleTestHigh)<-c("N_High", "ID")
  
  PairedSampleTest<-merge(SampleTestHigh, SampleTestLow, by=c("ID"), all=TRUE)
  PairedSampleTest[, NResult:=N_High-N_Low]
  PairedSampleTest[NResult>0, NResultBin:="C"]
  PairedSampleTest[NResult<0, NResultBin:="I"]
  PairedSampleTest[NResult==0, NResultBin:="E"]
}


RandomPFun<-function(Data, Var1, Var2){
  SampleTest<-Data[, sample(Var1, 100, replace=TRUE), by=Var2]
  names(SampleTest)<-c("Pred_Rank","P")
  SampleTest[, ID:=1:100, by=Pred_Rank]
  
  SampleTestLow<-SampleTest[Pred_Rank=="Low"]
  SampleTestLow[,Pred_Rank:=NULL]
  SampleTestHigh<-SampleTest[Pred_Rank=="High"]
  SampleTestHigh[,Pred_Rank:=NULL]
  names(SampleTestLow)<-c("P_Low", "ID")
  names(SampleTestHigh)<-c("P_High", "ID")
  
  PairedSampleTest<-merge(SampleTestHigh, SampleTestLow, by=c("ID"), all=TRUE)
  PairedSampleTest[, PResult:=P_High-P_Low]
  PairedSampleTest[PResult>0, PResultBin:="C"]
  PairedSampleTest[PResult<0, PResultBin:="I"]
  PairedSampleTest[PResult==0, PResultBin:="E"]
  
}

#Now Run the N Random Draw function 100 times
N<-lapply(seq(1,100), function(i){
  
  OutN<-RandomNFun(Data=Spruce, Var1=Spruce$N, Var2=Spruce$Pred_Rank)
  N<-  OutN[, .N , by=NResultBin]
  N[, V:=i]
  
})

NN<-rbindlist(N)  #collapse the lapply output of 100 lists into a datasheet

#Now run the P random draw function 100 times
P<-lapply(seq(1,100), function(i){
  
  OutP<-RandomPFun(Data=Spruce, Var1=Spruce$P, Var2=Spruce$Pred_Rank)  
  P<-  OutP[, .N , by=PResultBin]
  P[, V:=i]
  
})

PP<-rbindlist(P)  #collapse the lapply output of 100 lists into a datasheet

#the mean number of times (out of 100) for each result for both N and P
PP[,mean(N), by=PResultBin]
NN[,mean(N), by=NResultBin]
#Turn N into a percentage
PP[, Percent:=N/100]
NN[, Percent:=N/100]

#Get the 99% percentile of percentages out of the time the "correct" Nitrogen was pulled
VectorN<-NN[NResultBin=="C"]
Nline<-quantile(VectorN$Percent, 0.975)

#Get the 99% percentile of percentages out of the time the "correct" Posphorus was pulled
VectorP<-PP[PResultBin=="C"]
Pline<-quantile(VectorP$Percent, 0.975)


#Cafeteria experiment results, binned into categories Correct, Incorrect, and Equal
DTtrials[Diff > 0, ResultsBin := "C"]
DTtrials[Diff < 0, ResultsBin := "I"]
DTtrials[is.na(ResultsBin), ResultsBin :="E"]

DTtrials[, .N, by=ResultsBin]
#what percent out of the time did hares make the right call?
13/22

              ### S1 ####

#CARBON
C<-ggplot(Spruce)+
  geom_boxplot(aes(x=Pred_Rank, y=C), stat="boxplot", outlier.shape = NA, alpha=1)+
  geom_jitter(aes(x=Pred_Rank, y=C), size=3, width=.3)+
  labs(y="Measured % C", x="Predicted Nutritional Rank", title="A")+
  theme(axis.text=element_text(size=11, color="black"),
        axis.title=element_text(size=14),
        panel.background = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor.y=element_line(color="grey"),
        panel.grid.major.y=element_line(color="grey"))

#NITROGEN
N<-ggplot(Spruce)+
  geom_boxplot(aes(x=Pred_Rank, y=N), stat="boxplot", outlier.shape = NA, alpha=1)+
  geom_jitter(aes(x=Pred_Rank, y=N), size=3, width=.3)+
  labs(y="Measured % N", x="Predicted Nutritional Rank", title="B")+
  theme(axis.text=element_text(size=11, color="black"),
        axis.title=element_text(size=14),
        panel.background = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor.y=element_line(color="grey"),
        panel.grid.major.y=element_line(color="grey"))

#PHOSPHORUS
P<-ggplot(Spruce)+
  geom_boxplot(aes(x=Pred_Rank, y=P), stat="boxplot", outlier.shape = NA, alpha=1)+
  geom_jitter(aes(x=Pred_Rank, y=P), size=3, width=.3)+
  labs(y="Measured % P", x="Predicted Nutritional Rank", title="C")+
  theme(axis.text=element_text(size=11, color="black"),
        axis.title=element_text(size=14),
        panel.background = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor.y=element_line(color="grey"),
        panel.grid.major.y=element_line(color="grey"))

CNP<-ggarrange(C,N,P, ncol=2, nrow=2)
ggsave(filename="Findings/SupInfo1.jpeg", CNP, width = 12, height = 9, units = "in")


        ### S2 ###

lm(P~N, data=Spruce)
ranks<-c("High"=19, "Low"=1)
Corr<-ggplot(Spruce)+
  geom_point(aes(y=P, x=N, shape=Pred_Rank), size=3)+
  geom_abline(intercept = -0.116, slope = 0.231)+
  scale_shape_manual(values = ranks, name="Predicted Rank")+
  labs(y="% Phosphorus", x="% Nitrogen")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "right",
        legend.direction = "vertical")
Corr
ggsave(filename="Findings/SupInfo2.jpeg", Corr, width = 6, height = 4, units = "in")

              ### S3 ####

SamplingShapes<-c("Summer Sample"=21, "Fall Offering"=25)
PredColors<-c("High"="grey15", "Low"="grey70")
FullPlot<-ggplot(data=Full)+
  geom_point(aes(y=P, x=N, shape=Sampling, fill=Pred_Rank), size=2.5)+
  scale_shape_manual(values=SamplingShapes, name="Sample Type")+
  scale_fill_manual(values=PredColors, na.value=NA, guide=FALSE)+
  geom_abline(intercept = 0.01354, slope = 0.1256)+
  #geom_abline(intercept = -0.116, slope = 0.231, color="Red")+
  labs(y="% Phosphorus", x="% Nitrogen")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "right",
        legend.direction = "vertical")
FullPlot
ggsave(filename="Findings/SupInfo3.jpeg", FullPlot)


            ## S4 ###

DensityN<-ggplot()+
  geom_density(aes(N), fill="grey", alpha=.7, data=VectorN)+
  geom_vline(xintercept=59, linetype="dashed")+
  geom_vline(xintercept=Nline*100)+
  ggtitle("Random Draw from N Subsample Results")+
  labs(x="Percentage of draws with correct subsample ranks", y="Density")+
  theme(axis.title=element_text(size=13),
        title = element_text(size=15),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "right",
        legend.direction = "vertical")

DensityP<-ggplot()+
  geom_density(aes(N), fill="grey", alpha=.7, data=VectorP)+
  geom_vline(xintercept=59, linetype="dashed")+
  geom_vline(xintercept=Pline*100)+
  ggtitle("Random Draw from P Subsample Results")+
  labs(x="Percentage of draws with correct subsample ranks", y="Density")+
  theme(axis.title=element_text(size=13),
        title = element_text(size=15),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "right",
        legend.direction = "vertical")


Density<-ggarrange(DensityN, DensityP, ncol=1, nrow=2)
ggsave(filename="Findings/SupInfo4.jpeg", Density, width = 6, height = 8, units = "in")

