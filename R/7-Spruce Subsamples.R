library(ggplot2)
library(data.table)
library(ggpubr)

#import data
Spruce<-as.data.table(read.csv("Input/Spruce_subsamples_2019.csv"))
DTtraps<-readRDS("Input/all_trap_locs.rds")

#set up spruce datatable
Spruce$Pred_rank<-factor(Spruce$Pred_rank, levels=c("Low", "High"))  #set predicted rank with levels
setnames(Spruce, "Total.N", "N")   #Changing names "old", "new"
setnames(Spruce, "TC", "C")         #Changing names "old", "new"
Spruce[, Sampling:="Fall subsample"]     #making new column for merge with the trap dt

#set up trap datatable
DTtraps<-DTtraps[Sampling=="Offered"]    #keeping only the 6 locations where subsamples were taken
DTtraps[,Sampling:="Original clipping"]   #giving new value for later merge with the Spruce dt
DTtraps$Spruce<-NULL
setnames(DTtraps, "TC", "C")               #renaming
setnames(DTtraps, "Rank", "Pred_rank")     #renaming
Spruce$Pred_rank<-factor(Spruce$Pred_rank, levels=c("Low", "High"))    #giving ranks levels

#Combine the spruce and trap datatables for future figure S3
Full<- rbindlist(list(DTtraps, Spruce), fill = TRUE, use.names = TRUE)

#Summary Statistics for subsampling
Spruce[, median(C), by=Pred_rank]
Spruce[, sd(C), by=Pred_rank]
Spruce[, median(N), by= Pred_rank]
Spruce[, sd(N), by= Pred_rank]
Spruce[, median(P), by=Pred_rank]
Spruce[, sd(P), by=Pred_rank]

#stats on difference in rank
summary(lm(N~Pred_rank, data=Spruce))
summary(lm(P~Pred_rank, data=Spruce))
summary(lm(C~Pred_rank, data=Spruce))
cor(Spruce$P, Spruce$N)



              ### Appendix 2 ####

#CARBON
C1<-ggplot(Spruce)+
  geom_boxplot(aes(x=Pred_rank, y=C), stat="boxplot", outlier.shape = NA, alpha=1)+
  geom_jitter(aes(x=Pred_rank, y=C), size=3, width=.3)+
  labs(y="Measured % C", x="Predicted Nutritional Rank", title="A")+
  theme(axis.text=element_text(size=11, color="black"),
        axis.title=element_text(size=14),
        panel.background = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor.y=element_line(color="grey"),
        panel.grid.major.y=element_line(color="grey"))

#NITROGEN
N1<-ggplot(Spruce)+
  geom_boxplot(aes(x=Pred_rank, y=N), stat="boxplot", outlier.shape = NA, alpha=1)+
  geom_jitter(aes(x=Pred_rank, y=N), size=3, width=.3)+
  labs(y="Measured % N", x="Predicted Nutritional Rank", title="B")+
  theme(axis.text=element_text(size=11, color="black"),
        axis.title=element_text(size=14),
        panel.background = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor.y=element_line(color="grey"),
        panel.grid.major.y=element_line(color="grey"))

#PHOSPHORUS
P1<-ggplot(Spruce)+
  geom_boxplot(aes(x=Pred_rank, y=P), stat="boxplot", outlier.shape = NA, alpha=1)+
  geom_jitter(aes(x=Pred_rank, y=P), size=3, width=.3)+
  labs(y="Measured % P", x="Predicted Nutritional Rank", title="C")+
  theme(axis.text=element_text(size=11, color="black"),
        axis.title=element_text(size=14),
        panel.background = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor.y=element_line(color="grey"),
        panel.grid.major.y=element_line(color="grey"))

CNP1<-ggarrange(C1,N1,P1, ncol=2, nrow=2)
ggsave(filename="Findings/FigureA2.jpeg", CNP1, width = 12, height = 9, units = "in")


                    ### Appendix 3 ####

rankcols<- c("High"="grey70", "Low"="white")

#CARBON
C2<-ggplot(Full)+
  geom_boxplot(aes(x=Sampling, y=C, fill=Pred_rank), stat="boxplot", outlier.shape = NA, alpha=1)+
  scale_fill_manual(values=rankcols, guide=FALSE)+
  labs(y="Measured % C", title="A")+
  theme(axis.text=element_text(size=11, color="black"),
        axis.title.y=element_text(size=14),
        axis.title.x=element_blank(),
        panel.background = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor.y=element_line(color="grey"),
        panel.grid.major.y=element_line(color="grey"))

#NITROGEN
N2<-ggplot(Full)+
  geom_boxplot(aes(x=Sampling, y=N, fill=Pred_rank), stat="boxplot", outlier.shape = NA, alpha=1)+
  scale_fill_manual(values=rankcols, guide=FALSE)+
  labs(y="Measured % N", title="B")+
  theme(axis.text=element_text(size=11, color="black"),
        axis.title.y=element_text(size=14),
        axis.title.x=element_blank(),
        panel.background = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor.y=element_line(color="grey"),
        panel.grid.major.y=element_line(color="grey"))

#PHOSPHORUS
P2<-ggplot(Full)+
  geom_boxplot(aes(x=Sampling, y=P, fill=Pred_rank), stat="boxplot", outlier.shape = NA, alpha=1)+
  scale_fill_manual(values=rankcols, guide=FALSE)+
  labs(y="Measured % P", title="C")+
  theme(axis.text=element_text(size=11, color="black"),
        axis.title.y=element_text(size=14),
        axis.title.x=element_blank(),
        panel.background = element_blank(),
        legend.key = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor.y=element_line(color="grey"),
        panel.grid.major.y=element_line(color="grey"))

CNP2<-ggarrange(C2, N2, P2, ncol=3, nrow=1)
ggsave(filename="Findings/FigureA3.jpeg", CNP2, width = 12, height = 5, units = "in")




                        ### Figure A4 ###

#make linear regression
lmNP<-lm(P~N, data=Spruce)
ranks<-c("High"=19, "Low"=1) #making shapes

#plot that takes coef and intercepts directly from linear regression
Corr<-ggplot(Spruce)+
  geom_point(aes(y=P, x=N, shape=Pred_rank), size=3)+
  geom_abline(intercept = (coef(lmNP)["(Intercept)"]), slope = (coef(lmNP)["N"]))+
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
ggsave(filename="Findings/FigureA4.jpeg", Corr, width = 6, height = 4, units = "in")

