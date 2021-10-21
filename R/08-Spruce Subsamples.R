library(ggplot2)
library(data.table)
library(ggpubr)
library(patchwork)

#import data
Spruce<-as.data.table(read.csv("Input/Spruce_subsamples_2019.csv"))
DTtraps<-readRDS("Input/all_trap_locs.rds")

themeblank <- theme(axis.title = element_text(size=10),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8),
                    legend.key = element_blank(),
                    panel.background = element_blank(),
                    axis.line.y.left = element_line(color="black", size = .5),
                    axis.line.x.bottom = element_line(color="black", size = .5),
                    #panel.border = element_rect(colour = "black", fill=NA, size=1),
                    legend.position = "right",
                    legend.direction = "vertical",
                    legend.text = element_text(size=9),
                    legend.title = element_text(size=11))


#set up spruce datatable
Spruce$Pred_rank<-factor(Spruce$Pred_rank, levels=c("Low", "High"))  #set predicted rank with levels
setnames(Spruce, "Total.N", "N")   #Changing names "old", "new"
setnames(Spruce, "TC", "C")         #Changing names "old", "new"
Spruce[, Sampling:="Offering subsample"]     #making new column for merge with the trap dt

#set up trap datatable
DTtraps<-DTtraps[Sampling=="Offered"]    #keeping only the 6 locations where subsamples were taken
DTtraps[,Sampling:="Original sample"]   #giving new value for later merge with the Spruce dt
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



              ### Appendix 3 ####
#show just the subsample data
LocCols<-c("I2"="forest green", "I4"="darkolivegreen3", "J1"="blue4", 
           "L4"="yellow3", "L6"="orange2", "N5"="darkgoldenrod4")

#CARBON
C1<-ggplot(Spruce)+
  geom_boxplot(aes(x=Pred_rank, y=C), stat="boxplot", outlier.shape = NA, alpha=1)+
  geom_jitter(aes(x=Pred_rank, y=C, colour=SampleLoc), size=4, width=.3)+
  scale_color_manual(values=LocCols, name="Site")+
  labs(x="Predicted Quality", subtitle = "(a) % Carbon")+
  themeblank

#NITROGEN
N1<-ggplot(Spruce)+
  geom_boxplot(aes(x=Pred_rank, y=N), stat="boxplot", outlier.shape = NA, alpha=1)+
  geom_jitter(aes(x=Pred_rank, y=N, colour=SampleLoc), size=4, width=.3)+
  scale_color_manual(values=LocCols, name="Site")+
  labs(x="Predicted Quality", subtitle = "(b) % Nitrogen")+
  themeblank

#PHOSPHORUS
P1<-ggplot(Spruce)+
  geom_boxplot(aes(x=Pred_rank, y=P), stat="boxplot", outlier.shape = NA, alpha=1)+
  geom_jitter(aes(x=Pred_rank, y=P, colour=SampleLoc), size=4, width=.3)+
  scale_color_manual(values=LocCols, name="Site")+
  labs(x="Predicted Quality", subtitle = "(c) % Phosphorus")+
  themeblank

(CNP1<-(C1 + N1 + P1 + plot_layout(guides = 'collect')))


                    ### Appendix 4 ####
#show the subsample data compared to original samples

rankcols<- c("High"="grey70", "Low"="white")

#CARBON
C2<-ggplot(Full)+
  geom_boxplot(aes(x=Sampling, y=C, fill=Pred_rank), stat="boxplot", outlier.shape = NA, alpha=1)+
  scale_fill_manual(values=rankcols, name="Predicted Quality")+
  labs(subtitle="(a) % Carbon")+
  themeblank

#NITROGEN
N2<-ggplot(Full)+
  geom_boxplot(aes(x=Sampling, y=N, fill=Pred_rank), stat="boxplot", outlier.shape = NA, alpha=1)+
  scale_fill_manual(values=rankcols, name="Predicted Quality")+
  labs(subtitle="(b) % Nitrogen")+
  themeblank

#PHOSPHORUS
P2<-ggplot(Full)+
  geom_boxplot(aes(x=Sampling, y=P, fill=Pred_rank), stat="boxplot", outlier.shape = NA, alpha=1)+
  scale_fill_manual(values=rankcols, name="Predicted Quality")+
  labs(subtitle="(c) % Phosphorus")+
  themeblank

(CNP2<-C2 + N2 + P2 + plot_layout(guides = 'collect'))



#### Save figures ####

ggsave(filename="Output/FigureA3.jpeg", CNP1, width = 12, height = 5, units = "in")

ggsave(filename="Output/FigureA4.jpeg", CNP2, width = 12, height = 5, units = "in")
