libs<-c('dplyr', 'data.table','sf', 'rgdal','raster','sp', 'ggplot2','RColorBrewer', 'ggpubr', 'effects')
lapply(libs, require, character.only = TRUE)
utm21N <- '+proj=longlat +zone=21 ellps=WGS84'

      
           ### Manuscript Figures ###


### IMPORTING ###

DTpiles<-readRDS("Input/pile_format.rds")
DTtrials<-readRDS("Input/trial_format.rds")



            ### Figure 3 ####

#if you want a path plot, need to create a ID-Year column. A2081 has 2 trial 2s.
Intake<-ggplot(data=DTtrials)+
  geom_boxplot(position="dodge", aes(y=IR, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=IR, x=Trial), width=.25, size=3)+
  labs(y="Intake Rate (g/kg/day)", x=" ")+
  ggtitle("A")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

ggplot(data=DTtrials)+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  geom_boxplot(position="dodge", aes(y=Diff_IR, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=Diff_IR, x=Trial), width=.25, size=3)+
  labs(y="Preference for High Ranked Spruce", x="Trial Number")+
  ggtitle("B")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

Fig3<-ggarrange(Intake, Pref,  ncol=1, nrow=2, label.x = 3)
ggsave(filename="Findings/Figure3.jpeg", Fig3, width = 5.5, height = 9.5, units = "in")




DTpiles<-DTpiles[!Trial==1]
DTtrials<-DTtrials[!Trial==1]



                  #### Figure 6 ####

#Boxplot showing the total trend for habituated hares
boxplot<-ggplot(data=DTpiles, aes(y=IR, x=Treatment))+
  geom_boxplot(position="dodge", notch=FALSE)+ 
  geom_jitter(width=.25, size=3)+
  scale_fill_manual(values=cols, guide=FALSE)+ #this is where the manual colors come in
  labs(y="Intake Rate (g/kg/day)", x="Nutritional Rank")+
  ggtitle("A")+
  theme(axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=12),
        axis.text.x = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
boxplot

#path plot that matches boxplot
pathplot<-ggplot(data=DTpiles)+
  geom_path(aes(y=IR, x=Treatment, group=sampleID), size=1)+
  labs(y="Intake Rate (g/kg/day)", x="Nutritional Rank")+
  ggtitle("B")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
pathplot

Fig6<-ggarrange(boxplot, pathplot,  ncol=2, nrow=1, label.x = 3)
ggsave(filename="Findings/Figure6.jpeg", Fig6, width = 7.7, height = 4.3, units = "in")


                #### Figure 4 ####

#Total prop vs. percent white
RankShape<-c("High"=19, "Low"=1 )

#Uses line from Coat model
ggplot()+
  geom_point(aes(y=IR, x=White, shape=Treatment),data=DTpiles, color="grey20", size=4)+
  #geom_ribbon(aes(x=N, ymin=lower, ymax=upper), data=effsO, colour="grey80", alpha=.4 )+
  geom_abline(intercept = 39.9, slope = 6.89, size=1.2, colour="grey20")+
  #geom_text(aes(.25, 0, label="y = -31.12x + 71.307"), size=5)+
  scale_shape_manual(values=RankShape, name="Nutrient Rank", guide=FALSE)+
  labs(y="Intake Rate (g/kg/day)", x="Percent White")+
  ggtitle("A")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#line from temp model
ggplot()+
  geom_point(aes(y=IR, x=Low_temp, shape=Treatment),data=DTpiles, color="grey20", size=4)+
  #geom_ribbon(aes(x=N, ymin=lower, ymax=upper), data=effsO, colour="grey80", alpha=.4 )+
  geom_abline(intercept = 42.2, slope = -.43, size=1.2, colour="grey20")+
  #geom_text(aes(.25, 0, label="y = -31.12x + 71.307"), size=5)+
  scale_shape_manual(values=RankShape, name="Nutrient Rank", guide=FALSE)+
  labs(y="Intake Rate (g/kg/day)", x="Low Ambient Temperature (C)")+
  ggtitle("B")+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

Fig4<-ggarrange(IRWhite, IRTemp,  ncol=2, nrow=1, label.x = 3)
ggsave(filename="Findings/Figure4.jpeg", Fig4, width = 9.3, height = 4.3, units = "in")






                   ### Figure 5 ####

Whitemod<-lm(Diff_IR~White, data=DTtrials)
summary(Whitemod)
effsWhite <- as.data.table(effect(c("White"), xlevels=10, Whitemod))

DiffWhite<-ggplot(data=DTtrials)+
  geom_ribbon(aes(x=White, ymin=lower, ymax=upper), data=effsWhite, colour="grey80", alpha=.3 )+
  geom_abline(aes(intercept=4.19, slope=5.99), size=1.2, color="grey20")+
  geom_point(aes(y=Diff_IR, x=White), size=4, colour="grey20")+
  #geom_text(aes(.89, -45, label="y = -146.7x + 154.6"), size=5)+
  labs(y="Preference for High Ranked Spruce", x="Percent White")+
  ggtitle("A")+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
DiffWhite

TempMod<-lm(Diff_IR~Low_temp, data=DTtrials)
summary(TempMod)
effsT<-as.data.table(effect(c("Low_temp"), xlevels=10, TempMod))

DiffTemp<-ggplot(data=DTtrials)+
  geom_ribbon(aes(x=Low_temp, ymin=lower, ymax=upper), data=effsT, colour="grey80", alpha=.3 )+
  geom_abline(aes(intercept=5.9, slope=-1.3), size=1.2, colour="grey20")+
  geom_point(aes(y=Diff_IR, x=Low_temp), size=4, colour="grey20")+
  #geom_text(aes(1.4, -40, label="y = 6.17x + 17"), size=5)+
  labs(y="Preference for High Ranked Spruce", x="Low Ambient Temperature (C)")+
  ggtitle("B")+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
DiffTemp

Fig5<-ggarrange(DiffWhite, DiffTemp,  ncol=2, nrow=1, label.x = 3)
ggsave(filename="Findings/Figure5.jpeg", Fig5, width = 9.3, height = 4.3, units = "in")










#Figure 6
ggplot(data=DTtrials)+
  geom_point(aes(y=Diff, x=TotalEaten), size=4, colour="grey20")+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))




