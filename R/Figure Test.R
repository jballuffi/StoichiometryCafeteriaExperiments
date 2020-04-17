libs<-c('dplyr', 'data.table','sf', 'rgdal','raster','sp', 'ggplot2','RColorBrewer', 'ggpubr', 'effects')
lapply(libs, require, character.only = TRUE)
utm21N <- '+proj=longlat +zone=21 ellps=WGS84'

      
           ### Manuscript Figures ###


### IMPORTING ###

DTpiles<-readRDS("Input/pile_format.rds")
DTtrials<-readRDS("Input/trial_format.rds")


lmMC<-lm(Mass_change~Diff_IR, data=DTtrials)
Weightloss<-ggplot(DTtrials)+
  geom_point(aes(y=Mass_change, x=Diff_IR),  size=3, colour="grey20")+
  geom_abline(intercept = (coef(lmMC)["(Intercept)"]), slope = (coef(lmMC)["Diff_IR"]), size=1.2)+
  geom_vline(xintercept=0, size=1.2, color="grey40", linetype="dashed")+
  labs(x="Preference for high ranked spruce", y="Weight lost during trial (%)")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
ggsave(filename="Findings/Weightloss.jpeg", Weightloss, width = 6, height = 4, units = "in")





#Boxplot showing the total trend
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

ggarrange(boxplot, pathplot,  ncol=2, nrow=1, label.x = 3)

                            #### Figure 5 ####

#Total prop vs. percent white
RankShape<-c("High"=19, "Low"=1 )

#Uses line from Energetic model
ggplot()+
  geom_point(aes(y=IR, x=White, colour=Habituation),data=DTtrials, size=4)+
  #geom_text(aes(.25, 0, label="y = -31.12x + 71.307"), size=5)+
  labs(y="Intake Rate (g/kg/day)", x=" ")+
  ggtitle("A")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))


#line from energetic model
ggplot()+
  geom_point(aes(y=IR, x=Low_temp),data=DTtrials, color="grey20", size=4)+
  #geom_text(aes(.25, 0, label="y = -31.12x + 71.307"), size=5)+
  #scale_shape_manual(values=RankShape, name="Nutrient Rank", guide=FALSE)+
  labs(y=" ", x=" ")+
  ggtitle("B")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#line from new model
ggplot(data=DTtrials)+
  geom_point(aes(y=Diff_IR, x=White), size=4, colour="grey20")+
  #geom_text(aes(.89, -45, label="y = -146.7x + 154.6"), size=5)+
  labs(y="Preference for High Ranked Spruce", x="Percent White")+
  ggtitle("C")+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#line from new model
ggplot(data=DTtrials)+
  geom_point(aes(y=Diff_IR, x=Low_temp), size=4, colour="grey20")+
  #geom_text(aes(1.4, -40, label="y = 6.17x + 17"), size=5)+
  labs(y=" ", x="Low Ambient Temperature (C)")+
  ggtitle("D")+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))


Fig5<-ggarrange(IRWhite, IRTemp, DiffWhite, DiffTemp, ncol = 2, nrow = 2)

ggsave(filename="Findings/Figure5.jpeg", Fig5, width = 9.3, height = 9.3, units = "in")


              ### Additional figure of Total consumption vs. preference
ggplot(data=DTtrials)+
  geom_point(aes(y=Diff_IR, x=IR), size=4, colour="grey20")+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))






              ##### Figure A1 ######

DBHN<-ggplot(data=DTtraps)+
  geom_point(aes(y=N, x=AvgDBH), size = 3)+
  geom_text(aes(5, 1.35, label="p = 0.36"), size=5)+
  labs(x=NULL, y="% Nitrogen")+
  ggtitle("A")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.text = element_text(size=9),
        legend.title = element_text(size=11))

CanopyN<-ggplot(data=DTtraps)+
  geom_point(aes(y=N, x=CanopyClosure), size = 3)+
  geom_abline(aes(intercept=.637, slope=.00336), size=1.2, colour="grey20")+
  geom_text(aes(30, 1.35, label="y = 0.00336x + 0.637"), size=5)+
  geom_text(aes(25, 1.25, label="t = 3.09, p < 0.01"), size=5)+
  labs(x=NULL, y=NULL)+
  ggtitle("B")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.text = element_text(size=9),
        legend.title = element_text(size=11))

DBHP<-ggplot(data=DTtraps)+
  geom_point(aes(y=P, x=AvgDBH), size = 3)+
  geom_text(aes(5, .2, label="p = 0.69"), size=5)+
  labs(x="Mean DBH (cm)", y="% Phosphorus")+
  ggtitle("C")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.text = element_text(size=9),
        legend.title = element_text(size=11))

CanopyP<-ggplot(data=DTtraps)+
  geom_point(aes(y=P, x=CanopyClosure), size = 3)+
  geom_abline(aes(intercept=0.098, slope=0.00068), size=1.2, colour="grey20")+
  geom_text(aes(33, .2, label="y = 0.098x + 0.000682"), size=5)+
  geom_text(aes(26, .185, label="t = 3.041, p < 0.01"), size=5)+
  labs(x="Canopy Closure (%)", y=NULL)+
  ggtitle("D")+
  theme(axis.title=element_text(size=14),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = "right",
        legend.direction = "vertical",
        legend.text = element_text(size=9),
        legend.title = element_text(size=11))

FigA1<-ggarrange(DBHN, CanopyN, DBHP, CanopyP, ncol=2, nrow=2)
ggsave(filename="Findings/FigureA1.jpeg", FigA1, width = 8, height = 7, units = "in")



