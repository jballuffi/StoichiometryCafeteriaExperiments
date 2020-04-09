libs<-c('dplyr', 'data.table','sf', 'rgdal','raster','sp', 'ggplot2','RColorBrewer', 'ggpubr', 'effects')
lapply(libs, require, character.only = TRUE)
utm21N <- '+proj=longlat +zone=21 ellps=WGS84'

           ### Manuscript Figures ###

### IMPORTING ###

DTpiles<-readRDS("Input/pile_format.rds")
DTtrials<-readRDS("Input/trial_format.rds")
DTtraps<-readRDS("Input/all_trap_locs.rds")
GridBuffer<-st_read("Input/GridPts_Buffer100m.shp")
effsC<-readRDS("Input/effects_coat.rds")
effsT<-readRDS("Input/effects_temp.rds")

### Subsetting feeding exepriments into habitated and non-habituated ###
DTpiles1<-DTpiles[Trial==1]
DTtrials1<-DTtrials[Trial==1]

DTpiles2<-DTpiles[!Trial==1]
DTtrials2<-DTtrials[!Trial==1]


### Creating effects for temperature and coat colour figures
TempMod<-lm(Diff_IR~Low_temp, data=DTtrials)
effsT2<-as.data.table(effect(c("Low_temp"), xlevels=10, TempMod))
Whitemod<-lm(Diff_IR~White, data=DTtrials)
effsC2 <- as.data.table(effect(c("White"), xlevels=10, Whitemod))


### Import raster layers for N and P and project correctly
bloomN<-raster("Input/PIMA_N.tif")
bloomP<-raster("Input/PIMA_P.tif")
bloomN<-projectRaster(bloomN,crs=utm21N)
bloomP<-projectRaster(bloomP,crs=utm21N)
bloomN_points <- rasterToPoints(bloomN)
DFbloomN <- data.frame(bloomN_points)
bloomP_points <- rasterToPoints(bloomP)
DFbloomP <- data.frame(bloomP_points)


  #### FIGURE 1 ####

TrapShapes<-c("Sampled"=16, "Interpolated"=9, "Offered"=8)


Nmap<-ggplot(data=DFbloomN) + 
  geom_raster(aes(x=x, y=y, fill=PIMA_N))+
  scale_fill_gradient(high="darkgreen", low="yellow", name="% N")+
  ggtitle("A")+
  geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
  scale_shape_manual(values=TrapShapes, guide=FALSE)+
  theme(axis.text= element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.key.size = unit(.75, "line"),
        legend.key = element_blank(),
        legend.position = "right")

Pmap<-ggplot(data=DFbloomP) + 
  geom_raster(aes(x=x, y=y, fill=PIMA_P))+
  scale_fill_gradient(high="darkgreen", low="yellow", name="% P")+
  geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
  scale_shape_manual(values=TrapShapes, guide=FALSE)+
  ggtitle("B")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.75, "line"),
        legend.position = "right")

NPscatter<-ggplot(data=DTtraps)+
  geom_point(aes(y=P, x=N, shape=Sampling), size=3)+
  scale_shape_manual(values=TrapShapes, name="Spruce Status")+
  geom_abline(intercept = 0.01354, slope = 0.1256)+
  geom_text(aes(.85, .2, label="y = 0.13x + 0.014"), size=4)+
  geom_text(aes(.8, .19, label="R2 = 0.43"), size=4)+
  labs(y="% Phosphorus", x="% Nitrogen")+
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

Fig1<-ggarrange(Nmap, Pmap, NPscatter, ncol=1, nrow=3)
ggsave(filename="Findings/Figure1.jpeg", Fig1, width = 4.75, height = 8.5, units = "in")


    #### Figure 2 = diagrams made in powerpoint

    #### FIGURE 3 ####

#if you want a path plot, use ID_Year, not Eartag.
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

Pref<-ggplot(data=DTtrials)+
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



                  #### Figure 4 ####

#Boxplot showing the total trend
boxplot<-ggplot(data=DTpiles, aes(y=IR, x=Treatment))+
  geom_boxplot(position="dodge", notch=FALSE)+ 
  geom_jitter(width=.25, size=3)+
  labs(y="Intake Rate (g/kg/day)", x="Nutritional Rank")+
  #ggtitle("A) Non-habituated Experiments")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        strip.background = element_blank(),   #for facet wrap
        strip.text = element_text(size=12),   #for facet wrap
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_line(color="grey"),
        panel.grid.major.y = element_line(color="grey"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

Fig4<- boxplot + facet_wrap(~Habituation, ncol = 2)
Fig4
ggsave(filename="Findings/Figure4.jpeg", Fig4, width = 7.7, height = 4.3, units = "in")


                            #### Figure 5 ####

#Total prop vs. percent white
RankShape<-c("High"=19, "Low"=1 )

#Uses line from Energetic model
IRWhite<-ggplot()+
  geom_point(aes(y=IR, x=White, shape=Treatment),data=DTpiles1, color="grey20", size=4)+
  geom_ribbon(aes(x=White, ymin=lower, ymax=upper), data=effsC, colour="grey80", alpha=.4 )+
  geom_abline(intercept = 47.32, slope = -27.6099, size=1.2, colour="grey20")+
  #geom_text(aes(.25, 0, label="y = -31.12x + 71.307"), size=5)+
  scale_shape_manual(values=RankShape, name="Nutrient Rank", guide=FALSE)+
  labs(y="Intake Rate (g/kg/day)", x=" ")+
  ggtitle("A")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#line from energetic model
IRTemp<-ggplot()+
  geom_point(aes(y=IR, x=Low_temp, shape=Treatment),data=DTpiles1, color="grey20", size=4)+
  geom_ribbon(aes(x=Low_temp, ymin=lower, ymax=upper), data=effsT, colour="grey80", alpha=.4 )+
  geom_abline(intercept = 47.32, slope = -2.7633, size=1.2, colour="grey20")+
  #geom_text(aes(.25, 0, label="y = -31.12x + 71.307"), size=5)+
  scale_shape_manual(values=RankShape, name="Nutrient Rank", guide=FALSE)+
  labs(y=" ", x=" ")+
  ggtitle("B")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#line from new model
DiffWhite<-ggplot(data=DTtrials1)+
  geom_ribbon(aes(x=White, ymin=lower, ymax=upper), data=effsC2, colour="grey80", alpha=.3 )+
  geom_abline(aes(intercept=-0.3709, slope=17.19), size=1.2, color="grey20")+
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
DiffTemp<-ggplot(data=DTtrials1)+
  geom_ribbon(aes(x=Low_temp, ymin=lower, ymax=upper), data=effsT2, colour="grey80", alpha=.3 )+
  geom_abline(aes(intercept=2.75, slope=.506), size=1.2, colour="grey20")+
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



