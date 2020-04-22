libs<-c('dplyr', 'data.table','sf', 'rgdal','raster','sp', 'ggplot2','RColorBrewer', 'ggpubr', 'effects', 'patchwork')
lapply(libs, require, character.only = TRUE)
utm21N <- '+proj=longlat +zone=21 ellps=WGS84'


### Plant Figures ###


##IMPORTING

#trap datatab;e and shapefile of grid outline
DTtraps<-readRDS("Input/all_trap_locs.rds")
GridBuffer<-st_read("Input/GridPts_Buffer100m.shp")
#subsetting trap data for visualizing only sampled locations
DTtraps2<-DTtraps[!Sampling=="Interpolated"]

# Import rasters that were previously converted to datatables
gridN<-readRDS("Input/Bloomfield_N.rds")
gridP<-readRDS("Input/Bloomfield_P.rds")
gridPSC<-readRDS("Input/Bloomfield_PSC.rds")



#### FIGURE 1 ####

TrapShapes<-c("Sampled"=16, "Interpolated"=9, "Offered"=8)

(Nmap<-ggplot(data=gridN) + 
    geom_raster(aes(x=x, y=y, fill=N))+
    scale_fill_gradient(high="darkgreen", low="yellow", name="% N")+
    ggtitle("A) Nitrogen")+
    geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
    scale_shape_manual(values=TrapShapes, name="Site Status")+
    theme(axis.text= element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.key.size = unit(.75, "line"),
          legend.key = element_blank(),
          legend.position = "right"))

(Pmap<-ggplot(data=gridP) + 
    geom_raster(aes(x=x, y=y, fill=P))+
    scale_fill_gradient(high="darkgreen", low="yellow", name="% P")+
    geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
    scale_shape_manual(values=TrapShapes, name="Site Status")+
    ggtitle("B) Phosphorus")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(.75, "line"),
          legend.position = "right"))

(PSCmap<-ggplot(data=gridPSC) + 
    geom_raster(aes(x=x, y=y, fill=PSC))+
    scale_fill_gradient(high="yellow", low="darkgreen", name="Total PSC")+
    geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
    scale_shape_manual(values=TrapShapes, name="Site Status")+
    ggtitle("C) Plant Secondary Compounds")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(.75, "line"),
          legend.position = "right"))

Fig1 <- Nmap / Pmap / PSCmap + plot_layout(guides = 'collect')
ggsave(filename="Findings/Figure1.jpeg", Fig1, width = 4.75, height = 8.5, units = "in")


#### FIGURE 2 ####

TrapShapes2<-c("Sampled"=16, "Offered"=8)

(NPscatter<-ggplot(data=DTtraps2)+
    geom_point(aes(y=P, x=N, shape=Sampling), size=3)+
    scale_shape_manual(values=TrapShapes2, name="Site Status")+
    geom_abline(intercept = 0.01354, slope = 0.1256)+
    geom_text(aes(.85, .2, label="y = 0.13x + 0.014"), size=4)+
    geom_text(aes(.8, .19, label="R2 = 0.43"), size=4)+
    labs(y="% Phosphorus", x="% Nitrogen")+
    ggtitle("A")+
    theme(axis.title=element_text(size=14),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=11)))

(NPSCscatter<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=N, shape=Sampling), size=3)+
    scale_shape_manual(values=TrapShapes2, name="Site Status")+
    geom_abline(intercept = 24.761, slope = -7.56)+
    geom_text(aes(1.27, 25, label="y = -7.6x + 24.8"), size=4)+
    geom_text(aes(1.27, 24, label="R2 = 0.20"), size=4)+
    labs(y="PSC", x="% Nitrogen")+
    ggtitle("B")+
    theme(axis.title=element_text(size=14),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=11)))

(PPSCscatter<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=P, shape=Sampling), size=3)+
    scale_shape_manual(values=TrapShapes, name="Site Status")+
    geom_abline(intercept = 21.74, slope = -32.95)+
    geom_text(aes(.115, 25, label="y = -33.0x + 21.7"), size=4)+
    geom_text(aes(.115, 24, label="R2 = 0.14"), size=4)+
    labs(y="PSC", x="% Phosphorus")+
    ggtitle("C")+
    theme(axis.title=element_text(size=14),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=11)))

Fig2 <- NPscatter / NPSCscatter / PPSCscatter + plot_layout(guides = 'collect')
ggsave(filename="Findings/Figure2.jpeg", Fig2, width = 4.75, height = 8.5, units = "in")



#### FIGURE 3 ####

(DBHN<-ggplot(data=DTtraps2)+
   geom_point(aes(y=N, x=AvgDBH), size = 3)+
   geom_text(aes(5, 1.35, label="p = 0.36"), size=5)+
   labs(x=NULL, y="% Nitrogen")+
   ggtitle("A")+
   theme(axis.title=element_text(size=14),
         axis.text.x = element_text(size=8),
         axis.text.y = element_text(size=8),
         legend.key = element_blank(),
         panel.background = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, size=1),
         legend.position = "right",
         legend.direction = "vertical",
         legend.text = element_text(size=9),
         legend.title = element_text(size=11)))

(CanopyN<-ggplot(data=DTtraps2)+
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
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=11)))

(DBHP<-ggplot(data=DTtraps2)+
    geom_point(aes(y=P, x=AvgDBH), size = 3)+
    geom_text(aes(5, .2, label="p = 0.69"), size=5)+
    labs(x="Mean DBH (cm)", y="% Phosphorus")+
    ggtitle("C")+
    theme(axis.title=element_text(size=14),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=11)))

(CanopyP<-ggplot(data=DTtraps2)+
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
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=11)))

(DBHPSC<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=AvgDBH), size = 3)+
    geom_text(aes(12, 25.2, label="y = -0.42x + 24.4"), size=5)+
    geom_text(aes(12, 24, label="t = -1.85, p = 0.07"), size=5)+
    geom_abline(aes(intercept=24.38, slope=-0.42), size=1.2, colour="grey20")+
    labs(x="Mean DBH (cm)", y="PSC")+
    ggtitle("E")+
    theme(axis.title=element_text(size=14),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=11)))

(CanopyPSC<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=CanopyClosure), size = 3)+
    geom_abline(aes(intercept=24.38, slope=-0.045), size=1.2, colour="grey20")+
    geom_text(aes(24, 14,  label="y = 0.098x + 0.000682"), size=5)+
    geom_text(aes(24, 13, label="t = -2.44, p =0.02"), size=5)+
    labs(x="Canopy Closure (%)", y=NULL)+
    ggtitle("F")+
    theme(axis.title=element_text(size=14),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=11)))


Fig3<-ggarrange(DBHN, CanopyN, DBHP, CanopyP, DBHPSC, CanopyPSC, ncol=2, nrow=3)
ggsave(filename="Findings/Figure3.jpeg", FigA1, width = 8, height = 7, units = "in")

