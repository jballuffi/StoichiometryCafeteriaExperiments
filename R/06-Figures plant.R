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

#blanktheme
themeblank <- theme(axis.title = element_text(size=14),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8),
                    legend.key = element_blank(),
                    panel.background = element_blank(),
                    panel.border = element_rect(colour = "black", fill=NA, size=1),
                    legend.position = "right",
                    legend.direction = "vertical",
                    legend.text = element_text(size=9),
                    legend.title = element_text(size=11))

#### FIGURE 1 ####

TrapShapes<-c("Sampled"=16, "Interpolated"=9, "Offered"=8)
thememap <- theme(axis.text= element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  legend.key.size = unit(.75, "line"),
                  legend.key = element_blank(),
                  legend.position = "right")

(Nmap<-ggplot(data=gridN) + 
    geom_raster(aes(x=x, y=y, fill=N))+
    scale_fill_gradient(high="darkgreen", low="yellow", name="N (%)              ")+
    ggtitle("A) Nitrogen")+
    geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
    scale_shape_manual(values=TrapShapes, name="Site Status")+
    thememap)

(Pmap<-ggplot(data=gridP) + 
    geom_raster(aes(x=x, y=y, fill=P))+
    scale_fill_gradient(high="darkgreen", low="yellow", name="P (%)              ")+
    geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
    scale_shape_manual(values=TrapShapes, name="Site Status")+
    ggtitle("B) Phosphorus")+
    thememap)

(PSCmap<-ggplot(data=gridPSC) + 
    geom_raster(aes(x=x, y=y, fill=PSC))+
    scale_fill_gradient(high="yellow", low="darkgreen", name="Terpene (mg/g)")+
    geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
    scale_shape_manual(values=TrapShapes, name="Site Status")+
    ggtitle("C) Plant Secondary Compounds")+
    thememap)

(Fig1 <- Nmap / Pmap / PSCmap + plot_layout(guides = 'collect'))
ggsave(filename="Findings/Figure1.jpeg", Fig1, width = 4.75, height = 8.5, units = "in")


#### FIGURE 2 ####
qualcols<-c("High" = "Forestgreen", "Low" = "Yellow3", "None" = "Black")
TrapShapes2<-c("Sampled"=16, "Offered"=8)

(NPscatter<-ggplot(data=DTtraps2)+
    geom_point(aes(y=P, x=N, shape=Sampling, colour=Rank), size=3)+
    scale_shape_manual(values=TrapShapes2, name="Site Status   ")+
    scale_color_manual(values=qualcols, name="Quality Choice")+
    geom_abline(intercept = 0.01354, slope = 0.1256)+
    geom_text(aes(.85, .2, label="y = 0.13x + 0.014"), size=4)+
    geom_text(aes(.8, .19, label="R2 = 0.43"), size=4)+
    labs(y="% Phosphorus", x="% Nitrogen")+
    ggtitle("A")+
    themeblank)

(NPSCscatter<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=N, shape=Sampling, colour=Rank), size=3)+
    scale_shape_manual(values=TrapShapes2, name="Site Status   ")+
    scale_color_manual(values=qualcols, name="Quality Choice")+
    geom_abline(intercept = 28.09, slope = -10.74)+
    geom_text(aes(1.27, 30, label="y = -10.74x + 28.09"), size=4)+
    geom_text(aes(1.27, 28, label="R2 = 0.17"), size=4)+
    labs(y="Terpene content (mg/g)", x="% Nitrogen")+
    ggtitle("B")+
    themeblank)

(PPSCscatter<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=P, shape=Sampling, colour=Rank), size=3)+
    scale_shape_manual(values=TrapShapes2, name="Site Status   ")+
    scale_color_manual(values=qualcols, name="Quality Choice")+
    geom_abline(intercept = 21.93, slope = -33.61)+
    geom_text(aes(.115, 31, label="y = -33.6x + 21.9"), size=4)+
    geom_text(aes(.115, 29, label="R2 = 0.059"), size=4)+
    labs(y="Terpene content (mg/g)", x="% Phosphorus")+
    ggtitle("C")+
    themeblank)

(Fig2 <- (NPscatter / NPSCscatter / PPSCscatter) + plot_layout(guides = 'collect'))
ggsave(filename="Findings/Figure2.jpeg", Fig2, width = 6, height = 9.5, units = "in")



#### FIGURE 3 ####

(DBHN<-ggplot(data=DTtraps2)+
   geom_point(aes(y=N, x=AvgDBH), size = 3)+
   geom_text(aes(5, 1.35, label="p = 0.36"), size=5)+
   labs(x=NULL, y="% Nitrogen")+
   ggtitle("A")+
   themeblank)

(CanopyN<-ggplot(data=DTtraps2)+
    geom_point(aes(y=N, x=CanopyClosure), size = 3)+
    geom_abline(aes(intercept=.637, slope=.00336), size=1.2, colour="grey20")+
    geom_text(aes(30, 1.35, label="y = 0.00336x + 0.637"), size=5)+
    geom_text(aes(25, 1.25, label="t = 3.09, p < 0.01"), size=5)+
    labs(x=NULL, y=NULL)+
    ggtitle("B")+
    themeblank)

(DBHP<-ggplot(data=DTtraps2)+
    geom_point(aes(y=P, x=AvgDBH), size = 3)+
    geom_text(aes(5, .2, label="p = 0.69"), size=5)+
    labs(x=NULL, y="% Phosphorus")+
    ggtitle("C")+
    themeblank)

(CanopyP<-ggplot(data=DTtraps2)+
    geom_point(aes(y=P, x=CanopyClosure), size = 3)+
    geom_abline(aes(intercept=0.098, slope=0.00068), size=1.2, colour="grey20")+
    geom_text(aes(33, .2, label="y = 0.098x + 0.000682"), size=5)+
    geom_text(aes(26, .185, label="t = 3.041, p < 0.01"), size=5)+
    labs(x=NULL, y=NULL)+
    ggtitle("D")+
    themeblank)

(DBHC<-ggplot(data=DTtraps2)+
        geom_point(aes(y=TC, x=AvgDBH), size = 3)+
        geom_text(aes(12, 51, label="p = 0.80"), size=5)+
        labs(x=NULL, y="% Carbon")+
        ggtitle("E")+
        themeblank)

(CanopyC<-ggplot(data=DTtraps2)+
        geom_point(aes(y=TC, x=CanopyClosure), size = 3)+
        geom_text(aes(15, 51, label="p = 0.53"), size=5)+
        labs(x=NULL, y=NULL)+
        ggtitle("F")+
        themeblank)

(DBHPSC<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=AvgDBH), size = 3)+
    geom_text(aes(5, 30, label="p = 0.48"), size=5)+
    labs(x="Mean DBH (cm)", y="PSC")+
    ggtitle("G")+
    themeblank)

(CanopyPSC<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=CanopyClosure), size = 3)+
    geom_text(aes(14, 30, label="p =0.11"), size=5)+
    labs(x="Canopy Closure (%)", y=NULL)+
    ggtitle("H")+
    themeblank)



Fig3<-ggarrange(DBHN, CanopyN, DBHP, CanopyP, DBHC, CanopyC, DBHPSC, CanopyPSC, ncol=2, nrow=4)
ggsave(filename="Findings/Figure3.jpeg", Fig3, width = 9, height = 10, units = "in")

