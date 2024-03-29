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

#### FIGURE A1 ####

TrapShapes<-c("Sampled"=16, "Interpolated"=9, "Offered"=8)
thememap <- theme(axis.title = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  legend.key = element_blank(),
                  panel.background = element_blank(),
                  axis.line.y.left = element_blank(),
                  axis.line.x.bottom = element_blank(),
                  #panel.border = element_rect(colour = "black", fill=NA, size=1),
                  legend.position = "right",
                  legend.direction = "vertical",
                  legend.text = element_text(size=9),
                  legend.title = element_text(size=11))




(Nmap<-ggplot(data=gridN) + 
    geom_raster(aes(x=x, y=y, fill=N))+
    scale_fill_gradient(high="darkgreen", low="yellow", name="N (%)              ")+
    labs(subtitle = "(a) Nitrogen")+
    geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
    scale_shape_manual(values=TrapShapes, name="Site Status")+
    thememap)

(Pmap<-ggplot(data=gridP) + 
    geom_raster(aes(x=x, y=y, fill=P))+
    scale_fill_gradient(high="darkgreen", low="yellow", name="P (%)              ")+
    geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
    scale_shape_manual(values=TrapShapes, name="Site Status")+
    labs(subtitle = "(b) Phosphorus")+
    thememap)

(PSCmap<-ggplot(data=gridPSC) + 
    geom_raster(aes(x=x, y=y, fill=PSC))+
    scale_fill_gradient(high="yellow", low="darkgreen", name="Terpene (mg/g)")+
    geom_point(aes(POINT_X, POINT_Y, shape=Sampling), color="black", data = DTtraps, size=3)+
    scale_shape_manual(values=TrapShapes, name="Site Status")+
    labs(subtitle = "(c) Plant Secondary Compounds")+
    thememap)

#make multi-panel
(FigA1 <- Nmap / Pmap / PSCmap + plot_layout(guides = 'collect'))


#### FIGURE A2 ####
qualcols<-c("High" = "Forestgreen", "Low" = "Yellow3", "None" = "Black")
TrapShapes2<-c("Sampled"=16, "Offered"=8)

(NPscatter<-ggplot(data=DTtraps2)+
    geom_point(aes(y=P, x=N, shape=Sampling, colour=Rank), size=3)+
    scale_shape_manual(values=TrapShapes2, name="Site Status   ")+
    scale_color_manual(values=qualcols, name="Quality Choice")+
    geom_abline(intercept = 0.01354, slope = 0.1256)+
    geom_text(aes(.8, .2, label="y = 0.13x + 0.014"), size=3)+
    geom_text(aes(.8, .19, label="R2 = 0.43"), size=3)+
    labs(y="% Phosphorus", x="% Nitrogen", subtitle = "(a)")+
    themeblank)

(NPSCscatter<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=N, shape=Sampling, colour=Rank), size=3)+
    scale_shape_manual(values=TrapShapes2, name="Site Status   ")+
    scale_color_manual(values=qualcols, name="Quality Choice")+
    geom_abline(intercept = 28.09, slope = -10.74)+
    geom_text(aes(1.27, 30, label="y = -10.74x + 28.09"), size=3)+
    geom_text(aes(1.27, 28, label="R2 = 0.17"), size=3)+
    labs(y="Terpene content (mg/g)", x="% Nitrogen", subtitle = "(b)")+
    themeblank)

(PPSCscatter<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=P, shape=Sampling, colour=Rank), size=3)+
    scale_shape_manual(values=TrapShapes2, name="Site Status   ")+
    scale_color_manual(values=qualcols, name="Quality Choice")+
    geom_abline(intercept = 21.93, slope = -33.61)+
    geom_text(aes(.115, 31, label="y = -33.6x + 21.9"), size=3)+
    geom_text(aes(.115, 29, label="R2 = 0.059"), size=3)+
    labs(y="Terpene content (mg/g)", x="% Phosphorus", subtitle = "(c)")+
    themeblank)

#make multi-panel
(FigA2 <- (NPscatter / NPSCscatter / PPSCscatter) + plot_layout(guides = 'collect'))



#### FIGURE A7 ####

(DBHN<-ggplot(data=DTtraps2)+
   geom_point(aes(y=N, x=AvgDBH), size = 3)+
   geom_text(aes(5, 1.35, label="p = 0.36"), size=3)+
   labs(x=NULL, y="% Nitrogen", subtitle = "(a)")+
   themeblank)

(CanopyN<-ggplot(data=DTtraps2)+
    geom_point(aes(y=N, x=CanopyClosure), size = 3)+
    geom_abline(aes(intercept=.637, slope=.00336), size=1, colour="grey20")+
    geom_text(aes(25, 1.35, label="y = 0.00336x + 0.637"), size=3)+
    geom_text(aes(25, 1.25, label="t = 3.09, p < 0.01"), size=3)+
    labs(x=NULL, y=NULL, subtitle = "(b)")+
    themeblank)

(DBHP<-ggplot(data=DTtraps2)+
    geom_point(aes(y=P, x=AvgDBH), size = 3)+
    geom_text(aes(5, .2, label="p = 0.69"), size=3)+
    labs(x=NULL, y="% Phosphorus", subtitle = "(c)")+
    themeblank)

(CanopyP<-ggplot(data=DTtraps2)+
    geom_point(aes(y=P, x=CanopyClosure), size = 3)+
    geom_abline(aes(intercept=0.098, slope=0.00068), size=1, colour="grey20")+
    geom_text(aes(26, .2, label="y = 0.098x + 0.000682"), size=3)+
    geom_text(aes(26, .185, label="t = 3.041, p < 0.01"), size=3)+
    labs(x=NULL, y=NULL, subtitle = "(d)")+
    themeblank)

(DBHC<-ggplot(data=DTtraps2)+
        geom_point(aes(y=TC, x=AvgDBH), size = 3)+
        geom_text(aes(12, 51, label="p = 0.80"), size=3)+
        labs(x=NULL, y="% Carbon", subtitle = "(e)")+
        themeblank)

(CanopyC<-ggplot(data=DTtraps2)+
        geom_point(aes(y=TC, x=CanopyClosure), size = 3)+
        geom_text(aes(15, 51, label="p = 0.53"), size=5)+
        labs(x=NULL, y=NULL, subtitle = "(f)")+
        themeblank)

(DBHPSC<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=AvgDBH), size = 3)+
    geom_text(aes(5, 30, label="p = 0.48"), size=3)+
    labs(x="Mean DBH (cm)", y="PSC", subtitle = "(g)")+
    themeblank)

(CanopyPSC<-ggplot(data=DTtraps2)+
    geom_point(aes(y=PSC, x=CanopyClosure), size = 3)+
    geom_text(aes(14, 30, label="p =0.11"), size=3)+
    labs(x="Canopy Closure (%)", y=NULL, subtitle = "(h)")+
    themeblank)

#make multi-panel
FigA7<-ggarrange(DBHN, CanopyN, DBHP, CanopyP, DBHC, CanopyC, DBHPSC, CanopyPSC, ncol=2, nrow=4)




#save all appendix figures
ggsave(filename="Output/FigureA1.jpeg", FigA1, width = 4.75, height = 8.5, units = "in")
ggsave(filename="Output/FigureA2.jpeg", FigA2, width = 6, height = 9.5, units = "in")
ggsave(filename="Output/FigureA7.jpeg", FigA7, width = 9, height = 10, units = "in")
