libs<-c('dplyr', 'data.table', 'ggplot2','ggpubr', 'ggeffects', 'patchwork', 'gghighlight')
lapply(libs, require, character.only = TRUE)
utm21N <- '+proj=longlat +zone=21 ellps=WGS84'

           ### Manuscript Figures ###

### IMPORTING ###

DTpiles<-readRDS("Input/pile_format.rds")
DTtrials<-readRDS("Input/trial_format.rds")
effsC<-readRDS("Input/effects_coat.rds")
effsT<-readRDS("Input/effects_temp.rds")
effsP<-readRDS("Input/effects_pref.rds")

themeblank <- theme(axis.title = element_text(size=14),
                axis.text.x = element_text(size=10),
                legend.key = element_blank(),
                legend.position = "none",
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill=NA, size=1))

themeblankguide <- theme(axis.title = element_text(size=14),
                    axis.text.x = element_text(size=10),
                    legend.key = element_blank(),
                    panel.background = element_blank(),
                    panel.border = element_rect(colour = "black", fill=NA, size=1))

qualcols<-c("High" = "Forestgreen", "Low" = "Yellow3")

#### FIGURE 4 ####

#if you want a path plot, use ID_Year, not Eartag.
(Intake<-ggplot(data=DTtrials)+
  geom_boxplot(position="dodge", aes(y=IR, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=IR, x=Trial), width=.25, size=3)+
  labs(y="Spruce intake rate (g/kg/day)", x=" ")+
  ggtitle("A")+
  themeblank)

(Pref<-ggplot(data=DTtrials)+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  geom_boxplot(position="dodge", aes(y=Diff_IR, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=Diff_IR, x=Trial), width=.25, size=3)+
  labs(y="Preference for high-quality spruce", x=" ")+
  ggtitle("B")+
  themeblank)

(Weight<-ggplot(data=DTtrials)+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  geom_boxplot(position="dodge", aes(y=Mass_change, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=Mass_change, x=Trial), width=.25, size=3)+
  labs(y="Lost body mass (%)", x="Trial Number")+
  ggtitle("C")+
  themeblank)


Fig4<-ggarrange(Intake, Pref, Weight,  ncol=1, nrow=3, label.x = 3)
ggsave(filename="Findings/Figure4.jpeg", Fig4, width = 5.5, height = 11.5, units = "in")



#### Figure 5 ####

#Boxplot showing the total trend
(boxplot<-ggplot(data=DTpiles, aes(y=IR, x=Treatment))+
   geom_boxplot(position="dodge", notch=FALSE)+ 
   geom_jitter(width=.25, size=2.5, colour="grey70")+
   scale_color_manual(values=qualcols)+
   labs(y="Spruce intake rate (g/kg/day)", x="Spruce quality")+
   ggtitle("A")+
   themeblank)

#path plot that matches boxplot
(pathplot<-ggplot(data=DTpiles)+
    geom_path(aes(y=IR, x=Treatment, group=sampleID, colour=Eartag), size=1)+
    scale_x_discrete(expand = c(.2, .2))+
    gghighlight(sampleID %in% list("A1680_10/27/2019", "A1667_11/24/2019"), max_highlight = 2, use_direct_label = NULL)+
    labs(y=" ", x="Spruce quality")+
    scale_color_manual(values=c("#1b9e77", "#d95f02"))+
    ggtitle("B")+
    themeblank)

(Fig5<- ggarrange(boxplot, pathplot,  ncol=2, nrow=1, label.x = 3))
ggsave(filename="Findings/Figure5.jpeg", Fig5, width = 7.3, height = 4.3, units = "in")


#### Figure 6 ####

#plot that shows intake as a function of coat colour with spruce quality
(Coat<-ggplot()+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, group=group, fill=group),
              data=effsC, colour="grey80", alpha=.3 )+
  geom_line(aes(x=x, y=predicted, group=group), size = 1, color="grey 50", data=effsC)+
  labs(x = "White (%)", y = "Spruce intake rate (g/kg/day)", title = "A")+
  themeblankguide)

#plot that shows intake as a function of temperature with spruce quality
(Temp<-ggplot()+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, group=group, fill=group),
              data=effsT, colour="grey80", alpha=.3 )+
  geom_line(aes(x=x, y=predicted, group=group), size = 1, color="grey 50", data=effsT)+
  labs(x = "Low ambient temperature (°C)", y = " ", title = "B")+
  themeblankguide)

(Fig6<- (Coat + Temp & scale_fill_manual(values=qualcols, name = "Spruce quality")) + plot_layout(guides = 'collect'))
ggsave(filename="Findings/Figure6.jpeg", Fig6, width = 9, height = 4.5, units = "in")


### Figure 7 ###

(Fig7<-ggplot(DTtrials)+
    geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, group=group),
                data=effsP, colour="grey80", alpha=.3 )+
    geom_line(aes(x=x, y=predicted, group=group), size = 1, color="grey 50", data=effsP)+
    geom_point(aes(y=Mass_change, x=Diff_IR),  size=3, colour="grey20")+
    geom_vline(xintercept=0, size=1.2, color="grey40", linetype="dashed")+
    labs(x="Preference for high-quality spruce", y="Lost body mass(%)")+
    themeblank)

ggsave(filename="Findings/Figure7.jpeg", Fig7, width = 6, height = 4, units = "in")




### Figure A5 ###

#Additional figure of Total consumption vs. preference
Prefmod<-lm(Diff_IR~IR, data=DTtrials)

(FigA5<-ggplot(data=DTtrials)+
    geom_point(aes(y=Diff_IR, x=IR), size=3, colour="grey20")+
    labs(x="Total intake rate (g/kg/day)", y="Preference for high quality spruce")+
    themeblank)

ggsave(filename="Findings/FigureA5.jpeg", FigA5, width = 6, height = 4, units = "in")


### Figure A6 ###
Massmod<-lm(Mass_change~Start_mass, data=DTtrials)

(FigA6<-ggplot(data=DTtrials)+
    geom_abline(intercept = (coef(Massmod)["(Intercept)"]), slope = (coef(Massmod)["Start_mass"]), 
                size = 1, color="grey 50")+
    geom_point(aes(y=Mass_change, x=Start_mass), size=3, colour="grey20")+
    labs(x="Starting mass (g)", y="Mass change (% lost)")+
    themeblank)



