libs<-c('dplyr', 'data.table','sf', 'rgdal','raster','sp', 'ggplot2','RColorBrewer', 'ggpubr', 'effects', 'patchwork')
lapply(libs, require, character.only = TRUE)
utm21N <- '+proj=longlat +zone=21 ellps=WGS84'

           ### Manuscript Figures ###

### IMPORTING ###

DTpiles<-readRDS("Input/pile_format.rds")
DTtrials<-readRDS("Input/trial_format.rds")
effsC<-readRDS("Input/effects_coat.rds")
effsT<-readRDS("Input/effects_temp.rds")
effsP<-readRDS("Input/effects_pref.rds")


#### FIGURE 4 ####
theme4 <- theme(axis.title = element_text(size=14),
              axis.text.x = element_text(size=10),
              legend.key = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=1))

#if you want a path plot, use ID_Year, not Eartag.
(Intake<-ggplot(data=DTtrials)+
  geom_boxplot(position="dodge", aes(y=IR, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=IR, x=Trial), width=.25, size=3)+
  labs(y="Spruce intake rate (g/kg/day)", x=" ")+
  ggtitle("A")+
  theme4)

(Pref<-ggplot(data=DTtrials)+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  geom_boxplot(position="dodge", aes(y=Diff_IR, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=Diff_IR, x=Trial), width=.25, size=3)+
  labs(y="Preference for high-quality spruce", x=" ")+
  ggtitle("B")+
  theme4)

(Weight<-ggplot(data=DTtrials)+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  geom_boxplot(position="dodge", aes(y=Mass_change, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=Mass_change, x=Trial), width=.25, size=3)+
  labs(y="Lost body mass (%)", x="Trial Number")+
  ggtitle("C")+
  theme4)


Fig4<-ggarrange(Intake, Pref, Weight,  ncol=1, nrow=3, label.x = 3)
ggsave(filename="Findings/Figure4.jpeg", Fig4, width = 5.5, height = 11.5, units = "in")



#### Figure 5 ####
theme5 <- theme(axis.title.y = element_text(size=14),
                axis.title.x = element_text(size=12),
                axis.text.x = element_text(size=10),
                legend.key = element_blank(),
                panel.background = element_blank(),
                panel.grid.minor.y = element_line(color="grey"),
                panel.grid.major.y = element_line(color="grey"),
                panel.border = element_rect(colour = "black", fill=NA, size=1))

#Boxplot showing the total trend
(boxplot<-ggplot(data=DTpiles, aes(y=IR, x=Treatment))+
   geom_boxplot(position="dodge", notch=FALSE)+ 
   geom_jitter( width=.25, size=3)+
   labs(y="Spruce intake rate (g/kg/day)", x="Spruce quality")+
   ggtitle("A")+
   theme5)

#path plot that matches boxplot
(pathplot<-ggplot(data=DTpiles)+
    geom_path(aes(y=IR, x=Treatment, group=sampleID), size=1)+
    labs(y=" ", x="Spruce quality")+
    ggtitle("B")+
    theme5)

Fig5<- ggarrange(boxplot, pathplot,  ncol=2, nrow=1, label.x = 3)
ggsave(filename="Findings/Figure5.jpeg", Fig5, width = 7.3, height = 4.3, units = "in")


#### Figure 6 ####
qualcols<-c("High" = "Forestgreen", "Low" = "Yellow3")
theme6 <- theme(axis.title = element_text(size=14),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               legend.key = element_blank(),
               panel.background = element_blank(),
               panel.grid = element_line(colour = "grey90", size = .3),
               panel.border = element_rect(colour = "black", fill=NA, size=1))

#plot that shows intake as a function of coat colour with spruce quality
(Coat<-ggplot()+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, group=group, fill=group),
              data=effsC, colour="grey80", alpha=.3 )+
  geom_line(aes(x=x, y=predicted, group=group), size = 1, color="grey 50", data=effsC)+
  # geom_point(aes(y=IR, x=White, color=Treatment), size= 2, data=DTpiles)+
  # scale_color_manual(values=qualcols, name = "Spruce Quality", guide=FALSE)+
  labs(x = "White (%)", y = "Spruce intake rate (g/kg/day)")+
  theme6)

#plot that shows intake as a function of temperature with spruce quality
(Temp<-ggplot()+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, group=group, fill=group),
              data=effsT, colour="grey80", alpha=.3 )+
  geom_line(aes(x=x, y=predicted, group=group), size = 1, color="grey 50", data=effsT)+
  #geom_point(aes(y=IR, x=Low_temp, color=Treatment), size= 2, data=DTpiles)+
  # scale_color_manual(values=qualcols, name = "Spruce Quality", guide=FALSE)+
  labs(x = "Low ambient temperature (°C)", y = " ")+
  theme6)

(Fig6<- (Coat + Temp & scale_fill_manual(values=qualcols, name = "Spruce quality")) + plot_layout(guides = 'collect'))
ggsave(filename="Findings/Figure6.jpeg", Fig6, width = 9, height = 4.5, units = "in")


### Figure 7 ###

(Fig7<-ggplot(DTtrials)+
    geom_ribbon(aes(x=Diff_IR, ymin=lower, ymax=upper), data=effsP, colour="grey80", alpha=.3 )+
    geom_point(aes(y=Mass_change, x=Diff_IR),  size=3, colour="grey20")+
    geom_abline(intercept = 0.06, slope = -0.0003809, size=1.2)+
    geom_vline(xintercept=0, size=1.2, color="grey40", linetype="dashed")+
    labs(x="Preference for high-quality spruce", y="Lost body mass(%)")+
    theme(axis.title = element_text(size=14),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          legend.key = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1)))
ggsave(filename="Findings/Figure7.jpeg", Fig7, width = 6, height = 4, units = "in")




### Figure 8 ###

#Additional figure of Total consumption vs. preference
summary(lm(Diff_IR~IR, data=DTtrials))
(Fig8<-ggplot(data=DTtrials)+
  geom_abline(intercept=1.13, slope=0.038, size=1.2)+
  geom_point(aes(y=Diff_IR, x=IR), size=4, colour="grey20")+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)))




