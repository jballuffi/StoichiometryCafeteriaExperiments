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

# ### Creating effects for temperature and coat colour figures
# TempMod<-lm(Diff_IR~Low_temp, data=DTtrials)
# effsT2<-as.data.table(effect(c("Low_temp"), xlevels=10, TempMod))
# Whitemod<-lm(Diff_IR~White, data=DTtrials)
# effsC2 <- as.data.table(effect(c("White"), xlevels=10, Whitemod))



#### FIGURE 4 ####

#if you want a path plot, use ID_Year, not Eartag.
(Intake<-ggplot(data=DTtrials)+
  geom_boxplot(position="dodge", aes(y=IR, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=IR, x=Trial), width=.25, size=3)+
  labs(y="Intake Rate (g/kg/day)", x=" ")+
  ggtitle("A")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)))

(Pref<-ggplot(data=DTtrials)+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  geom_boxplot(position="dodge", aes(y=Diff_IR, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=Diff_IR, x=Trial), width=.25, size=3)+
  labs(y="Preference for High Ranked Spruce", x=" ")+
  ggtitle("B")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)))

(Weight<-ggplot(data=DTtrials)+
  geom_hline(yintercept=0, size=1.2, color="grey40", linetype="dashed")+
  geom_boxplot(position="dodge", aes(y=Mass_change, x=Trial), outlier.shape = NA)+
  geom_jitter(aes(y=Mass_change, x=Trial), width=.25, size=3)+
  labs(y="Change in body mass (% lost)", x="Trial Number")+
  ggtitle("C")+
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=10),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)))


Fig4<-ggarrange(Intake, Pref, Weight,  ncol=1, nrow=3, label.x = 3)
ggsave(filename="Findings/Figure4.jpeg", Fig4, width = 5.5, height = 11.5, units = "in")



#### Figure 5 ####

#Boxplot showing the total trend
(boxplot<-ggplot(data=DTpiles, aes(y=IR, x=Treatment))+
   geom_boxplot(position="dodge", notch=FALSE)+ 
   geom_jitter(width=.25, size=3)+
   #scale_fill_manual(values=cols, guide=FALSE)+ #this is where the manual colors come in
   labs(y="Intake Rate (g/kg/day)", x="Nutritional Rank")+
   ggtitle("A")+
   theme(axis.title.y = element_text(size=14),
         axis.title.x = element_text(size=12),
         axis.text.x = element_text(size=10),
         legend.key = element_blank(),
         panel.background = element_blank(),
         panel.grid.minor.y = element_line(color="grey"),
         panel.grid.major.y = element_line(color="grey"),
         panel.border = element_rect(colour = "black", fill=NA, size=1)))

#path plot that matches boxplot
(pathplot<-ggplot(data=DTpiles)+
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
          panel.border = element_rect(colour = "black", fill=NA, size=1)))

ggarrange(boxplot, pathplot,  ncol=2, nrow=1, label.x = 3)
ggsave(filename="Findings/Figure5.jpeg", Fig5, width = 7.7, height = 4.3, units = "in")


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




