library(tidyverse)
library(hrbrthemes)
library(viridis)
library(Routliers)
library(dplyr)
library(lme4)
library(Rcpp)
library(cowplot) 
library(sjPlot)
library(sjmisc) 
library(effects)
library(sjstats)
library(simr)
library(ggpubr)
library(rlang)
library(plotrix)
library(Hmisc)
library(patchwork)
setwd("C:/Users/cindy/Desktop/data_tss1")
dsa<-read.table("dsa.txt", header=T,  sep="\t", dec=".", fill=T)


###################### split by short and long condition
dsa_k <- dsa[dsa$bedingung=="kurz",]
dsa_l <- dsa[dsa$bedingung=="lang",]



####################################################
#Outlier script 
source("C:/Users/cindy/Desktop/TSS/tss_outlier.R")



######################Aggregate

dsa_k_agg = aggregate(dsa_k$distWalked, by=list(dsa_k$geschw, dsa_k$bedingung, 
                    dsa_k$Blick, dsa_k$ID, dsa_k$distCorr), mean, na.rm=T)
colnames(dsa_k_agg) = c("speed","distance","presentation","ID","targetDist" , "prodDist" )


dsa_l_agg= aggregate(dsa_l$distWalked, by=list(dsa_l$geschw, dsa_l$bedingung, 
                    dsa_l$Blick, dsa_l$ID, dsa_l$distCorr), mean, na.rm=T)
colnames(dsa_l_agg) = c("speed","distance","presentation","ID","targetDist",  "prodDist")





##########################################################################
#####Combine

dsa_all<-rbind(dsa_k_agg,dsa_l_agg)
#######Ratios
dsa_all$ratioWalked <-dsa_all$prodDist/dsa_all$targetDist 


#Modell ratio ~ distance condition * speed condition * presentation
mixed.lmer<- lmer(prodDist ~ distance*speed  * presentation + (1|ID), data = dsa_all)

##############Tabel
tabelle<-tab_model(mixed.lmer,
                   show.re.var= FALSE, 
                   pred.labels =c("(Intercept)",  "Distance [long]","Speed [fast]", 
                                  "Presentation [HMD] ", "Distance [long] * Speed [fast] ",
                                  "Distance [long] * Presentation [HMD]","Speed [fast] * Presentation [HMD]", 
                                  "Distance [long] * Speed [fast] * Presentation [HMD]"),
                   dv.labels= "",show.zeroinf = FALSE,
                   show.r2 = FALSE,
                   show.icc = FALSE)

#t-test
dsa_k_agg$ratioWalked <-dsa_k_agg$prodDist/dsa_k_agg$targetDist 
dsa_l_agg$ratioWalked <-dsa_l_agg$prodDist/dsa_l_agg$targetDist

#short-long speed
t.test(dsa_l_agg[dsa_l_agg$speed == "langsam",]$ratio, 
       dsa_l_agg[dsa_l_agg$speed == "schnell",]$ratio, paired = TRUE)
t.test(dsa_k_agg[dsa_k_agg$speed == "langsam",]$ratio, 
       dsa_k_agg[dsa_k_agg$speed == "schnell",]$ratio, paired = TRUE)

#short-long presentation
t.test(dsa_k_agg[dsa_k_agg$presentation == "vr",]$ratio, 
       dsa_k_agg[dsa_k_agg$presentation == "desktop",]$ratio, paired = TRUE)
t.test(dsa_l_agg[dsa_l_agg$presentation == "vr",]$ratio, 
       dsa_l_agg[dsa_l_agg$presentation == "desktop",]$ratio, paired = TRUE)



##################################################################
###PLOTS
std.error <- function(x) {
  sqrt(var(x, na.rm=TRUE) / length(x))
}

supp.labs <- c("Desktop", "HMD")
names(supp.labs) <- c("desktop", "vr")

##Plot distance speed
p1 <- ggplot(dsa_all, aes(x = speed, y = ratioWalked, fill = distance)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  geom_hline(yintercept = 1, lty = "dashed", col = "black") +
  scale_fill_manual(values = c("skyblue", "skyblue4"), labels = c("short", "long")) +
  theme(
    axis.text = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    strip.text = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  ) +
  ggtitle("") +
  ylab("Ratio (produced distance/target distance)") +
  xlab("Speed Condition") +
  coord_cartesian(ylim = c(0.6, 1.2))+
  stat_summary(
    fun.data = function(x) {
      ymin = mean(x) - std.error(x)
      ymax = mean(x) + std.error(x)
      data.frame(ymin = ymin, ymax = ymax)
    },
    geom = "errorbar",
    width = 0.4,
    size = 1,
    aes(group = interaction(distance, speed)),
    position = position_dodge(width = 0.9)
  ) +
  guides(fill = guide_legend(title = "Distance Condition"), 
         color = guide_legend(title = "Distance Condition")) +
  scale_x_discrete(labels = c("slow", "fast")) 



#plot presentation distance
p2 <- ggplot(dsa_all, aes(x = presentation, y = ratioWalked, fill = distance)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  geom_hline(yintercept = 1, lty = "dashed", col = "black") +
  scale_fill_manual(values = c("skyblue", "skyblue4"), labels = c("short", "long")) +
  theme(
    axis.text = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 18),
    legend.position = "none",
    axis.text.y.left = element_blank(),  
    axis.title.y.left = element_blank()
  ) +
  ggtitle("") +
  ylab("") +
  xlab("Presentation Mode") +
  coord_cartesian(ylim = c(0.6, 1.2))+
  stat_summary(
    fun.data = function(x) {
      ymin = mean(x) - std.error(x)
      ymax = mean(x) + std.error(x)
      data.frame(ymin = ymin, ymax = ymax)
    },
    geom = "errorbar",
    width = 0.4,
    size = 1,
    aes(group = interaction(distance, presentation)),
    position = position_dodge(width = 0.9)
  ) +
  guides(fill = guide_legend(title = "Distance Condition"), 
         color = guide_legend(title = "Distance Condition")) +
  scale_x_discrete(labels = c("Desktop", "HMD")) 


p1 + p2 + plot_layout(ncol = 2)








############Questionnaire 
##prepare for wilk.test
dsa_quest <- dsa %>%
  group_by(ID) %>%
  summarize_all(mean)

dsa_quest <- dsa_quest %>%
  select_if(~sum(!is.na(.)) > 0)

#wilk.test
wilk.test.vr <- wilcox.test(dsa_quest$TimeVsSpeed_K_VR,
                            dsa_quest$X.TimeVsSpeed_L_VR, paired = TRUE)
wilk.test.desk <- wilcox.test(dsa_quest$TimeVsSpeed_K_D, 
                              dsa_quest$TimeVsSpeed_L_D, paired = TRUE)



###Prepare for plots
Desk <- data.frame(dist = rep(c("short", "long"), 
                               each = length(dsa$TimeVsSpeed_K_D)),
                   Value = c(dsa$TimeVsSpeed_K_D, dsa$TimeVsSpeed_L_D))


VR <- data.frame(dist = rep(c("short", "long"), 
                                each = length(dsa$TimeVsSpeed_K_VR)),
                    Value = c(dsa$TimeVsSpeed_K_VR, dsa$TimeVsSpeed_L_VR))

# Boxplots
deskplot<-ggplot(Desk, aes(x = dist, y = Value)) +
  geom_boxplot(fill= c("lightblue", "mediumseagreen")) +
  ylab("")+
  xlab("")+
  labs(title = "Desktop", 
  )+
  theme( axis.text = element_text( size = 24 ),
         axis.text.x = element_text( size = 24 ),
         axis.title = element_text( size = 24, face = "bold" ),
         strip.text = element_text(size = 24),
         legend.text= element_text( size = 24),
         legend.title = element_text( size = 24),
         title = element_text( size = 24))+
  geom_hline(yintercept = 10, lty = "dashed", col = "black") +
  scale_x_discrete( labels = c("short", "long"))
deskplot <- deskplot + scale_y_continuous(breaks = c(3, 10, 18), labels = c("Time", "Equal", "Speed"))

VRplot<-ggplot(VR, aes(x = dist, y = Value)) +
  geom_boxplot(fill= c("lightblue", "mediumseagreen")) +
  ylab("")+
  xlab("")+
  labs(title = "HMD", 
  )+
  theme( axis.text = element_text( size = 24 ),
         axis.text.x = element_text( size = 24 ),
         axis.title = element_text( size = 24, face = "bold" ),
         strip.text = element_text(size = 24),
         legend.text= element_text( size = 24),
         legend.title = element_text( size = 24),
         title = element_text( size = 24) )+
  geom_hline(yintercept = 10, lty = "dashed", col = "black") +
  scale_x_discrete( labels = c("short", "long"))
require(gridExtra)

VRplot <- VRplot + scale_y_continuous(breaks = c(3, 10, 18), labels = c("Time", "Equal", "Speed"))

deskplot <- deskplot + coord_flip()
VRplot <- VRplot + coord_flip()
grid.arrange( deskplot,VRplot, ncol=2)

