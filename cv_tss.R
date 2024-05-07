CV <- function(x) {
  sd_value <- sd(x, na.rm = TRUE)
  mean_value <- mean(x, na.rm = TRUE)
  if (mean_value == 0) {
    return(NA) 
  } else {
    return(sd_value / mean_value)
  }
}

##ratios
dsa_k$ratio<- dsa_k$distWalked/dsa_k$distCorr
dsa_l$ratio<- dsa_l$distWalked/dsa_l$distCorr

###aggregate
#short
dsa_k_cv = aggregate(dsa_k$ratio, by=list(dsa_k$geschw, 
                                          dsa_k$bedingung, 
                                          dsa_k$Blick, 
                                          dsa_k$ID, 
                                          dsa_k$distCorr), FUN = CV)
colnames(dsa_k_cv) = c("speed","distance","presentation","ID","targetDist" , "cv" )

#long
dsa_l_cv = aggregate(dsa_l$ratio, by=list(dsa_l$geschw, 
                                          dsa_l$bedingung, 
                                          dsa_l$Blick, 
                                          dsa_l$ID, 
                                          dsa_l$distCorr), FUN = CV)
colnames(dsa_l_cv) = c("speed","distance","presentation","ID","targetDist" , "cv" )

###combine
dsa_all_cv<-rbind(dsa_k_cv,dsa_l_cv)




#DESKTOP VS HMD LONG DISTANCE
t.test(dsa_l_cv[ dsa_l_cv$presentation == "desktop",]$cv, 
       dsa_l_cv[ dsa_l_cv$presentation == "vr",]$cv, paired = TRUE)
#DESKTOP VS HMD SHORT DISTANCE
t.test(dsa_k_cv[dsa_k_cv$presentation == "desktop",]$cv, 
       dsa_k_cv[dsa_k_cv$presentation == "vr",]$cv, paired = TRUE)
#HMD SHORT VS LONG
t.test(dsa_l_cv[dsa_l_cv$presentation == "vr",]$cv, 
       dsa_k_cv[dsa_k_cv$presentation == "vr",]$cv, paired = TRUE)
#DESKTOP SHORT VS LONG
t.test(dsa_k_cv[dsa_k_cv$presentation == "desktop",]$cv, 
       dsa_l_cv[dsa_l_cv$presentation == "desktop",]$cv, paired = TRUE)



#
ggplot(dsa_all_cv, aes(x = speed, y = cv, fill = distance)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  scale_fill_manual(values = c("skyblue", "slateblue4"), labels = c("short", "long")) +
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
  ylab("Coefficient of Variation for Ratios") +
  xlab("Speed Condition") +
  facet_wrap(~ presentation, labeller = labeller(presentation = supp.labs))+
  coord_cartesian(ylim = c(0.06, 0.3))+
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











ggplot(dsa_all_cv, aes(x = distance, y = cv, fill = distance)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  scale_fill_manual(values = c("skyblue", "slateblue4"), labels = c("short", "long")) +
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
  ylab("Coefficient of Variation for Ratios") +
  xlab("") +
  facet_wrap(~ presentation, labeller = labeller(presentation = supp.labs))+
  coord_cartesian(ylim = c(0.06, 0.3))+
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
  scale_x_discrete(labels = c("", "")) 
