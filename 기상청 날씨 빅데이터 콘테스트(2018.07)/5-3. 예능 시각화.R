#par(mfrow=c(1,2))
ggarrange(as6,ad6, 
          labels = c("예능 주말 황사", "예능 평일 황사"),
          ncol = 2, nrow = 1)

####드라마 주말
str(a2)
a2$호우 <- as.factor(a2$호우)
a2$강풍 <- as.factor(a2$강풍)
a2$폭염 <- as.factor(a2$폭염)
a2$한파 <- as.factor(a2$한파)
a2$대설 <- as.factor(a2$대설)
a2$건조 <- as.factor(a2$건조)
a22 <- a2[,c(1,2,14:20)]
a22$황사<-factor(a22$황사 , levels=c("매우나쁨","나쁨","보통","좋음"))
a22$slice<-"주말드라마"
str(a22)
#주말
summary(lm(Rate~한파,a22))
summary(lm(Rate~대설,a22))
summary(lm(Rate~강풍,a22))
summary(lm(Rate~건조,a22))
summary(lm(Rate~호우,a22))
summary(lm(Rate~폭염,a22))
summary(lm(Rate~황사,a22))
#평일
summary(lm(Rate~한파,a33))
summary(lm(Rate~대설,a33))
summary(lm(Rate~강풍,a33))
summary(lm(Rate~건조,a33))
summary(lm(Rate~호우,a33))
summary(lm(Rate~폭염,a33))
summary(lm(Rate~황사,a33))
#주말
summary(lm(Rate~한파,b22))
summary(lm(Rate~대설,b22))
summary(lm(Rate~강풍,b22))
summary(lm(Rate~건조,b22))
summary(lm(Rate~호우,b22))
summary(lm(Rate~폭염,b22))
summary(lm(Rate~황사,b22))
#평일
summary(lm(Rate~한파,b33))
summary(lm(Rate~대설,b33))
summary(lm(Rate~강풍,b33))
summary(lm(Rate~건조,b33))
summary(lm(Rate~호우,b33))
summary(lm(Rate~폭염,b33))
summary(lm(Rate~황사,b33))


#평일드라마
a3$호우 <- as.factor(a3$호우)
a3$강풍 <- as.factor(a3$강풍)
a3$폭염 <- as.factor(a3$폭염)
a3$한파 <- as.factor(a3$한파)
a3$대설 <- as.factor(a3$대설)
a3$건조 <- as.factor(a3$건조)
a33 <- a3[,c(1,2,14:20)]
a33$황사<-factor(a33$황사 , levels=c("매우나쁨","나쁨","보통","좋음"))
a33$slice<-"평일드라마"
str(a33)

#예능 주말
str(b2)
b2$호우 <- as.factor(b2$호우)
b2$강풍 <- as.factor(b2$강풍)
b2$폭염 <- as.factor(b2$폭염)
b2$한파 <- as.factor(b2$한파)
b2$대설 <- as.factor(b2$대설)
b2$건조 <- as.factor(b2$건조)
b22 <- b2[,c(1,2,14:20)]
str(b22)
b22$황사<-factor(b22$황사 , levels=c("매우나쁨","나쁨","보통","좋음"))
b22$slice<-"주말예능"

#예능 평일- 예능은 건조랑 호우가 거의 상관 x
str(b3)
b3$호우 <- as.factor(b3$호우)
b3$강풍 <- as.factor(b3$강풍)
b3$폭염 <- as.factor(b3$폭염)
b3$한파 <- as.factor(b3$한파)
b3$대설 <- as.factor(b3$대설)
b3$건조 <- as.factor(b3$건조)
b33 <- b3[,c(1,2,14:20)]
b33$황사<-factor(b33$황사 , levels=c("매우나쁨","나쁨","보통","좋음"))
b33$slice<-"평일예능"







ab23<-rbind(b22,b33,a22,a33)
str(ab23)
sum(is.na(ab23))
ab23$slice<-as.factor(ab23$slice)
ab23$slice<-factor(ab23$slice,levels=c("평일드라마","주말드라마","평일예능","주말예능"))


str(ab23)
colnames(ab23)<-c("Date","Rate","호우","강풍","대설","건조","한파","폭염","미세먼지","slice")
##ggplot
windows()
ggplot(ab23, aes(x = 한파, y = Rate,fill=한파)) +
  geom_boxplot(size=1, aes(group=한파)) +
  scale_y_continuous(name = "시청률",
                     breaks = seq(5, 25, 5),
                     limits=c(5, 25)) +
  scale_x_discrete(name = "한파") +
  ggtitle("Boxplot of 한파 특보별 예능과 드라마의 평일과 주말 시청률") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"))+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 30, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=30), 
        legend.text=element_text(size=30),
        strip.text.x = element_text(size = 20, face="bold")) +
  facet_grid(. ~ slice)

ggplot(ab23, aes(x = 대설, y = Rate,fill=대설)) +
  geom_boxplot(size=1, aes(group=대설)) +
  scale_y_continuous(name = "시청률",
                     breaks = seq(5, 25, 5),
                     limits=c(5, 25)) +
  scale_x_discrete(name = "대설") +
  ggtitle("Boxplot of 대설 특보별 예능과 드라마의 평일과 주말 시청률") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"))+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 30, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=30), 
        legend.text=element_text(size=30),
        strip.text.x = element_text(size = 20, face="bold")) +
  facet_grid(. ~ slice)

ggplot(ab23, aes(x = 강풍, y = Rate,fill=강풍)) +
  geom_boxplot(size=1, aes(group=강풍)) +
  scale_y_continuous(name = "시청률",
                     breaks = seq(5, 25, 5),
                     limits=c(5, 25)) +
  scale_x_discrete(name = "강풍") +
  ggtitle("Boxplot of 강풍 특보별 예능과 드라마의 평일과 주말 시청률") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"))+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 30, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=30), 
        legend.text=element_text(size=30),
        strip.text.x = element_text(size = 20, face="bold")) +
  facet_grid(. ~ slice)

ggplot(ab23, aes(x = 건조, y = Rate,fill=건조)) +
  geom_boxplot(size=1, aes(group=건조)) +
  scale_y_continuous(name = "시청률",
                     breaks = seq(5, 25, 5),
                     limits=c(5, 25)) +
  scale_x_discrete(name = "건조") +
  ggtitle("Boxplot of 건조 특보별 예능과 드라마의 평일과 주말 시청률") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"))+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 30, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=30), 
        legend.text=element_text(size=30),
        strip.text.x = element_text(size = 20, face="bold")) +
  facet_grid(. ~ slice)

ggplot(ab23, aes(x = 호우, y = Rate,fill=호우)) +
  geom_boxplot(size=1, aes(group=호우)) +
  scale_y_continuous(name = "시청률",
                     breaks = seq(5, 25, 5),
                     limits=c(5, 25)) +
  scale_x_discrete(name = "호우") +
  ggtitle("Boxplot of 호우 특보별 예능과 드라마의 평일과 주말 시청률") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"))+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 30, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=30), 
        legend.text=element_text(size=30),
        strip.text.x = element_text(size = 20, face="bold")) +
  facet_grid(. ~ slice)

ggplot(ab23, aes(x = 폭염, y = Rate,fill=폭염)) +
  geom_boxplot(size=1, aes(group=폭염)) +
  scale_y_continuous(name = "시청률",
                     breaks = seq(5, 25, 5),
                     limits=c(5, 25)) +
  scale_x_discrete(name = "폭염") +
  ggtitle("Boxplot of 폭염 특보별 예능과 드라마의 평일과 주말 시청률") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"))+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 30, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=30), 
        legend.text=element_text(size=30),
        strip.text.x = element_text(size = 20, face="bold")) +
  facet_grid(. ~ slice)

ggplot(ab23, aes(x = 미세먼지, y = Rate,fill=미세먼지)) +
  geom_boxplot(size=1, aes(group=미세먼지)) +
  scale_y_continuous(name = "시청률",
                     breaks = seq(7.5, 22.5, 5),
                     limits=c(7.5, 22.5)) +
  scale_x_discrete(name = "미세먼지") +
  ggtitle("Boxplot of 미세먼지 특보별 예능과 드라마의 평일과 주말 시청률") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.8, linetype = 'solid',
                                        colour = "grey"))+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 30, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=30), 
        legend.text=element_text(size=30),
        strip.text.x = element_text(size = 20, face="bold")) +
  facet_grid(. ~ slice)
windows()
str(ab23)
