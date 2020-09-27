a<-read.csv("drama_data.csv")
a2<-read.csv("drama_a.csv")
a3<-read.csv("drama_b.csv")
a4<-read.csv("drama_1.csv")
a5<-read.csv("drama_2.csv")
a6<-read.csv("drama_3.csv")
a7<-read.csv("drama_4.csv")

a<-na.omit(a)
library(psych)
#상관계수
windows()
pairs.panels(a)
#ggplot-line
ggplot(a,aes(x=Date,y=Rate,group=폭염,color=폭염))+
  geom_line(size=1.5)+
  xlab("date")+
  ylab("view rate")

ggplot(a,aes(x=Date,y=Rate,group=호우,color=호우))+
  geom_line(size=1.5)+
  xlab("date")+
  ylab("view rate")

ggplot(a$Rate,names.arg = a$Date)+
  geom_line(size=1.5,aes(group=폭염,color=폭염))+
  xlab("date")+
  ylab("view rate")

ggplot(a,aes(x=Date,y=Rate,group=요일,color=요일))+
  geom_line(size=0.6)+
  xlab("date")+
  ylab("view rate")

#수치형들 제외한 특보 전처리
windows()
a$호우 <- as.factor(a$호우)
a$강풍 <- as.factor(a$강풍)
a$폭염 <- as.factor(a$폭염)
a$한파 <- as.factor(a$한파)
a$대설 <- as.factor(a$대설)
a$건조 <- as.factor(a$건조)
a77 <- a[,c(1,13:19,20)]
head(a77)
str(a77)

#stat = "identity"
#geom_vline(xintercept = c(182,273))
install.packages("ggpubr")
library(ggpubr)
#ggplot-boxplot -drama전체
windows()
bxp<-ggplot(a77, aes(x=한파,y=Rate,fill=한파))+
  geom_boxplot(size=1, aes(group=한파))+
  geom_hline(yintercept = 15.5) 

ggplot(a77, aes(x=대설,y=Rate,fill=대설))+
  geom_boxplot(size=1, aes(group=대설))

ggplot(a77, aes(x=강풍,y=Rate,fill=강풍))+
  geom_boxplot(size=1, aes(group=강풍))

dp<-ggplot(a77, aes(x=건조,y=Rate,fill=건조))+
  geom_boxplot(size=1, aes(group=건조))+
  geom_hline(yintercept = 14.8) 

ggplot(a77, aes(x=호우,y=Rate,fill=호우))+
  geom_boxplot(size=1, aes(group=호우))

ggplot(a77, aes(x=폭염,y=Rate,fill=폭염))+
  geom_boxplot(size=1, aes(group=폭염))

ggplot(a77, aes(x=황사,y=Rate,group=황사,fill=황사))+
  geom_boxplot()

ggarrange(bxp, dp, 
          labels = c("평일 폭염", "주말 폭염"),
          ncol = 2, nrow = 1)

#선형회귀 - 유의한것들이 많음 수치형 날씨 데이터에서
summary(lm(Rate~.-Date,data=a))

#drama 주말
str(a2)
a2$호우 <- as.factor(a2$호우)
a2$강풍 <- as.factor(a2$강풍)
a2$폭염 <- as.factor(a2$폭염)
a2$한파 <- as.factor(a2$한파)
a2$대설 <- as.factor(a2$대설)
a2$건조 <- as.factor(a2$건조)
a22 <- a2[,c(1,2,14:20)]
str(a22)

windows()
ggplot(a22, aes(x=한파,y=Rate,fill=한파))+
  geom_boxplot(size=1, aes(group=한파))+
  geom_hline(yintercept = 19.5) 

ggplot(a22, aes(x=대설,y=Rate,fill=대설))+
  geom_boxplot(size=1, aes(group=대설))+
  geom_hline(yintercept = 19) 

ggplot(a22, aes(x=강풍,y=Rate,fill=강풍))+
  geom_boxplot(size=1, aes(group=강풍))+
  geom_hline(yintercept = 17.5) 

ggplot(a22, aes(x=건조,y=Rate,fill=건조))+
  geom_boxplot(size=1, aes(group=건조))+
  geom_hline(yintercept = 16.5) 

ggplot(a22, aes(x=호우,y=Rate,fill=호우))+
  geom_boxplot(size=1, aes(group=호우))

ggplot(a22, aes(x=폭염,y=Rate,fill=폭염))+
  geom_boxplot(size=1, aes(group=폭염))+
  geom_hline(yintercept = 16) 


ggplot(a22, aes(x=황사,y=Rate,group=황사,fill=황사))+
  geom_boxplot()

#drama-평일
str(a3)
a3$호우 <- as.factor(a3$호우)
a3$강풍 <- as.factor(a3$강풍)
a3$폭염 <- as.factor(a3$폭염)
a3$한파 <- as.factor(a3$한파)
a3$대설 <- as.factor(a3$대설)
a3$건조 <- as.factor(a3$건조)
a33 <- a3[,c(1,2,14:20)]
str(a33)

windows()
ggplot(a33, aes(x=한파,y=Rate,fill=한파))+
  geom_boxplot(size=1, aes(group=한파))+
  geom_hline(yintercept = 14.3) 

ggplot(a33, aes(x=대설,y=Rate,fill=대설))+
  geom_boxplot(size=1, aes(group=대설))+
  geom_hline(yintercept = 14.5) 

ggplot(a33, aes(x=강풍,y=Rate,fill=강풍))+
  geom_boxplot(size=1, aes(group=강풍))+
  geom_hline(yintercept = 14) 

ggplot(a33, aes(x=건조,y=Rate,fill=건조))+
  geom_boxplot(size=1, aes(group=건조))+
  geom_hline(yintercept = 14.3) 

ggplot(a33, aes(x=호우,y=Rate,fill=호우))+
  geom_boxplot(size=1, aes(group=호우))+
  geom_hline(yintercept = 14) 

ggplot(a33, aes(x=폭염,y=Rate,fill=폭염))+
  geom_boxplot(size=1, aes(group=폭염))+
  geom_hline(yintercept = 12.8) 


ggplot(a33, aes(x=황사,y=Rate,group=황사,fill=황사))+
  geom_boxplot()

#드라마 평일 주말 boxplot비교
windows()
we<-ggplot(a22, aes(x=한파,y=Rate,fill=한파))+
  geom_boxplot(size=1, aes(group=한파))+
  geom_hline(yintercept = 19.5) 

we1<-ggplot(a22, aes(x=대설,y=Rate,fill=대설))+
  geom_boxplot(size=1, aes(group=대설))+
  geom_hline(yintercept = 19) 

we2<-ggplot(a22, aes(x=강풍,y=Rate,fill=강풍))+
  geom_boxplot(size=1, aes(group=강풍))+
  geom_hline(yintercept = 17.5) 

we3<-ggplot(a22, aes(x=건조,y=Rate,fill=건조))+
  geom_boxplot(size=1, aes(group=건조))+
  geom_hline(yintercept = 16.5) 

we4<-ggplot(a22, aes(x=호우,y=Rate,fill=호우))+
  geom_boxplot(size=1, aes(group=호우))

we5<-ggplot(a22, aes(x=폭염,y=Rate,fill=폭염))+
  geom_boxplot(size=1, aes(group=폭염))+
  geom_hline(yintercept = 16) 

we6<-ggplot(a22, aes(x=황사,y=Rate,group=황사,fill=황사))+
  geom_boxplot()

ev<-ggplot(a33, aes(x=한파,y=Rate,fill=한파))+
  geom_boxplot(size=1, aes(group=한파))+
  geom_hline(yintercept = 14.3) 

ev1<-ggplot(a33, aes(x=대설,y=Rate,fill=대설))+
  geom_boxplot(size=1, aes(group=대설))+
  geom_hline(yintercept = 14.5) 

ev2<-ggplot(a33, aes(x=강풍,y=Rate,fill=강풍))+
  geom_boxplot(size=1, aes(group=강풍))+
  geom_hline(yintercept = 14) 

ev3<-ggplot(a33, aes(x=건조,y=Rate,fill=건조))+
  geom_boxplot(size=1, aes(group=건조))+
  geom_hline(yintercept = 14.3) 

ev4<-ggplot(a33, aes(x=호우,y=Rate,fill=호우))+
  geom_boxplot(size=1, aes(group=호우))+
  geom_hline(yintercept = 14) 

ev5<-ggplot(a33, aes(x=폭염,y=Rate,fill=폭염))+
  geom_boxplot(size=1, aes(group=폭염))+
  geom_hline(yintercept = 12.8) 


ev6<-ggplot(a33, aes(x=황사,y=Rate,group=황사,fill=황사))+
  geom_boxplot()

#대체적으로 평일이 날씨별 시청률에 영향을 많이 끼침
windows()
ggarrange(we,ev, 
          labels = c("주말 한파", "평일 한파"),
          ncol = 2, nrow = 1)
ggarrange(we1,ev1, 
          labels = c("주말 대설", "평일 대설"),
          ncol = 2, nrow = 1)
ggarrange(we2,ev2, 
          labels = c("주말 강풍", "평일 강풍"),
          ncol = 2, nrow = 1)
#주말에 건조일때도 시청률이 낮은 것으로 보아 영향x
ggarrange(we3,ev3, 
          labels = c("주말 건조", "평일 건조"),
          ncol = 2, nrow = 1)
#호우는 주말엔 변동량이 거의 없음
ggarrange(we4,ev4, 
          labels = c("주말 호우", "평일 호우"),
          ncol = 2, nrow = 1)
ggarrange(we5,ev5, 
          labels = c("주말 폭염", "평일 폭염"),
          ncol = 2, nrow = 1)
#주말엔 영향이 거의 없는 것으로 보임 하지만 평일에는 황사주의보에 따른 시청률의
#변화량이 뚜렷
ggarrange(we6,ev6, 
          labels = c("주말 황사", "평일 황사"),
          ncol = 2, nrow = 1)


#드라마 주말 저녁시간 -> 이것도 주말의 시청률은 날씨의 영향을 그렇게 받지 않음
str(a4)
a4$호우 <- as.factor(a4$호우)
a4$강풍 <- as.factor(a4$강풍)
a4$폭염 <- as.factor(a4$폭염)
a4$한파 <- as.factor(a4$한파)
a4$대설 <- as.factor(a4$대설)
a4$건조 <- as.factor(a4$건조)
a44 <- a4[,c(1,2,14:20)]
str(a44)

windows()
ggplot(a44, aes(x=한파,y=Rate,fill=한파))+
  geom_boxplot(size=1, aes(group=한파))+
  geom_hline(yintercept = 18.5) 

ggplot(a44, aes(x=대설,y=Rate,fill=대설))+
  geom_boxplot(size=1, aes(group=대설))+
  geom_hline(yintercept = 18.5) 

ggplot(a44, aes(x=강풍,y=Rate,fill=강풍))+
  geom_boxplot(size=1, aes(group=강풍))+
  geom_hline(yintercept = 17.6) 

ggplot(a44, aes(x=건조,y=Rate,fill=건조))+
  geom_boxplot(size=1, aes(group=건조))+
  geom_hline(yintercept = 16.9) 

ggplot(a44, aes(x=호우,y=Rate,fill=호우))+
  geom_boxplot(size=1, aes(group=호우))

ggplot(a44, aes(x=폭염,y=Rate,fill=폭염))+
  geom_boxplot(size=1, aes(group=폭염))+
  geom_hline(yintercept = 16) 


ggplot(a44, aes(x=황사,y=Rate,group=황사,fill=황사))+
  geom_boxplot()

#드라마 평일 8.9,11시
str(a5)
a5$호우 <- as.factor(a5$호우)
a5$강풍 <- as.factor(a5$강풍)
a5$폭염 <- as.factor(a5$폭염)
a5$한파 <- as.factor(a5$한파)
a5$대설 <- as.factor(a5$대설)
a5$건조 <- as.factor(a5$건조)
a55 <- a5[,c(1,2,14:20)]
str(a55)

windows()
mm<-ggplot(a55, aes(x=한파,y=Rate,fill=한파))+
  geom_boxplot(size=1, aes(group=한파))

mm1<-ggplot(a55, aes(x=대설,y=Rate,fill=대설))+
  geom_boxplot(size=1, aes(group=대설))

mm2<-ggplot(a55, aes(x=강풍,y=Rate,fill=강풍))+
  geom_boxplot(size=1, aes(group=강풍))

mm3<-ggplot(a55, aes(x=건조,y=Rate,fill=건조))+
  geom_boxplot(size=1, aes(group=건조))

mm4<-ggplot(a55, aes(x=호우,y=Rate,fill=호우))+
  geom_boxplot(size=1, aes(group=호우))

mm5<-ggplot(a55, aes(x=폭염,y=Rate,fill=폭염))+
  geom_boxplot(aes(group=폭염))


mm6<-ggplot(a55, aes(x=황사,y=Rate,group=황사,fill=황사))+
  geom_boxplot()

#드라마 평일 19-20시간대
str(a6)
a6$호우 <- as.factor(a6$호우)
a6$강풍 <- as.factor(a6$강풍)
a6$폭염 <- as.factor(a6$폭염)
a6$한파 <- as.factor(a6$한파)
a6$대설 <- as.factor(a6$대설)
a6$건조 <- as.factor(a6$건조)
a66 <- a6[,c(1,2,14:20)]
str(a66)

windows()
nn<-ggplot(a66, aes(x=한파,y=Rate,fill=한파))+
  geom_boxplot(size=1, aes(group=한파))

nn1<-ggplot(a66, aes(x=대설,y=Rate,fill=대설))+
  geom_boxplot(size=1, aes(group=대설))

nn2<-ggplot(a66, aes(x=강풍,y=Rate,fill=강풍))+
  geom_boxplot(size=1, aes(group=강풍))

nn3<-ggplot(a66, aes(x=건조,y=Rate,fill=건조))+
  geom_boxplot(size=1, aes(group=건조))

nn4<-ggplot(a66, aes(x=호우,y=Rate,fill=호우))+
  geom_boxplot(size=1, aes(group=호우))

nn5<-ggplot(a66, aes(x=폭염,y=Rate,fill=폭염))+
  geom_boxplot(aes(group=폭염))


nn6<-ggplot(a66, aes(x=황사,y=Rate,group=황사,fill=황사))+
  geom_boxplot()

##드라마 평일21~23
str(a7)
a7$호우 <- as.factor(a7$호우)
a7$강풍 <- as.factor(a7$강풍)
a7$폭염 <- as.factor(a7$폭염)
a7$한파 <- as.factor(a7$한파)
a7$대설 <- as.factor(a7$대설)
a7$건조 <- as.factor(a7$건조)
a77 <- a7[,c(1,2,14:20)]
str(a77)

windows()
bb<-ggplot(a77, aes(x=한파,y=Rate,fill=한파))+
  geom_boxplot(size=1, aes(group=한파))

bb1<-ggplot(a77, aes(x=대설,y=Rate,fill=대설))+
  geom_boxplot(size=1, aes(group=대설))

bb2<-ggplot(a77, aes(x=강풍,y=Rate,fill=강풍))+
  geom_boxplot(size=1, aes(group=강풍))

bb3<-ggplot(a77, aes(x=건조,y=Rate,fill=건조))+
  geom_boxplot(size=1, aes(group=건조))

bb4<-ggplot(a77, aes(x=호우,y=Rate,fill=호우))+
  geom_boxplot(size=1, aes(group=호우))

bb5<-ggplot(a77, aes(x=폭염,y=Rate,fill=폭염))+
  geom_boxplot(aes(group=폭염))
  

bb6<-ggplot(a77, aes(x=황사,y=Rate,group=황사,fill=황사))+
  geom_boxplot()
#평일 시간대별 특보별 시청률 비교
windows()
ggarrange(mm,nn,bb, 
          labels = c("평일 8~11", "평일 19~20","평일 21~23"),
          ncol = 2, nrow = 2)
ggarrange(mm1,nn1,bb1, 
          labels = c("평일 8~11", "평일 19~20","평일 21~23"),
          ncol = 2, nrow = 2)
#강풍은 살짝
ggarrange(mm2,nn2,bb2, 
          labels = c("평일 8~11", "평일 19~20","평일 21~23"),
          ncol = 2, nrow = 2)
#건조는 별차이 없음 위에서부터 알게된 내용
ggarrange(mm3,nn3,bb3, 
          labels = c("평일 8~11", "평일 19~20","평일 21~23"),
          ncol = 2, nrow = 2)
ggarrange(mm4,nn4,bb4, 
          labels = c("평일 8~11", "평일 19~20","평일 21~23"),
          ncol = 2, nrow = 2)
ggarrange(mm5,nn5,bb5, 
          labels = c("평일 8~11", "평일 19~20","평일 21~23"),
          ncol = 2, nrow = 2)
#황사의 특성이 두드러지게 나타남
ggarrange(mm6,nn6,bb6, 
          labels = c("평일 8~11", "평일 19~20","평일 21~23"),
          ncol = 2, nrow = 2)





