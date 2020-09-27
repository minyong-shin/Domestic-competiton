#1시즌 조회수
str(dong)
#1시즌 댓글수
co88_a<-co8_a[,-c(39,38)]

co88_a$comfac<-ifelse(co88_a$총댓글수>mean(co88_a$총댓글수),2,1)
str(co88_a)
co88_a<-co88_a[,-c(1:4)]



dok<-data.frame(이름=character(0),범주=numeric(0), viewfac=numeric(0))

for(i in 1:30){
  dong2_a= data.frame(viewfac=co88_a$comfac,이름=colnames(co88_a[i]),범주=co88_a[,i])
  dok = bind_rows(dok,dong2_a)
}

dok$viewfac<-factor(dok$viewfac,levels=c(1,2))
View(dok)
#1시즌 dc
str(co8_a)
co88_b<-co8_a[,-c(1:7,38)]
str(co88_b)

co88_b$snsfac<-as.numeric(co88_b$snsfac)


doi<-data.frame(이름=character(0),범주=numeric(0), viewfac=numeric(0))

for(i in 1:30){
  dong2_b= data.frame(viewfac=co88_b$snsfac,이름=colnames(co88_b[i]),범주=co88_b[,i])
  doi = bind_rows(doi,dong2_b)
}

doi$viewfac<-factor(doi$viewfac,levels=c(1,2))

#2시즌 조회수
fix<-ho_d[,-c(39,1:5)]
str(fix)

fix1<-fix
fix1$viewfac<-ifelse(fix1$tview>mean(fix$tview),1,2)
str(fix1)
fix1<-fix1[,-c(1,2,33)]


kok<-data.frame(이름=character(0),범주=numeric(0), viewfac=numeric(0))

for(i in 1:30){
  dong3_a= data.frame(viewfac=fix1$viewfac,이름=colnames(fix1[i]),범주=fix1[,i])
  kok = bind_rows(kok,dong3_a)
}

kok$viewfac<-factor(kok$viewfac,levels=c(1,2))
str(kok)

#2시즌 댓글
fix2<-fix
fix2$comfac<-ifelse(fix2$총댓글수>mean(fix2$총댓글수),1,2)
str(fix2)
fix2<-fix2[,-c(1,2,33)]


koi<-data.frame(이름=character(0),범주=numeric(0), viewfac=numeric(0))

for(i in 1:30){
  dong3_b= data.frame(viewfac=fix2$comfac,이름=colnames(fix2[i]),범주=fix2[,i])
  koi = bind_rows(koi,dong3_b)
}

koi$viewfac<-factor(koi$viewfac,levels=c(1,2))
str(koi)

#2시즌 dc
fix3<-fix
fix3$snsfac<-ifelse(fix3$snsdc>mean(fix3$snsdc),1,2)
str(fix3)
fix3<-fix3[,-c(1,2,33)]


kog<-data.frame(이름=character(0),범주=numeric(0), viewfac=numeric(0))

for(i in 1:30){
  dong3_c= data.frame(viewfac=fix3$snsfac,이름=colnames(fix3[i]),범주=fix3[,i])
  kog = bind_rows(kog,dong3_c)
}

kog$viewfac<-factor(kog$viewfac,levels=c(1,2))
str(kog)

####2시즌- kok , koi, kog   / 1시즌 dong , dok , doi
koi_three<-subset(koi,이름=="threep")
dok_three<-subset(dok,이름=="threep")

ggplot(koi_three,aes(이름,범주,group=viewfac,fill=viewfac))+
  geom_boxplot()


##########
co7_s<-com77[,c(1:4,6,7,8,23:30,34)]
str(co7_s)
str(co8_a)
ho_ds2<-merge(x=co8_a, y=co7_s, by.x=c("일자","대진팀","대진팀2"),by.y=c("일자","대진팀","대진팀2"),all.x=T)
str(ho_ds2)  
ho_ds2<-ho_ds2[,-c(4,6,7,8,9,41,39,38)]
ho_ds2$대진팀<-as.factor(ho_ds2$대진팀)
ho_ds2$대진팀2<-as.factor(ho_ds2$대진팀2)
ho_ds2$지역<-as.factor(ho_ds2$지역)
ho_ds2$시간<-as.factor(ho_ds2$시간)
ho_ds2$장소<-as.factor(ho_ds2$장소)
ho_ds2$미세먼지농도<-as.numeric(ho_ds2$미세먼지농도)

sum(is.na(ho_ds2$미세먼지농도))
ho_ds2[is.na(ho_ds2)]<-20

str(ho_ds2)
hol2<-merge(x=ho_ds2,y=holi,by.x="일자",by.y="date",all.x=T)

str(hol2)
hol2<-hol2[,-46]
hol2$holiday<-factor(hol2$holiday,levels=c("평일","공휴일","주말"))
####################################################
####################################################
ho_d<-ho_d[,-39]
sum<-ho_d[,]
str(ho_d)
str(co7)
str(com6_s)
com6_s<-com6[,c(1:4,6,7,8,10,23:34)]

ho_ds<-merge(x=ho_d,y=com6_s,by.x=c("일자","대진팀","대진팀2"),by.y=c("일자","대진팀","대진팀2"),all.x=T)
str(ho_ds)  
ho_ds<-ho_ds[,-c(8,9,38,6,7,52:54)]  
ho_ds$대진팀<-as.factor(ho_ds$대진팀)
ho_ds$대진팀2<-as.factor(ho_ds$대진팀2)
ho_ds$지역<-as.factor(ho_ds$지역)
ho_ds$시간<-as.factor(ho_ds$시간)
ho_ds$장소<-as.factor(ho_ds$장소)
ho_ds$특보<-as.factor(ho_ds$특보)
ho_ds$한파<-as.factor(ho_ds$한파)
ho_ds$rival<-as.factor(ho_ds$rival)
ho_ds<-ho_ds[,-35]

holi<-data.frame(fread("holi.csv"))
str(holi)
unique(holi$holiday)
holi$date<-as.Date(holi$date)

hol<-merge(x=ho_ds,y=holi,by.x="일자",by.y="date",all.x=T)
str(hol2)
hol<-hol[,-48]
hol$holiday<-as.factor(hol$holiday)
hol$holiday<-relevel(hol$holiday,ref = "평일")

hol<-hol[,-4]
hol<-hol[,-36]
hol<-hol[,-c(1,2,3,33)]
hol<-hol[,-30]
str(hol)
hol_lm<-lm(관중~.,hol)
summary(hol_lm)

str(hol2)
hol3<-hol2
hol2<-hol2[,-c(1,2)]
hol_lm2<-lm(관중~.,hol2)
summary(hol_lm2)
sum(is.na(hol2))
hol2$holiday=="주말"


#lm정리
summary(hol_lm)
summary(hol_lm2)

####2시즌요일별
str(hol22)
str(hol11)
hol22<-hol2
hol11<-hol
hol11$시즌<-"17-18시즌"
hol22$시즌<-"16-17시즌"

gra<-rbind(hol11[,c(1,41,40)],hol22[,c(1,41,40)])
str(gra)

ggplot(gra,aes(holiday,관중,fill=holiday))+
  geom_boxplot()+
  facet_grid(.~시즌)+
  scale_y_continuous(name = "관중수") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 요일에 따른 관중수 추이")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"))

####2시즌rival
gra2<-rbind(hol11[,c(1,41,39)],hol22[,c(1,41,39)])
str(gra2)
gra2$rival<-ifelse(gra2$rival==0,"라이벌매치x","라이벌매치")

ggplot(gra2,aes(rival,관중,fill=rival))+
  geom_boxplot()+
  facet_grid(.~시즌)+
  scale_y_continuous(name = "관중수") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 라이벌유무에 따른 관중수 추이")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"))

####2시즌장소
jang1<-ggplot(hol2,aes(장소,관중,fill=장소))+
  geom_boxplot()+
  scale_y_continuous(name = "관중수") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 장소에 따른 관중수 추이")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")

jang2<-ggplot(hol,aes(장소,관중,fill=장소))+
  geom_boxplot()+
  scale_y_continuous(name = "관중수") +
  scale_x_discrete(name = "") +
  ggtitle("")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")

library(ggpubr)
ggarrange(jang1,jang2, 
          labels = c("16-17시즌", "17-18시즌"),
          ncol = 1, nrow = 2)




####의미없는 날씨
str(hol11)
str(hol22)
gra3<-rbind(hol11[,c(1,41,38)],hol22[,c(1,41,38)])
gra4<-rbind(hol11[,c(1,41,36)],hol22[,c(1,41,36)])


han<-ggplot(gra3,aes(한파,관중,fill=한파))+
  geom_boxplot()+
  facet_grid(.~시즌)+
  scale_y_continuous(name = "관중수") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 한파에 따른 관중수 추이")+
  theme(plot.title = element_text(size = 18,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 16, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 18),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 15, face="bold"))

mun<-ggplot(gra4,aes(특보,관중,fill=특보))+
  geom_boxplot()+
  facet_grid(.~시즌)+
  scale_y_continuous(name = "관중수") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 미세먼지에 따른 관중수 추이")+
  theme(plot.title = element_text(size = 18,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 1, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 18),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 15, face="bold"))

ggarrange(han,mun, 
          labels = c("한파특보", "미세먼지특보"),
          ncol = 2, nrow = 1)








