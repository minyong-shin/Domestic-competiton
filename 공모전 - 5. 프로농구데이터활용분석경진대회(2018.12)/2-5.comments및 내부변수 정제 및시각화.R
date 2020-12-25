str(view3)

com<- data.frame(fread("comments_1718_2.csv"))
str(com)
com$날짜<-as.Date(com$날짜,format="%Y.%m.%d")

com2<- com %>% group_by(날짜,팀,경기번호) %>% summarise(총댓글수=n())
str(com2)
com2<-as.data.frame(com2)
View(com2)
colnames(com2)<-c("일자","대진팀","번호","home댓글수")
unique(com2$대진팀)
com2<-com2[,-3]
colnames(com2)<-c("일자","대진팀2","away댓글수")

com3<- com2 %>% group_by(날짜,경기번호) %>% summarise(총댓글수=sum(총댓글수))
View(com3)
colnames(com3)<-c("일자","번호","총댓글수")

com4<-merge(x=test33,y=com2,by.x = c("일자","대진팀"),by.y = c("일자","대진팀"),all.x=T)
str(com4)
View(com4)

com5<-merge(x=com4,y=com2,by.x=c("일자","대진팀2"),by.y=c("일자","대진팀2"),all.x=T)
str(com5)
View(com5)
com5$총댓글수<-com5$home댓글수+com5$away댓글수

view333<-view3[,c(1,2,3,41)]
str(view333)
com6<-merge(x=com5,y=view333,by.x=c("일자","대진팀","대진팀2"),by.y=c("일자","대진팀","대진팀2"),all.x=T)
str(com6)

com5_f<-com6[,-c(1:8)]
str(com5_f2)
View(com5_f)
com5_f$home<-as.factor(com5_f$home)
com5_f$away<-as.factor(com5_f$away)
com5_f2<-com5_f[,-c(31,32)]
library(psych)
windows()
pairs.panels(com5_f)
m<-cor(com5_f2,use='complete.obs')
install.packages("corrplot")
library(corrplot)
corrplot(m, method="number")

fit<-lm(관중~.-home-away,com5_f)
summary(fit)
step(fit,direction="both")

write.csv(com6,"지금까지변수.csv",row.names=F)
library(data.table)
com6<-data.frame(fread("지금까지변수.csv"))
str(com6)











#######################
###시각화#######
########################
eda<-com6 %>% group_by(대진팀,대진팀2) %>% summarise(mean=mean(관중))
str(eda)

eda<-as.data.frame(eda)
eda$대진팀2<-factor(eda$대진팀2,levels=c(""))
View(eda2)
eda2<-eda[c(46:90),]
eda<-eda[c(1:45),]

library(ggplot2)
windows()
View(eda)
eda$mean<-round(eda$mean,0)
ggplot(eda,aes(reorder(대진팀,-mean),mean,group=대진팀2,fill=대진팀2))+
  geom_bar(stat="identity",position="dodge",colour="black",width=0.75)+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(name = "관중수")+
  scale_x_discrete(name = "Home팀")+
  guides(fill=guide_legend(title="Away팀"))+
  geom_text(aes(label=mean), size=2.65,vjust=1.6 ,color='black',position=position_dodge(0.75))

eda2$mean<-round(eda2$mean,0)
ggplot(eda2,aes(reorder(대진팀,-mean),mean,group=대진팀2,fill=대진팀2))+
  geom_bar(stat="identity",position="dodge",colour="black",width=0.85)+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(name = "관중수")+
  scale_x_discrete(name = "Home팀")+
  guides(fill=guide_legend(title="Away팀"))+
  geom_text(aes(label=mean), size=3,vjust=1.6 ,color='black',position=position_dodge(0.9))

str(com6)
com6[75,44]<-41235
eda_view<-com6 %>% group_by(대진팀,대진팀2) %>% summarise(mean=mean(tview))
eda_view<-as.data.frame(eda_view)
str(eda_view)
eda_view2<-eda_view[c(46:90),]
eda_view<-eda_view[c(1:45),]


ggplot(eda_view,aes(reorder(대진팀,-mean),mean,group=대진팀2,fill=대진팀2))+
  geom_bar(stat="identity",position="dodge",colour="black",width=0.75)+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(name = "조회수")+
  scale_x_discrete(name = "Home팀")+
  guides(fill=guide_legend(title="Away팀"))

ggplot(eda_view2,aes(reorder(대진팀,-mean),mean,group=대진팀2,fill=대진팀2))+
  geom_bar(stat="identity",position="dodge",colour="black",width=0.75)+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(name = "조회수")+
  scale_x_discrete(name = "Home팀")+
  guides(fill=guide_legend(title="Away팀"))

#댓글수
eda_com<-com6 %>% group_by(대진팀,대진팀2) %>% summarise(mean=mean(총댓글수))
eda_com<-as.data.frame(eda_com)
str(eda_com)
eda_com2<-eda_com[c(46:90),]
eda_com<-eda_com[c(1:45),]

ggplot(eda_com,aes(reorder(대진팀,-mean),mean,group=대진팀2,fill=대진팀2))+
  geom_bar(stat="identity",position="dodge",colour="black",width=0.75)+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(name = "댓글수")+
  scale_x_discrete(name = "Home팀")+
  guides(fill=guide_legend(title="Away팀"))

ggplot(eda_com2,aes(reorder(대진팀,-mean),mean,group=대진팀2,fill=대진팀2))+
  geom_bar(stat="identity",position="dodge",colour="black",width=0.75)+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(name = "댓글수")+
  scale_x_discrete(name = "Home팀")+
  guides(fill=guide_legend(title="Away팀"))

#팀별 홈팀일 때 평균 댓글 수
eda_homecom<-com6 %>% group_by(대진팀) %>% summarise(mean=mean(home댓글수))
eda_homecom<-as.data.frame(eda_homecom)
str(eda_homecom)

ggplot(eda_homecom,aes(대진팀,mean))+
  geom_bar(stat="identity",fill="skyblue")+
  scale_y_continuous(name = "댓글수")+
  scale_x_discrete(name = "Home팀")
  
#팀별 어웨이팀일 때 평균 댓글 수
eda_awaycom<-com6 %>% group_by(대진팀2) %>% summarise(mean=mean(away댓글수))
eda_awaycom<-as.data.frame(eda_awaycom)
str(eda_awaycom)

ggplot(eda_awaycom,aes(대진팀2,mean))+
  geom_bar(stat="identity",fill="skyblue")+
  scale_y_continuous(name = "댓글수")+
  scale_x_discrete(name = "away팀")


eda_time<-com6 %>% group_by(대진팀,시간) %>% summarise(mean=sum(관중))
str(eda_time)
eda_time<-as.data.frame(eda_time)
View(eda_time)
eda_time$시간<-as.factor(eda_time$시간)

ggplot(eda_time,aes(reorder(대진팀,-mean),mean,group=시간,fill=시간))+
  geom_bar(stat="identity",position="dodge",colour="black",width=0.75)+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(name = "관중수")+
  scale_x_discrete(name = "팀")+
  guides(fill=guide_legend(title="시간대"))

#장소는 sum해야함
unique(com6$장소)
eda_spot<-com6 %>% group_by(대진팀,지역) %>% summarise(mean=sum(관중))
View(eda_spot)
ggplot(eda_spot,aes(reorder(대진팀,-mean),mean,group=지역,fill=지역))+
  geom_bar(stat="identity",position="dodge",colour="black",width=0.75)+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(name = "관중수")+
  scale_x_discrete(name = "팀")+
  guides(fill=guide_legend(title="지역"))

#이것은 sum해야함 -지역별 관중수   ----인사이트를 어떻게 도출할까 sum과 mean
eda_spot2<-com6 %>% group_by(지역) %>% summarise(mean=sum(관중))
ggplot(eda_spot2,aes(reorder(지역,-mean),mean))+
  geom_bar(stat="identity",fill="skyblue")+
  scale_y_continuous(name = "관중수")+
  scale_x_discrete(name = "지역")

ggplot(com6, aes(x = 지역, y = 관중,fill=지역)) +
  geom_boxplot(size=1, aes(group=지역))




#지역별시간대별
eda_spott<-com6 %>% group_by(지역,시간) %>% summarise(mean=sum(관중))
ggplot(eda_spott,aes(reorder(지역,-mean),mean,group=시간,fill=시간))+
  geom_bar(stat="identity",position="dodge",colour="black",width=0.75)+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(name = "관중수")+
  scale_x_discrete(name = "지역")+
  guides(fill=guide_legend(title="시간대"))

#미세먼지
com6 %>% group_by(특보) %>% summarise(mean= mean(관중))
ggplot(com6, aes(x = 특보, y = 관중,fill=특보)) +
  geom_boxplot(size=1, aes(group=특보))

#미세먼지농도
ggplot(com6,aes(x=미세먼지농도,y=관중))+
  geom_point(color="#FF0099")+
  stat_smooth(color='#0000CC', fill='grey',method="lm")

#기온
ggplot(com6,aes(x=기온,y=관중))+
  geom_point(color="#FF0099")+
  stat_smooth(color='#0000CC', fill='grey')

#최저기온
ggplot(com6,aes(x=최저기온,y=관중))+
  geom_point(color="#FF0099")+
  stat_smooth(color='#0000CC', fill='grey')
#한파
ggplot(com6, aes(x = 한파, y = 관중,fill=한파)) +
  geom_boxplot(size=1, aes(group=한파))

#요일
com6 %>% group_by(특보) %>% summarise(mean= mean(관중))
ggplot(com6, aes(x = 요일dum, y = 관중,fill=요일dum)) +
  geom_boxplot(size=1, aes(group=요일dum))
str(com6)
str(co7)

#라이벌
ggplot(com6, aes(x = rival, y = 관중,fill=rival)) +
  geom_boxplot(size=1, aes(group=rival))
ggplot(com77, aes(x = rival, y = 관중,fill=rival)) +
  geom_boxplot(size=1, aes(group=rival))




#홈팀과어웨이팀의 경기에서 어떤팀이 더 주목을 받는지 댓글수,검색량로 시각화
str(com6)
homecomment<-com6[,c(2,35)]
awaycomment<-com6[,c(3,36)]
homecomment$HA<-"HOME"
awaycomment$HA<-"away"

colnames(homecomment)<-c("팀","검색량","HA")
colnames(awaycomment)<-c("팀","검색량","HA")

homecomment_a<-homecomment %>% group_by(팀,HA) %>% summarise(mean=mean(검색량))
awaycomment_a<-awaycomment %>% group_by(팀,HA) %>% summarise(mean=mean(검색량))
str(homecomment_a)

hoawcomment<-rbind(homecomment_a,awaycomment_a)
hoawcomment<-as.data.frame(hoawcomment)
str(hoawcomment)
#홈팀어웨이 일때 팀별 댓글수 차이
ggplot(hoawcomment,aes(x=팀,y=mean,group=HA,color=HA))+
  geom_line()

#홈팀어웨이 일때 팀별 검색량 차이
str(com6)
homecomment2<-com6[,c(2,41)]
awaycomment2<-com6[,c(3,42)]
homecomment2$HA<-"HOME"
awaycomment2$HA<-"away"

colnames(homecomment2)<-c("팀","댓글수","HA")
colnames(awaycomment2)<-c("팀","댓글수","HA")

homecomment2_a<-homecomment2 %>% group_by(팀,HA) %>% summarise(mean=mean(댓글수))
awaycomment2_a<-awaycomment2 %>% group_by(팀,HA) %>% summarise(mean=mean(댓글수))
str(homecomment2)

hoawcomment2<-rbind(homecomment2_a,awaycomment2_a)
hoawcomment2<-as.data.frame(hoawcomment2)

ggplot(hoawcomment2,aes(x=팀,y=mean,group=HA,color=HA))+
  geom_line()

#
ggplot(com6,aes(x=관중,y=총댓글수))+
  geom_point(color="#FF0099")+
  stat_smooth(color='#0000CC', fill='grey')


