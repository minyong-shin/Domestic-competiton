str(com6)
View(com6)

com6$tsearch<-(com6$search+com6$search2)
com6$tsw<-(com6$sw+com6$sw2)

ho<-com6[,c(1,2,3,5,9,43,44,45,46)]
str(ho)

bk3<-merge(x=bk,y=bk2,by.x="번호",by.y="game_no")
str(bk3)

##########1718경기내부기록
bk3_b<-bk3 %>% group_by(대진팀,대진팀2,번호) %>% 
  summarise(threep=sum(threep),threep_a=sum(threep_a),
            threepr=(sum(threep)/sum(threep_a))*100,
            fg=sum(fg),fg_a=sum(fg_a),
            ft=sum(ft),ft_a=sum(ft_a),
            dk=sum(dk),dk_a=sum(dk_a),
            pp=sum(pp),pp_a=sum(pp_a),
            o_r=sum(o_r),d_r=sum(d_r),
            a_s=sum(a_s),b_s=sum(b_s),
            s_t=sum(s_t),gd=sum(gd),
            t_o=sum(t_o),wft=sum(wft),
            woft=sum(woft),idf=sum(idf),
            tf=sum(tf),ef=sum(ef),foul_tot=sum(foul_tot),
            fb=sum(fb),p_score=sum(p_score),
            jumsu=sum(score),fo=sum(fo))

str(bk3_b)

ho_a<-merge(x=ho,y=bk3_b,by.x=c("대진팀","대진팀2","번호"),by.y=c("대진팀","대진팀2","번호"),all.x=T)
str(ho_a)
str(tw2_a)
str(co8_a)
ho_a$일자<-as.Date(ho_a$일자)
ho_b<-merge(x= ho_a , y= tw2_a , by.x="일자" , by.y="time" , all.x=T)
str(ho_b)

####트윗빈도+sns
dc1718<-data.frame(fread("1718DC.csv"))

str(dc1718)
dc21718<-data.frame(fread("1718DC2.csv"))
str(dc21718)
View(dc21718)
dc21718<-na.omit(dc21718)
sum(is.na(dc21718))

dc_a78<-dc1718 %>% group_by(date) %>% summarise(총컴= n())
dc_a278<-dc21718 %>% group_by(date) %>% summarise(총컴= n())

dc_a278$date<-as.Date(dc_a278$date)

dc_a278[13,2]<-1783
dc_a78<-dc_a78[-1,]


dc_78<-rbind(dc_a278,dc_a78)

View(dc_78)

dc_a78$date<-as.Date(dc_a78$date)

ho_c<-merge(x=ho_b,y=dc_78,by.x="일자",by.y="date",all.x=T) 

sum(is.na(ho_c))
str(ho_c)

ho_c$snsdc<-(ho_c$트윗빈도+ho_c$총컴)
str(ho_d)
ho_d<-ho_c[,-c(39,38)]

##################1718시즌 총조회수
zk<-data.frame(이름=character(0),범주=numeric(0), mean=numeric(0))
str(zk)

#dk_r뺀것
ho_a<-ho_a[,-21]
str(ho_d)

for(i in 5:38){
  yy= data.frame(tview=ho_d$tview,이름=colnames(ho_d[i]),범주=ifelse(ho_d[,i]>mean(ho_d[,i]),1,0))
  zz = yy %>% group_by(이름,범주) %>% summarise(mean=sum(tview)) 
  zk = bind_rows(zk,zz)
}

str(zk)
View(zk)
sum(is.na(zk))
zk<-zk[-c(5,6),]
zk$범주<-as.factor(zk$범주)
windows()

#fb,idf,tf를 빼야함
ggplot(zk,aes(이름,mean,group=범주,color=범주))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))

zk2 <- zk[-c(56,49,51,50),]
windows()
ggplot(zk2,aes(이름,mean,group=범주,color=범주))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))

##########조회수 그래프를 좀 더 이쁘게
#103000 -> 모든 그래프에서 tf와 sw는 너무 어긋나서 뺌
zk22<-subset(zk2,범주=="0")
zk22$조회수합<-9200000

zk222<-subset(zk2,범주=="1")
zk222$조회수합<-zk222$mean

zk2222<-rbind(zk222,zk22)

str(zk2222)
View(zk2222)
windows()
zk2222$범주<-ifelse(zk2222$범주==0,"평균기준선","평균보다 높음")
zk2222$범주<-factor(zk2222$범주,levels=c("평균기준선","평균보다 높음"))

ggplot(zk2222,aes(이름,조회수합,group=범주,color=범주))+
  geom_line(size=1.3)+
  geom_point(size=2)+
  theme(axis.text.x = element_text(angle=30, hjust=1))+
  scale_y_continuous(name = "조회수빈도",limits=c(5000000,13000000))+
  ggtitle("17-18시즌 : 각 변수별 평균적으로 높고 낮을 때의 조회수 빈도 차이 ")+
  scale_x_discrete(name = "변수")+
  theme(plot.title = element_text(size = 20, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 15, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

#############################조회수 glm
str(ho_d)
ho_d$viewfac<-ifelse(ho_d$tview>mean(ho_d$tview),1,0)
table(co8_a$viewfac)
ho_d$viewfac<-factor(ho_d$viewfac,levels=c(0,1))
ho_d1<-ho_d[,-c(1:4)]

str(ho_d111)
ho_d11<-ho_d1[,-3]
ho_d111<-ho_d11[,-c(29,25)]


hod1_f<-glm(viewfac~.,data = ho_d111,family=binomial)
summary(hod1_f)
exp(0.0415)

library(caret)
confusionMatrix(ho_d11$viewfac,as.factor(as.numeric(hod1_f$fitted.values>0.35)))
#73% /0.007

##################1718시즌 총댓글수
yk<-data.frame(이름=character(0),범주=numeric(0), mean=numeric(0))
str(yk)
str(ho_d)
ho_d<-ho_d[,-39]

for(i in 5:38){
  yyy= data.frame(댓글수=ho_d$총댓글수,이름=colnames(ho_d[i]),범주=ifelse(ho_d[,i]>mean(ho_d[,i]),1,0))
  zzz = yyy %>% group_by(이름,범주) %>% summarise(mean=sum(댓글수)) 
  yk = bind_rows(yk,zzz)
}

sum(is.na(yk))
View(yk)
yk<-yk[-c(3,4),]
yk$범주<-as.factor(yk$범주)
windows()

#fb,idf,tf,tsw를 빼야함
ggplot(yk,aes(이름,mean,group=범주,color=범주))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))

yk2 <- yk[-c(56,49,51,50,7,8),]
windows()
ggplot(yk2,aes(이름,mean,group=범주,color=범주))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))

##########조회수 그래프를 좀 더 이쁘게
#103000 -> 모든 그래프에서 tf와 sw는 너무 어긋나서 뺌
yk22<-subset(yk2,범주=="0")
yk22$조회수합<-95100

yk222<-subset(yk2,범주=="1")
yk222$조회수합<-yk222$mean

yk2222<-rbind(yk222,yk22)

str(yk2222)
View(yk2222)

yk2222$범주<-ifelse(yk2222$범주==0,"평균기준선","평균보다 높음")
yk2222$범주<-factor(yk2222$범주,levels=c("평균기준선","평균보다 높음"))

ggplot(yk2222,aes(이름,조회수합,group=범주,color=범주))+
  geom_line(size=1.3)+
  geom_point(size=2)+
  theme(axis.text.x = element_text(angle=30, hjust=1))+
  scale_y_continuous(name = "댓글빈도",limits=c(40000,120000))+
  ggtitle("17-18시즌 : 각 변수별 평균적으로 높고 낮을 때의 댓글빈도 차이 ")+
  scale_x_discrete(name = "변수")+
  theme(plot.title = element_text(size = 20, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 15, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

###################댓글수 glm
str(ho_d)
ho_d$댓글fac<-ifelse(ho_d$총댓글수>mean(ho_d$총댓글수),1,0)

ho_d$댓글fac<-factor(ho_d$댓글fac,levels=c(0,1))
ho_d2<-ho_d[,-c(1:4)]

str(ho_d22)
ho_d22<-ho_d2[,-c(2,26,30)]


#음수면 
hod2_f<-glm(댓글fac~.,data = ho_d22,family=binomial)
summary(hod2_f)
exp(-0.1568)

library(caret)
confusionMatrix(ho_d222$댓글fac,as.factor(as.numeric(hod2_f$fitted.values>0.5)))
#73% /0.007


##################dc/sns
xz<-data.frame(이름=character(0),범주=numeric(0), mean=numeric(0))
str(ho_d)
ho_d<-ho_d[,-39]

for(i in 5:38){
  yyyy= data.frame(sns=ho_d$snsdc,이름=colnames(ho_d[i]),범주=ifelse(ho_d[,i]>mean(ho_d[,i]),1,0))
  zzzz = yyyy %>% group_by(이름,범주) %>% summarise(mean=sum(sns)) 
  xz = bind_rows(xz,zzzz)
}


xz$범주<-as.factor(xz$범주)
View(xz)
str(xz)
xz<-xz[-c(66,65),]

ggplot(xz,aes(이름,mean,group=범주,color=범주))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))

xz2 <- xz[-c(58,53,52,51,9,10),]

ggplot(xz2,aes(이름,mean,group=범주,color=범주))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))


windows()
#16800
xz22<-subset(xz2,범주=="0")
xz22$sns합<-275387
xz222<-subset(xz2,범주=="1")
xz222$sns합<-xz222$mean

xz2222<-rbind(xz22,xz222)
str(xz2222)
View(xz2222)

xz2222$범주<-ifelse(xz2222$범주==0,"평균기준선","평균보다 높음")
xz2222$범주<-factor(xz2222$범주,levels=c("평균기준선","평균보다 높음"))


ggplot(xz2222,aes(이름,sns합,group=범주,color=범주))+
  geom_line(size=1.3)+
  geom_point(size=2)+
  theme(axis.text.x = element_text(angle=30, hjust=1))+
  scale_y_continuous(name = "sns+커뮤니티빈도수",limits=c(180000,350000))+
  ggtitle("17-18시즌 : 각 변수별 평균적으로 높고 낮을 때의 sns+커뮤니티 빈도수 차이 ")+
  scale_x_discrete(name = "변수")+
  theme(plot.title = element_text(size = 20, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 15, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

#####################커뮤니티 glm
str(ho_d)
ho_d$dcfac<-ifelse(ho_d$snsdc>mean(ho_d$snsdc),1,0)

ho_d$dcfac<-factor(ho_d$dcfac,levels=c(0,1))
ho_d3<-ho_d[,-c(1:4)]

str(ho_d3)
ho_d33<-ho_d3[,-c(34,30,26)]


hod3_f<-glm(dcfac~.,data = ho_d33,family=binomial)
summary(hod3_f)
exp(-0.1005)
step(hod3_f,direction="both")

library(caret)
confusionMatrix(ho_d333$dcfac,as.factor(as.numeric(hod3_f$fitted.values>0.4)))



#######################################긍부정
play<-data.frame(fread("play2.csv"))
str(play)
play$긍부정<-ifelse(play$positive-play$negative>=0,1,0)

play16<-subset(play,season_code=="29")
play18<-subset(play,season_code=="31")
str(play16)
str(play18)

#count = 관중수
play16_a<-play16[,-c(1,2,3,4,5,38,37,36,35,33,32,41,40)]
play18_a<-play18[,-c(1,2,3,4,5,38,37,36,35,33,32,41,40)]

str(play18_a)

play18_a$긍부정<-factor(play18_a$긍부정,levels=c(0,1))
pn_model<-glm(긍부정~.,play18_a,family="binomial")
summary(pn_model)
step(pn_model,direction="both")

confusionMatrix(play18_a$긍부정,as.factor(as.numeric(pn_model$fitted.values>0.155)))


play18_b<-play18 %>% group_by(game_no) %>% summarise(posum=sum(positive),nesum=sum(negative))
play18_b$game_no <- play18_b$game_no-270


str(ho_d)
str(play18_b)
View(play18_b)

play18_c<-merge(x=ho_d, y=play18_b ,by.x="번호",by.y="game_no",all.x=T)
str(play18_c)
play18_c$긍부정도 <- ifelse(play18_c$posum-play5$nesum>=0,1,0)
play18_c$긍부정도<-factor(play18_c$긍부정도,levels=c(0,1))

windows()
ggplot(play18_c, aes(x =긍부정도 , y = snsdc,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))















