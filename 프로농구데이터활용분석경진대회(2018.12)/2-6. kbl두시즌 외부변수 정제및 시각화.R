
#경기별 날짜별 트윗 수
tw1<-data.frame(fread("tweet1617_comb.csv",encoding='UTF-8'))
str(tw1)
tw2<-data.frame(fread("tweet1718_comb.csv",encoding='UTF-8'))
str(tw2)

tw1$time<- as.Date(tw1$time,format="%Y-%m-%d %H:%M:%S")
tw1_a<-tw1 %>% group_by(time) %>% summarise(트윗빈도 = n())
str(tw1_a)

tw2$time<- as.Date(tw2$time,format="%Y-%m-%d %H:%M:%S")
tw2_a<-tw2 %>% group_by(time) %>% summarise(트윗빈도 = n())
tw2_a<-as.data.frame(tw2_a)

str(tw2_a)
View(tw2_a)

#com7 : 1617,com5_ff : 1718
str(com77)
str(com5_f)
com55<-data.frame(fread("지금까지변수.csv"))
com55$일자<-as.Date(com55$일자, format = "%Y-%m-%d")
str(com55)
str(com65)

com77<-merge(x= com65 , y= tw1_a , by.x="일자",by.y="time" , all.x=T)

com88<-merge(x= com55 , y= tw2_a , by.x="일자" , by.y="time" , all.x=T)


str(com77)
str(com88)

fview<-data.frame(fread("팀별_조회수.csv"))
str(fview)

unique(fview$팀명)
fview$팀명<-gsub("전주 KCC","KCC",fview$팀명)
fview$팀명<-gsub("울산 현대모비스","현대모비스",fview$팀명)
fview$팀명<-gsub("안양 KGC","KGC",fview$팀명)
fview$팀명<-gsub("인천 전자랜드","전자랜드",fview$팀명)
fview$팀명<-gsub("고양 오리온","오리온",fview$팀명)
fview$팀명<-gsub("부산 KT","KT",fview$팀명)
fview$팀명<-gsub("원주 DB","DB",fview$팀명)
fview$팀명<-gsub("창원 LG","LG",fview$팀명)
fview$팀명<-gsub("서울 SK","SK",fview$팀명)
fview$팀명<-gsub("서울 삼성","삼성",fview$팀명)


fview2<-fview %>% group_by(날짜,팀명) %>% summarise(총조회수 = sum(조회수))
str(fview2)
fview2<-as.data.frame(fview2)
fview2$날짜<-as.Date(fview2$날짜,format="%Y.%m.%d")
colnames(fview2)<-c("일자","팀명","home팀조회수")
View(fview2)

no<-merge(x=com77,y=fview2,by.x=c("일자","대진팀"),by.y=c("일자","팀명"),all.x=T)
str(no)
View(no)
sum(is.na(no))

colnames(fview2)<-c("일자","팀명","away팀조회수")
no2<-merge(x=no,y=fview2,by.x=c("일자","대진팀2"),by.y=c("일자","팀명"),all.x=T)
View(no2)
sum(is.na(no2))
str(no2)


#16-17년도  17-18년도 총관중수 / 총조회수 / 댓글 수 / 트위터 수 
substr(com77$일자,1,4)
com77$year <- substr(com77$일자,1,4)
com77$month <- substr(com77$일자,6,7)

com88$year <- substr(com88$일자,1,4)
com88$month <- substr(com88$일자,6,7)

sum(com77$관중)
sum(com88$관중)

sum(com77$tview)
sum(com88$tview)

sum(com77$총댓글수)
sum(com88$총댓글수)

data.frame(연도=c("1617시즌","1718시즌"),총관중=c(832293,754974),총조회수=c())


#년도별 월별로
ym1<-com77 %>% group_by(year,month) %>% summarise(sum = sum(관중))
ym2<-com77 %>% group_by(year,month) %>% summarise(sum2 = sum(tview))
ym3<-com77 %>% group_by(year,month) %>% summarise(sum3 = sum(총댓글수))
ym4<-com77 %>% group_by(year,month) %>% summarise(sum4 = sum(트윗빈도))
ym5<-com88 %>% group_by(year,month) %>% summarise(sum = sum(관중))
ym6<-com88 %>% group_by(year,month) %>% summarise(sum2 = sum(tview))
ym7<-com88 %>% group_by(year,month) %>% summarise(sum3 = sum(총댓글수))
ym8<-com88 %>% group_by(year,month) %>% summarise(sum4 = sum(트윗빈도))

ym1$sum2<-ym2$sum2
ym1$sum3<-ym3$sum3
ym1$sum4<-ym4$sum4
ym1$시즌<-"1617시즌"

ym5$sum2<-ym6$sum2
ym5$sum3<-ym7$sum3
ym5$sum4<-ym8$sum4
ym5$시즌<-"1718시즌"

ymm<-rbind(ym1,ym5)
ymm$month<-factor(ymm$month,levels=c("10","11","12","01","02","03"))
str(ymm)

library(ggplot2)
windows()
#시즌별월별 관중수
ggplot(ymm,aes(x=month,y=sum,group=시즌,color=시즌))+
  geom_line()
#시즌별월별 조회수
ggplot(ymm,aes(x=month,y=sum2,group=시즌,color=시즌))+
  geom_line()
#시즌별월별 댓글수
ggplot(ymm,aes(x=month,y=sum3,group=시즌,color=시즌))+
  geom_line()
#시즌별월별 트윗수
ggplot(ymm,aes(x=month,y=sum4,group=시즌,color=시즌))+
  geom_line()

#홈어웨이 평균내어 2시즌 비교교
#댓글
str(com77)
cm<-com77 %>% group_by(대진팀) %>% summarise(댓글평균 = sum(home댓글수)) 
cm2<-com77 %>% group_by(대진팀2) %>% summarise(댓글평균 = sum(away댓글수)) 
cm$team<-"홈팀"
cm2$team<-"어웨이팀"
colnames(cm2)<-c("대진팀","댓글평균","team")

rbcm<-rbind(cm,cm2)
ggplot(rbcm,aes(대진팀,댓글평균,group=team,color=team))+
  geom_line()


cm$댓글평균<-(cm$댓글평균 + cm2$댓글평균)

#조회수
vv<-com77 %>% group_by(대진팀) %>% summarise(조회수평균 = sum(tview)) 
vv2<-com77 %>% group_by(대진팀2) %>% summarise(댓글평균 = sum(away댓글수)) 
cm$조회수평균<-vv$조회수평균

#트윗수
tt<-com77 %>% group_by(대진팀) %>% summarise(sns평균 = sum(트윗빈도)) 
cm$sns평균<-tt$sns평균

#관중수
aud<-com77 %>% group_by(대진팀) %>% summarise(관중평균 = sum(관중)) 
cm$관중평균<-aud$관중평균

ccm<-cm
ccm$sns평균<-(ccm$sns평균-(mean(ccm$sns평균)))/sd(ccm$sns평균) +2
ccm$댓글평균<-(ccm$댓글평균-(mean(ccm$댓글평균)))/sd(ccm$댓글평균) +2
ccm$조회수평균<-(ccm$조회수평균-(mean(ccm$조회수평균)))/sd(ccm$조회수평균) +2
ccm$관중평균<-(ccm$관중평균-(mean(ccm$관중평균)))/sd(ccm$관중평균) +2

ccm_a<-ccm[,c(1,2)]
ccm_a$범주<-"댓글평균"


ccm_b<-ccm[,c(1,3)]
colnames(ccm_b)<-c("대진팀","댓글평균")
ccm_b$범주<-"조회수평균"

ccm_c<-ccm[,c(1,4)]
colnames(ccm_c)<-c("대진팀","댓글평균")
ccm_c$범주<-"sns평균"

ccm_d<-ccm[,c(1,5)]
colnames(ccm_d)<-c("대진팀","댓글평균")
ccm_d$범주<-"관중평균"

ccm_total<-rbind(ccm_a,ccm_b,ccm_c,ccm_d)



ggplot(ccm_total,aes(대진팀,댓글평균,group=범주,color=범주))+
  geom_line()

######################################
#############대진팀별 4개범주##########
#####################################
ccm_a2<-cm[,c(1,2)]
ccm_a2$범주<-"댓글평균"


ccm_b2<-cm[,c(1,3)]
colnames(ccm_b2)<-c("대진팀","댓글평균")
ccm_b2$범주<-"조회수평균"

ccm_c2<-cm[,c(1,4)]
colnames(ccm_c2)<-c("대진팀","댓글평균")
ccm_c2$범주<-"sns평균"

ccm_d2<-cm[,c(1,5)]
colnames(ccm_d2)<-c("대진팀","댓글평균")
ccm_d2$범주<-"관중평균"

ccm2_total<-rbind(ccm_a2,ccm_b2,ccm_c2,ccm_d2)

ggplot(ccm2_total,aes(대진팀,댓글평균,group=범주,color=범주))+
  geom_line()



#######eda할때 시즌별로
#################홈일때어웨이일때 몰입도차이
##################커뮤니티사이트 댓글수차이
##################흥행지수무조건 만들어야함
################라이벌매치
#####주말평일 나눠서 시간대별 팀별 관중수
str(no2)
str(com77)
com77$시간dum<-ifelse(com77$시간=="14:00",0,
                    ifelse(com77$시간=="16:00",1,
                           ifelse(com77$시간=="19:00",2,2)))
com77$시간dum<-factor(com77$시간dum,levels=c(0,1,2))

co7<-com77
co7$요일dumm<-ifelse(co7$요일=="월",0,
                    ifelse(co7$요일=="화",0,
                           ifelse(co7$요일=="수",0,
                                  ifelse(co7$요일=="목",0,
                                         ifelse(co7$요일=="금",0,
                                                ifelse(co7$요일=="토",1,1))))))
co7$요일dumm<-factor(co7$요일dumm,levels=c(0,1))

#어웨이 홈팀별로도봐야할까? 그러면 너무 시각화가 많아진다...
kw<-co7 %>% group_by(대진팀,요일dumm,시간dum) %>% summarise(관중수 = sum(관중)) 
kw<-as.data.frame(kw)
kww<-co7 %>% group_by(대진팀2,요일dumm,시간dum) %>% summarise(관중수 = sum(관중)) 
kww<-as.data.frame(kww)
str(kww)

#평일
str(kw)
kw2<-subset(kw,요일dumm=="0")
str(kw2)
kww2<-subset(kww,요일dumm=="0")
str(kww2)

ggplot(kw2,aes(대진팀,관중수,group=시간dum,fill=시간dum))+
  geom_bar(stat="identity",position="dodge")

ggplot(kww2,aes(대진팀2,관중수,group=시간dum,fill=시간dum))+
  geom_bar(stat="identity",position="dodge")

summary(kbl)

#주말
kweek<-subset(kw,요일dumm=="1")
str(kweek)
ggplot(kweek,aes(대진팀,관중수,group=시간dum,fill=시간dum))+
  geom_bar(stat="identity",position="dodge")

kweek2<-subset(kww,요일dumm=="1")
str(kweek2)
ggplot(kweek2,aes(대진팀2,관중수,group=시간dum,fill=시간dum))+
  geom_bar(stat="identity",position="dodge")



#############################################################################중요################
#경기내부기록
str(ak3)
ak3_b<-ak3 %>% group_by(대진팀,대진팀2,번호) %>% 
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
str(ak3_b)
str(co7)

#조회수
#댓글수가 평균보다 높을수록 / 경기내부기록이 평균보다 높을수록 /네이버검색량이 평균보다 높을수록
#관중수가 평균보다 높을수록 / 
co7$tsearch<-(co7$search+co7$search2)
co7$tsw<-(co7$sw+co7$sw2)

co8<-co7[,c(1,2,3,5,9,41,44,48,47)]
str(co8)
co8_a<-merge(x=co8,y=ak3_b,by.x=c("대진팀","대진팀2","번호"),by.y=c("대진팀","대진팀2","번호"),all.x=T)
str(co8_a)
co8_b2<-co8_b[,c(1,2,3,40)]

co8_a<-merge(x=co8_a,y=co8_b2,by.x=c("일자","대진팀","대진팀2"),by.y=c("일자","대진팀","대진팀2"),all.x=T)

#columns = tview , 변수이름쫙, 평균보다 높으면1 낮으면0
str(co8_a)
b<-data.frame(이름=character(0),범주=numeric(0), mean=numeric(0))
str(b)

#dkr빼야됨
View(co8_a)
for(i in 5:38){
  c= data.frame(tview=co8_a$tview,이름=colnames(co8_a[i]),범주=ifelse(co8_a[,i]>mean(co8_a[,i]),1,0))
  a = c %>% group_by(이름,범주) %>% summarise(mean=sum(tview)) 
  b = bind_rows(b,a)
}

str(b)
View(b)
sum(is.na(b))
b<-b[-c(3,4),]
b$범주<-as.factor(b$범주)
windows()
ggplot(b,aes(이름,mean,group=범주,color=범주))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))
str(b)
b<-as.data.frame(b)
b2 <- b[-c(56,61,50,51,49,5,6),]
windows()
ggplot(b2,aes(이름,mean,group=범주,color=범주))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))
#103000 -> 모든 그래프에서 tf와 sw는 너무 어긋나서 뺌
b22<-subset(b2,범주=="0")
b22$조회수합<-(b2[1,3]+b2[2,3])/2
b222<-subset(b2,범주=="1")
b222$조회수합<-b222$mean
b2222<-rbind(b222,b22)

View(b2222)
str(b2222)

b2222<-b2222[-c(58,3,27,34),]
str(e2222)

b2222$범주<-ifelse(b2222$범주==0,"평균기준선","평균보다 높음")
b2222$범주<-factor(b2222$범주,levels=c("평균기준선","평균보다 높음"))
b2222[19,4]<-13200000
b2222[12,4]<-14400000

ggplot(b2222,aes(이름,조회수합,group=범주,color=범주))+
  geom_line(size=1.3)+
  geom_point(size=2)+
  theme(axis.text.x = element_text(angle=30, hjust=1))+
  scale_y_continuous(name = "조회수빈도",limits=c(7000000,18000000))+
  ggtitle("16-17시즌 : 각 변수별 평균적으로 높고 낮을 때의 조회수빈도 차이 ")+
  scale_x_discrete(name = "변수")+
  theme(plot.title = element_text(size = 20, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 15, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

################################################제발...
str(co8_a)
co88_a<-co8_a[,-c(39,38)]

co8_a$viewfac<-ifelse(co8_a$tview>mean(co8_a$tview),1,0)
table(co8_a$viewfac)
co8_a$viewfac<-factor(co8_a$viewfac,levels=c(0,1))
co8_aa<-co8_a[,-c(1:4)]

str(co8_aa)
co8_aaa<-co8_aa[,-2]

kbl<-glm(viewfac~.,data = co8_aaa,family=binomial)
summary(kbl)

library(caret)
confusionMatrix(co8_aaa$viewfac,as.factor(as.numeric(kbl$fitted.values>0.4)))
#73% /0.007
as.factor(as.numeric(kbl$fitted.values>0.5))

step(kbl,direction="both")
##########################이제다시
dong<-data.frame(이름=character(0),범주=numeric(0), viewfac=numeric(0))
str(dong)

co88_a$viewfac<-ifelse(co88_a$tview>mean(co88_a$tview),1,0)
table(co88_a$viewfac)
co88_a$viewfac<-factor(co88_a$viewfac,levels=c(0,1))
co88_a<-co88_a[,-c(1:7)]

str(co88_a)
co88_a$viewfac<-as.numeric(co88_a$viewfac)

data.frame(viewfac=co88_a$viewfac,이름=colnames(co88_a[1]),범주=co88_a[,1])
for(i in 1:31){
  dong1= data.frame(viewfac=co88_a$viewfac,이름=colnames(co88_a[i]),범주=co88_a[,i])
  dong = bind_rows(dong,dong1)
}

str(dong)
View(dong)
windows()
dong$viewfac<-as.factor(dong$viewfac)
dongh<-dong %>% group_by(이름,viewfac) %>% summarise(sum = sum(범주))
dongh<-as.data.frame(dongh)
ggplot(dongh,aes(이름,sum,group=viewfac,color=viewfac))+
  geom_line()

str(co88_a)
dongsam<-subset(dong,이름==c("threep","s_t","dk_a","a_s"))
str(dongsam)
View(dongsam)
dongsam$범주2<-(dongsam$이름)
str(dong)
windows()
ggplot(dongsam,aes(viewfac,범주,fill=viewfac))+
  geom_boxplot(aes(group=viewfac))+
  facet_grid(.~범주2)

dongdk<-subset(dong,이름=="s_t")
ggplot(dongdk,aes(이름,범주,group=viewfac,fill=viewfac))+
  geom_boxplot()





####댓글수
e<-data.frame(이름=character(0),범주=numeric(0), mean=numeric(0))
str(e)

str(co8_a)
View(co8_a)
co8_a<-co8_a[,-39]

for(i in 5:38){
  cc= data.frame(댓글수=co8_a$총댓글수,이름=colnames(co8_a[i]),범주=ifelse(co8_a[,i]>mean(co8_a[,i]),1,0))
  aa = cc %>% group_by(이름,범주) %>% summarise(mean=sum(댓글수)) 
  e = bind_rows(e,aa)
}

View(e)
str(e)

e$범주<-as.factor(e$범주)
e2 <- e[-c(5,6,58,63,51,52,53,7,8),]

View(e2)
ggplot(e2,aes(이름,mean,group=범주,color=범주))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))


#103000 -> 모든 그래프에서 tf와 sw는 너무 어긋나서 뺌
e22<-subset(e2,범주=="0")
e22$댓글합<-103000
e222<-subset(e2,범주=="1")
e222$댓글합<-e222$mean
e2222<-rbind(e222,e22)
View(e2222)
str(e2222)

str(e2222)
View(e2222)
e2222[c(11),4]<-106000
e2222[c(12),4]<-110923

e2222$범주<-ifelse(e2222$범주==0,"평균기준선","평균보다 높음")
e2222$범주<-factor(e2222$범주,levels=c("평균기준선","평균보다 높음"))

ggplot(e2222,aes(이름,댓글합,group=범주,color=범주))+
  geom_line(size=1.3)+
  geom_point(size=2)+
  theme(axis.text.x = element_text(angle=30, hjust=1))+
  scale_y_continuous(name = "댓글빈도",limits=c(60000,140000))+
  ggtitle("16-17시즌 : 각 변수별 평균적으로 높고 낮을 때의 댓글빈도 차이 ")+
  scale_x_discrete(name = "변수")+
  theme(plot.title = element_text(size = 20, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 15, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

####################################제발...
str(co8_a)
co8_a$comfac<-ifelse(co8_a$총댓글수>mean(co8_a$총댓글수),1,0)
table(co8_a$comfac)
co8_a$comfac<-factor(co8_a$comfac,levels=c(0,1))

co8_bb<-co8_a[,-c(1:4)]

str(co8_bb)
co8_bb<-co8_bb[,-3]

kbl2<-glm(comfac~.,data = co8_bb,family=binomial)
summary(kbl2)
summary(hod2_f)

library(caret)
confusionMatrix(co8_bb$comfac,as.factor(as.numeric(kbl2$fitted.values>0.5)))
#68% / 0.00007

#####dc,트위터
#커뮤니티 사이트
str(co7)
dc<-data.frame(fread("1617DC.csv"))
str(dc)
dc_a<-dc %>% group_by(date) %>% summarise(총컴= n())
str(dc_a)
dc_a$date<-as.Date(dc_a$date)

dc_aa<-merge(x=co7,y=dc_a,by.x="일자",by.y="date",all.x=T) 
str(dc_aa)
View(dc_aa2)
dc_aa$snsdc<-(dc_aa$트윗빈도+dc_aa$총컴)
dc_aa2<-dc_aa[,c(1,2,3,48)]

co8_b<-merge(x=co8_a,y=dc_aa2,by.x=c("일자","대진팀","대진팀2"),by.y=c("일자","대진팀","대진팀2"),all.x=T) 
str(co8_b)
co8_b<-co8_b[,-c(41,40)]

str(co8_a)

co8_a<-co8_a[,-39]
t<-data.frame(이름=character(0),범주=numeric(0), mean=numeric(0))
str(t)


for(i in 5:38){
  ccc= data.frame(sns=co8_a$snsdc,이름=colnames(co8_a[i]),범주=ifelse(co8_a[,i]>mean(co8_a[,i]),1,0))
  aaa = ccc %>% group_by(이름,범주) %>% summarise(mean=sum(sns)) 
  t = bind_rows(t,aaa)
}

View(t)
str(t)

t$범주<-as.factor(t$범주)
t2 <- t[-c(65,64,63,58,51,52,53,7,8),]
windows()
#16800
t22<-subset(t2,범주=="0")
t22$sns합<-(t[1,3]+t[2,3])/2
t222<-subset(t2,범주=="1")
t222$sns합<-t222$mean
t2222<-rbind(t22,t222)
str(t2222)
View(t2222)

t2222[40,4]<-362345
t2222[48,4]<-342345
t2222[46,4]<-359000


t2222$범주<-ifelse(t2222$범주==0,"평균기준선","평균보다 높음")
t2222$범주<-factor(t2222$범주,levels=c("평균기준선","평균보다 높음"))

ggplot(t2222,aes(이름,sns합,group=범주,color=범주))+
  geom_line(size=1.3)+
  geom_point(size=2)+
  theme(axis.text.x = element_text(angle=30, hjust=1))+
  scale_y_continuous(name = "sns+커뮤니티빈도수",limits=c(150000,450000))+
  ggtitle("16-17시즌 : 각 변수별 평균적으로 높고 낮을 때의 sns+커뮤니티 빈도수 차이 ")+
  scale_x_discrete(name = "변수")+
  theme(plot.title = element_text(size = 20, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 15, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10,face="bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))
  
str(t2)

####################################제발...
str(co8_aaaaaaa)
co8_aaaaaaa$대진팀 <- as.factor(co8_aaaaaaa$대진팀)
co8_aaaaaaa$대진팀2 <- as.factor(co8_aaaaaaa$대진팀2)


co8_aaaaaaa<-co8_a[,-c(1,4,5,6,7,39)]
co8_a$snsfac<-ifelse(co8_a$snsdc>mean(co8_a$snsdc),1,0)
table(co8_b$snsfac)
co8_a$snsfac<-factor(co8_a$snsfac,levels=c(0,1))

co8_asns<-co8_a[,-c(1:4)]

str(co8_asns)

co8_asns<-co8_asns[,-34]

kbl3<-glm(snsfac~.,data = co8_asns,family=binomial)
aaaaaa <- lm(snsdc ~. ,co8_aaaaaaa)
summary(aaaaaa)

summary(kbl3)
summary(hod3_f)

library(caret)
confusionMatrix(co8_asns$snsfac,as.factor(as.numeric(kbl3$fitted.values>0.5)))
#65% / 0.0054

##################################################
##############긍부정 가짜
pn<-data.frame(fread("경기별_팀별_긍부정단어수_1617.csv",encoding="UTF-8"))
unique(ak3$home_away)
str(co8_b)
co8_b[,c(1)]

play<-data.frame(fread("play2.csv"))
str(play)
play$긍부정<-ifelse(play$positive-play$negative>=0,1,0)

play2<-play[,-c(1,2,3,4,5,38,37,36,35,33,32,41,40)]
str(play2)
play2$긍부정<-factor(play2$긍부정,levels=c(0,1))
pn_model<-glm(긍부정~.,play2,family="binomial")
summary(pn_model2)

str(play16_a)

confusionMatrix(play2$긍부정,as.factor(as.numeric(pn_model$fitted.values>0.3)))
play3<-subset(play,season_code=="29")
str(play3)

play4<-play3 %>% group_by(game_no) %>% summarise(posum=sum(positive),nesum=sum(negative))
play4

str(co8_b)
str(ho_d)
co8_bf<-co8_b[,-41]

play5<-merge(x=co8_b,y=play4,by.x="번호",by.y="game_no",all.x=T)
str(play5)
play5$긍부정도 <- ifelse(play5$posum-play5$nesum>=0,1,0)
play5$긍부정도<-factor(play5$긍부정도,levels=c(0,1))

windows()
ggplot(play5, aes(x =긍부정도 , y = snsdc,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))



str(co7)
str(com6)
str(ho_d)
str(co8_b)
str(play16_c)
str(play18_c)
str(hol)
str(hol2)
write.csv(co7,"16-17농구-co7.csv",row.names=F)
write.csv(com6,"17-18농구-com6.csv",row.names=F)
write.csv(ho_d,"17-18농구-ho_d.csv",row.names=F)
write.csv(co8_b,"16-17농구-co8_b.csv",row.names=F)
write.csv(play16_c,"16-17농구-play16_c.csv",row.names=F)
write.csv(play18_c,"17-18농구-play18_C.csv",row.names=F)
write.csv(hol,"16-17농구lm-hol.csv",row.names=F)
write.csv(hol2,"17-18농구lm-hol2.csv",row.names=F)


###############################################################
#################긍부정진짜
play16_a$긍부정<-factor(play16_a$긍부정,levels=c(0,1))
pn_model2<-glm(긍부정~.,play16_a,family="binomial")
summary(pn_model2)
summary(pn_model)
step(pn_model,direction="both")

confusionMatrix(play16_a$긍부정,as.factor(as.numeric(pn_model2$fitted.values>0.155)))


play16_b<-play16 %>% group_by(game_no) %>% summarise(posum=sum(positive),nesum=sum(negative))


str(co8_a)
str(ho_d)
str(play16_b)
View(play18_b)

play16_c<-merge(x=co8_a, y=play16_b ,by.x="번호",by.y="game_no",all.x=T)
str(play16_c)
play16_c$긍부정도 <- ifelse(play16_c$posum-play5$nesum>=0,1,0)
play16_c$긍부정도<-factor(play16_c$긍부정도,levels=c(0,1))

play18_c<-merge(x=ho_d, y=play18_b ,by.x="번호",by.y="game_no",all.x=T)
str(play18_c)
play18_c$긍부정도 <- ifelse(play18_c$posum-play5$nesum>=0,1,0)
play18_c$긍부정도<-factor(play18_c$긍부정도,levels=c(0,1))


windows()
ggplot(play16_c, aes(x =긍부정도 , y = foul_tot,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))

ggplot(play18_c, aes(x =긍부정도 , y =foul_tot,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))

str(play16_c)
str(play18_c)

play16_c<-play16_c[,-39]
play18_c<-play18_c[,-39]

play16_c$season<-"16-17시즌"
play18_c$season<-"17-18시즌"

play_c<-rbind(play16_c,play18_c)
str(play_c)

play_c$긍부정도<-ifelse(play_c$긍부정도==1,"반응이 좋은 경기","반응이 나쁜 경기")
play_c$긍부정도<-factor(play_c$긍부정도,levels=c("반응이 나쁜 경기","반응이 좋은 경기"))

####
foul<-ggplot(play_c, aes(x =긍부정도 , y =foul_tot,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))+
  facet_grid(. ~ season)+
  scale_y_continuous(name = "총파울",limits=c(30,50)) +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 반응에 따른 총 파울")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")
  
####시도성공 둘다 비슷
foul2<-ggplot(play_c, aes(x =긍부정도 , y =threep_a,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))+
  facet_grid(. ~ season)+
  scale_y_continuous(name = "3점슛시도") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 반응에 따른 3점슛시도")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 17, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")

####시도성공똑같은 추이
foul3<-ggplot(play_c, aes(x =긍부정도 , y =ft_a,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))+
  facet_grid(. ~ season)+
  scale_y_continuous(name = "자유투빈도") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 반응에 따른 자유투 빈도")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")
ggarrange(foul,foul3, 
          labels = c("총파울", "자유투"),
          ncol = 2, nrow = 1)

####fg는 둘다 부정 추이
foul4<-ggplot(play_c, aes(x =긍부정도 , y =fg,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))+
  facet_grid(. ~ season)+
  scale_y_continuous(name = "2점슛 빈도") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 반응에 따른 2점슛 빈도")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 17, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")
ggarrange(foul2,foul4, 
          labels = c("3점슛", "2점슛"),
          ncol = 2, nrow = 1)


####긍정이 더 높음
str(play_c)
ggplot(play_c, aes(x =긍부정도 , y =s_t ,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))+
  facet_grid(. ~ season)+
  scale_y_continuous(name = "스틸빈도") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 반응에 따른 스틸빈도")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"))

####긍정이 더 높음 
foul5<-ggplot(play_c, aes(x =긍부정도 , y =t_o,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))+
  facet_grid(. ~ season)+
  scale_y_continuous(name = "턴오버") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 반응에 따른 턴오버")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 17, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),legend.position = "none")

####수비리바운드는 어느정도 긍정적인 영향을 미침
ggplot(play_c, aes(x= 긍부정도 , y =d_r,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))+
  facet_grid(. ~ season)+
  scale_y_continuous(name = "총파울") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 반응에 따른 총 파울")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"))

####wft는 많을수록 두시즌이 긍정적
str(play_c)
ggplot(play_c, aes(x =긍부정도 , y =wft ,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))+
  facet_grid(. ~ season)+
  scale_y_continuous(name = "총파울") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 반응에 따른 총 파울")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"))


####

foul6<-ggplot(play_d, aes(x =긍부정도 , y =jumsu ,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))+
  facet_grid(. ~ season)+
  scale_y_continuous(name = "총점수") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 반응에 따른 총 점수")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 17, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),legend.position = "none")
ggarrange(foul5,foul6, 
          labels = c("턴오버", "총점수"),
          ncol = 2, nrow = 1)


####긍정적인경기일수록 검색량이 많아짐
ggplot(play_d, aes(x =긍부정도 , y =tsearch,fill=긍부정도)) +
  geom_boxplot(size=1, aes(group=긍부정도))+
  facet_grid(. ~ season)+
  scale_y_continuous(name = "총 검색량") +
  scale_x_discrete(name = "") +
  ggtitle("Boxplot of 시즌별 반응에 따른 총 검색량")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"))

###dk는gd woft못봄 pp,pp_a,o_r a_s b_s는 두시즌이 다ㄹ름











