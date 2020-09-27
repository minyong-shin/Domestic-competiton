library(data.table)
bk <- data.frame(fread("1718경기번호.csv"))
bk2 <- data.frame(fread("1718경기기록.csv"))
unique(bk$대진팀)

#kcc
#전경기
subset(bk,대진팀=="KCC")[-27,c(8)]
#후경기
subset(bk,대진팀=="KCC")[-1,c(8)]

#증감률
((subset(bk,대진팀=="KCC")[-1,c(8)]-subset(bk,대진팀=="KCC")[-27,c(8)])/subset(bk,대진팀=="KCC")[-27,c(8)])*100

kcc<-subset(bk,대진팀=="KCC")
kcc$증감률<-0
kcc[2:27,9]<-((subset(bk,대진팀=="KCC")[-1,c(8)]-subset(bk,대진팀=="KCC")[-27,c(8)])/subset(bk,대진팀=="KCC")[-27,c(8)])*100

#오리온
subset(bk,대진팀=="오리온")[-27,c(8)]
subset(bk,대진팀=="오리온")[-1,c(8)]
((subset(bk,대진팀=="오리온")[-1,c(8)]-subset(bk,대진팀=="오리온")[-27,c(8)])/subset(bk,대진팀=="오리온")[-27,c(8)])*100
ori<-subset(bk,대진팀=="오리온")
ori$증감률<-0
ori[2:27,9]<-((subset(bk,대진팀=="오리온")[-1,c(8)]-subset(bk,대진팀=="오리온")[-27,c(8)])/subset(bk,대진팀=="오리온")[-27,c(8)])*100
ori



#삼성
subset(bk,대진팀=="삼성")[-27,c(8)]
subset(bk,대진팀=="삼성")[-1,c(8)]
sam<-subset(bk,대진팀=="삼성")
sam$증감률<-0
sam[2:27,9]<-((subset(bk,대진팀=="삼성")[-1,c(8)]-subset(bk,대진팀=="삼성")[-27,c(8)])/subset(bk,대진팀=="삼성")[-27,c(8)])*100
sam

#kgc
subset(bk,대진팀=="KGC")[-27,c(8)]
subset(bk,대진팀=="KGC")[-1,c(8)]
kgc<-subset(bk,대진팀=="KGC")
kgc$증감률<-0
kgc[2:27,9]<-((subset(bk,대진팀=="KGC")[-1,c(8)]-subset(bk,대진팀=="KGC")[-27,c(8)])/subset(bk,대진팀=="KGC")[-27,c(8)])*100
kgc

#sk
subset(bk,대진팀=="SK")[-27,c(8)]
subset(bk,대진팀=="SK")[-1,c(8)]
sk<-subset(bk,대진팀=="SK")
sk$증감률<-0
sk[2:27,9]<-((subset(bk,대진팀=="SK")[-1,c(8)]-subset(bk,대진팀=="SK")[-27,c(8)])/subset(bk,대진팀=="SK")[-27,c(8)])*100
sk

#kt
subset(bk,대진팀=="KT")[-27,c(8)]
subset(bk,대진팀=="KT")[-1,c(8)]
kt<-subset(bk,대진팀=="KT")
kt$증감률<-0
kt[2:27,9]<-((subset(bk,대진팀=="KT")[-1,c(8)]-subset(bk,대진팀=="KT")[-27,c(8)])/subset(bk,대진팀=="KT")[-27,c(8)])*100
kt

#lg
subset(bk,대진팀=="LG")[-27,c(8)]
subset(bk,대진팀=="LG")[-1,c(8)]
lg<-subset(bk,대진팀=="LG")
lg$증감률<-0
lg[2:27,9]<-((subset(bk,대진팀=="LG")[-1,c(8)]-subset(bk,대진팀=="LG")[-27,c(8)])/subset(bk,대진팀=="LG")[-27,c(8)])*100
lg

#현대모비스
subset(bk,대진팀=="현대모비스")[-27,c(8)]
subset(bk,대진팀=="현대모비스")[-1,c(8)]
hyun<-subset(bk,대진팀=="현대모비스")
hyun$증감률<-0
hyun[2:27,9]<-((subset(bk,대진팀=="현대모비스")[-1,c(8)]-subset(bk,대진팀=="현대모비스")[-27,c(8)])/subset(bk,대진팀=="현대모비스")[-27,c(8)])*100
hyun

#전자랜드
subset(bk,대진팀=="전자랜드")[-27,c(8)]
subset(bk,대진팀=="전자랜드")[-1,c(8)]
land<-subset(bk,대진팀=="전자랜드")
land$증감률<-0
land[2:27,9]<-((subset(bk,대진팀=="전자랜드")[-1,c(8)]-subset(bk,대진팀=="전자랜드")[-27,c(8)])/subset(bk,대진팀=="전자랜드")[-27,c(8)])*100
land

#db
subset(bk,대진팀=="DB")[-27,c(8)]
subset(bk,대진팀=="DB")[-1,c(8)]
db<-subset(bk,대진팀=="DB")
db$증감률<-0
db[2:27,9]<-((subset(bk,대진팀=="DB")[-1,c(8)]-subset(bk,대진팀=="DB")[-27,c(8)])/subset(bk,대진팀=="DB")[-27,c(8)])*100
db

#merge
bk3<-merge(x=bk,y=bk2,by.x="번호",by.y="game_no")
str(bk3)
bk$관중<-as.integer(bk$관중)
sum(is.na(bk3$back_num))
sum(is.na(bk3))
bk<-bk[-c(271:275),]

#팀별 경기기록
library("dplyr")
bk3_a<-bk3 %>% group_by(대진팀,대진팀2,번호) %>% 
  summarise(score1=sum(threep),score2=sum(threep_a),
            score3=(sum(threep)/sum(threep_a))*100,
            bound=sum(o_r),bound2=sum(d_r),
            dk=sum(dk),dk2=sum(dk_a),
            dk3=(sum(dk)/sum(dk_a))*100,
            st=sum(s_t),gd=sum(gd),
            jumsu=sum(score))
View(bk3_a)
bk3_a<-as.data.frame(bk3_a)
str(bk3_a)


#오리온
ori2<-merge(ori,bk3_a,by=c("번호","대진팀","대진팀2"),all=F)
ori2<-as.data.frame(ori2)
str(ori2)

#kcc
kcc2<-merge(kcc,bk3_a,by=c("번호","대진팀","대진팀2"),all=F)
kcc2<-as.data.frame(kcc2)
str(kcc2)

#삼성
sam2<-merge(sam,bk3_a,by=c("번호","대진팀","대진팀2"),all=F)
sam2<-as.data.frame(sam2)
str(sam2)

#kgc
kgc2<-merge(kgc,bk3_a,by=c("번호","대진팀","대진팀2"),all=F)
kgc2<-as.data.frame(kgc2)
str(kgc2)

#sk
sk2<-merge(sk,bk3_a,by=c("번호","대진팀","대진팀2"),all=F)
sk2<-as.data.frame(sk2)
str(sk2)

#kt
kt2<-merge(kt,bk3_a,by=c("번호","대진팀","대진팀2"),all=F)
kt2<-as.data.frame(kt2)
str(kt2)

#lg
lg2<-merge(lg,bk3_a,by=c("번호","대진팀","대진팀2"),all=F)
lg2<-as.data.frame(lg2)
str(lg2)

#현대
hyun2<-merge(hyun,bk3_a,by=c("번호","대진팀","대진팀2"),all=F)
hyun2<-as.data.frame(hyun2)
str(hyun2)

#전자랜드
land2<-merge(land,bk3_a,by=c("번호","대진팀","대진팀2"),all=F)
land2<-as.data.frame(land2)
str(land2)

#db
db2<-merge(db,bk3_a,by=c("번호","대진팀","대진팀2"),all=F)
db2<-as.data.frame(db2)
str(db2)

##rbind
gon<-rbind(ori2,kcc2,sam2,kgc2,sk2,kt2,lg2,hyun2,land2,db2)
str(gon)

unique(gon$장소)
unique(gon$일자)

#날씨
wea<-data.frame(fread("농구날씨.csv"))
View(wea)
wea[is.na(wea)]=0
sum(is.na(wea))

str(wea)
colnames(wea)<-c("지점","일시","기온","강수량","풍속","습도")
wea$지역<-ifelse(wea$지점==108,"서울",
               ifelse(wea$지점==112,"인천",
                      ifelse(wea$지점==114,"원주",
                             ifelse(wea$지점==140,"군산",
                                    ifelse(wea$지점==146,"전주",
                                           ifelse(wea$지점==152,"울산",
                                                  ifelse(wea$지점==155,"창원","부산")))))))



####
gon$일자<-as.Date(gon$일자,format="%Y.%m.%d")
unique(gon$장소)
gon$지역<-ifelse(gon$장소=="고양","서울",
               ifelse(gon$장소=="잠실실내","서울",
                      ifelse(gon$장소=="안양","서울",
                             ifelse(gon$장소=="잠실학생","서울",
                                    ifelse(gon$장소=="전주","전주",
                                          ifelse(gon$장소=="군산","군산",
                                                 ifelse(gon$장소=="부산","부산",
                                                        ifelse(gon$장소=="창원","창원",
                                                               ifelse(gon$장소=="울산","울산",
                                                                      ifelse(gon$장소=="인천","인천","원주"))))))))))

View(gon2)
gon2<-merge(x=gon,y=wea,by.x=c("일자","지역"),by.y=c("일시","지역"),all=F)
gon2[is.na(gon2)]<-0
sum(is.na(gon2))
str(gon2)



#미세먼지
dust<-data.frame(fread("농구미세먼지.csv"))
str(dust)
length(dust[dust$지점명=="전주",]$미세먼지농도)
dust<-dust[,-1]
dust[is.na(dust)]<-0
#미세먼지 (창원,인천,원주,부산)
dust2<-data.frame(fread("창원미먼.csv"))
dust3<-data.frame(fread("인천미먼.csv"))
dust4<-data.frame(fread("부산미먼.csv"))
dust5<-data.frame(fread("강원도미먼.csv"))

dust6<-rbind(dust,dust2,dust3,dust4,dust5)
str(dust6)

dust6$특보<-ifelse(dust6$미세먼지농도<=30,1,
                ifelse(dust6$미세먼지농도<=80,2,
                       ifelse(dust6$미세먼지농도<=150,3,4)))
dust6$특보<-factor(dust6$특보,levels=c("1","2","3","4"))
str(dust6)

gon3<-merge(x=gon2,y=dust6,by.x=c("일자","지역"),by.y=c("일자","지점명"),all.x=T)
View(gon3)
str(gon3)
sum(is.na(gon3))
gon3[is.na(gon3)]<-1

#최저기온
wea2<-data.frame(fread("최저기온.csv"))
View(wea2)
wea2$지역<-ifelse(wea2$지점==108,"서울",
                ifelse(wea2$지점==112,"인천",
                       ifelse(wea2$지점==114,"원주",
                              ifelse(wea2$지점==140,"군산",
                                     ifelse(wea2$지점==146,"전주",
                                            ifelse(wea2$지점==152,"울산",
                                                   ifelse(wea2$지점==155,"창원","부산")))))))






wea2<-wea2[,-1]
gon4<-merge(x=gon3,y=wea2,by.x=c("일자","지역"),by.y=c("일시","지역"),all=F)
View(gon4)
#한파주의보
gon4$한파<-ifelse(gon4$최저기온 < -12,2,
                ifelse(gon4$최저기온 < -8,1,0))
gon4$한파<-factor(gon4$한파,levels=c(0,1,2))

#시간대별로
unique(gon4$시간)
gon4$시간dum<-ifelse(gon4$시간=="15:00",0,
                   ifelse(gon4$시간=="17:00",1,
                          ifelse(gon4$시간=="19:00",2,3)))
gon4$시간dum<-factor(gon4$시간dum,levels=c(0,1,2,3))

#요일별
gon4$요일dum<-ifelse(gon4$요일=="월",0,
                   ifelse(gon4$요일=="화",0,
                          ifelse(gon4$요일=="수",0,
                                 ifelse(gon4$요일=="목",0,
                                        ifelse(gon4$요일=="금",1,
                                               ifelse(gon4$요일=="토",2,2))))))
gon4$요일dum<-factor(gon4$요일dum,levels=c(0,1,2))

unique(gon4$지역)
#지역별
gon4$지역dum<-ifelse(gon4$지역=="서울",0,
                   ifelse(gon4$지역=="울산",1,
                          ifelse(gon4$지역=="원주",2,
                                 ifelse(gon4$지역=="인천",3,
                                        ifelse(gon4$지역=="부산",4,
                                               ifelse(gon4$지역=="전주",5,
                                                      ifelse(gon4$지역=="창원",6,7)))))))
gon4$지역dum<-factor(gon4$지역dum,levels=c(0,1,2,3,4,5,6,7))
str(gon4)

#s더비                                                 
sdu<-rbind(subset(gon4,대진팀=="삼성" & 대진팀2=="SK"),subset(gon4,대진팀=="SK" & 대진팀2=="삼성"))[,c(4,5)]
sdu$rival<-1
#통신사더비
tong<-rbind(subset(gon4,대진팀=="SK" & 대진팀2=="KT"),subset(gon4,대진팀=="KT" & 대진팀2=="SK"))[,c(4,5)]
tong$rival<-1
#재계라이벌더비
je<-rbind(subset(gon4,대진팀=="삼성" & 대진팀2=="LG"),subset(gon4,대진팀=="LG" & 대진팀2=="삼성"))[,c(4,5)]
je$rival<-1
#낙동강더비
nack<-rbind(subset(gon4,대진팀=="LG" & 대진팀2=="KT"),subset(gon4,대진팀=="KT" & 대진팀2=="LG"))[,c(4,5)]
nack$rival<-1

duvi<-rbind(sdu,tong,je,nack)
duvi

duvi$일자 <- c("2017-11-18","2018-01-16","2017-11-01","2018-02-18","2017-12-25","2018-01-24","2017-12-30","2018-02-13","2017-11-07","2018-03-04","2017-10-21","2017-12-16","2018-02-11","2018-01-01","2017-10-17","2017-12-16","2018-03-10","2017-11-07","2018-01-06","2017-12-13","2017-10-27","2017-11-19","2018-02-15","2018-01-27")
xx<-merge(gon4,duvi,by=c("대진팀","대진팀2","일자"),all.x=T)
View(xx)
str(xx)


xx[is.na(xx)]<-0
xx$rival<-factor(xx$rival,levels=c(0,1))
str(xx)

ns <- data.frame(fread("naversearch.csv"))
str(ns)
ns<-ns[-c(1087:42403),]
library(tidyr)

#wide to long
ns2<-melt(ns,id.vars=c("date"))
tail(ns2)
str(ns2)


colnames(ns2)<-c("일자","대진팀","search")

ns2$일자<-as.Date(ns2$일자)
ns2$대진팀<-as.character(ns2$대진팀)
str(ns2)
sum(is.na(ns2))
View(ns2)

ns2$대진팀<-gsub("KCC이지스","KCC",ns2$대진팀)
ns2$대진팀<-gsub("현대모비스피버스","현대모비스",ns2$대진팀)
ns2$대진팀<-gsub("안양KGC인삼공사","KGC",ns2$대진팀)
ns2$대진팀<-gsub("인천전자랜드엘리펀츠","전자랜드",ns2$대진팀)
ns2$대진팀<-gsub("고양오리온오리온스","오리온",ns2$대진팀)
ns2$대진팀<-gsub("부산KT소닉붐","KT",ns2$대진팀)
ns2$대진팀<-gsub("원주DB프로미","DB",ns2$대진팀)
ns2$대진팀<-gsub("창원LG세이커스","LG",ns2$대진팀)
ns2$대진팀<-gsub("서울SK나이츠","SK",ns2$대진팀)
ns2$대진팀<-gsub("삼성서울썬더스","삼성",ns2$대진팀)

test<-merge(x=xx,y=ns2,by.x=c("대진팀","일자"),by.y=c("대진팀","일자"),all.x=T)
str(test)
View(test)

ns3<-ns2
ns3$search2<-ns2$search
str(ns3)
ns3$대진팀2 <- ns2$대진팀

ns3<-ns3[,-c(2,3)]

test2<-merge(x=test,y=ns3,by.x=c("대진팀2","일자"),by.y=c("대진팀2","일자"),all.x=T)
str(test2)
View(test2)

ga1718 <- data.frame(fread("가중치1718.csv"))
str(ga1718)
ga1718$일자<-as.Date(ga1718$일자,format="%Y.%m.%d")

ga1718_a<-ga1718[,-4]
test22<-merge(x=test2,y=ga1718_a,by.x=c("일자","대진팀","대진팀2"),by.y=c("일자","home","away"),all.x=T)
str(test22)

test2_t<-test2[,-c(1:8)]
str(test2_t)
test_lm<-lm(관중~.,test2_t)
summary(test_lm)


ps<-data.frame(fread("player_searchdata.csv"))
str(ps)
colnames(ps)<-c("일자","대진팀","sw")

ps$대진팀<-gsub("kcc","KCC",ps$대진팀)
ps$대진팀<-gsub("hyundai","현대모비스",ps$대진팀)
ps$대진팀<-gsub("kgc","KGC",ps$대진팀)
ps$대진팀<-gsub("jeonja","전자랜드",ps$대진팀)
ps$대진팀<-gsub("orion","오리온",ps$대진팀)
ps$대진팀<-gsub("KT","KT",ps$대진팀)
ps$대진팀<-gsub("db","DB",ps$대진팀)
ps$대진팀<-gsub("lg","LG",ps$대진팀)
ps$대진팀<-gsub("sk","SK",ps$대진팀)
ps$대진팀<-gsub("samsung","삼성",ps$대진팀)

test3<-merge(x=test2,y=ps,by.x=c("일자","대진팀"),by.y=c("일자","대진팀"),all.x=T)
str(test3)

ps2<-ps
colnames(ps2)<-c("일자","대진팀2","sw2")

test33<-merge(x=test3,y=ps2,by.x=c("일자","대진팀2"),by.y=c("일자","대진팀2"),all.x=T)
str(test33)

####
view<-data.frame(fread("네이버스포츠_농구영상조회수.csv"))
str(view)
view2<-view[,-2]
colnames(view2)<-c("일자","조회수","대진팀","대진팀2","home팀점수","away팀점수")
str(view2)
tail(view2)
unique(view2$대진팀)
view2$일자<-as.Date(view2$일자,format="%Y.%m.%d")

view2$대진팀<-gsub("전주KCC","KCC",view2$대진팀)
view2$대진팀<-gsub("울산현대모비스","현대모비스",view2$대진팀)
view2$대진팀<-gsub("안양KGC","KGC",view2$대진팀)
view2$대진팀<-gsub("인천전자랜드","전자랜드",view2$대진팀)
view2$대진팀<-gsub("고양오리온","오리온",view2$대진팀)
view2$대진팀<-gsub("부산KT","KT",view2$대진팀)
view2$대진팀<-gsub("원주DB","DB",view2$대진팀)
view2$대진팀<-gsub("창원LG","LG",view2$대진팀)
view2$대진팀<-gsub("서울SK","SK",view2$대진팀)
view2$대진팀<-gsub("서울삼성","삼성",view2$대진팀)
#대진팀2
view2$대진팀2<-gsub("전주KCC","KCC",view2$대진팀2)
view2$대진팀2<-gsub("울산현대모비스","현대모비스",view2$대진팀2)
view2$대진팀2<-gsub("안양KGC","KGC",view2$대진팀2)
view2$대진팀2<-gsub("인천전자랜드","전자랜드",view2$대진팀2)
view2$대진팀2<-gsub("고양오리온","오리온",view2$대진팀2)
view2$대진팀2<-gsub("부산KT","KT",view2$대진팀2)
view2$대진팀2<-gsub("원주DB","DB",view2$대진팀2)
view2$대진팀2<-gsub("창원LG","LG",view2$대진팀2)
view2$대진팀2<-gsub("서울SK","SK",view2$대진팀2)
view2$대진팀2<-gsub("서울삼성","삼성",view2$대진팀2)

view22<-view2 %>% group_by(일자,대진팀,대진팀2) %>% summarise(tview=sum(조회수))
str(view22)
sum(is.na(view22))
View(view22)
view22<-as.data.frame(view22)

view3<-merge(x=test33,y=view22,by.x=c("일자","대진팀","대진팀2"),by.y=c("일자","대진팀","대진팀2"),all.x=T)
str(view3)
View(view3)
sum(is.na(test33))

view3$tview<-scale(view3$tview)
view3<-as.data.frame(view3)

test33$home<-factor(test33$대진팀)
test33$away<-factor(test33$대진팀2)


###다중회귀
test3_f<-view3[,-c(1:8)]
str(test3_f)
sum(is.na(test3_f))
View(test3_f)
test3_f[75,33]<-30900

test3_f$home<-factor(test33$대진팀)
str(test3_f)
test3_f$away<-factor(test33$대진팀2)

fit<-lm(tview ~ .-home-away,test3_f)
summary(fit)
step(fit,direction="both")



test2_tt<-test22[,-c(1:8)]
str(test2_tt)
test_lm<-lm(관중~.,test2_tt)
summary(test_lm)

step(fit,direction = "both")

summary(lm(formula = 관중 ~ 증감률 + score2 + st + gd + 풍속 + 특보 + 
     최저기온 + 시간dum + 지역dum + rival + search + search2 + 
     tview, data = test3_f))



fit <- lm(관중 ~ home, data=test2_std2)
summary(fit)
str(test2_tt)

test2_std<-test2_tt[,-c(20,22:26)]
str(test2_std2)
test2_std2<-scale(test2_std)
View(test2_std2)

test2_std2<-as.data.frame(test2_std2)

test2_std2$관중 <- test2_std$관중

test2_std2<-cbind(test2_std2,test2_tt[,c(20,22:26)])
