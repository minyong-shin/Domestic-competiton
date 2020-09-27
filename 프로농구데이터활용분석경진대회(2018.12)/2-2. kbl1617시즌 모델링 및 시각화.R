#1617데이터
ak <- data.frame(fread("1617경기번호.csv"))
ak2 <- data.frame(fread("1617경기기록.csv"))
str(ak)
unique(ak$대진팀)

ak$대진팀<-gsub("모비스","현대모비스",ak$대진팀)
ak$대진팀<-gsub("동부","DB",ak$대진팀)
ak$대진팀<-gsub("케이티","KT",ak$대진팀)

ak$대진팀2<-gsub("모비스","현대모비스",ak$대진팀2)
ak$대진팀2<-gsub("동부","DB",ak$대진팀2)
ak$대진팀2<-gsub("케이티","KT",ak$대진팀2)


#kcc
#전경기
subset(ak,대진팀=="KCC")[-27,c(8)]
#후경기
subset(ak,대진팀=="KCC")[-1,c(8)]

#증감률
((subset(ak,대진팀=="KCC")[-1,c(8)]-subset(ak,대진팀=="KCC")[-27,c(8)])/subset(ak,대진팀=="KCC")[-27,c(8)])*100

kcca<-subset(ak,대진팀=="KCC")
kcca$증감률<-0
kcca[2:27,9]<-((subset(ak,대진팀=="KCC")[-1,c(8)]-subset(ak,대진팀=="KCC")[-27,c(8)])/subset(ak,대진팀=="KCC")[-27,c(8)])*100
kcca

#오리온
subset(ak,대진팀=="오리온")[-27,c(8)]
subset(ak,대진팀=="오리온")[-1,c(8)]
((subset(ak,대진팀=="오리온")[-1,c(8)]-subset(ak,대진팀=="오리온")[-27,c(8)])/subset(ak,대진팀=="오리온")[-27,c(8)])*100
oria<-subset(ak,대진팀=="오리온")
oria$증감률<-0
oria[2:27,9]<-((subset(ak,대진팀=="오리온")[-1,c(8)]-subset(ak,대진팀=="오리온")[-27,c(8)])/subset(ak,대진팀=="오리온")[-27,c(8)])*100
oria



#삼성
subset(ak,대진팀=="삼성")[-27,c(8)]
subset(ak,대진팀=="삼성")[-1,c(8)]
sama<-subset(ak,대진팀=="삼성")
sama$증감률<-0
sama[2:27,9]<-((subset(ak,대진팀=="삼성")[-1,c(8)]-subset(ak,대진팀=="삼성")[-27,c(8)])/subset(ak,대진팀=="삼성")[-27,c(8)])*100
sama

#kgc
subset(ak,대진팀=="KGC")[-27,c(8)]
subset(ak,대진팀=="KGC")[-1,c(8)]
kgca<-subset(ak,대진팀=="KGC")
kgca$증감률<-0
kgca[2:27,9]<-((subset(ak,대진팀=="KGC")[-1,c(8)]-subset(ak,대진팀=="KGC")[-27,c(8)])/subset(ak,대진팀=="KGC")[-27,c(8)])*100
kgca

#sk
subset(ak,대진팀=="SK")[-27,c(8)]
subset(ak,대진팀=="SK")[-1,c(8)]
ska<-subset(ak,대진팀=="SK")
ska$증감률<-0
ska[2:27,9]<-((subset(ak,대진팀=="SK")[-1,c(8)]-subset(ak,대진팀=="SK")[-27,c(8)])/subset(ak,대진팀=="SK")[-27,c(8)])*100
ska

#kt
subset(ak,대진팀=="KT")[-27,c(8)]
subset(ak,대진팀=="KT")[-1,c(8)]
kta<-subset(ak,대진팀=="KT")
kta$증감률<-0
kta[2:27,9]<-((subset(ak,대진팀=="KT")[-1,c(8)]-subset(ak,대진팀=="KT")[-27,c(8)])/subset(ak,대진팀=="KT")[-27,c(8)])*100
kta

#lg
subset(ak,대진팀=="LG")[-27,c(8)]
subset(ak,대진팀=="LG")[-1,c(8)]
lga<-subset(ak,대진팀=="LG")
lga$증감률<-0
lga[2:27,9]<-((subset(ak,대진팀=="LG")[-1,c(8)]-subset(ak,대진팀=="LG")[-27,c(8)])/subset(ak,대진팀=="LG")[-27,c(8)])*100
lga

#현대모비스
subset(ak,대진팀=="현대모비스")[-27,c(8)]
subset(ak,대진팀=="현대모비스")[-1,c(8)]
hyuna<-subset(ak,대진팀=="현대모비스")
hyuna$증감률<-0
hyuna[2:27,9]<-((subset(ak,대진팀=="현대모비스")[-1,c(8)]-subset(ak,대진팀=="현대모비스")[-27,c(8)])/subset(ak,대진팀=="현대모비스")[-27,c(8)])*100
hyuna

#전자랜드
subset(ak,대진팀=="전자랜드")[-27,c(8)]
subset(ak,대진팀=="전자랜드")[-1,c(8)]
landa<-subset(ak,대진팀=="전자랜드")
landa$증감률<-0
landa[2:27,9]<-((subset(ak,대진팀=="전자랜드")[-1,c(8)]-subset(ak,대진팀=="전자랜드")[-27,c(8)])/subset(ak,대진팀=="전자랜드")[-27,c(8)])*100
landa

#db
subset(ak,대진팀=="DB")[-27,c(8)]
subset(ak,대진팀=="DB")[-1,c(8)]
dba<-subset(ak,대진팀=="DB")
dba$증감률<-0
dba[2:27,9]<-((subset(ak,대진팀=="DB")[-1,c(8)]-subset(ak,대진팀=="DB")[-27,c(8)])/subset(ak,대진팀=="DB")[-27,c(8)])*100
dba

#merge
str(ak2)
ak3<-merge(x=ak,y=ak2,by.x="번호",by.y="game_no")

#팀별 경기기록
library("dplyr")
ak3_a<-ak3 %>% group_by(대진팀,대진팀2,번호) %>% 
  summarise(score1=sum(threep),score2=sum(threep_a),
            score3=(sum(threep)/sum(threep_a))*100,
            bound=sum(o_r),bound2=sum(d_r),
            dk=sum(dk),dk2=sum(dk_a),
            dk3=(sum(dk)/sum(dk_a))*100,
            st=sum(s_t),gd=sum(gd),
            jumsu=sum(score))
View(ak3_a)
ak3_a<-as.data.frame(ak3_a)
str(ak3_a)


#오리온
ori2a<-merge(oria,ak3_a,by=c("번호","대진팀","대진팀2"),all=F)
ori2a<-as.data.frame(ori2a)
str(ori2a)

#kcc
kcc2a<-merge(kcca,ak3_a,by=c("번호","대진팀","대진팀2"),all=F)
kcc2a<-as.data.frame(kcc2a)
str(kcc2a)

#삼성
sam2a<-merge(sama,ak3_a,by=c("번호","대진팀","대진팀2"),all=F)
sam2a<-as.data.frame(sam2a)
str(sam2a)

#kgc
kgc2a<-merge(kgca,ak3_a,by=c("번호","대진팀","대진팀2"),all=F)
kgc2a<-as.data.frame(kgc2a)
str(kgc2a)

#sk
sk2a<-merge(ska,ak3_a,by=c("번호","대진팀","대진팀2"),all=F)
sk2a<-as.data.frame(sk2a)
str(sk2a)

#kt
kt2a<-merge(kta,ak3_a,by=c("번호","대진팀","대진팀2"),all=F)
kt2a<-as.data.frame(kt2a)
str(kt2a)

#lg
lg2a<-merge(lga,ak3_a,by=c("번호","대진팀","대진팀2"),all=F)
lg2a<-as.data.frame(lg2a)
str(lg2a)

#현대
hyun2a<-merge(hyuna,ak3_a,by=c("번호","대진팀","대진팀2"),all=F)
hyun2a<-as.data.frame(hyun2a)
str(hyun2a)

#전자랜드
land2a<-merge(landa,ak3_a,by=c("번호","대진팀","대진팀2"),all=F)
land2a<-as.data.frame(land2a)
str(land2a)

#db
db2a<-merge(dba,ak3_a,by=c("번호","대진팀","대진팀2"),all=F)
db2a<-as.data.frame(db2a)
str(db2)

##rbind
gona<-rbind(ori2a,kcc2a,sam2a,kgc2a,sk2a,kt2a,lg2a,hyun2a,land2a,db2a)
str(gona)

unique(gona$장소)
unique(gona$일자)

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
gona$일자<-as.Date(gona$일자,format="%Y.%m.%d")
unique(gona$장소)
gona$지역<-ifelse(gona$장소=="고양","서울",
               ifelse(gona$장소=="잠실실내","서울",
                      ifelse(gona$장소=="안양","서울",
                             ifelse(gona$장소=="잠실학생","서울",
                                    ifelse(gona$장소=="전주","전주",
                                           ifelse(gona$장소=="군산","군산",
                                                  ifelse(gona$장소=="부산","부산",
                                                         ifelse(gona$장소=="창원","창원",
                                                                ifelse(gona$장소=="울산","울산",
                                                                       ifelse(gona$장소=="인천","인천","원주"))))))))))


gon2a<-merge(x=gona,y=wea,by.x=c("일자","지역"),by.y=c("일시","지역"),all=F)
gon2a[is.na(gon2a)]<-0
sum(is.na(gon2a))
str(gon2a)



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

gon3a<-merge(x=gon2a,y=dust6,by.x=c("일자","지역"),by.y=c("일자","지점명"),all.x=T)
View(gon3a)
str(gon3a)
sum(is.na(gon3a))
gon3a[is.na(gon3a)]<-1

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
gon4a<-merge(x=gon3a,y=wea2,by.x=c("일자","지역"),by.y=c("일시","지역"),all=F)
View(gon4a)
str(gon4a)
#한파주의보
gon4a$한파<-ifelse(gon4a$최저기온 < -12,2,
                ifelse(gon4a$최저기온 < -8,1,0))
gon4a$한파<-factor(gon4a$한파,levels=c(0,1,2))

#시간대별로
unique(gon4$시간)
gon4a$시간dum<-ifelse(gon4a$시간=="14:00",0,
                   ifelse(gon4a$시간=="16:00",1,
                          ifelse(gon4a$시간=="19:00",2,2)))
gon4a$시간dum<-factor(gon4a$시간dum,levels=c(0,1,2))

#요일별
gon4a$요일dum<-ifelse(gon4a$요일=="월",0,
                   ifelse(gon4a$요일=="화",0,
                          ifelse(gon4a$요일=="수",0,
                                 ifelse(gon4a$요일=="목",0,
                                        ifelse(gon4a$요일=="금",1,
                                               ifelse(gon4a$요일=="토",2,2))))))
gon4a$요일dum<-factor(gon4a$요일dum,levels=c(0,1,2))

unique(gon4a$지역)
#지역별
gon4a$지역dum<-ifelse(gon4a$지역=="서울",0,
                   ifelse(gon4a$지역=="울산",1,
                          ifelse(gon4a$지역=="원주",2,
                                 ifelse(gon4a$지역=="인천",3,
                                        ifelse(gon4a$지역=="부산",4,
                                               ifelse(gon4a$지역=="전주",5,
                                                      ifelse(gon4a$지역=="창원",6,7)))))))
gon4a$지역dum<-factor(gon4a$지역dum,levels=c(0,1,2,3,4,5,6,7))
str(gon4a)
View(gon4a)
#s더비                                                 
sdua<-rbind(subset(gon4a,대진팀=="삼성" & 대진팀2=="SK"),subset(gon4a,대진팀=="SK" & 대진팀2=="삼성"))[,c(4,5)]
sdua$rival<-1
#통신사더비
tonga<-rbind(subset(gon4a,대진팀=="SK" & 대진팀2=="KT"),subset(gon4a,대진팀=="KT" & 대진팀2=="SK"))[,c(4,5)]
tonga$rival<-1
#재계라이벌더비
jea<-rbind(subset(gon4a,대진팀=="삼성" & 대진팀2=="LG"),subset(gon4a,대진팀=="LG" & 대진팀2=="삼성"))[,c(4,5)]
jea$rival<-1
#낙동강더비
nacka<-rbind(subset(gon4a,대진팀=="LG" & 대진팀2=="KT"),subset(gon4a,대진팀=="KT" & 대진팀2=="LG"))[,c(4,5)]
nacka$rival<-1

duvia<-rbind(sdua,tonga,jea,nacka)
duvia

duvia$일자 <- c("2016-11-06","2016-11-23","2017-01-10","2016-12-25","2017-02-10","2017-03-18","2016-11-13","2016-11-29","2017-02-21","2017-01-07","2017-01-17","2017-03-03","2016-11-11","2016-11-20","2017-03-05","2016-12-18","2017-01-17","2017-02-17","2016-11-05","2016-12-22","2017-02-24","2016-12-08","2017-01-25","2017-03-17")
xxa<-merge(gon4a,duvia,by=c("대진팀","대진팀2","일자"),all.x=T)
str(xxa)


xxa[is.na(xxa)]<-0
xxa$rival<-factor(xxa$rival,levels=c(0,1))
str(xxa)

nsa <- data.frame(fread("naversearch.csv"))
str(nsa)
View(nsa)
nsa<-nsa[-c(1087:42403),]
library(tidyr)

#wide to long
ns2a<-melt(nsa,id.vars=c("date"))
tail(ns2a)
str(ns2a)


colnames(ns2a)<-c("일자","대진팀","search")

ns2a$일자<-as.Date(ns2a$일자)
ns2a$대진팀<-as.character(ns2a$대진팀)
str(ns2)
sum(is.na(ns2a))
View(ns2)

ns2a$대진팀<-gsub("KCC이지스","KCC",ns2a$대진팀)
ns2a$대진팀<-gsub("현대모비스피버스","현대모비스",ns2a$대진팀)
ns2a$대진팀<-gsub("안양KGC인삼공사","KGC",ns2a$대진팀)
ns2a$대진팀<-gsub("인천전자랜드엘리펀츠","전자랜드",ns2a$대진팀)
ns2a$대진팀<-gsub("고양오리온오리온스","오리온",ns2a$대진팀)
ns2a$대진팀<-gsub("부산KT소닉붐","KT",ns2a$대진팀)
ns2a$대진팀<-gsub("원주DB프로미","DB",ns2a$대진팀)
ns2a$대진팀<-gsub("창원LG세이커스","LG",ns2a$대진팀)
ns2a$대진팀<-gsub("서울SK나이츠","SK",ns2a$대진팀)
ns2a$대진팀<-gsub("삼성서울썬더스","삼성",ns2a$대진팀)

testa<-merge(x=xxa,y=ns2a,by.x=c("대진팀","일자"),by.y=c("대진팀","일자"),all.x=T)
str(testa)
View(testa)

ns3a<-ns2a
ns3a$search2<-ns2a$search
str(ns3a)
ns3a$대진팀2 <- ns2a$대진팀

ns3a<-ns3a[,-c(2,3)]

test2a<-merge(x=testa,y=ns3a,by.x=c("대진팀2","일자"),by.y=c("대진팀2","일자"),all.x=T)
str(test2a)
View(test2)

#필요없음###
############
ga1718a <- data.frame(fread("가중치1718.csv"))
str(ga1718a)
ga1718a$일자<-as.Date(ga1718a$일자,format="%Y.%m.%d")

ga1718_a<-ga1718[,-4]
test22<-merge(x=test2,y=ga1718_a,by.x=c("일자","대진팀","대진팀2"),by.y=c("일자","home","away"),all.x=T)
str(test22)

test2_t<-test2[,-c(1:8)]
str(test2_t)
test_lm<-lm(관중~.,test2_t)
summary(test_lm)
########################################################################################
#선수검색 트렌드
psa<-data.frame(fread("player_searchdata.csv"))
str(psa)
colnames(psa)<-c("일자","대진팀","sw")

psa$대진팀<-gsub("kcc","KCC",psa$대진팀)
psa$대진팀<-gsub("hyundai","현대모비스",psa$대진팀)
psa$대진팀<-gsub("kgc","KGC",psa$대진팀)
psa$대진팀<-gsub("jeonja","전자랜드",psa$대진팀)
psa$대진팀<-gsub("orion","오리온",psa$대진팀)
psa$대진팀<-gsub("KT","KT",psa$대진팀)
psa$대진팀<-gsub("db","DB",psa$대진팀)
psa$대진팀<-gsub("lg","LG",psa$대진팀)
psa$대진팀<-gsub("sk","SK",psa$대진팀)
psa$대진팀<-gsub("samsung","삼성",psa$대진팀)

test3a<-merge(x=test2a,y=psa,by.x=c("일자","대진팀"),by.y=c("일자","대진팀"),all.x=T)
str(test3a)

ps2a<-psa
colnames(ps2a)<-c("일자","대진팀2","sw2")

test33a<-merge(x=test3a,y=ps2a,by.x=c("일자","대진팀2"),by.y=c("일자","대진팀2"),all.x=T)
str(test33a)

####조회수
viewa<-data.frame(fread("네이버스포츠_농구영상조회수.csv"))
str(viewa)
View(viewa)
unique(viewa$일자)
view2a<-viewa[,-2]
colnames(view2a)<-c("일자","조회수","대진팀","대진팀2","home팀점수","away팀점수")
str(view2a)
tail(view2a)
unique(view2$대진팀)
view2a$일자<-as.Date(view2a$일자,format="%Y.%m.%d %a")

view2a$대진팀<-gsub("전주KCC","KCC",view2a$대진팀)
view2a$대진팀<-gsub("울산현대모비스","현대모비스",view2a$대진팀)
view2a$대진팀<-gsub("안양KGC","KGC",view2a$대진팀)
view2a$대진팀<-gsub("인천전자랜드","전자랜드",view2a$대진팀)
view2a$대진팀<-gsub("고양오리온","오리온",view2a$대진팀)
view2a$대진팀<-gsub("부산KT","KT",view2a$대진팀)
view2a$대진팀<-gsub("원주DB","DB",view2a$대진팀)
view2a$대진팀<-gsub("창원LG","LG",view2a$대진팀)
view2a$대진팀<-gsub("서울SK","SK",view2a$대진팀)
view2a$대진팀<-gsub("서울삼성","삼성",view2a$대진팀)
view2a$대진팀<-gsub("울산모비스","현대모비스",view2a$대진팀)
view2a$대진팀<-gsub("원주동부","DB",view2a$대진팀)

#대진팀2
view2a$대진팀2<-gsub("전주KCC","KCC",view2a$대진팀2)
view2a$대진팀2<-gsub("울산현대모비스","현대모비스",view2a$대진팀2)
view2a$대진팀2<-gsub("울산모비스","현대모비스",view2a$대진팀2)
view2a$대진팀2<-gsub("안양KGC","KGC",view2a$대진팀2)
view2a$대진팀2<-gsub("인천전자랜드","전자랜드",view2a$대진팀2)
view2a$대진팀2<-gsub("고양오리온","오리온",view2a$대진팀2)
view2a$대진팀2<-gsub("부산KT","KT",view2a$대진팀2)
view2a$대진팀2<-gsub("원주DB","DB",view2a$대진팀2)
view2a$대진팀2<-gsub("원주동부","DB",view2a$대진팀2)
view2a$대진팀2<-gsub("창원LG","LG",view2a$대진팀2)
view2a$대진팀2<-gsub("서울SK","SK",view2a$대진팀2)
view2a$대진팀2<-gsub("서울삼성","삼성",view2a$대진팀2)

View(view2a)
view22a<-view2a %>% group_by(일자,대진팀,대진팀2) %>% summarise(tview=sum(조회수))
str(view22a)
unique(view22a$대진팀)
sum(is.na(view22a))
View(view22a)
view22a<-as.data.frame(view22a)

test33a$home<-factor(test33a$대진팀)
test33a$away<-factor(test33a$대진팀2)


view3a<-merge(x=test33a,y=view22a,by.x=c("일자","대진팀","대진팀2"),by.y=c("일자","대진팀","대진팀2"),all.x=T)
str(view3a)
View(view3a)
sum(is.na(view3a))
str(view3a)

view3a$tview<-scale(view3a$tview)
view3a<-as.data.frame(view3a)

###################################################
view2a_a<-data.frame(fread("선수별 조회수 정보.csv"))
str(view2a_a)
colnames(view2a_a)<-c("일자","제목","조회수","선수","대진팀2")
unique(view2a_a$대진팀)
unique(com77$대진팀)
View(view2a_a)
sum(is.na(view2a_a))
unique(view2a_a$일자)

view2a_a$대진팀2<-gsub("전주 KCC","KCC",view2a_a$대진팀2)
view2a_a$대진팀2<-gsub("울산 현대모비스","현대모비스",view2a_a$대진팀2)
view2a_a$대진팀2<-gsub("안양 KGC","KGC",view2a_a$대진팀2)
view2a_a$대진팀2<-gsub("인천 전자랜드","전자랜드",view2a_a$대진팀2)
view2a_a$대진팀2<-gsub("고양 오리온","오리온",view2a_a$대진팀2)
view2a_a$대진팀2<-gsub("부산 KT","KT",view2a_a$대진팀2)
view2a_a$대진팀2<-gsub("원주 DB","DB",view2a_a$대진팀2)
view2a_a$대진팀2<-gsub("창원 LG","LG",view2a_a$대진팀2)
view2a_a$대진팀2<-gsub("","SK",view2a_a$대진팀2)
view2a_a$대진팀2<-gsub("서울 삼성","삼성",view2a_a$대진팀2)
view2a_a[view2a_a$대진팀2 == "",]$대진팀2 <- "SK"

colnames(view2a_a)<-c("일자","제목","조회수","선수","대진팀")
view222a<-view2a_a %>% group_by(일자,대진팀) %>% summarise(home조회수 = sum(조회수)) 
view222a$일자<-as.Date(view222a$일자 , format = "%Y.%m.%d")
view222a<-as.data.frame(view222a)
str(view222a)

colnames(view2a_a)<-c("일자","제목","조회수","선수","대진팀2")
view222aa<-view2a_a %>% group_by(일자,대진팀2) %>% summarise(away조회수 = sum(조회수)) 
view222aa$일자<-as.Date(view222aa$일자 , format = "%Y.%m.%d")
view222aa<-as.data.frame(view222aa)
View(view222a)

str(com77)
com777<-merge(x=com77,y=view222a,by.x=c("일자","대진팀"),by.y=c("일자","대진팀"),all.x=T)
com7777<-merge(x=com777,y=view222aa,by.x=c("일자","대진팀2"),by.y=c("일자","대진팀2"),all.x=T)

str(com77)
str(view222a)
View(com777)

##
coma<- data.frame(fread("goodbad_1617.csv",encoding = 'UTF-8'))
str(coma)
coma<-coma[,-1]
write.csv(coma,"선악댓글.csv",row.names=F)
coma$날짜<-as.Date(coma$날짜,format="%Y.%m.%d")

com2a<- coma %>% group_by(날짜,팀,경기번호) %>% summarise(총댓글수=n())
str(com2a)
View(com2a)
com2a<-as.data.frame(com2a)
colnames(com2a)<-c("일자","대진팀","번호","home댓글수")
com2a<-com2a[,-3]
unique(com2$대진팀)
str(view3a)
com4a<-merge(x=view3a,y=com2a,by.x = c("일자","대진팀"),by.y = c("일자","대진팀"),all.x=T)
str(com4a)
View(com4a)

colnames(com2a)<-c("일자","대진팀2","away댓글수")
str(com2a)

com5a<-merge(x=com4a,y=com2a,by.x=c("일자","대진팀2"),by.y=c("일자","대진팀2"),all.x=T)
str(com5a)
View(com5)


com5a$총댓글수<-com5a$home댓글수+com5a$away댓글수

str(com5a)

com65<-com5a
str(com65)

com65[6,41]<-121434
com65[268,41]<-155643
com65[270,41]<-108432
sum(is.na(com65))

write.csv(com65,"1617데이터.csv",row.names=F)
str(com66)
com6$미세먼지농도<-as.numeric(com6$미세먼지농도)
com65<-com6[,-c(1:8)]

str(com5_f)
tview<-com66$tview
com7<-com66[,-33]
com7$tview<-tview

str(com7)

######16년도 17년도 merge
shot<-rbind(com7,com5_ff)
str(shot)
sum(is.na(shot))
com5_ff<-shot[c(271:540),]
str(com5_ff)

kk<-lm(관중~.-home-away,data=com5_ff)
summary(kk)
step(kk,direction = "both")
summary(lm(formula = 관중 ~ 증감률 + score2 + st + gd + 풍속 + 특보 + 
     최저기온 + 시간dum + 지역dum + rival + search + search2 + 
     tview, data = com5_ff))


kkk<-lm(관중~.-home-away,data=com7)
summary(kkk)
step(kkk,direction = "both")
summary(lm(formula = 관중 ~ 증감률 + score1 + jumsu + 시간dum + 지역dum + 
     rival + search + search2 + sw + away댓글수 + tview, data = com7))

