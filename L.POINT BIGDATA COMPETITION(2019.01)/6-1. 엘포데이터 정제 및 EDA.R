#데이터 불러오기
library(data.table)
library(dplyr)
library(ggplot2)

mas <- data.frame(fread("master.csv",encoding="UTF-8"))
cus<-data.frame(fread("custom.csv",encoding="UTF-8"))
pro<-data.frame(fread("product.csv",encoding="UTF-8"))
ser1<-data.frame(fread("search1.csv",encoding="UTF-8"))
ser2<-data.frame(fread("search2.csv",encoding="UTF-8"))
ses<-data.frame(fread("session.csv",encoding="UTF-8"))
clu2<-data.frame(fread("cluster2.csv",encoding="UTF-8"))#아마 2가 진짜-->유클리리안으로구한 클러스터
clu_d2<-data.frame(fread("cluster_distance2.csv"))#아마 2가 진짜 --> 유클리디안으로 구한 군집간 거리 벡터

#참고
clu<-data.frame(fread("cluster.csv",encoding="UTF-8"))#코사인 유사도로 구한 클러스터
clu_d<-data.frame(fread("cluster_distance.csv"))#코사인 유사도로 구한 군집간 거리 벡터


#데이터구조확인
View(clu2)
View(clu_d2)
View(clu)
View(clu_d)
View(clu_d2)
str(mas)
str(ses)
str(cus)
str(ser1)
str(ser2)
str(pro)

#데이터머지
sum(is.na(ses$TOT_PAG_VIEW_CT))
ses[is.na(ses)]<-0
sopo<-merge(x=pro,y=ses,by.x=c("CLNT_ID","SESS_ID"),by.y=c("CLNT_ID","SESS_ID"),all.x=T)
str(sopo)
length(unique(sopo$CLNT_ID))
sum(is.na(sopo))

so<-merge(x=sopo,y=mas,by.x=c("PD_C"),by.y=c("PD_C"),all.x=T)
str(so)
sum(is.na(so))

cuso<-merge(x=cus,y=so,by.x="CLNT_ID",by.y="CLNT_ID",all.x=T)
str(cuso)
sum(is.na(cuso))
length(unique(cuso$clu))
cuso2<-cuso
cuso<-cuso2

cuso$SESS_DT<-as.character(cuso$SESS_DT)
cuso$SESS_DT<-as.Date(cuso$SESS_DT,format="%Y%m%d")

#데이터형식을 맞춰주기 위해 이 과정이 필요함

cuso$PD_BUY_AM<-gsub(",","",cuso$PD_BUY_AM)
cuso$PD_BUY_CT<-gsub(",","",cuso$PD_BUY_CT)

cuso$PD_BUY_AM<-as.numeric(cuso$PD_BUY_AM)
cuso$PD_BUY_CT<-as.numeric(cuso$PD_BUY_CT)

#일단 보류 -> 계속 na가 생김
cuso$TOT_SESS_HR_V<-gsub(",","",cuso$TOT_SESS_HR_V)
cuso$TOT_SESS_HR_V<-as.numeric(cuso$TOT_SESS_HR_V)

str(cuso)
sum(is.na(cuso))
clu

write.csv(cuso,"cuso.csv",row.names = F)


###############나중에 37만개 필요할때
str(ser1)
unique(ser1$CLNT_ID)
str(cuso)
ser1$test<-1
cuso3<-subset(cuso, CLNT_ID %in% unique(ser1$CLNT_ID))
str(cuso3)
length(unique(cuso3$clu))
str(cuso2)
dd<-as.data.frame(unique(ser1$CLNT_ID))
str(dd)
dd$test<-1
colnames(dd)<-c("CLNT_ID","test")
length(unique(cuso$CLNT_ID))
cuso2<-merge(x=cuso,y=dd,by.x=c("CLNT_ID"),by.y=c("CLNT_ID"),all.x=T)

#############################################################
#각 소,중,대 별로 분포를 알고 싶을 때######################################################
unique(cuso$CLAC2_NM)
#대분류로 하고싶을 때
#.은 줄바꿈을 제외한 모든 글자이므로
#단어의 시작이든 끝이든 중간이든 상관없이
#뽑으라는 것 그것이 *이라는 [0,]의 범위를 가지므로
hwa<-cuso[grep(".*주방.*", cuso$CLAC1_NM),]
#중분류로 하고 싶을 때
hwa<-cuso[grep(".*골프.*", cuso$CLAC2_NM),]
#소분류로 하고 싶을 때
hwa<-cuso[grep(".*수도용품.*", cuso$CLAC3_NM),]





#################성별#############################
#총 성별 인원 구한 것 해당 소분류에 대한 시각화를 하기 위해서는 이 코드는 고정

ss<-cuso %>% group_by(CLNT_ID,CLNT_GENDER) %>% tally()
ss_a<-ss %>% group_by(CLNT_GENDER) %>% summarise(남녀합 = n())


#성별 소분류에서 구입한 것 구한 것(빈도 x 단순히 행의 개수를 계산)
sex2<- hwa %>% group_by(CLNT_ID,CLNT_GENDER) %>% tally()
sex2_a<-sex2 %>% group_by(CLNT_GENDER) %>% summarise(남녀합 = n())
hwa2 <- hwa %>% group_by(CLNT_GENDER) %>% summarise(빈도 = n())

hwa2$비율<-(sex2_a$남녀합/ss_a$남녀합)
hwa2

windows()
ggplot(hwa2,aes(CLNT_GENDER,비율,group=CLNT_GENDER,fill=CLNT_GENDER))+
  geom_bar(stat="identity",position="dodge")


#################################################################
###나이대########################################################
unique(cuso$CLAC2_NM)
hwa<-cuso[grep(".*.*", cuso$CLAC3_NM),]
str(hwa)
hwa<-cuso[grep(".*주방.*", cuso$CLAC1_NM),]
str(hwa)
##########고정##
kk<-cuso %>% group_by(CLNT_ID,CLNT_AGE) %>% tally()
kk_a<-kk %>% group_by(CLNT_AGE) %>% summarise(나이별합 = n())
###############

age2<- hwa %>% group_by(CLNT_ID,CLNT_AGE) %>% tally()
age2_a<-age2 %>% group_by(CLNT_AGE) %>% summarise(나이별합 = n())
hwa3 <- hwa %>% group_by(CLNT_AGE) %>% summarise(빈도 = n())

hwa3$비율<-(age2_a$나이별합/kk_a$나이별합)
hwa3


windows()
ggplot(hwa3,aes(CLNT_AGE,비율2,group=CLNT_GENDER,fill=CLNT_GENDER))+
  geom_bar(stat="identity",position="dodge")




################성별 대분류
str(cuso)
str(cus)
View(cuso)
g1<-cuso %>% group_by(CLNT_GENDER,CLAC1_NM) %>% summarise(빈도=n())
ss_a<-as.data.frame(ss_a)
g2<-data.frame(성별=c(rep("F",37),c(rep("M",37))),비율=c(rep(ss_a[1,2],37),rep(ss_a[2,2],37)))
g1$비율<-(g1$빈도/g2$비율)

windows()
ggplot(g1,aes(CLAC1_NM,비율,group=CLNT_GENDER,col=CLNT_GENDER))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))

################성별 클러스터
gg1<-cuso %>% group_by(CLNT_GENDER,clu) %>% summarise(빈도=n())
ss_a<-as.data.frame(ss_a)
gg2<-data.frame(성별=c(rep("F",47),c(rep("M",47))),비율=c(rep(ss_a[1,2],47),rep(ss_a[2,2],47)))
gg1$비율<-(gg1$빈도/gg2$비율)

windows()
str(gg1)
gg1<-as.data.frame(gg1)
ggplot(gg1,aes(clu,비율,group=CLNT_GENDER,col=CLNT_GENDER))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 47, 1))
###############
str(cuso)

###############나이대별 대분류 -80대가 너무 이상치처럼 뽑힘 (분모로들어가는 인원수가 너무 적기때문에)
g_1<-cuso %>% group_by(CLNT_AGE,CLAC1_NM) %>% summarise(빈도=n())
kk_a<-as.data.frame(kk_a)
View(g_1)
g_2<-data.frame(성별=c(rep("10",length(subset(g_1,CLNT_AGE==10)$CLNT_AGE)),c(rep("20",length(subset(g_1,CLNT_AGE==20)$CLNT_AGE))),
                     c(rep("30",length(subset(g_1,CLNT_AGE==30)$CLNT_AGE))),c(rep("40",length(subset(g_1,CLNT_AGE==40)$CLNT_AGE))),
                     c(rep("50",length(subset(g_1,CLNT_AGE==50)$CLNT_AGE))),c(rep("60",length(subset(g_1,CLNT_AGE==60)$CLNT_AGE))),
                     c(rep("70",length(subset(g_1,CLNT_AGE==70)$CLNT_AGE))),c(rep("80",length(subset(g_1,CLNT_AGE==80)$CLNT_AGE)))),
                  비율=c(rep(kk_a[1,2],length(subset(g_1,CLNT_AGE==10)$CLNT_AGE)),rep(kk_a[2,2],length(subset(g_1,CLNT_AGE==20)$CLNT_AGE)),
                       rep(kk_a[3,2],length(subset(g_1,CLNT_AGE==30)$CLNT_AGE)),rep(kk_a[4,2],length(subset(g_1,CLNT_AGE==40)$CLNT_AGE)),
                       rep(kk_a[5,2],length(subset(g_1,CLNT_AGE==50)$CLNT_AGE)),rep(kk_a[6,2],length(subset(g_1,CLNT_AGE==60)$CLNT_AGE)),
                       rep(kk_a[7,2],length(subset(g_1,CLNT_AGE==70)$CLNT_AGE)),rep(kk_a[8,2],length(subset(g_1,CLNT_AGE==80)$CLNT_AGE))))

g_1$비율 <- (g_1$빈도)/(g_2$비율)

library(RColorBrewer)
str(g_1)
g_1$CLNT_AGE<-factor(g_1$CLNT_AGE,levels=c(10,20,30,40,50,60,70,80))
ggplot(g_1,aes(CLAC1_NM,비율,group=CLNT_AGE,col=CLNT_AGE))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=30, hjust=1))
  
