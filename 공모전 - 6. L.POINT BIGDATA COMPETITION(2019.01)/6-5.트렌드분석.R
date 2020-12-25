library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
install.packages("xlsx")
library(xlsx)
install.packages("doBy")
library(doBy)
#네이버 검색트렌드와 비교하기 위한시각화
ntrend<-data.frame(fread("month_trend_table.csv",encoding="UTF-8"))
ntrend_a<-data.frame(fread("search_trend_table_plus.csv",encoding="UTF-8"))
str(ntrend_a)
str(ntrend)
View(ntrend)

#1
rownames(ntrend) <- ntrend$period
ntrend<-ntrend[,-1]

ntrend2 <- as.data.frame(t(as.matrix(ntrend)))
str(ntrend2)
View(ntrend2)

ntrend2 <- cbind(CLAC3_NM = rownames(ntrend2),ntrend2)
rownames(ntrend2) <- 1:nrow(ntrend2)
#2
rownames(ntrend_a) <- ntrend_a$period
ntrend_a<-ntrend_a[,-1]

ntrend2_a <- as.data.frame(t(as.matrix(ntrend_a)))
str(ntrend2_a)
View(ntrend2_a)

ntrend2_a <- cbind(CLAC3_NM = rownames(ntrend2_a),ntrend2_a)
rownames(ntrend2_a) <- 1:nrow(ntrend2_a)


ntrend2$CLAC3_NM<-gsub("X3단우산","3단우산",ntrend2$CLAC3_NM)
ntrend2$CLAC3_NM<-gsub("X2단우산","2단우산",ntrend2$CLAC3_NM)

#그냥 온점을 gsub를 한다면 정규표현식에서 모든 문자열을 의미하기 때문에 .앞에 \를 추가해준다.
ntrend2$CLAC3_NM<-gsub("\\.","/",ntrend2$CLAC3_NM)

ntrend2_a$CLAC3_NM<-gsub("\\.$",")",ntrend2_a$CLAC3_NM)
ntrend2_a$CLAC3_NM<-gsub("\\.","(",ntrend2_a$CLAC3_NM)

colnames(ntrend2_a)<-c("CLAC3_NM","m4","m5","m6","m7","m8","m9")
colnames(ntrend2)<-c("CLAC3_NM","m4","m5","m6","m7","m8","m9")
str(ntrend2_a)
str(ntrend2)

ntrend3<-rbind(ntrend2_a,ntrend2)
str(ntrend3)

ntrend3$clu<-ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[1,2]," ")),1,
                ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[2,2]," ")),2,
                       ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[3,2]," ")),3,
                              ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[4,2]," ")),4,
                                     ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[5,2]," ")),5,
                                            ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[6,2]," ")),6,
                                                   ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[7,2]," ")),7,
                                                          ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[8,2]," ")),8,
                                                                 ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[9,2]," ")),9,
                                                                        ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[10,2]," ")),10,
                                                                               ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[11,2]," ")),11,
                                                                                      ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[12,2]," ")),12,
                                                                                             ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[13,2]," ")),13,
                                                                                                    ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[14,2]," ")),14,
                                                                                                           ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[15,2]," ")),15,
                                                                                                                  ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[16,2]," ")),16,
                                                                                                                         ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[17,2]," ")),17,
                                                                                                                                ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[18,2]," ")),18,
                                                                                                                                       ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[19,2]," ")),19,
                                                                                                                                              ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[20,2]," ")),20,
                                                                                                                                                     ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[21,2]," ")),21,
                                                                                                                                                            ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[22,2]," ")),22,
                                                                                                                                                                   ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[23,2]," ")),23,
                                                                                                                                                                          ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[24,2]," ")),24,
                                                                                                                                                                                 ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[25,2]," ")),25,
                                                                                                                                                                                        ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[26,2]," ")),26,
                                                                                                                                                                                               ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[27,2]," ")),27,
                                                                                                                                                                                                      ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[28,2]," ")),28,
                                                                                                                                                                                                             ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[29,2]," ")),29,
                                                                                                                                                                                                                    ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[30,2]," ")),30,
                                                                                                                                                                                                                           ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[31,2]," ")),31,
                                                                                                                                                                                                                                  ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[32,2]," ")),32,
                                                                                                                                                                                                                                         ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[33,2]," ")),33,
                                                                                                                                                                                                                                                ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[34,2]," ")),34,
                                                                                                                                                                                                                                                       ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[35,2]," ")),35,
                                                                                                                                                                                                                                                              ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[36,2]," ")),36,
                                                                                                                                                                                                                                                                     ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[37,2]," ")),37,
                                                                                                                                                                                                                                                                            ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[38,2]," ")),38,
                                                                                                                                                                                                                                                                                   ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[39,2]," ")),39,
                                                                                                                                                                                                                                                                                          ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[40,2]," ")),40,
                                                                                                                                                                                                                                                                                                 ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[41,2]," ")),41,
                                                                                                                                                                                                                                                                                                        ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[42,2]," ")),42,
                                                                                                                                                                                                                                                                                                               ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[43,2]," ")),43,
                                                                                                                                                                                                                                                                                                                      ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[44,2]," ")),44,
                                                                                                                                                                                                                                                                                                                             ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[45,2]," ")),45,
                                                                                                                                                                                                                                                                                                                                    ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[46,2]," ")),46,
                                                                                                                                                                                                                                                                                                                                           ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[47,2]," ")),47,59)))))))))))))))))))))))))))))))))))))))))))))))



ntrend3$clu<-ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[48,2]," ")),48,
                ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[49,2]," ")),49,
                       ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[50,2]," ")),50,
                              ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[51,2]," ")),51,
                                     ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[52,2]," ")),52,
                                            ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[53,2]," ")),53,
                                                   ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[54,2]," ")),54,
                                                          ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[55,2]," ")),55,
                                                                 ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[56,2]," ")),56,
                                                                        ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[57,2]," ")),57,
                                                                               ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[58,2]," ")),58,
                                                                                      ifelse(ntrend3$CLAC3_NM %in% unlist(strsplit(clu[60,2]," ")),60,ntrend3$clu))))))))))))
str(ntrend3)
length(unique(ntrend3$clu))
sum(is.na(ntrend3))
ntrend3[is.na(ntrend3)]<-0

#기존의 데이터에서 월별 정보 subset
###############################################################################
###############################################################################
###############################################################################
#군집별 월별 구매량 시각화 --> count나 구매건수의 합이나 그래프의 형태는 비슷 
month_length<- data.frame(clu=integer(0),length=numeric(0))
View(clu)

for(i in 1:60){
  month_len=data.frame(clu=i,length=length(unlist(strsplit(clu[i,2]," "))))
  month_length = bind_rows(month_length,month_len)
}

str(month_length)
sum(month_length$length)

str(cuso)
length(unique(cuso$clu))
cuso$month <- substr(cuso$SESS_DT,6,7)

clu_m4 <- subset(cuso,month=="04")
str(clu_m4)
gro_m4<- clu_m4 %>% group_by(clu) %>% summarise(count=sum(PD_BUY_CT))
str(gro_m4)

clu_m5 <- subset(cuso,month=="05")
str(clu_m5)
gro_m5<- clu_m5 %>% group_by(clu) %>% summarise(count=sum(PD_BUY_CT))
str(gro_m5)



clu_m6 <- subset(cuso,month=="06")
str(clu_m6)
gro_m6<- clu_m6 %>% group_by(clu) %>% summarise(count=sum(PD_BUY_CT))
str(gro_m6)


clu_m7 <- subset(cuso,month=="07")
str(clu_m7)
gro_m7<- clu_m7 %>% group_by(clu) %>% summarise(count=sum(PD_BUY_CT))
str(gro_m7)

clu_m8 <- subset(cuso,month=="08")
str(clu_m8)
gro_m8<- clu_m8 %>% group_by(clu) %>% summarise(count=sum(PD_BUY_CT))
str(gro_m8)

clu_m9 <- subset(cuso,month=="09")
str(clu_m9)
gro_m9<- clu_m9 %>% group_by(clu) %>% summarise(count=sum(PD_BUY_CT))
str(gro_m9)



#4월####################################################
ntrend3_eda<-ntrend3 %>% group_by(clu) %>% summarise(month4_a=sum(m4))
str(ntrend3_eda)
rb<-cbind(ntrend3_eda,gro_m4$count)
str(rb)
colnames(rb)<-c("clu","month4_a","범주","month4")
rb<-rb[,-3]

#상관계수보기위해서
cor(rb[-c(58,54,9,3),-1])

#축을 다르게 하여 시각화
windows()
ggplot(rb)+
  geom_line(aes(x=clu,y=month4_a,col="구매량",group=1))+
  geom_line(aes(x=clu,y=month4*1/10,col="검색 트렌드",group=1))+
  scale_y_continuous(name = "검색 트렌드",sec.axis = sec_axis(~ . * 10 , name = "구매량"))+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1),name="군집")+
  ggtitle("네이버 검색트렌드와 구매량의 추이 - 4월")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 18, face="bold"))


#5월####################################################
str(ntrend3)
ntrend3_eda2<-ntrend3 %>% group_by(clu) %>% summarise(month5_a=sum(m5))
str(ntrend3_eda2)
View(gro_m5)
gro_m5[60,]<-c(60,0)
rb2<-cbind(ntrend3_eda2,gro_m5$count)
str(rb2)
colnames(rb2)<-c("clu","month5_a","month5")

cor(rb2[-c(58,9,41,3,54),-1])


ggplot(rb2)+
  geom_line(aes(x=clu,y=month5_a,col="구매량",group=1))+
  geom_line(aes(x=clu,y=month5*1/10,col="검색 트렌드",group=1))+
  scale_y_continuous(name = "검색 트렌드",sec.axis = sec_axis(~ . * 10 , name = "구매량"))+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1),name="군집")+
  ggtitle("네이버 검색트렌드와 구매량의 추이 - 5월")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 18, face="bold"))
                     
#6월####################################################
View(gro_m6)
gro_m6[60,]<-c(42,100)
str(ntrend3)
ntrend3_eda3<-ntrend3 %>% group_by(clu) %>% summarise(month6_a=sum(m6))
str(ntrend3_eda3)
rb3<-merge(ntrend3_eda3,gro_m6,by="clu",all.x=T)
str(rb3)
colnames(rb3)<-c("clu","month6_a","month6")
cor(rb3[-c(3,9,27,54,58),-1])

ggplot(rb3)+
  geom_line(aes(x=clu,y=month6_a,col="구매량",group=1))+
  geom_line(aes(x=clu,y=month6*1/10,col="검색 트렌드",group=1))+
  scale_y_continuous(name = "검색 트렌드",sec.axis = sec_axis(~ . * 10 , name = "구매량"))+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1),name="군집")+
  ggtitle("네이버 검색트렌드와 구매량의 추이 - 6월")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 18, face="bold"))

#7월####################################################                     
View(gro_m7)
str(ntrend3)
ntrend3_eda4<-ntrend3 %>% group_by(clu) %>% summarise(month7_a=sum(m7))
str(ntrend3_eda4)
rb4<-merge(ntrend3_eda4,gro_m7,by="clu",all.x=T)
str(rb4)
colnames(rb4)<-c("clu","month7_a","month7")
cor(rb4[-c(3,9,27,54,58,41),-1])

ggplot(rb4)+
  geom_line(aes(x=clu,y=month7_a,col="구매량",group=1))+
  geom_line(aes(x=clu,y=month7*1/10,col="검색 트렌드",group=1))+
  scale_y_continuous(name = "검색 트렌드",sec.axis = sec_axis(~ . * 10 , name = "구매량"))+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1),name="군집")+
  ggtitle("네이버 검색트렌드와 구매량의 추이 - 7월")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 18, face="bold"))

#8월####################################################
View(gro_m8)
gro_m8[60,]<-c(42,100)
str(ntrend3)
ntrend3_eda5<-ntrend3 %>% group_by(clu) %>% summarise(month8_a=sum(m8))
str(ntrend3_eda5)
rb5<-merge(ntrend3_eda5,gro_m8,by="clu",all.x=T)
str(rb5)
colnames(rb5)<-c("clu","month8_a","month8")
cor(rb[-c(3,9,27,41,54,58),-1])

ggplot(rb5)+
  geom_line(aes(x=clu,y=month8_a,col="구매량",group=1))+
  geom_line(aes(x=clu,y=month8*1/10,col="검색 트렌드",group=1))+
  scale_y_continuous(name = "검색 트렌드",sec.axis = sec_axis(~ . * 10 , name = "구매량"))+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1),name="군집")+
  ggtitle("네이버 검색트렌드와 구매량의 추이 - 8월")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 18, face="bold"))

#9월####################################################
View(gro_m9)
gro_m9[60,]<-c(42,100)
str(ntrend3)
ntrend3_eda6<-ntrend3 %>% group_by(clu) %>% summarise(month9_a=sum(m9))
str(ntrend3_eda6)
rb6<-merge(ntrend3_eda6,gro_m9,by="clu",all.x=T)
str(rb6)
colnames(rb6)<-c("clu","month9_a","month9")
cor(rb6[-c(3,9,27,41,54,58),-1])


ggplot(rb6)+
  geom_line(aes(x=clu,y=month9_a,col="구매량",group=1))+
  geom_line(aes(x=clu,y=month9*1/10,col="검색 트렌드",group=1))+
  scale_y_continuous(name = "검색 트렌드",sec.axis = sec_axis(~ . * 10 , name = "구매량"))+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1),name="군집")+
  ggtitle("네이버 검색트렌드와 구매량의 추이 - 9월")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 18, face="bold"))


#검생량과 구매건수 월 총평균
#검색량
cb_ser<-cbind(ntrend3_eda[,c(1,2)],ntrend3_eda2[,2],ntrend3_eda3[,2],ntrend3_eda4[,2],ntrend3_eda5[,2],ntrend3_eda6[,2])
str(cb_ser)

cb_ser_f<-function(x){
  sum(x)
}

cb_a<-apply(cb_ser[,-1],1,cb_ser_f)
str(cb_a)
#구매건수
str(gro_m9)
cb_data<-cbind(gro_m4,gro_m5[,2],gro_m6[,2],gro_m7[,2],gro_m8[,2],gro_m9[,2])
str(cb_data)

cb_ser_f2<-function(x){
  sum(x)
}

cb_a2<-apply(cb_data[,-1],1,cb_ser_f2)
str(cb_a2)

str(rb6)
rb7<-data.frame(clu=c(1:60),군집별트랜드=cb_a,군집별구매량=cb_a2)
str(rb7)
ggplot(rb7)+
  geom_line(aes(x=clu,y=군집별트랜드,col="군집별트랜드",group=1))+
  geom_line(aes(x=clu,y=군집별구매량*1/10,col="군집별구매량",group=1))+
  scale_y_continuous(name = "군집별트랜드",sec.axis = sec_axis(~ . * 10 , name = "군집별구매량"))+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1))
cor(rb7[,-1])


####################################################################
####################################################################
####################################################################
#########################트렌드 전체 eda###########################
str(ntrend3)
str(cuso3)
cuso3$month<-substr(cuso3$SESS_DT,6,7)

t_4<-ntrend3[,c(1,2,8)]
str(t_4)
t_4$month<-"04"
colnames(t_4)<-c("CLAC3_NM","trend","clu","month")

t_5<-ntrend3[,c(1,3,8)]
t_5$month<-"05"
colnames(t_5)<-c("CLAC3_NM","trend","clu","month")
str(t_5)

t_6<-ntrend3[,c(1,4,8)]
t_6$month<-"06"
colnames(t_6)<-c("CLAC3_NM","trend","clu","month")
str(t_6)


t_7<-ntrend3[,c(1,5,8)]
t_7$month<-"07"
colnames(t_7)<-c("CLAC3_NM","trend","clu","month")
str(t_7)

t_8<-ntrend3[,c(1,6,8)]
t_8$month<-"08"
colnames(t_8)<-c("CLAC3_NM","trend","clu","month")
str(t_8)

t_9<-ntrend3[,c(1,7,8)]
t_9$month<-"09"
colnames(t_9)<-c("CLAC3_NM","trend","clu","month")
str(t_9)


trend<-rbind(t_4,t_5,t_6,t_7,t_8,t_9)
str(trend)
str(cuso3)
View(trend)
trend<-trend[,c(1,2,4)]

cuso33<-merge(cuso3,trend,by=c("CLAC3_NM","clu","month"),all.x=T)
cuso33<-left_join(cuso3, trend, by = c("CLAC3_NM","month"))
str(cuso33)
length(unique(cuso33$clu))

write.csv(cuso33[,c(1,22,25)],"cuso33.csv",row.names=F)

#메모리할당문제
cstr_trend <- dcast(cuso33[,c(1,22,25)], CLNT_ID ~ clu, value.var="trend", fun.aggregate=sum)

#파이썬으로해서 불러오기
cstr_trend<-data.frame(fread("민용변수5.csv"))
str(cstr_trend)
str(cuso33)
sum(is.na(cstr_trend))

ser_cv3<-function(x){
  sd(x)/(sum(x)/60)
}

ser_cv_f3<-apply(cstr_trend[,-1],1,ser_cv3)
str(ser_cv_f3)

#트렌드 군집eda
cstr_aa4<-cuso33 %>% group_by(clu) %>% summarise(검색트렌드=sum(trend))
str(cstr_aa4)

ggplot(cstr_aa4,aes(clu,검색트렌드,col="red"))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1),name="군집")+
  ggtitle("군집별 검색 트렌드 추세")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.position = "none",
        strip.text.x = element_text(size = 18, face="bold"))
