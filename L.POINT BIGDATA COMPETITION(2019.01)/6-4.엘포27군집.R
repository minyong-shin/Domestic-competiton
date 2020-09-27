library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
install.packages("xlsx")
library(xlsx)
install.packages("doBy")
library(doBy)
clst27<-data.frame(fread("clst_27.csv"))
str(clst27)
#성별
clst27_a<-subset(clst27,n==1)
str(clst27_a)

sum(clst27_a$man)
sum(clst27_a$female)
sex<-data.frame(sex=c("Man","Woman"),인원=c(376,4624))
str(sex)

ggplot(sex,aes(sex,인원,fill=sex))+
  geom_bar(stat="identity",width=0.3)+
  ggtitle("고객별 성별 차이")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"))

#나이
sum(clst27_a$X10age)
sum(clst27_a$X20age)
sum(clst27_a$X30age)
sum(clst27_a$X40age)
sum(clst27_a$X50age)
sum(clst27_a$X60age)
sum(clst27_a$X70age)
sum(clst27_a$X80age)


age<-data.frame(AGE=c("10대","20대","30대","40대","50대","60대","70대","80대"),인원=c(1,52,2884,2003,38,20,2,0))
ggplot(age,aes(AGE,인원,fill=AGE))+
  geom_bar(stat="identity",width=0.3)+
  ggtitle("고객별 나이대 차이")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"))

#RFM분석
str(clst27)
clst27$n<-factor(clst27$n,levels=c(0,1))

rec_eda<-ggplot(clst27,aes(n,rec,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 거래의 최근성")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),legend.position = "none")+
  scale_x_discrete(name = "")+
  scale_y_continuous(name = "거래의 최근성",
                     limits=c(0, 150))

fre_eda<-ggplot(clst27,aes(n,fre,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 총 구매건수")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),legend.position = "none")+
  scale_x_discrete(name = "")+
  scale_y_continuous(name = "고객별 총 구매건수",
                     limits=c(0, 40))
str(clst27)
mon_eda<-ggplot(clst27,aes(n,total_price,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 구매총액")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 구매총액",
                     limits=c(0, 1000000))

library(ggpubr)
ggarrange(rec_eda,fre_eda,mon_eda, 
          labels = c("Recency", "Frequency","Monetary"),
          ncol = 3, nrow = 1)
#rfm-score
ggplot(clst27,aes(n,rfm_score,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 RFM-Score")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 RFM-Score",
                     limits=c(0, 1.5))
#구매관련변수
str(clst27)
#구매단가의 합
unit_eda<-ggplot(clst27,aes(n,unit_price,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 구매단가의 합")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 구매단가의 합",
                     limits=c(0, 1500000))

#구매총액의 평균
mean_eda<-ggplot(clst27,aes(n,totalcost.mean,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 구매총액의 평균")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 구매총액의 평균",
                     limits=c(0, 100000))
mean_eda

#구매총액의 중앙값
median_eda<-ggplot(clst27,aes(n,totalcost.median,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 구매총액의 중앙값")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 구매총액의 중앙값",
                     limits=c(0, 100000))
median_eda

#구매횟수
count_eda<-ggplot(clst27,aes(n,totalcost.length,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 구매횟수")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 구매횟수",
                     limits=c(0, 20))
count_eda

ggarrange(unit_eda,count_eda,mean_eda,median_eda, 
          ncol = 4, nrow = 1)

#변동계수시각화
str(clst27)

ggplot(clst27,aes(n,total_price_cv,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 변동계수")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 변동계수",
                     limits=c(0.3, 2))
#구매주기 시각화
ggplot(clst27,aes(n,purchase_cycle,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 구매주기")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 구매주기",
                     limits=c(0.3, 30))

#구매량 변동계수시각화
str(clst27)
ggplot(clst27,aes(n,nmbr_prchs_cv,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 구매량 CV")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 구매량 CV",
                     limits=c(0.3, 8))


#구매총액 변동계수시각화
str(clst27)
ggplot(clst27,aes(n,clst_total_price_cv,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 구매총액 CV")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 구매총액 CV",
                     limits=c(0.3, 8))

#검색량 변동계수시각화
str(clst27)
ggplot(clst27,aes(n,srch_amnt_cv,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 검색량 CV")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 검색량 CV",
                     limits=c(0.3, 18))

#고객별 외부변수시각화
ggplot(clst27,aes(n,naver_trend_cv,fill=n,group=n))+
  geom_boxplot()+
  ggtitle("고객별 네이버트렌드 CV")+
  scale_x_discrete(name = "")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 20),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18),
        strip.text.x = element_text(size = 18, face="bold"),
        legend.position = "none")+
  scale_y_continuous(name = "고객별 네이버트렌드 CV",
                     limits=c(0.3, 10))
