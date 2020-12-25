ku<-read.csv("경기도 미세먼지.csv")
str(ku)
View(ku)
colnames(ku)<-c("year","pm10")
mean(ku$pm10[1:90])
mean(ku$pm10[183:274])

mean(ku$pm10[397:486])
mean(ku$pm10[579:662])

ku2011<-ku[c(1:90,183:274),]
ku2017<-ku[c(397:486,579:662),]

sum(ku2011$pm10[1:90] >= 81) 
sum(ku2011$pm10[91:182] >= 81) 

sum(ku2017$pm10[1:90] >= 81) 
sum(ku2017$pm10[91:174] >= 81) 

mean(ku2017$pm10[1:90] >= 81) 

mean(subset(ku2017,pm10 >= 81)$pm10)


#electronic
pica<-read.csv("pica.csv",header=T)
str(pica)
pica$기간<-as.character(pica$기간)
pica<-pica[pica$지역!="합계",]
pica$year<-substr(pica$기간,1,4)
pica$month<-substr(pica$기간,6,7)
pica$month<-ifelse(pica$month=="1","10",pica$month)
unique(pica$month)

library(dplyr)
pica<-pica %>% select(month,year,지역,합계,가정용,공공용,서비스소계,산업용소계)

unique(pica$month)
write.csv(pica,"pica1.csv")
dim(pica)

fin<-read.csv("finalx.csv")
str(fin)

step(lm(mdust~1,fin),scope=formula(lm(mdust~.,fin)),direction = "forward")

summary(lm(formula = mdust ~ meandust + meantemp + name + year + month + 
             meanhumi + building_area + apt_green_size, data = fin))


trend<-read.csv("trend.csv",header = T)
str(trend)
trend$date<-as.Date(trend$date)
trend$year<-substr(trend$date,1,4)
trend$day<-paste(substr(trend$date,6,7),substr(trend$date,9,10),sep="")
trend$year<-as.numeric(trend$year)
trend2<-trend[trend$year<2018,]
str(trend2)

trend$month<-substr(trend$date,6,7)

library(ggplot2)
windows()
ggplot(trend2, aes(date,dtrend,colour="미세먼지"))+
  xlab("Date")+
  ylab("rate")+
  geom_line()+
  geom_line(aes(date,mdtrend,colour="초미세먼지"))+scale_colour_manual(values=c("blue","red"))+
  scale_x_date(breaks=datebreaks) +theme(axis.text.x = element_text(angle=30, hjust=1))+
  ggtitle("미세먼지, 초미세먼지 구글트렌드")+
  theme(plot.title = element_text(size = 25, family = "Tahoma", face = "bold"),
        text = element_text(size = 15, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))


datebreaks <- seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by="1 month")


trend2
trend2$date<-as.Date(trend2$date)
trend2$date2<-paste(substr(trend2$date,1,4),substr(trend2$date,6,7),substr(trend2$date,9,10),sep="")
str(trend2)
trend2$date2<-as.integer(trend2$date2)
trend2$month<-as.numeric(substr(trend2$date2,5,6))


plot(trend2$date,trend2$dtrend,type="l")
lines(trend2$date,trend2$mdtrend,col="red")

plot()
table(trend2$date)

##zinzza
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
zin<-read.csv("zinzza.csv",header=T)
str(zin)
zin<-zin[,-1]
str(zin)
windows()
zin2017<-zin[zin$year>2016,]
str(zin2017)
dim(zin2017)


#건물밀도
windows()
str(zin2017)
zin2017gun<-zin2017 %>% group_by(name) %>% summarise(mean = mean(building_per))
ggplot(zin2017gun,aes(x=reorder(name,-mean),y=mean))+
  geom_bar(size=1,stat = "identity",fill='#9966FF')+
  #scale_fill_manual("legend", values = c("중구" = "black", "동대문구" = "black", "강남구" = "black"))+
  scale_y_continuous(name = "건축물 면적/구 면적",
                     limits=c(0, 2.25))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 건물 밀도") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  geom_text(x=1,y=2.15,label="중구",size=8,col="#CC0000")+
  geom_text(x=3,y=1.35,label="강남구",size=8,col="#CC0000")+
  geom_text(x=2,y=1.45,label="동대문구",size=8,col="#CC0000")+
  geom_text(x=21.9,y=0.8,label="은평구",size=8)+
  geom_text(x=23,y=0.75,label="노원구",size=8)+
  geom_text(x=24.2,y=0.72,label="종로구",size=8)+
  geom_text(x=25,y=0.60,label="강북구",size=8)+
  geom_hline(yintercept = mean(zin2017gun$mean),col="red",cex=0.9)

a=mean(zin2017$building_per)


#인구밀도
str(zin2017)
zin2017pop<-zin2017 %>% group_by(name) %>% summarise(mean = mean(pop))

windows()
ggplot(zin2017pop,aes(x=reorder(name,-mean),y=mean))+
  geom_bar(size=1,stat = "identity",fill='#000666')+
  scale_y_continuous(name = "인구 밀도",
                     limits=c(0, 30000))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 인구 밀도(단위:명/㎢)") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  geom_text(x=1,y=28500,label="양천구",size=8,col="#CC0000")+
  geom_text(x=3.3,y=25700,label="동작구",size=8,col="#CC0000")+
  geom_text(x=2,y=27000,label="동대문구",size=8,col="#CC0000")+
  geom_text(x=24,y=11000,label="서초구",size=7)+
  geom_text(x=23,y=13000,label="용산구",size=7)+
  geom_text(x=25,y=8500,label="종로구",size=7)+
  geom_hline(yintercept  = mean(zin2017pop$mean),col="red",cex=0.9)
#position='dodge' =barplot beside=T

#열섬현상
str(zin2017)
win17 <-rbind(subset(zin2017, month==11),
               subset(zin2017, month==12),
               subset(zin2017, month==1))
sum17 <-rbind(subset(zin2017, month==6),
              subset(zin2017, month==7),
              subset(zin2017, month==8))

library(dplyr)
win17a<-win17 %>% group_by(name) %>% summarise(meanhot=mean(sumhot))
str(win17a)
win17a$season<-"겨울"

sum17a<-sum17 %>% group_by(name) %>% summarise(meanhot=mean(sumhot))
sum17a$season<-"여름"
str(sum17a)

wisu<-rbind(win17a,sum17a)
dim(wisu)
str(wisu)
wisu<-as.data.frame(wisu)


#열섬
windows()
str(wisu)
str(zin2017pop)
ggplot(wisu,aes(x=reorder(name,-meanhot),y=meanhot))+
  geom_bar(size=1,stat = "identity",fill='#CC3300')+
  scale_y_continuous(name = "열섬 현상 일수",
                     limits=c(0, 13))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 열섬 현상") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 25,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        strip.text.x = element_text(size = 20, face="bold"))+
  facet_grid(. ~ season) + 
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=c(8,4,2,1.3,6,3.3,1,5.4,7,2), 
                            y=c(9,12.2,8.8,9.8,8.4,9.8,12.1,9.8,7.6,7.4), 
                            lab=c("강동구","노원구","광진구","송파구","서초구","용산구","송파구","종로구","금천구","광진구"),
                            season=le[1:2]), vjust=1,size=7.5)+
  geom_hline(data=dummy2,aes(yintercept = Z),col="darkgreen",cex=0.9)

le=c("겨울","여름")
le
dummy2 <- data.frame(season = c("겨울", "여름"), Z = c(mean(win17a$meanhot),mean(sum17a$meanhot)))

str(wisu)
wisu$season<-as.factor(wisu$season)

#ggplot 격자별 y절편 다르게 표시
z=c(7.4,6)
table(wisu$season)
sum17a<-as.data.frame(sum17a)
str(sum17a)
win17a<-as.data.frame(win17a)

dummy1 <- expand.grid(X = factor(c("A", "B")), Y = rnorm(10))
dummy1$D <- rnorm(nrow(dummy1))
dummy2 <- data.frame(season = c("겨울", "여름"), Z = c(mean(win17a$meanhot),mean(sum17a$meanhot)))
ggplot(dummy1, aes(x = D, y = Y)) + geom_point() + facet_grid(~X) + 
  geom_hline(data = dummy2, aes(yintercept = Z))
#ggplot 격자별 text 다른위치에 표시
x <-runif(9, 0, 125) 
data <- as.data.frame(x) 
data$y <- runif(9, 0, 125) 
data$yy <- factor(c("a","b","c")) 

ggplot(data, aes(x, y)) + 
  geom_point(shape = 2) + 
  facet_grid(~yy) + 
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=c(60,25,30,40,30,40), y=c(20,30,40,50,30,30), lab=c("this","le","is","the way","a","b"),
                            yy=letters[1:3]), vjust=1)

#가정전력사용량
str(zin2017)
gaw17 <-rbind(subset(zin2017, month==11),
              subset(zin2017, month==12),
              subset(zin2017, month==1))
gas17 <-rbind(subset(zin2017, month==6),
              subset(zin2017, month==7),
              subset(zin2017, month==8))

library(dplyr)
win17ga<-gaw17 %>% group_by(name) %>% summarise(meanele=mean(fam_elect))
str(win17ga)
win17ga$season<-"겨울"

sum17ga<-gas17 %>% group_by(name) %>% summarise(meanele=mean(fam_elect))
sum17ga$season<-"여름"
str(sum17a)

gasw<-rbind(win17ga,sum17ga)
dim(gasw)
str(gasw)
gasw<-as.data.frame(gasw)


windows()
ggplot(gasw,aes(x=reorder(name,-meanele),y=meanele))+
  geom_bar(size=1,stat = "identity",fill='#FFCC33')+
  scale_y_continuous(name = "가정 전력 사용량",
                     limits=c(0, 1250))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 가정 전력 사용량") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 25,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        strip.text.x = element_text(size = 20, face="bold"))+
  facet_grid(. ~ season) + 
  geom_text(aes(x, y, label=lab),
            data=data.frame(x=c(1.3,1.2,3.2,3.2,4,4,5,5,2.2,2.2,23,23,24,24,25,25), 
                            y=c(1210,1240,1115,1145,1050,1035,1010,1090,1160,1195,190,207,150,167,100,100), 
                            lab=c("강남구","강남구","강서구","강서구","노원구","노원구","서초구","서초구","송파구","송파구","금천구","금천구","종로구","종로구","중구","중구"),
                            season=le[1:2]), vjust=1,size=7.5,col=a)+
  geom_hline(data=dummy3,aes(yintercept = Z),col="#FF6600",cex=0.9)

dummy3 <- data.frame(season = c("겨울", "여름"), Z = c(mean(win17ga$meanele),mean(sum17ga$meanele)))
a=c("#CC0000","#CC0000","#CC0000","#CC0000","#CC0000","#000000","#000000","#000000","#CC0000","#CC0000","#CC0000","#CC0000","#CC0000","#000000","#000000","#000000")
sum(is.na(win17ga))
View(win17ga)
#가정용 진짜
str(zin2017)
gasw<-zin2017 %>% group_by(name) %>% summarise(meanele=mean(fam_elect))
gasw<-as.data.frame(gasw)
str(gasw)

ggplot(gasw,aes(x=reorder(name,-meanele),y=meanele))+
  geom_bar(size=1,stat = "identity",fill='#FFCC33')+
  scale_y_continuous(name = "가정 전력 사용량",
                     limits=c(0, 1250))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 가정 전력 사용량") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 25,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        strip.text.x = element_text(size = 20, face="bold"))+
  geom_text(x=1,y=1190,label="송파구",size=8,col="#CC0000")+
  geom_text(x=2.3,y=1180,label="강남구",size=8,col="#CC0000")+
  geom_text(x=3,y=1120,label="강서구",size=8,col="#CC0000")+
  geom_text(x=3.8,y=1000,label="노원구",size=8,col="#CC0000")+
  geom_text(x=5.1,y=1000,label="서초구",size=8,col="#CC0000")+
  geom_text(x=23,y=150,label="금천구",size=7)+
  geom_text(x=24,y=100,label="종로구",size=7)+
  geom_text(x=25,y=50,label="중구",size=7)+
  geom_hline(yintercept = mean(gasw$meanele),col="#FF6600",cex=0.9)


#산업 전력 사용량
str(zin2017)
indus<-zin2017 %>% group_by(name) %>% summarise(industry = mean(industry_elect))
str(indus)
indus<-as.data.frame(indus)

ggplot(indus,aes(x=reorder(name,-industry),y=industry))+
  geom_bar(size=1,stat = "identity",fill='#FFCC33')+
  scale_y_continuous(name = "산업용 전력 사용량",
                     limits=c(0, 1100))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 산업용 전력 사용량") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  geom_text(x=4.3,y=950,label="강서구",size=8,col="#CC0000")+
  geom_text(x=1.2,y=1060,label="동대문구",size=8,col="#CC0000")+
  geom_text(x=2,y=1010,label="종로구",size=8,col="#CC0000")+
  geom_text(x=3,y=950,label="중랑구",size=8,col="#CC0000")+
  geom_text(x=22,y=200,label="강남구",size=7)+
  geom_text(x=24,y=130,label="동작구",size=7)+
  geom_text(x=23,y=170,label="서대문구",size=7)+
  geom_text(x=25,y=130,label="은평구",size=7)+
  geom_hline(yintercept  = mean(indus$industry),col="red",cex=0.9)

#public 전력 사용량
pub<-zin2017 %>% group_by(name) %>% summarise(pub = mean(public_elect))
str(pub)
pub<-as.data.frame(pub)
windows()
ggplot(pub,aes(x=reorder(name,-pub),y=pub))+
  geom_bar(size=1,stat = "identity",fill='#FFCC33')+
  scale_y_continuous(name = "공공용 전력 사용량",
                     limits=c(0, 1100))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 공공용 전력 사용량") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  geom_text(x=1,y=1090,label="송파구",size=8,col="#CC0000")+
  geom_text(x=2,y=1030,label="광진구",size=8,col="#CC0000")+
  geom_text(x=3,y=980,label="은평구",size=8,col="#CC0000")+
  geom_text(x=4,y=900,label="양천구",size=8,col="#CC0000")+
  geom_text(x=22,y=330,label="노원구",size=7)+
  geom_text(x=23.2,y=325,label="금천구",size=7)+
  geom_text(x=24,y=260,label="마포구",size=7)+
  geom_text(x=25,y=190,label="영등포구",size=7)+
  geom_hline(yintercept  = mean(pub$pub),col="red",cex=0.9)

#service 전력 사용량
str(zin2017)
ser<-zin2017 %>% group_by(name) %>% summarise(ser = mean(service_elect))
str(ser)
ser<-as.data.frame(ser)
windows()
ggplot(ser,aes(x=reorder(name,-ser),y=ser))+
  geom_bar(size=1,stat = "identity",fill='#FFCC33')+
  scale_y_continuous(name = "서비스용 전력 사용량",
                     limits=c(0, 1100))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 서비스용 전력 사용량") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20))+
  geom_text(x=0.95,y=1020,label="양천구",size=8,col="#CC0000")+
  geom_text(x=2.25,y=1020,label="금천구",size=8,col="#CC0000")+
  geom_text(x=3,y=970,label="성동구",size=8,col="#CC0000")+
  geom_text(x=4.2,y=980,label="강동구",size=8,col="#CC0000")+
  geom_text(x=22,y=300,label="서초구",size=7)+
  geom_text(x=23,y=245,label="중구",size=7)+
  geom_text(x=24,y=215,label="영등포구",size=7)+
  geom_text(x=25,y=170,label="송파구",size=7)+
  geom_hline(yintercept  = mean(ser$ser),col="red",cex=0.9)


#미세먼지(3,4,5월),초미세먼지(3월만)
str(zin2017)
dust <- rbind(subset(zin2017, month==3),
                subset(zin2017, month==4),
                subset(zin2017, month==5))
str(dust)

dust2<- dust %>% group_by(name) %>% summarise(dustmean = mean(meandust))

str(dust2)
dust2<-as.data.frame(dust2)

ggplot(dust2,aes(x=reorder(name,-dustmean),y=dustmean))+
  geom_bar(size=1,stat = "identity",fill='#006699')+
  scale_y_continuous(name = "평균 미세먼지 농도",
                     limits=c(0, 75))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 미세먼지 농도") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  geom_text(x=3,y=67,label="강동구",size=8,col="#CC0000")+
  geom_text(x=1,y=73,label="성동구",size=8,col="#CC0000")+
  geom_text(x=2,y=70,label="영등포구",size=8,col="#CC0000")+
  geom_text(x=25,y=52,label="강북구",size=7)+
  geom_text(x=23,y=55,label="동작구",size=7)+
  geom_text(x=24,y=53,label="용산구",size=7)+
  geom_hline(yintercept  = mean(dust2$dustmean),col="red",cex=0.9)

#초미세먼지
str(dust)

dust3<-subset(dust,month==3)
str(dust3)
head(dust3)  
View(dust3)
dim(dust3)

ggplot(dust3,aes(x=reorder(name,-mdust),y=mdust))+
  geom_bar(size=1,stat = "identity",fill='#999999')+
  scale_y_continuous(name = "초미세먼지 농도",
                     limits=c(0, 55))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 초미세먼지 농도") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  geom_text(x=1,y=52,label="마포구",size=8,col="#CC0000")+
  geom_text(x=1.8,y=47,label="강남구",size=8,col="#CC0000")+
  geom_text(x=3.1,y=47,label="성동구",size=8,col="#CC0000")+
  geom_text(x=25,y=34,label="강북구",size=7)+
  geom_text(x=22.9,y=37,label="도봉구",size=7)+
  geom_text(x=24.15,y=36,label="동대문구",size=7)+
  geom_hline(yintercept  = mean(dust3$mdust),col="red",cex=0.9)


#서울시 녹지 전체 면적
str(zin2017)
View(zin2017)
zin2017a<-subset(zin2017,month==1)
str(zin2017a)
zin2017a<-zin2017 %>% group_by(name) %>% summarise(meansize = mean(green_size))
str(zin2017a)
zin2017a<-as.data.frame(zin2017a)

ggplot(zin2017a,aes(x=reorder(name,-meansize),y=meansize))+
  geom_bar(size=1,stat = "identity",fill='#339900')+
  scale_y_continuous(name = "녹지 면적",
                     limits=c(0, 2250000))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 녹지 면적") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  geom_text(x=2,y=1650000,label="강남구",size=8,col="#CC0000")+
  geom_text(x=3,y=1500000,label="서초구",size=8,col="#CC0000")+
  geom_text(x=1,y=2100000,label="중구",size=8,col="#CC0000")+
  geom_text(x=25,y=200000,label="강북구",size=7)+
  geom_text(x=24,y=250000,label="관악구",size=7)+
  geom_text(x=23,y=300000,label="종로구",size=7)+
  geom_hline(yintercept  = mean(zin2017a$meansize),col="red",cex=0.9)


#서울시 아파트 녹지 면적
str(zin2017)
zin2017apt<-zin2017 %>% group_by(name) %>% summarise(meansize = mean(apt_green_size))
str(zin2017apt)
zin2017apt<-as.data.frame(zin2017apt)
windows()
ggplot(zin2017apt,aes(x=reorder(name,-meansize),y=meansize))+
  geom_bar(size=1,stat = "identity",fill='#66CC00')+
  scale_y_continuous(name = "건물 녹지 면적",
                     limits=c(0, 150000))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 건물 녹지 면적") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  geom_text(x=2,y=120000,label="강남구",size=8,col="#CC0000")+
  geom_text(x=1,y=139000,label="노원구",size=8,col="#CC0000")+
  geom_text(x=3,y=100000,label="은평구",size=8,col="#CC0000")+
  geom_text(x=24,y=26000,label="금천구",size=7)+
  geom_text(x=23,y=32000,label="서대문구",size=7)+
  geom_text(x=25,y=20000,label="종로구",size=7)+
  geom_hline(yintercept  = mean(zin2017apt$meansize),col="red",cex=0.9)

str(zin)
windows()

str(zin)
ggplot(zin)+
  geom_point(aes(x= month , y=meandust))
                                                           
zin20<-zin[zin$year>2015,]
str(zin20)

zin20a <- zin20 %>% group_by(year) %>% summarise(mdust=mean(mdust),meandust=mean(meandust),size=mean(green_size)) 
zin20a<-as.data.frame(zin20a)
str(zin20a)
zin20a$year<-as.Date(zin20a$year,format="yyyy")

str(trend2)
ggplot(zin20a, aes(year,meandust,colour="미세먼지"))+
  xlab("Date")+
  ylab("rate")+
  geom_line()+
  geom_line(aes(year,mdust,colour="초미세먼지"))+scale_colour_manual(values=c("blue","red","black"))+
  geom_line(aes(year,size,colour="초미세먼지"))+scale_colour_manual(values=c("blue","red","black"))+
  theme(axis.text.x = element_text(angle=30, hjust=1))+
  ggtitle("미세먼지, 초미세먼지 구글트렌드")+
  scale_y_continuous(sec.axis = sec_axis(~.*1600, name="y2*651"))+
  theme(plot.title = element_text(size = 25, family = "Tahoma", face = "bold"),
        text = element_text(size = 15, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))

scale_y_continuous(sec.axis = sec_axis(~.*1600, name="y2*651"))

datebreaks <- seq(2016.00,2017.00, 1)
datebreaks
geom_line(aes(date,mdtrend,colour="초미세먼지"))+scale_colour_manual(values=c("blue","red"))

scale_x_date(breaks=datebreaks) 

str(zin20a)
zin20b<-data.frame(year=c(2016,2017,2016,2017,2016,2017),num=c(26.2,24.6,47.8,43.8,603333,612668),a=c("초미세먼지","초미세먼지","미세먼지","미세먼지","녹지면적","녹지면적"))
str(zin20b)


ggplot(zin20b, aes(year,num,colour=a))+
  geom_line()+
  xlab("Date")+
  ylab("rate")
  
str(zin20a)

plot(zin20a$year,zin20a$mdust,type="l",ylim=c(20,50))
lines(zin20a$year,zin20a$meandust,type="l",col="red")
par(new=TRUE)
plot(zin20a$year,zin20a$size,type="l",col="blue")
axis(side=4)


str(zin2017)

a<-zin2017 %>% group_by(name) %>% summarise(sum=sum(fam_elect))
b<-zin2017 %>% group_by(name) %>% summarise(sum=sum(industry_elect))
c<-zin2017 %>% group_by(name) %>% summarise(sum=sum(service_elect))
d<-zin2017 %>% group_by(name) %>% summarise(sum=sum(public_elect))

sum(a$sum);sum(b$sum);sum(c$sum);sum(d$sum)



aaa<-zin2017 %>% group_by(name) %>% summarise(ser = mean(sumelect))
str(aaa)
aaa<-as.data.frame(aaa)
windows()
ggplot(aaa,aes(x=reorder(name,-ser),y=ser))+
  geom_bar(size=1,stat = "identity",fill='#FFCC33')+
  scale_y_continuous(name = "서비스용 전력 사용량",
                     limits=c(0, 1100))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 서비스용 전력 사용량") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20))+
  geom_text(x=0.95,y=1020,label="양천구",size=8,col="#CC0000")+
  geom_text(x=2.25,y=1020,label="금천구",size=8,col="#CC0000")+
  geom_text(x=3,y=970,label="성동구",size=8,col="#CC0000")+
  geom_text(x=4.2,y=980,label="강동구",size=8,col="#CC0000")+
  geom_text(x=22,y=300,label="서초구",size=7)+
  geom_text(x=23,y=245,label="중구",size=7)+
  geom_text(x=24,y=215,label="영등포구",size=7)+
  geom_text(x=25,y=170,label="송파구",size=7)+
  geom_hline(yintercept  = mean(aaa$ser),col="red",cex=0.9)

str(zin2017)
str(zin)
#####총 전력 사용량
pica<-read.csv("pica2017.csv",stringsAsFactors = F,sep=",")
str(pica)
pica<-pica[,-1]
pica$sumelect<-as.numeric(pica$sumelect)


tail(pica)
bbb<-pica %>% group_by(name) %>% summarise(ser = mean(sumelect))
bbb<-as.data.frame(bbb)
str(bbb)  

windows()
ggplot(bbb,aes(x=reorder(name,-ser),y=ser/1000))+
  geom_bar(size=1,stat = "identity",fill='#FFCC33')+
  scale_y_continuous(name = "총 전력 사용량",
                     limits=c(0, 400))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 총 전력 사용량(단위 : GWh)") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20))+
  geom_text(x=1,y=400,label="강남구",size=7,col="#CC0000")+
  geom_text(x=2,y=300,label="서초구",size=7,col="#CC0000")+
  geom_text(x=3,y=240,label="송파구",size=7,col="#CC0000")+
  geom_text(x=4,y=225,label="영등포구",size=7,col="#CC0000")+
  geom_text(x=5,y=220,label="중구",size=7,col="#CC0000")+
  geom_text(x=6,y=200,label="강서구",size=7,col="#CC0000")+
  geom_text(x=7,y=180,label="마포구",size=7,col="#CC0000")+
  geom_hline(yintercept  = mean(bbb$ser)/1000,col="red",cex=0.9)

#가정용 전력
ccc<-pica %>% group_by(name) %>% summarise(ser = mean(fam_elect))
ccc<-as.data.frame(ccc)
str(bbb)  

windows()
ggplot(ccc,aes(x=reorder(name,-ser),y=ser/1000))+
  geom_bar(size=1,stat = "identity",fill='#FFCC33')+
  scale_y_continuous(name = "가정용 전력 사용량",
                     limits=c(0, 75))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 가정용 전력 사용량(단위 : GWh)") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20))+
  geom_text(x=1,y=74,label="강남구",size=7,col="#CC0000")+
  geom_text(x=2,y=73,label="송파구",size=7,col="#CC0000")+
  geom_text(x=3,y=68,label="강서구",size=7,col="#CC0000")+
  geom_text(x=4,y=59,label="노원구",size=7,col="#CC0000")+
  geom_text(x=5.1,y=59,label="서초구",size=7,col="#CC0000")+
  geom_hline(yintercept  = mean(ccc$ser)/1000,col="red",cex=0.9)

zin2017
str(zin2017)
unique(zin2017$sumrain)
rain<-zin2017 %>% group_by(name) %>% summarise(rain = mean(sumrain))
str(rain)
rain<-as.data.frame(rain)
windows()
ggplot(rain,aes(x=reorder(name,-rain),y=rain))+
  geom_bar(size=1,stat = "identity",fill='#FFCC33')+
  scale_y_continuous(name = "강수량",
                     limits=c(0, 1300))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 강수량") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20))+
  geom_text(x=0.95,y=1020,label="양천구",size=8,col="#CC0000")+
  geom_text(x=2.25,y=1020,label="금천구",size=8,col="#CC0000")+
  geom_text(x=3,y=970,label="성동구",size=8,col="#CC0000")+
  geom_text(x=4.2,y=980,label="강동구",size=8,col="#CC0000")+
  geom_text(x=22,y=300,label="서초구",size=7)+
  geom_text(x=23,y=245,label="중구",size=7)+
  geom_text(x=24,y=215,label="영등포구",size=7)+
  geom_text(x=25,y=170,label="송파구",size=7)+
  geom_hline(yintercept  = mean(rain$rain),col="red",cex=0.9)

humi<-zin2017 %>% group_by(name) %>% summarise(humi = mean(meanhumi))
str(humi)
humi<-as.data.frame(humi)
windows()
ggplot(humi,aes(x=reorder(name,-humi),y=humi))+
  geom_bar(size=1,stat = "identity",fill='#FFCC33')+
  scale_y_continuous(name = "습도",
                     limits=c(0, 70))+
  scale_x_discrete(name = "자치구") +
  ggtitle("서울 자치구별 습도") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 30,face="bold"),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20))+
  geom_text(x=0.95,y=1020,label="양천구",size=8,col="#CC0000")+
  geom_text(x=2.25,y=1020,label="금천구",size=8,col="#CC0000")+
  geom_text(x=3,y=970,label="성동구",size=8,col="#CC0000")+
  geom_text(x=4.2,y=980,label="강동구",size=8,col="#CC0000")+
  geom_text(x=22,y=300,label="서초구",size=7)+
  geom_text(x=23,y=245,label="중구",size=7)+
  geom_text(x=24,y=215,label="영등포구",size=7)+
  geom_text(x=25,y=170,label="송파구",size=7)+
  geom_hline(yintercept  = mean(humi$humi),col="red",cex=0.9)

abc<-zin2017[[2]]
str(zin2017[1,2])
zin2017$month
