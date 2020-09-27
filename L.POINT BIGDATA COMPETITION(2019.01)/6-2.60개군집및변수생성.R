install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(lubridate)
install.packages("xlsx")
library(xlsx)
install.packages("doBy")
library(doBy)
str(cuso3)
clu3<-data.frame(fread("cluster60.csv",encoding="UTF-8"))#아마 2가 진짜-->유클리리안으로구한 클러스터
clu_d3<-data.frame(fread("cluster_distance60.csv"))#아마 2가 진짜 --> 유클리디안으로 구한 군집간 거리 벡터
View(clu3)
View(clu_d3)
clu3<-clu3[-1,]
clu_d3<-clu_d3[-1,]

rownames(clu_d3)<-NULL
rownames(clu3)<-NULL

clu<-clu3
View(clu)

mas$clu<-ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[1,2]," ")),1,
                ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[2,2]," ")),2,
                       ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[3,2]," ")),3,
                              ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[4,2]," ")),4,
                                     ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[5,2]," ")),5,
                                            ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[6,2]," ")),6,
                                                   ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[7,2]," ")),7,
                                                          ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[8,2]," ")),8,
                                                                 ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[9,2]," ")),9,
                                                                        ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[10,2]," ")),10,
                                                                               ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[11,2]," ")),11,
                                                                                      ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[12,2]," ")),12,
                                                                                             ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[13,2]," ")),13,
                                                                                                    ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[14,2]," ")),14,
                                                                                                           ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[15,2]," ")),15,
                                                                                                                  ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[16,2]," ")),16,
                                                                                                                         ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[17,2]," ")),17,
                                                                                                                                ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[18,2]," ")),18,
                                                                                                                                       ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[19,2]," ")),19,
                                                                                                                                              ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[20,2]," ")),20,
                                                                                                                                                     ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[21,2]," ")),21,
                                                                                                                                                            ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[22,2]," ")),22,
                                                                                                                                                                   ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[23,2]," ")),23,
                                                                                                                                                                          ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[24,2]," ")),24,
                                                                                                                                                                                 ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[25,2]," ")),25,
                                                                                                                                                                                        ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[26,2]," ")),26,
                                                                                                                                                                                               ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[27,2]," ")),27,
                                                                                                                                                                                                      ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[28,2]," ")),28,
                                                                                                                                                                                                             ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[29,2]," ")),29,
                                                                                                                                                                                                                    ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[30,2]," ")),30,
                                                                                                                                                                                                                           ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[31,2]," ")),31,
                                                                                                                                                                                                                                  ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[32,2]," ")),32,
                                                                                                                                                                                                                                         ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[33,2]," ")),33,
                                                                                                                                                                                                                                                ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[34,2]," ")),34,
                                                                                                                                                                                                                                                       ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[35,2]," ")),35,
                                                                                                                                                                                                                                                              ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[36,2]," ")),36,
                                                                                                                                                                                                                                                                     ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[37,2]," ")),37,
                                                                                                                                                                                                                                                                            ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[38,2]," ")),38,
                                                                                                                                                                                                                                                                                   ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[39,2]," ")),39,
                                                                                                                                                                                                                                                                                          ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[40,2]," ")),40,
                                                                                                                                                                                                                                                                                                 ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[41,2]," ")),41,
                                                                                                                                                                                                                                                                                                        ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[42,2]," ")),42,
                                                                                                                                                                                                                                                                                                               ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[43,2]," ")),43,
                                                                                                                                                                                                                                                                                                                      ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[44,2]," ")),44,
                                                                                                                                                                                                                                                                                                                             ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[45,2]," ")),45,
                                                                                                                                                                                                                                                                                                                                    ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[46,2]," ")),46,
                                                                                                                                                                                                                                                                                                                                           ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[47,2]," ")),47,59)))))))))))))))))))))))))))))))))))))))))))))))



mas$clu<-ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[48,2]," ")),48,
                ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[49,2]," ")),49,
                       ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[50,2]," ")),50,
                              ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[51,2]," ")),51,
                                     ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[52,2]," ")),52,
                                            ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[53,2]," ")),53,
                                                   ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[54,2]," ")),54,
                                                          ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[55,2]," ")),55,
                                                                 ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[56,2]," ")),56,
                                                                        ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[57,2]," ")),57,
                                                                               ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[58,2]," ")),58,
                                                                                      ifelse(mas$CLAC3_NM %in% unlist(strsplit(clu[60,2]," ")),60,mas$clu))))))))))))
length(unique(mas$clu))
#군집제대로 묶였는지
clu50<-subset(mas,clu==59)
str(clu50)

#클러스터 시각화전
order(clu_d3[,27],decreasing = F)
order(clu_d3[,14],decreasing = F)

#클러스터 시각화1 --> 각 군집에서 거리가 가장 가까운 군집과 먼 군집을 시각화(2,50)
clu_d2<-clu_d3
str(cuso)

dok<-data.frame(군집=numeric(0) ,교집합수=numeric(0), 군집간거리=character(0))

for(i in 1:60){
  dong2_a= data.frame(군집 = c(i,i),교집합수=c(length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[2]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[2],2]),
                                         length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[55]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[55],2])),
                        군집간거리=c("가장 가까운 군집","가장 먼 군집"))
  dok = bind_rows(dok,dong2_a)
}

#클러스터 시각화2 --> 각 군집에서 거리가 가까운 것부터 먼 순서대로 시각화한 것(2,10,20,30,40번째로 먼 군집)
dok_2<-data.frame(군집=numeric(0) ,교집합수=numeric(0), 군집간거리=character(0))

for(i in 1:60){
  dong2_a= data.frame(군집 = c(i,i,i,i,i),교집합수=c(length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[2]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[2],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[10]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[10],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[20]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[20],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[30]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[30],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[50]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[50],2])),
                        군집간거리=c("1군집","2군집","3군집","4군집","5군집"))
  dok_2 = bind_rows(dok_2,dong2_a)
}

str(dok)
windows()
View(clu)
ggplot(dok,aes(군집,교집합수,group=군집간거리,col=군집간거리))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1))

ggplot(dok_2,aes(군집,교집합수,group=군집간거리,col=군집간거리))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1))

#################군집별 거리에 따른 구매량 변화 시각화
dok2<-data.frame(군집=numeric(0) ,교집합수=numeric(0), 군집간거리=numeric(0))

for(i in 1:60){
  dong3_a= data.frame(군집 = c(rep(i,59)),교집합수=c(length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[2]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[2],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[3]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[3],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[4]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[4],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[5]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[5],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[6]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[6],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[7]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[7],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[8]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[8],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[9]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[9],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[10]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[10],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[11]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[11],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[12]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[12],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[13]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[13],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[14]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[14],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[15]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[15],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[16]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[16],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[17]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[17],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[18]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[18],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[19]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[19],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[20]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[20],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[21]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[21],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[22]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[22],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[23]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[23],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[24]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[24],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[25]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[25],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[26]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[26],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[27]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[27],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[28]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[28],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[29]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[29],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[30]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[30],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[31]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[31],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[32]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[32],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[33]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[33],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[34]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[34],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[35]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[35],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[36]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[36],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[37]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[37],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[38]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[38],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[39]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[39],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[40]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[40],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[41]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[41],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[42]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[42],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[43]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[43],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[44]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[44],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[45]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[45],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[46]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[46],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[47]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[47],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[48]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[48],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[49]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[49],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[50]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[50],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[51]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[51],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[52]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[52],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[53]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[53],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[54]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[54],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[55]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[55],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[56]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[56],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[57]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[57],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[58]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[58],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[59]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[59],2]),
                                               length(unique(inner_join(subset(cuso,clu==order(clu_d2[,i],decreasing = F)[1]),subset(cuso,clu==order(clu_d2[,i],decreasing = F)[60]),by="CLNT_ID")$CLNT_ID))/length(clu[order(clu_d2[,i],decreasing = F)[60],2])),
                        군집간거리=c(seq(1,59,1)))
  dok2 = bind_rows(dok2,dong3_a)
}

View(dok2)
str(dok2_a)
write.csv(dok2,"dok2_a.csv")
View(dok2_a)
dok2_a<-data.frame(fread("dok2_a.csv"))


dok4<-data.frame(군집거리=numeric(0) ,거리별고객수=numeric(0))

for (i in 1:59){
  aa=data.frame(군집거리=i,거리별고객수=mean(subset(dok2_a,군집간거리==i)$교집합수))
  dok4=bind_rows(dok4,aa)
}
str(dok4)
windows()
ggplot(dok4,aes(군집거리,거리별고객수,col="red"))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=90, vjust=1),
        legend.position = "none")+
  scale_x_continuous(breaks=seq(1, 59, 1))
####################################
#클러스터 텍스트 시각화#############
####################################
clu_eda<-data.frame(fread("visual_cluster.csv",encoding = "UTF-8"))
str(clu_eda)

clu_eda$predict<-as.numeric(clu_eda$predict)
clu_eda$predict<-as.factor(clu_eda$predict)

clu_eda2<-subset(clu_eda,predict>=51 & predict<=60)
clu_eda2$predict<-as.factor(clu_eda2$predict)

ggplot(clu_eda2, aes(x=x,y=y))+
  geom_text(aes(label = words,color=predict,group=predict,fontface="bold"), size = 5)+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15,face="bold"),
        legend.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20,face="bold"),
        legend.position = "none")


###############################################################
############################변수생성##########################
################################################################
#고객별 평균 구매주기
library(lubridate)
install.packages("xlsx")
library(xlsx)
install.packages("doBy")
library(doBy)
str(cuso3)

#고객id별 날짜별 정렬
cuso3_a<-cuso3[order(cuso3$CLNT_ID, cuso3$SESS_DT),]
head(cuso3_a)
View(cuso3_a)

cuso3_a$trans_date_lag<-shift(cuso3_a$SESS_DT, n=1, fill=NA, type = "lag")
cuso3_a$trans_date_diff<-cuso3_a$SESS_DT - cuso3_a$trans_date_lag
cuso3_a$trans_date_final<-ifelse(cuso3_a$CLNT_ID == shift(cuso3_a$CLNT_ID, n=1, fill = NA, type = "lag"),
                                 cuso3_a$SESS_DT-shift(cuso3_a$SESS_DT, n=1, fill=NA, type = "lag"),
                                 NA)
head(cuso3_a)
str(cuso3_a)

#groupby를 사용해도 ㄷ
Cust_Pattern <- summaryBy(trans_date_final~CLNT_ID, data = cuso3_a, FUN= mean, na.rm =TRUE)
head(Cust_Pattern)
length(Cust_Pattern$CLNT_ID)

#군집별 구매량
library(reshape2)
str(cuso3_a)됨
#dcast를 사용하여 python에서 pivot table과 같은 효과를 낸다.
clugun <- dcast(cuso3_a, CLNT_ID ~ clu, value.var="PD_BUY_CT", fun.aggregate=sum)
str(clugun)

cstr_aa3<-cuso3 %>% group_by(clu) %>% summarise(구매량 = sum(PD_BUY_CT))
ggplot(cstr_aa3,aes(clu,구매량,col="red"))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1),name="군집")+
  ggtitle("군집별 구매량 추세")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.position = "none",
        strip.text.x = element_text(size = 18, face="bold"))


#고객별 경제력1
money <- cuso3 %>% group_by(CLNT_ID) %>% summarise(total = sum(PD_BUY_CT*PD_BUY_AM),total2 = sum(PD_BUY_AM))
str(money)


#지역
land <- cuso3 %>% select(CLNT_ID,ZON_NM,CITY_NM) %>% group_by(CLNT_ID,ZON_NM,CITY_NM) %>% unique() 
head(land)
length(unique(land$CLNT_ID))
str(land)

land2<-land[!duplicated(land$CLNT_ID),]
str(land2)
head(land2)


#데이터 통합1
vv<-merge(money,Cust_Pattern,by="CLNT_ID",all=F)
str(vv2)
vv2<-merge(vv,land2,by="CLNT_ID",all.x=T)
vv3<-merge(vv2,clugun,by="CLNT_ID",all=F)
str(vv3)

write.csv(vv3,"민용변수.csv",row.names=F)

###############################################################
############################변수생성2##########################
################################################################

#summary
cuso3$totalcost<-(cuso3$PD_BUY_AM*cuso3$PD_BUY_CT)

rfm1<-summaryBy(totalcost~CLNT_ID, data = cuso3, 
                FUN = c(sum, mean, median, length))


#RFM
#고객별로 max_date뽑기
mdate <- cuso3 %>% group_by(CLNT_ID) %>% summarise(mdate=max(SESS_DT)) 
head(mdate)
str(mdate)

#최근구매경과일수 recency구하기
mdate$rec<-as.Date("2018-10-01",format="%Y-%m-%d")-mdate$mdate
str(cuso3)
str(mdate)
#고객별 구매건수 --> frequency
sell2<- cuso3 %>% group_by(CLNT_ID) %>% summarise(fre=sum(PD_BUY_CT))
str(sell2)

#고객별 총구매액 --> money의 total을 쓰면 됨
str(money)

#RFM-score
rfm2<-merge(mdate[,c(1,3)],sell2,by="CLNT_ID",all=F)
rfm3<-merge(rfm2,money[,c(1,2)],by="CLNT_ID",all=F)
rfm3<-as.data.frame(rfm3)
str(rfm3)

sum(is.na(rfm3))
rfm3$rec<-as.numeric(rfm3$rec)

rfm3$rfm_score<- (1-(rfm3$rec-min(rfm3$rec))/(max(rfm3$rec)-min(rfm3$rec)))+
                        (rfm3$fre-min(rfm3$fre))/(max(rfm3$fre)-min(rfm3$fre))+
                        (rfm3$total-min(rfm3$total))/(max(rfm3$total)-min(rfm3$total))

#데이터 통합
str(rfm1)
rfm_f<-merge(rfm3,rfm1[,c(1,3,4,5)],by="CLNT_ID",all=F)
str(rfm_f)

write.csv(rfm_f,"민용변수22.csv",row.names=F)


###########################################################
#########################변동계수 모음######################
############################################################
######################################################
#고객별 군집별로 구매량에 따른 변동계수
#total,totalcost.mean,totalcost.median에대한 변동계수
str(clugun)

cv = function(x){
  sd(x)/(sum(x)/60)
}
#행단위 계사
a = apply(clugun[,-1],1,cv)
length(a)
str(a)

rfm_cv<-rfm_f[,c(1,4,6,7)]
str(rfm_cv)
str(clugun)

cv2 = function(x){
  sd(x)/(sum(x)/3)
}
b = apply(rfm_cv[,-1],1,cv2)
length(b)


str(clugun)
clugun$clstcv<-a
clugun$clstcv2<-b
ze<-clugun[,c(1,62,63)]
str(ze)

write.csv(ze,"민용변수33.csv",row.names=F)


###############################################################
############################변수생성3##########################
################################################################
#군집별 검색량
str(cuso3)
length(unique(cuso3$CLNT_ID))
length(unique(ser1$CLNT_ID))

str(ser1)
unique(ser1$KWD_NM)
ser11<-gsub(" ","",ser1$KWD_NM)
str(ser11)

str(ser1$KWD_NM)


ctgr_ser<-subset(cuso3,PD_BRA_NM %in% ser1$KWD_NM)

str(ctgr_ser)
length(unique(ctgr_ser$CLNT_ID))
ctgr_ser$plus<-1

str(vv)
cuso3$plus<-1
str(cuso3)
clugun3 <- dcast(ctgr_ser, CLNT_ID ~ clu, value.var="plus", fun.aggregate=sum)
str(clugun3)
str(vv[,1])
clugun3_ser<-merge(vv[,c(1,2)],clugun3,by="CLNT_ID",all.x=T)
str(clugun3_ser)
sum(is.na(clugun3_ser))#52번째군집이 없음
clugun3_ser[is.na(clugun3_ser)]<-0
clugun3_ser<-clugun3_ser[,-2]

cstr_aa2<-ctgr_ser %>% group_by(clu) %>% summarise(검색량 = sum(plus))

ggplot(cstr_aa2,aes(clu,검색량,col="red"))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1),name="군집")+
  ggtitle("군집별 검색량 추세")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.position = "none",
        strip.text.x = element_text(size = 18, face="bold"))


#변동계수
ser_cv<-function(x){
  sd(x)/(sum(x)/59)
}
ser_cv_f<-apply(clugun3_ser[,-1],1,ser_cv)

#군집별 구매액
str(cuso3)
clugun2 <- dcast(cuso3, CLNT_ID ~ clu, value.var="totalcost", fun.aggregate=sum)
Car_table_4 <- xtabs(~ Type + Cylinders, data=Cars93) #table과 형식이 비슷하게 만들어준다.
str(clugun2)
str(cuso3)
cstr_aa<-cuso3 %>% group_by(clu) %>% summarise(구매총액 = sum(totalcost))
str(cstr_aa)

ggplot(cstr_aa,aes(clu,구매총액,col="red"))+
  geom_line()+
  geom_point()+
  theme(axis.text.x = element_text(angle=90, vjust=1))+
  scale_x_continuous(breaks=seq(1, 60, 1),name="군집")+
  ggtitle("군집별 구매총액 추세")+
  theme(plot.title = element_text(size = 20,hjust=0.5, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.position = "none",
        strip.text.x = element_text(size = 18, face="bold"))

#변동계수
ser_cv2<-function(x){
  sd(x)/(sum(x)/60)
}

ser_cv_f2<-apply(clugun2[,-1],1,ser_cv2)
str(ser_cv_f2)
sum(is.na(ser_cv_f3))

clugun_serr<-merge(clugun3_ser[,-2],clugun2,by="CLNT_ID",all=F)
str(clugun_serr)



write.csv(clugun_serr,"민용변수4.csv",row.names=F)

cv_aa<-data.frame(CLNT_ID=clugun3_ser$CLNT_ID,검색량cv=ser_cv_f,구매총액cv=ser_cv_f2,트렌드cv=ser_cv_f3)
str(cv_aa)

sum(is.na(cv_aa))

write.csv(cv_aa,"민용변수6.csv",row.names=F)













