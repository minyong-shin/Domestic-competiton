hob2 <- c('A1' = "전시회", 'A2' = "박물관", 'A3' = "연주회", 'A4' = "전통예술공연", 'A5' = "연극공연", 'A6' = "무용공연", 'A7' = "영화", 'A8' = "콘서트", 'B1' = "문학행사","B2" = "서예" , 'B3' = "문예창작", 'B4' = "디자인", 'B5' = "연주 노래", 'B6' = "전통예술배우기", 'B7' = "사진촬영", 'B8' = "춤 무용", 'C1' = "스포츠경기장관람", 'C2' = "격투기경기관람",'C3' = "E-SPORTS현장관람", 'D1' = "구기종목" ,'D2' = "수상스포츠", 'D3' = "설상스포츠", 'D4' = "빙상스포츠", "D5" = '헬스 에어로빅', 'D6' = "요가 필라테스", 'D7' = "배드민턴 줄넘기 스트레칭", 'D8' = '육상스포츠', 'D9' = '격투기', 'D10' = "댄스스포츠", 'D11' = '사이클링 자전거','D12' = '인라인스케이트', 'E1' = '유적지관광', 'E2' = "자연풍경관람", 'E3' = "삼림욕", 'E4' = "국내캠핑", 'E5' = '해외여행', 'E6' = '소풍 워크샵', 'E7' = '온천 해수욕', 'E8'= '유람선타기', 'E9' = '테마파크','E10' = '놀이공원 동물원 식물원', 'E11' = '지역축제', 'E12' = '드라이브', 'F1' = '수집활동', 'F2' = '공예', 'F3' = '요리', 'F4' = '반려동물돌보기', 'F5' = '노래방', 'F6' = '인테리어', 'F7' = '등산', 'F8' = '낚시', 'F9' = '블로그관리' , 'F10' = '동영상제작', "F11" = "인터넷서핑",'F12' = '게임', 'F13' = '보드게임', 'F14' = '바둑 장기 체스', 'F15' = '사행성게임 로또', 'F16' = '쇼핑 외식', 'F17' = '음주', 'F18' = '독서 만화책', 'F19' = '미용', 'F20' = '자기계발', 'G1' = '산책', 'G2' = '사우나 찜질방', 'G3' = '낮잠', 'G4' = 'TV시청', 'G5' = '비디오시청', 'G6' = '라디오청취', 'G7' = '음악감상', 'G8' = '잡지보기', 'H1' = '사회봉사활동', 'H2' = '종교활동', 'H3' = '클럽 나이트', 'H4' = '친지방문', 'H5' = '잡담 통화', 'H6' = '동창회 파티', 'H7' = '데이트 미팅 소개팅', 'H8' = '동호회', 'H9' = '기타')
hob <- c('101' = "전시회", '102' = "박물관", '103' = "연주회", '104' = "전통예술공연", '105' = "연극공연", '106' = "무용공연", '107' = "영화", '108' = "콘서트", '201' = "문학행사", '202' = "문예창작", '203' = "디자인", '204' = "연주 노래", '205' = "전통예술배우기", '206' = "사진촬영", '207' = "춤 무용", '301' = "스포츠경기장관람", '302' = "스포츠TV",'303' = "격투기경기관람", '304' = "E-SPORTS현장관람", '407' = "수상스포츠", '408' = "설상스포츠", '409' = "빙상스포츠", "410" = '헬스 에어로빅', '411' = "요가 필라테스", '412' = "배드민턴 줄넘기 스트레칭", '413' = '육상스포츠', '414' = '격투기', '415' = "댄스스포츠", '416' = '사이클링 자전거','417' = '인라인스케이트' '501' = '유적지관광', '502' = '자연풍경관람', '503' = "삼림욕", '504' = "국내캠핑", '505' = '해외여행', '506' = '소풍 워크샵', '507' = '온천 해수욕', '508'= '유람선 타기', '509' = '놀이공원 동물원 식물원', '510' = '지역축제', '511' = '드라이브', '601' = '수집활동', '602' = '공예', '603' = '요리', '604' = '반려동물돌보기', '605' = '노래방', '606' = '인테리어', '607' = '등산', '608' = '낚시', '609' = '블로그관리' , '610' = '인터넷서핑 동영상제작 SNS', '611' = '게임', '612' = '보드게임', '613' = '바둑 장기 체스', '614' = '사행성게임 로또', '615' = '쇼핑 외식', '616' = '음주', '617' = '독서 만화책', '618' = '미용', '619' = '자기계발', '701' = '산책', '702' = '사우나 찜질방', '703' = '낮잠', '704' = 'TV시청', '705' = '비디오시청', '706' = '라디오청취', '707' = '음악감상', '708' = '잡지보기', '801' = '사회봉사활동', '802' = '종교활동', '803' = '클럽 나이트', '804' = '친지방문', '805' = '잡담 통화', '806' = '동창회 파티', '807' = '데이트 미팅 소개팅', '808' = '동호회', '809' = '기타')

#skt 유동인구
skt<-read.csv("FLOW_AGE_WKDY_201510.csv",sep="|")
str(skt)
View(skt)
skt_cd<-unique(skt$BLOCK_CD)
skt_cd

library(data.table)
spot<-fread("spot.csv",stringsAsFactors = F)
str(spot)
spot<-as.data.frame(spot)
spot<-spot[,-1]
spot$block<-as.factor(spot$block)
str(spot)
unique(spot$block)

block1<- unique(spot$block)
block1

my_list <- list()
for(i in 1:length(block1)){
  my_list[[i]] <- assign(paste("bc",i,sep=""), spot[spot$block==block1[i],][1,])
}
library(data.table)
data <- rbind(my_list)

str(data)
data <- data %>% bind_rows

str(data)
View(data)
write.csv(data,"data.csv",row.names=F)
###################################################
#######################################################
###############################################
install.packages('haven', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(haven)

r_10 <- read_sav("2010 국민여가활동조사 원자료.sav", encoding = 'CP949')
str(r_10)
unique(r_10$old)
r_10$nike<-ifelse(r_10$old>=10 & r_10$old<20,10,
                    r_10$old>=20 & r_10$old<30,20)

r_11<- r_10[,c(r_10$q1,r_10$q2_02)]
str(r_10)

library(dplyr)
r_10a<-r_10 %>% select(q1,old,q10_1_01,q10_2_01,q10_3_01,q10_4_01,q10_5_01)
str(r_10a)
r_10a$q10_1_01<-as.character(r_10a$q10_1_01)
r_10a$q10_2_01<-as.character(r_10a$q10_2_01)
r_10a$q10_3_01<-as.character(r_10a$q10_3_01)
r_10a$q10_4_01<-as.character(r_10a$q10_4_01)
r_10a$q10_5_01<-as.character(r_10a$q10_5_01)

hob2 <- c('A1' = "전시회", 'A2' = "박물관", 'A3' = "연주회", 'A4' = "전통예술공연", 'A5' = "연극공연", 'A6' = "무용공연", 'A7' = "영화", 'A8' = "콘서트", 'B1' = "문학행사","B2" = "서예" , 'B3' = "문예창작", 'B4' = "디자인", 'B5' = "연주 노래", 'B6' = "전통예술배우기", 'B7' = "사진촬영", 'B8' = "춤 무용", 'C1' = "스포츠경기장관람", 'C2' = "격투기경기관람",'C3' = "E-SPORTS현장관람", 'D1' = "구기종목" ,'D2' = "수상스포츠", 'D3' = "설상스포츠", 'D4' = "빙상스포츠", "D5" = '헬스 에어로빅', 'D6' = "요가 필라테스", 'D7' = "배드민턴 줄넘기 스트레칭", 'D8' = '육상스포츠', 'D9' = '격투기', 'D10' = "댄스스포츠", 'D11' = '사이클링 자전거','D12' = '인라인스케이트', 'E1' = '유적지관광', 'E2' = "자연풍경관람", 'E3' = "삼림욕", 'E4' = "국내캠핑", 'E5' = '해외여행', 'E6' = '소풍 워크샵', 'E7' = '온천 해수욕', 'E8'= '유람선타기', 'E9' = '테마파크','E10' = '놀이공원 동물원 식물원', 'E11' = '지역축제', 'E12' = '드라이브', 'F1' = '수집활동', 'F2' = '공예', 'F3' = '요리', 'F4' = '반려동물돌보기', 'F5' = '노래방', 'F6' = '인테리어', 'F7' = '등산', 'F8' = '낚시', 'F9' = '블로그관리' , 'F10' = '동영상제작', "F11" = "인터넷서핑",'F12' = '게임', 'F13' = '보드게임', 'F14' = '바둑 장기 체스', 'F15' = '사행성게임 로또', 'F16' = '쇼핑 외식', 'F17' = '음주', 'F18' = '독서 만화책', 'F19' = '미용', 'F20' = '자기계발', 'G1' = '산책', 'G2' = '사우나 찜질방', 'G3' = '낮잠', 'G4' = 'TV시청', 'G5' = '비디오시청', 'G6' = '라디오청취', 'G7' = '음악감상', 'G8' = '잡지보기', 'H1' = '사회봉사활동', 'H2' = '종교활동', 'H3' = '클럽 나이트', 'H4' = '친지방문', 'H5' = '잡담 통화', 'H6' = '동창회 파티', 'H7' = '데이트 미팅 소개팅', 'H8' = '동호회', 'H9' = '기타')
num2 <- c('1' = '남', '2' = '여')

r_10a<-as.data.frame(r_10a)
colnames(r_10a)<-c("GENDER","AGE","A","B","C","D","E")
year<-data.frame(YEAR = rep(2010,3051))

r_10b<-cbind(year,r_10a)

r_10b$GENDER <- num2[r_10b$GENDER]

r_10b$A<-hob2[r_10b$A]
r_10b$B<-hob2[r_10b$B]
r_10b$C<-hob2[r_10b$C]
r_10b$D<-hob2[r_10b$D]
r_10b$E<-hob2[r_10b$E]
str(r_10b)
View(r_10b)
write.csv(r_10b,"2010.csv",row.names=F)

#######################################################################
r_8 <- read_sav("2008 국민여가활동조사 원자료.sav", encoding = 'CP949')

r_7 <- read_sav("2007 국민여가활동조사 원자료.sav", encoding = 'CP949')

r_6 <- read_sav("2006 국민여가활동조사 원자료.sav", encoding = 'CP949')

#####################################################################
####
r_12 <- read_sav("2012 국민여가활동조사_raw data.sav", encoding = 'CP949')

hob <- c('101' = "전시회 관람(미술, 사진, 건축, 디자인 등)", '102' = "박물관 관람", '103' = "음악연주회 관람(클래식, 오페라 등)", '104' = "전통예술공연 관람(국악, 민속놀이 등)", '105' = "연극공연 관람(뮤지컬 포함)", '106' = "무용공연 관람", '107' = "영화보기", '108' = "연예공연 관람(쇼, 콘서트, 마술 쇼 등)", '201' = "문학행사 참여", '202' = "문예창작/독서토론", '203' = "미술활동(그림, 서예, 조각, 디자인 등)", '204' = "악기연주/노래교실", '205' = "전통예술 배우기(사물놀이, 줄타기 등)", '206' = "사진촬영(디지털카메라 등)", '207' = "춤/무용(발레, 한국무용, 현대무용 등)", '301' = "스포츠 경기 직접관람-경기장방문 관람(축구, 야구, 농구, 배구 등)", '302' = "스포츠 경기 간접관람-TV, DMB를 통한 관람(축구, 야구, 농구, 배구 등)",'303' = "격투기 경기관람", '304' = "온라인게임 경기 현장관람(E-SPORTS 경기 포함)", '401' = "농구, 배구, 야구, 축구, 족구",  '402' = "테니스,스쿼시", '403' = "당구/포켓볼",'404' = "볼링, 탁구", '405' = "골프", '406' = "수영", '407' = "윈드서핑, 수상스키, 스킨스쿠버다이빙, 래프팅, 요트", '408' = "스노보드,스키 등", '409' = "아이스스케이트, 아이스하키", "410" = '헬스(보디빌딩)/에어로빅', '411' = "요가/필라테스/태보", '412' = "배드민턴/줄넘기/맨손스트레칭/훌라후프", '413' = '육상/조깅/속보', '414' = '격투기운동(태권도, 유도, 합기도,검도, 권투 등)', '415' = "댄스스포츠", '416' = '사이클링/산악자전거','417' = '인라인스케이트', '418' = '승마, 암벽등반, 철인삼종경기, 서바이벌', '501' = '문화유적방문(고궁, 절, 유적지 등', '502' = '자연명승 및 풍경 관람', '503' = "삼림욕", '504' = "국내캠핑", '505' = '해외여행', '506' = '소풍/야유회', '507' = '온천/해수욕', '508'= '유람선 타기', '509' = '테마파크 가기/놀이공원/동물원/식물원 가기', '510' = '지역축제 참가', '511' = '자동차 드라이브', '601' = '수집활동(스크랩 포함)', '602' = '생활공예(십자수, 비즈공예, D.I.Y 등)', '603' = '요리하기/다도', '604' = '애완동돌 돌보기', '605' = '노래방 가기', '606' = '인테리어(집, 자동차 등)', '607' = '등산', '608' = '낚시', '609' = '미니홈피/블로그 관리' , '610' = '인터넷 검색/채팅/UCC 제작/SNS', '611' = '게임(인터넷, 닌텐도, PSP, PS3 등)', '612' = '보드게임/퍼즐/큐브 맞추기', '613' = '바둑/장기/체스', '614' = '겜블/복권구입', '615' = '쇼핑/외식', '616' = '음주', '617' = '독서/만화책 보기', '618' = '미용(피부 관리, 헤어관리, 네일아트, 마사지, 성형 등)', '619' = '어학/기술/자격증 취득, 공부 등', '701' = '산책', '702' = '목욕/사우나/찜질방', '703' = '낮잠', '704' = 'TV시청(DMB/IPTV 포함)', '705' = '비디오(DVD) 시청', '706' = '라디오 청취', '707' = '음악 감상', '708' = '신문/잡지보기', '801' = '사회봉사활동', '802' = '종교활동', '803' = '클럽/나이트/디스코/카바레 가기', '804' = '가족 및 친지방문', '805' = '잡담/통화하기/문자보내기', '806' = '계모임/동창회/사교(파티)모임', '807' = '이성교제(데이트)/미팅/소개팅', '808' = '친구만남/동호회 모임', '809' = '위서 분류되지 않은 기여 여가활동')

r_12a<-r_12 %>% select(GENDER,AGE,Q5_1,Q5_2,Q5_3,Q5_4,Q5_5)

hob <- c('101' = "전시회", '102' = "박물관", '103' = "연주회", '104' = "전통예술공연", '105' = "연극공연", '106' = "무용공연", '107' = "영화", '108' = "콘서트", '201' = "문학행사", '202' = "문예창작", '203' = "디자인", '204' = "연주 노래", '205' = "전통예술배우기", '206' = "사진촬영", '207' = "춤 무용", '301' = "스포츠경기장관람", '302' = "스포츠TV",'303' = "격투기경기관람", '304' = "E-SPORTS현장관람", '401' = "농구 배구 야구 축구 족구",  '402' = "테니스 스쿼시", '403' = "당구 포켓볼",'404' = "볼링 탁구", '405' = "골프", '406' = "수영", '407' = "수상스포츠", '408' = "설상스포츠", '409' = "빙상스포츠", "410" = '헬스 에어로빅', '411' = "요가 필라테스", '412' = "배드민턴 줄넘기 스트레칭", '413' = '육상스포츠', '414' = '격투기', '415' = "댄스스포츠", '416' = '사이클링 자전거','417' = '인라인스케이트', '418' = '승마 암벽등반 철인삼종경기', '501' = '유적지관광', '502' = '자연풍경관람', '503' = "삼림욕", '504' = "국내캠핑", '505' = '해외여행', '506' = '소풍 워크샵', '507' = '온천 해수욕', '508'= '유람선타기', '509' = '놀이공원 동물원 식물원', '510' = '지역축제', '511' = '드라이브', '601' = '수집활동', '602' = '공예', '603' = '요리', '604' = '반려동물돌보기', '605' = '노래방', '606' = '인테리어', '607' = '등산', '608' = '낚시', '609' = '블로그관리' , '610' = '인터넷서핑 동영상제작 SNS', '611' = '게임', '612' = '보드게임', '613' = '바둑 장기 체스', '614' = '사행성게임 로또', '615' = '쇼핑 외식', '616' = '음주', '617' = '독서 만화책', '618' = '미용', '619' = '자기계발', '701' = '산책', '702' = '사우나 찜질방', '703' = '낮잠', '704' = 'TV시청', '705' = '비디오시청', '706' = '라디오청취', '707' = '음악감상', '708' = '잡지보기', '801' = '사회봉사활동', '802' = '종교활동', '803' = '클럽 나이트', '804' = '친지방문', '805' = '잡담 통화', '806' = '동창회 파티', '807' = '데이트 미팅 소개팅', '808' = '동호회', '809' = '기타')

num <- c('1' = '남', '2' = '여')

r_12a<-as.data.frame(r_12a)
str(r_12a)

r_12a$Q5_1<-as.character(r_12a$Q5_1)
r_12a$Q5_2<-as.character(r_12a$Q5_2)
r_12a$Q5_3<-as.character(r_12a$Q5_3)
r_12a$Q5_4<-as.character(r_12a$Q5_4)
r_12a$Q5_5<-as.character(r_12a$Q5_5)

year2<-data.frame(YEAR = rep(2012,5003))
r_12b<-cbind(year2,r_12a)
colnames(r_12b)<-c("YEAR","GENDER","AGE","A","B","C","D","E")
str(r_12b)

r_12b$GENDER <- num[r_12b$GENDER]

r_12b$A<-hob[r_12b$A]
r_12b$B<-hob[r_12b$B]
r_12b$C<-hob[r_12b$C]
r_12b$D<-hob[r_12b$D]
r_12b$E<-hob[r_12b$E]
View(r_12b)

write.csv(r_12b,"2012.csv",row.names=F)

######
search<-rbind(r_10b,r_12b)
View(search)
sum(is.na(search))

write.csv(search,"search.csv",row.names=F)


####
str(r_12b)
str(r_10b)
a<-as.data.frame(r_10b$A)
colnames(a)<-"a"
a2<-as.data.frame(r_10b$B)
colnames(a2)<-"a"
a3<-as.data.frame(r_10b$C)
colnames(a3)<-"a"
a4<-as.data.frame(r_10b$D)
colnames(a4)<-"a"
a5<-as.data.frame(r_10b$E)
colnames(a5)<-"a"

aa<-rbind(a,a2,a3,a4,a5)
count(unique(aa))

######
b<-as.data.frame(r_12b$A)
colnames(b)<-"a"
b2<-as.data.frame(r_12b$B)
colnames(b2)<-"a"
b3<-as.data.frame(r_12b$C)
colnames(b3)<-"a"
b4<-as.data.frame(r_12b$D)
colnames(b4)<-"a"
b5<-as.data.frame(r_12b$E)
colnames(b5)<-"a"

bb<-rbind(b,b2,b3,b4,b5,a,a2,a3,a4,a5)
count(unique(bb))

##################################################
fi<-read.csv("yaman.csv",header = T,stringsAsFactors = F)
str(fi)
sum(is.na(fi))
str(fi)
search<-read.csv("search.csv",header = T,stringsAsFactors = F)
str(search)
final<-rbind(search,fi)
str(final)
library(dplyr)
k1 <- data_frame(a = final$A)
k2 <- data_frame(a = final$B)
k3 <- data_frame(a = final$C)
k4 <- data_frame(a = final$D)
k5 <- data_frame(a = final$E)
aa <- list(k1, k2, k3, k4, k5) %>% bind_rows()
count(unique(as.data.frame(aa)))
str(aa)
aa<-as.data.frame(aa)
aa2<-aa %>% group_by(a) %>% summarise(count=n())
aa2<-as.data.frame(aa2)
write.csv(unique(as.data.frame(aa2)),"giveyou.csv",row.names=F)

write.csv(unique(as.data.frame(aa)),"unique.csv",row.names=F)

write.csv(final,"final.csv",row.names=F)

################
##전체 취미별 빈도수
str(aa2)
library(ggplot2)
library(RColorBrewer)
windows()
aa2<-aa2[-93,]
ggplot(aa2,aes(x=reorder(a,-count),y=count))+
  geom_bar(size=1,stat = "identity",fill="#000666")+
  scale_y_continuous(name = "빈도수",
                     limits=c(0, 18000))+
  scale_x_discrete(name = "여가") +
  ggtitle("여가별 빈도수") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))

##장소별 추천이 필요 없는 취미별 빈도수
uni<-read.csv("unique.csv",stringsAsFactors = F,header=T)
str(uni)
uni2<-uni[uni$need==0,]
View(uni2)
uni3<-as.data.frame(unique(uni2$leisure))
str(uni3)
colnames(uni3)<-"leisure"
uni3$leisure<-as.character(uni3$leisure)
uni4<-uni3 %>% left_join(aa2,by=c("leisure"="a"))
uni4<-uni4[-50,]

uni4$Base<-"권장"
uni4$Base[1]<-"지양"
uni4$Base[14]<-"지양"
uni4$Base[22]<-"지양"
uni4$Base[27]<-"지양"
uni4$Base[36]<-"지양"
uni4$Base[45]<-"지양"
uni4$Base[51]<-"지양"
str(uni4)

uni4$Base<-factor(uni4$Base,levels=c("지양","권장"))
ggplot(uni4,aes(x=reorder(leisure,-count),y=count,fill=Base))+
  geom_bar(size=1,stat = "identity")+
  scale_y_continuous(name = "빈도수",
                     limits=c(0, 18000))+
  scale_x_discrete(name = "여가") +
  ggtitle("장소 추천이 필요없는 여가별 빈도수") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(size=30,face="bold"),
        axis.text.x=element_text(size = 16,face="bold"),
        legend.title=element_text(size=22,face="bold"), 
        legend.text=element_text(size=22,face="bold"))

#장소별 추천이 필요 있는 취미별 빈도수
uni22<-uni[uni$need==1,]
View(uni22)
uni33<-as.data.frame(unique(uni22$leisure))
str(uni33)
colnames(uni33)<-"leisure"
uni33$leisure<-as.character(uni33$leisure)
uni44<-uni33 %>% left_join(aa2,by=c("leisure"="a"))
str(uni44)
View(uni44)

ggplot(uni44,aes(x=reorder(leisure,-count),y=count))+
  geom_bar(size=1,stat = "identity",fill="#006633")+
  scale_y_continuous(name = "빈도 수",
                     limits=c(0, 6600))+
  scale_x_discrete(name = "여가") +
  ggtitle("여가별 수요") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma",face="bold"),
        axis.title = element_text(size=30,face="bold"),
        axis.text.x=element_text(size = 16,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))


####장소가 필요한 취미별 관광지의 합
index<- read.csv("index.csv",stringsAsFactors = F, header=T)
str(index)
sum(is.na(index))
index<-index[-38,]
ggplot(index,aes(x=reorder(leisure,-supply),y=supply))+
  geom_bar(size=1,stat = "identity",fill='#0066CC')+
  scale_y_continuous(name = "관광지 합",
                     limits=c(0, 3200))+
  scale_x_discrete(name = "여가") +
  ggtitle("여가별 관광지의 합") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 25, family = "Tahoma",face="bold"),
        axis.title = element_text(size=30,face="bold"),
        axis.text.x=element_text(size = 18,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))

View(index)

####관광지의 합과 취미의 빈도수를 나눈 값을 분해
index1<-index[index$z > 1,]
str(index1)
index1$supply<-log(index1$supply)
index1$demand<-log(index1$demand)
index1$z<-index1$demand/index1$supply
index1$col<-ifelse(((index1$supply+1.9) < index1$demand),'수요가 많음',
                   ifelse(((index1$supply-1.9) > index1$demand),'공급이 많음','적절함'))
index1$col<-factor(index1$col,levels=c("취미 대비 관광지 수가 많음","적절함","관광지 대비 취미 빈도가 높음"))

index2<-index[index$z < 1,]
str(index2)
index2$supply<-log(index2$supply)
index2$demand<-log(index2$demand)
index2$z<-index2$demand/index2$supply
index2$col<-ifelse(((index2$supply+2.1) < index2$demand),'수요가 많음',
                   ifelse(((index2$supply-2.1) > index2$demand),'공급이 많음','적절함'))




####
#######
######수요폭발, 공급폭발 관광지별 취미
in1<-ggplot(index1,aes(x=reorder(leisure,-z),y=z,fill=col))+
  geom_bar(size=1,stat = "identity")+
  scale_y_continuous(name = "Z",
                     limits=c(0, 22))+
  scale_x_discrete(name = "",position="top") +
  coord_flip()+
  scale_y_reverse()+
  ggtitle("공급 > 수요 여가")+
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))

windows()
in1
index2<-index2[-22,]#박물과INF여서 박물관은 뺌
index2<-index2[-16,]

in2<-ggplot(index2,aes(x=reorder(leisure,-z),y=z,fill=col))+
  geom_bar(size=1,stat = "identity")+
  scale_y_continuous(name = "Z",
                     limits=c(0,3.8))+
  scale_x_discrete(name = "") +
  coord_flip()+
  ggtitle("공급 < 수요 여가") +
  scale_fill_manual(breaks = c("수요가 많음", "적절함"), 
                    values=c("#669900", "#993300"))+
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))
in2
windows()
library(ggpubr)
ggarrange(in1,in2, 
          ncol = 2, nrow = 1)
#############################


####소분류별 관광지 빈도 -> 필요 x 
so<-read.csv("sobun.csv",stringsAsFactors = F,header=T)
str(so)
so<-so[-38,]
windows()
ggplot(so,aes(x=reorder(uni2,-n),y=n))+
  geom_bar(size=1,stat = "identity",fill='#FF8C00')+
  scale_y_continuous(name = "관광지 합",
                     limits=c(0,2200))+
  scale_x_discrete(name = "관광지 소분류") +
  ggtitle("관광지 소분류별 관광지의 합") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 25, family = "Tahoma",face="bold"),
        axis.title = element_text(size=30,face="bold"),
        axis.text.x=element_text(size = 18,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))



ggarrange(ni,ni2, 
          ncol = 2, nrow = 1)


#######################################################이것도 뺌############################################3
str(index)
left<-index %>% left_join(kk,by=c("leisure"="leisure"))
left$supply<-log(left$supply)
left$demand<-log(left$demand)
left$z<-left$demand/left$supply
str(left)
left2<-left[left$z >= 1,]
left3<-left[left$z < 1,]


nn1<-ggplot(left2,aes(x=reorder(leisure,-z),y=z,fill=col))+
  geom_bar(size=1,stat = "identity")+
  scale_y_continuous(name = "Z",
                     limits=c(0, 22))+
  scale_x_discrete(name = "",position="top") +
  coord_flip()+
  scale_y_reverse()+
  ggtitle("공급 > 수요 여가") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))
windows()
nn1
index2<-index2[-22,]
in2<-ggplot(index2,aes(x=reorder(leisure,-z),y=z,fill=col))+
  geom_bar(size=1,stat = "identity")+
  scale_y_continuous(name = "Z",
                     limits=c(0,3.8))+
  scale_x_discrete(name = "") +
  coord_flip()+
  ggtitle("공급 < 수요 여가") +
  theme(axis.text.x=element_text( angle=90))+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))
in2
##############################################################################################################3
#kk<-indexx[,c(1,5)]
#head(kk)
#index
indexx<-index
str(indexx)

indexx$supply<-log(indexx$supply)
indexx$demand<-log(indexx$demand)
indexx$col<-ifelse(((indexx$supply+2.1) < indexx$demand),'수요가 많음',
                   ifelse(((indexx$supply-1.9) > indexx$demand),'공급이 많음','적절함'))
indexx<-indexx[-38,]
indexx<-indexx[-32,]
windows()
#텍스트 시각화 수요 공급 별
ggplot(indexx, aes(x=supply,y=demand))+
  geom_text(aes(label = leisure,color=col,group=col,fontface="bold"), size = 5)+
  scale_y_continuous(name = "demand",
                     limits=c(0, 10))+
  ggtitle("공급과 수요별 여가") +
  geom_abline(intercept= 2.1, slope=1, color='#333333', size = 0.8)+
  geom_abline(intercept= -1.9, slope=1, color='#333333', size = 0.8)+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15,face="bold"),
        legend.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20,face="bold"))




######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
#skt 모든 데이터 합친 것
skt<-read.csv("skt.csv",stringsAsFactors = F,header=T)
str(skt)
#기존에 년 월 확인해보려고 뽑았던 것임 x,y코드 별로 뽑는 것이므로 필요x
#skt$month<-substr(skt$STD_YM,5,6)
#skt$year <- substr(skt$STD_YM,1,4)
library(dplyr)
mean(skt$WEEKDAY)

##x코드 y코드별로 평일의 유동인구수를 평균 낸 것
skt2<-skt %>% group_by(X_COORD,Y_COORD) %>% summarise(mean = mean(WEEKDAY))
str(skt2)
skt2<-as.data.frame(skt2)
#mean확인차 해봄
#mean(skt2$mean)


#utmk좌표계로 되어 있는 x,y코드를 위도 경도로 바꿔주는 함수
install.packages("sp")
library(sp)
install.packages("rgdal")
library(rgdal)
#함수
convertCoordSystem <- function(lat, long, from.crs, to.crs){
  xy <- data.frame(lat=lat, long=long)
  coordinates(xy) <- ~lat+long
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("long", "lat")
  
  return(changed)
}

from.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
to.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#위도 경도 완성!!
coord <- convertCoordSystem(skt2$X_COORD, skt2$Y_COORD, from.crs, to.crs)
head(coord)

##위도 경도 바꾼 데이터 프레임을 위에서 만든 skt2와 cbind
yolo<-cbind(skt2,coord)


#나심심씨의 위치와 우리가 구한 위도 경도 사이의 거리 구하기
map_dist_list <- list()
install.packages("Imap")
library(Imap)

#거리 구하는 함수 -> 종로구 적선재에 위치한 나심심씨
for (i in 1:nrow(yolo)) {
  
  map_dist_list[[i]] <- gdist(lon.1 = yolo$lon[i], 
                              lat.1 = yolo$lat[i], 
                              lon.2 = 126.987454,#나심심씨의 경도 
                              lat.2 = 37.582194,#나심심씨의 위도 
                              units="km")
}
#리스트 형식이므로 unlist
map_dist_mat <- sapply(map_dist_list, unlist)
str(map_dist_mat)
bb<-as.data.frame(map_dist_mat)

#위의 yolo 와 cbind
yolo2<-cbind(yolo,bb)
str(yolo2)

#나심심씨의 위치에서 1km 반경에 있는 것들만 뽑음 
#era 반경을 다르게 뽑으려면저 숫자만 바꿔주면 됨
era<-tbl_df(yolo2[yolo2$map_dist_mat<1,])
library(RColorBrewer)
library(ggmap)


#################3
#나심심씨의 위치를 대충 유동인구수는 4000으로 넣고 한 행을 만들어서 rbind함
era2<-rbind(era,data.frame(X_COORD=NA,Y_COORD=NA,mean=4000,long=126.987454,lat=37.582194,map_dist_mat=NA))
#나심심씨의 위치와 다른 곳의 색깔을 다르게 하기 위해서 뽑음
era2$col<-0
era2[2134,7]<-1
era2$col<-as.character(era2$col)
str(era2)

##나심심씨의 위치를 중심으로 지도를 뽑음
g_m<-get_map(location = c(126.987454,37.582194),zoom=15,maptype="roadmap")
windows()
#지도 시각화 
gang.map<-ggmap(g_m)+
  geom_point(data=era2,aes(x=long,y=lat,size=mean,col=col))
gang.map

#################상위2개 하위2개 뽑은 것
era2 %>% arrange(mean) %>% slice(1:5)
era2 %>% arrange(desc(mean)) %>% slice(1:5)

era_a<-rbind(era2 %>% arrange(mean) %>% slice(1:5),era2 %>% arrange(desc(mean)) %>% slice(1:5))
era_a<-era_a[,-7]
era_a$X_COORD<-as.character(era_a$X_COORD)

era_a<-rbind(era_a,data.frame(X_COORD="내위치",Y_COORD=NA,mean=4000,long=126.987454,lat=37.582194,map_dist_mat=NA))
era_a$col<-"유동인구"
era_a[11,7]<-"내위치"
str(era_a)

#방식은 위와 똑같음 숫자만 바꿔가면서 하면 돼
g_m<-get_map(location = c(126.987454,37.582194),zoom=15,maptype="roadmap")
windows()
gang.map<-ggmap(g_m)+
  geom_point(data=era_a,aes(x=long,y=lat,size=mean,col=col))+
  geom_text(data=era_a,aes(x=long,y=lat+0.0006,label=X_COORD),size=2.5)
gang.map

write.csv(yolo2,"dist.csv",row.names = F)




