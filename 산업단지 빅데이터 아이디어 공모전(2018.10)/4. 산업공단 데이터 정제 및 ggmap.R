if(!require(data.table)) install.packages("data.table"); library(data.table)
fac = fread('factory.csv')
fac = fac[,-1]
str(fac)

names(fac)[18:23] = c("소음진동여부","생활용수","산업용수",
                      "자기자본액","타인자본액","회사번호")

fac = data.frame(fac)
head(fac)

fac$회사명 = as.character(fac$회사명)
fac$주소 = as.character(fac$주소)
fac$행정기관 = as.factor(fac$행정기관)
fac$단지명 = as.factor(fac$단지명)
fac$설립구분 = as.factor(fac$설립구분)
fac$용지면적 = as.numeric(fac$용지면적)
fac$건축면적 = as.numeric(fac$건축면적)
fac$종업원수 = as.integer(fac$종업원수)
fac$공장규모 = as.factor(fac$공장규모)
fac$공장등록일 = as.Date(fac$공장등록일)
fac$용도지역 = as.factor(fac$용도지역)
fac$지목 = as.factor(fac$지목)
fac$업종 = as.character(fac$업종)
fac$업종코드 = as.character(fac$업종코드)
fac$생산품 = as.character(fac$생산품)
fac$대기오염 = as.factor(fac$대기오염)
fac$수질오염 = as.factor(fac$수질오염)
fac$소음진동여부 = as.factor(fac$소음진동여부)
fac$생활용수 = as.numeric(fac$생활용수)
fac$산업용수 = as.numeric(fac$산업용수)
fac$자기자본액 = as.numeric(fac$자기자본액)
fac$타인자본액 = as.numeric(fac$타인자본액)
fac$회사번호 = as.character(fac$회사번호)
fac$홈페이지 = as.character(fac$회사번호)

unique(fac$업종)
a = c()
for(i in 1:ncol(fac)){
  a = c(a,(mean(is.na(fac[,i]))))
}
na_list = data.frame(cbind(names(fac),a))
na_list$V1 = as.character(na_list$V1)
na_list$a = as.numeric(as.character(na_list$a))

str(na_list)

fac = fac[,-c(19:22)]
str(fac)
list = data.frame(table(fac$단지명))

#입주업체 수대로 sample[1-10]까지 생성.
for(i in 1:10){
  assign(paste0("sample",i),fac[fac$단지명 == c(as.character(head(list[order(list$Freq,decreasing = TRUE),],10)$Var1))[i],])
}


sample1$업종코드 = strsplit(sample1$업종코드,",")


# 업종코드를 일렬로 핀다.
# 4,3,2 자리까지 끊어서 붙여준다.

s = sample1$업종코드
n = sample1$회사명

name = c()
for(i in 1:length(s)){
  for(j in length(s[[i]])){
    name = c(name,rep(n[i],j))
  }
}

s2 = unlist(s)
s3 = substr(s2,1,4)
s4 = substr(s2,1,3)
s5 = substr(s2,1,2)

n2 = as.data.frame(cbind(name,s2,s3,s4,s5))
names(n2) = c("name","5c","4c","3c","2c")
head(n2,20)



##########################################ggmap
install.packages("ggmap")
library(ggmap)
data<-read.csv("address.csv",header=T)
data$address<-as.character(data$address)
data$address<-enc2utf8(data$address)
str(data)
data2<-data[1:2500,]
data2<-as.data.frame(data2)
str(data2)
colnames(data2)<-"address"
data2$address<-enc2utf8(data2$address)
data2$address<-as.character(data2$address)


#도로명주소이므로 na가 매우 많음
data_lonlat<-mutate_geocode(data2,address,source="google")

View(data_lonlat)
sum(is.na(data_lonlat))

###
#나심심씨의 위치와 우리가 구한 위도 경도 사이의 거리 구하기
geo<-read.csv("geocode.csv",header = T)
str(geo)
colnames(geo)<-c("address","lat","lon")

map_dist_list <- list()
install.packages("Imap")
library(Imap)

#거리 구하는 함수 -> 종로구 적선재에 위치한 나심심씨
for (i in 1:nrow(geo)) {
  
  map_dist_list[[i]] <- gdist(lon.1 = geo$lon[i], 
                              lat.1 = geo$lat[i], 
                              lon.2 = 126.97477,#나심심씨의 경도 
                              lat.2 = 37.31372,#나심심씨의 위도 
                              units="km")
}
#리스트 형식이므로 unlist
map_dist_mat <- sapply(map_dist_list, unlist)
str(map_dist_mat)

bb<-as.data.frame(map_dist_mat)
str(bb)

#위의 yolo 와 cbind
yolo2<-cbind(geo,bb)
str(yolo2)

#나심심씨의 위치에서 1km 반경에 있는 것들만 뽑음 
#era 반경을 다르게 뽑으려면저 숫자만 바꿔주면 됨
era<-tbl_df(yolo2[yolo2$map_dist_mat<50,])
str(era)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(ggmap)


#################3
#나심심씨의 위치를 대충 유동인구수는 4000으로 넣고 한 행을 만들어서 rbind함
era1_1<-era[,-1]
str(era1_1)
era1_1<-as.data.frame(era1_1)
era2<-rbind(era1_1,data.frame(lat=37.31372,long=126.97477,map_dist_mat=NA))
#나심심씨의 위치와 다른 곳의 색깔을 다르게 하기 위해서 뽑음
str(era2)
era2$col<-0
era2[2134,]<-1
era2$col<-as.character(era2$col)
str(era2)

##나심심씨의 위치를 중심으로 지도를 뽑음
g_m<-get_map(location = c(126.987454,37.582194),zoom=15,maptype="roadmap")
windows()
#지도 시각화 
gang.map<-ggmap(g_m)+
  geom_point(data=era2,aes(x=long,y=lat,size=mean,col=col))
gang.map

str(geo)
require(devtools)
install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")
library(ggplot2)
remove.packages("ggmap")
geo<-read.csv("geocode.csv",header = T)
install.packages("ggmap",type="source")
library(ggmap)
windows()
ky_map<-get_map(location=c(126.738188,37.331078),zoom=10,maptype="roadmap")
korea_map <- get_map(location= "South Korea", source="google", maptype="roadmap",zoom=7)
ggmap(korea_map)+
  geom_point(data=geo,aes(x=lon,y=lat))
  

#지도시각화
devtools::install_github("dkahle/ggmap")
library(ggmap)
library(lubridate)

str(geo)
View(geo)
geo$lat<-as.double(geo$lat)
geo$lon<-as.double(geo$lon)

cent <- c(126.96136, 37.52962)
bmap <- ggmap(get_navermap(center = cent, level = 4, baselayer = "default",
                           overlayers = c("anno_satellite", "traffic"), marker = data.frame(cent[1],
                                                                                            cent[2]), key = "c75a09166a38196955adee04d3a51bf8", uri = "www.r-project.org"),
              extent = "device", base_layer = ggplot(geo, aes(x = lon, y = lat)))
