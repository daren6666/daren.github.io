library(lubridate)
library(ggplot2)

setwd("/Users/prj/Desktop/金融應用科技11:5")
dir()

SunShine=read.csv("11月日照時數.csv")#csv
View(SunShine)
SunShine<-SunShine[-1,]#刪除第一項
View(SunShine)
str(SunShine)
SunShine$觀測時間.day.<-as.character(SunShine$觀測時間.day.)
SunShine$觀測時間.day.<-as.numeric(SunShine$觀測時間.day.)
SunShine$日照時數.hour.<-as.character(SunShine$日照時數.hour.)
SunShine$日照時數.hour.<-as.numeric(SunShine$日照時數.hour.)


year(SunShine$測站最高氣壓時間.LST.[1])
month(SunShine$測站最高氣壓時間.LST.[1])

SunShine$觀測時間.day.
SunShine$日照時數.hour.
str(SunShine)

dev.new()
ggplot(SunShine,aes(x=觀測時間.day.,y=日照時數.hour.))+geom_bar(stat="identity")+
  theme_bw()+#匡格
  theme(panel.grid.major.x=element_blank()#刪除主要直網格線)
        ,panel.grid.minor.x=element_blank())+#刪除次要直網格線
  xlab("日期")+ylab("日照時數")+
  theme(axis.title.x=element_text(face="italic",colour="darkred",size=10))+
  #調整x軸標題為斜體，深紅色，大小為14
  theme(axis.title.y=element_text(angle=90,face="italic",colour="darkred",size=14))+
  ggtitle(paste(year(SunShine$測站最高氣壓時間.LST.[1]),"年"
                ,month(SunShine$測站最高氣壓時間.LST.[1]),"月","每日日照時數"))+
  theme(plot.title=element_text(face="italic",colour="red"
                                ,hjust=0.5,size=20))+#標題
  theme(axis.text.x=element_text(face="italic",size=20))+#調整x刻度
  theme(axis.text.y=element_text(face="italic",size=20))+
  theme(text=element_text(family='STKaitiTC-Regular'))

library(ggplot2)
library(scales)
library(lubridate)
library(stringr)
options(scipen=999)
setwd("/Users/prj/Desktop/金融應用科技11:5")
dir()

Yonghe=read.csv("永和空品歷史資料.csv")
View(Yonghe)
names(Yonghe)#查欄位名稱
Yonghe<-Yonghe[,c("監測日期","溫度.br...AMB_TEMP.br......")]#保留日期及溫度的欄位
names(Yonghe)[1]<-"Monitoring_date"
names(Yonghe)[2]<-"Temperate"
head(Yonghe,3)

str(Yonghe)#查詢格式
Yonghe$TimeCharacter<-as.character(Yonghe$Monitoring_date)#日期先轉成文字格式
Yonghe$TimeSeries<-strptime(Yonghe$TimeCharacter,"%y/%m/%d",tz=Sys.timezone())#再將文字格式時間註記
Yonghe$TimeSeries<-as.POSIXct(Yonghe$TimeSeries)#時間註記改成時間序列格式
Yonghe$Monitoring_date2<-str_sub(Yonghe$TimeCharacter,6)#擷取第六字元到結束的索引位置的字串
Yonghe$TimeSeries2<-strptime(Yonghe$Monitoring_date2,"%m/%d",tz=Sys.timezone())#再將文字格式時間註記
Yonghe$TimeSeries2<-as.POSIXct(Yonghe$TimeSeries2)#時間註記改成時間序列格式
Yonghe$TimeSeries2

Yonghe$Temperate<-as.character(Yonghe$Temperate)#溫度先改成文字格式
Yonghe$Temperate<-as.numeric(Yonghe$Temperate)#溫度再轉成數字格式
str(Yonghe)#再次檢查資料格式

Yonghe2020<-subset(Yonghe,TimeSeries<"2020-11-26 CST"&TimeSeries>"2020-11-01 CST")#刪選特定日期資料
Yonghe2019<-subset(Yonghe,TimeSeries<"2019-11-30 CST"&TimeSeries>"2019-11-01 CST")#刪選特定日期資料
Yonghe2018<-subset(Yonghe,TimeSeries<"2018-11-30 CST"&TimeSeries>"2018-11-01 CST")#刪選特定日期資料
Yonghe2017<-subset(Yonghe,TimeSeries<"2017-11-30 CST"&TimeSeries>"2017-11-01 CST")#刪選特定日期資料
Yonghe2016<-subset(Yonghe,TimeSeries<"2016-11-30 CST"&TimeSeries>"2016-11-01 CST")#刪選特定日期資料
Yonghe2015<-subset(Yonghe,TimeSeries<"2015-11-30 CST"&TimeSeries>"2015-11-01 CST")#刪選特定日期資料

dev.new()
ggplot()+#x軸日期,y軸價格（不含息淨值）
  geom_line(data=Yonghe2020,#data.frame的資料
            aes(x=TimeSeries2,y=Temperate,colour="2020溫度"),size=1)+#折線圖
  geom_line(data=Yonghe2019,#data.frame的資料
            aes(x=TimeSeries2,y=Temperate,colour="2019溫度"),size=1)+#折線圖
  geom_line(data=Yonghe2018,#data.frame的資料
            aes(x=TimeSeries2,y=Temperate,colour="2018溫度"),size=1)+#折線圖
  geom_line(data=Yonghe2017,#data.frame的資料
            aes(x=TimeSeries2,y=Temperate,colour="2017溫度"),size=1)+#折線圖
  geom_line(data=Yonghe2016,#data.frame的資料
            aes(x=TimeSeries2,y=Temperate,colour="2016溫度"),size=1)+#折線圖
  geom_line(data=Yonghe2015,#data.frame的資料
            aes(x=TimeSeries2,y=Temperate,colour="2015溫度"),size=1)+#折線圖
  theme(text = element_text(family='黑體-繁 中黑'))


  
  
  