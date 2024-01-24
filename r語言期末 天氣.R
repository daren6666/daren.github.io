library(ggplot2)#r數據視覺化下載
library(scales)#下載scales
library(lubridate)
options(scipen=999)
setwd("/Users/prj/Desktop/")
dir()

yonhe=read.csv("/Users/prj/Desktop/", fileEncoding = "big5")
View(yonhe)
names(yonhe)#查欄位名稱
yonhe <- subset(yonhe, ItemEngName=="AMB_TEMP")
yonhe<-yonhe[,c("MonitorDate" ,"Concentration" )]#保留日期及溫度欄位
names(yonhe)[1]<-"monitoring_date"
names(yonhe)[2]<-"temperate"
View(yonhe)

yonhe$TimeCharacter<-as.character(yonhe$monitoring_date)#日期先傳成文字格式
yonhe$TimeSeries<-strptime(yonhe$TimeCharacter,"%Y/%m/%d",tz=Sys.timezone())#再將文字格式轉乘時間註記

yonhe$TimeSeries<-as.POSIXct(yonhe$TimeSeries)#時間註記改成時間序列
yonhe$TimeSeries

yonhe$temperate<-as.character(yonhe$temperate)#溫度先傳成文字格式
yonhe$temperate<-as.numeric(yonhe$temperate)#溫度再轉成數字格式
str(yonhe)#再次檢查資料格式

yonhe2021<-subset(yonhe,TimeSeries<"2021-01-12 CST"& TimeSeries>"2021-01-01 CST")
yonhe2020<-subset(yonhe,TimeSeries<"2021-01-01 CST"& TimeSeries>"2020-01-01 CST")#篩選特定日期的資料
yonhe2019<-subset(yonhe,TimeSeries<"2020-01-01 CST"& TimeSeries>"2019-01-01 CST")#篩選特定日期的資料
yonhe2018<-subset(yonhe,TimeSeries<"2019-01-01 CST"& TimeSeries>"2018-01-01 CST")#篩選特定日期的資料
yonhe2017<-subset(yonhe,TimeSeries<"2018-01-01 CST"& TimeSeries>"2017-01-01 CST")#篩選特定日期的資料
yonhe2016<-subset(yonhe,TimeSeries<"2017-01-01 CST"& TimeSeries>"2016-01-01 CST")#篩選特定日期的資料
yonhe2015<-subset(yonhe,TimeSeries<"2016-01-01 CST"& TimeSeries>"2015-01-01 CST")#篩選特定日期的資料
yonhe2014<-subset(yonhe,TimeSeries<"2015-01-01 CST"& TimeSeries>"2014-01-01 CST")#篩選特定日期的資料

yonhe2020$temperate[which.min(yonhe2020$temperate)]#2020年平均最低日均溫室多少
yonhe2020$TimeCharacter[which.min(yonhe2020$temperate)]#2020年平均最低日均溫室多少，在哪某個日期

yonhe2020$temperate[which.max(yonhe2020$temperate)]#2020年平均最高日均溫室多少
yonhe2020$TimeCharacter[which.max(yonhe2020$temperate)]#2020年平均最高日均溫室多少，在哪某個日期

grep("2020/7/13",yonhe$TimeCharacter)#練習找到哪個特定日期，在長期報表的日期欄位位址
grep(yonhe2020$TimeCharacter[which.max(yonhe2020$temperate)],yonhe$TimeCharacter)#練習找到那個最高溫特定日
grep(yonhe2020$TimeCharacter[which.min(yonhe2020$temperate)],yonhe$TimeCharacter)#練習找到那個羧低溫日
dev.new()
ggplot()+#x軸日期 Y軸溫度
  geom_line(data=yonhe,#data.frame的資料
            aes(x=TimeSeries,y=temperate),size=1)+#x軸必須是時間序列TIMESERIES才可以製圖
  annotate("text"#標籤
           ,yonhe2021$TimeSeries[which.min(yonhe2021$temperate)]#自動找出最低溫的日期
           ,y=yonhe2021$temperate[which.min(yonhe2021$temperate)]#自動找出最低溫的溫度 y軸位置
           ,label=paste(yonhe2021$TimeSeries[which.min(yonhe2021$temperate)]
                        ,"Lo"
                        ,yonhe2021$temperate[which.min(yonhe2021$temperate)])
           ,colour="blue",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2021$TimeSeries[which.max(yonhe2021$temperate)]#自東找出最高溫的日期
           ,y=yonhe2021$temperate[which.max(yonhe2021$temperate)]#自動找出最高溫的日期 y軸位置
           ,label=paste(yonhe2021$TimeSeries[which.max(yonhe2021$temperate)]
                        ,"hi"
                        ,yonhe2021$temperate[which.max(yonhe2021$temperate)])
           ,colour="red",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2020$TimeSeries[which.min(yonhe2020$temperate)]#自動找出最低溫的日期
           ,y=yonhe2020$temperate[which.min(yonhe2020$temperate)]#自動找出最低溫的溫度 y軸位置
           ,label=paste(yonhe2020$TimeSeries[which.min(yonhe2020$temperate)]
                        ,"Lo"
                        ,yonhe2020$temperate[which.min(yonhe2020$temperate)])
           ,colour="blue",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2020$TimeSeries[which.max(yonhe2020$temperate)]#自東找出最高溫的日期
           ,y=yonhe2020$temperate[which.max(yonhe2020$temperate)]#自動找出最高溫的日期 y軸位置
           ,label=paste(yonhe2020$TimeSeries[which.max(yonhe2020$temperate)]
                        ,"hi"
                        ,yonhe2020$temperate[which.max(yonhe2020$temperate)])
           ,colour="red",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2019$TimeSeries[which.min(yonhe2019$temperate)]#自動找出最低溫的日期
           ,y=yonhe2019$temperate[which.min(yonhe2019$temperate)]#自動找出最低溫的溫度 y軸位置
           ,label=paste(yonhe2019$TimeSeries[which.min(yonhe2019$temperate)]
                        ,"Lo"
                        ,yonhe2019$temperate[which.min(yonhe2019$temperate)])
           ,colour="blue",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2019$TimeSeries[which.max(yonhe2019$temperate)]#自東找出最高溫的日期
           ,y=yonhe2019$temperate[which.max(yonhe2019$temperate)]#自動找出最高溫的日期 y軸位置
           ,label=paste(yonhe2019$TimeSeries[which.max(yonhe2019$temperate)]
                        ,"hi"
                        ,yonhe2019$temperate[which.max(yonhe2019$temperate)])
           ,colour="red",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2018$TimeSeries[which.min(yonhe2018$temperate)]#自動找出最低溫的日期
           ,y=yonhe2018$temperate[which.min(yonhe2018$temperate)]#自動找出最低溫的溫度 y軸位置
           ,label=paste(yonhe2018$TimeSeries[which.min(yonhe2018$temperate)]
                        ,"Lo"
                        ,yonhe2018$temperate[which.min(yonhe2018$temperate)])
           ,colour="blue",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2018$TimeSeries[which.max(yonhe2018$temperate)]#自東找出最高溫的日期
           ,y=yonhe2018$temperate[which.max(yonhe2018$temperate)]#自動找出最高溫的日期 y軸位置
           ,label=paste(yonhe2018$TimeSeries[which.max(yonhe2018$temperate)]
                        ,"hi"
                        ,yonhe2018$temperate[which.max(yonhe2018$temperate)])
           ,colour="red",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2017$TimeSeries[which.min(yonhe2017$temperate)]#自動找出最低溫的日期
           ,y=yonhe2017$temperate[which.min(yonhe2017$temperate)]#自動找出最低溫的溫度 y軸位置
           ,label=paste(yonhe2017$TimeSeries[which.min(yonhe2017$temperate)]
                        ,"Lo"
                        ,yonhe2017$temperate[which.min(yonhe2017$temperate)])
           ,colour="blue",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2017$TimeSeries[which.max(yonhe2017$temperate)]#自東找出最高溫的日期
           ,y=yonhe2017$temperate[which.max(yonhe2017$temperate)]#自動找出最高溫的日期 y軸位置
           ,label=paste(yonhe2017$TimeSeries[which.max(yonhe2017$temperate)]
                        ,"hi"
                        ,yonhe2017$temperate[which.max(yonhe2017$temperate)])
           ,colour="red",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2016$TimeSeries[which.min(yonhe2016$temperate)]#自動找出最低溫的日期
           ,y=yonhe2016$temperate[which.min(yonhe2016$temperate)]#自動找出最低溫的溫度 y軸位置
           ,label=paste(yonhe2016$TimeSeries[which.min(yonhe2016$temperate)]
                        ,"Lo"
                        ,yonhe2016$temperate[which.min(yonhe2016$temperate)])
           ,colour="blue",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2016$TimeSeries[which.max(yonhe2016$temperate)]#自東找出最高溫的日期
           ,y=yonhe2016$temperate[which.max(yonhe2016$temperate)]#自動找出最高溫的日期 y軸位置
           ,label=paste(yonhe2016$TimeSeries[which.max(yonhe2016$temperate)]
                        ,"hi"
                        ,yonhe2016$temperate[which.max(yonhe2016$temperate)])
           ,colour="red",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2015$TimeSeries[which.min(yonhe2015$temperate)]#自動找出最低溫的日期
           ,y=yonhe2015$temperate[which.min(yonhe2015$temperate)]#自動找出最低溫的溫度 y軸位置
           ,label=paste(yonhe2015$TimeSeries[which.min(yonhe2015$temperate)]
                        ,"Lo"
                        ,yonhe2015$temperate[which.min(yonhe2015$temperate)])
           ,colour="blue",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2015$TimeSeries[which.max(yonhe2015$temperate)]#自東找出最高溫的日期
           ,y=yonhe2015$temperate[which.max(yonhe2015$temperate)]#自動找出最高溫的日期 y軸位置
           ,label=paste(yonhe2015$TimeSeries[which.max(yonhe2015$temperate)]
                        ,"hi"
                        ,yonhe2015$temperate[which.max(yonhe2015$temperate)])
           ,colour="red",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2014$TimeSeries[which.min(yonhe2014$temperate)]#自動找出最低溫的日期
           ,y=yonhe2014$temperate[which.min(yonhe2014$temperate)]#自動找出最低溫的溫度 y軸位置
           ,label=paste(yonhe2014$TimeSeries[which.min(yonhe2014$temperate)]
                        ,"Lo"
                        ,yonhe2014$temperate[which.min(yonhe2014$temperate)])
           ,colour="blue",size=3,vjust=1.5)+
  annotate("text"#標籤
           ,yonhe2014$TimeSeries[which.max(yonhe2014$temperate)]#自東找出最高溫的日期
           ,y=yonhe2014$temperate[which.max(yonhe2014$temperate)]#自動找出最高溫的日期 y軸位置
           ,label=paste(yonhe2014$TimeSeries[which.max(yonhe2014$temperate)]
                        ,"hi"
                        ,yonhe2014$temperate[which.max(yonhe2014$temperate)])
           ,colour="red",size=3,vjust=1.5)+
  ggtitle(paste("2014-2021永和日均溫時間序列圖"))+
  theme(
    plot.title=element_text(colour="blue",hjust=0.5,size=20),
    panel.grid.major.y = element_line(color = "#3e518a",linetype = 5),
    panel.background = element_rect(fill="white"),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.line = element_blank()
  )

#第二題/Users/prj/Desktop/
library(ggplot2)#r數據視覺化下載
library(gcookbook)#R數據視覺化資料集下載
library(scales)#下載scales
library(lubridate)
options(scipen=999)
setwd("/Users/prj/Desktop/金融應用科技11:5/台東")
dir()
path<-"/Users/prj/Desktop/金融應用科技11:5/台東"#路徑
files<-list.files(path=path,pattern=".csv")#路徑下副檔名是csv檔案集
files#檔案集

df1<-data.frame()#檔案資料集一
df2<-data.frame()#檔案資料集二

for(file in files){     #從第一個檔案開始
  df1<-read.csv(paste(path,file,sep="/"))    #read csv在路徑/檔案
  df2<-rbind(df2,df1)      #資料垂直合併
}

write.csv(df2,file="index.csv")#建立一個csv資料
dir()
index=read.csv("index.csv")
View(index)


str(index)
index$ItemName<-as.character(index$ItemEngName)

index_sub<-index[,4:31]#取需要用的資料


index_sub<-subset(index_sub,ItemName=="AMB_TEMP")#再一次篩選只看溫度

View(index_sub)
index_sub<-index_sub[,5:28]
View(index_sub)#再次篩選，只需要溫度

index_sub$MonitorValue00<-as.numeric(as.character(index_sub$MonitorValue00))
index_sub$MonitorValue01<-as.numeric(as.character(index_sub$MonitorValue01))
index_sub$MonitorValue02<-as.numeric(as.character(index_sub$MonitorValue02))
index_sub$MonitorValue03<-as.numeric(as.character(index_sub$MonitorValue03))
index_sub$MonitorValue04<-as.numeric(as.character(index_sub$MonitorValue04))
index_sub$MonitorValue05<-as.numeric(as.character(index_sub$MonitorValue05))
index_sub$MonitorValue06<-as.numeric(as.character(index_sub$MonitorValue06))
index_sub$MonitorValue07<-as.numeric(as.character(index_sub$MonitorValue07))
index_sub$MonitorValue08<-as.numeric(as.character(index_sub$MonitorValue08))
index_sub$MonitorValue09<-as.numeric(as.character(index_sub$MonitorValue09))
index_sub$MonitorValue10<-as.numeric(as.character(index_sub$MonitorValue10))
index_sub$MonitorValue11<-as.numeric(as.character(index_sub$MonitorValue11))
index_sub$MonitorValue12<-as.numeric(as.character(index_sub$MonitorValue12))
index_sub$MonitorValue13<-as.numeric(as.character(index_sub$MonitorValue13))
index_sub$MonitorValue14<-as.numeric(as.character(index_sub$MonitorValue14))
index_sub$MonitorValue15<-as.numeric(as.character(index_sub$MonitorValue15))
index_sub$MonitorValue16<-as.numeric(as.character(index_sub$MonitorValue16))
index_sub$MonitorValue17<-as.numeric(as.character(index_sub$MonitorValue17))
index_sub$MonitorValue18<-as.numeric(as.character(index_sub$MonitorValue18))
index_sub$MonitorValue19<-as.numeric(as.character(index_sub$MonitorValue19))
index_sub$MonitorValue20<-as.numeric(as.character(index_sub$MonitorValue20))
index_sub$MonitorValue21<-as.numeric(as.character(index_sub$MonitorValue21))
index_sub$MonitorValue22<-as.numeric(as.character(index_sub$MonitorValue22))
index_sub$MonitorValue23<-as.numeric(as.character(index_sub$MonitorValue23))

colnames(index_sub)=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18",
                      "19","20","21","22","23")
dev.new()
par(family='黑體-繁 中黑')
boxplot(index_sub,xlab="時間",ylab="溫度",main=paste(tail(index$MonitorDate,1),"到",
                                                 head(index$MonitorDate,1),
                                                 head(index$SiteName,1),"各時間溫度"),col=rainbow(23),outline=TRUE)

#第三題
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)

options(scipen = 999)
setwd('/Users/prj/Desktop/金融應用科技11:5/台東')
dir()

path <- '/Users/prj/Desktop/金融應用科技11:5/台東' 
files <- list.files(path = path, pattern = '*.csv') 
files 

df1 <- data.frame() 
df2 <- data.frame() 

for(file in files){                                                   
  df1 <- read.csv(paste(path,file,sep = "/")) 
  df2 <- rbind(df2,df1)                       
}

write.csv(df2, file = 'Yonghe.csv') 
dir()
Yonghe = read.csv("Yonghe.csv")
View(Yonghe)

Yonghe <- subset(Yonghe, ItemEngName == 'AMB_TEMP') 
Yonghe <- Yonghe[,c("MonitorDate","Concentration")]   
names(Yonghe)[1] <- "MonitorDate"
names(Yonghe)[2] <- "Temperate"
head(Yonghe,3)

str(Yonghe) 
Yonghe$TimeCharacter <- as.character(Yonghe$MonitorDate) 
Yonghe$TimeSeries <- strptime(Yonghe$TimeCharacter ,"%Y/%m/%d" ,tz = Sys.timezone())  
Yonghe$TimeSeries <- as.POSIXct(Yonghe$TimeSeries) 
head(Yonghe$TimeSeries)

Yonghe$MonitorDate2 <- str_sub(Yonghe$TimeCharacter,6) 
Yonghe$TimeSeries2 <- strptime(Yonghe$MonitorDate2 ,"%m/%d" ,tz = Sys.timezone()) 
Yonghe$TimeSeries2 <- as.POSIXct(Yonghe$TimeSeries2) 
head(Yonghe$TimeSeries2)

Yonghe$Temperate <- as.character(Yonghe$Temperate) 
Yonghe$Temperate <- as.numeric(Yonghe$Temperate) 
str(Yonghe) 
View(Yonghe)

Yonghe2021 <- subset(Yonghe ,TimeSeries <= "2021-01-13 CST" & TimeSeries > "2021-01-01 CST") 
Yonghe2020 <- subset(Yonghe ,TimeSeries <= "2020-01-31 CST" & TimeSeries > "2020-01-01 CST") 
Yonghe2019 <- subset(Yonghe ,TimeSeries <= "2019-01-31 CST" & TimeSeries > "2019-01-01 CST")
Yonghe2018 <- subset(Yonghe ,TimeSeries <= "2018-01-31 CST" & TimeSeries > "2018-01-01 CST")
Yonghe2017 <- subset(Yonghe ,TimeSeries <= "2017-01-31 CST" & TimeSeries > "2017-01-01 CST")
Yonghe2016 <- subset(Yonghe ,TimeSeries <= "2016-01-31 CST" & TimeSeries > "2016-01-01 CST")
Yonghe2015 <- subset(Yonghe ,TimeSeries <= "2015-01-31 CST" & TimeSeries > "2015-01-01 CST")


dev.new()
ggplot() + 
  geom_line(data = Yonghe2020 , 
            aes(x = TimeSeries2 ,y = Temperate ,colour = "2020溫度") ,size = 1) + 
  geom_line(data = Yonghe2019 , 
            aes(x = TimeSeries2 ,y = Temperate ,colour = "2019溫度") ,size = 1) + 
  geom_line(data = Yonghe2018 , 
            aes(x = TimeSeries2 ,y = Temperate ,colour = "2018溫度") ,size = 1) + 
  geom_line(data = Yonghe2017 , 
            aes(x = TimeSeries2 ,y = Temperate ,colour = "2017溫度") ,size = 1) + 
  geom_line(data = Yonghe2016 , 
            aes(x = TimeSeries2 ,y = Temperate ,colour = "2016溫度") ,size = 1) + 
  geom_line(data = Yonghe2015 , 
            aes(x = TimeSeries2 ,y = Temperate ,colour = "2015溫度") ,size = 1) + 
  theme_bw()+ 
  annotate("text" ,  
           Yonghe2020$TimeSeries2[which.min(Yonghe2020$Temperate)] , 
           y = Yonghe2020$Temperate[which.min(Yonghe2020$Temperate)] , 
           label = paste(Yonghe2020$TimeSeries[which.min(Yonghe2020$Temperate)] ,
                         "Lo" ,Yonghe2020$Temperate[which.min(Yonghe2020$Temperate)]) ,
           colour = "blue" , size = 3 , vjust = 1.5) +
  annotate("text" , 
           Yonghe2020$TimeSeries2[which.max(Yonghe2020$Temperate)] ,
           y = Yonghe2020$Temperate[which.max(Yonghe2020$Temperate)] ,
           label = paste(Yonghe2020$TimeSeries[which.max(Yonghe2020$Temperate)] ,
                         "Hi" ,Yonghe2020$Temperate[which.max(Yonghe2020$Temperate)]) ,
           colour = "red" , size = 3 , vjust = -1.5)+
  annotate("text" , 
           Yonghe2019$TimeSeries2[which.min(Yonghe2019$Temperate)] ,
           y = Yonghe2019$Temperate[which.min(Yonghe2019$Temperate)] ,
           label = paste(Yonghe2019$TimeSeries[which.min(Yonghe2019$Temperate)] ,
                         "Lo" ,Yonghe2019$Temperate[which.min(Yonghe2019$Temperate)]) ,
           colour = "blue" , size = 3 , vjust = 1.5) +
  annotate("text" , 
           Yonghe2019$TimeSeries2[which.max(Yonghe2019$Temperate)] ,
           y = Yonghe2019$Temperate[which.max(Yonghe2019$Temperate)] ,
           label = paste(Yonghe2019$TimeSeries[which.max(Yonghe2019$Temperate)] ,
                         "Hi" ,Yonghe2019$Temperate[which.max(Yonghe2019$Temperate)]) ,
           colour = "red" , size = 3 , vjust = -1.5) +
  annotate("text" , 
           Yonghe2018$TimeSeries2[which.min(Yonghe2018$Temperate)] ,
           y = Yonghe2018$Temperate[which.min(Yonghe2018$Temperate)] ,
           label = paste(Yonghe2018$TimeSeries[which.min(Yonghe2018$Temperate)] ,
                         "Lo" ,Yonghe2018$Temperate[which.min(Yonghe2018$Temperate)]) ,
           colour = "blue" , size = 3 , vjust = 1.5) +
  annotate("text" , 
           Yonghe2018$TimeSeries2[which.max(Yonghe2018$Temperate)] ,
           y = Yonghe2018$Temperate[which.max(Yonghe2018$Temperate)] ,
           label = paste(Yonghe2018$TimeSeries[which.max(Yonghe2018$Temperate)] ,
                         "Hi" ,Yonghe2018$Temperate[which.max(Yonghe2018$Temperate)]) ,
           colour = "red" , size = 3 , vjust = -1.5) +
  annotate("text" , 
           Yonghe2017$TimeSeries2[which.min(Yonghe2017$Temperate)] ,
           y = Yonghe2017$Temperate[which.min(Yonghe2017$Temperate)] ,
           label = paste(Yonghe2017$TimeSeries[which.min(Yonghe2017$Temperate)] ,
                         "Lo" ,Yonghe2017$Temperate[which.min(Yonghe2017$Temperate)]) ,
           colour = "blue" , size = 3 , vjust = 1.5) +
  annotate("text" , 
           Yonghe2017$TimeSeries2[which.max(Yonghe2017$Temperate)] ,
           y = Yonghe2017$Temperate[which.max(Yonghe2017$Temperate)] ,
           label = paste(Yonghe2017$TimeSeries[which.max(Yonghe2017$Temperate)] ,
                         "Hi" ,Yonghe2017$Temperate[which.max(Yonghe2017$Temperate)]) ,
           colour = "red" , size = 3 , vjust = -1.5) +
  annotate("text" , 
           Yonghe2016$TimeSeries2[which.min(Yonghe2016$Temperate)] ,
           y = Yonghe2016$Temperate[which.min(Yonghe2016$Temperate)] ,
           label = paste(Yonghe2016$TimeSeries[which.min(Yonghe2016$Temperate)] ,
                         "Lo" ,Yonghe2016$Temperate[which.min(Yonghe2016$Temperate)]) ,
           colour = "blue" , size = 3 , vjust = 1.5) +
  annotate("text" , 
           Yonghe2016$TimeSeries2[which.max(Yonghe2016$Temperate)] ,
           y = Yonghe2016$Temperate[which.max(Yonghe2016$Temperate)] ,
           label = paste(Yonghe2016$TimeSeries[which.max(Yonghe2016$Temperate)] ,
                         "Hi" ,Yonghe2016$Temperate[which.max(Yonghe2016$Temperate)]) ,
           colour = "red" , size = 3 , vjust = -1.5) +
  annotate("text" , 
           Yonghe2015$TimeSeries2[which.min(Yonghe2015$Temperate)] ,
           y = Yonghe2015$Temperate[which.min(Yonghe2015$Temperate)] ,
           label = paste(Yonghe2015$TimeSeries[which.min(Yonghe2015$Temperate)] ,
                         "Lo" ,Yonghe2015$Temperate[which.min(Yonghe2015$Temperate)]) ,
           colour = "blue" , size = 3 , vjust = 1.5) +
  annotate("text" , 
           Yonghe2015$TimeSeries2[which.max(Yonghe2015$Temperate)] ,
           y = Yonghe2015$Temperate[which.max(Yonghe2015$Temperate)] ,
           label = paste(Yonghe2015$TimeSeries[which.max(Yonghe2015$Temperate)] ,
                         "Hi" ,Yonghe2015$Temperate[which.max(Yonghe2015$Temperate)]) ,
           colour = "red" , size = 3 , vjust = -1.5)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey50",linetype = "dashed")) +  
  xlab("監測日期") + ylab("溫度") +
  theme(axis.title.x = element_text(face = "italic",colour = "darkred",size = 10)) +
  theme(axis.title.y = element_text(angle = 90,face = "italic",colour = "darkred",size = 14)) +
  ggtitle(paste("Taitong_Temperate_history: ",str_sub(Yonghe2020$TimeSeries[1], 6,7),"月")) +
  theme(plot.title = element_text(face = "italic",colour = "red",
                                  hjust = 0.5, size = 20)) + 
  theme(axis.text.x = element_text(face = "italic",size = 20)) + 
  theme(axis.text.y = element_text(face = "italic",size = 20)) + 
  theme(text = element_text(family='STKaitiTC-Regular'))

#第四題
library(ggplot2)
setwd("/Users/prj/Desktop/")
INDEX=read.csv("台東.csv", fileEncoding = "big5")
str(INDEX)
INDEX$ItemName<-as.character(INDEX$ItemName)
INDEX_sub<-INDEX[,4:31]
INDEX_sub<-subset(INDEX_sub,ItemName=="溫度")

INDEX_sub<-INDEX_sub[,5:28]
INDEX_sub$MonitorValue00 <-as.numeric(as.character(INDEX_sub$MonitorValue00))
INDEX_sub$MonitorValue01 <-as.numeric(as.character(INDEX_sub$MonitorValue01))
INDEX_sub$MonitorValue02 <-as.numeric(as.character(INDEX_sub$MonitorValue02))
INDEX_sub$MonitorValue03 <-as.numeric(as.character(INDEX_sub$MonitorValue03))
INDEX_sub$MonitorValue04 <-as.numeric(as.character(INDEX_sub$MonitorValue04))
INDEX_sub$MonitorValue05 <-as.numeric(as.character(INDEX_sub$MonitorValue05))
INDEX_sub$MonitorValue06 <-as.numeric(as.character(INDEX_sub$MonitorValue06))
INDEX_sub$MonitorValue07 <-as.numeric(as.character(INDEX_sub$MonitorValue07))
INDEX_sub$MonitorValue08 <-as.numeric(as.character(INDEX_sub$MonitorValue08))
INDEX_sub$MonitorValue09 <-as.numeric(as.character(INDEX_sub$MonitorValue09))
INDEX_sub$MonitorValue10 <-as.numeric(as.character(INDEX_sub$MonitorValue10))
INDEX_sub$MonitorValue11 <-as.numeric(as.character(INDEX_sub$MonitorValue11))
INDEX_sub$MonitorValue12 <-as.numeric(as.character(INDEX_sub$MonitorValue12))
INDEX_sub$MonitorValue13 <-as.numeric(as.character(INDEX_sub$MonitorValue13))
INDEX_sub$MonitorValue14 <-as.numeric(as.character(INDEX_sub$MonitorValue14))
INDEX_sub$MonitorValue15 <-as.numeric(as.character(INDEX_sub$MonitorValue15))
INDEX_sub$MonitorValue16 <-as.numeric(as.character(INDEX_sub$MonitorValue16))
INDEX_sub$MonitorValue17 <-as.numeric(as.character(INDEX_sub$MonitorValue17))
INDEX_sub$MonitorValue18 <-as.numeric(as.character(INDEX_sub$MonitorValue18))
INDEX_sub$MonitorValue19 <-as.numeric(as.character(INDEX_sub$MonitorValue19))
INDEX_sub$MonitorValue20 <-as.numeric(as.character(INDEX_sub$MonitorValue20))
INDEX_sub$MonitorValue21 <-as.numeric(as.character(INDEX_sub$MonitorValue21))
INDEX_sub$MonitorValue22 <-as.numeric(as.character(INDEX_sub$MonitorValue22))
INDEX_sub$MonitorValue23 <-as.numeric(as.character(INDEX_sub$MonitorValue23))
colnames(INDEX_sub)=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13",
                      "14","15","16","17","18","19","20","21","22","23")
INDEX_sub<-t(INDEX_sub)
final<-data.frame(hours = c(0:23),
                  temp=c(INDEX_sub))

dev.new()
ggplot()+
  geom_line(data=final,aes(x=hours,y=temp))+
  geom_point(data=final,aes(x=hours,y=temp,size=3))+
  ggtitle('台東0124 24小時溫度')+
  theme(
    plot.title=element_text(face='italic',color='red',hjust=0.5,size=20)
  )