install.packages("formattable")
install.packages("RColorBrewer")
library(ggplot2)#r數據視覺化下載
library(gcookbook)#R數據視覺化資料集下載
library(scales)#下載scales
library(lubridate)
library(RColorBrewer)
library(formattable)
options(scipen=999)
setwd("/Users/prj/Desktop")
#板橋
INDEX=read.csv("板橋天氣.csv")#讀取指數csv
str(INDEX)
INDEX$監測日期<-as.character(INDEX$監測日期)
INDEX$監測日期<-as.Date(INDEX$監測日期)
INDEX$日照時數.小時. <-gsub("-", "0", INDEX$日照時數.小時.)#gsub取代值
INDEX$日照時數.小時. <-as.numeric(INDEX$日照時數.小時.)
head(INDEX$日照時數.小時.,20)
#aggregate([指定做計算的欄位]~[指定作依據的欄位],[資料名稱],[使用函數])
INDEX2<-aggregate(日照時數.小時.~監測日期,INDEX,sum)
names(INDEX2)[2]<-"日照時數.小時._板橋"
str(INDEX2)

#淡水
INDEX3=read.csv("淡水天氣.csv")#讀取指數csv
View(INDEX3)
INDEX3$監測日期<-as.character(INDEX$監測日期)
INDEX3$監測日期<-as.Date(INDEX$監測日期)
INDEX3$日照時數.小時. <-gsub("-", "0", INDEX$日照時數.小時.)#gsub取代值
INDEX3$日照時數.小時. <-as.numeric(INDEX$日照時數.小時.)
head(INDEX3$日照時數.小時.,20)
str(INDEX3)
remove<-which(INDEX3$日照時數.小時.,20)
str(INDEX3)
remove<-which(INDEX3$日照時數.小時.==34.5)
INDEX3<-INDEX3[-remove, ]
INDEX4<-aggregate( 日照時數.小時. ~監測日期 ,INDEX3,sum)
names(INDEX4)[2]<-"日照時數.小時._淡水"
str(INDEX4)
INDEX2<-merge(INDEX2, INDEX4,by="監測日期")
str(INDEX2)

install.packages("ggplot")
library(ggplot)
dev.new()
ggplot()+#x軸日期：y軸價格
  geom_line(data=INDEX2,#data.frame的資料
            aes(x=監測日期,y=日照時數.小時._板橋,colour="板橋"),size=1)+#折線圖
  geom_line(data=INDEX2,#data.frame的資料
            aes(x=監測日期,y=日照時數.小時._淡水,colour="淡水"),size=1)+#折線圖
  theme_bw()+#匡格
  scale_x_date(breaks = datebrakes("months"),labels=data_format("%b"))+
  theme(panel.grid.major.x=element_blank()#刪除主要直網隔線
        ,panel.grid.minor.x = element_blank()#刪除次要直往格線
        ,panel.grid.major.y = element_line(colour="grey50",linetype = "dashed"))+
  
  xlab("監測日期")+ylab("該日總日照數")+
  theme(axis.title.x = element_text(face="italic",colour="darkred",size=10))+
  #調整x軸標題為斜體，深紅色，大小為14
  theme(axis.title.y = element_text(angle=50,face="italic",colour="darkred",size=14))+
  ggtitle(paste(head(INDEX2$監測日期,1),"到",tail(INDEX2$監測日期,1),"新北日照圖"))+
  theme(plot.title=element_text(face="italic",colour="red",hjust=0.5,size=20))+#標題
  theme(axis.text.x = element_text(face="italic",size=20))+#調整x刻度
  theme(axis.text.y = element_text(face="italic",size=20))