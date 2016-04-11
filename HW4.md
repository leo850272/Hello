---
title: "Facebook粉絲團分析（分析專頁:柯文哲）"
output: github_document
---
分析柯文哲粉絲專業，資料分析區間為2016/01/01至
2016/04/11貼文分析

## 讀取DoctorKoWJ粉絲團資料
```{r}
if (!require('Rfacebook')){
  install.packages("Rfacebook")
  library(Rfacebook)
}
```
```{r}
token<-'EAACEdEose0cBAHhbhifFoYj97WPWNJoM8mTEu4CrG180nMbtUPGD7jzayr2cbyhxf6RNpdNhKfquOBvIRAGZCoHDvhhEmHDdXJwy7l5wiSE4RmDdEu4a3NcO5ZBZAYeAWWmyOX7BlBt5M0rTOJOrbSBFaWZACwtOimv6is18MjvmxyUX0Q3pl02nU09ZBdPAZD'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
  tempPage<-getPage("DoctorKoWJ", token,
                    since = DateVectorStr[i],until = DateVectorStr[i+1])
  totalPage<-rbind(totalPage,tempPage)
}
nrow(totalPage)
```
## 每日發文數分析


分析DoctorKoWJ粉絲團每天的發文數，先將其轉換為台灣時區，可以看到1月9日篇文數4篇最多，可能因為柯文哲當天挑戰單車「一日北高，雙城挑戰」的關係
```{r}
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(id~dateTPE,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))
```
|   |dateTPE    | id|
|:--|:----------|--:|
|6  |2016-01-09 |  4|
|5  |2016-01-08 |  2|
|7  |2016-01-10 |  2|
|25 |2016-02-06 |  2|
|44 |2016-03-22 |  2|
|1  |2016-01-01 |  1|
## 每日likes數分析

可以看到1月16日的按讚數高達32萬人最多，可能因為當天為選舉日，而且他發有關選舉感想的文章
```{r}

totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(likes_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(PostCount[order(PostCount$likes_count,decreasing = T),]))
```
|   |dateTPE    | likes_count|
|:--|:----------|-----------:|
|11 |2016-01-16 |    329086.0|
|33 |2016-02-28 |    228903.0|
|7  |2016-01-10 |    223664.5|
|9  |2016-01-14 |    187450.0|
|32 |2016-02-27 |    180921.0|
|47 |2016-03-28 |    139077.0|
## 每日comments數分析
可以看到1月10日留言數接近6000最多，可能因為他剛完成單車挑戰，並且發挑戰完成的感想文章緣故
```{r}

totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(comments_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(PostCount[order(PostCount$comments_count,decreasing = T),]))
```
|   |dateTPE    | comments_count|
|:--|:----------|--------------:|
|7  |2016-01-10 |        5981.50|
|6  |2016-01-09 |        5153.25|
|47 |2016-03-28 |        5111.00|
|33 |2016-02-28 |        3565.00|
|32 |2016-02-27 |        3268.00|
|9  |2016-01-14 |        2848.00|
## 每日shares數分析
可以看到1月14日分享數34775次最高，可能因為他當天發的文章內容很激勵人心，內容很勵志，有提到夢想
```{r}

totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(shares_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(PostCount[order(PostCount$shares_count,decreasing = T),]))

```
|   |dateTPE    | shares_count|
|:--|:----------|------------:|
|9  |2016-01-14 |      34775.0|
|7  |2016-01-10 |      16406.5|
|33 |2016-02-28 |      11235.0|
|35 |2016-03-04 |       5333.0|
|47 |2016-03-28 |       4964.0|
|11 |2016-01-16 |       4897.0|
