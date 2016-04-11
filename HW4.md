分析柯文哲粉絲專業，資料分析區間為2016/01/01至
2016/04/11貼文分析

## 讀取Cherng粉絲團資料
```{r}
if (!require('Rfacebook')){
  install.packages("Rfacebook")
  library(Rfacebook)
}
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

說明說明～～～

範例：分析粉Cherng絲團每天的發文數，由於日期格式OOXOXX，
先將其轉換為台灣時區...xxxoo

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
ˋˋˋ
## 每日likes數分析
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
## 每日comments數分析
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
## 每日shares數分析
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
