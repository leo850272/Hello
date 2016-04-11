���R�_��������M�~�A��Ƥ��R�϶���2016/01/01��
2016/04/11�K����R

## Ū��Cherng�����θ��
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
## �C��o��Ƥ��R

������������

�d�ҡG���R��Cherng���ΨC�Ѫ��o��ơA�ѩ����榡OOXOXX�A
���N���ഫ���x�W�ɰ�...xxxoo

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
������
## �C��likes�Ƥ��R
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
## �C��comments�Ƥ��R
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
## �C��shares�Ƥ��R
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