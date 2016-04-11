if (!require('Rfacebook')){
  install.packages("Rfacebook")
  library(Rfacebook)
}
token<-'CAACEdEose0cBAHGrME31kF8LHlmsWdZBpJtSO5GsKZBmxrVkNQknhjWAKyNAhKkIuhNHlPbLweSNNw4P75w2ZBTEpeliOeum8h6cPDUOuHo2gqgUdMBS6Xc8ZAq5aoAtgP1gZCAN6zAtbQ1SukZBJoqsCc3IQ0YkrEAbmWjZCZANKUZAw9xdLuTskLWQqkHuJtj1j972HAblY6k4m6ZBwoyW2c'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
  tempPage<-getPage("llchu", token,
                    since = DateVectorStr[i],until = DateVectorStr[i+1])
  totalPage<-rbind(totalPage,tempPage)
}
nrow(totalPage)
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(id~dateTPE,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))

totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(likes_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(PostCount[order(PostCount$likes_count,decreasing = T),]))
