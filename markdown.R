unzip("activity.zip")
activity<-read.csv("activity.csv")
activity$date<-as.Date(activity$date)
total<-aggregate(steps~date,data = activity,sum)
hist<-hist(total$steps)
mean1<-mean(total$steps)
median<-median(total$steps)
interval<-aggregate(steps~interval,data = activity,mean)
plot(interval$interval,interval$steps,type = "l",xlab = "Interval",ylab = "Steps")
interval[which.max(interval$steps),]
Missing<-sum(is.na(activity$steps))
addMean<-merge(interval,activity,by.x = "interval",by.y = "interval")

filled<-addMean
count = 0
for (i in 1:nrow(filled)){
  if (is.na(filled[i,3])){
  filled[i,3]<-filled[i,2]
  count = count+1
  }}

FilledTotal<-aggregate(steps.y~date,data = filled,sum)
FilledHist<-hist(FilledTotal$steps.y)
meanperDay<-mean(FilledTotal$steps.y)
medianperDay<-median(FilledTotal$steps.y)

weekday<- ifelse(weekdays(filled$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
weekday<-cbind(filled,weekday)
average<-aggregate(steps.y~interval+weekday,data = weekday,mean)

qplot(interval,steps.y,data=average,facets = ~weekday)+geom_line()


