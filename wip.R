library(data.table)
activity_data<-read.csv("activity.csv",as.is="date")
summary(activity_data)
activity_data$date<-as.Date(activity_data$date)
complete_data<-data.table(activity_data[complete.cases(activity_data$steps),])
incomplete_data<-data.table(activity_data[!complete.cases(activity_data$steps),])
complete_data_summarized<-complete_data[,list(sum_steps=sum(steps),mean_steps=mean(steps),med_steps=median(steps)),by=list(date,interval)]
complete_data_summarized$dayofweek<-factor(ifelse(weekdays(complete_data_summarized$date)%in%c('Saturday','Sunday'),"weekend","weekday"))
with(complete_data_summarized,plot(date,sum_steps,type="h",col="blue",lwd=4,xlab="Date",ylab="Total Steps"))
windows()
library(lattice)
with(complete_data_summarized,xyplot(mean_steps~interval|dayofweek,type="l",layout=c(1,2),xlab="Interval",ylab="Number of Steps"))


