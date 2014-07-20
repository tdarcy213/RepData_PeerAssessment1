library(data.table)
activity_data<-read.csv("activity.csv",as.is="date")
summary(activity_data)
activity_data$date<-as.Date(activity_data$date)
complete_data<-data.table(activity_data[complete.cases(activity_data$steps),])
complete_data$dayofweek<-factor(weekdays(complete_data$date))

incomplete_data<-data.table(activity_data[!complete.cases(activity_data$steps),])
incomplete_data$dayofweek<-factor(weekdays(incomplete_data$date))

complete_data_summarized<-complete_data[,list(sum_steps=sum(steps),mean_steps=mean(steps),med_steps=median(steps)),by=list(date,interval)]
complete_data_summarized$dayofweek<-factor(ifelse(weekdays(complete_data_summarized$date)%in%c('Saturday','Sunday'),"weekend","weekday"))
with(complete_data_summarized,plot(date,sum_steps,type="h",col="blue",lwd=4,xlab="Date",ylab="Total Steps"))
windows()
library(lattice)
with(complete_data_summarized,xyplot(mean_steps~interval|dayofweek,type="l",layout=c(1,2),xlab="Interval",ylab="Number of Steps"))
impute_values<-complete_data[,list(mean_steps=mean(steps)),by=list(dayofweek,interval)]
imputed_data<-merge(incomplete_data,impute_values,by=c("dayofweek","interval"))
imputed_data2<-subset(imputed_data,select=c("mean_steps","date","interval","dayofweek"))
colnames(imputed_data2)<-(c("steps","date","interval","dayofweek"))
all_data<-rbind(imputed_data2,complete_data)
with(all_data_summarized,xyplot(mean_steps~interval|daytype,type="l",layout=c(1,2),xlab="Interval",ylab="Number of Steps"))


