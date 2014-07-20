library(data.table)
library(lattice)
activity_data<-read.csv("activity.csv",as.is="date")
activity_data$date<-as.Date(activity_data$date)
## create data tables with & without NAs
complete_data<-data.table(activity_data[complete.cases(activity_data$steps),])
incomplete_data<-data.table(activity_data[!complete.cases(activity_data$steps),])
complete_data$dayofweek<-factor(ifelse(weekdays(complete_data$date)%in%c('Saturday','Sunday'),"weekend","weekday"))
## create histogram of total steps per day; calculate mean and median
stepsBydate<-complete_data[,list(sum_steps=sum(steps)),by=date]
windows()
with(stepsBydate,plot(date,sum_steps,type="h",col="blue",lwd=4,xlab="Date",ylab="Total Steps"))
meanSteps<-mean(stepsBydate$sum_steps)
medSteps<-median(stepsBydate$sum_steps)
## create line plot of average steps per time interval
stepsByinterval<-complete_data[,list(avg_steps=mean(steps)),by=interval]
windows()
with(stepsByinterval,plot(interval,avg_steps,type="l",col="blue",xlab="Interval",ylab="Average Steps"))
stepsByinterval[avg_steps==max(avg_steps),]
## how many NAs?
nrow(incomplete_data)
## We will impute 
impute_values<-complete_data[,list(mean_steps=mean(steps)),by=list(dayofweek,interval)]
imputed_data<-merge(incomplete_data,impute_values,by=c("dayofweek","interval"))
imputed_data2<-subset(imputed_data,select=c("mean_steps","date","interval","dayofweek"))
colnames(imputed_data2)<-(c("steps","date","interval","dayofweek"))
all_data<-rbind(imputed_data2,complete_data)
all_data$daytype<-factor(ifelse(weekdays(all_data$date)%in%c('Saturday','Sunday'),"weekend","weekday"))
allstepsBydate<-all_data[,list(sum_steps=sum(steps)),by=date]
with(allstepsBydate,plot(date,sum_steps,type="h",col="blue",lwd=4,xlab="Date",ylab="Total Steps"))

allmeanSteps<-mean(allstepsBydate$sum_steps)
allmedSteps<-median(allstepsBydate$sum_steps)
with(all_data,xyplot(mean_steps~interval|dayofweek,type="l",layout=c(1,2),xlab="Interval",ylab="Number of Steps"))
all_data_summarized<-all_data[,list(mean_steps=mean(steps)),by=list(date,interval)]
all_data_summarized$daytype<-factor(ifelse(weekdays(all_data_summarized$date)%in%c('Saturday','Sunday'),"weekend","weekday"))
with(all_data_summarized,xyplot(mean_steps~interval|daytype,type="l",layout=c(1,2),xlab="Interval",ylab="Number of Steps"))
