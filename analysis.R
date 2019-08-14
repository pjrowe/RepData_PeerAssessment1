library("dplyr")
library(lubridate)
library(ggplot2)

#
# 1. Reading in dataset and processing (unzip, load into mydata dataframe)
#

unzip('activity.zip')
mydata<-read.csv('activity.csv',header = TRUE, sep = ",", dec = ".")
# change to date format
mydata$date<-as.Date(mydata$date,"%Y-%m-%d")

# add column with # of weekday which will come in handy later for separating between Weekend 
# and Weekday
mydata$weekday<-wday(mydata$date)

#
# 2. Histogram of total steps by date, before removal of NAs 
#

# two options to do this one, either by aggregate() or by group_by() and summarize()
# total_by_day <- aggregate(steps ~ date, mydata, sum)
# hist(total_by_day$steps,xlab="Number of Steps in Day",
#     ylab="Frequency (Number of Days)",breaks=10,main='Histogram of Total Steps in Day')

data_bydate<-group_by(mydata,date)
stepsbydate<-summarize(data_bydate,total_steps=sum(steps))
png(file="./instructions_fig/steps_histo.png",width=480,height=480)
hist(stepsbydate$total_steps,xlab="Number of Steps in Day",
     ylab="Frequency (Number of Days)",breaks=10,main='Histogram of Total Steps in Day')
dev.off()

#
# 3. Mean and median steps per day
#

# two ways I tried to do this
stepmean1<-mean(total_by_day$steps)
stepmedian1<-median(total_by_day$steps)

# this ignores the days with NAs
smean<-mean(stepsbydate$total_steps[!is.na(stepsbydate$total_steps)])
smedian<-median(stepsbydate$total_steps[!is.na(stepsbydate$total_steps)])

#
# 4. Time series plot of the average number of steps taken
#

# easiest way
total_by_interval <- aggregate(steps ~ interval, mydata, sum)
mean_by_interval <- aggregate(steps ~ interval, mydata, mean)

# manual way, summing total and then dividing by # of days w/o NA
good_days<-sum(!is.na(stepsbydate$total_steps))
calc_mean_by_interval<-total_by_interval
calc_mean_by_interval$steps<-calc_mean_by_interval$steps/good_days

# this shows the answers are identical
mean_by_interval==calc_mean_by_interval

# these do not work because of NAs; once cleaned of NAs, these will work
data_byinterval<-group_by(mydata,interval)
interval_mean<-summarize(data_byinterval,mean_steps=mean(steps))

# this is the plot
png(file='./instructions_fig/mean_interval.png',width=480,height=480)
with(mean_by_interval,plot(x=interval,y=steps,xlab='Interval #',
          ylab="Mean Steps During Interval",type="l",main="Mean # of Steps by Interval"))
dev.off()

#
# 5. The 5-minute interval that, on average, contains the maximum number of steps
#
mean_by_interval$interval[which(mean_by_interval$mean_steps==max(mean_by_interval$mean_steps))]

# max is 206.1698, interval is 835


#
# 6. remove days with 'NA' for all intervals in day; we see that all the 'NA's were concentrated in 8 days 
#
dates_toremove<-stepsbydate$date[is.na(stepsbydate$total_steps)]
 
# now mydata2 has no NAs
mydata2<-filter(mydata,!(date%in%dates_toremove))
data_bydate2<-group_by(mydata2,date)
stepsbydate2<-summarize(data_bydate2,total_steps=sum(steps))

#
# 7. Histogram after missing values imputed
#
png(file="./instructions_fig/steps_histogram2.png",width=480,height=480)
hist(stepsbydate2$total_steps,xlab="Number of Steps in Day",
     ylab="Frequency (Number of Days)",breaks=10,main='Histogram of Total Steps in Day, NAs Removed')

dev.off()


#
# 8.Panel plot comparing the average number of steps taken per 5-minute interval 
#
# across weekdays and weekends
weekdays_data<-filter(data_bydate2,(weekday>1&weekday<7))
weekend_data<-filter(data_bydate2,(weekday==1|weekday==7))

data_byinterval_wd<-group_by(weekdays_data,interval)
interval_mean_wd<-summarize(data_byinterval_wd,mean_steps=mean(steps))
interval_mean_wd$type<-"Weekday"

data_byinterval_we<-group_by(weekend_data,interval)
interval_mean_we<-summarize(data_byinterval_we,mean_steps=mean(steps))
interval_mean_we$type<-"Weekend"
mrg<-rbind(interval_mean_we,interval_mean_wd)

png(file="./instructions_fig/weekend_vs_weekday2.png",width=480,height=480)
g<-ggplot(mrg,aes(x=interval,y=mean_steps),color=type) 
g+facet_grid(.~type)+geom_line()+labs(x="Interval",y="Mean Steps Per Interval",title="Mean Steps Comparison")

dev.off()

#Alternate plot 8, same grid
#png(file="weekend_vs_weekday.png",width=480,height=480)
#g<-ggplot(mrg,aes(x=mrg$interval,y=mrg$mean_steps)) 
#g+geom_line(aes(color=type),lwd=2)+labs(x="Interval",y="Mean Steps Per Interval",title="Mean Steps Comparison")
#dev.off()

# these can be used for doing mean steps by interval plot
# data_byinterval<-group_by(mydata2,interval)
# interval_mean<-summarize(data_byinterval,mean_steps=mean(steps))


