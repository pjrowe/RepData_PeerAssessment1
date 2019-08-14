---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv() )
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
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
```

## What is mean total number of steps taken per day?

For this part of the assignment, we are told that we can ignore the missing values in the dataset.  The tasks are the following, and are contained in items 2 and 3 in the **Commit containing full submission** section below, which is how I commented the code:

1. Calculate the total number of steps taken per day 
2. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
#
# 2. Histogram of total steps by date, before removal of NAs 
#

total_by_day <- aggregate(steps ~ date, mydata, sum)

# hist(total_by_day$steps,xlab="Number of Steps in Day",
#     ylab="Frequency (Number of Days)",breaks=10,main='Histogram of Total Steps in Day')

data_bydate<-group_by(mydata,date)
stepsbydate<-summarize(data_bydate,total_steps=sum(steps))
png(file="./instructions_fig/steps_histo.png",width=480,height=480)
hist(stepsbydate$total_steps,xlab="Number of Steps in Day",
     ylab="Frequency (Number of Days)",breaks=10,main='Histogram of Total Steps in Day')
dev.off()
```

```
## png 
##   2
```

```r
#
# 3. Mean and median steps per day
#

# two ways I tried to do this
stepmean1<-mean(total_by_day$steps)
stepmedian1<-median(total_by_day$steps)
print(c("Mean steps per day is:",round(stepmean1,3)))
```

```
## [1] "Mean steps per day is:" "10766.189"
```

```r
print(c("Median steps per day is:",round(stepmedian1,3)))
```

```
## [1] "Median steps per day is:" "10765"
```

```r
# this ignores the days with NAs
smean<-mean(stepsbydate$total_steps[!is.na(stepsbydate$total_steps)])
smedian<-median(stepsbydate$total_steps[!is.na(stepsbydate$total_steps)])
print(c("Method 2: Mean steps per day is:",round(stepmean1,3)))
```

```
## [1] "Method 2: Mean steps per day is:" "10766.189"
```

```r
print(c("Method 2: Median steps per day is:",round(stepmedian1,3)))
```

```
## [1] "Method 2: Median steps per day is:"
## [2] "10765"
```

## What is the average daily activity pattern?

Again, the numeration in the commented code is different than the numbers of this particular section.

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
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
# mean_by_interval==calc_mean_by_interval

# these do not work because of NAs; once cleaned of NAs, these will work
data_byinterval<-group_by(mydata,interval)
interval_mean<-summarize(data_byinterval,mean_steps=mean(steps))

# this is the plot
png(file='./instructions_fig/mean_interval.png',width=480,height=480)
with(mean_by_interval,plot(x=interval,y=steps,xlab='Interval #',
          ylab="Mean Steps During Interval",type="l",main="Mean # of Steps by Interval"))
dev.off()
```

```
## png 
##   2
```

```r
#
# 5. The 5-minute interval that, on average, contains the maximum number of steps
#
max_interval<-mean_by_interval$interval[which(mean_by_interval$steps==max(mean_by_interval$steps))]
the_max<-round(max(mean_by_interval$steps),3)
     
print(paste("Max is ",the_max," and occurs at interval ",max_interval))
```

```
## [1] "Max is  206.17  and occurs at interval  835"
```

```r
# max is 206.1698, interval is 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Actually, by just removing the days full on NA's, we do not need to impute any data into any intervals.  We in effect did the same in calculating the median and mean beforehand, so there is no effect on these values.  If some days were partially full of data, then imputing an average value for the missing values would probably increase the total mean of steps for a full day.


```r
#
# 6. Code to describe and show a strategy for imputing missing data
#
summary(mydata$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

```r
num_nas<-sum(is.na(mydata$steps))
dates_toremove<-stepsbydate$date[is.na(stepsbydate$total_steps)]
table(mydata$date[is.na(mydata$steps)])
```

```
## 
## 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10 
##        288        288        288        288        288        288 
## 2012-11-14 2012-11-30 
##        288        288
```

```r
print(paste("Number of NA's is ",num_nas))
```

```
## [1] "Number of NA's is  2304"
```

```r
print(paste("There are only ",length(dates_toremove)," days with NAs, and they are completely full"))
```

```
## [1] "There are only  8  days with NAs, and they are completely full"
```

```r
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
```

```
## png 
##   2
```

```r
mean2<-mean(stepsbydate2$total_steps)
median2<-median(stepsbydate2$total_steps)
print(paste("Mean from cleaned data = ",mean2))
```

```
## [1] "Mean from cleaned data =  10766.1886792453"
```

```r
print(paste("Median from cleaned data = ",median2))
```

```
## [1] "Median from cleaned data =  10765"
```

## Are there differences in activity patterns between weekdays and weekends?

We use the cleaned dataset for this part. From the beginning, when we loade the data, we had added a column variable with the 'weekend'/'weekday' factor.  The charts show that weekdays have a higher peak early in the day, but the mean steps during the weekend is higher throughout the day.  And the mean and median values are higher on the weekend. 


```r
#
# 8.Panel plot comparing the average number of steps taken per 5-minute interval 
#
# across weekdays and weekends; we 
weekdays_data<-filter(data_bydate2,(weekday>1&weekday<7))
weekend_data<-filter(data_bydate2,(weekday==1|weekday==7))

total_by_weekday <- aggregate(steps ~ date, weekdays_data, sum)
total_by_weekend <- aggregate(steps ~ date, weekend_data, sum)
stepmean_we<-mean(total_by_weekend$steps)
stepmedian_we<-median(total_by_weekend$steps)
stepmean_wd<-mean(total_by_weekday$steps)
stepmedian_wd<-median(total_by_weekday$steps)

print(c("Mean steps per weekday is:",round(stepmean_wd,3)))
```

```
## [1] "Mean steps per weekday is:" "10177.333"
```

```r
print(c("Median steps per weekday is:",round(stepmedian_wd,3)))
```

```
## [1] "Median steps per weekday is:" "10304"
```

```r
print(c("Mean steps per weekend is:",round(stepmean_we,3)))
```

```
## [1] "Mean steps per weekend is:" "12406.571"
```

```r
print(c("Median steps per weekend is:",round(stepmedian_we,3)))
```

```
## [1] "Median steps per weekend is:" "12130"
```

```r
data_byinterval_wd<-group_by(weekdays_data,interval)
interval_mean_wd<-summarize(data_byinterval_wd,mean_steps=mean(steps))
interval_mean_wd$type<-"Weekday"

data_byinterval_we<-group_by(weekend_data,interval)
interval_mean_we<-summarize(data_byinterval_we,mean_steps=mean(steps))
interval_mean_we$type<-"Weekend"
mrg<-rbind(interval_mean_we,interval_mean_wd)

png(file="./instructions_fig/weekend_vs_weekday2.png",width=480,height=480)
g<-ggplot(mrg,aes(x=interval,y=mean_steps),color=type) 
g+facet_grid(type~.)+geom_line()+labs(x="Interval",y="Mean Steps Per Interval",title="Mean Steps Comparison")

dev.off()
```

```
## png 
##   2
```

## Commit containing full submission

From the Coursera Project description, there are 9 requirements: 5 requirements for code (items 1,3,5,6, and 9) and 4 plots.  The comments in **analysis.R** use the numeration below, which don't exactly match the outline provided in the Rmd file. 

1. Code for reading in the dataset and/or processing the data (contained in **analysis.R**)
2. Histogram of the total number of steps taken each day
3. Mean and median number of steps taken each day
4. Time series plot of the average number of steps taken
5. The 5-minute interval that, on average, contains the maximum number of steps
6. Code to describe and show a strategy for imputing missing data (contained in **analysis.R**) 
7. Histogram of the total number of steps taken each day after missing values are imputed
8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report (contained in **analysis.R**)

