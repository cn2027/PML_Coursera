#Load potentially necessary packages
require(RCurl);require(data.table);require(sqldf);require(RSQLite);require(jpeg);require(data.table);library(quantmod);require(ggplot2);library(scales);require(knitr)


#Define destination of downloaded file, unzip, and then load data
location <- getwd()
file <- "repdata-data-activity.zip"
file.path(location,file)
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", file.path(location,file),method="curl")
activity<-read.csv(unzip(file.path(location,file), "activity.csv",list=FALSE),header=TRUE)
head(activity)
activity$steps<-as.numeric(activity$steps)
sapply(activity,class) 


#Create histogram of total number of steps taken per day
pdf("histogram1.pdf")
qplot(tapply(activity$steps, activity$date, sum), xlab='Total Number of Steps Taken Each Day', ylab='Frequency',main="Total Number of Steps Taken Each Day")
dev.off()

#What is mean and median number of steps
mean.steps<-mean(activity$steps,na.rm=TRUE)
median(activity$steps,na.rm=TRUE)


#Create time series plot for average steps per day
activity$date<-as.POSIXct(activity$date)
activity$time <- as.POSIXct(format(as.POSIXct(paste(activity$date, formatC(activity$interval / 100, 2, 										format='f')),format='%Y-%m-%d %H.%M', tz='GMT'),format='%H:%M:%S'),format='%H:%M:%S')
activity$mean.time<-tapply(activity$steps, activity$time, mean,na.rm=TRUE)
pdf("Time_series1.pdf")
ggplot(activity, aes(time, mean.time)) + 
    		geom_line() + xlab('Time of day (5-Minute Intervals)') + ylab('Mean number of steps') + 
    		scale_x_datetime(labels=date_format(format='%H:%M'))
dev.off()

#What is interval with max steps and total number of missing values
subset(activity,steps==max(activity$steps,na.rm=TRUE)) 
sum(is.na(activity))


#Make histogram of total steps per day with imputed means
NA_activity<-activity
NA_activity[is.na(NA_activity)] <- mean.steps
pdf("histogram2.pdf")
qplot(tapply(NA_activity$steps, activity$date, sum),xlab="Steps",main="Total Number of Steps Taken Each Day (with Imputed Means)",ylab='Frequency')
dev.off()

#What is mean and median of steps without missing values
mean(NA_activity$steps,na.rm=TRUE)
median(NA_activity$steps,na.rm=TRUE)


#Difference between weekends and weekdays
NA_activity$days <- factor(ifelse(weekdays(NA_activity$date) == c("Saturday","Sunday"), "Weekends", "Weekdays"))


#Plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
pdf("Time_series2.pdf")
plot1<-ggplot(NA_activity, aes(x = interval, y = steps, group = days)) +
        geom_line() +
        xlab(expression(paste("Interval"))) +
        ylab(expression(paste("Number of Steps")))+
        #scale_x_datetime(labels=date_format(format='%H:%M')+
        labs(title="Total Number of Steps Taken on Weekdays vs. Weekends") +
        facet_wrap( ~ days,ncol=1)
plot1
dev.off()

