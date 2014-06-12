# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data.
file <- "repdata-data-activity.zip"
file.path(location,file)
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", file.path(location,file))
activity<-read.csv(unzip(file.path(location,file), "activity.csv",list=FALSE),header=TRUE)
head(activity)
activity$steps<-as.numeric(activity$steps)
sapply(activity,class)

## Create Histogram.
hist(activity$steps,xlab="Steps",main="Total Number of Steps Taken Each Day")

## What is mean number of steps?
mean(activity$steps,na.rm=TRUE)

[1] 37.3826

## What is median of steps?
median(activity$steps,na.rm=TRUE)

[1] 0

activity$date<-as.POSIXct(activity$date)

## Create time series plot.
plot(activity$interval,activity$steps,type="l",main="Time Series Plot of Number of Steps per Time Interval",xlab="Interval",ylab="Number of Steps")

## What is interval with max steps?
subset(activity,steps==max(activity$steps,na.rm=TRUE))

      steps       date interval
16492   806 2012-11-27      615

## What is total number of missing values?
sum(is.na(activity))

#[1] 2304

## Imputing missing values
NA_activity<-activity
NA_activity[is.na(NA_activity)] <- mean(activity$steps,na.rm=TRUE)

## Make histogram of new data.frame with filled in missing values.
hist(NA_activity$steps,xlab="Steps",main="Total Number of Steps Taken Each Day",)

## What is mean number of steps taken?
mean(NA_activity$steps,na.rm=TRUE)

[1] 37.3826

## What is median of steps taken?
median(NA_activity$steps,na.rm=TRUE)

[1] 0

## Are there differences in activity patterns between weekdays and weekends?
install.packages("ggplot2")
require(ggplot2)
NA_activity$days <- factor(ifelse(weekdays(NA_activity$date) == c("Saturday","Sunday"), "Weekends", "Weekdays"))
plot1<-ggplot(NA_activity, aes(x = interval, y = steps, group = days)) +
        geom_line() +
        xlab(expression(paste("Interval"))) +
        ylab(expression(paste("Number of Steps")))+
        labs(title="Total Number of Steps Taken on Weekdays vs. Weekends") +
        facet_wrap( ~ days,ncol=1)
print(plot1)

