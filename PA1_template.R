require(ggplot2)
require(knitr)
#Define destination of downloaded file, unzip, and then load data
location <- getwd()
file <- "repdata-data-activity.zip"
file.path(location,file)
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", file.path(location,file))
activity<-read.csv(unzip(file.path(location,file), "activity.csv",list=FALSE),header=TRUE)
head(activity)
activity$steps<-as.numeric(activity$steps)
sapply(activity,class)
#Create Histogram
hist(activity$steps,xlab="Steps",main="Total Number of Steps Taken Each Day",)
#What is mean of steps
mean(activity$steps,na.rm=TRUE)
#what is median of steps
median(activity$steps,na.rm=TRUE)
activity$date<-as.POSIXct(activity$date)
#ts(activity,start=1,frequency="interval")
#Create time series plot
plot(activity$interval,activity$steps,type="l",main="Time Series Plot of Number of Steps per Time Interval",xlab="Interval",ylab="Number of Steps")
#What is interval with max steps
subset(activity,steps==max(activity$steps,na.rm=TRUE))
#what is total number of missing values
sum(is.na(activity))
#Create data.frame of activity and then fill in NAs
NA_activity<-activity
NA_activity[is.na(NA_activity)] <- mean(activity$steps,na.rm=TRUE)
#Make histogram of NA_activity
hist(NA_activity$steps,xlab="Steps",main="Total Number of Steps Taken Each Day",)
#What is mean of steps
mean(NA_activity$steps,na.rm=TRUE)
#what is median of steps
median(NA_activity$steps,na.rm=TRUE)
#Difference bet weekend and weekdays
NA_activity$days <- factor(ifelse(weekdays(NA_activity$date) == c("Saturday","Sunday"), "Weekends", "Weekdays"))
plot1<-ggplot(NA_activity, aes(x = interval, y = steps, group = days)) +
        geom_line() +
        xlab(expression(paste("Interval"))) +
        ylab(expression(paste("Number of Steps")))+
        labs(title="Total Number of Steps Taken on Weekdays vs. Weekends") +
        facet_wrap( ~ days,ncol=1)
print(plot1)
