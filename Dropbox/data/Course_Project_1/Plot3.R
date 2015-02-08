#Set working directory
setwd("~/Dropbox/data")
Explo_hmwk <- read.table("~/Dropbox/data/household_power_consumption.txt", sep=";",header=TRUE, quote="\"",
                         nrows=2075259,comment.char="", check.names=FALSE,stringsAsFactors=FALSE)

#Format $Date to Date and format $Global_active_power & $Sub_metering... to numeric values.
Explo_hmwk$Date <- as.Date(Explo_hmwk$Date, format="%d/%m/%Y")
Explo_hmwk$Global_active_power<-as.numeric(Explo_hmwk$Global_active_power)
Explo_hmwk$Sub_metering_1<-as.numeric(Explo_hmwk$Sub_metering_1)
Explo_hmwk$Sub_metering_2<-as.numeric(Explo_hmwk$Sub_metering_2)
Explo_hmwk$Sub_metering_3<-as.numeric(Explo_hmwk$Sub_metering_3)

#Check for NAs
nas<-subset(Explo_hmwk,is.na(Global_active_power)==TRUE & Date >= "2007-02-01" & Date <= "2007-02-02")
nas
#We will only be using data from the dates 2007-02-01 and 2007-02-02.
Explo_hmwk <- subset(Explo_hmwk, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))

#Convert $Dates and $Time to a formatted Date.Time entry
Date.Time <- paste(as.Date(Explo_hmwk$Date), Explo_hmwk$Time)
Explo_hmwk$Date.Time <- as.POSIXct(Date.Time)

#Plot 3: create png file
png(filename = "Plot3.png",
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,type = "quartz")
with(Explo_hmwk, {
        plot(Sub_metering_1~Date.Time, type="l",col="black", ylab="Energy sub metering", xlab="")
        lines(Sub_metering_2~Date.Time,col='red') 
        lines(Sub_metering_3~Date.Time,col='blue')
})
legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()

