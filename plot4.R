#Set working directory
setwd("~/Dropbox/data")
Explo_hmwk <- read.table("~/Dropbox/data/household_power_consumption.txt", sep=";",header=TRUE, quote="\"",
                         nrows=2075259,comment.char="", check.names=FALSE,stringsAsFactors=FALSE)

#Format $Date to Date and format $Global... & $Sub_metering... to numeric values.
Explo_hmwk$Date <- as.Date(Explo_hmwk$Date, format="%d/%m/%Y")
Explo_hmwk$Global_active_power<-as.numeric(Explo_hmwk$Global_active_power)
Explo_hmwk$Global_reactive_power<-as.numeric(Explo_hmwk$Global_reactive_power)
Explo_hmwk$Voltage<-as.numeric(Explo_hmwk$Voltage)
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

#Plot 4: create png file with four embedded plots
png(filename = "Plot4.png",
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,type = "quartz")
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(Explo_hmwk, {
        plot(Global_active_power~Date.Time, type="l",
             ylab="Global Active Power (kilowatts)", xlab="")
        plot(Voltage~Date.Time, type="l", 
             ylab="Voltage", xlab="datetime")
        plot(Sub_metering_1~Date.Time, type="l",col="black", ylab="Energy sub metering", xlab="")
                lines(Sub_metering_2~Date.Time,col='red') 
                lines(Sub_metering_3~Date.Time,col='blue')
                legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
                legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        plot(Global_reactive_power~Date.Time, type="l", xlab="datetime")
        
})
dev.off()


