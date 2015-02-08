#Set working directory
setwd("~/Dropbox/data")
Explo_hmwk <- read.table("~/Dropbox/data/household_power_consumption.txt", sep=";",header=TRUE, quote="\"",
                         nrows=2075259,comment.char="", check.names=FALSE,stringsAsFactors=FALSE)

#Format $Date to Date and format $Global_active_power to numeric values.
Explo_hmwk$Date <- as.Date(Explo_hmwk$Date, format="%d/%m/%Y")
Explo_hmwk$Global_active_power<-as.numeric(Explo_hmwk$Global_active_power)

#Check for NAs
nas<-subset(Explo_hmwk,is.na(Global_active_power)==TRUE & Date >= "2007-02-01" & Date <= "2007-02-02")
nas
#We will only be using data from the dates 2007-02-01 and 2007-02-02.
Explo_hmwk <- subset(Explo_hmwk, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))

#Plot 1: use png() and save file
png(filename = "Plot1.png",
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,type = "quartz")
plot1<-hist(Explo_hmwk$Global_active_power, breaks=12, col="red", main="Global Active Power", 
            xlab="Global Active Power (kilowatts)") 
dev.off()
