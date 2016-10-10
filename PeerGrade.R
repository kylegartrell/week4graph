library(plyr)
library(dplyr)
library(data.table)
library(lattice)

file.info("C:/Users/Kyle/Downloads/exdata%2Fdata%2Fhousehold_
power_consumption/household_power_consumption.txt")

fpath <- "C:/Users/Kyle/Downloads/exdata%2Fdata%2Fhousehold_power_consumption/household_power_consumption.txt"
setwd("C:/Users/Kyle/Downloads/exdata%2Fdata%2Fhousehold_power_consumption")

dataP <- read.table(fpath, sep=";", header=TRUE,stringsAsFactors = FALSE)
dataP[dataP == "?"] <- NA 
View(dataP)

str(dataP)
DatewTime <- strptime(paste(dataP$Date, dataP$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
dataP <- mutate(dataP, Date = as.Date(dataP$Date,"%d/%m/%Y"))
dataP <- cbind(DatewTime, dataP)


dataP <- subset(dataP, Date >= "2007-02-01" & Date <= "2007-02-02")
dataP <- mutate(dataP, Global_active_power = as.numeric(dataP$Global_active_power))
gap <- dataP$Global_active_power
dt <- dataP$DatewTime
subm1 <- as.numeric(dataP$Sub_metering_1)
subm2 <- as.numeric(dataP$Sub_metering_2)
subm3 <- as.numeric(dataP$Sub_metering_3)
volt <- as.numeric(dataP$Voltage)
grp <- as.numeric(dataP$Global_reactive_power)


#Plot1
with(dataP, hist(gap, col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)", ylab = "Frequency"))
par(bg="White")
dev.copy(png, file="plot1.png")
dev.off()

#Plot2
with(dataP,plot(dt,gap, type="l", xlab="",ylab="Global Active Power (kilowatts)"))
par(bg="White")
dev.copy(png, file="plot2.png")
dev.off()
length(subm)

#Plot3
with(dataP,plot(dt,subm1, type="l", xlab="",ylab="Energy sub metering", col="green"))
lines(dt,subm2, col="red")
lines(dt,subm3, col="blue")
legend("topright",lty=c(1,1), col=c("green","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
par(bg="Transparent")
dev.copy(png,file="plot3.png")
dev.off()

#Plot4
par(mfrow=c(2,2))
plot(dt,gap, type="l", xlab="",ylab="Global Active Power")
plot(dt,volt, type="l", xlab="datetime",ylab="Voltage")
plot(dt,subm1, type="l", xlab="",ylab="Energy sub metering", col="green")
lines(dt,subm2, col="red")
lines(dt,subm3, col="blue")
legend("topright",lty=c(1,1), cex=.75, col=c("green","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
plot(dt,grp, xlab="datetime",type="l", lwd=.5, ylab="Global Reactive Power")

par(bg="white")
dev.copy(png,file="plot4.png")
dev.off()






