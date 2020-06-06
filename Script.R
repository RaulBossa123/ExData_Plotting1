#read data
setwd("C:/Users/USUARIO/Desktop/Specialization/4 plots")
data = read.table("household_power_consumption.txt", sep = ";" , header = TRUE, na.strings = "?", dec=".")

#covert to date type
data$Date <- as.Date(data$Date,format = "%d/%m/%Y")

#subseting
data= subset(data,Date >= "2007-2-1" & Date <= "2007-2-2")

#remove NA
data = data[complete.cases(data),]

#combine date and time
TS = paste(data$Date, data$Time)
TS = setNames(TS, "TimeS")
data <- data[ ,!(names(data) %in% c("Date","Time"))]#remove date and time
data <- cbind(TS, data) #add TimeS vector
data$datatime <- as.POSIXct(TS)#convert to date and hour format


#plot 1

hist(as.numeric(data$Global_active_power), breaks = 16, col= "red", 
     xlab= "Global active power (kilowatts)", main = "Global active power")


png(file = "plot1.png", width = 480, height =480, units = "px") #export
hist(as.numeric(data$Global_active_power), breaks = 16, col= "red", 
     xlab= "Global active power (kilowatts)", main = "Global active power")

dev.off()

#plot 2

plot(data$Global_active_power~data$datatime, type="l", ylab="Global Active Power (kilowatts)",xlab = " ")

png(file = "plot2.png", width = 480, height =480, units = "px") #export
plot(data$Global_active_power~data$datatime, type="l", 
     ylab="Global Active Power (kilowatts)",xlab = " ")
dev.off()


#plot 3
attach(data)
with(data, {
        plot(Sub_metering_1~datatime, type="l",
             ylab="Global Active Power (kilowatts)", xlab="")
        lines(Sub_metering_2~datatime,col='Red')
        lines(Sub_metering_3~datatime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))


png(file = "plot3.png", width = 480, height =480, units = "px") #export
with(data, {
        plot(Sub_metering_1~datatime, type="l",
             ylab="Global Active Power (kilowatts)", xlab="")
        lines(Sub_metering_2~datatime,col='Red')
        lines(Sub_metering_3~datatime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()


#plot 4
## Create Plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(data, {
        plot(Global_active_power~datatime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
        plot(Voltage~datatime, type="l", ylab="Voltage (vol)", xlab="")
        plot(Sub_metering_1~datatime, type="l", 
             ylab="Global Active Power (kilowatts)", xlab="")
        lines(Sub_metering_2~datatime,col='Red')
        lines(Sub_metering_3~datatime,col='Blue')
        legend("topleft", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        plot(Global_reactive_power~datatime, type="l", 
             ylab="Global Rective Power (kilowatts)",xlab="")
})

png(file = "plot4.png", width = 480, height =480, units = "px") #export
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(data, {
        plot(Global_active_power~datatime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
        plot(Voltage~datatime, type="l", ylab="Voltage (vol)", xlab="")
        plot(Sub_metering_1~datatime, type="l", 
             ylab="Global Active Power (kilowatts)", xlab="")
        lines(Sub_metering_2~datatime,col='Red')
        lines(Sub_metering_3~datatime,col='Blue')
        legend("topleft", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        plot(Global_reactive_power~datatime, type="l", 
             ylab="Global Rective Power (kilowatts)",xlab="")
})
dev.off()

