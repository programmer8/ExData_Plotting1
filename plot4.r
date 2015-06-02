# 
# Since we are submitting code, I have written everything in function definitions.
# All four .r files are identical, with the exception of the function calls invoked
# at the end.  The 10 functions are:
#     
#     ReadSubsetData()        - Function to read, subset, and clean up required data
# 
#     ActivePowerHist()       - Function to generate an image required for Plot1
#     ActivePowerPlot()       - Function to generate an image required for Plot2 and Plot4
#     SubMeteringPlot()       - Function to generate an image required for Plot3 and Plot4
#     VoltagePlot()           - Function to generate an image required for Plot4
#     ReactivePowerPlot()     - Function to generate an image required for Plot4
# 
#     Plot1()                 - Wrapper function to read data and save plot to "plot1.png"
#     Plot2()                 - Wrapper function to read data and save plot to "plot2.png"
#     Plot3()                 - Wrapper function to read data and save plot to "plot3.png"
#     Plot4()                 - Wrapper function to read data and save plot to "plot4.png"
# 

ReadSubsetData <- function(zip.name = "../exdata_data_household_power_consumption.zip", 
                           file.name = "household_power_consumption.txt", 
                           date.range.start = as.Date("2007-02-01"), 
                           date.range.end = as.Date("2007-02-02")) {

    
    # Read some initial lines from file to get some necessary info
    data = read.table(unz(zip.name, file.name), 
                      header = TRUE, sep = ";", na.strings = "?", 
                      colClasses = "character", 
                      nrows = 2)

    date.line1 = as.Date(data[1,"Date"], format = "%d/%m/%Y")
    
    col.names = names(data)
    col.classes = rep.int("numeric", length(col.names))
    col.classes[which(col.names == "Date" | col.names == "Time")] = "character"
    
    
    # Re-read rows of interest from file - give approx. 1 day's buffer on each end
    buffer = 1500
    approx.start.line = as.integer(date.range.start - date.line1) * 24 * 60 - buffer
    approx.end.line = as.integer(date.range.end - date.line1) * 24 * 60 + buffer

    data = read.table(unz(zip.name, file.name), 
                      header = FALSE, sep = ";", na.strings = "?", 
                      colClasses = col.classes, 
                      col.names = col.names, 
                      nrows = approx.end.line - approx.start.line, skip = approx.start.line)

    
    # Clean up Date and Time columns into a single Date_time column
    datetime = strptime(paste(data$Date, data$Time, sep = " "), format = "%d/%m/%Y %H:%M:%S")
    data$Time = datetime
    names(data)[which(col.names == "Time")] = "Date_time"
    
    
    # Return data subset - no need for Date column
    dates = as.Date(datetime)
    data = data[(dates >= date.range.start & dates <= date.range.end) , - which(col.names == "Date")]
}

ActivePowerHist <- function(data) {
    
    hist(data$Global_active_power, col = "red", 
         xlab = "Global Active Power (kilowatts)", 
         main ="Global Active Power")
}

ActivePowerPlot <- function(data, label.units = TRUE) {
    
    ylabel = "Global Active Power"
    if (label.units) {
        ylabel = paste(ylabel, "(kilowatts)", collapse = " ")
    }
    
    plot(data$Date_time, data$Global_active_power, type = "l", 
         xlab = "", ylab = ylabel)
}

SubMeteringPlot <- function(data, legend.border = TRUE) {
    
    bty = "n"  # "none"
    if (legend.border) {
        bty = "o"
    }
    
    plot(data$Date_time, data$Sub_metering_1, type = "l", 
         xlab = "", ylab = "Energy sub metering")
    points(data$Date_time, data$Sub_metering_2, type = "l", col = "red")
    points(data$Date_time, data$Sub_metering_3, type = "l", col = "blue")
    legend("topright", lty = 1, 
           col = c("black", "red", "blue"), bty = bty, 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
}

VoltagePlot <- function(data) {
    
    plot(data$Date_time, data$Voltage, type = "l", 
         xlab = "datetime", ylab = "Voltage")
}

ReactivePowerPlot <- function(data) {
    
    plot(data$Date_time, data$Global_reactive_power, type = "l", 
         xlab = "datetime", ylab = "Global_reactive_power")
}


Plot1 <- function() {
    
    data = ReadSubsetData()
    
    png(filename = "plot1.png")
    ActivePowerHist(data)
    dev.off()
}

Plot2 <- function() {
    
    data = ReadSubsetData()
    
    png(filename = "plot2.png")
    ActivePowerPlot(data)
    dev.off()
}

Plot3 <- function() {
    
    data = ReadSubsetData()
    
    png(filename = "plot3.png")
    SubMeteringPlot(data)
    dev.off()
}

Plot4 <- function() {
    
    data = ReadSubsetData()
    
    png(filename = "plot4.png")
    par(mfcol = c(2, 2))
    
    ActivePowerPlot(data, FALSE)
    SubMeteringPlot(data, FALSE)
    VoltagePlot(data)
    ReactivePowerPlot(data)
    
    par(mfcol = c(1, 1))
    dev.off()
}


# Plot1()
# Plot2()
# Plot3()
Plot4()

