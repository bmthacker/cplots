#===============================================================================
# This script takes data from the "household_power_consumption.txt" file and
# plots a time series from "2007-02-01 00:00:00" to "2007-02-02 23:59:59".
#===============================================================================
#           Auther: Brandon M. Thacker
#             Date: 2016-09-25
#===============================================================================


# Read in the data, Note: The format has sep = ";", thus .csv2.
power.df <- read.csv2("household_power_consumption.txt",
                      header = TRUE,
                      stringsAsFactors = FALSE,
                      na.strings = "?")

# Create a function to mung some data and plot a time series of three variables,
# Sub-metering1, sub-metering2 and sub-metering3.
plot.four <- function(){
      
      # Create a subset of only the Date, Time, G.A.P and metering variables.
      sub.df <- select(power.df,
                       Date, Time, 
                       Global_active_power,
                       Global_reactive_power,
                       Voltage,
                       Sub_metering_1,
                       Sub_metering_2,
                       Sub_metering_3)
      
      # Create a new variable called "DateTime" with the Date and Time
      # variables pasted together and convert the result to POSIXct.
      sub.df$DateTime <- strptime(paste(sub.df$Date,sub.df$Time),
                                  format = "%d/%m/%Y %H:%M:%S")
      
      # Get rid of old Date and Time variables leaving only DateTime
      # & G.A.P. and the metering variables.
      sub.df <- select(sub.df, DateTime,
                       Global_active_power,
                       Global_reactive_power,
                       Voltage,
                       Sub_metering_1,
                       Sub_metering_2,
                       Sub_metering_3)
      
      # Subset the data to comprise only the section of time required.
      sub.df <- subset(sub.df, DateTime >= as.POSIXct("2007-02-01 00:00:00")
                       & DateTime <= as.POSIXct("2007-02-02 23:59:59"))
      
      # Open .png graphic object.
      png(filename = "plot4.png")
      
      # Layout plot grid.
      par(mfcol = c(2,2))
      
      # Top left
      #-------------
      plot(sub.df$DateTime, sub.df$Global_active_power,
           type = "l",
           xlab = "Time (Days)",
           ylab = "Global Active Power (Kilowatts)")
      
      # Bottom left 
      #-------------
      # Plot the first sub-metering variable as a time series to graphic object.
      plot(sub.df$DateTime, sub.df$Sub_metering_1, col = "green",
           type = "l",
           ylab = "Energy sub-metering",
           xlab = "Time (days)")
      
      # Add the 2nd and 3rd metering variables.
      lines(sub.df$DateTime,sub.df$Sub_metering_2, col = "red")                                
      lines(sub.df$DateTime,sub.df$Sub_metering_3, col = "blue")
      
      # Create a legend.
      legend("topright", legend = c("Sub_metering_1",
                                    "Sub_metering_2",
                                    "Sub_metering_3"),
             
             col = c("green","red","blue"),
             lty = "solid",
             lwd = 2)
      
      # Top right
      #-------------
      plot(sub.df$DateTime, sub.df$Voltage,
           type = "l",
           xlab = "Time (Days)",
           ylab = "Voltage")
      
      # Bottom right
      #-------------
      plot(sub.df$DateTime, sub.df$Global_reactive_power,
           type = "l",
           xlab = "Time (Days)",
           ylab = "Global Reactive Power (Kilowatts)")
      
      # uhh..... close .png
      dev.off()
}