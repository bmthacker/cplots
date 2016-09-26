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

# Create a function to mung some data and plot a time series of Global active
# power vs. Time.
power.time <- function(){
      
      # Create a subset of only the Date, Time, and G.A.P variables.
      sub.df <- select(power.df, Date:Global_active_power)
      
      # Create a new variable called "DateTime" with the Date and Time
      # variables pasted together and convert the result to POSIXct.
      sub.df$DateTime <- strptime(paste(sub.df$Date,sub.df$Time),
                                  format = "%d/%m/%Y %H:%M:%S")
      
      # Get rid of old Date and Time variables leaving only DateTime
      # & G.A.P.
      sub.df <- select(sub.df, DateTime, Global_active_power)
     
      # Subset the data to comprise only the section of time required.
      sub.df <- subset(sub.df, DateTime >= as.POSIXct("2007-02-01 00:00:00")
                      & DateTime <= as.POSIXct("2007-02-02 23:59:59"))
      
      # Open .png graphic object.
      png(filename = "plot2.png")
      
      # Plot the remaining variables in sub.df as a time series to file.
      plot(sub.df$DateTime, sub.df$Global_active_power,
           type = "l",
           main = "Global Active Power Time Series",
           xlab = "Time (Days)",
           ylab = "Global Active Power (Kilowatts)")
      
      # uhh..... close .png
      dev.off()
     
     

}
      
      