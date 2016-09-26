power.df <- read.csv2("household_power_consumption.txt",
                      header = TRUE,
                      stringsAsFactors = FALSE,
                      na.strings = "?")


plot.hist <- function(){
      
      
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
      
      
      
      #clean.p.df <- as.numeric(na.omit(power.df$Global_active_power))

      hist.data = hist(as.numeric(sub.df$Global_active_power), plot=F)

      #hist.data$counts = hist.data$counts/sum(hist.data$counts)
      #dev.new(4,5)
      png(filename = "plot1.png")
      plot(hist.data,
           #freq = TRUE,
           col = "light blue",
           ylab= "Frequency",
           main = "Global Active Power",
           xlab = "Gobal Active Power (Kilowatts)"
           #xlim = c(0,6),
           #ylim = c(0,0.5)
           )
      dev.off()
}