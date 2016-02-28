# Coursera
# Exploratory Data Analysis  --- Assignment 1 --  Panel Plot
# Leigh Matthews
# Feb 28, 2016


#### Dataset:  Electronic Power Consumption
# URL:  https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip

#### Description: 
# Measurements of electronic power consumption in one household with 
# a one-minute sampling rate over a period of almost 4 years.  
# Different electrical quantities and some sub-metering values are available.

#### Overall Goal: Examine how household energy usage varaies 
# over a 2-day period in February 2007 by reconstructing the 
#4 given plots using the base plotting system.

# Remember to set Working Directory and turn off devices

##################################################################################

#### Load the full household power consumption dataset then filter by dates

full_data_to_subset <- function(filepath) {

    # Read in the full data from the filepath parameter
    x <- read.table(filepath, sep=";", header=TRUE, na.strings="?")

    # Create new "datetime" variable by concatenateing Date and Time (type=POSIXlt)  
    x$datetime <- strptime(paste(x$Date, x$Time), "%d/%m/%Y %H:%M:%S")

    # Assign the desired dates to subset for the 2-day period [Feb 1, 2007 and Feb 2, 2007]
    dates <- c(as.Date("2007-02-01"), as.Date("2007-02-02"))

    # Output the subset of data for the 2-day period 
    x[as.Date(x$datetime) %in% dates,]
}

######################################################

setwd("C:/Users/Leigh/Documents/GitHub")

#### Create Plot 4: Four Panel Plot 

## It incorporates the previous 4 plots
## Note: Plots are not exactly the same as previous plots


# Four panel plot
panelplot <- function(x) {
    par(mfrow=c(2, 2))
    with(x, {
        ### Subplot 1 (top left)
        plot(datetime, Global_active_power, type="l", xlab="", ylab="Global Active Power")

        ### Subplot 2 (top right)
        plot(datetime, Voltage, type="l", xlab="datetime", ylab="Voltage")

        ### Subplot 3 (bottom left)
        plot(datetime, Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")

        colors <- c("black", "red", "blue")
        # Set variables to Sub_metering_1, Sub..._2, ..._3
        variables <- paste("Sub_metering_", 1:3, sep="")

        # Loop from i = 1 to 3 to draw lines for each variable i in corresponding color i 
        for (i in seq_along(variables)) {
            var <- variables[i] ## Name of variable 
            # Use double bracket notation to extract an entire column from
            # a data.frame by a name stored as in a string
            data <- x[[var]]
            lines(datetime, data, col=colors[i])
        }

        legend("topright", bty="n", ## Disable legend border, 
            legend=variables, col=colors, lty="solid" ## Draw solid lines            
         )

        ### Subplot 4 (bottom right)
        plot(datetime, Global_reactive_power, type="l")

    }) # end with command
}


x <- full_data_to_subset("./household_power_consumption.txt")

#### Copy to PNG file and set size
png("plot4.png", width=480, height=480)

# Run the function to get the line plot in the screen device
panelplot(x)

#### Turn off device
dev.off()
