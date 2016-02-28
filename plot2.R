# Coursera
# Exploratory Data Analysis  --- Assignment 1 -- Plot  Histogram
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

#### Create Plot 2: Line Plot of global active power with y-axis label 

lineplot <- function(x) {
    with(x, {
        plot(datetime, Global_active_power, type="l", xlab="",
            ylab="Global Active Power (kilowatts)")
    }) # ends the with command

}

x <- full_data_to_subset("./household_power_consumption.txt")

#### Copy to PNG file and set size
png("plot1.png", width=480, height=480)

# Run the function to get the histogram in the screen device
lineplot(x)

#### Turn off device
dev.off()
