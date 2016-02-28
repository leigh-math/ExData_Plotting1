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

#### Create Plot 3: Line Plot Sub_metering_{1,2,3} in {using 
# {black, red, blue} colors, with y-axis label and legend 
# Uses colored lines where type = 1


lineplot_sub <- function(x) {
    with(x, {
        # Create the plot initialization.
        plot(datetime, Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")

        # Set the colors
        colors <- c("black", "red", "blue")
        # Set the variables to Sub_metering_1, _2 and _3
        variables <- paste0("Sub_metering_", 1:3)

        # Loop from i = 1 to 3 and draw the lines each variable
        # (variables[i]) in the appropriate color (colors[i])
        for (i in seq_along(variables)) {
            var <- variables[i] ## Name of the variable converting to string
            # Double bracket notation extracts entire dataframe column by name
            data <- x[[var]]
            lines(datetime, data, col=colors[i])
        }

        legend("topright",
            legend=variables, ## Name items in the legend
            col=colors, ## Set legend colors explicitly
            lty="solid" ## Draw solid lines next to each entry
            )
    }) # ends the with command

}

x <- full_data_to_subset("./household_power_consumption.txt")

#### Copy to PNG file and set size
png("plot3.png", width=480, height=480)

# Run the function to get the line plot in the screen device
lineplot_sub(x)

#### Turn off device
dev.off()
