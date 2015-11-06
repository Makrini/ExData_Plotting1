##  This is the R file required to create Plot 1 for Course Project 1 of 
##    Exploratory Data Analysis
##  I have formatted this into functions as I reuse the extracting and 
##    formatting part in all of the plots.  When creating these they were
##    all in the one R file and I have only split due to assignment requirements


##  GetPlottingData function extracts the data from the required txt file, and
##     then extracts sub_data for the days required.  It then formats as per
##     needs for plotting the graphs.
##  As my computer has sufficient memory I am extracting the entire data set
##     for efficiency purposes and then removing that set from memory once
##     the sub data is extracted
##  I have also cached the sub_data so as to not require extracting and formatting
##     again for subsequent graphs
GetPlottingData <- function ()
{  
        
        if (exists("sub_data")) {
                return(1)
        }
        
        oldWD <- getwd
        setwd("C:/Users/nrablin/Desktop/Coursera/ExData_Plotting/ExData_Plotting1/")
        dta <-
                read.table(
                        "./data/household_power_consumption.txt", header = TRUE, sep = ";"
                )
        sub_data <-
                subset(dta[(dta$Date == "1/2/2007" | dta$Date == "2/2/2007"),])
        
        rm(dta)  ## Clean out the large data to help keep memory usage low
        
        sub_data <-
                cbind(sub_data,DateTime = as.POSIXct(paste(sub_data$Date, sub_data$Time), format = "%d/%m/%Y %H:%M:%S"))
        
        sub_data$Global_active_power <- as.numeric(levels(sub_data$Global_active_power)[sub_data$Global_active_power])
        sub_data$Global_reactive_power <- as.numeric(levels(sub_data$Global_reactive_power)[sub_data$Global_reactive_power])
        sub_data$Voltage <- as.numeric(levels(sub_data$Voltage)[sub_data$Voltage])
        sub_data$Sub_metering_1 <- as.numeric(levels(sub_data$Sub_metering_1)[sub_data$Sub_metering_1])
        sub_data$Sub_metering_2 <- as.numeric(levels(sub_data$Sub_metering_2)[sub_data$Sub_metering_2])
        
        
        
        message("Data extract Complete")
        
        sub_data <<- sub_data
        
}

## Creates the required Plot
##   NOTE:- The example images forked appear to be 504 by 504 where the images
##      I have supplied are 480 by 480 as per instruction on the Coursera site
CreatePlot1 <- function() {
        GetPlottingData()  ## Get the Formatted data
        message("Plotting Graph")
        png("plot1.png", width = 480, height = 480)
        par(mfrow=c(1,1))
        hist(
                sub_data$Global_active_power,col = "red"
                ,main = "Global Active Power", xlab = "Global Active Power (kilowatts)",ylab = "Frequency"
        )
        dev.off()
}




