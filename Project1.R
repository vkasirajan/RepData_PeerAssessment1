# Step 1
# Loading and preprocessing the data
# Clean up a little bit and set working directory.
# Load necessary libraries
rm(list = ls())
setwd("C:/Data Science/05 - Reproducible Research/project1/RepData_PeerAssessment1/")
library(ggplot2)

# Check and unzip the file
if (!file.exists("activity.csv")) {
    unzip("activity.zip")
}
# and then read it.
activityData <- read.csv("activity.csv", header = TRUE, comment.char = "")

# Step 2
# 1. Plot the total number of steps taken per day

totalSteps <- tapply(activityData$steps, activityData$date, FUN = sum, na.rm = TRUE)
qplot(totalSteps, binwidth = 1000, xlab = "Total number of steps taken each day")
dev.copy(png, "figure/TotalStepsTakenEachDay.png")
dev.off()

# 2. Calcuate the mean and median
mean(totalSteps, na.rm = TRUE)
median(totalSteps, na.rm = TRUE)


# Step 3
# 1. Plot Average Daily Acitivity Pattern

averageActivity <- aggregate(x = list(steps = activityData$steps), by = list(interval = activityData$interval),
                      FUN = mean, na.rm = TRUE)
ggplot(
    data = averageActivity, aes(x = interval, y = steps)) +
    geom_line() +
    xlab("Interval in 5 minutes") +
    ylab("Average number of steps taken")
dev.copy(png, "figure/AverageStepsTaken.png")
dev.off()

# 2. find out the maximum
averageActivity[which.max(averageActivity$steps),]


# Step 4
# 1. Find out how much data is missing
missingData <- is.na(activityData$steps)
table(missingData)

# 2. Find a method to replace missing values
missingValue <- function(steps, interval) {

    if ( is.na(steps)) {
        mV <- (averageActivity[averageActivity$interval == interval, "steps"])
    }
    else {
        mV <- c(steps)
    }
        
           
    return(mV)
}

# 3. Create a new Data Set

fullData <- activityData
fullData$steps <- mapply(missingValue, fullData$steps, fullData$interval)

# 4. Compute the graph and print mean and median
totalSteps <- tapply(fullData$steps, fullData$date, FUN = sum)
qplot(totalSteps, binwidth = 1000, xlab = "Total number of steps taken each day")

dev.copy(png, "figure/TotalStepsTakenAfterFillingMissingData.png")
dev.off()

mean(totalSteps)
median(totalSteps)

# Step 5
# activity pattern between week days and week ends

# 1. Create a new factor variable in the dataset with two levels � �weekday� and �weekend� indicating whether a given date is a weekday or weekend day.
checkWeekDayOrWeekEnd <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else 
        return("weekend")
}

fullData$date <- as.Date(fullData$date)
fullData$day <- sapply(fullData$date, FUN = checkWeekDayOrWeekEnd)

#2. Make the plot now
averageActivity <- aggregate(steps ~ interval + day, data = fullData, mean)
ggplot(averageActivity, aes(interval, steps)) +
    geom_line() +
    facet_grid(day ~ .) +
    xlab("Interval") +
    ylab("Number of steps taken")

dev.copy(png, "figure/ActivityPatternWeekDaysAndWeekEnds.png")
dev.off()