# Clean up a little bit and set working directory.
# Load necessary libraries
rm(list = ls())
setwd("C:/Data Science/05 - Reproducible Research/project1/RepData_PeerAssessment1/")

# Loading and preprocessing the data
if (!file.exists("activity.csv")) {
    unzip("activity.zip")
}

activityData <- read.csv("activity.csv", header = TRUE, comment.char = "")

# mean and median of the total number of steps taken per day
library(ggplot2)
totalSteps <- tapply(activityData$steps, activityData$date, FUN = sum, na.rm = TRUE)
qplot(totalSteps, binwidth = 1000, xlab = "Total number of steps taken each day")
mean(totalSteps, na.rm = TRUE)
median(totalSteps, na.rm = TRUE)

dev.copy(png, "figure/TotalStepsTakenEachDay.png")
dev.off()



