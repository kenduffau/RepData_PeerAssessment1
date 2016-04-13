## Unzip and Load the activity data
unzip("C://Program Files/Git/Projects/DataScientistsToolbox/RepData_PeerAssessment1/activity.zip")
activityDataRaw <- read.csv("activity.csv", stringsAsFactors = FALSE)

## Ensure date variable is data class
activityDataRaw$date <- as.Date(activityDataRaw$date)

## Aggregate by date and sum step counts.
dailySteps <- aggregate(steps ~ date, data = activityDataRaw, sum, na.action = NULL)

## Plot histogram of frequency of daily step count. Copy to file and return dev to "off"
hist(dailySteps$steps, xlab = "Daily Steps", ylab = "Number of Days", main = "Daily Steps Frequency", col = "red")
dev.copy(png, "StepFrequencyCount.png")
dev.off()

## Calculate and store mean/median of data. Does not count days with NA.
avgDailySteps <- mean(dailySteps$steps, na.rm = TRUE)
medDailySteps <- as.numeric(median(dailySteps$steps, na.rm = TRUE))

## Output results
avgDailySteps
medDailySteps

## Calculate mean steps per 5 minute interval for all days
intervalSteps <- aggregate(steps ~ interval, data = activityDataRaw, mean)

## Load ggplot2. Plot mean steps per day. Output to PNG.
library(ggplot2)
plotInterval <- ggplot(intervalSteps, aes(intervalSteps$interval, intervalSteps$steps))
plotInterval + geom_line(col = "red") + 
    labs(x = "Intervals", y = "Avg # of Steps", title = "Average Number of Steps by Interval")

ggsave("AvgStepsByInterval.png", plot = last_plot())
dev.off()

## Calculate and store interval with maximum avg number of steps
maxIntervalSteps <- intervalSteps[intervalSteps$steps == max(intervalSteps$steps),]

## Output result
maxIntervalSteps

## Calculate number of rows with NA values
naCheck <- is.na(activityDataRaw$steps)
sum(naCheck)

## Impute missing values. 
## Create separate table, bind previous interval step data frame, and rename bound columns
imputedActivityData <- activityDataRaw
imputedActivityData <- cbind(activityDataRaw,intervalSteps)
names(imputedActivityData) <- c("steps", "date", "interval", "imputeInt", "imputeSteps")

## Impute values with transform and ifelse function, replacing NA with imputeSteps value
imputedActivityData = transform(imputedActivityData, steps = ifelse(is.na(steps), imputeSteps, steps))

## Validate NA values have been imputed
naImputeCheck <- is.na(imputedActivityData$steps)
sum(naImputeCheck)

## Aggregate imputed data frame by date and sum step counts.
dailyStepsImputed <- aggregate(steps ~ date, data = imputedActivityData, sum)

## Plot histogram of frequency of daily step count. Copy to file and return dev to "off"
hist(dailyStepsImputed$steps, xlab = "Daily Steps", ylab = "Number of Days", main = "Daily Steps Frequency", col = "red")
dev.copy(png, "StepFrequencyCountImputed.png")
dev.off()

## Calculate and store mean/median of imputed data
avgDailyStepsImputed <- mean(dailyStepsImputed$steps)
medDailyStepsImputed <- as.numeric(median(dailyStepsImputed$steps))

## Output results
avgDailyStepsImputed
medDailyStepsImputed

## Add weekday variable to imputed data frame
imputedActivityData$wkday <- weekdays(imputedActivityData$date)

## Create character vector to use for setting up weekday/weekend factors
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

## Create factor levels and then aggregate complete, imputed data into new data frame.
imputedActivityData$wkday <- factor(imputedActivityData$wkday %in% weekdays1, levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
intervalImputedSteps <- aggregate(steps ~ wkday + interval, data = imputedActivityData, mean)

## Plot weekday and weekend interval averages

plotInterval <- ggplot(intervalImputedSteps, aes(interval, steps))
plotInterval + geom_line(col = "red") + 
    facet_grid(. ~ wkday) +
    labs(x = "Intervals", y = "Avg # of Steps", title = "Average Number of Steps by Interval")

## Print plot and close graphic device
ggsave("AvgStepsByImputedInterval.png", plot = last_plot())
dev.off()

## End of script
