#RepData_PeerAssessment1
#JHU - Reproducible Research
#Ray lugo, Jr.
#Purpose:  To create a literate program that can be used to provide the method used for data processing, analysis and
#visualizations in a form in which the research can be easily reproduced.

#prepare environment
library(tidyverse)
library(lattice)
library(knitr)
library(xtable)

#load data from zip
activity_data <- readr::read_csv(file = "data/activity.zip", col_names = TRUE, col_types = readr::cols(readr::col_integer(),
    readr::col_date(format = "%Y-%m-%d"),readr::col_integer()))


##Question 1: What is mean total number of steps taken per day?
### Make a data.frame for total number of steps per day (excluding NA.)
activity_data_no_NA <- filter(activity_data,!is.na(activity_data$steps))
total_steps_per_day <- activity_data_no_NA %>%
    select(date, steps) %>%
    group_by(date) %>%
    summarise(totalSteps = sum(steps))

### Plot histogram of steps per day.
numBins <- ceiling(sqrt(range(total_steps_per_day$totalSteps)))
#binSize = round_any((max(total_steps_per_day$totalSteps) - min(total_steps_per_day$totalSteps))/7,100, f = ceiling) #uses plyr
binSize <- 3100

plot_histogram_total_steps_per_day <- ggplot(data = total_steps_per_day, aes(x = totalSteps)) +
    geom_histogram(stat = "bin", bins = numBins, binwidth = binSize, color = "darkblue", fill = "steelblue") +
    labs(title = "Total Steps per Day", x = "Total Steps Taken", y = "Count") +
    ggsave(filename = "figures/histogram_total_steps_per_day.png", width = 5, height = 3)

plot_histogram_total_steps_per_day

###calculate the mean value of total steps
meanTotalSteps <- mean(total_steps_per_day$totalSteps)
print(paste("Mean total steps per day:", meanTotalSteps))

###calculate the mid value of total steps
midTotalSteps <- median(total_steps_per_day$totalSteps)
print(paste("Median total steps per day:", midTotalSteps))


##Question 2: What is the average daily activity pattern?
### Make a data.frame for mean steps taken per interval across all days
mean_steps_per_interval_all_days <- activity_data_no_NA %>%
    select(interval, steps) %>%
    group_by(interval) %>%
    summarise(meanSteps = mean(steps))

### Plot meanSteps by interval in time series plot
plot_time_series_mean_steps_per_interval <- ggplot(data = mean_steps_per_interval_all_days, aes(interval, meanSteps)) +
    geom_step(color = 'blue', lwd = 0.25) +
    scale_x_continuous(breaks = c(seq(0,2500,250)),minor_breaks = c(seq(0,2500,50))) +
    theme_bw() +
    labs(title = "Average Total Steps per Interval", x = "Interval", y = "Average Total Steps") +
    ggsave(filename = "figures/time_series_mean_steps_per_interval.png", width = 5, height = 3)

plot_time_series_mean_steps_per_interval

### plot vline at interval with max steps
interval_max_steps = mean_steps_per_interval_all_days %>%
    select(interval, meanSteps) %>%
    filter(meanSteps == max(meanSteps))

print(paste("Interval #",interval_max_steps$interval,"has the max average steps per day of", interval_max_steps$meanSteps))

plot_time_series_interval_max_steps = plot_time_series_mean_steps_per_interval +
    geom_vline(xintercept = interval_max_steps$interval, color = "red", lwd = 0.1, linetype = "longdash") +
    geom_point(x = interval_max_steps$interval, y = interval_max_steps$meanSteps, pch = 21, size = 0.5, color = "red") +
    labs(title = "Average Total Steps per Interval ", x = "Interval", y = "Average Total Steps") +
    geom_text(aes(x  = interval_max_steps$interval, y = 0), nudge_x = 200, size = 2, label = paste("Interval: ",interval_max_steps$interval), color = "red") +
    geom_text(aes(x  = interval_max_steps$interval, y = interval_max_steps$meanSteps), nudge_x = 200, size = 2, label = paste("Max:", round(interval_max_steps$meanSteps,2)), color = "red") +
    ggsave(filename = "figures/time_series_interval_max_steps.png", width = 5, height = 3)

plot_time_series_interval_max_steps

##Question 3: Imputing missing values
### Calculate and report the number of missing values
missing_activity_data <- filter(activity_data,is.na(activity_data$steps))
print(paste("Number of Observations missing data:", count(missing_activity_data)))

### Devise strategy to replace NA values with average steps value for each corresponding interval.
imputed_activity_data <- inner_join(missing_activity_data, mean_steps_per_interval_all_days, by = "interval") %>%
    select(steps = meanSteps, date,interval)

### Create new dataset that contains original with replaced NA observations.
revised_activity_data <- rbind(activity_data_no_NA, imputed_activity_data)

### Make a histogram of the total number of steps taken each day using new dataset.
revised_total_steps_per_day <- revised_activity_data %>%
    select(date, steps) %>%
    group_by(date) %>%
    summarise(totalSteps = sum(steps))

### Plot histogram of steps per day.
plot_histogram_revised_total_steps_per_day <- ggplot(data = revised_total_steps_per_day, aes(totalSteps)) +
    geom_histogram(stat = "bin", bins = numBins, binwidth = binSize, color = "darkred", fill = "red") +
    labs(title = "Revised Total Steps per Day", x = "Total Steps Taken", y = "Count") +
    ggsave(filename = "figures/histogram_revised_total_steps_per_day.png", width = 5, height = 3)

plot_histogram_revised_total_steps_per_day

###calculate the mean value of total steps
revisedMeanTotalSteps <- mean(revised_total_steps_per_day$totalSteps)
print(paste("Revised Mean total steps per day:", revisedMeanTotalSteps))

###calculate the mid value of total steps
revisedMidTotalSteps <- median(revised_total_steps_per_day$totalSteps)
print(paste("Revised Median total steps per day:", revisedMidTotalSteps))


### Do the values differ from the original mean and mid?
### Yes, the Median and the Mean are equal which means this is a symmetric distribution.

### What is the impact of imputing missing data on the estimates of the total daily number of steps?
### Calculating Mode to test a hunch
#getmode function was found here https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

revisedMode <- getmode(revised_total_steps_per_day$totalSteps)
print(paste("Revised Mode of averaged total steps per day:", revisedMode))
### The mid value is now equal to the mean value, so the slight skewness that was present initially has been corrected by replacing the missing values with the average total daily steps at each interval.
### Furthermore, the Mode = Median = Mean, which meets the criteria for a perfect normal distribution.
### With a normal distribution of interval data, the mean is usually the best measure of central tendency and is why we are using it for our calculatons.


##Question #4: Are there differences in activity patterns between weekdays and weekends?
### Create vectorized function to determine day of week using date and function "weekdays()" then return either "weekday" or "weekend".
determineDayType <- function(x) {
    dayOfWeek <- weekdays(x, FALSE)
    ifelse(is.element(dayOfWeek,c("Saturday","Sunday")),return("weekend"),return("weekday"))
}

vdetermineDayType <- Vectorize(determineDayType)

### Create and populate a new column in a new dataset with the revised data and the results of vdetermineDayType(date).
dayType_activity_data <- mutate(revised_activity_data, dayType = vdetermineDayType(revised_activity_data$date))

### make the column a factor with 2 levels.
dayType_activity_data$dayType <- as.factor(dayType_activity_data$dayType)

### Calculate average steps by dayType and interval
dayType_Mean_Total_Steps_by_Interval <- dayType_activity_data %>%
    group_by(dayType, interval) %>%
    summarise(meanSteps = mean(steps))

# plot_timeline_mean_by_interval_facet_dayType <- ggplot(data = dayType_Mean_Total_Steps_by_Interval, aes(interval, meanSteps)) +
#     geom_step(color = 'blue', lwd = 0.25) +
#     scale_x_continuous(breaks = c(seq(0,2500,250)),minor_breaks = c(seq(0,2500,50))) +
#     facet_grid(dayType ~ .) +
#     facet_wrap(facets, strip.position = "top") +
#     theme_bw() +
#     labs(title = "Average Steps per Interval by Day Type ", x = "Interval", y = "Average Total Steps") +
#     ggsave(filename = "figures/plot_timeline_mean_by_interval_facet_dayType.png", width = 5, height = 3)

### Plot using "lattice" to match example
time_series_mean_steps_interval_dayType <-  xyplot(meanSteps ~ interval|dayType, data = dayType_Mean_Total_Steps_by_Interval, type = "l", layout = c(1,2), xlab = "Interval", ylab = "Average Total Steps", main = "Average Total Steps per Interval by Day Type")
#plot(time_series_mean_steps_interval_dayType)
#note: lattice can only save to the current working directory (unless I missed something), so added steps to change to "figures" dir and then back.
currwd = getwd()
setwd(paste0(getwd(),"/figures"))
png(filename = "time_series_mean_steps_interval_dayType.png", units = "in", width  = 6, height = 5, res = 96)
print(time_series_mean_steps_interval_dayType)
dev.off()
setwd(currwd)
