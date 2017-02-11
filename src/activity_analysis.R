#Place holder for project info and comments
#
#
#
#

#prepare environment
library(tidyverse)
library(lubridate)
library(xtable)

#load data from zip
activity_data <- readr::read_csv(file = "data/activity.zip", col_names = TRUE, col_types = readr::cols(readr::col_integer(),readr::col_date(format = "%Y-%m-%d"),readr::col_integer()))


##Question 1: What is mean total number of steps taken per day?
### Make a data.frame for total number of steps per day (excluding NA.)
activity_data_no_NA <- filter(activity_data,!is.na(activity_data$steps))
total_steps_per_day <- activity_data_no_NA %>%
    select(date, steps) %>%
    group_by(date) %>%
    summarise(totalSteps = sum(steps))

### Plot histogram of steps per day.
plot_histogram_total_steps_per_day <- ggplot(data = total_steps_per_day, aes(date, totalSteps, fill = totalSteps)) +
    geom_bar(stat = "identity", color = "steelblue") +
    scale_x_date(date_labels = "%m/%d", date_minor_breaks = "1 day") +
    labs(title = "Total Steps per Day", x = "Date", y = "Steps") +
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
    labs(title = "Average Steps per Interval", x = "Interval", y = "Average Steps") +
    ggsave(filename = "figures/time_series_mean_steps_per_interval.png", width = 5, height = 3)

plot_time_series_mean_steps_per_interval

### plot vline at interval with max steps
interval_max_steps = mean_steps_per_interval_all_days %>%
    select(interval, meanSteps) %>%
    filter(meanSteps == max(meanSteps))

print(paste("Interval with max average steps per day:", interval_max_steps$interval))

plot_time_series_interval_max_steps = plot_time_series_mean_steps_per_interval +
    geom_vline(xintercept = interval_max_steps$interval, color = "red", lwd = 0.1, linetype = "longdash") +
    geom_text(aes(x  = interval_max_steps$interval, y = 0), nudge_x = 200, size = 3, label = paste("Interval: ",interval_max_steps$interval), color = "red") +
    ggsave(filename = "figures/time_series_interval_max_steps.png", width = 5, height = 3)

plot_time_series_interval_max_steps

##Question 3: Inputting missing values
### Calculate number of missing values
missing_activity_data <- filter(activity_data,is.na(activity_data$steps))
print(paste("Number of Observations missing data:", count(missing_activity_data)))

###
