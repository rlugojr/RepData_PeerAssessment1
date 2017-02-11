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
### Make a vector for number of steps (excluding NA.)
total_steps_per_day <- activity_data %>%
    filter(!is.na(steps)) %>%
    select(date, steps) %>%
    group_by(date) %>%
    summarise(totalSteps = sum(steps))

### Plot histogram of steps per day.
ggplot(data = total_steps_per_day, aes(date, totalSteps, fill = totalSteps)) +
    geom_bar(stat = "identity", color = "steelblue") +
    scale_x_date(date_labels = "%m/%d", date_minor_breaks = "1 day") +
    labs(title = "Total Steps per Day", x = "Date", y = "Steps")

#calculate the mean value of total steps
meanTotalSteps <- mean(total_steps_per_day$totalSteps)
meanTotalSteps

#calculate the mid value of total steps
midTotalSteps <- median(total_steps_per_day$totalSteps)
midTotalSteps
