# Reproducible Research: Assessment 1
Ray Lugo, Jr.  
12 Feb 2017  


##Purpose

To create a literate program that will document the methods used for data processing, analysis and visualization, provided in a form through which the research can be easily understood and reproduced.

##Prepare environment
###Load libraries.
The libraries used to create this literate program are : Tidyverse (includes Tidyr, dplyr, ggplot2, etc,) for data preparation and visualization, Lattice for additional visualizations, XTable to facilitate the display tables in this document.  Also, a custom CSS file (included in the repo) was used to modify the style for tables and other HTML elements.


```r
library(tidyverse)
library(lattice)
library(knitr)
library(xtable)
```



###Load data.
The data was loaded directly from ZIP archive using Readr's read_csv function.  This simplified data loading and saves disk space by eliminating the need to extract the data file.


```r
activity_data <- readr::read_csv(file = "data/activity.zip", col_names = TRUE,
    col_types = readr::cols(readr::col_integer(),
    readr::col_date(format = "%Y-%m-%d"),readr::col_integer()))
```

##Question 1: What is the mean total number of steps taken per day?
The first step towards answering this question requires a subset of the data which excludes any incomplete observations (excluding NA) and returns the sum of steps by date.


```r
activity_data_no_NA <- filter(activity_data,!is.na(activity_data$steps))
total_steps_per_day <- activity_data_no_NA %>%
    select(date, steps) %>%
    group_by(date) %>%
    summarise(totalSteps = sum(steps))
```

<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Mon Feb 13 23:44:40 2017 -->
<table border=1>
<caption align="bottom"> total_steps_per_day </caption>
<tr> <th>  </th> <th> date </th> <th> totalSteps </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> 2012-10-02 </td> <td align="right"> 126 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> 2012-10-03 </td> <td align="right"> 11352 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> 2012-10-04 </td> <td align="right"> 12116 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> 2012-10-05 </td> <td align="right"> 13294 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> 2012-10-06 </td> <td align="right"> 15420 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> 2012-10-07 </td> <td align="right"> 11015 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> 2012-10-09 </td> <td align="right"> 12811 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> 2012-10-10 </td> <td align="right"> 9900 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> 2012-10-11 </td> <td align="right"> 10304 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> 2012-10-12 </td> <td align="right"> 17382 </td> </tr>
   </table>



Now to plot the histogram of the **Total Steps per Day (totalSteps)** to visualize the distribution.  To determine the number of bins which may be optimal for analysis, I've used the square root of the range of the variable, applying the ceiling method of rounding in order to get a whole number that is larger than the variable's max value.

The bin-size may have required a custom function or locating an existing package, however since we only had one Histogram I chose to divide the max value by the number of bins previously calculated and the result was rounded up to the next hundred, resulting in a bin width = 3,100.


```r
numBins <- 1 + ceiling(sqrt(range(total_steps_per_day$totalSteps)))
binSize <- 3100

plot_histogram_total_steps_per_day <- ggplot(data = total_steps_per_day, aes(x = totalSteps)) +
    geom_histogram(stat = "bin", bins = numBins, binwidth = binSize, color = "darkblue", fill = "steelblue") +
    labs(title = "Total Steps per Day", x = "Total Steps Taken", y = "Count") +
    ggsave(filename = "figures/histogram_total_steps_per_day.png", width = 5, height = 3)

plot_histogram_total_steps_per_day
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Although we can visually identify that the bins with the largest number of values are in the center of the graph, those 2 bins can account for up to 6,200 values so it is highly unlikely that we can guess the exact value at the center of the distribution. We can, however, determine the central value by calculating the measures of central tendency.

The Mean and Median values are measures of central tendency and comparing the two would allow us to gauge the strength or weakness of the central tendency for this distribution. This being the latter case, the comparison of Mean and Median values would provide us with the size and direction of skew.  As the skew increases, so does variablity in the data set and the chance of finding a valid center, preventing it from being a symmetrical distribution.

First, calculate the **Mean** value of **totalSteps**.


```r
meanTotalSteps <- mean(total_steps_per_day$totalSteps)
print(paste("Mean total steps per day:", meanTotalSteps))
```

```
## [1] "Mean total steps per day: 10766.1886792453"
```

Then, calculate the **Median** value of **totalSteps**.


```r
###calculate the mid value of total steps
midTotalSteps <- median(total_steps_per_day$totalSteps)
print(paste("Median total steps per day:", midTotalSteps))
```

```
## [1] "Median total steps per day: 10765"
```

*There is a very slight difference between the Mean and Median which represents a slight skew.*

##Question 2: What is the average daily activity pattern?

Create a subset from the original dataset, minus the incomplete observations, and group the subset by interval, calculating the average of the number of steps in each interval.  The result is  a dataset with interval and meanSteps, across all date values. Date is not a grouping in this set, it is ignored in order to focus on the relationship of the interval and average of the number of steps.


```r
mean_steps_per_interval_all_days <- activity_data_no_NA %>%
    select(interval, steps) %>%
    group_by(interval) %>%
    summarise(meanSteps = mean(steps))
```

<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Mon Feb 13 23:44:41 2017 -->
<table border=1>
<caption align="bottom"> mean_steps_per_interval_all_days </caption>
<tr> <th>  </th> <th> interval </th> <th> meanSteps </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right">   0 </td> <td align="right"> 1.72 </td> </tr>
  <tr> <td align="right"> 2 </td> <td align="right">   5 </td> <td align="right"> 0.34 </td> </tr>
  <tr> <td align="right"> 3 </td> <td align="right">  10 </td> <td align="right"> 0.13 </td> </tr>
  <tr> <td align="right"> 4 </td> <td align="right">  15 </td> <td align="right"> 0.15 </td> </tr>
  <tr> <td align="right"> 5 </td> <td align="right">  20 </td> <td align="right"> 0.08 </td> </tr>
  <tr> <td align="right"> 6 </td> <td align="right">  25 </td> <td align="right"> 2.09 </td> </tr>
  <tr> <td align="right"> 7 </td> <td align="right">  30 </td> <td align="right"> 0.53 </td> </tr>
  <tr> <td align="right"> 8 </td> <td align="right">  35 </td> <td align="right"> 0.87 </td> </tr>
  <tr> <td align="right"> 9 </td> <td align="right">  40 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td align="right">  45 </td> <td align="right"> 1.47 </td> </tr>
   </table>

Create the timeseries plot using the interval as the x value and the meanSteps as the continuous y value.


```r
plot_time_series_mean_steps_per_interval <- ggplot(data = mean_steps_per_interval_all_days, aes(interval, meanSteps)) +
    geom_step(color = 'blue', lwd = 0.25) +
    scale_x_continuous(breaks = c(seq(0,2500,250)),minor_breaks = c(seq(0,2500,50))) +
    theme_bw() +
    labs(title = "Average Total Steps per Interval", x = "Interval", y = "Average Total Steps") +
    ggsave(filename = "figures/time_series_mean_steps_per_interval.png", width = 5, height = 3)

plot_time_series_mean_steps_per_interval
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Find the interval with the max average steps per day


```r
interval_max_steps = mean_steps_per_interval_all_days %>%
    select(interval, meanSteps) %>%
    filter(meanSteps == max(meanSteps))

print(paste("Interval #",interval_max_steps$interval,"has the max average steps per day of", interval_max_steps$meanSteps))
```

```
## [1] "Interval # 835 has the max average steps per day of 206.169811320755"
```

plot vline at interval with max steps


```r
plot_time_series_interval_max_steps = plot_time_series_mean_steps_per_interval +
    geom_vline(xintercept = interval_max_steps$interval, color = "red", lwd = 0.1, linetype = "longdash") +
    geom_point(x = interval_max_steps$interval, y = interval_max_steps$meanSteps, pch = 21, size = 2, color = "red") +
    labs(title = "Average Total Steps per Interval ", x = "Interval", y = "Average Total Steps") +
    geom_text(aes(x  = interval_max_steps$interval, y = 0), nudge_x = 200, size = 4, label = paste("Interval: ",interval_max_steps$interval), color = "red") +
    geom_text(aes(x  = interval_max_steps$interval, y = interval_max_steps$meanSteps), nudge_x = 200, size = 4, label = paste("Max:", round(interval_max_steps$meanSteps,2)), color = "red") +
    ggsave(filename = "figures/time_series_interval_max_steps.png", width = 5, height = 3)

plot_time_series_interval_max_steps
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Question 3: Imputing missing values
Calculate and report the number of missing values


```r
missing_activity_data <- filter(activity_data,is.na(activity_data$steps))
print(paste("Number of Observations missing data:", count(missing_activity_data)))
```

```
## [1] "Number of Observations missing data: 2304"
```

<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Mon Feb 13 23:44:42 2017 -->
<table border=1>
<caption align="bottom"> missing_activity_data </caption>
<tr> <th>  </th> <th> steps </th> <th> date </th> <th> interval </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">   0 </td> </tr>
  <tr> <td align="right"> 2 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">   5 </td> </tr>
  <tr> <td align="right"> 3 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  10 </td> </tr>
  <tr> <td align="right"> 4 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  15 </td> </tr>
  <tr> <td align="right"> 5 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  20 </td> </tr>
  <tr> <td align="right"> 6 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  25 </td> </tr>
  <tr> <td align="right"> 7 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  30 </td> </tr>
  <tr> <td align="right"> 8 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  35 </td> </tr>
  <tr> <td align="right"> 9 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  40 </td> </tr>
  <tr> <td align="right"> 10 </td> <td align="right">  </td> <td> 2012-10-01 </td> <td align="right">  45 </td> </tr>
   </table>

Devise strategy to replace NA values with average steps value for each corresponding interval.


```r
imputed_activity_data <- inner_join(missing_activity_data, mean_steps_per_interval_all_days, by = "interval") %>%
    select(steps = meanSteps, date,interval)
```

<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Mon Feb 13 23:44:42 2017 -->
<table border=1>
<caption align="bottom"> imputed_activity_data </caption>
<tr> <th>  </th> <th> steps </th> <th> date </th> <th> interval </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 1.72 </td> <td> 2012-10-01 </td> <td align="right">   0 </td> </tr>
  <tr> <td align="right"> 2 </td> <td align="right"> 0.34 </td> <td> 2012-10-01 </td> <td align="right">   5 </td> </tr>
  <tr> <td align="right"> 3 </td> <td align="right"> 0.13 </td> <td> 2012-10-01 </td> <td align="right">  10 </td> </tr>
  <tr> <td align="right"> 4 </td> <td align="right"> 0.15 </td> <td> 2012-10-01 </td> <td align="right">  15 </td> </tr>
  <tr> <td align="right"> 5 </td> <td align="right"> 0.08 </td> <td> 2012-10-01 </td> <td align="right">  20 </td> </tr>
  <tr> <td align="right"> 6 </td> <td align="right"> 2.09 </td> <td> 2012-10-01 </td> <td align="right">  25 </td> </tr>
  <tr> <td align="right"> 7 </td> <td align="right"> 0.53 </td> <td> 2012-10-01 </td> <td align="right">  30 </td> </tr>
  <tr> <td align="right"> 8 </td> <td align="right"> 0.87 </td> <td> 2012-10-01 </td> <td align="right">  35 </td> </tr>
  <tr> <td align="right"> 9 </td> <td align="right"> 0.00 </td> <td> 2012-10-01 </td> <td align="right">  40 </td> </tr>
  <tr> <td align="right"> 10 </td> <td align="right"> 1.47 </td> <td> 2012-10-01 </td> <td align="right">  45 </td> </tr>
   </table>

Create new dataset that contains original with replaced NA observations.


```r
revised_activity_data <- rbind(activity_data_no_NA, imputed_activity_data)
```

<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Mon Feb 13 23:44:42 2017 -->
<table border=1>
<caption align="bottom"> revised_activity_data </caption>
<tr> <th>  </th> <th> steps </th> <th> date </th> <th> interval </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 117.00 </td> <td> 2012-10-02 </td> <td align="right"> 2210 </td> </tr>
  <tr> <td align="right"> 2 </td> <td align="right"> 9.00 </td> <td> 2012-10-02 </td> <td align="right"> 2215 </td> </tr>
  <tr> <td align="right"> 3 </td> <td align="right"> 4.00 </td> <td> 2012-10-03 </td> <td align="right"> 410 </td> </tr>
  <tr> <td align="right"> 4 </td> <td align="right"> 36.00 </td> <td> 2012-10-03 </td> <td align="right"> 430 </td> </tr>
  <tr> <td align="right"> 5 </td> <td align="right"> 25.00 </td> <td> 2012-10-03 </td> <td align="right"> 535 </td> </tr>
  <tr> <td align="right"> 6 </td> <td align="right"> 90.00 </td> <td> 2012-10-03 </td> <td align="right"> 550 </td> </tr>
  <tr> <td align="right"> 7 </td> <td align="right"> 411.00 </td> <td> 2012-10-03 </td> <td align="right"> 555 </td> </tr>
  <tr> <td align="right"> 8 </td> <td align="right"> 413.00 </td> <td> 2012-10-03 </td> <td align="right"> 600 </td> </tr>
  <tr> <td align="right"> 9 </td> <td align="right"> 415.00 </td> <td> 2012-10-03 </td> <td align="right"> 605 </td> </tr>
  <tr> <td align="right"> 10 </td> <td align="right"> 519.00 </td> <td> 2012-10-03 </td> <td align="right"> 610 </td> </tr>
   </table>

Make a histogram of the total number of steps taken each day using new dataset.


```r
revised_total_steps_per_day <- revised_activity_data %>%
    select(date, steps) %>%
    group_by(date) %>%
    summarise(totalSteps = sum(steps))
```

<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Mon Feb 13 23:44:43 2017 -->
<table border=1>
<caption align="bottom"> revised_total_steps_per_day </caption>
<tr> <th>  </th> <th> date </th> <th> totalSteps </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> 2012-10-01 </td> <td align="right"> 10766.19 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> 2012-10-02 </td> <td align="right"> 126.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> 2012-10-03 </td> <td align="right"> 11352.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> 2012-10-04 </td> <td align="right"> 12116.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> 2012-10-05 </td> <td align="right"> 13294.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> 2012-10-06 </td> <td align="right"> 15420.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> 2012-10-07 </td> <td align="right"> 11015.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> 2012-10-08 </td> <td align="right"> 10766.19 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> 2012-10-09 </td> <td align="right"> 12811.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> 2012-10-10 </td> <td align="right"> 9900.00 </td> </tr>
   </table>

Plot histogram of steps per day.


```r
numBins <- 1 + ceiling(sqrt(range(revised_total_steps_per_day$totalSteps)))
binSize <- 3100

plot_histogram_revised_total_steps_per_day <- ggplot(data = revised_total_steps_per_day, aes(totalSteps)) +
    geom_histogram(stat = "bin", bins = numBins, binwidth = binSize, color = "darkred", fill = "red") +
    labs(title = "Revised Total Steps per Day", x = "Total Steps Taken", y = "Count") +
    ggsave(filename = "figures/histogram_revised_total_steps_per_day.png", width = 5, height = 3)

plot_histogram_revised_total_steps_per_day
```

![](PA1_template_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

calculate the mean value of total steps


```r
revisedMeanTotalSteps <- mean(revised_total_steps_per_day$totalSteps)
print(paste("Revised Mean total steps per day:", revisedMeanTotalSteps))
```

```
## [1] "Revised Mean total steps per day: 10766.1886792453"
```

calculate the mid value of total steps


```r
revisedMidTotalSteps <- median(revised_total_steps_per_day$totalSteps)
print(paste("Revised Median total steps per day:", revisedMidTotalSteps))
```

```
## [1] "Revised Median total steps per day: 10766.1886792453"
```

Do the values differ from the original mean and mid?
Yes, the Median and the Mean are equal which means this is a symmetric distribution.

What is the impact of imputing missing data on the estimates of the total daily number of steps?
Calculating Mode to test a hunch


```r
#getmode function was found here https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

revisedMode <- getmode(revised_total_steps_per_day$totalSteps)
print(paste("Revised Mode of averaged total steps per day:", revisedMode))
```

```
## [1] "Revised Mode of averaged total steps per day: 10766.1886792453"
```

The mid value is now equal to the mean value, so the slight skewness that was present initially has been corrected by replacing the missing values with the average total daily steps at each interval.
Furthermore, the Mode = Median = Mean, which meets the criteria for a perfect normal distribution.
With a normal distribution of interval data, the mean is usually the best measure of central tendency and is why we are using it for our calculatons.


Question #4: Are there differences in activity patterns between weekdays and weekends?

Create vectorized function to determine day of week using date and function "weekdays()" then return either "weekday" or "weekend".


```r
determineDayType <- function(x) {
    dayOfWeek <- weekdays(x, FALSE)
    ifelse(is.element(dayOfWeek,c("Saturday","Sunday")),return("weekend"),return("weekday"))
}

vdetermineDayType <- Vectorize(determineDayType)
```

Create and populate a new column in a new dataset with the revised data and the results of vdetermineDayType(date).


```r
dayType_activity_data <- mutate(revised_activity_data, dayType = vdetermineDayType(revised_activity_data$date))

### make the column a factor with 2 levels.
dayType_activity_data$dayType <- as.factor(dayType_activity_data$dayType)
```

<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Mon Feb 13 23:44:45 2017 -->
<table border=1>
<caption align="bottom"> dayType_activity_data </caption>
<tr> <th>  </th> <th> steps </th> <th> date </th> <th> interval </th> <th> dayType </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 0.00 </td> <td> 2012-10-02 </td> <td align="right">   0 </td> <td> weekday </td> </tr>
  <tr> <td align="right"> 2 </td> <td align="right"> 0.00 </td> <td> 2012-10-02 </td> <td align="right">   5 </td> <td> weekday </td> </tr>
  <tr> <td align="right"> 3 </td> <td align="right"> 0.00 </td> <td> 2012-10-02 </td> <td align="right">  10 </td> <td> weekday </td> </tr>
  <tr> <td align="right"> 4 </td> <td align="right"> 0.00 </td> <td> 2012-10-02 </td> <td align="right">  15 </td> <td> weekday </td> </tr>
  <tr> <td align="right"> 5 </td> <td align="right"> 0.00 </td> <td> 2012-10-02 </td> <td align="right">  20 </td> <td> weekday </td> </tr>
  <tr> <td align="right"> 6 </td> <td align="right"> 0.00 </td> <td> 2012-10-02 </td> <td align="right">  25 </td> <td> weekday </td> </tr>
  <tr> <td align="right"> 7 </td> <td align="right"> 0.00 </td> <td> 2012-10-02 </td> <td align="right">  30 </td> <td> weekday </td> </tr>
  <tr> <td align="right"> 8 </td> <td align="right"> 0.00 </td> <td> 2012-10-02 </td> <td align="right">  35 </td> <td> weekday </td> </tr>
  <tr> <td align="right"> 9 </td> <td align="right"> 0.00 </td> <td> 2012-10-02 </td> <td align="right">  40 </td> <td> weekday </td> </tr>
  <tr> <td align="right"> 10 </td> <td align="right"> 0.00 </td> <td> 2012-10-02 </td> <td align="right">  45 </td> <td> weekday </td> </tr>
   </table>



```r
### Calculate average steps by dayType and interval
dayType_Mean_Total_Steps_by_Interval <- dayType_activity_data %>%
    group_by(dayType, interval) %>%
    summarise(meanSteps = mean(steps))
```

<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Mon Feb 13 23:44:45 2017 -->
<table border=1>
<caption align="bottom"> dayType_Mean_Total_Steps_by_Interval </caption>
<tr> <th>  </th> <th> dayType </th> <th> interval </th> <th> meanSteps </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> weekday </td> <td align="right">   0 </td> <td align="right"> 2.25 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> weekday </td> <td align="right">   5 </td> <td align="right"> 0.45 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> weekday </td> <td align="right">  10 </td> <td align="right"> 0.17 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> weekday </td> <td align="right">  15 </td> <td align="right"> 0.20 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> weekday </td> <td align="right">  20 </td> <td align="right"> 0.10 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> weekday </td> <td align="right">  25 </td> <td align="right"> 1.59 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> weekday </td> <td align="right">  30 </td> <td align="right"> 0.69 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> weekday </td> <td align="right">  35 </td> <td align="right"> 1.14 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> weekday </td> <td align="right">  40 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> weekday </td> <td align="right">  45 </td> <td align="right"> 1.80 </td> </tr>
   </table>

Plot using "lattice" to match example


```r
time_series_mean_steps_interval_dayType <-  xyplot(meanSteps ~ interval|dayType, data = dayType_Mean_Total_Steps_by_Interval, type = "l", layout = c(1,2), xlab = "Interval", ylab = "Average Total Steps", main = "Average Total Steps per Interval by Day Type")
#plot(time_series_mean_steps_interval_dayType)
#note: lattice can only save to the current working directory (unless I missed something), so added steps to change to "figures" dir and then back.
time_series_mean_steps_interval_dayType
```

![](PA1_template_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

We can see from the comparison of the two visualizations that the subject's pattern has a slight shift to the right on weekends.  Based on the average observations, they are more active later in the morning and remain very active until later at night on a weekend than on weekdays.

The weekday displays a higher level of activity at first, then a rapid and severe drop in activity that remains low to moderate for the duration of the day and night, leading to an earlier drop in activity in the last intervals, slowing activity much sooner than during the weekend. 
