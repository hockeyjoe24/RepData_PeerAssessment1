# Reproducible Research: Peer Assessment 1

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit][Link_fitbit], [Nike Fuelband][Link_nike], or [Jawbone Up][Link_jawbone]. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web site:

Dataset: [Activity monitoring data] [Link_Data] [52K] 

The variables included in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- **date**: The date on which the measurement was taken in YYYY-MM-DD format

- **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use `echo = TRUE` so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())


```r
activityfile <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activityfile$asDate <- as.Date(activityfile$date, "%Y-%m-%d")
library(data.table)
dt = as.data.table(activityfile)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
dt.total<-dt[, list(steps_total=sum(steps, na.rm = TRUE)), by=asDate]
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

This is a barplot of total steps:


```r
barplot(dt.total$steps_total)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

This is a histogram of total steps broken into 10 buckets:


```r
hist(x=dt.total$steps_total, breaks=10, main="Histogram of Total Steps Per Day", xlab="Total Steps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


3. Calculate and report the mean and median of the total number of steps taken per day


```r
dt.total[, list(steps_median = median(as.double(steps_total), na.rm = TRUE), 
                steps_mean= mean(steps_total, na.rm = TRUE))]
```

```
##    steps_median steps_mean
## 1:        10395    9354.23
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
dt.interval<-dt[, list(steps_mean=mean(steps, na.rm = TRUE)), by=interval]
with(dt.interval, plot(x=interval, y=steps_mean, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
dt.interval[steps_mean==max(dt.interval$steps_mean),]
```

```
##    interval steps_mean
## 1:      835   206.1698
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
dt[is.na(dt$steps),.N] # use the .N with is.na for steps.
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# First verify that the mean of each 5-minute interval is not NA, then use the 
# rounded 5-minute interval means to populate the NA steps as integers.
dt.interval[is.na(dt.interval$steps_mean),]
```

```
## Empty data.table (0 rows) of 2 cols: interval,steps_mean
```

```r
dt.new <- copy(dt)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
setkey(dt.new, interval)
setkey(dt.interval, interval)
dt.new[is.na(steps), steps := dt.interval[.(.SD), as.integer(round(steps_mean,0))]]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

We total steps by day in the imputed value data table and see the histogram shifting to the right, giving a more normal distribution. The frequencies also see an increase near the center of the distribution here.

```r
dt.totals<-dt.new[, list(steps_total=sum(steps, na.rm = TRUE)), by=asDate]
hist(x=dt.totals$steps_total, breaks=10, main="Histogram of Total Steps Per Day - Imputing", xlab="Total Steps Per Day - Imputing")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

We also see steps by day in the imputed value data table having an increase in mean and median. If we replace missing data with mean values, the totals will increase.

```r
dt.totals[, list(steps_median = median(as.double(steps_total), na.rm = TRUE), 
                 steps_mean= mean(steps_total, na.rm = TRUE))]
```

```
##    steps_median steps_mean
## 1:        10762   10765.64
```

## Are there differences in activity patterns between weekdays and weekends?

1. For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

I copy the dataset and add a new field that will become my factor once I split it into weekend and weekday. I initially populate with "weekday" since there are only two weekend days to be used in criteria below.

```r
dt.daysplit <- copy(dt.new)
dt.daysplit[,daytype:="weekday"]
```

2. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Now use the weekdays function on the date field to find the weekend days.

```r
dt.daysplit[weekdays(asDate)=="Saturday" | weekdays(asDate)=="Sunday", daytype:="weekend"]
dt.daysplit$daytype <- as.factor(dt.daysplit$daytype)
```

3. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
dt.intervalsplit<-dt.daysplit[, list(steps_mean=mean(steps, na.rm = TRUE)), by=.(interval, daytype)]
par(mfrow=c(2,1))
with(dt.intervalsplit[dt.intervalsplit$daytype=="weekend",], plot(x=interval, y=steps_mean, type = "l", main="weekend", xlab="", ylab="", xaxt="n", ylim=c(-5,250)))

with(dt.intervalsplit[dt.intervalsplit$daytype=="weekday",], plot(x=interval, y=steps_mean, type = "l", main="weekday", xlab = "", ylab = "", ylim=c(-5,250)))
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 






[Link_fitbit]: http://www.fitbit.com/
[Link_nike]: http://www.nike.com/us/en_us/c/nikeplus-fuelband
[Link_jawbone]: https://jawbone.com/up
[Link_Data]: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)


