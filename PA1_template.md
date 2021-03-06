---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Reproducible Research: Peer Assessment 1
==================================================

Introduction
-------------------------


It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from **an anonymous individual** collected **during the months of October and November, 2012** and include the number of steps taken in **5 minute intervals** each day.


Data
-------------------------
The data for this assignment can be downloaded from the course web site: 
* Dataset: Activity monitoring data [52K]

The variables included in this dataset are:
* steps: Number of steps taking in a 5-minute interval (missing values are
coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD
format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


Assignment
-------------------------

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use **echo = TRUE** so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)
Fork/clone the [GitHub repository created for this assignment](https://github.com/rdpeng/RepData_PeerAssessment1). You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

## Loading and preprocessing the data
1. Load the data 
2. Process/transform the data (if necessary) into a format suitable for your
analysis


```r
# create a temporary directory
td = tempdir()
# get the name of the first file in the zip archive
fname = unzip("activity.zip", list=TRUE)$Name
# unzip the file to the temporary directory
unzip("activity.zip", files=fname, exdir=td, overwrite=TRUE)
# fpath is the full path to the extracted file
fpath = file.path(td, fname)
# read the file
d = read.csv(fpath, header=TRUE, row.names=NULL, 
             stringsAsFactors=FALSE)
```

Check the data

```r
class(d)
```

```
## [1] "data.frame"
```

```r
head(d)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
dim(d) # 17568 obs. verified
```

```
## [1] 17568     3
```

Transfrom to the data-time 

```r
d$date = as.Date(d$date)
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day
2. Calculate and report the mean and median total number of steps taken per day

```r
# function to sum step in a Date
step_per_day <- tapply(d$steps,as.factor(d$date),sum,na.rm =T)
#tmp = aggregate(d$steps,list(d$date),sum)
#head(step_per_day)
#hist(tmp$x)
hist(step_per_day,xlab="Steps(per day)")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 



```r
# mean and median total number of steps taken per day
mean(step_per_day) #[1] 9354.23
```

```
## [1] 9354.23
```

```r
# median total number of steps taken per day
median(step_per_day) #[1] 10395
```

```
## [1] 10395
```

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
step_per_interval<-tapply(d$steps,as.factor(d$interval),mean,na.rm=T)
plot(levels(as.factor(d$interval)),step_per_interval,type="l",
     xlab="5-min interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(which.max(step_per_interval))
```

```
## [1] "835"
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

```r
sum(is.na(d))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
d_new <- d
# replace na by the mean for that 5-minute interval
d_new$steps[is.na(d$steps)] = 
  step_per_interval[as.character(d_new$interval[is.na(d$steps)])]
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
step_per_day2 <- tapply(d_new$steps,as.factor(d$date),sum)

# histogram
hist(step_per_day2,xlab="Steps(per day)")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
# mean
mean(step_per_day2) #[1] 9354.23 vs now: 10766.19
```

```
## [1] 10766.19
```

```r
# median
median(step_per_day2) #[1] 10395 vs now: 10766.19
```

```
## [1] 10766.19
```
Do these values differ from the estimates from the first part of the assignment? 
* *Yes*

What is the impact of imputing missing data on the estimates of the total daily number of steps?
* *Increased the value; also make mean equal to median*

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
weekend_factor<-weekdays(d$date)
weekend_idx<-weekend_factor %in% c("Saturday","Sunday")
weekend_factor[weekend_idx]<-"weekend"
weekend_factor[!weekend_idx]<-"weekday"
weekend_factor<-as.factor(weekend_factor)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
library("lattice")
d_new$weekend_factor = weekend_factor
xyplot(steps~interval|weekend_factor,data=d_new,scales="free",
       layout=c(1,2),region=F,ylim=c(0,250),ylab="Number of Steps",
       panel=function(x,y){
         panel.average(x,y,horizontal=F,col="blue")})
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
