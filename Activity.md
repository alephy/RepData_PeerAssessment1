# RR-Assignment1
Alephy  
Monday, July 13, 2015  

This is an R Markdown document.

The assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

**Data**

The data for this assignment can be downloaded from Activity monitoring data [52K]
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. The variables included in this dataset are:

    -steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
    -date: The date on which the measurement was taken in YYYY-MM-DD format
    -interval: Identifier for the 5-minute interval in which measurement was taken

Download the data file, unzip and read in memory. We can ignore the missing values.


```r
if (!file.exists("activity.csv"))
  {
  URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(URL, destfile = "activity.zip")
  unzip("activity.zip")
  }
activity <- read.csv("activity.csv" ,header = TRUE, na.string = "NA")  
```
Format date:

```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```
**What is mean total number of steps taken per day?**
  
  1. Calculate the total number of steps taken per day

```r
TotalperDay <- aggregate(steps ~ date, activity, sum) 
TotalperDay
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

  2. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.1
```

```r
g <- ggplot(TotalperDay, aes(x=steps)) + geom_histogram()
print(g)
```

![](Activity_files/figure-html/unnamed-chunk-4-1.png) 

3. Calculate and report the mean of the total number of steps taken per day

```r
as.integer(mean(TotalperDay$steps,na.rm = T))
```

```
## [1] 10766
```

Calculate and report the median of the total number of steps taken per day

```r
as.integer(median(TotalperDay$steps))
```

```
## [1] 10765
```

**What is the average daily activity pattern?**

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
StepsperInterval<-aggregate(steps~interval,activity,mean)
plot(StepsperInterval$interval,StepsperInterval$steps,type="l",
     main='Average daily activity pattern',xlab='Time Interval (5 min units)',
     ylab='Steps')
```

![](Activity_files/figure-html/unnamed-chunk-7-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
StepsperInterval$interval[StepsperInterval$steps==max(StepsperInterval$steps)]
```

```
## [1] 835
```

**Imputing missing values**

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing<-sum(is.na(activity$steps))
missing
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 

We will use use positive integer random numbers with mean and standard deviation similar to existing data as input for missing values


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
Fullactivity<-activity
meansteps<- mean(Fullactivity$steps, na.rm = T)
stdevsteps<- sd(Fullactivity$steps, na.rm = T)
Fullactivity$steps[is.na(Fullactivity$steps)] <- abs(round((rnorm(missing, meansteps, stdevsteps)),digits=0))
```

4. Make a histogram of the total number of steps taken each day.


```r
Fullactivity_aggregate <- aggregate(steps ~ date, Fullactivity, sum) 
h <- ggplot(Fullactivity_aggregate, aes(x=steps)) + geom_histogram()
print(h)
```

![](Activity_files/figure-html/unnamed-chunk-11-1.png) 


Calculate and report the mean total number of steps taken per day.

```r
as.integer(mean(Fullactivity_aggregate$steps))
```

```
## [1] 12862
```
Calculate and report the median total number of steps taken per day.

```r
as.integer(median(Fullactivity_aggregate$steps))
```

```
## [1] 11458
```

Do these values differ from the estimates from the first part of the assignment?

  Yes.

What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
  Both mean and median is higher.

**Are there differences in activity patterns between weekdays and weekends?**

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
Fullactivity$days <-weekdays(as.Date(Fullactivity$date))
Fullactivity$daytype <- "weekday"
Fullactivity$daytype[Fullactivity$days=="Sunday" | Fullactivity$days=="Saturday"  ] <- "weekend"
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
Fullactivity_aggregate_new<-aggregate(steps~interval + daytype, Fullactivity, FUN=mean)
library(lattice)
xyplot(steps~interval|daytype, Fullactivity_aggregate_new,type="l",layout=c(1,2), ylab = "steps", xlab="Time Interval (5 min units)")
```

![](Activity_files/figure-html/unnamed-chunk-15-1.png) 

```r
ggplot(data=Fullactivity_aggregate_new, aes(x=interval, y=steps, group=daytype, color=daytype)) + geom_line() + xlab("Time Interval (5 min units)") + ylab("steps") 
```

![](Activity_files/figure-html/unnamed-chunk-15-2.png) 
