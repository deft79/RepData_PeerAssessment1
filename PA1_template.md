# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
## load libraries
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(magrittr)
library(ggplot2)

if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityData <- read.csv('activity.csv')
```

## What is mean total number of steps taken per day?


```r
		Total_Steps<- activityData%>%
group_by(date)%>%
summarise(total_steps = sum(steps, na.rm=TRUE))

Total_Steps$total_steps <- as.numeric(Total_Steps$total_steps)

Total_StepsMean <- mean(Total_Steps$total_steps)
Total_StepsMedian <- median(Total_Steps$total_steps)

ggplot(Total_Steps, aes(x = total_steps)) +
        geom_histogram(fill = "blue", binwidth = 1000) +
        labs(title = "Daily Steps", x = "Total Steps", y = "Frequency") + geom_vline(aes(xintercept = Total_StepsMedian, color="median"), linetype="solid", size=1.5) + geom_vline(aes(xintercept = Total_StepsMean, color="mean"), linetype="dotted",, size=1.5) + scale_color_manual(name = NULL, values = c(median = "black", mean = "red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

* Mean: 9354
* Median:  10395


## What is the average daily activity pattern?

```r
averages <- aggregate(x=list(steps=activityData$steps), by=list(interval=activityData$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
maxinterval <- filter(averages,steps==max(steps))
```

* The 5-minute interval that, on average, contains the maximum number of steps: 835


## Imputing missing values

##### 1. Calculate and report the total number of missing values in the dataset 

```r
missingData <- is.na(activityData$steps)
table(missingData)
```

```
## missingData
## FALSE  TRUE 
## 15264  2304
```

##### 2. Devise a strategy for filling in all of the missing values in the dataset.
##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityData2<- activityData
naSteps<- is.na(activityData2$steps)
avg_interval<- tapply(activityData2$steps, activityData2$interval, mean, na.rm=TRUE, simplify = TRUE)
activityData2$steps[naSteps] <- avg_interval[as.character(activityData2$interval[naSteps])]
names(activityData2) 
```

```
## [1] "steps"    "date"     "interval"
```
##### 4. Check if no missing value is appearing:


```r
sum(is.na(activityData2))
```

```
## [1] 0
```


##### 4. Make a histogram of the total number of steps taken each day 

```r
stepsByDay <- activityData2%>%
group_by(date)%>%
summarise(total_steps = sum(steps, na.rm=TRUE))

stepsByDay$total_steps <- as.numeric(stepsByDay$total_steps)

	stepsByDayMean <- mean(stepsByDay$total_steps)
stepsByDayMedian <- median(stepsByDay$total_steps)

ggplot(stepsByDay, aes(x = total_steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Total Steps", y = "Frequency") + geom_vline(aes(xintercept = stepsByDayMedian, color="median"), linetype="solid", size=1.5) + geom_vline(aes(xintercept = stepsByDayMean, color="mean"), linetype="dotted",, size=1.5) + scale_color_manual(name = NULL, values = c(median = "black", mean = "red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
##### ... and Calculate and report the mean and median total number of steps taken per day. 

* Mean (Imputed): 10766
* Median (Imputed):  10766



## Are there differences in activity patterns between weekdays and weekends?



```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
activityData2$date <- as.Date(activityData2$date)
activityData2$day <- sapply(activityData2$date,FUN=weekday.or.weekend)
```


```r
averages <- aggregate(steps ~ interval + day, data=activityData2, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

