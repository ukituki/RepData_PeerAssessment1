Reproducible Research: Peer Assessment 1
=========================================

### Loading and preprocessing the data

Before running the code we assume that the working directory is set up correctly and that "activity.zip" file containing data has been extracted, so that "activity.csv" file is now located in main directory. Otherwise, we should execute the code that was commented in the first lines of a code chunk below. 

We start with loading data from the csv file and converting "date" column 
to a "Date" format.



```r
# setwd('~/Documents/edu/datasciencecoursera/5 Reproducible
# research/hw/RepData_PeerAssessment1') unzip('activity.zip')

activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity[, 2] <- as.Date(activity[, 2])
```


### What is mean total number of steps taken per day?


```r
stepsPerDay <- xtabs(steps ~ date, data = activity)
m <- mean(stepsPerDay)
med <- median(stepsPerDay)

# png(file = 'figure/histogram1.png')
hist(stepsPerDay, breaks = 20)
```

![plot of chunk histogram: total steps per day](figure/histogram:_total_steps_per_day.png) 

```r
# dev.off()
```


The mean of total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> and median is 1.0765 &times; 10<sup>4</sup>.


## What is the average daily activity pattern?



```r
library(reshape2)
m <- melt(activity, id = "interval", variable.name = "steps", na.rm = TRUE)
d <- dcast(m, interval ~ "steps", mean)

# png(file = 'figure/averActivity.png' )
plot(d[, 1], d[, 2], type = "l", xlab = "5-min interval", ylab = "Average number of steps taken", 
    main = "Average daily activity pattern")
```

![plot of chunk average daily activity pattern](figure/average_daily_activity_pattern.png) 

```r
# dev.off()

idx <- which.max(d[, 2])
```

Maximum number of steps (8466.7632 steps) were made in this 5-minute interval: 835. 


## Imputing missing values



```r
# sum(is.na(activity))

# get time intervals for each row with missing number of steps
time <- activity[is.na(activity), 3]
# vector of values used to fill the NAs
intervalAver <- c()

for (i in 1:length(time)) {
    intervalAver <- c(intervalAver, d[d[, 1] == time[i], "steps"])
}

# fill NAs with the average number of steps for given 5 minute period
completeData <- activity  #create new data set
completeData[is.na(completeData), 1] <- intervalAver

stepsPerDayCompl <- xtabs(steps ~ date, data = completeData)
# head(stepsPerDayCompl,20)
hist(stepsPerDayCompl, breaks = 20)
```

![plot of chunk Imputing missing values](figure/Imputing_missing_values.png) 

```r

# png(file='figure/filled_missing_values.png') hist(stepsPerDayCompl, breaks
# = 20) dev.off()

# mean(stepsPerDayCompl) == mean(stepsPerDay) as.vector(stepsPerDay)
# as.vector(stepsPerDayCompl)
rbind(summary(as.vector(stepsPerDay)), summary(as.vector(stepsPerDayCompl)))
```

```
##      Min. 1st Qu. Median   Mean 3rd Qu.    Max.
## [1,]   41    8840  10800  10800   13300   21200
## [2,]   41    9820  11500 326000   15100 2420000
```

```r
# median(stepsPerDayCompl) == median(stepsPerDay)

# boxplot(as.vector(stepsPerDay), as.vector(stepsPerDayCompl))
# sum(as.vector(stepsPerDay)) sum(as.vector(stepsPerDayCompl))
```


There are 2304 rows with missing values in this data set. 
After filling the missing values new data set has different median (1.1458 &times; 10<sup>4</sup> vs. previously 1.0765 &times; 10<sup>4</sup>) and much higher mean (3.2618 &times; 10<sup>5</sup> vs. previously 1.0766 &times; 10<sup>4</sup>).


## Are there differences in activity patterns between weekdays and weekends?



```r

wd <- factor(weekdays(completeData[, 2]), levels = c("poniedziałek", "wtorek", 
    "środa", "czwartek", "piątek", "sobota", "niedziela"))
weekendDays <- levels(wd)[6:7]

# create a logical vector - is this a weekend day?
isWeekend <- wd %in% weekendDays

# create factor variable 'day': 'weekend' or 'weekday'
completeData[isWeekend, "day"] <- "weekend"
completeData[!isWeekend, "day"] <- "weekday"
completeData[, "day"] <- as.factor(completeData[, "day"])

# summary(completeData) summary(as.factor(completeData$day))


# mean steps by 'day'
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## Następujące obiekty zostały zakryte from 'package:stats':
## 
##     filter, lag
## 
## Następujące obiekty zostały zakryte from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# summary(completeData) select(completeData, -date) filter(completeData, day
# == 'weekend') mutate(completeData, ) summarise(completeData, meansteps =
# mean(steps))


bytime <- group_by(completeData, interval, day)
# summarise(bytime, mean(steps))

meanStepsWeekend <- completeData %.% group_by(day, interval) %.% summarise(meansteps = mean(steps)) %.% 
    filter(day == "weekend")

meanStepsWeekday <- completeData %.% group_by(day, interval) %.% summarise(meansteps = mean(steps)) %.% 
    filter(day == "weekday")

# png(file = 'figure/weekend_vs_weekdays.png')
par(mfcol = c(2, 1))
plot(meanStepsWeekday[, 2], meanStepsWeekday[, 3], type = "l", main = "Weekadays: average number of steps during the day", 
    xlab = "Time", ylab = "Steps per 5 min. intervals")
plot(meanStepsWeekend[, 2], meanStepsWeekend[, 3], type = "l", main = "Weekend", 
    xlab = "Time", ylab = "Steps per 5 min. intervals")
```

![plot of chunk weekdays vs. weekend](figure/weekdays_vs__weekend.png) 

```r
par(mfcol = c(1, 1))
# dev.off()
```

