Peer assingment 1 - Reproducible research
========================================================
## Loading the dataset and converting date column into date:

```r
df<- read.csv(file="activity.csv")
df$date<- as.Date(df$date)
summary(df)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

## What is mean total number of steps taken per day?
*Make a histogram of the total number of steps taken each day*

```r
steps.sum <- tapply(df$steps, df$date, sum)
steps.sum.na <- steps.sum[!is.na(steps.sum)]  #omitting NA values
hist(steps.sum.na, main="histogram | total number of steps taken each day", xlab="Number of steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 
*Calculate and report the mean and median total number of steps taken per day*

```r
mean.steps <- mean(steps.sum.na)
median.steps <- median(steps.sum.na)
```
The mean number of steps equals to 1.0766 &times; 10<sup>4</sup> and median is 10765.

## What is the average daily activity pattern?
*Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

```r
av.steps <- tapply(df$interval, df$steps, mean) 
noOfSteps <- data.frame(interval=names(av.steps), steps=av.steps, stringsAsFactors=F)
plot(noOfSteps$interval, noOfSteps$steps, type="l", xlab="interval", ylab="number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
  
*Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*

```r
max.interval <- noOfSteps[order(noOfSteps$steps, decreasing=T),][1,1]
```
Interval **591** contains the maximum number of steps.

## Imputing missing values
*Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*

```r
missing.values <- sum(is.na(df$steps))
```
The number of missing values is **2304**.

*Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in. Create a new dataset that is equal to the original dataset but with the missing data filled in. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.*
I am using mean for that day.

```r
days.df <- split(df, df$date)
new.days.df <- days.df
mean.in.day <- tapply(df$steps, df$date, mean)

for (i in 1:length(days.df)) {
    for (j in 1:length(days.df[[i]]$steps)) {
        if (is.na(days.df[[i]]$steps[j])) {new.days.df[[i]]$steps[j] <- mean.in.day[j]}
            }
}
new<-data.frame(new.days.df[[1]])
for (i in 1:(length(new.days.df)-1)) {
    new<- rbind(new.days.df[[i+1]], new)
    }
new.df <- new[!is.na(new$steps),]

new.df.sum <- tapply(new.df$steps, new.df$date, sum)
hist(new.df.sum, main="histogram | total number of steps taken each day", xlab="Number of steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

*Calculate and report the mean and median total number of steps taken per day.*

```r
mean.new.df <- mean(new.df.sum)
median.new.df <- median(new.df.sum)
```
When NA values were filled with means for each day, the mean number of steps were 1.0766 &times; 10<sup>4</sup> and median 10765.

*Do these values differ from the estimates from the first part of the assignment?*

```r
mean.diff <- mean.new.df-mean.steps
median.diff <- median.new.df-median.steps
```
Yes.  

*What is the impact of imputing missing data on the estimates of the total daily number of steps?*

Both mean and median number of steps are lower (-1152.1195, -370, respectively).
    
##Are there differences in activity patterns between weekdays and weekends?
*Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.*

```r
Sys.setlocale(category = "LC_TIME", locale = "C") #english
```

```
## [1] "C"
```

```r
week<- weekdays(new.df$date)   # dataframe with filled missing values
days<- unique(week)
for (i in 1:5) { week <- gsub(days[i], "Weekdays", week) }
for (i in c(6,7)) { week <- gsub(days[i], "Weekend", week) }
new.df$week <- as.factor(week)          
str(new.df)
```

```
## 'data.frame':	15688 obs. of  4 variables:
##  $ steps   : num  0.438 39.417 42.069 46.16 53.542 ...
##  $ date    : Date, format: "2012-11-30" "2012-11-30" ...
##  $ interval: int  5 10 15 20 25 30 40 45 50 55 ...
##  $ week    : Factor w/ 2 levels "Weekdays","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```
*Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*

```r
splited <- split(new.df, new.df$week)
new.df.weekdays<- splited[[1]]; new.df.weekend<- splited[[2]]

av.steps.weekdays <- tapply(new.df.weekdays$interval, new.df.weekdays$steps, mean)
noOfSteps.weekdays <- data.frame(interval=names(av.steps.weekdays), steps=av.steps.weekdays, stringsAsFactors=F)

av.steps.weekend <- tapply(new.df.weekend$interval, new.df.weekend$steps, mean)
noOfSteps.weekend <- data.frame(interval=names(av.steps.weekend), steps=av.steps.weekend, stringsAsFactors=F)

par(mfrow=c(2,1))
plot(noOfSteps.weekdays$interval, noOfSteps.weekdays$steps, type="l", xlab="interval", ylab="number of steps", main="Weekdays")
plot(noOfSteps.weekend$interval, noOfSteps.weekend$steps, type="l", xlab="interval", ylab="number of steps", main="Weekend")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


