---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Load the csv file into a data frame object, and have a quick look at the data:

```r
ourData <- read.csv("~/Coursera/C5W2/Assignment/activity.csv",header=TRUE,stringsAsFactors=FALSE)
head(ourData,10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

Check the data types of the variables:


```r
lapply(ourData,class)
```

```
## $steps
## [1] "integer"
## 
## $date
## [1] "character"
## 
## $interval
## [1] "integer"
```

Convert the date variable into a POSIXct type, so that it's a date instead of a character object:

```r
ourData$date <- as.POSIXct(ourData$date)
class(ourData$date)
```

```
## [1] "POSIXct" "POSIXt"
```


## What is mean total number of steps taken per day?

The number of steps taken per day is:

```r
num_steps <- with(ourData,tapply(steps,as.factor(date),sum,na.rm=TRUE))
num_steps
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

And we can have a look at the data in a histogram:

```r
library(ggplot2)
qplot(num_steps,xlab="Total number of steps per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The mean of the total number of steps taken per day is:

```r
mean(num_steps)
```

```
## [1] 9354.23
```

And the median is:

```r
median(num_steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Let's find the average number of steps taken on each 5-minute time lapse across all days, and plot it:

```r
avg_num_steps <- with(ourData,tapply(steps,as.factor(interval),mean,na.rm=TRUE))
plot(names(avg_num_steps),avg_num_steps,type="l",xlab="Time lapse (5 minutes period)",ylab="Average number of steps",main="Average number of steps per time interval across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Find the time interval with the highest average number of steps:


```r
parse_time<-function(x){
    if(nchar(x)<4){
        x<-paste0(paste0(rep(0,4-nchar(x)),collapse=""),x)
    }
    time_x <- paste0(substr(x,1,2),":",substr(x,3,4))
    return(time_x)
}

pos_max <- which(avg_num_steps == max(avg_num_steps))
parse_time(names(avg_num_steps[pos_max]))
```

```
## [1] "08:35"
```

## Imputing missing values

Find the number of rows with at least one NA:

```r
sum(!complete.cases(ourData))
```

```
## [1] 2304
```

In fact all the NAs appear only in the "steps" variable:

```r
lapply(ourData,function(x) sum(is.na(x)))
```

```
## $steps
## [1] 2304
## 
## $date
## [1] 0
## 
## $interval
## [1] 0
```

If we look at the data, there seem to be entire days with missing values. And if we look at the number of NAs by time interval, they look more evenly distributed (8 over 61). Therefore, we'll impute the values grouping by time interval


```r
with(ourData,tapply(steps,as.factor(date),function(x) sum(is.na(x))))
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##        288          0          0          0          0          0 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##          0        288          0          0          0          0 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##          0          0          0          0          0          0 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##          0          0          0          0          0          0 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##          0          0          0          0          0          0 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##          0        288          0          0        288          0 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##          0          0          0        288        288          0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##          0          0        288          0          0          0 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##          0          0          0          0          0          0 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##          0          0          0          0          0          0 
## 2012-11-30 
##        288
```

```r
with(ourData,tapply(steps,as.factor(interval),function(x) sum(is.na(x))))
```

```
##    0    5   10   15   20   25   30   35   40   45   50   55  100  105  110 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  115  120  125  130  135  140  145  150  155  200  205  210  215  220  225 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  230  235  240  245  250  255  300  305  310  315  320  325  330  335  340 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  345  350  355  400  405  410  415  420  425  430  435  440  445  450  455 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  500  505  510  515  520  525  530  535  540  545  550  555  600  605  610 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  615  620  625  630  635  640  645  650  655  700  705  710  715  720  725 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  730  735  740  745  750  755  800  805  810  815  820  825  830  835  840 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
##  845  850  855  900  905  910  915  920  925  930  935  940  945  950  955 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1000 1005 1010 1015 1020 1025 1030 1035 1040 1045 1050 1055 1100 1105 1110 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1115 1120 1125 1130 1135 1140 1145 1150 1155 1200 1205 1210 1215 1220 1225 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1230 1235 1240 1245 1250 1255 1300 1305 1310 1315 1320 1325 1330 1335 1340 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1345 1350 1355 1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1500 1505 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605 1610 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1615 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710 1715 1720 1725 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815 1820 1825 1830 1835 1840 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 1845 1850 1855 1900 1905 1910 1915 1920 1925 1930 1935 1940 1945 1950 1955 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2000 2005 2010 2015 2020 2025 2030 2035 2040 2045 2050 2055 2100 2105 2110 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2115 2120 2125 2130 2135 2140 2145 2150 2155 2200 2205 2210 2215 2220 2225 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2230 2235 2240 2245 2250 2255 2300 2305 2310 2315 2320 2325 2330 2335 2340 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2345 2350 2355 
##    8    8    8
```

And we'll use the mean value to do so


```r
ourData$meanTL <- tapply(ourData$steps, as.factor(ourData$interval), mean, na.rm=TRUE)
imputed_df <- data.frame(date = ourData$date, interval=ourData$interval)
imputed_df$steps <- ifelse(is.na(ourData$steps),ourData$meanTL,ourData$steps)
```

Now look at the total number of steps taken each day after having imputed the missing values:


```r
num_steps_imputedNAs <- with(imputed_df,tapply(steps,as.factor(date),sum,na.rm=TRUE))
qplot(num_steps_imputedNAs,xlab="Total number of steps per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

And finally, report on the new mean and median values of the total number of steps taken each day:


```r
mean(num_steps_imputedNAs)
```

```
## [1] 10766.19
```

```r
median(num_steps_imputedNAs)
```

```
## [1] 10766.19
```

Imputing values does have an influence on the final data reports, so we have to be very careful, and always bear in mind what's the final measure we want to take to minimize the impact.

## Are there differences in activity patterns between weekdays and weekends?

Add the extra column which will indicate whether the day is a "weekday" or "weekend":


```r
imputed_df$day_name <- weekdays(imputed_df$date,FALSE)
imputed_df$weekdayend <- ifelse(imputed_df$day_name == "sábado" | imputed_df$day_name == "domingo","weekend","weekday")
```

Finally, plot the average number of steps taken each time interval, split by weekend/weekday, which will indicate whether there's a different pattern depending on this chosen variable. 


```r
weekdays <- subset(imputed_df, weekdayend == "weekday")
weekend <- subset(imputed_df, weekdayend == "weekend")

avg_num_steps_WD <- with(weekdays,tapply(steps,as.factor(interval),mean,na.rm=TRUE))
avg_num_steps_WE <- with(weekend,tapply(steps,as.factor(interval),mean,na.rm=TRUE))

plot(names(avg_num_steps_WD),avg_num_steps_WD,type="l",col="green",xlab="Time lapse (5 minutes period)",ylab="Average number of steps",main="Average number of steps per time interval across all days")
lines(names(avg_num_steps_WE),avg_num_steps_WE,type="l",col="red")
legend("topleft",c("Weekday","Weekend"),col=c("green","red"),lty=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
