---
title: "Reproducible Research: Peer Assessment 1"
author: "JM"
date: "2022-12-28"
output: html_document
keep_md: true
---

## Load libraries

```r
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(knitr)
```


## Loading and preprocessing the data
Show any code that is needed to

1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis

Answer:

```r
setwd("C:/R/Coursera")

df<-read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
Answer:

```r
df2<-na.omit(df)

dfagg<-aggregate(df2["steps"], by=df2["date"], sum)

kable(dfagg)
```



|date       | steps|
|:----------|-----:|
|2012-10-02 |   126|
|2012-10-03 | 11352|
|2012-10-04 | 12116|
|2012-10-05 | 13294|
|2012-10-06 | 15420|
|2012-10-07 | 11015|
|2012-10-09 | 12811|
|2012-10-10 |  9900|
|2012-10-11 | 10304|
|2012-10-12 | 17382|
|2012-10-13 | 12426|
|2012-10-14 | 15098|
|2012-10-15 | 10139|
|2012-10-16 | 15084|
|2012-10-17 | 13452|
|2012-10-18 | 10056|
|2012-10-19 | 11829|
|2012-10-20 | 10395|
|2012-10-21 |  8821|
|2012-10-22 | 13460|
|2012-10-23 |  8918|
|2012-10-24 |  8355|
|2012-10-25 |  2492|
|2012-10-26 |  6778|
|2012-10-27 | 10119|
|2012-10-28 | 11458|
|2012-10-29 |  5018|
|2012-10-30 |  9819|
|2012-10-31 | 15414|
|2012-11-02 | 10600|
|2012-11-03 | 10571|
|2012-11-05 | 10439|
|2012-11-06 |  8334|
|2012-11-07 | 12883|
|2012-11-08 |  3219|
|2012-11-11 | 12608|
|2012-11-12 | 10765|
|2012-11-13 |  7336|
|2012-11-15 |    41|
|2012-11-16 |  5441|
|2012-11-17 | 14339|
|2012-11-18 | 15110|
|2012-11-19 |  8841|
|2012-11-20 |  4472|
|2012-11-21 | 12787|
|2012-11-22 | 20427|
|2012-11-23 | 21194|
|2012-11-24 | 14478|
|2012-11-25 | 11834|
|2012-11-26 | 11162|
|2012-11-27 | 13646|
|2012-11-28 | 10183|
|2012-11-29 |  7047|

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
Answer:

```r
hist(dfagg$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day
Mean = 10766 
Median = 10765

Answer:

```r
summary(dfagg$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Answer:

```r
dfaggint<-aggregate(df2["steps"], by=df2["interval"], mean)

summary(dfaggint)
```

```
##     interval          steps        
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

```r
pts<-ggplot(dfaggint,aes(x=interval, y = steps))+geom_line()

pts
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Answer:

```r
which(dfaggint$steps >= 206)
```

```
## [1] 104
```

```r
dfaggint$interval[which(dfaggint$steps >= 206)]
```

```
## [1] 835
```

## Inputing missing values
Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
Answer:

```r
length(is.na(df))
```

```
## [1] 52704
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
Answer for 2 and 3:

```r
NA2mean <-function(df) replace(df, is.na(df), mean(df, na.rm = T))

dfnarep<-replace(df,T,lapply(df,NA2mean))
```

```
## Warning in mean.default(df, na.rm = T): 引数は数値でも論理値でもありません。NA 値を返します
```

```r
dfaggnanmean<-aggregate(dfnarep["steps"], by=dfnarep["date"], mean)

dfaggnanmedian<-aggregate(dfnarep["steps"], by=dfnarep["date"], median)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of inputing missing data on the estimates of the total daily number of steps?
Answer:
Impact of inputting missing values is we get a minimum value, a more realistic lower max steps value.

```r
summary(df)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
summary(dfaggnanmean)
```

```
##      date               steps        
##  Length:61          Min.   : 0.1424  
##  Class :character   1st Qu.:34.0938  
##  Mode  :character   Median :37.3826  
##                     Mean   :37.3826  
##                     3rd Qu.:44.4826  
##                     Max.   :73.5903
```

```r
hist(dfaggnanmean$steps)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)


## Are there differences in activity patterns between weekdays and weekends?
For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
Answer:

```r
dfnarep$date <- as.Date(dfnarep$date)
weekdays1<- c('Monday','Tuesday','Wednesday','Thursday','Friday')

dfnarep$wDay <- factor((weekdays(dfnarep$date)  %in%  weekdays1), levels = c(FALSE,TRUE), labels = c('weekend','weekday'))

glimpse(dfnarep)
```

```
## Rows: 17,568
## Columns: 4
## $ steps    [3m[38;5;246m<dbl>[39m[23m 37.3826, 37.3826, 37.3826, 37.3826, 37.3826, 37.3826, 37.3826, 37.3826, 37.3826, 37.3826, 37.3826, 3…
## $ date     [3m[38;5;246m<date>[39m[23m 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 201…
## $ interval [3m[38;5;246m<dbl>[39m[23m 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150,…
## $ wDay     [3m[38;5;246m<fct>[39m[23m weekday, weekday, weekday, weekday, weekday, weekday, weekday, weekday, weekday, weekday, weekday, w…
```

```r
X<-split(dfnarep,dfnarep$wDay)

Xwy<-X$weekday
Xwd<-X$weekend

xwymean<-aggregate(Xwy["steps"], by=Xwy["interval"], mean)

xwdmean<-aggregate(Xwd["steps"], by=Xwd["interval"], mean)
```


2. Make a panel plot containing a time series plot (i.e. \color{black}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
Answer:

```r
ptswy<-ggplot(xwymean,aes(x=interval, y = steps, group=1))+geom_line()+ggtitle("Weekday")

ptswy
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
ptswd<-ggplot(xwdmean,aes(x=interval, y = steps, group=1))+geom_line()+ggtitle("Weekend")

ptswd
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-2.png)
