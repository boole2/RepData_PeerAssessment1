---
title: "PA1_template"
author: "boole2"
date: "July 20, 2016"
output: html_document
---



## Foot Step Monitoring Project

Analysis of data based on the  steps done every day, measured every 5 minutes,
for a person monitored from 2012-10-1 to 2012-11-30, two month.


## Loading and preprocessing the data

```r
#set working directory
setwd("/home/boole2/coursera/reproducibleAnalysis/prj1")

#load data
rowData <- read.csv('activity.csv', stringsAsFactors = F, na.strings = "NA")
rowData$date <- as.Date(as.character(rowData$date), "%Y-%m-%d")

head(rowData)
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

## What is mean total number of steps taken per day?




```r
tot_step_day <-  tapply(rowData$steps,as.factor(rowData$date), FUN = sum)
hist(tot_step_day, breaks = seq(0, 25000, 2500), col = "green", xlab = "Total Number of Step per day", ylab = " Frequency", main = " Historgram of Total Number of Steps per Day" )
```

<img src="PA1_template_files/figure-html/steps per day-1.png" title="" alt="" width="672" />

```r
#mean and mendian
 summary(tot_step_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

```r
 # standard deviation
  sd(tot_step_day, na.rm = TRUE)
```

```
## [1] 4269.18
```


What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
# Remove NA row from Data otherwise not possible to summarize.
na.vect <- is.na(rowData$steps)
# Number of Row with Steps NA
sum(na.vect)
```

```
## [1] 2304
```

```r
#remove and create a new Data Frame rowD without NA row
rowD <- rowData[ -which(na.vect),]

#Summarize the Data for each one of the 288 type of Interval 
length( unique(rowD$interval))
```

```
## [1] 288
```

```r
rr <- rowD %>% group_by(interval) %>% summarize(
                      total = sum(steps),
                      median = median(steps),
                      avg_steps = mean(steps))



df_graph <- data.frame(inteval  = unique(rowD$interval), avg_steps = rr$avg_steps )

qplot(x = inteval, y= avg_steps, data=df_graph, geom=c("point", "line"), 
   main="Pattern of Walking for Avarage Day", 
   xlab="Interval", ylab="Avarage Steps")
```

<img src="PA1_template_files/figure-html/activity pattern-1.png" title="" alt="" width="672" />

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# Interval with Max Average Steps
rr[ max(rr$avg_steps)  == rr$avg_steps, ]$interval
```

```
## [1] 835
```

## Imputing missing values

We  use  the mean for that 5-minute interval to fill in missing value.

```r
na.vect <- is.na(rowData$steps)
# Number of Row with Steps NA
sum(na.vect)
```

```
## [1] 2304
```

```r
replace_Na <- function(data){
        row_index <-  which (is.na(data$steps))  
   
       for( i in  row_index){
               data[i,]$steps <- rr[ data[i,]$interval == rr$interval,]$avg_steps 
       }
      data      
}

df_fillin <- replace_Na(rowData)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
tot_step_day_filledIn <-  tapply(df_fillin$steps,as.factor(df_fillin$date), FUN = sum)
hist(tot_step_day_filledIn, breaks = seq(0, 25000, 2500), col = "yellow", xlab = "Total Number of Step per day ", ylab = " Frequency", main = " Historgram of Total Number of Steps per Day with NA filled In" )
```

<img src="PA1_template_files/figure-html/steps per day NA Filled In-1.png" title="" alt="" width="672" />

```r
 summary(tot_step_day_filledIn)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

```r
  # standard deviation
 sd(tot_step_day_filledIn)
```

```
## [1] 3974.391
```

Median and Mean now are the same and quartile more close to the mean, the distribution is more simmetric and over all variance reduced due to increase number of sample matching the avarage of the corresponding interval .



## Are there differences in activity patterns between weekdays and weekends?


```r
tag <- rep("weekday",nrow(df_fillin))
tag[ which ( weekdays(df_fillin$date) == "Sunday" |  weekdays(df_fillin$date) == "Saturday")] <- "weekend"
df_fillin$tag <- tag
df_wend <-  df_fillin[ tag == "weekend", ]
df_wday <- df_fillin[ tag == "weekday", ]


rr_wend <- df_wend %>% group_by(interval) %>% summarize(
                      total = sum(steps),
                      median = median(steps),
                      avg_steps = mean(steps))

rr_wday <- df_wday %>% group_by(interval) %>% summarize(
                      total = sum(steps),
                      median = median(steps),
                      avg_steps = mean(steps))

#Create a marged Data Frame to plot the two interval time series.
#toghether, one for WeekEnd Avg Steps by Interval and the other one for Weekday.
df_graph_2 <- data.frame(inteval  = unique(rr_wend$interval), 
                         avg_wend = rr_wend$avg_steps,
                         avg_wday =  rr_wday$avg_steps)

df_graph_melted <- melt(df_graph_2, id = c( "inteval"))
head(df_graph_melted)
```

```
##   inteval variable       value
## 1       0 avg_wend 0.214622642
## 2       5 avg_wend 0.042452830
## 3      10 avg_wend 0.016509434
## 4      15 avg_wend 0.018867925
## 5      20 avg_wend 0.009433962
## 6      25 avg_wend 3.511792453
```

```r
names(df_graph_melted) <- c("inteval","type","Average.Steps")


qplot(x = inteval, y= Average.Steps, data=df_graph_melted, geom=c("point", "line"), 
      facets = . ~ type,color = type,
   main="Pattern of Walking of Week Day vs Week end", 
   xlab="Interval", ylab="Avarage Steps")
```

<img src="PA1_template_files/figure-html/Week Day vs  Week end Pattern-1.png" title="" alt="" width="672" />

Week end activities are more flat and distribuited all around day light interval while douring the Week there's a pick of activity in the moring interval. 

