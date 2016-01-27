---
output: html_document
---
# Reproducible Research: Peer Assessment 1
The following packages must be loaded into R
```{r, echo=TRUE}
library(lubridate)    ## For dates
library(ggplot2)      ## For plottingt
library(dplyr)        ## Used by group_by and %>%
```

## Loading and preprocessing the data
It is assumed that the reader has set the correct working directory in R by function setwd()
```{r, echo=TRUE}
df <- read.csv("activity.csv", header = TRUE, sep = ',')
```
A first view and knowledge of the data is obtained by function str() and head()
```{r, echo=TRUE}
str(df)
head(df)
```
Convert the date from a factor to a date format and remove all rows from data set with step == NA  
and store the result in new object df_NoNA
```{r, echo=TRUE}
df$date <- ymd(df$date)
df_NoNA<-na.omit(df)
str(df_NoNA)
```
2304 rows are omitted due to NA in df  
15264 rows in data frame df_NoNA

```{r, echo=TRUE}
head(df_NoNA)
```
## What is mean total number of steps taken per day?
Object StepsPerDay gives the number of steps per day by grouping df_NoNA by date and summarize  
the steps
```{r, echo=TRUE}
StepsPerDay<- df_NoNA %>% group_by(date) %>% summarise(step=sum(steps))
head(StepsPerDay)
```
A histogram gives the distribution of steps per day
```{r, echo=TRUE}
HistDate<-ggplot(StepsPerDay, aes(date, step)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue")+ labs(title = "Total Number of Steps Per Day", x = "Date", y = "# Steps")
HistDate
```


The mean total of number of steps per day is 10766.19  
```{r, echo=TRUE}
mean(StepsPerDay$step)
```
The median of the total number of steps per day is given by the 27th observation (half of the total # of observations)  
when the data is ordered by StepsPerDay and the value is 10765  
```{r, echo=TRUE}
median(StepsPerDay$step)
```

## What is the average daily activity pattern?
The object DayActivity gives the number of steps per interval (5 min interval) averaged over all days 
```{r, echo=TRUE}
DayActivity<-df_NoNA %>% group_by(interval) %>% summarise(step=mean(steps))
head(DayActivity)
```
A histogram gives the distribution of steps per interval (time of day)
```{r, echo=TRUE}
HistDayActivity<-ggplot(DayActivity, aes(interval,step)) + geom_line(colour = "steelblue")+ labs(title = "Avg steps during a day", x = "Time of day", y = "# Steps")
HistDayActivity
```


The most active time of the day is at 8:35 AM with 206 steps in average.
```{r, echo=TRUE}
DayActivity[which.max(DayActivity$step),]
```

## Imputing missing values
The numer of missing values NA in the original data set df is 2304 (summarize all rows having NAs)
```{r, echo=TRUE}
sum(is.na(df[1]))
```

The missing values are replaced by the mean total of number of steps per day (10766.19) divided by  
number of intervals per day (288)  
```{r, echo=TRUE}
df = transform(df, steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
```
Execute sum(is.na(df[1])) once again gives zero meaning zero rows containing NA.
```{r, echo=TRUE}
sum(is.na(df[1]))
```

Object StepsPerDayFull gives the number of steps per day by grouping the modified df by date and summarize  
the steps
```{r, echo=TRUE}
StepsPerDayFull<- df %>% group_by(date) %>% summarise(step=sum(steps))
head(StepsPerDayFull)
```
Note the 1st of Oct now has 10766 steps, which is equal to the mean total of number of steps per day  
since in the original data set no steps were recorded for 2012-10-01

The histogram is given by
```{r, echo=TRUE}
HistDateFull<-ggplot(StepsPerDayFull, aes(date, step)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue") + labs(title = "Total Number of Steps Per Day", x = "Date", y = "# Steps")
HistDateFull
```



The mean and the median is now 10766.19. The mean is unchanged and the median has changed from 10765 to 10766.19  

```{r, echo=TRUE}
mean(StepsPerDayFull$step)
median(StepsPerDayFull$step)
```
## Are there differences in activity patterns between weekdays and weekends?
Set the date/time related display language to English
```{r, echo=TRUE}
Sys.setlocale("LC_TIME","English")
```
Addes a column named weektype to calculate each row in df if the day is a weekday or weekend 
```{r, echo=TRUE}
df<-mutate(df, weektype = ifelse(weekdays(as.Date(df$date))=="Saturday" | weekdays(as.Date(df$date))=="Sunday", "weekend", "weekday"))
head(df)
```
The object WeekTypeActivity groups df by interval and weektype and summarize by steps so it will be possible to plot the two  different week types.
```{r, echo=TRUE}
WeekTypeActivity<-df %>% group_by(interval,weektype) %>% summarise(step=mean(steps))
head(WeekTypeActivity)
```
A panel plot of the two week activities is done by using the facet_grid() function with argument (weektype ~.) 
```{r, echo=TRUE}
HistWeekTypeActivity<-ggplot(WeekTypeActivity, aes(interval,step)) + geom_line(colour = "steelblue")+ labs(x = "Time of day", y = "# Steps") + facet_grid(weektype ~.)
HistWeekTypeActivity
```

