---
title: "PA1_template.Rmd"
author: "Jaime Veracka"
date: "Sunday, April 19, 2015"
output: html_document
#Reproducible Research: Peer assessment 1#

---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.


This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file



**Loading and processing the data**

Show any code that is needed to

Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis; loadind necessary packages, ensuring echo=TRUE is global





```{r}
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
library(data.table)
library(ggplot2)
```
This assignment instructions request to show any code that is needed to loading and preprocessing the data:

Loading and processing the data
Load the data (i.e. > read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis

**Load the required data**

The following statement is used to load the data using read.csv().

```{r, echo=FALSE}
rdata <- rdata <- read.csv('activity.csv', header = TRUE, sep = ",", colClasses=c("numeric", "character", "numeric"))
```


**Process data converting date field to date class and interval field to factor class**

```{r, echo=FALSE}
rdata$date <- as.Date(rdata$date, format = "%Y-%m-%d")
rdata$interval <- as.factor(rdata$interval)
```

Check data using str()

```{r, echo=FALSE}
str(rdata)
```

**What is mean total number of steps taken per day?**


Per instructions, we first ignore missing values:
Calculating the total steps per day:

```{r,echo=FALSE}
steps_per_day <- aggregate(steps ~ date, rdata, sum)
colnames(steps_per_day) <- c("date","steps")
head(steps_per_day)
```

Histogram of the total number of steps taken per day, plotted with appropriate bin interval #syntax taken from another student's github

```{r,echo=FALSE}
ggplot(steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "green", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 
```

Mean, and Median:

```{r,echo=FALSE}
steps_mean   <- mean(steps_per_day$steps, na.rm=TRUE)
steps_median <- median(steps_per_day$steps, na.rm=TRUE)
```

The mean is r steps_mean and median is r steps_median


**What is the average daily activity pattern?**


Calculate the aggregation of steps by intervals of 5-minutes and convert the intervals as integers and save them in a data frame called steps_per_interval:


```{r,echo=FALSE}
steps_per_interval <- aggregate(steps ~ interval, rdata, mean)
hist(steps_per_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
plot(steps_per_interval$interval,steps_per_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),1]
rmean <- mean(steps_per_day$steps)
rmedian <- median(steps_per_day$steps)
```


Plot with the time series of the average number of steps taken (averaged across all days) versus the 5-minute intervals:


```{r,echo=FALSE}
ggplot(steps_per_interval, aes(x=interval, y=steps)) +geom_line(color="orange", size=1) + labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") + theme_bw()
```


Find the 5-minute interval with the containing the maximum number of steps:


```{r,echo=FALSE}
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]
```

The r max_interval$intervalth interval has maximum r round(max_interval$steps) steps

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is r max_interval.


**Impute missing values. Compare imputed to non-imputed data**


```{r,echo=FALSE}
incomplete <- sum(!complete.cases(rdata))
imputed_data <- transform(rdata, steps = ifelse(is.na(rdata$steps), steps_per_interval$steps[match(rdata$interval, steps_per_interval$interval)], rdata$steps))
```

**Total number of missing values**

```{r,echo=FALSE}
missing_vals <- sum(is.na(rdata$steps))
```

**Strategy for filling in all of the missing values in the dataset**


Replace or repopulate missing values with the mean (since median value is almost the same it doesn't matter) value at the same interval across days


```{r,echo=FALSE}
na_fill <- function(data, pervalue) {
        na_index <- which(is.na(data$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- data$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}

rdata_fill <- data.frame(  
        steps = na_fill(rdata, steps_per_interval),  
        date = rdata$date,  
        interval = rdata$interval)
str(rdata_fill)
#now check if no missing values
sum(is.na(rdata_fill$steps))
```

**A histogram of the total number of steps taken each day**


pPlot a histogram of the daily total number of steps taken, plotted with a bin interval of 1000 steps, after filling missing values.


```{r,echo=FALSE}
fill_steps_per_day <- aggregate(steps ~ date, rdata_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")

##plotting the histogram
ggplot(fill_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 
```


**Calculate and report the mean and median total number of steps taken per day.**


```{r,echo=FALSE}
steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)
```

The mean is r format(steps_mean_fill,digits = 8) and median is r format(steps_median_fill,digits = 8).???borrowed, and doesn't make sense -find answer in next attempt


**Do these values differ from the estimates from the first part of the assignment?**


Yes, slightly


**What is the impact of imputing missing data on the estimates of the total daily number of steps?**


didn't finish


**Are there differences in activity patterns between weekdays and weekends?**



```{r,echo=FALSE}
weekdays_steps <- function(data) {
    weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                          FUN=mean, na.rm=T)
    # convert to integers for plotting
    weekdays_steps$interval <- 
            as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
    colnames(weekdays_steps) <- c("interval", "steps")
    weekdays_steps
}

data_by_weekdays <- function(data) {
    data$weekday <- 
            as.factor(weekdays(data$date)) # weekdays
    weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
    weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))

    weekend_steps <- weekdays_steps(weekend_data)
    weekday_steps <- weekdays_steps(weekday_data)

    weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
    weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

    data_by_weekdays <- rbind(weekend_steps, weekday_steps)
    data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
    data_by_weekdays
}

data_weekdays <- data_by_weekdays(rdata_fill)

#now plot
ggplot(data_weekdays, aes(x=interval, y=steps)) + 
        geom_line(color="violet") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
```


There are differences, with weekdays showing greater concentrations in certain time periods, with weekends being more evenly distributed

END
