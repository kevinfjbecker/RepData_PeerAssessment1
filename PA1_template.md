# Reproducible Research: Peer Assessment 1

## Libraries
library(dplyr)
library(tidyr)
library(ggplot2)


## Loading and preprocessing the data
data <- read.csv(unzip('activity.zip'))


## What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day
(function(d) { # get summarized steps per day
  d %>% 
    group_by(date) %>%
    select(steps) %>%
    summarise(stepsperday = sum(steps, na.rm = TRUE))
})(data)

### 2. Make a histogram of the total number of steps taken each day
(function(d) { # generate histogram
  perday <- d %>% 
    group_by(date) %>%
    select(steps) %>%
    summarise(stepsperday = sum(steps, na.rm = TRUE))

  qplot(perday$stepsperday, geom="histogram")
})(data)

### 3. Calculate and report the mean and median of the total number of steps taken per day
(function(d) { # get mean daily total
  d %>% 
    group_by(date) %>%
    select(steps) %>%
    summarise(stepsperday = sum(steps, na.rm = TRUE)) %>%
    summarise(mean(stepsperday, rm.na = TRUE))
})(data)

(function(d) { # get median daily total
  d %>% 
    group_by(date) %>%
    select(steps) %>%
    summarise(stepsperday = sum(steps, na.rm = TRUE)) %>%
    summarise(median(stepsperday))
})(data)


## What is the average daily activity pattern?

### 1. Time series plot of average number of steps takenthe per 5-minute interval 
(function(d) { # get summarized steps per day
  perinterval <- d %>% 
    group_by(interval) %>%
    select(steps) %>%
    summarise(stepsperinterval = mean(steps, na.rm = TRUE))
    
    plot(perinterval$interval, perinterval$stepsperinterval, type = 'l')
})(data)

### 2. Which averaged 5-minute interval contains the maximum number of steps?
(function(d) { # get summarized steps per day
  d %>% 
    group_by(interval) %>%
    select(steps) %>%
    summarise(stepsperinterval = mean(steps, na.rm = TRUE)) %>%
    filter(stepsperinterval == max(stepsperinterval))
})(data)




## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
