##load lubridate, knitr, and dplyr packages

library(lubridate)
library(knitr)
library(dplyr)
library(lattice)

activity<-read.csv("activity.csv", colClasses=c("numeric", "Date", "numeric"))

##Group data by date (61 unqiwue dates)
n_distinct(activity$date)
dailysteps<-group_by(activity, date)

#Generate sum of steps for each day for use in histogram
total_dailysteps<-summarise(dailysteps, sum(steps))
##Convert from list to numeric vector
num_total<-unlist(total_dailysteps[,2])
hist(num_total, main = "Histogram of Daily Step Totals",
     xlab ="Daily Step Totals")

##Calculate mean and median
mean(num_total, na.rm=TRUE)
median(num_total, na.rm=TRUE)

##Group data by intervals (there are 288 5-minute intervals each day)
n_distinct(activity$interval)
intervalsteps<-group_by(activity, interval)

##Calculate average number of steps in each interval
avg_intervalsteps<-summarise(intervalsteps, mean(steps, na.rm=TRUE))
interval<-unlist(avg_intervalsteps[,1])
num_avg<-unlist(avg_intervalsteps[,2])
##Create plot
plot(interval, num_avg, type="l", 
     main = "Average Daily Activity Pattern", 
     sub = "From Oct 1, 2012 to Nov 30, 2012", 
     xlab = "5-Minute Interval Throughout Day", 
     ylab="Average Number of Steps")

##Find interval with maximum average value
filter(avg_intervalsteps, avg_intervalsteps[,2]==max(avg_intervalsteps[,2]))

##Calculate and report total number of missing values
sum(!complete.cases(activity))

##Replace NA step values with average steps for that interval
impute<-mutate(intervalsteps, interval.avg = mean(steps, na.rm=TRUE))
step.na <- is.na(impute$steps)
impute$steps[step.na] <- impute$interval.avg[step.na]

##Create new data set equal to original data set but with missing data filled in
activity<-select(impute, steps, date, interval)

##Make histogram and calculate mean and median

dailysteps<-group_by(activity, date)

#Generate sum of steps for each day for use in histogram
total_dailysteps<-summarise(dailysteps, sum(steps))
num_total<-unlist(total_dailysteps[,2])
hist(num_total, main = "Histogram of Daily Step Total 
     with NA step values imputed from interval averages",
     xlab ="Daily Step Totals")

##Calculate mean and median
mean(num_total, na.rm=TRUE)
median(num_total, na.rm=TRUE)

##Due to the imputed values, the mean and median are now identical
##With the NAs replaced, the frequencies for the average are higher, 
##because days that were missing values now have values equal to the average values

##Separate weekdays and weekends and create as factor
activity$sort<-ifelse(!weekdays(as.Date(activity$date)) %in% 
                              c("Saturday", "Sunday"),"weekday","weekend")
activity$sort<-as.factor(activity$sort)

##Calculate average activity by weekend or weekday for each interval
groupedsteps<-group_by(activity, sort, interval)
totalgroupedsteps<-summarise(groupedsteps, mean(steps))
sort<-unlist(totalgroupedsteps[,1])
interval<-unlist(totalgroupedsteps$interval)
avg_steps<-unlist((totalgroupedsteps$mean))
intervaldf<-data.frame(sort, interval, avg_steps)
xyplot(avg_steps ~ interval | sort, 
       intervaldf, layout = c(1, 2), type = "b", pch = "", 
       xlab ="5-Minute Interval Throughout Day", 
       ylab ="Average Number of Steps", 
       main = "Average Daily Activity Pattern")
