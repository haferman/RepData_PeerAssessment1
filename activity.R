## See full description at https://github.com/haferman/ExData_Project2
##

# Loading and preprocessing the data (ignore missing)
# 1. Load the data (i.e. read.csv())
# 2. Process/transform the data (if necessary) into a format suitable 
#    for your analysis

# move into working directory and download dataset
setwd("~/Z-R/RepData")
if(!file.exists('activity.zip')){
  url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url,destfile = "activity.zip")
}

## uncompress file if necessary and read data
if (!file.exists("activity.csv")) { unzip("activity.zip") }
a_data <- read.table("activity.csv", sep=",", header=T)

# What is mean total number of steps taken per day?
# 1. Calculate the total number of steps taken per day
# 2. If you do not understand the difference between a histogram and a barplot, 
#    research the difference between them. Make a histogram of the total number 
#    of steps taken each day
# 3. Calculate and report the mean and median of the total number 
#    of steps taken per day

nsteps <- aggregate(steps ~ date, data = a_data, sum, na.rm = TRUE)
hist(nsteps$steps,main="Distribution of Total Steps per day",
     xlab="Total Steps per day")
mu_steps <- mean(nsteps$steps)
med_steps <- median(nsteps$steps)

# What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval 
#    (x-axis) and the average number of steps taken, averaged across 
#    all days (y-axis)
# 2. Which 5-minute interval, on average across all the days in the dataset,
#    contains the maximum number of steps?

steps2 <- aggregate(steps ~ interval, data = a_data, mean, 
                            na.rm = TRUE)
plot(steps ~ interval, data = steps2, type = "l", 
     main = "Average number of steps at 5 minute Intervals",
     xlab = "5-minute Time Intervals", 
     ylab = "Average number of steps")
max <- steps2[which.max(steps2$steps),]

# Imputing missing values
# Note that there are a number of days/intervals where there are 
# missing values (coded as NA). The presence of missing days may 
# introduce bias into some calculations or summaries of the data.
# 1. Calculate and report the total number of missing values in the 
#    dataset (i.e. the total number of rows with NAs)
# 2. Devise a strategy for filling in all of the missing values in the dataset.
#    The strategy does not need to be sophisticated. For example, you could 
#    use the mean/median for that day, or the mean for that 5-minute interval,
#    etc.
# 3. Create a new dataset that is equal to the original dataset but with 
#    the missing data filled in.
# 4. Make a histogram of the total number of steps taken each day and 
#    Calculate and report the mean and median total number of steps taken 
#    per day. Do these values differ from the estimates from the first part
#    of the assignment? What is the impact of imputing missing data on the 
#    estimates of the total daily number of steps?
nmiss <- sum(!complete.cases(a_data))

# now impute data. copy a_data to b_data, loop through b_data and
# impute mean value to any missing values
b_data <- a_data
for (i in 1:nrow(b_data)) {
  if (is.na(b_data$steps[i])) {
    idx <- b_data$interval[i]
    x <- steps2[steps2$interval==idx,"steps"]
    b_data$steps[i] <- x
  }
}

nsteps_b <- aggregate(steps ~ date, data = b_data, sum)
hist(nsteps_b$steps,main="Distribution of Total Steps per day",
     xlab="Total Steps per day")
mu_steps_b <- mean(nsteps_b$steps)
med_steps_b <- median(nsteps_b$steps)
summary(nsteps$steps)
sd(nsteps$steps)
summary(nsteps_b$steps)
sd(nsteps_b$steps)

# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.
# 1. Create a new factor variable in the dataset with two levels – 
#    “weekday” and “weekend” indicating whether a given date is a weekday 
#    or weekend day.
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of 
#    the 5-minute interval (x-axis) and the average number of steps taken, 
#    averaged across all weekday days or weekend days (y-axis). See the 
#    README file in the GitHub repository to see an example of what this 
#    plot should look like using simulated data.
b_data$daytype <- as.factor(ifelse(weekdays(as.Date(b_data$date)) %in% 
                                  c("Saturday","Sunday"),"weekend", "weekday"))

library("lattice")
j <- aggregate(steps ~ interval + daytype, b_data, mean)

myplot <- xyplot(steps ~ interval | factor(daytype), data = j, aspect = 0.5, 
             type = "l")
print(myplot)
