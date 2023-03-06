# make sure that you're in the directory where you want to work
if (file.exists("activity.csv")==0) {
  unzip("activity.zip")
}
d <- read.csv("activity.csv");
library(dplyr)
bydate <- group_by(d,date)
steps_per_day <- summarize(bydate,steps=sum(steps,na.rm=TRUE))
hist(steps_per_day$steps,xlab="Number of steps",main="Histogram of steps per day")
median_steps <- median(steps_per_day$steps)
mean_steps <- mean(steps_per_day$steps)
num_missing = nrow(d)- sum(complete.cases(d))
byinterval <- group_by(d,interval)
steps_by_interval = summarize(byinterval,steps=mean(steps,na.rm=TRUE))
plot(steps_by_interval,type="l",ylab="Average steps in interval")
max_steps_idx = which.max(steps_by_interval$steps)
max_steps_interval = steps_by_interval$interval[max_steps_idx]
tweekend <- function(day) {
  if (day %in% c("Saturday","Sunday")) "Weekend" else "Weekday"
}
# get the day of the week
dow <- weekdays(as.Date.factor(d$date))
# get whether its a weekday or weekend
x <- sapply(dow, tweekend, USE.NAMES = FALSE)
# turn it into a factor
daytyp <- factor(x)
# add it to the data
d <- mutate(d,daytyp)
