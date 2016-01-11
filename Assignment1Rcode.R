##Set the working directory to desired folder
setwd("d:\\datascience\\reproducibleresearch\\Assignment1\\repdata_peerassessment1")

##Loading the data into R
activity_data<-read.csv("activity.csv")

##Calculating the total number of steps taken per day
agg_data<-aggregate(steps~date,data=activity_data,sum)

##Making a histogram of the total number of steps taken each day
hist(agg_data$steps,col=2,main="Histogram of Total Number of Steps Taken per Day",xlab="Total number of steps in a day")

##calculating and reporting mean and median of steps taken each day
step_median<-median(agg_data$steps)
print(paste("step_median:",step_median))
step_mean<-mean(agg_data$steps)
print(paste("step_mean:",step_mean))

##calculating average number of steps in each 5-minutes interval,across all days
aspi1<-aggregate(steps~interval,data=activity_data,FUN=mean) #aspi stands for Average Steps Per Interval

##converting 5-minutes intervals to 00:00 time format for better plotting
temp <- sprintf("%04d", aspi1$interval)
aspi1$dfi<-format(strptime(temp, format="%H%M"), format = "%H:%M") #dfi stands for Desired Format Intervals

##Make a time series plot (i.e. type = "l") of the 5-minute interval
##(x-axis) and the average number of steps taken, averaged across
##all days (y-axis)
plot(aspi1$interval,aspi1$steps,type="l",xaxt="n",main="Average Daily Activity Pattern",xlab="Time of the day",ylab="average number of steps across all days")
axis(1,at=aspi1$interval,labels=aspi1$dfi)

##reporting 5_minutes interval that contains the maximum number of steps(average across all days)
max_steps<-max(aspi1$steps)
max_interval<-aspi1[which(aspi1$steps==max_steps),]
print(paste("5_min Interval that contains the maximum number of steps:",max_interval$dfi))
Print(paste("The total number of steps in this interval is:",max_step))
##Calculatinge and reporting the total number of missing values in the datase                                      
na_number<-sum(!complete.cases(activity_data))
print(paste("Total number of rows with N.A:",na_number))

##creating new dataset with filling in all of the missing values in the dataset with mean of the intervals
step_mean <- aggregate(steps ~ interval, data = activity_data,mean,na.rm=TRUE)
new_act_data<-activity_data
for (i in 1:nrow(activity_data)) {
  each_row <-activity_data[i, ]
  if (is.na(each_row$steps)) {
    each_row$steps <- step_mean[which(step_mean$interval==each_row$interval), 2]
  }
  new_act_data[i,]<-each_row
}

##Making a histogram of the total number of steps taken each day
new_agg_data<-aggregate(steps~date,data=new_act_data,sum,na.rm=TRUE)
hist(new_agg_data$steps,col=2,main="Histogram of Total Number of Steps Taken per Day",xlab="Total number of steps in a day")


##Calculating and reporting the mean and median total number of steps taken per day

new_step_median<-median(new_agg_data$steps,na.rm=TRUE)
print(paste("new_step_median:",new_step_median))
new_step_mean<-mean(new_agg_data$steps,na.rm=TRUE)
print(paste("new_step_mean:",new_step_mean))

##Creating a new factor variable in the dataset with two levels - "weekday" and "weekend"
new_act_data$date<-as.POSIXct(new_act_data$date)
weekday <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
new_act_data$WDay<-factor((weekdays(new_act_data$date)%in%weekday),levels=c(FALSE,TRUE),labels=c("weekend","weekday"))

##Make a panel plot containing a time series plot (i.e. type = "l") 
##of the 5-minute interval (x-axis) and the average number of steps taken,
##averaged across all weekday days or weekend days (y-axis).

aspi2<-aggregate(steps~interval+WDay,data=new_act_data,FUN=mean)
library(lattice)
p2<-xyplot(steps~interval|WDay,data=aspi2,type="l",layout=c(1,2),xlab="Interval",ylab="Number of Steps")
print(p2)