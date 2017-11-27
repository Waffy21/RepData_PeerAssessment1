---
title: "PA1_template"
output:
  html_document: default
  pdf_document: default
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)


```

```{r PA1_template}


##1.Code for reading in the dataset and/or processing the data
week2 <- read.csv("activity.csv")


week2$date <- as.POSIXct(strptime(week2$date,"%F"))
stepByDay <- summarise(group_by(week2, date), sumStep=sum(steps))

##2.Histogram of the total number of steps taken each day
hist(stepByDay$sumStep)


##3.Mean and median number of steps taken each day

meanStepByDay <- summarise(group_by(week2, date), steps=mean(steps), interval=0)

medianStepByDay <- summarise(group_by(week2, date), steps=median(steps), interval=0)

print(meanStepByDay)
print(medianStepByDay)


##4.Time series plot of the average number of steps taken
with(meanStepByDay, plot(date,steps))



##5.The 5-minute interval that, on average, contains the maximum number of steps

maxStepByInterval <- summarise(group_by(week2, interval), steps=sum(steps), date=0)
##View(maxStepByInterval[maxStepByInterval$steps==max(maxStepByInterval$steps)]$interval)



##6.Code to describe and show a strategy for imputing missing data
sum(is.na(week2$steps)) ##number of rows with NA

week2fixed <- week2

 for(i in 1:nrow(week2fixed)){
     if(is.na(week2fixed$steps[i])){
         week2fixed$steps[i] <- mean(na.omit(week2fixed[week2fixed$interval==week2fixed$interval[i],]$steps))
      }
 }

meanStepByDayFixed <- summarise(group_by(week2fixed, date), steps=mean(steps), interval=0)

medianStepByDayFixed <- summarise(group_by(week2fixed, date), steps=median(steps), interval=0)

sum(meanStepByDayFixed$steps)
sum(na.omit(meanStepByDay$steps))
sum(medianStepByDayFixed$steps)
sum(na.omit(medianStepByDay$steps))


##7.Histogram of the total number of steps taken each day after missing values are imputed

stepByDay <- summarise(group_by(week2fixed, date), sumStep=sum(steps))

hist(stepByDay$sumStep)


##8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

  
stepSabado <- week2fixed[weekdays(week2fixed$date)=="sábado",]
stepDomingo <- week2fixed[weekdays(week2fixed$date)=="domingo",]
weekend2 <- rbind(stepSabado,stepDomingo)

meanStepByIntervalWeekend <- summarise(group_by(weekend2, interval), steps=mean(steps), date=0)



stepLunes <- week2fixed[weekdays(week2fixed$date)=="lunes",]
stepMartes <- week2fixed[weekdays(week2fixed$date)=="martes",]
stepMiercoles <- week2fixed[weekdays(week2fixed$date)=="miércoles",]
stepJueves <- week2fixed[weekdays(week2fixed$date)=="jueves",]
stepViernes <- week2fixed[weekdays(week2fixed$date)=="viernes",]

weekday2 <- rbind(stepLunes,stepMartes,stepMiercoles,stepJueves,stepViernes)
meanStepByIntervalWeekday <- summarise(group_by(weekday2, interval), steps=mean(steps), date=0)

par(mfrow = c(1, 2))
with(meanStepByIntervalWeekday, plot(interval,steps))
with(meanStepByIntervalWeekend, plot(interval,steps))
```
