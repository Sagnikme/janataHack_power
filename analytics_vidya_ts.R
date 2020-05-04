rm(list = ls())

##############################################
## package
##############################################

install.packages("tidyverse") 
library(xts)
library(lubridate)
library(dplyr)
library(magrittr)
library(knitr)
library(chron)
library(tidyverse)
library(gmodels)
install.packages("xts")
install.packages("forecast", dependencies = TRUE)
library(forecast)
library(ggplot2)
data(mpg, package="ggplot2")
theme_set(theme_bw())

##############################################
## import csv
##############################################

cv <- read.csv("train_6BJx641.csv")
class(cv)

##############################################
## Separate Time
##############################################

data_2 = cv%>%mutate(start_time = strftime(cv$datetime , tz="GMT", format = "%H:%M:%S"))
class(data_2$start_time)
data_2 = data_2%>%mutate(start_time = chron(times = start_time))

##############################################
## Add new End Time to the data
##############################################

end_time <- data_2$start_time
data_2 = data_2%>%mutate(end_time = chron(times = end_time))

##############################################
## Trends
##############################################

start_stamp <- data_2$start_time
df <- data.frame(start_stamp)

##############################################
## looping
##############################################

diff_val <- 0
for (i in 1:26495) {
  diff_val[i] <- df$start_stamp[i+1] - df$start_stamp[i]
}
diff_value <- data.frame(diff_val)
diff_value <- rbind(diff_value,diff_val = 0)  
tail(diff_value)
class(diff_value)
diff_value = diff_value%>%mutate(diff_val = chron(times = diff_val))
class(diff_value$diff_val)
diff_value = diff_value%>%mutate(diff_val = as.numeric(diff_value$diff_val))
class(diff_value$diff_val)

##############################################
## convert back to time
##############################################

diff_value = diff_value%>%mutate(diff_val =format(as.POSIXct((diff_value$diff_val) * 86400,
                                                      origin = "1970-01-01",
                                                      tz = "UTC"),
                                                      "%H:%M:%S"))

##############################################
## Separate
##############################################

data <- data.frame(data_2$datetime,df$start_stamp,data_2$temperature,data_2$var1,data_2$pressure,data_2$windspeed,data_2$var2,data_2$electricity_consumption,diff_value$diff_val)
colnames(data)
names(data)[1] <- "date"
names(data)[2] <- "start"
names(data)[3] <- "temp"
names(data)[4] <- "var1"
names(data)[5] <- "pressure"
names(data)[6] <- "windspeed"
names(data)[7] <- "var2"
names(data)[8] <- "power"
names(data)[9] <- "diff"

##############################################
## Separate
##############################################

day <- as.Date(data$date)
day <- data.frame(day)
new_data <- data.frame(day$day,data$start,data$temp,data$var1,data$pressure,data$windspeed,data$var2,data$power,data$diff)
names(new_data)[1] <- "date"
names(new_data)[2] <- "start"
names(new_data)[3] <- "temp"
names(new_data)[4] <- "var1"
names(new_data)[5] <- "pressure"
names(new_data)[6] <- "windspeed"
names(new_data)[7] <- "var2"
names(new_data)[8] <- "power"
names(new_data)[9] <- "diff"

##############################################
## Test
##############################################

tser_sagnik <- ts(new_data, freq = 24)
tser_sagnik
tail(tser_sagnik)

##############################################
## predictioncfiting in arima
##############################################

tser_new <- data.frame(tser_sagnik[,2],tser_sagnik[,1],tser_sagnik[,8])
names(tser_new)[1] <- "start"
names(tser_new)[2] <- "date"
names(tser_new)[3] <- "power"

##############################################
## Daily Forcast Arima
##############################################

power_am <- auto.arima(tser_new[,3])
predict_2 <- forecast(power_am,h = 365)
ts.plot(predict_2, col = c("blue"))




