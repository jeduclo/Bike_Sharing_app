# Load the required packages
if (!require("shiny")) install.packages("shiny")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("plotly")) install.packages("plotly")
if (!require("readr")) install.packages("readr")
if (!require("forecast")) install.packages("forecast")
if (!require("zoo")) install.packages("zoo")
if (!require("stats")) install.packages("stats")
if (!require("mgcv")) install.packages("mgcv")
if (!require("forecast")) install.packages("forecast")
if (!require("prophet")) install.packages("prophet")
if (!require("lubridate")) install.packages("lubridate")
if (!require("htmlwidgets")) install.packages("htmlwidgets")
library(htmlwidgets)
library(lubridate)
library(mgcv)
library(forecast)
library(prophet)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(readr)
library(forecast)
library(zoo)
library(stats)
library(utils)
library(corrplot)


# 1 - Download and Unzip the Dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip"
download.file(url, destfile = "Bike-Sharing-Dataset.zip")
unzip("Bike-Sharing-Dataset.zip", exdir = "Bike_Sharing_Data")

# 2 - Read the Data
hour_data <- read_csv("Bike_Sharing_Data/hour.csv")
day_data <- read_csv("Bike_Sharing_Data/day.csv")
head(hour_data)
head(day_data)

# Data Preprocessing
day_data$date <- as.Date(day_data$dteday)
day_data$year <- year(day_data$date)
day_data$month <- month(day_data$date)
hour_data$date <- as.Date(hour_data$dteday)
hour_data$hour <- hour_data$hr

# Converting factors
day_data$season <- factor(day_data$season, levels = 1:4, labels = c("Spring", "Summer", "Fall", "Winter"))
day_data$weathersit <- factor(day_data$weathersit, levels = 1:3, labels = c("Clear-Few clouds", "Mist-Cloudy", "Light Snow-Rain"))
hour_data$weathersit <- factor(hour_data$weathersit, levels = 1:4, labels = c("Clear-Few clouds", "Mist-Cloudy", "Light Snow-Rain","Heavy Rain-Ice Pallets"))
day_data$yr <- factor(day_data$yr, labels = c("2011", "2012"))
day_data$holiday <- factor(day_data$holiday, labels = c("No", "Yes"))
day_data$weekday <- factor(day_data$weekday, labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# Convert month numbers to abbreviated month names
day_data$mnth <- factor(day_data$mnth, labels = month.abb)
hour_data$mnth <- factor(hour_data$mnth, labels = month.abb)


write.csv(hour_data, "hour_data.csv")
write.csv(day_data, "day_data.csv")

# Script to Pre-compute Weather usage pattern Models and Save Results


# Script to Pre-compute Models and Save Results

# Impact of Temperature on Bike Rentals
p12 <- ggplot(day_data, aes(x = temp, y = cnt)) +
  geom_point(aes(color = temp)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Impact of Temperature on Bike Rentals", x = "Normalized Temperature", y = "Total Rentals")
saveRDS(ggplotly(p12), file = "p12_plot.rds")

# Relationship Between Humidity and Bike Rentals
p14 <- ggplot(day_data, aes(x = hum, y = cnt)) +
  geom_point(aes(color = hum)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between Humidity and Bike Rentals", x = "Normalized Humidity", y = "Total Rentals")
saveRDS(ggplotly(p14), file = "p14_plot.rds")

# Effect of Windspeed on Bike Rentals
p15 <- ggplot(day_data, aes(x = windspeed, y = cnt)) +
  geom_point(aes(color = windspeed)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Effect of Windspeed on Bike Rentals", x = "Normalized Windspeed", y = "Total Rentals")
saveRDS(ggplotly(p15), file = "p15_plot.rds")




# Function definitions for RMSE and MAE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}


# 1 - GAM Model
gam_model <- gam(cnt ~ s(temp) + s(hum) + s(windspeed) + factor(season) + factor(yr) + factor(weekday), 
                 data = day_data, 
                 family = poisson())
saveRDS(gam_model, file = "gam_model.rds")

# 2 - ARIMA Model
cnt_ts <- ts(day_data$cnt, start = c(year(min(day_data$dteday)), month(min(day_data$dteday))), frequency = 365)
arima_model <- auto.arima(cnt_ts)
saveRDS(arima_model, file = "arima_model.rds")

# 3 - Prophet Model
prophet_data <- data.frame(ds = day_data$dteday, y = day_data$cnt)
m <- prophet(prophet_data)
saveRDS(m, file = "prophet_model.rds")



