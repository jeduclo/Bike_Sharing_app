library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(data.table) # Ensure this package is loaded for fread

server <- function(input, output, session) {
  # Function to load day data
  load_day_data <- function() {
    req(file.exists("day_data.csv")) 
    return(fread("day_data.csv"))
  }
  
  # Function to load hour data
  load_hour_data <- function() {
    req(file.exists("hour_data.csv")) 
    return(fread("hour_data.csv"))
  }
  
  # Seasonal Variation in Bike Rentals
  output$seasonalVariationPlot <- renderPlotly({
    day_data <- load_day_data() # Load data
    p1 <- ggplot(day_data, aes(x = season, y = cnt)) +
      geom_boxplot(color = 'navy') +
      labs(title = "Seasonal Variation in Bike Rentals", x = "Season", y = "Total Rentals")
    ggplotly(p1)
  })
  
  # Yearly Trend in Bike Rentals
  output$yearlyTrendPlot <- renderPlotly({
    day_data <- load_day_data()
    p2 <- ggplot(day_data, aes(x = dteday, y = cnt, group = year)) +
      geom_line(color = 'navy') +
      labs(title = "Yearly Trend in Bike Rentals", x = "Date", y = "Total Rentals")
    ggplotly(p2)
  })
  
  
  # Bike Rentals: Holidays vs Regular Days
  output$holidaysVsRegularPlot <- renderPlotly({
    day_data <- load_day_data()
    p4 <- ggplot(day_data, aes(x = holiday, y = cnt)) +
      geom_boxplot(color = 'navy') +
      labs(title = "Bike Rentals: Holidays vs Regular Days", x = "Holiday", y = "Total Rentals")
    ggplotly(p4)
  })
  
  # 20-Day Moving Average of Bike Rentals
  output$movingAveragePlot <- renderPlotly({
    day_data <- load_day_data()
    day_data$ma20 <- stats::filter(day_data$cnt, rep(1/20, 20), sides = 2)
    p5 <- ggplot(day_data, aes(x = dteday, y = cnt)) +
      geom_line(color = 'lightblue') +
      geom_line(aes(y = ma20), color = 'red') +
      labs(title = '20-Day Moving Average of Bike Rentals', x = 'Date', y = 'Total Rentals')
    ggplotly(p5)
  })
  
  # Average Bike Rentals per Hour
  output$avgRentalsPerHourPlot <- renderPlotly({
    hour_data <- load_hour_data()
    hourly_avg <- hour_data %>%
      group_by(hour) %>%
      summarize(avg_rentals = mean(cnt))
    p6 <- ggplot(hourly_avg, aes(x = hour, y = avg_rentals)) +
      geom_line(color = 'navy') +
      labs(title = "Average Bike Rentals per Hour", x = "Hour of Day", y = "Average Rentals")
    ggplotly(p6)
  })
  
  ################################################ Stat Exploration

  # Statistical Exploration Outputs
  output$avgSdPerDayPlot <- renderTable({
    day_data <- load_day_data()
    day_data %>% 
      summarise(average_count = mean(cnt), sd_count = sd(cnt))
  })
  
  output$avgSdPerMonthPlot <- renderTable({
    day_data <- load_day_data()
    day_data %>% 
      group_by(mnth) %>% 
      summarise(average_count = mean(cnt), sd_count = sd(cnt))
  })
  
  output$avgSdPerSeasonPlot <- renderTable({
    day_data <- load_day_data()
    day_data %>% 
      group_by(season) %>% 
      summarise(average_count = mean(cnt), sd_count = sd(cnt))
  })
  
  output$statsByHourPlot <- renderTable({
    hour_data <- load_hour_data()
    hour_data %>% 
      group_by(hr) %>% 
      summarise(mean = mean(cnt), std_dev = sd(cnt))
  })
  
  output$statsByWeatherPlot <- renderTable({
    hour_data <- load_hour_data()
    hour_data %>% 
      group_by(weathersit) %>% 
      summarise(mean = mean(cnt), std_dev = sd(cnt))
  })
  
  # Plots
  output$pMonthPlot <- renderPlot({
    day_data <- load_day_data()
    avg_sd_per_month <- day_data %>% 
      group_by(mnth) %>% 
      summarise(average_count = mean(cnt), sd_count = sd(cnt))
    ggplot(avg_sd_per_month, aes(x = mnth, y = average_count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_errorbar(aes(ymin = average_count - sd_count, ymax = average_count + sd_count), width = 0.4, color = "navy") +
      ggtitle("Average and Standard Deviation of Bike Usage Per Month") +
      xlab("Month") +
      ylab("Average Bike Count")
  })
  
  output$pSeasonPlot <- renderPlot({
    day_data <- load_day_data()
    avg_sd_per_season <- day_data %>% 
      group_by(season) %>% 
      summarise(average_count = mean(cnt), sd_count = sd(cnt))
    ggplot(avg_sd_per_season, aes(x = season, y = average_count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_errorbar(aes(ymin = average_count - sd_count, ymax = average_count + sd_count), width = 0.4, color = "navy") +
      ggtitle("Average and Standard Deviation of Bike Usage Per Season") +
      xlab("Season") +
      ylab("Average Bike Count")
  })
  
  output$pHourPlot <- renderPlot({
    hour_data <- load_hour_data()
    stats_by_hour <- hour_data %>% 
      group_by(hr) %>% 
      summarise(mean = mean(cnt), std_dev = sd(cnt))
    ggplot(stats_by_hour, aes(x = hr)) +
      geom_line(aes(y = mean), color = "skyblue") +
      geom_errorbar(aes(ymin = mean - std_dev, ymax = mean + std_dev), width = 0.1, color = "navy") +
      ggtitle("Mean and Standard Deviation of Bike Usage by Hour") +
      xlab("Hour of the Day") +
      ylab("Bike Count")
  })
  
  output$pWeatherPlot <- renderPlot({
    hour_data <- load_hour_data()
    stats_by_weather <- hour_data %>% 
      group_by(weathersit) %>% 
      summarise(mean = mean(cnt), std_dev = sd(cnt))
    ggplot(stats_by_weather, aes(x = as.factor(weathersit), y = mean)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_errorbar(aes(ymin = mean - std_dev, ymax = mean + std_dev), width = 0.2, color = "navy") +
      ggtitle("Mean and Standard Deviation of Bike Usage by Weather Condition") +
      xlab("Weather Condition") +
      ylab("Bike Count")
  })
  
  output$pOutliersPlot <- renderPlot({
    hour_data <- load_hour_data()
    ggplot(hour_data, aes(x = as.factor(hr), y = cnt)) +
      geom_boxplot() +
      ggtitle("Boxplot of Hourly Bike Count") +
      xlab("Hour of the Day") +
      ylab("Bike Count")
  })
  
  output$pDailyOutliersPlot <- renderPlot({
    day_data <- load_day_data()
    ggplot(day_data, aes(x = as.factor(mnth), y = cnt)) +
      geom_boxplot() +
      ggtitle("Boxplot of Daily Bike Count") +
      xlab("Month of the Year") +
      ylab("Bike Count")
  })
  
  output$correlationPlot <- renderPlot({
    hour_data <- load_hour_data()
    cor_matrix <- cor(hour_data %>% select_if(is.numeric))
    corrplot(cor_matrix, method = "circle")
  })
  
  ########################################### Usage Pattern
  
  # Usage Pattern Exploration
  
  # Analysis of Bike Usage Between Subscribers and Casual Riders
  output$userTypeSummary <- renderTable({
    day_data <- load_day_data()
    day_data %>%
      group_by(user_type = if_else(casual > registered, "Casual", "Registered")) %>%
      summarize(count = sum(cnt))
  })
  
  # Bike Rental Patterns by User Type
  output$p9Plot <- renderPlotly({
    hour_data <- load_hour_data()
    hour_data %>%
      mutate(hour = as.factor(hour)) %>%
      gather(key = "user_type", value = "count", casual, registered) %>%
      ggplot(aes(x = hour, y = count, fill = user_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Bike Rental Patterns by User Type", x = "Hour of Day", y = "Number of Rentals")
  })
  
  # Weekly Bike Rental Patterns
  output$p10Plot <- renderPlotly({
    hour_data <- load_hour_data()
    ggplot(hour_data, aes(x = weekday, y = cnt)) +
      geom_line(stat = "summary", fun = "mean") +
      labs(title = "Weekly Bike Rental Patterns", x = "Day of Week", y = "Average Rentals")
  })
  
  # Seasonal Bike Rental Patterns by User Type
  output$p11Plot <- renderPlotly({
    day_data <- load_day_data()
    day_data %>%
      gather(key = "user_type", value = "count", casual, registered) %>%
      ggplot(aes(x = season, y = count, fill = user_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Seasonal Bike Rental Patterns by User Type", x = "Season", y = "Number of Rentals")
  })
  
  # Pie chart
  output$p11bPlot <- renderPlotly({
    hour_data <- load_hour_data()
    pie_data <- hour_data %>%
      summarise(casual = sum(casual), registered = sum(registered)) %>%
      gather(key = "user_type", value = "count", casual, registered)
    plot_ly(pie_data, labels = ~user_type, values = ~count, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = 'Proportion of Bike Rentals by User Type')
  })
  
  # Bike usage by weather situation
  output$p23Plot <- renderPlotly({
    hour_data <- load_hour_data()
    usage_by_weather <- hour_data %>% 
      group_by(weathersit) %>% 
      summarise(average_count = mean(cnt))
    ggplot(usage_by_weather, aes(x = weathersit, y = average_count, fill = weathersit)) + 
      geom_bar(stat = "identity") +
      ggtitle("Hourly Bike Usage by Weather Situation") +
      xlab("Weather Situation") +
      ylab("Average Count")
  })
  
  # Differences in usage patterns between Registered and Casual users
  output$p25Plot <- renderPlotly({
    hour_data <- load_hour_data()
    avg_usage_by_user_type <- hour_data %>% 
      group_by(hr) %>% 
      summarise(average_registered = mean(registered), 
                average_casual = mean(casual))
    ggplot(avg_usage_by_user_type) + 
      geom_line(aes(x = hr, y = average_registered, color = "Registered")) +
      geom_line(aes(x = hr, y = average_casual, color = "Casual")) +
      ggtitle("Registered vs Casual Usage Patterns") +
      xlab("Hour of the Day") +
      ylab("Average Count")
  })
  
  ################################################ 
  
  # Weather Pattern Exploration
  
  # Load pre-computed plots
  p12_plot <- readRDS("p12_plot.rds")
  p14_plot <- readRDS("p14_plot.rds")
  p15_plot <- readRDS("p15_plot.rds")
  p23_plot <- readRDS("p23_plot.rds") 
  
  # Display the plots in your Shiny UI
  
  output$p12Plot <- renderPlotly({
    p12_plot
  })
  
  output$p14Plot <- renderPlotly({
    p14_plot
  })
  
  output$p15Plot <- renderPlotly({
    p15_plot
  })
  
  output$p23Plot <- renderPlotly({
     p23_plot
  })
  
  ########################################################
  
  # Hypothesis Testing
  
  # Clear vs Adverse Weather
  output$tTestWeather <- renderPrint({
    day_data <- load_day_data()
    aov(cnt ~ weathersit, data = day_data)
  })
  
  output$p16Plot <- renderPlotly({
    day_data <- load_day_data()
    ggplot(day_data, aes(x = weathersit, y = cnt, fill = weathersit)) +
      geom_boxplot() +
      labs(title = "Bike Rentals: Clear vs Adverse Weather", x = "Weather Type", y = "Total Rentals")
  })
  
  # Holidays vs Non-Holidays
  output$tTestHoliday <- renderPrint({
    day_data <- load_day_data()
    t.test(cnt ~ holiday, data = day_data)
  })
  
  output$p17Plot <- renderPlotly({
    day_data <- load_day_data()
    ggplot(day_data, aes(x = holiday, y = cnt, fill = holiday)) +
      geom_boxplot() +
      labs(title = "Bike Rentals: Holidays vs Non-Holidays", x = "Holiday", y = "Total Rentals")
  })
  
  # Year 2011 vs 2012
  output$tTestYear <- renderPrint({
    day_data <- load_day_data()
    t.test(cnt ~ yr, data = day_data)
  })
  
  output$p18Plot <- renderPlotly({
    day_data <- load_day_data()
    ggplot(day_data, aes(x = yr, y = cnt, fill = yr)) +
      geom_boxplot() +
      labs(title = "Bike Rentals: 2011 vs 2012", x = "Year", y = "Total Rentals")
  })
  
  # Seasonal Analysis
  output$anovaSeasons <- renderPrint({
    day_data <- load_day_data()
    aov(cnt ~ season, data = day_data)
  })
  
  output$p19Plot <- renderPlotly({
    day_data <- load_day_data()
    ggplot(day_data, aes(x = season, y = cnt, fill = season)) +
      geom_boxplot() +
      labs(title = "Bike Rentals Across Different Seasons", x = "Season", y = "Total Rentals")
  })
  
  # Casual vs Registered Users
  output$tTestUsers <- renderPrint({
    day_data <- load_day_data()
    t.test(day_data$casual, day_data$registered)
  })
  
  output$p20Plot <- renderPlotly({
    day_data <- load_day_data()
    ggplot(day_data, aes(x = factor(1), y = casual, fill = "Casual")) +
      geom_boxplot() +
      geom_boxplot(aes(y = registered, fill = "Registered")) +
      labs(title = "Comparison of Bike Rentals: Registered vs Casual Users", x = "", y = "Total Rentals")
  })
  
  ######################################################################

  # Load pre-computed models
  gam_model <- readRDS("gam_model.rds")
  arima_model <- readRDS("arima_model.rds")
  m <- readRDS("prophet_model.rds")
  
  # Predictive Modeling
  
  # Function definitions for RMSE and MAE
  rmse <- function(actual, predicted) {
    sqrt(mean((actual - predicted) ^ 2))
  }
  mae <- function(actual, predicted) {
    mean(abs(actual - predicted))
  }
  
  # GAM (Generalized Additive Model)
  # GAM (Generalized Additive Model)
  output$gamModelSummary <- renderPrint({
    summary(gam_model)
  })
  
  output$gamModelPlot <- renderPlot({
    plot(gam_model)
  })
  
  output$gamModelMetrics <- renderPrint({
    day_data <- load_day_data() # Ensure this function is defined to load your day_data
    predictions_gam <- predict(gam_model, newdata = day_data, type = "response")
    rmse_gam <- rmse(day_data$cnt, predictions_gam)
    mae_gam <- mae(day_data$cnt, predictions_gam)
    paste("GAM - RMSE:", rmse_gam, "\nGAM - MAE:", mae_gam)
  })

  
  # ARIMA
  output$arimaModelSummary <- renderPrint({
    summary(arima_model)
  })
  
  output$arimaModelPlot <- renderPlot({
    forecast_arima <- forecast(arima_model, h = 30) # For example, forecasting the next 30 days
    plot(forecast_arima)
  })
  
  output$arimaModelMetrics <- renderPrint({
    day_data <- load_day_data() # Make sure this loads the correct time series data
    cnt_ts <- ts(day_data$cnt, start = c(year(min(day_data$dteday)), month(min(day_data$dteday))), frequency = 365)
    forecast_arima <- forecast(arima_model, h = 30)
    actual_arima <- cnt_ts
    predicted_arima <- forecast_arima$mean
    len <- min(length(actual_arima), length(predicted_arima))
    actual_arima <- actual_arima[1:len]
    predicted_arima <- predicted_arima[1:len]
    rmse_arima <- rmse(actual_arima, predicted_arima)
    mae_arima <- mae(actual_arima, predicted_arima)
    paste("ARIMA - RMSE:", rmse_arima, "\nARIMA - MAE:", mae_arima)
  })
  
  # Prophet
  output$prophetModelSummary <- renderPrint({
    future <- make_future_dataframe(m, periods = 365) # Forecasting the next 365 days
    forecast <- predict(m, future)
    summary(forecast)
  })
  
  output$prophetModelPlot <- renderPlot({
    future <- make_future_dataframe(m, periods = 365)
    forecast <- predict(m, future)
    plot(m, forecast)
  })
  
}







