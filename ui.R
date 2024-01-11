library(shiny)
library(shinydashboard)
library(plotly)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Bike Sharing Data Exploration", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trend Exploration", tabName = "trend_exploration", icon = icon("line-chart")),
      menuItem("Statistical Exploration", tabName = "stat_exploration", icon = icon("line-chart")),
      menuItem("Usage Patterns Exploration", tabName = "usage_patterns", icon = icon("bicycle")),
      menuItem("Weather Impact Exploration", tabName = "weather_impact", icon = icon("cloud")),
      menuItem("Hypothesis Testing", tabName = "hypothesis_testing", icon = icon("area-chart")),
      menuItem("Predictive Modeling", tabName = "predictive_modeling", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      # Trend Exploration
      tabItem(tabName = "trend_exploration",
              fluidRow(
                box(title = "Seasonal Variation in Bike Rentals", plotlyOutput("seasonalVariationPlot"), width = 12),
                box(title = "Yearly Trend in Bike Rentals", plotlyOutput("yearlyTrendPlot"), width = 12),
                box(title = "Bike Rentals: Holidays vs Regular Days", plotlyOutput("holidaysVsRegularPlot"), width = 12),
                box(title = "20-Day Moving Average of Bike Rentals", plotlyOutput("movingAveragePlot"), width = 12),
                box(title = "Average Bike Rentals per Hour", plotlyOutput("avgRentalsPerHourPlot"), width = 12)
              )
      ),
      # Statistical Exploration
      tabItem(tabName = "stat_exploration",
              fluidRow(
                box(title = "Average and Standard Deviation of Bike Usage Per Month", plotOutput("pMonthPlot"), width = 12),
                box(title = "Average and Standard Deviation of Bike Usage Per Season", plotOutput("pSeasonPlot"), width = 12),
                box(title = "Mean and Standard Deviation of Bike Usage by Hour", plotOutput("pHourPlot"), width = 12),
                box(title = "Mean and Standard Deviation of Bike Usage by Weather Condition", plotOutput("pWeatherPlot"), width = 12),
                box(title = "Boxplot of Hourly Bike Count", plotOutput("pOutliersPlot"), width = 12),
                box(title = "Boxplot of Daily Bike Count", plotOutput("pDailyOutliersPlot"), width = 12),
                box(title = "Correlation Analysis", plotOutput("correlationPlot"), width = 12)
              ),
              fluidRow(
                box(title = "Average and Standard Deviation Per Day", tableOutput("avgSdPerDayPlot"), width = 12),
                box(title = "Average and Standard Deviation Per Month", tableOutput("avgSdPerMonthPlot"), width = 12),
                box(title = "Average and Standard Deviation Per Season", tableOutput("avgSdPerSeasonPlot"), width = 12),
                box(title = "Statistics by Hour", tableOutput("statsByHourPlot"), width = 12),
                box(title = "Statistics by Weather Condition", tableOutput("statsByWeatherPlot"), width = 12)
              )
      ),
      
      # Usage Patterns Exploration
      tabItem(tabName = "usage_patterns",
              fluidRow(
                box(title = "Bike Usage Between Subscribers and Casual Riders", tableOutput("userTypeSummary"), width = 12),
                box(title = "Bike Rental Patterns by User Type", plotlyOutput("p9Plot"), width = 12),
                box(title = "Weekly Bike Rental Patterns", plotlyOutput("p10Plot"), width = 12),
                box(title = "Seasonal Bike Rental Patterns by User Type", plotlyOutput("p11Plot"), width = 12),
                box(title = "Proportion of Bike Rentals by User Type", plotlyOutput("p11bPlot"), width = 12),
                box(title = "Hourly Bike Usage by Weather Situation", plotlyOutput("p23Plot"), width = 12),
                box(title = "Registered vs Casual Usage Patterns", plotlyOutput("p25Plot"), width = 12)
              )
      ),
      
      # Weather Impact Exploration
      tabItem(tabName = "weather_impact",
              fluidRow(
                box(title = "Impact of Temperature on Bike Rentals", plotlyOutput("p12Plot"), width = 12),
                box(title = "Relationship Between Humidity and Bike Rentals", plotlyOutput("p14Plot"), width = 12),
                box(title = "Effect of Windspeed on Bike Rentals", plotlyOutput("p15Plot"), width = 12)
                #box(title = "Hourly Bike Usage by Weather Situation", plotlyOutput("p23Plot"), width = 12)
              )
      ),
      
      # Hypothesis Testing
      tabItem(tabName = "hypothesis_testing",
              fluidRow(
                box(title = "Clear vs Adverse Weather", plotlyOutput("p16Plot"), verbatimTextOutput("tTestWeather"), width = 12),
                box(title = "Holidays vs Non-Holidays", plotlyOutput("p17Plot"), verbatimTextOutput("tTestHoliday"), width = 12),
                box(title = "Year 2011 vs 2012", plotlyOutput("p18Plot"), verbatimTextOutput("tTestYear"), width = 12),
                box(title = "Bike Rentals Across Different Seasons", plotlyOutput("p19Plot"), verbatimTextOutput("anovaSeasons"), width = 12),
                box(title = "Comparison of Bike Rentals: Registered vs Casual Users", plotlyOutput("p20Plot"), verbatimTextOutput("tTestUsers"), width = 12)
              )
      ),
      
      # Predictive Modeling (Placeholder for future development)
      tabItem(tabName = "predictive_modeling",
              fluidRow(
                box(title = "GAM Model Plot", plotOutput("gamModelPlot"), width = 12),
                box(title = "GAM Model Summary", verbatimTextOutput("gamModelSummary"), width = 12),
                box(title = "GAM Model Metrics", verbatimTextOutput("gamModelMetrics"), width = 12),
                box(title = "ARIMA Model Summary", verbatimTextOutput("arimaModelSummary"), width = 12),
                box(title = "ARIMA Model Plot", plotOutput("arimaModelPlot"), width = 12),
                box(title = "ARIMA Model Metrics", verbatimTextOutput("arimaModelMetrics"), width = 12),
                box(title = "Prophet Model Plot", plotOutput("prophetModelPlot"), width = 12),
                box(title = "Prophet Model Summary", verbatimTextOutput("prophetModelSummary"), width = 12)
              )
      )
    )
  )
)
