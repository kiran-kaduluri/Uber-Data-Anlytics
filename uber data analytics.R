# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

# Define valid login credentials
valid_username <- "kiran"
valid_password <- "123"

# Define colors for plots
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

# Read the data for each month
apr <- read.csv("~/uber dataset/uber-raw-data-apr14.csv")
may <- read.csv("~/uber dataset/uber-raw-data-may14.csv")
june <- read.csv("~/uber dataset/uber-raw-data-jun14.csv")
july <- read.csv("~/uber dataset/uber-raw-data-jul14.csv")
aug <- read.csv("~/uber dataset/uber-raw-data-aug14.csv")
sept <- read.csv("~/uber dataset/uber-raw-data-sep14.csv")

# Combine the data together
data <- rbind(apr, may, june, july, aug, sept)

# Preprocess data
data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data$Date.Time <- ymd_hms(data$Date.Time)
data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label=TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label=TRUE))
data$second = factor(second(hms(data$Time)))
data$minute = factor(minute(hms(data$Time)))
data$hour = factor(hour(hms(data$Time)))

# UI
ui <- fluidPage(
  
  # Add custom CSS for styling
  tags$head(
    tags$style(HTML("
      /* Center the login box on the screen */
      .login-box {
        width: 350px;
        margin: 100px auto;
        padding: 30px;
        background-color: #f9f9f9;
        border: 1px solid #ccc;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      }
      /* Style the title */
      .login-box h3 {
        text-align: center;
        color: #333;
        margin-bottom: 20px;
        font-family: Arial, sans-serif;
      }
      /* Style the input fields */
      .login-box input[type='text'],
      .login-box input[type='password'] {
        width: 100%;
        padding: 10px;
        margin: 10px 0;
        border: 1px solid #ccc;
        border-radius: 5px;
        font-size: 16px;
      }
      /* Style the login button */
      .login-box button {
        width: 100%;
        padding: 10px;
        background-color: #28a745;
        border: none;
        color: white;
        font-size: 16px;
        border-radius: 5px;
        cursor: pointer;
      }
      .login-box button:hover {
        background-color: #218838;
      }
      /* Error message styling */
      .error-message {
        color: red;
        text-align: center;
        margin-top: 10px;
        font-size: 14px;
      }
    "))
  ),
  
  # Login UI
  uiOutput("login_ui"),
  
  # Main Dashboard UI (conditionally shown after login)
  conditionalPanel(
    condition = "output.logged_in == true",
    dashboardPage(
      dashboardHeader(title = "Uber Data Analysis"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Hourly Trips", tabName = "hourly", icon = icon("clock")),
          menuItem("Daily Trips", tabName = "daily", icon = icon("calendar")),
          menuItem("Monthly Trips", tabName = "monthly", icon = icon("calendar-alt")),
          menuItem("Heat Maps", tabName = "heatmaps", icon = icon("chart-area"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "hourly",
                  fluidRow(
                    box(title = "Trips Every Hour", status = "primary", solidHeader = TRUE, 
                        plotOutput("hourlyPlot")),
                    box(title = "Hourly Data", status = "primary", solidHeader = TRUE,
                        DTOutput("hourlyData"))
                  )
          ),
          tabItem(tabName = "daily",
                  fluidRow(
                    box(title = "Trips by Day of the Month", status = "primary", solidHeader = TRUE, 
                        plotOutput("dailyPlot"))
                  )
          ),
          tabItem(tabName = "monthly",
                  fluidRow(
                    box(title = "Trips in a Month", status = "primary", solidHeader = TRUE, 
                        plotOutput("monthlyPlot"))
                  )
          ),
          tabItem(tabName = "heatmaps",
                  fluidRow(
                    box(title = "Heat Map by Hour and Day", status = "primary", solidHeader = TRUE, 
                        plotOutput("heatmapHourDay")),
                    box(title = "Heat Map by Month and Day", status = "primary", solidHeader = TRUE, 
                        plotOutput("heatmapMonthDay"))
                  )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to keep track of login status
  logged_in <- reactiveVal(FALSE)
  
  # Render login UI
  output$login_ui <- renderUI({
    if (!logged_in()) {
      # Show login page if not logged in
      tagList(
        div(
          class = "login-box",
          h3("Login"),
          textInput("username", "Username", placeholder = "Enter Username"),
          passwordInput("password", "Password", placeholder = "Enter Password"),
          actionButton("login_btn", "Login"),
          div(id = "error_message", class = "error-message")
        )
      )
    } else {
      # Return empty UI when logged in
      NULL
    }
  })
  
  # Login logic
  observeEvent(input$login_btn, {
    if (input$username == valid_username && input$password == valid_password) {
      logged_in(TRUE)  # Correct login
    } else {
      # Show error message
      output$error_message <- renderUI({
        div("Incorrect username or password. Please try again.", class = "error-message")
      })
    }
  })
  
  # Output to control the conditional display of the dashboard
  output$logged_in <- reactive({
    logged_in()
  })
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)
  
  # Hourly Trips Plot
  hourly_data <- data %>% 
    group_by(hour) %>% 
    dplyr::summarize(Total = n())
  
  output$hourlyPlot <- renderPlot({
    ggplot(hourly_data, aes(hour, Total)) + 
      geom_bar(stat="identity", fill="steelblue", color="red") + 
      ggtitle("Trips Every Hour") + 
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5)) + 
      scale_y_continuous(labels=comma)
  })
  
  # Hourly Data Table
  output$hourlyData <- renderDT({
    datatable(hourly_data)
  })
  
  # Daily Trips Plot
  day_data <- data %>% group_by(day) %>% dplyr::summarize(Trips = n())
  
  output$dailyPlot <- renderPlot({
    ggplot(day_data, aes(day, Trips)) + 
      geom_bar(stat = "identity", fill = "steelblue") +
      ggtitle("Trips by Day of the Month") + 
      theme(legend.position = "none") + 
      scale_y_continuous(labels = comma)
  })
  
  # Monthly Trips Plot
  month_data <- data %>% group_by(month) %>% dplyr::summarize(Total = n())
  
  output$monthlyPlot <- renderPlot({
    ggplot(month_data, aes(month, Total, fill = month)) + 
      geom_bar(stat = "identity") + 
      ggtitle("Trips in a Month") + 
      theme(legend.position = "none") + 
      scale_y_continuous(labels = comma) + 
      scale_fill_manual(values = colors)
  })
  
  # Heat Map by Hour and Day
  day_hour_data <- data %>% group_by(day, hour) %>% dplyr::summarize(Total = n())
  
  output$heatmapHourDay <- renderPlot({
    ggplot(day_hour_data, aes(day, hour, fill = Total)) + 
      geom_tile(color = "white") + 
      ggtitle("Heat Map by Hour and Day")
  })
  
  # Heat Map by Month and Day
  month_day_data <- data %>% group_by(month, day) %>% dplyr::summarize(Trips = n())
  
  output$heatmapMonthDay <- renderPlot({
    ggplot(month_day_data, aes(day, month, fill = Trips)) + 
      geom_tile(color = "white") + 
      ggtitle("Heat Map by Month and Day")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

