library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(httr)
library(lubridate)
library(forecast)

# Lookup table for Body_Part codes and their corresponding labels
body_part_labels <- data.frame(
  Code = as.numeric(c(33, 80, 37, 94, 32, 77, 76, 92, 83, 82, 75, 0, 35, 36, 81, 88, 89, 38, 30, 93, 79, 31, 34, 84, 85, 87)),
  Label = c(
    "Arm, lower (not including elbow or wrist)", 
    "Arm, upper", 
    "Ankle", 
    "Ear", 
    "Elbow", 
    "Eyeball", 
    "Face (including eyelid, eye area and nose)", 
    "Finger", 
    "Foot", 
    "Hand", 
    "Head", 
    "Internal (use with aspiration and ingestion)", 
    "Knee", 
    "Leg, lower (not including knee or ankle)", 
    "Leg, upper", 
    "Mouth (including lips, tongue and teeth)", 
    "Neck", 
    "Pubic region", 
    "Shoulder (including clavicle, collarbone)", 
    "Toe", 
    "Trunk, lower", 
    "Trunk, upper (not including shoulders)", 
    "Wrist", 
    "25-50% of body (used for burns only)", 
    "All parts of body (more than 50% of body)", 
    "Not recorded"
  )
)

# Function to fetch and process injury data from multiple URLs
fetch_all_injury_data <- function() {
  file_urls <- c(
    "https://raw.githubusercontent.com/jonathancdb/NEISS/main/neiss2022.csv.zip",
    "https://raw.githubusercontent.com/jonathancdb/NEISS/main/neiss2021.csv.zip",
    "https://raw.githubusercontent.com/jonathancdb/NEISS/main/neiss2020.csv.zip"
  )
  
  combined_data <- data.frame()
  
  for (url in file_urls) {
    temp_zip <- tempfile(fileext = ".zip")
    temp_dir <- tempfile()  # Create a unique temp directory for each file
    dir.create(temp_dir)
    
    response <- httr::GET(url, httr::write_disk(temp_zip, overwrite = TRUE))
    if (httr::status_code(response) != 200) {
      stop(paste("Failed to fetch data from", url))
    }
    unzip(temp_zip, exdir = temp_dir)
    csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
    print(csv_file) 
    if (length(csv_file) == 0) {
      stop(paste("No CSV file found in the ZIP archive from", url))
    }
    
    injury_data <- read_csv(csv_file[1]) %>%
      mutate(Body_Part = as.numeric(Body_Part))
    
    combined_data <- bind_rows(combined_data, injury_data)
    unlink(temp_dir, recursive = TRUE)
  }
  
  return(combined_data)
}

perform_sarima_forecasting <- function(data, h = 1) {
  sarima_fit <- auto.arima(data, seasonal = TRUE, seasonal.test = "ocsb", stepwise = FALSE)
  forecasted_values <- forecast(sarima_fit, h = h)
  
  # Calculate accuracy if you have actual values to compare against
  accuracy_measures <- if (h <= length(data)) accuracy(forecasted_values, data[(length(data)-h+1):length(data)])
  
  # Returning model statistics as part of the list
  list(
    Forecast = forecasted_values,
    Stats = list(
      AIC = sarima_fit$aic,
      BIC = sarima_fit$bic,
      Accuracy = accuracy_measures  # Assuming 'accuracy' includes metrics like MAE, RMSE, etc.
    ),
    Model = sarima_fit
  )
}

ui <- fluidPage(
  titlePanel("NEISS: Injury Trends Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("body_part", "Select a Body Part:", choices = NULL, selected = NULL),
      dateRangeInput("date_range", "Select Date Range:", start = NULL, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd"),
      actionButton("last_month", "Last Month"),
      actionButton("last_3_months", "Last 3 Months"),
      actionButton("last_6_months", "Last 6 Months"),
      actionButton("max_time", "ALL")
    ),
    mainPanel(
      h3("Timeseries Visualization"),
      plotOutput("timeSeriesPlot"),
      h4("SARIMA Model Statistics"),
      p("Seasonal AutoRegressive Integrated Moving Average (SARIMA) is a statistical model used to forecast time series data that exhibits both non-seasonal and seasonal patterns."),
      verbatimTextOutput("modelStats"),
      h3("Descriptive Statistics"),
      h4("Patient Demographics"),
      tableOutput("statsTable"),
      h4("Patient Disposition"),
      p("Patient dispositions, as defined by the NEISS Coding Manual are as follows:",
        tags$ul(
          tags$li("1: Treated and released, or examined and released without treatment"),
          tags$li("2: Treated and transferred to another hospital"),
          tags$li("4: Treated and admitted for hospitalization (within same facility)"),
          tags$li("5: Held for observation (includes admitted for observation)"),
          tags$li("6: Left without being seen, Left against medical advice, Left without treatment, Eloped"),
          tags$li("8: Fatality, includes dead on arrival (“DOA”), died in the ED, and died after admission"),
          tags$li("9: Not recorded")
        )
      ),
      plotOutput("dispositionPlot"),
      h4("Authors"),
      div(style="font-size:14px; font-family:Arial, sans-serif;",
          p("Jonathan Collard de Beaufort, BS", tags$sup("1"))
      ),
      tags$ol(
        tags$li("Norton College of Medicine, SUNY Upstate, Syracuse NY")
      ),
      h4("Citation"),
      p(
        tags$ul(
          tags$li("US Consumer Product Safety Commission. National Electronic Injury Surveillance System (NEISS) Available at: https://www.cpsc.gov/Research–Statistics/NEISS-Injury-Data. Accessed January 11, 2025.")
        )  
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive function to fetch and process all injury data
  injury_data <- reactive({
    
    data <- fetch_all_injury_data()
    
    data <- data %>%
      left_join(body_part_labels, by = c("Body_Part" = "Code")) %>%
      mutate(Treatment_Date = as.Date(Treatment_Date, format = "%m/%d/%y"))
    if (!"Label" %in% colnames(data)) {
      stop("Failed to map Body_Part codes to labels.")
    }
    return(data)
  })
  
  # Populate UI elements based on available data
  observe({
    data <- injury_data()
    updateSelectInput(session, "body_part", choices = unique(data$Label), selected = unique(data$Label)[1])
    updateDateRangeInput(session, "date_range",
                         min = min(data$Treatment_Date, na.rm = TRUE),
                         max = max(data$Treatment_Date, na.rm = TRUE),
                         start = min(data$Treatment_Date, na.rm = TRUE),
                         end = max(data$Treatment_Date, na.rm = TRUE)
    )
  })
  
  # Date range controls
  observeEvent(input$last_month, {
    data <- injury_data()
    last_date <- max(data$Treatment_Date, na.rm = TRUE)
    updateDateRangeInput(session, "date_range", start = last_date - 30, end = last_date)
  })
  
  observeEvent(input$last_3_months, {
    data <- injury_data()
    last_date <- max(data$Treatment_Date, na.rm = TRUE)
    updateDateRangeInput(session, "date_range", start = last_date - 90, end = last_date)
  })
  
  observeEvent(input$last_6_months, {
    data <- injury_data()
    last_date <- max(data$Treatment_Date, na.rm = TRUE)
    updateDateRangeInput(session, "date_range", start = last_date - 180, end = last_date)
  })
  
  observeEvent(input$max_time, {
    data <- injury_data()
    oldest_date <- min(data$Treatment_Date, na.rm = TRUE)
    last_date <- max(data$Treatment_Date, na.rm = TRUE)
    updateDateRangeInput(session, "date_range", start = oldest_date, end = last_date)
  })
  
  # Render the timeseries plot
  output$timeSeriesPlot <- renderPlot({
    req(input$body_part, input$date_range)  # Ensure required inputs are available
    
    # Filter and summarize the training data
    filtered_data <- injury_data() %>%
      filter(Label == input$body_part, Treatment_Date >= input$date_range[1], Treatment_Date <= input$date_range[2]) %>%
      group_by(Treatment_Date) %>%
      summarize(Frequency = n(), .groups = "drop") %>%
      ungroup() %>%
      as.data.frame()  # Ensure this is a data frame
    
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return nothing if there's no data
    }
    
    # Define the split point for in-sample and out-of-sample data
    split_point <- floor(nrow(filtered_data) * 0.8)
    train_data <- filtered_data[1:split_point, ]
    test_data <- filtered_data[(split_point + 1):nrow(filtered_data), ]
    
    # Ensure both are data frames
    train_data <- as.data.frame(train_data)
    test_data <- as.data.frame(test_data)
    
    # Perform SARIMA forecasting on in-sample data
    results <- tryCatch({
      perform_sarima_forecasting(train_data$Frequency, h = nrow(test_data))  # Specify horizon h
    }, error = function(e) {
      print(paste("Error in SARIMA computation:", e$message))
      NULL  # Return NULL on error
    })
    
    if (is.null(results) || is.null(results$Forecast)) {
      return(NULL)  # Return nothing if forecasting failed
    }
    
    # Forecast dates should only include the test range
    forecast_dates <- test_data$Treatment_Date
    
    # Create the plot with both observed and forecasted data
    p <- ggplot() +
      geom_line(data = train_data, aes(x = Treatment_Date, y = Frequency), color = "blue", linewidth = 0.5) +  # Observed data
      geom_line(data = test_data, aes(x = Treatment_Date, y = Frequency), color = "black", linewidth = 1, linetype = "dashed") +  # Actual test data
      geom_line(aes(x = forecast_dates, y = results$Forecast$mean), color = "red", linewidth = 1) +  # Forecasted data
      geom_ribbon(aes(x = forecast_dates, ymin = results$Forecast$lower[, "80%"], ymax = results$Forecast$upper[, "80%"]), fill = "red", alpha = 0.2) +  # Confidence interval
      labs(title = paste("SARIMA Model Forecast vs Actual Data for", input$body_part), x = "Year-Month", y = "Number of Injuries") +
      theme_minimal()
    
    return(p)  # Return the plot
  })
  
  output$dispositionPlot <- renderPlot({
    req(input$body_part, input$date_range)  # Ensure required inputs are available
    
    disposition_data <- injury_data() %>%
      filter(Label == input$body_part, Treatment_Date >= input$date_range[1], Treatment_Date <= input$date_range[2]) %>%
      group_by(Disposition) %>%
      summarize(count = n())  # Count the number of occurrences for each disposition
    
    disposition_data$Disposition <- factor(disposition_data$Disposition)  # Convert Disposition to a factor
    
    disposition_plot <- ggplot(disposition_data, aes(x = Disposition, y = count)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Patient Disposition", x = "Disposition", y = "Frequency") +
      theme_minimal() +
      scale_x_discrete(drop = FALSE)  # Optional: to include all possible dispositions in the x-axis
    
    return(disposition_plot)
  })
  
  # Render SARIMA model statistics
  output$modelStats <- renderPrint({
    req(input$body_part, input$date_range)
    
    # Filter and summarize the training data
    filtered_data <- injury_data() %>%
      filter(Label == input$body_part, Treatment_Date >= input$date_range[1], Treatment_Date <= input$date_range[2]) %>%
      group_by(Treatment_Date) %>%
      summarize(Frequency = n(), .groups = "drop") %>%
      ungroup()
    
    if (nrow(filtered_data) == 0) {
      return("No data available for the selected range and body part.")
    }
    
    # Define the split point for in-sample and out-of-sample data
    split_point <- floor(nrow(filtered_data) * 0.8)
    train_data <- filtered_data[1:split_point, ]
    test_data <- filtered_data[(split_point + 1):nrow(filtered_data), ]
    
    # Perform SARIMA forecasting
    results <- tryCatch({
      perform_sarima_forecasting(train_data$Frequency, h = nrow(test_data))
    }, error = function(e) {
      return(paste("Error in SARIMA computation:", e$message))
    })
    
    if (is.null(results)) {
      return("Failed to compute SARIMA model.")
    }
    
    # Extract and display model statistics
    stats <- results$Stats
    cat("SARIMA Model Statistics:\n")
    cat("AIC:", stats$AIC, "\n")
    cat("BIC:", stats$BIC, "\n")
    if (!is.null(stats$Accuracy)) {
      cat("Accuracy Metrics (for forecasted data):\n")
      print(stats$Accuracy)
    } else {
      cat("No accuracy metrics available.\n")
    }
  })
  
  
  # Render the descriptive statistics table
  output$statsTable <- renderTable({
    req(input$date_range)
    filtered_data <- injury_data() %>%
      filter(Treatment_Date >= input$date_range[1], Treatment_Date <= input$date_range[2])
    
    stats <- data.frame(
      Metric = c("Average Age", "SD of Age", "Total Patients", "Unique Races"),
      Value = c(
        mean(filtered_data$Age, na.rm = TRUE),
        sd(filtered_data$Age, na.rm = TRUE),
        nrow(filtered_data),
        length(unique(filtered_data$Race))
      )
    )
    return(stats)
  })
  
  
}

shinyApp(ui, server)
