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
    "https://raw.githubusercontent.com/jonathancdb/NEISS/main/neiss2020.csv.zip",
    "https://raw.githubusercontent.com/jonathancdb/NEISS/main/neiss2019.csv.zip"
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
  sarima_fit <- auto.arima(
    data, 
    seasonal = TRUE,         # Include seasonal components
    stepwise = FALSE,        # Perform an exhaustive search
    approximation = FALSE    # Ensure accuracy, no approximations
  )
  
  # Past model attempts: 
  #auto.arima(data, seasonal = TRUE, seasonal.test = "ocsb", stepwise = FALSE)
  
  
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
      selectInput("body_part", "Select a Body Part:", choices = NULL, selected = "Head"),
      dateRangeInput("date_range", "Select Date Range:", start = NULL, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd"),
      actionButton("last_month", "Last Month"),
      actionButton("last_3_months", "Last 3 Months"),
      actionButton("last_6_months", "Last 6 Months"),
      actionButton("max_time", "ALL")
    ),
    mainPanel(
      h3("Project Synopsis"),
      p("The National Electronic Injury Surveillance System (NEISS) provides detailed data on consumer product-related injuries and deaths in the U.S. While this data is publicly accessible online, it is stored in large files that can be challenging to handle. This web application addresses these challenges by offering pre-built analysis and customizable statistics, making the data more accessible for individuals, researchers, and hospital administrators."),
      p("NEISS collects data on injuries from participating hospitals across the United States. To protect patient confidentiality, location details are excluded from the reports. However, basic patient demographics, such as age and gender, along with the disposition—such as whether the patient was treated and released or admitted—are included."),
      h3("Timeseries Visualization"),
      plotOutput("timeSeriesPlot"),
      h3("SARIMA Visualization"),
      plotOutput("sarimaPlot"),
      h4("SARIMA Model Statistics"),
      p("Seasonal AutoRegressive Integrated Moving Average (SARIMA) is a statistical model used to forecast time series data that exhibits both non-seasonal and seasonal patterns."),
      verbatimTextOutput("modelStats"),
      h5("SARIMA Model Explanations (click here to expand)", style="cursor: pointer;", onclick="$('#sarimaExplanations').toggle();"),
      conditionalPanel(
        condition = "false",  # Initially hidden
        id = "sarimaExplanations",
        tags$ul(
          tags$li("AIC: Measures the quality of the model with a penalty for the number of parameters."),
          tags$li("BIC: Similar to AIC but with a stronger penalty, favoring simpler models."),
          tags$li("ME: Mean Error - Indicates the average forecast bias."),
          tags$li("RMSE: Root Mean Squared Error - Measures the average magnitude of the forecast error."),
          tags$li("MAE: Mean Absolute Error - Averages the absolute forecast errors."),
          tags$li("MPE: Mean Percentage Error - Indicates the percent bias of the model."),
          tags$li("MAPE: Mean Absolute Percentage Error - Indicates accuracy as a percentage."),
          tags$li("MASE: Compares the model's MAE against a naive model."),
          tags$li("ACF1: Autocorrelation at lag 1, helps detect any need for further differencing.")
        )
      ),
      h3("Descriptive Statistics"),
      h4("Patient Demographics"),
      tableOutput("statsTable"),
      h4("Patient Disposition Histogram"),
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
      h4("Patient Disposition Timeseries"),
      plotOutput("dispositionTimeseries"),
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
    
    filtered_data <- injury_data() %>%
      filter(Label == input$body_part, Treatment_Date >= input$date_range[1], Treatment_Date <= input$date_range[2]) %>%
      group_by(Treatment_Date) %>%
      summarize(Frequency = n(), .groups = "drop") %>%
      ungroup() %>%
      as.data.frame()  # Ensure this is a data frame
    
    if (nrow(filtered_data) == 0) {
      return(NULL)  # Return nothing if there's no data
    }
    
    p <- ggplot() +
      geom_line(data = filtered_data, aes(x = Treatment_Date, y = Frequency), color = "blue", linewidth = 0.5) +  # Observed data
      labs(title = paste("Timeseries Plot", input$body_part), x = "Year-Month", y = "Number of Injuries") +
      theme_minimal()
    
    return(p)
    
  })
  
  # Reactive function that generates the SARIMA model
  
  sarimaResults <- reactive({
    req(input$body_part)  # Ensure the input is available
    
    filtered_data <- injury_data() %>%
      filter(Label == input$body_part) %>%
      mutate(Treatment_Date = ymd(Treatment_Date)) %>%  # Convert to Date if necessary
      mutate(YearMonth = floor_date(Treatment_Date, "month")) %>%  # Extract Year-Month
      group_by(YearMonth) %>%
      summarize(Frequency = n(), .groups = "drop") %>%
      ungroup() %>%
      as.data.frame()
    
    if (nrow(filtered_data) == 0) {
      return(list(dataAvailable = FALSE))  # Return indication that no data is available
    }
    
    split_point <- floor(nrow(filtered_data) * 0.8)
    train_data <- filtered_data[1:split_point, ]
    test_data <- filtered_data[(split_point + 1):nrow(filtered_data), ]
    
    model_results <- tryCatch({
      perform_sarima_forecasting(train_data$Frequency, h = length(test_data$Frequency))
    }, error = function(e) {
      print(paste("Error in SARIMA computation:", e$message))
      NULL
    })
    
    list(dataAvailable = TRUE, train_data = train_data, test_data = test_data, results = model_results)
  })
  
  # Render the SARIMA plot
  output$sarimaPlot <- renderPlot({
    sarima_output <- sarimaResults()  # Get the data and results from the reactive expression
    if (!sarima_output$dataAvailable || is.null(sarima_output$results) || is.null(sarima_output$results$Forecast)) {
      return(NULL)
    }
    
    forecast_dates <- sarima_output$test_data$Treatment_Date
    
    if (!"Treatment_Date" %in% names(sarima_output$train_data) || !"Treatment_Date" %in% names(sarima_output$test_data)) {
      print("Treatment_Date not found in the dataset.")
      print(names(sarima_output$train_data))
      return(NULL)
    }
    
    ggplot() +
      geom_line(data = sarima_output$train_data, aes(x = YearMonth, y = Frequency), color = "blue", linewidth = 0.5) +
      geom_line(data = sarima_output$test_data, aes(x = YearMonth, y = Frequency), color = "black", linewidth = 1, linetype = "dashed") +
      geom_line(aes(x = forecast_dates, y = sarima_output$results$Forecast$mean), color = "red", linewidth = 1) +
      geom_ribbon(aes(x = forecast_dates, ymin = sarima_output$results$Forecast$lower[, "80%"], ymax = sarima_output$results$Forecast$upper[, "80%"]), fill = "red", alpha = 0.2) +
      labs(title = paste("SARIMA Model Forecast vs Actual Data for", input$body_part), x = "Year-Month", y = "Number of Injuries") +
      theme_minimal()
  })
  
  # Render SARIMA model statistics
  output$modelStats <- renderPrint({
    sarima_output <- sarimaResults()  # Get the data and results from the reactive expression
    
    # Check if data was available and if results are not null
    if (!sarima_output$dataAvailable || is.null(sarima_output$results)) {
      cat("No SARIMA model results available.")
      return()
    }
    
    # Assuming 'Stats' is part of the results object returned from the forecasting function
    # You might need to adjust this if the structure of results from perform_sarima_forecasting() is different
    stats <- sarima_output$results$Stats  # Access the stats part of the results
    
    cat("SARIMA Model Statistics:\n")
    if (!is.null(stats$AIC)) {
      cat("AIC:", stats$AIC, "\n")
    } else {
      cat("AIC not available.\n")
    }
    if (!is.null(stats$BIC)) {
      cat("BIC:", stats$BIC, "\n")
    } else {
      cat("BIC not available.\n")
    }
    
    if (!is.null(stats$Accuracy)) {
      cat("Accuracy Metrics (for forecasted data):\n")
      print(stats$Accuracy)
    } else {
      cat("No accuracy metrics available.\n")
    }
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
  
  output$dispositionTimeseries <- renderPlot({
    req(input$body_part)  # Ensure required inputs are available
    
    disposition_data <- injury_data() %>%
      filter(Label == input$body_part) %>%
      group_by(Treatment_Date, Disposition) %>%
      summarise(count = n(), .groups = 'drop') %>%
      as.data.frame()
    
    if (nrow(disposition_data) == 0) {
      return(NULL)  # Return nothing if there's no data
    }
    
    disposition_data$Disposition <- factor(disposition_data$Disposition, levels = unique(disposition_data$Disposition))
    
    disposition_plot <- ggplot(disposition_data, aes(x = Treatment_Date, y = count, group = Disposition, color = Disposition)) +
      geom_line(linewidth = 0.5) +
      labs(title = "Timeseries of Patient Disposition", x = "Time", y = "Frequency") +
      theme_minimal()
    
    return(disposition_plot)
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
