library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(httr)

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
    temp_dir <- tempdir()
    
    response <- httr::GET(url, httr::write_disk(temp_zip, overwrite = TRUE))
    if (httr::status_code(response) != 200) {
      stop(paste("Failed to fetch data from", url))
    }
    unzip(temp_zip, exdir = temp_dir)
    csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_file) == 0) {
      stop(paste("No CSV file found in the ZIP archive from", url))
    }
    
    injury_data <- read_csv(csv_file[1]) %>%
      mutate(Body_Part = as.numeric(Body_Part))
    
    combined_data <- bind_rows(combined_data, injury_data)
  }
  
  return(combined_data)
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
      h3("Descriptive Statistics"),
      h4("Patient Demographics"),
      tableOutput("statsTable")
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
    req(input$body_part, input$date_range)
    filtered_data <- injury_data() %>%
      filter(Label == input$body_part, Treatment_Date >= input$date_range[1], Treatment_Date <= input$date_range[2]) %>%
      group_by(Treatment_Date) %>%
      summarize(Frequency = n(), .groups = "drop")
    if (nrow(filtered_data) == 0) return(NULL)
    ggplot(filtered_data, aes(x = Treatment_Date, y = Frequency)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = paste("Injury Trends for:", input$body_part), x = "Date", y = "Number of Injuries") +
      theme_minimal()
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
