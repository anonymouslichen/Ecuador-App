library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)

# --- Preprocessing ---

read_hobo_precip <- function(filepath) {
  # Line 1 is a Spanish title header; line 2 becomes column names
  df <- read.csv(filepath, skip = 1, header = TRUE,
                 stringsAsFactors = FALSE, check.names = FALSE)

  # Identify columns by keyword (robust to varying HOBO serial-number suffixes)
  col_dt    <- names(df)[grepl("Fecha|Date",  names(df))][1]
  col_temp  <- names(df)[grepl("Temp",        names(df))][1]
  col_event <- names(df)[grepl("Event",       names(df))][1]

  df <- df[, c(col_dt, col_temp, col_event)]
  names(df) <- c("datetime_raw", "Temp_C", "Event")

  # Parse datetime: "01/13/25 02:30:00 p.m." -> POSIXct (GMT-5)
  dt_str <- gsub("p\\.m\\.", "PM", df$datetime_raw)
  dt_str <- gsub("a\\.m\\.", "AM", dt_str)
  df$datetime <- as.POSIXct(dt_str, format = "%m/%d/%y %I:%M:%S %p",
                             tz = "Etc/GMT+5")

  # Normalize tips: any event > 0 counts as exactly 1 tip
  df$Event <- as.numeric(df$Event)
  df$Event[is.na(df$Event)] <- 0
  df$Event[df$Event > 0] <- 1

  df[!is.na(df$datetime), ]
}

aggregate_hourly <- function(df) {
  df %>%
    mutate(hour = floor_date(datetime, "hour")) %>%
    group_by(hour) %>%
    summarise(
      Precip_mm = 0.2 * sum(Event, na.rm = TRUE),
      Temp_C    = mean(Temp_C,  na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(
      Year    = year(hour),
      MonthN  = month(hour, label = TRUE, abbr = TRUE),
      Day     = day(hour),
      Season  = case_when(
        month(hour) %in% c(12, 1, 2)  ~ "DJF",
        month(hour) %in% c(3,  4, 5)  ~ "MAM",
        month(hour) %in% c(6,  7, 8)  ~ "JJA",
        month(hour) %in% c(9, 10, 11) ~ "SON"
      )
    )
}

season_levels <- c("DJF", "MAM", "JJA", "SON")

# --- UI ---
ui <- page_sidebar(
  title = "HOBO Rain Gauge Explorer",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 300,
    fileInput("file", "Upload HOBO CSV", accept = ".csv"),
    hr(),
    selectInput("view", "View",
                choices = c("Hourly Time Series", "Binned Summary")),
    conditionalPanel(
      condition = "input.view == 'Binned Summary'",
      selectInput("bin_by", "Bin by",
                  choices = c("Month","Day of Month", "Season", "Year"))
    ),
    uiOutput("date_range_ui"),
    hr(),
    uiOutput("station_info")
  ),

  navset_card_tab(
    nav_panel("Plot",    plotlyOutput("main_plot",     height = "550px")),
    nav_panel("Summary", DTOutput("summary_table")),
    nav_panel("Data",    DTOutput("data_table"))
  )
)

# --- Server ---
server <- function(input, output, session) {

  hourly <- reactive({
    req(input$file)
    raw <- read_hobo_precip(input$file$datapath)
    aggregate_hourly(raw)
  })

  output$date_range_ui <- renderUI({
    req(hourly())
    rng <- range(hourly()$hour, na.rm = TRUE)
    dateRangeInput("date_range", "Date Range",
                   start = as.Date(rng[1]), end = as.Date(rng[2]),
                   min   = as.Date(rng[1]), max = as.Date(rng[2]))
  })

  output$station_info <- renderUI({
    req(hourly())
    df    <- hourly()
    total <- sum(df$Precip_mm, na.rm = TRUE)
    ndays <- as.numeric(diff(range(df$hour, na.rm = TRUE)), units = "days")
    tagList(
      tags$strong("Record summary"),
      tags$p(sprintf("Total: %.1f mm", total),
             style = "font-size:0.9em; margin-bottom:2px;"),
      tags$p(sprintf("Span: %.0f days", ndays),
             style = "font-size:0.9em; margin-bottom:0;")
    )
  })

  filtered <- reactive({
    req(hourly(), input$date_range)
    hourly() %>%
      filter(as.Date(hour) >= input$date_range[1],
             as.Date(hour) <= input$date_range[2])
  })

  output$main_plot <- renderPlotly({
    req(filtered())
    df <- filtered()

    if (input$view == "Hourly Time Series") {
      plot_ly(df, x = ~hour, y = ~Precip_mm,
              type = "bar",
              marker = list(color = "#2c7bb6")) %>%
        layout(
          xaxis  = list(title = "Date / Time"),
          yaxis  = list(title = "Precipitation (mm / hr)"),
          bargap = 0.05
        )

    } else {
      grouped <- switch(input$bin_by,

        "Month" = df %>%
          group_by(label = MonthN) %>%
          summarise(Total_mm = sum(Precip_mm, na.rm = TRUE), .groups = "drop"),

        "Day of Month" = df %>%
          group_by(label = factor(Day, levels = 1:31)) %>%
          summarise(Total_mm = sum(Precip_mm, na.rm = TRUE), .groups = "drop"),

        "Season" = df %>%
          group_by(label = factor(Season, levels = season_levels)) %>%
          summarise(Total_mm = sum(Precip_mm, na.rm = TRUE), .groups = "drop") %>%
          arrange(label),

        "Year" = df %>%
          group_by(label = factor(Year)) %>%
          summarise(Total_mm = sum(Precip_mm, na.rm = TRUE), .groups = "drop")
      )

      plot_ly(grouped, x = ~label, y = ~Total_mm,
              type = "bar",
              marker = list(color = "#2c7bb6")) %>%
        layout(
          xaxis  = list(title = input$bin_by),
          yaxis  = list(title = "Total Precipitation (mm)"),
          bargap = 0.2
        )
    }
  })

  output$summary_table <- renderDT({
    req(filtered())
    df <- filtered()
    rain_hours <- df$Precip_mm[df$Precip_mm > 0]
    summary_df <- data.frame(
      Metric = c(
        "Total precipitation (mm)",
        "Max hourly intensity (mm/hr)",
        "Mean intensity during rain (mm/hr)",
        "Hours with measurable rain",
        "Mean temperature (°C)",
        "Record start",
        "Record end"
      ),
      Value = c(
        round(sum(df$Precip_mm, na.rm = TRUE), 1),
        round(max(df$Precip_mm, na.rm = TRUE), 2),
        round(mean(rain_hours, na.rm = TRUE), 3),
        length(rain_hours),
        round(mean(df$Temp_C, na.rm = TRUE), 2),
        format(min(df$hour, na.rm = TRUE)),
        format(max(df$hour, na.rm = TRUE))
      ),
      stringsAsFactors = FALSE
    )
    datatable(summary_df,
              options = list(dom = "t", ordering = FALSE),
              rownames = FALSE)
  })

  output$data_table <- renderDT({
    req(filtered())
    filtered() %>%
      select(hour, Precip_mm, Temp_C) %>%
      datatable(
        colnames = c("Date / Time", "Precip (mm/hr)", "Temp (°C)"),
        options  = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE,
        filter   = "top"
      )
  })
}

shinyApp(ui, server)
