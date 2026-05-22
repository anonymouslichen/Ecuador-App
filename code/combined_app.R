library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)
library(tidyr)

# ── Constants ──────────────────────────────────────────────────────────────────

# Can change, this is how to column names appear in the data panel
tms_colnames <- c("index", "datetime", "timezone", "T1", "T2", "T3",
                   "soil_moisture_count", "shake", "errFlag")

# ── Translations ───────────────────────────────────────────────────────────────
# All user-facing strings are defined here.
# To translate the app, only edit the RIGHT-hand side strings in this block.
# Do NOT change the left-hand side names (e.g. lang$app_title) — those are
# used throughout the code and must stay as-is.

lang <- list(
  # App title
  app_title            = "Ecuador Sensor Data Explorer",

  # Sidebar
  upload_label         = "Upload CSV files",
  assign_header        = "Assign type and label per file:",
  assign_subtext       = "Select the sensor type and provide a short label for each file.",
  apply_button         = "Apply & Load Data",
  date_range_label     = "Date Range",

  # File type choices (left = display label, right = internal value — do NOT change right side)
  ftype_tomst          = "TOMST TMS-3",
  ftype_hobo           = "HOBO Rain Gauge",

  # Tab names
  tab_tomst            = "TOMST Sensors",
  tab_rain             = "Rain Gauges",
  tab_data             = "Data",
  tab_data_tomst       = "TOMST",
  tab_data_rain        = "Rain Gauges",

  # Plot controls
  plot_controls        = "Plot controls",
  variable_label       = "Variable",
  plot_type_label      = "Plot type",
  view_label           = "View",
  bin_by_label         = "Bin by",

  # TOMST variable choices (left = display label, right = internal value — do NOT change right side)
  var_T1               = "T1 (-6cm)",
  var_T2               = "T2 (+2cm)",
  var_T3               = "T3 (+15cm)",
  var_soil             = "Soil Moisture",

  # Plot type choices (left = display label, right = internal value — do NOT change right side)
  plot_timeseries      = "Time Series",
  plot_boxplot         = "Boxplot",
  plot_histogram       = "Histogram",

  # HOBO view choices (left = display label, right = internal value — do NOT change right side)
  view_hourly          = "Hourly Time Series",
  view_binned          = "Binned Summary",

  # Bin-by choices (left = display label, right = internal value — do NOT change right side)
  bin_month            = "Month",
  bin_day              = "Day of Month",
  bin_season           = "Season",
  bin_year             = "Year",

  # Plot axis / legend labels
  axis_datetime        = "Date / Time",
  axis_label           = "Label",
  axis_count           = "Count",
  axis_precip_hr       = "Precipitation (mm/hr)",
  axis_precip_total    = "Total Precipitation (mm)",
  legend_label         = "Label",
  legend_station       = "Station",

  # Station info card
  station_summaries    = "Station summaries",
  station_total_span   = "Total: %.1f mm  |  Span: %.0f days",

  # Data table column names
  col_station          = "Station",
  col_datetime         = "Date / Time",
  col_precip_hr        = "Precip (mm/hr)",
  col_temp             = "Temp (°C)",

  # Month/year picker label (shown when bin by = Day of Month)
  month_year_label     = "Month / Year",

  # Error message (note: file name and error details are appended automatically)
  error_reading        = "Error reading"
)

lang <- list(
  # App title
  app_title            = "Ecuador Sensor Data Explorer",

  # Sidebar
  upload_label         = "Upload CSV files",
  assign_header        = "Assign type and label per file:",
  assign_subtext       = "Select the sensor type and provide a short label for each file.",
  apply_button         = "Apply & Load Data",
  date_range_label     = "Date Range",

  # File type choices (left = display label, right = internal value — do NOT change right side)
  ftype_tomst          = "TOMST TMS-3",
  ftype_hobo           = "HOBO Rain Gauge",

  # Tab names
  tab_tomst            = "TOMST Sensors",
  tab_rain             = "Rain Gauges",
  tab_data             = "Data",
  tab_data_tomst       = "TOMST",
  tab_data_rain        = "Rain Gauges",

  # Plot controls
  plot_controls        = "Plot controls",
  variable_label       = "Variable",
  plot_type_label      = "Plot type",
  view_label           = "View",
  bin_by_label         = "Bin by",

  # TOMST variable choices (left = display label, right = internal value — do NOT change right side)
  var_T1               = "T1 (-6cm)",
  var_T2               = "T2 (+2cm)",
  var_T3               = "T3 (+15cm)",
  var_soil             = "Soil Moisture",

  # Plot type choices (left = display label, right = internal value — do NOT change right side)
  plot_timeseries      = "Time Series",
  plot_boxplot         = "Boxplot",
  plot_histogram       = "Histogram",

  # HOBO view choices (left = display label, right = internal value — do NOT change right side)
  view_hourly          = "Hourly Time Series",
  view_binned          = "Binned Summary",

  # Bin-by choices (left = display label, right = internal value — do NOT change right side)
  bin_month            = "Month",
  bin_day              = "Day of Month",
  bin_season           = "Season",
  bin_year             = "Year",

  # Plot axis / legend labels
  axis_datetime        = "Date / Time",
  axis_label           = "Label",
  axis_count           = "Count",
  axis_precip_hr       = "Precipitation (mm/hr)",
  axis_precip_total    = "Total Precipitation (mm)",
  legend_label         = "Label",
  legend_station       = "Station",

  # Station info card
  station_summaries    = "Station summaries",
  station_total_span   = "Total: %.1f mm  |  Span: %.0f days",

  # Data table column names
  col_station          = "Station",
  col_datetime         = "Date / Time",
  col_precip_hr        = "Precip (mm/hr)",
  col_temp             = "Temp (°C)",

  # Month/year picker label (shown when bin by = Day of Month)
  month_year_label     = "Month / Year",

  # Error message (note: file name and error details are appended automatically)
  error_reading        = "Error reading"
)

# Named choice vectors built from lang — do NOT edit directly, edit lang above
# Format: internal value with display label as name, built via setNames()
tomst_var_choices <- setNames(
  c("T1", "T2", "T3", "soil_moisture_count"),
  c(lang$var_T1, lang$var_T2, lang$var_T3, lang$var_soil)
)

plot_type_choices <- setNames(
  c("time_series", "boxplot", "histogram"),
  c(lang$plot_timeseries, lang$plot_boxplot, lang$plot_histogram)
)

hobo_view_choices <- setNames(
  c("hourly_ts", "binned_summary"),
  c(lang$view_hourly, lang$view_binned)
)

bin_by_choices <- setNames(
  c("month", "day_of_month", "season", "year"),
  c(lang$bin_month, lang$bin_day, lang$bin_season, lang$bin_year)
)

season_levels <- c("DJF", "MAM", "JJA", "SON")

# ── Parsing helpers ────────────────────────────────────────────────────────────

read_tomst <- function(filepath, label) {
  df <- read.csv(filepath, sep = ";", header = FALSE, stringsAsFactors = FALSE)
  colnames(df) <- tms_colnames
  df$datetime <- dmy_hm(df$datetime)
  df$label    <- label
  df
}

read_hobo_precip <- function(filepath, label) {
  df <- read.csv(filepath, skip = 1, header = TRUE,
                 stringsAsFactors = FALSE, check.names = FALSE)
  col_dt    <- names(df)[grepl("Fecha|Date",  names(df), ignore.case = TRUE)][1]
  col_temp  <- names(df)[grepl("Temp",        names(df), ignore.case = TRUE)][1]
  col_event <- names(df)[grepl("Event",       names(df), ignore.case = TRUE)][1]
  df        <- df[, c(col_dt, col_temp, col_event)]
  names(df) <- c("datetime_raw", "Temp_C", "Event")
  dt_str    <- gsub("p\\.m\\.", "PM", df$datetime_raw)
  dt_str    <- gsub("a\\.m\\.", "AM", dt_str)
  df$datetime <- as.POSIXct(dt_str, format = "%m/%d/%y %I:%M:%S %p",
                             tz = "Etc/GMT+5")
  df$Event <- as.numeric(df$Event)
  df$Event[is.na(df$Event)] <- 0
  df$Event[df$Event > 0]    <- 1
  df$label <- label
  df[!is.na(df$datetime), ]
}

aggregate_hobo_hourly <- function(df) {
  df %>%
    mutate(hour = floor_date(datetime, "hour")) %>% 
    group_by(label, hour) %>%
    summarise(
      Precip_mm = 0.2 * sum(Event,  na.rm = TRUE),
      Temp_C    = mean(Temp_C, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(
      Year   = year(hour), 
      MonthN = month(hour, label = TRUE, abbr = TRUE, locale  = "es_ES"),
      Day    = day(hour),
      Season = case_when(
        month(hour) %in% c(12, 1, 2)  ~ "DJF", #T = Match to season_levels (L 21)
        month(hour) %in% c(3,  4, 5)  ~ "MAM",
        month(hour) %in% c(6,  7, 8)  ~ "JJA",
        month(hour) %in% c(9, 10, 11) ~ "SON"
      )
    )
}

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = lang$app_title,
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 400,
    fileInput("files", lang$upload_label, multiple = TRUE, accept = ".csv"),
    uiOutput("file_assign_ui"),
    uiOutput("date_range_ui")
  ),

  navset_card_tab(

    nav_panel(
      lang$tab_tomst,
      layout_columns(
        col_widths = c(3, 9),
        card(
          card_header(lang$plot_controls),
          selectInput("tomst_var", lang$variable_label, choices = tomst_var_choices),
          selectInput("tomst_plot_type", lang$plot_type_label,
                      choices = plot_type_choices)
        ),
        plotlyOutput("tomst_plot", height = "540px")
      )
    ),

    nav_panel(
      lang$tab_rain,
      layout_columns(
        col_widths = c(3, 9),
        card(
          card_header(lang$plot_controls),
          selectInput("hobo_view", lang$view_label,
                      choices = hobo_view_choices),
          conditionalPanel(
            condition = "input.hobo_view == 'binned_summary'",
            selectInput("hobo_bin_by", lang$bin_by_label,
                        choices = bin_by_choices),
            conditionalPanel(
              condition = "input.hobo_bin_by == 'day_of_month'",
              uiOutput("hobo_month_year_ui")
            )
          ),
          hr(),
          uiOutput("hobo_station_info")
        ),
        plotlyOutput("hobo_plot", height = "540px")
      )
    ),

    nav_panel(
      lang$tab_data,
      navset_tab(
        nav_panel(lang$tab_data_tomst, DTOutput("tomst_data_table")),
        nav_panel(lang$tab_data_rain,  DTOutput("hobo_data_table"))
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── File assignment UI ──────────────────────────────────────────────────────

  output$file_assign_ui <- renderUI({
    req(input$files)
    fnames <- input$files$name
    tagList(
      tags$strong(lang$assign_header),
      tags$p(lang$assign_subtext,
             style = "font-size: 0.85em; color: #666; margin-bottom: 8px;"),
      lapply(seq_along(fnames), function(i) {
        tagList(
          tags$label(fnames[i], style = "font-weight: 600; font-size: 0.9em;"),
          fluidRow(
            column(5,
              selectInput(paste0("ftype_", i), label = NULL,
                          choices = setNames(c("TOMST", "HOBO"),
                                            c(lang$ftype_tomst, lang$ftype_hobo)))
            ),
            column(7,
              textInput(paste0("flabel_", i), label = NULL,
                        placeholder = paste("Label", i), 
                        value = tools::file_path_sans_ext(fnames[i]))
            )
          ),
          tags$hr(style = "margin: 4px 0;")
        )
      }),
      actionButton("apply_files", lang$apply_button,
                   class = "btn-primary w-100 mt-2")
    )
  })

  # ── Parse on Apply ──────────────────────────────────────────────────────────

  parsed <- eventReactive(input$apply_files, {
    req(input$files)
    n          <- nrow(input$files)
    tomst_list <- list()
    hobo_list  <- list()

    for (i in seq_len(n)) {
      ftype  <- input[[paste0("ftype_",  i)]]
      flabel <- trimws(input[[paste0("flabel_", i)]])
      if (nchar(flabel) == 0) flabel <- paste("File", i)
      fpath  <- input$files$datapath[i]

      tryCatch({
        if (ftype == "TOMST") {
          tomst_list[[length(tomst_list) + 1]] <- read_tomst(fpath, flabel)
        } else {
          hobo_list[[length(hobo_list) + 1]]   <- read_hobo_precip(fpath, flabel)
        }
      }, error = function(e) {
        showNotification(
          paste(lang$error_reading, input$files$name[i], ":", e$message),
          type = "error", duration = 8 
        )
      })
    }

    list(
      tomst = if (length(tomst_list) > 0) bind_rows(tomst_list) else NULL,
      hobo  = if (length(hobo_list)  > 0) aggregate_hobo_hourly(bind_rows(hobo_list)) else NULL
    )
  })

  tomst_raw   <- reactive({ parsed()$tomst })
  hobo_hourly <- reactive({ parsed()$hobo  })

  # ── Date range UI ──────────────────────────────────────────────────────────

  # Date labels are currently handled in as.Date(), need to change to translate
  output$date_range_ui <- renderUI({
    req(parsed())
    dates <- as.Date(character(0))
    if (!is.null(tomst_raw()))
      dates <- c(dates, as.Date(range(tomst_raw()$datetime,   na.rm = TRUE)))
    if (!is.null(hobo_hourly()))
      dates <- c(dates, as.Date(range(hobo_hourly()$hour, na.rm = TRUE)))
    req(length(dates) > 0)
    rng <- range(dates)
    tagList(
      hr(),
      dateRangeInput("date_range", lang$date_range_label,
                     start = rng[1], end = rng[2],
                     min   = rng[1], max = rng[2],
                     language = "es")
    )
  })

  # ── Filtered data ───────────────────────────────────────────────────────────

  tomst_filtered <- reactive({
    req(tomst_raw(), input$date_range)
    tomst_raw() %>%
      filter(as.Date(datetime) >= input$date_range[1],
             as.Date(datetime) <= input$date_range[2])
  })

  hobo_filtered <- reactive({
    req(hobo_hourly(), input$date_range)
    hobo_hourly() %>%
      filter(as.Date(hour) >= input$date_range[1],
             as.Date(hour) <= input$date_range[2])
  })

  # ── TOMST plot ──────────────────────────────────────────────────────────────

  output$tomst_plot <- renderPlotly({
    req(tomst_filtered())
    df        <- tomst_filtered()
    var       <- input$tomst_var
    var_label <- names(tomst_var_choices)[tomst_var_choices == var]

    if (input$tomst_plot_type == "time_series") {
      plot_ly(df, x = ~datetime, y = as.formula(paste0("~", var)),
              color = ~label, type = "scatter", mode = "lines") %>%
        layout(xaxis  = list(title = lang$axis_datetime, tickformat = "%d %b %Y"),
               yaxis  = list(title = var_label),
               legend = list(title = list(text = lang$legend_label))) %>%
        config(locale = "es")

    } else if (input$tomst_plot_type == "boxplot") {
      plot_ly(df, x = ~label, y = as.formula(paste0("~", var)),
              color = ~label, type = "box") %>%
        layout(xaxis = list(title = lang$axis_label),
               yaxis = list(title = var_label))

    } else {
      lbls <- unique(df$label)
      p    <- plot_ly()
      for (lbl in lbls) {
        d <- df[df$label == lbl, , drop = FALSE]
        p <- p %>% add_histogram(x = d[[var]], name = lbl, opacity = 0.5)
      }
      p %>% layout(barmode = "overlay",
                   xaxis  = list(title = var_label),
                   yaxis  = list(title = lang$axis_count),
                   legend = list(title = list(text = lang$legend_label)))
    }
  })

  # ── HOBO month/year picker (day_of_month bin only) ─────────────────────────

  output$hobo_month_year_ui <- renderUI({
    req(hobo_filtered())
    df <- hobo_filtered()
    month_years <- df %>%
      mutate(ym = format(hour, "%Y-%m")) %>%
      distinct(ym) %>%
      arrange(ym) %>%
      pull(ym)
    ym_dates <- as.Date(paste0(month_years, "-01"))
    display_labels <- paste0(
      month(ym_dates, label = TRUE, abbr = TRUE, locale = "es_ES"),
      " ",
      year(ym_dates)
    )
    selectInput("hobo_month_year", lang$month_year_label,
                choices = setNames(month_years, display_labels))
  })

  # ── HOBO plot ───────────────────────────────────────────────────────────────

  output$hobo_plot <- renderPlotly({
    req(hobo_filtered())
    df       <- hobo_filtered()
    stations <- unique(df$label)

    if (input$hobo_view == "hourly_ts") {
      if (length(stations) == 1) {
        plot_ly(df, x = ~hour, y = ~Precip_mm,
                type = "bar", marker = list(color = "#2c7bb6")) %>%
          layout(xaxis  = list(title = lang$axis_datetime, tickformat = "%d %b %Y"),
                 yaxis  = list(title = lang$axis_precip_hr),
                 bargap = 0.05) %>%
          config(locale = "es")
      } else {
        plot_ly(df, x = ~hour, y = ~Precip_mm,
                color = ~label, type = "scatter", mode = "lines") %>%
          layout(xaxis  = list(title = lang$axis_datetime, tickformat = "%d %b %Y"),
                 yaxis  = list(title = lang$axis_precip_hr),
                 legend = list(title = list(text = lang$legend_station))) %>%
          config(locale = "es")
      }

    } else {
      bin_col <- switch(input$hobo_bin_by,
        "month"        = "MonthN",
        "day_of_month" = "Day",
        "season"       = "Season",
        "year"         = "Year"
      )

      # For day_of_month, restrict to the chosen month/year
      plot_df <- df
      if (input$hobo_bin_by == "day_of_month") {
        req(input$hobo_month_year)
        plot_df <- df %>% filter(format(hour, "%Y-%m") == input$hobo_month_year)
      }

      grouped <- plot_df %>%
        group_by(label, bin = .data[[bin_col]]) %>%
        summarise(Total_mm = sum(Precip_mm, na.rm = TRUE), .groups = "drop")

      if (input$hobo_bin_by == "season") {
        grouped$bin <- factor(grouped$bin, levels = season_levels)
      } else if (input$hobo_bin_by == "day_of_month") {
        grouped$bin <- factor(grouped$bin, levels = 1:31)
      } else {
        grouped$bin <- factor(grouped$bin)
      }
      grouped <- arrange(grouped, bin)

      # x-axis title: for day_of_month, include the selected month/year
      bin_axis_title <- switch(input$hobo_bin_by,
        "month"        = lang$bin_month,
        "day_of_month" = {
          ym <- as.Date(paste0(input$hobo_month_year, "-01"))
          paste0(lang$bin_day, " — ",
                 month(ym, label = TRUE, abbr = TRUE, locale = "es_ES"),
                 " ", year(ym))
        },
        "season"       = lang$bin_season,
        "year"         = lang$bin_year
      )

      if (length(stations) == 1) {
        plot_ly(grouped, x = ~bin, y = ~Total_mm,
                type = "bar", marker = list(color = "#2c7bb6")) %>%
          layout(xaxis  = list(title = bin_axis_title),
                 yaxis  = list(title = lang$axis_precip_total),
                 bargap = 0.2)
      } else {
        plot_ly(grouped, x = ~bin, y = ~Total_mm,
                color = ~label, type = "bar", barmode = "group") %>%
          layout(xaxis  = list(title = bin_axis_title),
                 yaxis  = list(title = lang$axis_precip_total),
                 legend = list(title = list(text = lang$legend_station)))
      }
    }
  })

  # ── HOBO station info sidebar card ─────────────────────────────────────────
  
  output$hobo_station_info <- renderUI({
    req(hobo_filtered())
    df       <- hobo_filtered()
    stations <- unique(df$label)
    items <- lapply(stations, function(s) {
      sdf   <- df[df$label == s, ]
      total <- sum(sdf$Precip_mm, na.rm = TRUE)
      ndays <- as.numeric(diff(range(sdf$hour, na.rm = TRUE)), units = "days")
      tags$div(
        tags$strong(s),
        tags$p(sprintf(lang$station_total_span, total, ndays),
               style = "font-size:0.85em; margin: 0 0 6px 0;")
      )
    })
    tagList(tags$strong(lang$station_summaries), tags$hr(style = "margin:4px 0;"), items)
  })
  


  # ── Raw data tables ─────────────────────────────────────────────────────────

  output$tomst_data_table <- renderDT({
    req(tomst_filtered())
    # select which columns
    datatable(tomst_filtered(),
              options  = list(pageLength = 25, scrollX = TRUE),
              # add colnames
              rownames = FALSE,
              filter   = "top")
  })

  output$hobo_data_table <- renderDT({
    req(hobo_filtered())
    hobo_filtered() %>%
      select(label, hour, Precip_mm, Temp_C) %>%
      datatable(
        colnames = c(lang$col_station, lang$col_datetime, lang$col_precip_hr, lang$col_temp),
        options  = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE,
        filter   = "top"
      )
  })
}

shinyApp(ui, server)
