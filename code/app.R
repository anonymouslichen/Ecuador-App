library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)

# Column names for TOMST TMS-3 data
tms_colnames <- c("index", "datetime", "timezone", "T1", "T2", "T3",
                   "soil_moisture_count", "shake", "errFlag")

# Variable choices for plotting
var_choices <- c("T1" = "T1", "T2" = "T2", "T3" = "T3",
                 "Soil Moisture" = "soil_moisture_count")

# --- UI ---
ui <- page_sidebar(
  title = "TOMST TMS-3 Data Explorer",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 380,
    fileInput("files", "Upload CSV files",
              multiple = TRUE, accept = ".csv"),
    uiOutput("treatment_ui"),
    hr(),
    selectInput("variable", "Variable", choices = var_choices),
    uiOutput("date_range_ui"),
    selectInput("plot_type", "Plot Type",
                choices = c("Time Series", "Boxplot", "Histogram"))
  ),

  navset_card_tab(
    nav_panel("Plot", plotlyOutput("main_plot", height = "600px")),
    nav_panel("Summary", DTOutput("summary_table")),
    nav_panel("Data", DTOutput("data_table"))
  )
)

# --- Server ---
server <- function(input, output, session) {

  # Shared pool of treatment labels (updated via Add buttons only)
  label_pool <- reactiveVal(character(0))

  # Read uploaded files
  raw_data <- reactive({
    req(input$files)
    lapply(seq_len(nrow(input$files)), function(i) {
      df <- read.csv(input$files$datapath[i], sep = ";", header = FALSE,
                     stringsAsFactors = FALSE)
      colnames(df) <- tms_colnames
      df$datetime <- dmy_hm(df$datetime)
      df$file_name <- input$files$name[i]
      df
    })
  })

  # Build treatment assignment UI once per upload (does NOT depend on label_pool)
  output$treatment_ui <- renderUI({
    req(input$files)
    fnames <- input$files$name

    tagList(
      tags$strong("Assign treatment labels:"),
      tags$p("Create a label, then select it for each file. Files with the same label are combined.",
             style = "font-size: 0.85em; color: #666; margin-bottom: 8px;"),
      lapply(seq_along(fnames), function(i) {
        select_id <- paste0("treat_select_", i)
        new_id <- paste0("treat_new_", i)
        add_id <- paste0("treat_add_", i)
        tagList(
          tags$label(fnames[i], style = "font-weight: 600; font-size: 0.9em;"),
          selectInput(select_id, label = NULL,
                      choices = c("-- New label --")),
          conditionalPanel(
            condition = paste0("input.", select_id, " == '-- New label --'"),
            div(
              style = "display: flex; gap: 6px; margin-bottom: 10px;",
              div(style = "flex: 1;",
                  textInput(new_id, label = NULL, placeholder = "Type new label...")),
              actionButton(add_id, "Add", class = "btn-sm btn-outline-secondary",
                           style = "height: 38px; margin-top: 0;")
            )
          ),
          tags$hr(style = "margin: 6px 0;")
        )
      }),
      actionButton("apply_labels", "Apply Labels & Load Data",
                   class = "btn-primary w-100 mt-2")
    )
  })

  # Create observers for each file's Add button
  # Use once = TRUE so observers are created only once per upload
  observers_created <- reactiveVal(0)

  observe({
    req(input$files)
    n <- nrow(input$files)
    # Only create observers when file count changes
    if (identical(observers_created(), n)) return()
    observers_created(n)

    lapply(seq_len(n), function(i) {
      add_id <- paste0("treat_add_", i)
      observeEvent(input[[add_id]], {
        new_id <- paste0("treat_new_", i)
        new_label <- trimws(input[[new_id]])
        if (nchar(new_label) == 0) return()

        # Add to pool if not already there
        pool <- label_pool()
        if (!(new_label %in% pool)) {
          pool <- c(pool, new_label)
          label_pool(pool)
        }

        # Update ALL dropdowns with full label list, preserving each one's selection
        updated_choices <- c(pool, "-- New label --")
        for (j in seq_len(n)) {
          sid <- paste0("treat_select_", j)
          current_sel <- input[[sid]]
          # For the file where Add was clicked, select the new label
          # For all others, keep their current selection
          sel <- if (j == i) new_label else current_sel
          updateSelectInput(session, sid, choices = updated_choices, selected = sel)
        }

        # Clear the text input
        updateTextInput(session, new_id, value = "")
      }, ignoreInit = TRUE)
    })
  })

  # Combine all files, using treatment labels from dropdowns
  combined_data <- eventReactive(input$apply_labels, {
    req(raw_data())
    dfs <- raw_data()
    for (i in seq_along(dfs)) {
      select_id <- paste0("treat_select_", i)
      new_id <- paste0("treat_new_", i)
      label <- input[[select_id]]
      # Fallback: if user typed a label but forgot to click Add
      if (is.null(label) || label == "-- New label --") {
        typed <- trimws(input[[new_id]])
        label <- if (nchar(typed) > 0) typed else paste("Unlabeled", i)
      }
      dfs[[i]]$treatment <- label
    }
    bind_rows(dfs)
  })

  # Date range filter
  output$date_range_ui <- renderUI({
    req(combined_data())
    rng <- range(combined_data()$datetime, na.rm = TRUE)
    dateRangeInput("date_range", "Date Range",
                   start = as.Date(rng[1]), end = as.Date(rng[2]),
                   min = as.Date(rng[1]), max = as.Date(rng[2]))
  })

  filtered_data <- reactive({
    req(combined_data(), input$date_range)
    combined_data() %>%
      filter(as.Date(datetime) >= input$date_range[1],
             as.Date(datetime) <= input$date_range[2])
  })

  # --- Plots: always show ALL treatments together for comparison ---
  output$main_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    var <- input$variable
    var_label <- names(var_choices)[var_choices == var]
    n_treatments <- length(unique(df$treatment))

    if (input$plot_type == "Time Series") {
      p <- plot_ly(df, x = ~datetime, y = as.formula(paste0("~", var)),
                   color = ~treatment, type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Date/Time"),
               yaxis = list(title = var_label),
               legend = list(title = list(text = "Treatment")))

    } else if (input$plot_type == "Boxplot") {
      p <- plot_ly(df, x = ~treatment, y = as.formula(paste0("~", var)),
                   color = ~treatment, type = "box") %>%
        layout(xaxis = list(title = "Treatment"),
               yaxis = list(title = var_label))

    } else {
      # Histogram: overlay distributions for each treatment
      treatments <- unique(df$treatment)
      p <- plot_ly()
      for (trt in treatments) {
        trt_data <- df[df$treatment == trt, , drop = FALSE]
        p <- p %>% add_histogram(x = trt_data[[var]], name = trt,
                                  opacity = 0.5)
      }
      p <- p %>%
        layout(barmode = "overlay",
               xaxis = list(title = var_label),
               yaxis = list(title = "Count"),
               legend = list(title = list(text = "Treatment")))
    }
    p
  })

  # Summary statistics: one row per treatment, side by side for comparison
  output$summary_table <- renderDT({
    req(filtered_data())
    var <- input$variable
    var_label <- names(var_choices)[var_choices == var]
    df <- filtered_data()
    summary_df <- df %>%
      group_by(Treatment = treatment) %>%
      summarise(
        Mean = round(mean(.data[[var]], na.rm = TRUE), 3),
        SD = round(sd(.data[[var]], na.rm = TRUE), 3),
        Min = round(min(.data[[var]], na.rm = TRUE), 3),
        Max = round(max(.data[[var]], na.rm = TRUE), 3),
        Median = round(median(.data[[var]], na.rm = TRUE), 3),
        N = n(),
        .groups = "drop"
      )
    datatable(summary_df,
              caption = paste("Summary statistics for", var_label),
              options = list(dom = "t", ordering = TRUE),
              rownames = FALSE)
  })

  # Raw data table
  output$data_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(),
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE,
              filter = "top")
  })
}

shinyApp(ui, server)
