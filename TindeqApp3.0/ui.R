ui <- page_navbar(
  title = span(
    img(src='csi.png', 
        style = "margin-bottom: 10px; padding-right: 0px; padding-bottom: 0px;", 
        height =25),
    "Tindeq Data Collection"
  ),
  window_title = "Tindeq Data Collection",
  theme = custom_theme,
  # global card styles (once)
  header = tags$head(
    tags$link(rel = "stylesheet", href = "app.css")
  ),
#   tags$style("
#   .vb-number .shiny-text-output { 
#     font-size: 1.6rem; 
#     font-weight: 600; 
#     line-height: 1.1;
#   }
# "),
  
  
  
  nav_panel(
    "Collect",
    layout_sidebar(
      sidebar = sidebar(
        width = "25%",
        # --- session metadata ---
        selectInput("athlete_name", "Name",
                    choices = c("Andrew Kates", "Homer Simpson", "Frank Reynolds"),
                    selected = "Andrew Kates"),
        selectInput("test_type", "Test Type",
                    choices = c("Quads", "Hamstrings"),
                    selected = "Quads"),
        shinyWidgets::radioGroupButtons(
          inputId  = "limb",
          label    = "Limb",
          choices  = c("Left", "Both", "Right"),
          justified = TRUE,          # buttons fill width evenly
          checkIcon = list(yes = icon("check")),   # checkmark on selected
          selected = character(0)
        ),
        #selectInput("units", "Units", choices = c("kg", "N"), selected = "kg"),
        shinyWidgets::radioGroupButtons(
          inputId  = "units",
          label    = "Units",
          choices = c("kg", "N"),
          justified = TRUE,          # buttons fill width evenly
          checkIcon = list(yes = icon("check")),   # checkmark on selected
          selected = "kg"
        ),
        #Controls
        div(
          class = "d-flex gap-2",
          actionButton("start", "Start", class = "btn-success w-50"),
          actionButton("stop", "Stop", class = "btn-danger w-50")
        ),
        checkboxInput("auto_tare", "Auto-tare on Start", TRUE),
        hr(),
        uiOutput("start_msg"),
        #actionButton("reset_peak", "Reset Peak Hold"),
        #actionButton("zero_now", "Zero Now (software)"),
        numericInput("metric_window", "Metrics window (s)", value = 2, min = 0.5, max = 10, step = 0.5),
        hr(),
        verbatimTextOutput("status"),
        #verbatimTextOutput("metrics"),
        # textInput("save_dir", "Save folder", value = getwd()),
        # textInput("save_name", "Save name (no extension)", value = "tindeq_session"),
        actionButton("save_csv", "Save session to CSV", class = "btn-outline-success"),
        actionButton("clear", "Clear Plot", class = "btn-outline-danger"),
        textOutput("save_status")
      ),
      mainPanel(
        # Value boxes row
        bslib::layout_column_wrap(
          width = 1/3,
          
          bslib::value_box(
            title = "Time Live",
            value = div(class = "vb-number", textOutput("vb_time_value")),
            showcase = bsicons::bs_icon("stopwatch"),
            theme = bslib::value_box_theme(
              bg = "#6c757d",   # subtle grey; change if you want
              fg = "white"
            )
          ),
          
          bslib::value_box(
            title = "Current Force",
            value = div(class = "vb-number", textOutput("vb_current_value")),
            showcase = bsicons::bs_icon("activity"),
            theme = bslib::value_box_theme(
              bg = "#8B8B83",   # Bootstrap success green
              fg = "white"
            )
          ),
          
          bslib::value_box(
            title = "Peak Force",
            value = div(class = "vb-number", textOutput("vb_peak_value")),
            showcase = bsicons::bs_icon("graph-up-arrow"),
            theme = bslib::value_box_theme(
              bg = "#FF6A6A",   # Bootstrap danger red
              fg = "white"
            )
          )
        ),
        br(),
        card(plotlyOutput("plot", height = 350)),
        div(
          style = "text-align: right; margin-top: 8px;",
          actionButton("calc_summary", "Calculate")
        ),
        br(),
        gt_output("summary_tbl"),
        div(
          style = "text-align: right; margin-top: 8px;",
          actionButton("save_db", "Save to database")
        ),
        textOutput("db_status")
        
        
        
      )
      
    )
  )
)












