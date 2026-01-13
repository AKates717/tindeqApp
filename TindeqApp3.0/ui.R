# ui <- fluidPage(
#   titlePanel("Tindeq Live Stream"),
#   sidebarLayout(
#     sidebarPanel(
#       # --- session metadata ---
#       selectInput("athlete_name", "Name",
#                   choices = c("Andrew Kates", "Homer Simpson", "Frank Reynolds"),
#                   selected = "Andrew Kates"),
#       selectInput("limb", "Limb",
#                   choices = c("Left", "Both", "Right"),
#                   selected = "Both"),
#       selectInput("test_type", "Test Type",
#                   choices = c("Quads", "Hamstrings"),
#                   selected = "Quads"),
#       checkboxInput("auto_tare", "Auto-tare on Start", TRUE),
#       actionButton("start", "Start Live"),
#       actionButton("stop",  "Stop Live"),
#       actionButton("clear", "Clear Plot"),
#       actionButton("reset_peak", "Reset Peak Hold"),
#       actionButton("zero_now", "Zero Now (software)"),
#       hr(),
#       selectInput("units", "Units", choices = c("kg", "N"), selected = "kg"),
#       numericInput("metric_window", "Metrics window (s)", value = 2, min = 0.5, max = 10, step = 0.5),
#       hr(),
#       verbatimTextOutput("status"),
#       verbatimTextOutput("metrics"),
#       hr(),
#       textInput("save_dir", "Save folder", value = getwd()),
#       textInput("save_name", "Save name (no extension)", value = "tindeq_session"),
#       actionButton("save_csv", "Save session to CSV"),
#       textOutput("save_status")
#     ),
#     mainPanel(
#       plotOutput("plot", height = 350)
#     )
#   )
# )




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
  
  
  nav_panel(
    "Collect",
    layout_sidebar(
      sidebar = sidebar(
        width = "35%",
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
        
        #Controls
        div(
          class = "d-flex gap-2",
          actionButton(
            "start",
            "Start",
            class = "btn-success w-50"
          ),
          actionButton(
            "stop",
            "Stop",
            class = "btn-danger w-50"
          )
        ),
        checkboxInput("auto_tare", "Auto-tare on Start", TRUE),
        
        actionButton("clear", "Clear Plot"),
        actionButton("reset_peak", "Reset Peak Hold"),
        actionButton("zero_now", "Zero Now (software)"),
        hr(),
        selectInput("units", "Units", choices = c("kg", "N"), selected = "kg"),
        numericInput("metric_window", "Metrics window (s)", value = 2, min = 0.5, max = 10, step = 0.5),
        hr(),
        verbatimTextOutput("status"),
        verbatimTextOutput("metrics"),
        hr(),
        textInput("save_dir", "Save folder", value = getwd()),
        textInput("save_name", "Save name (no extension)", value = "tindeq_session"),
        actionButton("save_csv", "Save session to CSV"),
        textOutput("save_status")
      ),
      mainPanel(
        plotOutput("plot", height = 350)
      )
    )
  )
)












