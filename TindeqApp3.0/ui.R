ui <- fluidPage(
  titlePanel("Tindeq Live Stream"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("auto_tare", "Auto-tare on Start", TRUE),
      actionButton("start", "Start Live"),
      actionButton("stop",  "Stop Live"),
      actionButton("clear", "Clear Plot"),
      hr(),
      verbatimTextOutput("status"),
      numericInput("metric_window", "Metrics window (s)", value = 2, min = 0.5, max = 10, step = 0.5),
      hr(),
      verbatimTextOutput("metrics")
    ),
    mainPanel(
      plotOutput("plot", height = 350)
    )
  )
)
