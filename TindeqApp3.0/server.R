server <- function(input, output, session) {
  
  # R-side rolling buffer
  buf <- reactiveVal(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
  
  observeEvent(input$start, {
    py$start_stream(name_prefix = "Progressor_1827",
                    auto_tare   = isTRUE(input$auto_tare),
                    reset_buffer = TRUE)
  })
  
  observeEvent(input$stop, {
    py$stop_stream()
  })
  
  observeEvent(input$clear, {
    buf(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
  })
  
  # Poll python buffer on a timer
  observe({
    invalidateLater(150, session)
    
    # grab new samples (and clear python-side buffer)
    samps <- py$get_samples(clear = TRUE)
    
    if (length(samps) > 0) {
      newdf <- data.frame(
        t_us   = vapply(samps, function(x) x[[1]], numeric(1)),
        weight = vapply(samps, function(x) x[[2]], numeric(1))
      )
      newdf$t_s <- newdf$t_us / 1e6
      
      cur <- buf()
      out <- rbind(cur, newdf)
      
      # keep last ~20 seconds of data
      if (nrow(out) > 2) {
        tmax <- max(out$t_s)
        out <- out[out$t_s >= (tmax - 20), , drop = FALSE]
      }
      buf(out)
    }
  })
  
  output$plot <- renderPlot({
    df <- buf()
    validate(need(nrow(df) >= 2, "No live data yet. Click Start Live."))
    plot(df$t_s, df$weight, type="l",
         xlab="Time (s)", ylab="Weight",
         main="Live stream (delivered ~5–6 Hz on macOS)")
  })
  
  output$status <- renderText({
    streaming <- py$is_streaming()
    df <- buf()
    if (nrow(df) >= 2) {
      duration <- max(df$t_s) - min(df$t_s)
      hz <- (nrow(df) - 1) / duration
      sprintf("Streaming: %s\nPoints in plot: %d\nWindow: %.1f s\nObserved Hz: %.2f",
              streaming, nrow(df), duration, hz)
    } else {
      sprintf("Streaming: %s\nPoints in plot: %d", streaming, nrow(df))
    }
  })
  
  
  # Windowed data for metrics (last N seconds)
  metric_df <- reactive({
    df <- buf()
    validate(need(nrow(df) >= 1, NULL))
    
    win <- input$metric_window
    if (is.null(win) || !is.finite(win) || win <= 0) win <- 2
    
    tmax <- max(df$t_s)
    df[df$t_s >= (tmax - win), , drop = FALSE]
  })
  
  output$metrics <- renderText({
    df_all <- buf()
    if (nrow(df_all) < 1) return("Current: —\nPeak (win): —\nMean (win): —")
    
    df_win <- metric_df()
    if (nrow(df_win) < 1) return("Current: —\nPeak (win): —\nMean (win): —")
    
    cur  <- df_all$weight[nrow(df_all)]
    peak <- max(df_win$weight, na.rm = TRUE)
    meanv <- mean(df_win$weight, na.rm = TRUE)
    
    sprintf(
      "Current: %.3f\nPeak (last %.1f s): %.3f\nMean (last %.1f s): %.3f",
      cur, input$metric_window, peak, input$metric_window, meanv
    )
  })
  
  
  
  
}