server <- function(input, output, session) {
  
  # R-side rolling buffer
  buf <- reactiveVal(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
  
  
  
  
  
  observeEvent(input$start, {
    # reset full session
    session_df(data.frame(
      t_us = numeric(0),
      t_s = numeric(0),
      weight_raw_kg = numeric(0),
      offset_raw_kg = numeric(0),
      weight_adj_kg = numeric(0)
    ))
    
    # (optional) reset peak hold at start of a new session
    peak_hold(NA_real_)
    
    py$start_stream(
      name_prefix = "Progressor_1827",
      auto_tare   = isTRUE(input$auto_tare),
      reset_buffer = TRUE
    )
  })
  
  
  observeEvent(input$stop, {
    py$stop_stream()
  })
  
  observeEvent(input$clear, {
    buf(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
    peak_hold(NA_real_)
    offset(0)
    # DO NOT reset session_df() here (so you can still save)
  })
  
  
  
  # Full session storage (not trimmed) ----
  session_df <- reactiveVal(data.frame(
    t_us = numeric(0),
    t_s = numeric(0),
    weight_raw_kg = numeric(0),
    offset_raw_kg = numeric(0),
    weight_adj_kg = numeric(0)
  ))
  
  
  
  #Units
  g <- 9.80665
  
  to_units <- function(x) {
    if (identical(input$units, "N")) x * g else x
  }
  
  
  
  #Peak Hold
  peak_hold <- reactiveVal(NA_real_)
  
  observeEvent(input$reset_peak, {
    peak_hold(NA_real_)
  })
  
  observeEvent(input$clear, {
    buf(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
    peak_hold(NA_real_)
  })
  
  
  #Offset
  offset <- reactiveVal(0)
  
  observeEvent(input$zero_now, {
    df <- buf()
    if (nrow(df) >= 1) {
      offset(df$weight[nrow(df)])
      peak_hold(NA_real_)  # optional: reset peak hold after zero
    }
  })
  
  output$offset_status <- renderText({
    sprintf("Software offset: %.3f %s", to_units(offset()), input$units)
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
      
      # ---- append to FULL session (raw kg + offset info) ----
      off_kg <- offset()
      
      sess_new <- data.frame(
        t_us = newdf$t_us,
        t_s  = newdf$t_s,
        weight_raw_kg = newdf$weight,
        offset_raw_kg = rep(off_kg, nrow(newdf)),
        weight_adj_kg = newdf$weight - off_kg
      )
      
      session_df(rbind(session_df(), sess_new))
      
      
      
      
      #
      cur <- buf()
      out <- rbind(cur, newdf)
      
      # keep last ~20 seconds of data
      if (nrow(out) > 2) {
        tmax <- max(out$t_s)
        out <- out[out$t_s >= (tmax - 20), , drop = FALSE]
      }
      buf(out)
      
      # update peak hold
      new_peak_raw <- max(out$weight - offset(), na.rm = TRUE)
      ph <- peak_hold()
      if (is.na(ph) || new_peak_raw > ph) peak_hold(new_peak_raw)
      
    }
  })
  
  output$plot <- renderPlot({
    
    df <- buf()
    validate(need(nrow(df) >= 2, "No live data yet. Click Start Live."))
    
    y <- to_units(df$weight - offset())
    
    plot(df$t_s, y, type="l",
         xlab="Time (s)", ylab=paste0("Force (", input$units, ")"),
         main="Live stream")
    
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
    
    cur_raw <- (df_all$weight - offset())[nrow(df_all)]
    w_win_raw <- df_win$weight - offset()
    
    cur   <- to_units(cur_raw)
    peak  <- to_units(max(w_win_raw, na.rm = TRUE))
    meanv <- to_units(mean(w_win_raw, na.rm = TRUE))
    
    ph <- peak_hold()
    ph_u <- if (is.na(ph)) NA_real_ else to_units(ph)
    
    sprintf(
      "Current: %.3f %s\nPeak (hold): %s\nPeak (last %.1f s): %.3f %s\nMean (last %.1f s): %.3f %s",
      cur, input$units,
      ifelse(is.na(ph_u), "—", sprintf('%.3f %s', ph_u, input$units)),
      input$metric_window, peak, input$units,
      input$metric_window, meanv, input$units
    )
   
  }) 
    
  
  
  
  
    #Save to csv ----
  save_msg <- reactiveVal("")
  
  observeEvent(input$save_csv, {
    df <- session_df()
    if (nrow(df) < 1) {
      save_msg("Nothing to save yet.")
      return()
    }
    
    dir <- normalizePath(input$save_dir, winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    nm   <- slugify(input$athlete_name)
    limb <- slugify(input$limb)
    test <- slugify(input$test_type)
    
    fname <- paste(stamp, nm, limb, test, sep = "_")
    path  <- file.path(dir, paste0(fname, ".csv"))
    
    # --- add Newton columns at save time ---
    df_out <- df %>%
      mutate(
        weight_raw_N = weight_raw_kg * g,
        weight_adj_N = weight_adj_kg * g
      )
    
    write.csv(df_out, path, row.names = FALSE)
    save_msg(paste("Saved:", path))
    
  })
  
  output$save_status <- renderText(save_msg())
  
    
    
    
    

  
  
  
  
}