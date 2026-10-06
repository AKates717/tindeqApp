server <- function(input, output, session) {
  
  #Housekeeping ----

  #Define directory for csvs
  default_save_dir <- file.path(getwd(), "data")
  
  #timing
  live_start_time <- reactiveVal(NULL)
  plot_reset_trigger <- reactiveVal(0L)
  timer_armed     <- reactiveVal(FALSE)
  live_elapsed_s  <- reactiveVal(0)
  start_click_time <- reactiveVal(NULL)
  
  #Units
  g <- 9.80665
  to_units <- function(x) {
    if (identical(input$units, "N")) x * g else x
  }
  
  # R-side rolling buffer
  buf <- reactiveVal(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
  buf_fast <- shiny::debounce(reactive(buf()), 150)  # for numbers if you want them faster
  
  # Loading Notification Helpers
  show_connecting_modal <- function() {
    showModal(modalDialog(
      title = "Connecting to Progressor…",
      div(
        class = "d-flex align-items-center gap-3",
        tags$div(class = "spinner-border", role = "status",
                 tags$span(class = "visually-hidden", "Loading...")),
        div("Preparing live stream. Please wait…")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
  }
  
  hide_connecting_modal <- function() {
    removeModal()
  }
  
  show_timeout_modal <- function() {
    showModal(modalDialog(
      title = "Connection timed out",
      "Unable to start streaming data. Please press the button on the device to 
     wake it, then reload the browser page.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  }
  
  
  
  
  
  
  
  
  
  #Start Button ----
  
  observeEvent(input$start, {
    
    #Loading Notification after button push
    show_connecting_modal()
    start_click_time(Sys.time())
    
    # Require limb before starting
    if (is.null(input$limb) || !nzchar(input$limb)) {
      showModal(modalDialog(
        title = "Select limb",
        "Please select a limb before starting collection.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # # reset full session
    # session_df(data.frame(
    #   t_us = numeric(0),
    #   t_s = numeric(0),
    #   weight_raw_kg = numeric(0),
    #   offset_raw_kg = numeric(0),
    #   weight_adj_kg = numeric(0)
    # ))
    
    # reset full session (chunks)
    session_chunks(list())
    session_n(0L)
    
    # (optional) reset peak hold at start of a new session
    peak_hold(NA_real_)
    
    # arm timer, but don't start it yet
    live_start_time(NULL)
    live_elapsed_s(0)
    timer_armed(TRUE)
    
    
    py$start_stream(
      name_prefix = "Progressor_3177",
      auto_tare   = isTRUE(input$auto_tare),
      reset_buffer = TRUE
    )
  })
  
  
  
  
  
  
  
  
  
  
  #Stop Button ----
  observeEvent(input$stop, {
    py$stop_stream()
    
    # Freeze timer at the current elapsed time
    if (!is.null(live_start_time())) {
      live_elapsed_s(as.numeric(difftime(Sys.time(), live_start_time(), units = "secs")))
    }
    
    # Prevent further updates
    live_start_time(NULL)
    timer_armed(FALSE)
    start_click_time(NULL)   # if you use this
    # hide_connecting_modal()  # if present
  })
  
  
  
  
  
  
  
  
  
  #Clear Data ----
    observeEvent(input$clear, {
      buf(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
      peak_hold(NA_real_)
      offset(0)

      # Reset timer to zero on Clear
      live_elapsed_s(0)
      live_start_time(NULL)
      timer_armed(FALSE)
      start_click_time(NULL)   # if you use this

      # Wipe the plotly chart
      plot_reset_trigger(plot_reset_trigger() + 1L)
  })
  
  
  
  
  
  
  
  # Full session storage (not trimmed) ----
  # session_df <- reactiveVal(data.frame(
  #   t_us = numeric(0),
  #   t_s = numeric(0),
  #   weight_raw_kg = numeric(0),
  #   offset_raw_kg = numeric(0),
  #   weight_adj_kg = numeric(0)
  # ))
  
  # Full session storage as list-of-chunks (fast appends)
  session_chunks <- reactiveVal(list())
  session_n <- reactiveVal(0L)  # optional: quick row count without binding
  
  
  

  
  
  
  #Peak Hold ----
  peak_hold <- reactiveVal(NA_real_)
  
  observeEvent(input$reset_peak, {
    peak_hold(NA_real_)
  })
  
  # observeEvent(input$clear, {
  #   buf(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
  #   peak_hold(NA_real_)
  # })
  
  
  
  
  #Offset ----
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
  
  
  
  
  # Poll python buffer on a timer ----
  observe({
    invalidateLater(125, session)
    
    # grab new samples (and clear python-side buffer)
    samps <- py$get_samples(clear = TRUE)
    
    # Hide modal when first data arrives
    if (isTRUE(timer_armed()) && length(samps) > 0) {
      hide_connecting_modal()
    }
    
    # Timeout if no data after 10 seconds
    if (isTRUE(timer_armed()) &&
        !is.null(start_click_time()) &&
        length(samps) == 0) {
      
      elapsed <- as.numeric(difftime(Sys.time(), start_click_time(), units = "secs"))
      
      if (elapsed > 10) {
        hide_connecting_modal()
        show_timeout_modal()
        timer_armed(FALSE)
        start_click_time(NULL)
      }
    }
    
    # If we are armed and the first samples just arrived, start the timer now
    if (isTRUE(timer_armed()) && is.null(live_start_time()) && length(samps) > 0) {
      live_start_time(Sys.time())
      timer_armed(FALSE)
    }
    
    # update "time live" while streaming
    if (!is.null(live_start_time())) {
      live_elapsed_s(as.numeric(difftime(Sys.time(), live_start_time(), units = "secs")))
    }
    
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
      
      #session_df(rbind(session_df(), sess_new))
      
      # append chunk (cheap)
      chunks <- session_chunks()
      chunks[[length(chunks) + 1]] <- sess_new
      session_chunks(chunks)
      
      # update running count (optional)
      session_n(session_n() + nrow(sess_new))
      
      
      
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

      # --- plotly proxy: react (smooth, no flicker) ---
      tmax_plot <- if (nrow(out) >= 1) max(out$t_s) else 0
      tmin_plot <- if (nrow(out) >= 1) max(tmax_plot - 20, min(out$t_s)) else 0
      ph_disp   <- if (is.na(peak_hold())) NA_real_ else to_units(peak_hold())

      # Downsample to ~500 points for display
      plot_out <- if (nrow(out) > 500) {
        idx <- round(seq(1, nrow(out), length.out = 500))
        out[idx, , drop = FALSE]
      } else {
        out
      }

      # Stable y range: upper bound driven by peak_hold (monotonically increasing)
      y_vals <- as.numeric(to_units(plot_out$weight - off_kg))
      y_max  <- max(c(if (!is.na(ph_disp)) ph_disp else 0, y_vals, 1), na.rm = TRUE) * 1.15
      y_min  <- min(c(0, y_vals), na.rm = TRUE)

      plotlyProxy("plot", session) %>%
        plotlyProxyInvoke(
          "react",
          list(list(
            x    = as.numeric(plot_out$t_s),
            y    = y_vals,
            type = "scattergl",
            mode = "lines",
            line = list(color = "#2C3E50", width = 2),
            hovertemplate = paste0("%{y:.1f} ", input$units, "<extra></extra>"),
            name = "Force"
          )),
          list(
            xaxis = list(
              title     = "Time (s)",
              range     = list(tmin_plot, tmax_plot),
              showgrid  = TRUE,
              gridcolor = "#eeeeee",
              zeroline  = FALSE
            ),
            yaxis = list(
              title     = paste0("Force (", input$units, ")"),
              range     = list(y_min, y_max),
              showgrid  = TRUE,
              gridcolor = "#eeeeee",
              zeroline  = FALSE
            ),
            paper_bgcolor = "white",
            plot_bgcolor  = "white",
            showlegend    = FALSE,
            margin        = list(l = 60, r = 20, t = 20, b = 50),
            shapes = if (!is.na(ph_disp)) {
              list(list(
                type = "line",
                x0   = 0, x1 = 1, xref = "paper",
                y0   = ph_disp, y1   = ph_disp,
                line = list(color = "#C0392B", width = 1.5, dash = "dash")
              ))
            } else {
              list()
            }
          )
        )

    }
  })
  
  
  
  
  
  
  
  
  
  #Value Boxes ----
  output$vb_time_value <- renderText({
    sprintf("%d s", as.integer(floor(live_elapsed_s())))
  })
  
  
  output$vb_current_value <- renderText({
    df <- buf_fast()
    if (nrow(df) < 1) return("—")
    sprintf("%.1f %s", round(to_units((df$weight - offset())[nrow(df)]), 1), input$units)
  })
  
  output$vb_peak_value <- renderText({
    ph <- peak_hold()
    if (is.na(ph)) return("—")
    sprintf("%.1f %s", round(to_units(ph), 1), input$units)
  })
  
  
  
  
  
  
  
  
  #Main Plot ----
  output$plot <- renderPlotly({
    plot_reset_trigger()  # re-initialize on Clear

    plot_ly() %>%
      add_trace(
        x = numeric(0), y = numeric(0),
        type = "scattergl", mode = "lines",
        line = list(color = "#2C3E50", width = 2),
        hovertemplate = paste0("%{y:.1f} ", isolate(input$units), "<extra></extra>"),
        name = "Force"
      ) %>%
      layout(
        xaxis = list(
          title = "Time (s)",
          showgrid = TRUE,
          gridcolor = "#eeeeee",
          zeroline = FALSE,
          fixedrange = FALSE
        ),
        yaxis = list(
          title = paste0("Force (", isolate(input$units), ")"),
          showgrid = TRUE,
          gridcolor = "#eeeeee",
          zeroline = FALSE,
          fixedrange = FALSE
        ),
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        showlegend = FALSE,
        margin = list(l = 60, r = 20, t = 20, b = 50)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  
  
  
  
  
  
  # Main Table ----
  #Summary table data (always exists; peaks filled on Calculate)
  summary_tbl_data <- reactiveVal(data.frame(
    Name = NA_character_,
    Test = NA_character_,
    Limb = NA_character_,
    `Peak Force (kg)` = NA_real_,
    `Peak Force (N)`  = NA_real_,
    check.names = FALSE
  ))
  
  # Initialize + keep Name/Test/Limb up to date as user changes inputs
  observe({
    req(input$athlete_name, input$test_type)  # filled from the database after startup

    # keep existing peak values (if already calculated)
    cur <- summary_tbl_data()
    
    summary_tbl_data(data.frame(
      Name = input$athlete_name,
      Test = input$test_type,
      Limb = if (is.null(input$limb) || input$limb == "") NA_character_ else input$limb,
      `Peak Force (kg)` = cur$`Peak Force (kg)`,
      `Peak Force (N)`  = cur$`Peak Force (N)`,
      check.names = FALSE
    ))
  })
  
  # When Calculate is clicked, fill peak values using current peak_hold()
  observeEvent(input$calc_summary, {
    cur <- summary_tbl_data()
    ph_raw <- peak_hold()
    
    summary_tbl_data(data.frame(
      Name = cur$Name,
      Test = cur$Test,
      Limb = cur$Limb,
      `Peak Force (kg)` = if (is.na(ph_raw)) NA_real_ else round(ph_raw, 2),
      `Peak Force (N)`  = if (is.na(ph_raw)) NA_real_ else round(ph_raw * g, 1),
      check.names = FALSE
    ))
  })
  
  #GT
  output$summary_tbl <- gt::render_gt({
    df <- summary_tbl_data()
    req(nrow(df) == 1)
    
    gt::gt(df) |>
      gt::sub_missing(columns = everything(), missing_text = "—") |>
      gt::cols_align(align = "left", columns = c(Name, Test, Limb)) |>
      gt::cols_align(align = "right", columns = c(`Peak Force (kg)`, `Peak Force (N)`)) |>
      gt::tab_options(
        table.font.size = gt::px(14),
        data_row.padding = gt::px(6)
      ) |>
      ak_gt_theme3() |>
      gt::cols_align(align = "center")
  })
  
  
  
  
  
  
  
  #Save Raw Data ----
  save_msg <- reactiveVal("")
  
  observeEvent(input$save_csv, {
    chunks <- session_chunks()
    if (length(chunks) == 0) {
      save_msg("Nothing to save yet.")
      return()
    }
    df <- do.call(rbind, chunks)
    if (nrow(df) < 1) {
      save_msg("Nothing to save yet.")
      return()
    }
    
    dir <- normalizePath(default_save_dir, winslash = "/", mustWork = FALSE)
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
    
    # Show full path so you can find it easily
    save_msg(paste("Saved:", fname))
  })
  
  output$save_status <- renderText(save_msg())
  
  
  
  
  
  
  #Save Summary Data ----
  
  #helper to ge the append working properly
  ensure_trailing_newline <- function(path) {
    if (!file.exists(path)) return(invisible(TRUE))
    sz <- file.info(path)$size
    if (is.na(sz) || sz < 1) return(invisible(TRUE))
    
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    seek(con, where = sz - 1, origin = "start")
    last <- readChar(con, nchars = 1, useBytes = TRUE)
    
    if (!identical(last, "\n")) {
      cat("\n", file = path, append = TRUE)
    }
    invisible(TRUE)
  }
  
  db_msg <- reactiveVal("")
  
  output$db_status <- renderText(db_msg())
  
  observeEvent(input$save_db, {
    
    # 1) Get the current table row
    df <- summary_tbl_data()   # <- THIS is your 1-row summary data.frame
    req(nrow(df) == 1)
    
    # 2) Require limb selected
    if (is.na(df$Limb) || !nzchar(df$Limb)) {
      showModal(modalDialog(
        title = "Missing selection",
        "Please select a limb before saving.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # 3) Require Calculate has been run (i.e., peak values exist)
    if (is.na(df$`Peak Force (kg)`) || is.na(df$`Peak Force (N)`)) {
      showModal(modalDialog(
        title = "Not calculated",
        "Please click Calculate before saving to the database.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # 4) Add timestamp
    out <- df
    out$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    
    # 5) Write/append to data/tindeq_results.csv
    dir.create("data", showWarnings = FALSE, recursive = TRUE)
    path <- file.path("data", "tindeq_results.csv")
    
    if (!file.exists(path)) {
      write.csv(out, path, row.names = FALSE)
    } else {
      ensure_trailing_newline(path)  # <-- ADD THIS LINE
      write.table(
        out, path,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE,
        eol = "\n"
      )
    }
    
    
    db_msg("Saved to database")
    
    # auto-clear after 3 seconds
    later::later(function() {
      db_msg("")
    }, delay = 3)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  #Athlete Database ----
  athlete_db <- reactiveVal(read_athlete_db())
  athlete_msg <- reactiveVal("")

  # Dropdowns driven by the "lists" sheet (read once at startup)
  updateSelectInput(session, "test_type", choices = read_list("Test"))
  updateSelectizeInput(session, "new_athlete_squad", choices = read_list("Squad"),
                       selected = character(0))

  # Collect tab athlete dropdown follows the database (keeps current pick if still present)
  observeEvent(athlete_db(), {
    nms <- sort(unique(athlete_db()$Name))
    cur <- isolate(input$athlete_name)
    updateSelectInput(session, "athlete_name", choices = nms,
                      selected = if (!is.null(cur) && cur %in% nms) cur else nms[1])
  })

  # Setup values for the selected athlete + test, shown under Test Type
  output$athlete_setup <- renderUI({
    req(input$athlete_name, input$test_type)
    s <- athlete_db() |>
      filter(Name == input$athlete_name, Test == input$test_type)

    txt <- if (nrow(s) == 0) {
      paste("No setup saved for", input$test_type)
    } else {
      s <- s[1, ]
      sprintf("Angle %s° · Seat depth %s · Seat angle %s · Chain links %s",
              s$Angle, s$`Seat Depth`, s$`Seat Angle`, s$`Chain Links`)
    }
    tags$small(class = "text-muted d-block mb-3", style = "margin-top: -0.5rem;", txt)
  })

  output$athlete_status <- renderText(athlete_msg())

  output$athlete_db_tbl <- gt::render_gt({
    gt::gt(athlete_db()) |>
      ak_gt_theme3() |>
      gt::cols_align(align = "center")
  })

  observeEvent(input$save_athlete, {
    nm <- trimws(input$new_athlete_name)

    if (!nzchar(nm)) {
      showModal(modalDialog(
        title = "Missing name",
        "Please enter the athlete's name before saving.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    if (tolower(nm) %in% tolower(athlete_db()$Name)) {
      showModal(modalDialog(
        title = "Athlete already exists",
        paste0("'", nm, "' is already in the database."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    # Every dropdown must be chosen (they start blank with a placeholder)
    picks <- list(Squad         = input$new_athlete_squad,
                  `Seat Depth`  = input$new_seat_depth,
                  `Seat Angle`  = input$new_seat_angle,
                  `Chain Links` = input$new_chain_links)
    blank <- names(picks)[!vapply(picks, isTruthy, logical(1))]
    if (length(blank) > 0) {
      showModal(modalDialog(
        title = "Missing selection",
        paste("Please select:", paste(blank, collapse = ", ")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    new_row <- data.frame(
      Name          = nm,
      Squad         = input$new_athlete_squad,
      Test          = "Quad ISO",
      Angle         = 90,
      `Seat Depth`  = as.integer(input$new_seat_depth),
      `Seat Angle`  = as.integer(input$new_seat_angle),
      `Chain Links` = as.integer(input$new_chain_links),
      check.names = FALSE
    )

    ok <- tryCatch({
      append_athlete(new_row)
      TRUE
    }, error = function(e) {
      showModal(modalDialog(
        title = "Could not save",
        paste("Saving failed (is the file open in Excel?):", conditionMessage(e)),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      FALSE
    })
    if (!ok) return()

    athlete_db(read_athlete_db())
    updateTextInput(session, "new_athlete_name", value = "")
    for (id in c("new_athlete_squad", "new_seat_depth", "new_seat_angle", "new_chain_links")) {
      updateSelectizeInput(session, id, selected = character(0))
    }
    athlete_msg(paste("Saved", nm))

    later::later(function() {
      athlete_msg("")
    }, delay = 3)
  })



  #Report ----
  # Re-reads the results file whenever it changes (e.g. after "Save to database")
  results_data <- reactiveFileReader(1000, session, results_path, read_results)

  updateSelectInput(session, "report_test", choices = read_list("Test"),
                    selected = "Quad ISO")

  # Athlete list follows the database; starts with no athlete selected
  observeEvent(athlete_db(), {
    nms <- sort(unique(athlete_db()$Name))
    cur <- isolate(input$report_athlete)
    updateSelectizeInput(session, "report_athlete", choices = nms,
                         selected = if (isTruthy(cur) && cur %in% nms) cur else character(0))
  })

  report <- reactive({
    req(input$report_athlete, input$report_test)
    build_report(input$report_athlete, input$report_test, results_data(), athlete_db())
  })

  output$report_body <- renderUI({
    if (!isTruthy(input$report_athlete)) {
      return(p(class = "text-muted mt-3", "Select an athlete to view their report."))
    }
    rep <- report()

    header <- div(
      class = "mb-2",
      h3(class = "mb-1", rep$athlete),
      div(class = "text-muted",
          paste("Squad:", rep$squad, " · Test:", rep$test,
                " · Generated:", fmt_date(rep$generated))),
      tags$small(class = "text-muted", paste("Setup:", rep$setup_txt))
    )

    if (nrow(rep$sessions) == 0) {
      return(tagList(header, p(class = "mt-3", paste("No results recorded for", rep$test, "yet."))))
    }

    tagList(
      header,
      card(card_header("Summary"), gt_output("report_summary_tbl")),
      card(card_header("Peak force over time"), plotOutput("report_plot", height = 300)),
      card(card_header("Sessions (best trial per limb)"), gt_output("report_sessions_tbl"))
    )
  })

  output$report_summary_tbl <- gt::render_gt({
    gt::gt(report()$summary) |>
      gt::fmt_date(`Last Tested`, date_style = "yMMMd") |>
      ak_gt_theme3() |>
      gt::cols_align(align = "center")
  })

  output$report_plot <- renderPlot(report_plot(report()))

  output$report_sessions_tbl <- gt::render_gt({
    gt::gt(report()$sessions) |>
      gt::fmt_date(Date, date_style = "yMMMd") |>
      ak_gt_theme3() |>
      gt::cols_align(align = "center")
  })

  # Download button only appears once an athlete is selected
  output$report_download_ui <- renderUI({
    req(input$report_athlete)
    downloadButton("report_pdf", "Download PDF", class = "btn-outline-dark w-100")
  })

  output$report_pdf <- downloadHandler(
    filename = function() {
      paste0(format(Sys.Date(), "%Y%m%d"), "_", slugify(input$report_athlete), "_",
             slugify(input$report_test), "_report.pdf")
    },
    content = function(file) {
      id <- showNotification("Building PDF…", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      render_report_pdf(report(), file)
    }
  )





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
  
  # output$metrics <- renderText({
  #   df_all <- buf()
  #   if (nrow(df_all) < 1) return("Current: —\nPeak (win): —\nMean (win): —")
  #   
  #   df_win <- metric_df()
  #   if (nrow(df_win) < 1) return("Current: —\nPeak (win): —\nMean (win): —")
  #   
  #   cur_raw <- (df_all$weight - offset())[nrow(df_all)]
  #   w_win_raw <- df_win$weight - offset()
  #   
  #   cur   <- to_units(cur_raw)
  #   peak  <- to_units(max(w_win_raw, na.rm = TRUE))
  #   meanv <- to_units(mean(w_win_raw, na.rm = TRUE))
  #   
  #   ph <- peak_hold()
  #   ph_u <- if (is.na(ph)) NA_real_ else to_units(ph)
  #   
  #   sprintf(
  #     "Current: %.3f %s\nPeak (hold): %s\nPeak (last %.1f s): %.3f %s\nMean (last %.1f s): %.3f %s",
  #     cur, input$units,
  #     ifelse(is.na(ph_u), "—", sprintf('%.3f %s', ph_u, input$units)),
  #     input$metric_window, peak, input$units,
  #     input$metric_window, meanv, input$units
  #   )
  #  
  # }) 
    
  
  
  
  

    
    
    

  
  
  
  
}