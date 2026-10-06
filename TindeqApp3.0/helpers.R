## Themes and Formatting ----

#app theme
custom_theme <- bslib::bs_theme(
  version = 5,
  preset = "bootstrap",
  spacer = "0.5rem",
  primary = "#000000"
)


#ak_gt_theme3 ----
#This is the working gt theme right now, ignore the first 2, create others based off this
ak_gt_theme3 <- function(
    gt_tbl,
    accent        = "#D71920",            # brand accent (primary red)
    header_bg     = "#FCFCFC",            # column label background (light grey)
    stripe_bg     = "#FFFFFF",            # row striping (white)
    border_col    = "#A7A9AC",            # neutral borders (grey)
    title_font    = google_font("Roboto Slab"),
    body_font     = google_font("Open Sans"),
    base_size_px  = 16,
    corner_radius = 10
) {
  
  gt_tbl %>%
    # fonts & base
    opt_table_font(font = list(body_font, default_fonts())) %>%
    tab_options(
      table.width                 = pct(100),
      table.background.color      = "white",
      table.font.size             = px(base_size_px),
      data_row.padding            = px(8),
      
      heading.align               = "left",
      heading.title.font.size     = px(22),
      heading.subtitle.font.size  = px(13),
      heading.padding             = px(6),
      
      # borders
      table.border.top.color      = accent,      # brand bar at top
      table.border.top.width      = px(3),
      table.border.bottom.color   = border_col,
      table.border.bottom.width   = px(1),
      column_labels.vlines.color  = "transparent",
      column_labels.border.bottom.color = border_col,  # red underline under headers
      column_labels.border.bottom.width = px(1),
      table_body.hlines.color     = border_col,
      table_body.hlines.width     = px(1),
      table_body.vlines.color     = "transparent",
      
      # headers + striping
      column_labels.background.color = header_bg,
      row.striping.background_color  = stripe_bg
    ) %>%
    # title styling
    tab_style(
      style = cell_text(color = accent, weight = "700", font = title_font),
      locations = cells_title(groups = "title")
    ) %>%
    # column labels
    tab_style(
      style = cell_text(weight = "600", color = "#111827"),
      locations = cells_column_labels(everything())
    ) %>%
    # body text
    tab_style(
      style = cell_text(color = "#222222"),
      locations = cells_body()
    ) %>%
    # enable striping (use color set above)
    opt_row_striping() %>%
    # rounded corners + gentle red-tinted hover + subtle left accent bar
    opt_css(sprintf("
  .gt_table {
    border-collapse: separate !important;
    border-spacing: 0 !important;
    border-radius: %dpx !important;
    overflow: hidden !important;
  }
  .gt_table tbody tr:hover td {
    background-color: rgba(215, 25, 32, 0.06) !important; /* light brand red */
  }
  .gt_footnotes, .gt_sourcenotes { font-size: 0.9em; }
", corner_radius))
}









## R helpers for server.R ----

samples_to_df <- function(samps) {
  if (length(samps) == 0) {
    return(data.frame(t_us = numeric(0), weight = numeric(0), t_s = numeric(0)))
  }
  df <- data.frame(
    t_us   = vapply(samps, function(x) x[[1]], numeric(1)),
    weight = vapply(samps, function(x) x[[2]], numeric(1))
  )
  df$t_s <- df$t_us / 1e6
  df
}

append_and_trim <- function(cur_df, new_df, window_s = 20) {
  out <- rbind(cur_df, new_df)
  if (nrow(out) >= 2) {
    tmax <- max(out$t_s)
    out <- out[out$t_s >= (tmax - window_s), , drop = FALSE]
  }
  out
}

# selectize option: start with no selection so the placeholder shows
# (otherwise the browser auto-selects the first choice)
clear_on_init <- I("function() { this.setValue(''); }")

slugify <- function(x) {
  x <- trimws(x)
  x <- gsub("[^A-Za-z0-9]+", "-", x)
  x <- gsub("(^-+|-+$)", "", x)
  if (nchar(x) == 0) "NA" else x
}



## Athlete database helpers ----

athlete_db_path  <- "tindeq-athlete-DB.xlsx"
athlete_db_sheet <- "athlete-db"
lists_sheet      <- "lists"

read_athlete_db <- function(path = athlete_db_path) {
  readxl::read_excel(path, sheet = athlete_db_sheet)
}

# Non-empty values of one column on the "lists" sheet (e.g. "Squad", "Test")
read_list <- function(col, path = athlete_db_path) {
  x <- readxl::read_excel(path, sheet = lists_sheet)[[col]]
  unique(x[!is.na(x) & nzchar(trimws(x))])
}

# Appends one row below the existing data; other sheets (e.g. "lists") are untouched.
# Columns are matched to the sheet's headers by name, so column order doesn't matter.
append_athlete <- function(row, path = athlete_db_path) {
  wb <- openxlsx::loadWorkbook(path)
  headers <- names(read_athlete_db(path))
  missing <- setdiff(headers, names(row))
  if (length(missing) > 0) stop("Missing column(s): ", paste(missing, collapse = ", "))
  next_row <- nrow(openxlsx::read.xlsx(wb, sheet = athlete_db_sheet)) + 2  # +1 header, +1 next
  openxlsx::writeData(wb, athlete_db_sheet, row[headers],
                      startRow = next_row, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(TRUE)
}


## Report helpers ----

results_path <- file.path("data", "tindeq_results.csv")
limb_levels  <- c("Left", "Both", "Right")
limb_colours <- c(Left = "#2C3E50", Both = "#A7A9AC", Right = "#D71920")

read_results <- function(path = results_path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    mutate(Date = as.Date(substr(timestamp, 1, 10)))
}

# Everything the report needs for one athlete + test, used by both the
# Report page and the PDF so they always match
build_report <- function(athlete, test, results, db) {
  trials <- results |> filter(Name == athlete, Test == test)

  # Best trial per limb per day (summaries are skipped when there's no data;
  # max() on no rows just warns)
  sessions <- if (nrow(trials) == 0) trials[0, ] else trials |>
    group_by(Date, Limb) |>
    summarise(`Peak Force (kg)` = max(`Peak Force (kg)`),
              `Peak Force (N)`  = max(`Peak Force (N)`),
              Trials = n(), .groups = "drop") |>
    mutate(Limb = factor(Limb, levels = limb_levels)) |>
    arrange(desc(Date), Limb)

  summary <- if (nrow(sessions) == 0) NULL else sessions |>
    group_by(Limb) |>
    summarise(`Best (kg)`   = max(`Peak Force (kg)`),
              `Best (N)`    = max(`Peak Force (N)`),
              `Latest (kg)` = last(`Peak Force (kg)`, order_by = Date),
              Sessions      = n_distinct(Date),
              `Last Tested` = max(Date), .groups = "drop")

  setup <- db |> filter(Name == athlete, Test == test)
  squad <- db$Squad[db$Name == athlete][1]

  list(
    athlete   = athlete,
    test      = test,
    squad     = if (is.na(squad)) "—" else squad,
    setup     = setup,
    setup_txt = if (nrow(setup) == 0) "No setup saved for this test" else
      sprintf("Angle %s° · Seat depth %s · Seat angle %s · Chain links %s",
              setup$Angle[1], setup$`Seat Depth`[1], setup$`Seat Angle`[1], setup$`Chain Links`[1]),
    sessions  = sessions,
    summary   = summary,
    generated = Sys.Date()
  )
}

# "Oct 5, 2026" (no leading zero on the day; %-d isn't portable to Windows)
fmt_date <- function(d) {
  paste0(format(d, "%b "), as.integer(format(d, "%d")), format(d, ", %Y"))
}

# Grouped bars: Left and Right side by side for each test date
report_plot <- function(rep) {
  df <- rep$sessions |>
    arrange(Date) |>
    mutate(Day = factor(fmt_date(Date), levels = unique(fmt_date(Date))),  # discrete, in date order
           # Always a Left and Right slot; "Both" only if it was tested
           Limb = factor(Limb, levels = intersect(limb_levels, c("Left", "Right", as.character(Limb))))) |>
    tidyr::complete(Day, Limb)  # keeps each limb in its own slot when one wasn't tested that day

  ggplot(df, aes(Day, `Peak Force (kg)`, fill = Limb)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
    geom_text(aes(label = round(`Peak Force (kg)`, 1)),
              position = position_dodge(width = 0.8), vjust = -0.4, size = 3.5, na.rm = TRUE) +
    scale_fill_manual(values = limb_colours, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = NULL, y = "Peak force (kg)", fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top",
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank())
}

# Renders report.qmd to PDF (Typst, via Quarto) and copies it to `file`
render_report_pdf <- function(rep, file) {
  dir <- tempfile("report_")
  dir.create(dir)
  dir <- normalizePath(dir)  # macOS: /var -> /private/var, else Quarto's cleanup errors
  file.copy("report.qmd", dir)
  rep$plot <- report_plot(rep)
  rep$fmt_date <- fmt_date
  saveRDS(rep, file.path(dir, "report.rds"))
  quarto::quarto_render(file.path(dir, "report.qmd"), quiet = TRUE)
  file.copy(file.path(dir, "report.pdf"), file, overwrite = TRUE)
}
