#' Give messages with my color
#'
#' @param ... The messages you wanna messgae, which will be pasted together
#' @param color Str. Preferred color. Default is yellow.
#'              Options are "31" (red), "32" (green), "34" (blue), "95" (light purple)...
#' @param return Logical. If TRUE, returns the formatted string.
#'  If FALSE (Default), prints directly.
#'
#' @export
#' @examples
#' leo_message("This is a red message", color = "31")
#' leo_message("This is a green message", "\nhaha", color = "32")
#' leo_message("This is a blue ", "message", color = "34")
#' leo_message("This is a light purple message", color = "95")
#' leo_message(" 🦁🦁🦁 Welcome to use the LEO package ! 🦁🦁🦁")
leo_message <- function(..., color = "31", return = FALSE) {
  message_content <- paste0(..., collapse = " ")
  formatted_message <- paste0("\033[", color, "m", message_content, "\033[0m")
  message(formatted_message)
}

#' Log messages with time stamps
#'
#' Logs messages with time stamps
#' The messages are styled using the `cli` package for enhanced readability.
#' This function can not deal with {} function in the `cli` package.
#'
#' @param ... The message string to log, which will be pasted together.
#' @param level The log level. Options are `"info"`, `"success"`, `"warning"`, and `"danger"`.
#' @param verbose in case you want to turn the info log off, set it to FALSE.
#'
#' @return No return value. Outputs a formatted log message with a timestamp.
#' @export
#'
#' @examples
#' n1 <- 10; n2 <- 20
#' leo_log("Processing the", n1, "and", n2, "files.")
#' leo_log("Task completed successfully!", level = "success")
#' leo_log("Potential issue detected.", level = "warning")
#' leo_log("Error occurred during processing!", level = "danger")
leo_log <- function(..., level = "info", verbose = TRUE) {
  if (verbose == FALSE) return(invisible())
  msg <- paste(..., collapse = " "); timestamp <- paste0("[", format(Sys.time(), '%H:%M:%S'),  "]")
  timestamp_colored <- switch(level,
                              "info" = cli::col_cyan(timestamp),     # cyan
                              "success" = cli::col_green(timestamp), # green
                              "warning" = cli::col_yellow(timestamp),# yellow
                              "danger" = cli::col_red(timestamp))    # red
  formatted_message <- paste(timestamp_colored, msg)
  switch(level,
         "info"    = cli::cli_alert_info(formatted_message, .envir = parent.frame()),
         "success" = cli::cli_alert_success(formatted_message, .envir = parent.frame()),
         "warning" = cli::cli_alert_warning(formatted_message, .envir = parent.frame()),
         "danger"  = cli::cli_alert_danger(formatted_message, .envir = parent.frame())
  )
}

#' Generate a custom colour palette
#'
#' Convenience wrapper around **`colorRampPalette()`** that follows my-preferred
#' coding conventions.
#'
#' @param color_set   **Character vector.** Base colours to interpolate between
#'                    (e.g. `c("#1f77b4", "#ff7f0e", "#2ca02c")`).
#' @param expected_num **Integer (scalar).** Number of colours you want to
#'                    generate.
#'
#' @return A character vector of hexadecimal colour codes of length
#'         `expected_num`.
#'
#' @examples
#' # 10-colour palette smoothly blended from three anchor colours
#' leo_more_color(c("#1f77b4", "#ff7f0e", "#2ca02c"), 10)
#'
#' @export
leo_more_color <- function(color_set, expected_num) {
  stopifnot(
    is.character(color_set),
    length(color_set) >= 2,
    is.numeric(expected_num),
    length(expected_num) == 1,
    expected_num > 0
  )
  colorRampPalette(color_set)(expected_num)
}

#' Custom ggplot2 theme
#'
#' I often forget my favorite ggplot theme settings, so I write this to reminds myself.
#'
#' @param plot_title_size Numeric; title font size (default 14).
#' @param legend_title_size Numeric; legend title font size (default 10).
#' @param legend_text_size Numeric; legend text size (default 9).
#' @param axis_title_size Numeric; axis title size (default 10).
#' @param axis_text_size Numeric; axis text size (default 8).
#' @param type Character; "object" to return theme, or "console" (default) to print copyable code.
#'
#' @importFrom ggplot2 theme element_text element_blank
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Add to a plot
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() +
#'   leo_theme()
#' # Print theme code
#' leo_theme(type = "console")
leo_theme <- function(
    plot_title_size   = 14,
    legend_title_size = 10,
    legend_text_size  = 9,
    axis_title_size   = 10,
    axis_text_size    = 8,
    type              = c("console", "object")
) {
  type <- match.arg(type)
  elems <- list(
    sprintf(paste0("    plot.title      = element_text(",
            "face = 'bold', hjust = 0, ",
            "size = %s, color = 'black')"), plot_title_size),
    sprintf(paste0("    legend.title    = element_text(",
            "size = %s, face = 'bold', ",
            "color = 'black')"), legend_title_size),
    sprintf(paste0("    legend.text     = element_text(",
            "size = %s, color = 'black')"), legend_text_size),
    sprintf(paste0("    axis.title      = element_text(",
            "size = %s, face = 'bold', ",
            "color = 'black')"), axis_title_size),
    sprintf("    axis.text       = element_text(size = %s, color = 'black')", axis_text_size),
    "    axis.title.x    = element_blank()",
    "    axis.text.x     = element_blank()",
    "    axis.ticks.x    = element_blank()",
    "    panel.grid.minor = element_blank()",
    "    legend.position = 'right'",
    "    legend.direction = 'vertical'",
    "    guides(color = guide_colorbar(frame.colour = 'black', ticks = FALSE))"
  )
  if (type == "console") {
    cat("theme(\n")
    cat(paste(elems, collapse = ",\n"), "\n")
    cat(")\n")
    return(invisible(NULL))
  }
  ggplot2::theme(
    plot.title       = ggplot2::element_text(
      face = "bold", hjust = 0,
      size = plot_title_size, color = "black"
    ),
    legend.title     = ggplot2::element_text(
      size = legend_title_size, face = "bold",
      color = "black"
    ),
    legend.text      = ggplot2::element_text(
      size = legend_text_size, color = "black"
    ),
    axis.title       = ggplot2::element_text(
      size = axis_title_size, face = "bold",
      color = "black"
    ),
    axis.text        = ggplot2::element_text(
      size = axis_text_size, color = "black"
    ),
    axis.title.x     = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_blank(),
    axis.ticks.x     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position  = "right",
    legend.background = ggplot2::element_blank()
  )
}

#' Intelligent Elapsed-Time Formatter
#'
#' Compute and format the time difference between **now** and `start_time`,
#' choosing the most appropriate unit automatically:
#' * `< 1 s`    → **milliseconds (ms)**
#' * `< 60 s`   → **seconds (sec)**
#' * `< 1 h`    → **minutes (min)**
#' * `< 1 day`  → **hours (hr)**
#' * `< 1 week` → **days (day)**
#' * `< 1 month`→ **weeks (wk)**
#' * `< 1 year` → **months (mon)**
#' * otherwise → **years (yr)**
#'
#' @param start_time POSIXct. output from `Sys.time()`.
#' @param digits     Integer. digits to keep (defaut: 2).
#' @param return     Logical. if TRUE, only return the calculated results.
#'
#' @return If `return = FALSE` (default), prints the elapsed time in a formatted string
#' @examples
#' t0 <- Sys.time()
#' Sys.sleep(0.123)
#' leo_time_elapsed(t0) # → [12:34:56] Elapsed 125 ms
#'
#' t1 <- Sys.time()
#' Sys.sleep(3.5)
#' leo_time_elapsed(t1) # → [12:35:00] Elapsed 3.5 sec
#'
#' # Capture string without printing
#' t2 <- Sys.time(); Sys.sleep(61)
#' elapsed_str <- leo_time_elapsed(t2, return = TRUE)
#' print(elapsed_str) # "1.08 min"
#' @export
leo_time_elapsed <- function(start_time, digits = 2, return = FALSE) {
  stopifnot(inherits(start_time, "POSIXct"))
  secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  # Breakpoints: 1s, 1m, 1h, 1d, 1w, 1mo (~30d), 1y (~365.25d)
  brks <- c(1, 60, 3600, 86400, 604800, 2592000, 31557600)
  idx  <- findInterval(secs, brks) + 1
  fmt  <- paste0("%.", digits, "f %s")
  out  <- switch(idx,
                 sprintf("%.0f ms", 1000 * secs),      # ms
                 sprintf(fmt, secs,            "sec"), # sec
                 sprintf(fmt, secs / 60,       "min"), # min
                 sprintf(fmt, secs / 3600,     "hr"),  # hr
                 sprintf(fmt, secs / 86400,    "day"), # day
                 sprintf(fmt, secs / 604800,   "wk"),  # week
                 sprintf(fmt, secs / 2592000,  "mon"), # month
                 sprintf(fmt, secs / 31557600, "yr")   # year
  )
  if (return) return(invisible(out))
  leo_log("Elapsed", out, level = "success")
  invisible(out)
}

#' View DataFrame with Visidata from Terminal
#'
#' Opens a dataframe in Visidata (`vd`) from the R console.
#' Uses `data.table` or `vroom` for fast writing if available.
#'
#' @param df   Dataframe or matrix to view.
#'
#' @export
#' @examples
#' \dontrun{
#'   data(mtcars)
#'   vd(mtcars)
#'   # Pipe support
#'   mtcars %>% vd()
#' }
vd <- function(df) {
  if (missing(df)) {
    cli::cli_alert_danger("Usage: vd(dataframe)")
    return(invisible(NULL))
  }

  # 1. Check for 'vd' command
  vd_cmd <- Sys.which("vd")
  if (vd_cmd == "") {
    # Fallback check for common user paths (especially for pip install --user)
    common_paths <- c(
      file.path(Sys.getenv("HOME"), ".local/bin/vd"),
      "/opt/homebrew/bin/vd", "/usr/local/bin/vd"
    )
    for (p in common_paths) {
      if (file.exists(p)) {
        vd_cmd <- p
        break
      }
    }
  }

  if (vd_cmd == "") {
    hint <- switch(Sys.info()[["sysname"]],
      Darwin  = "'brew/pip install visidata'",
      Linux   = "'apt/yum/pip install visidata'",
      Windows = "'choco/pip install visidata'",
      "'pip install visidata'"
    )
    cli::cli_alert_warning(glue::glue("Visidata (vd) not found. Please install via {hint}"))
    return(invisible(df))
  }

  # 2. Temp file creation
  # Tips for Visidata usage
  leo_log("Launching Visidata... ",
          "[Tips: Shift+F:Freq/Filter, ",
          "Shift+S:Sheets, -/+:Select, q:Back]",
          level = "info")
  tmp_file <- tempfile(pattern = "vd_r_data_", fileext = ".csv")
  
  # 3. Fast Write: data.table > vroom > base
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fwrite(df, tmp_file, showProgress = FALSE)
  } else if (requireNamespace("vroom", quietly = TRUE)) {
    vroom::vroom_write(df, tmp_file, delim = ",", progress = FALSE)
  } else {
    utils::write.csv(df, tmp_file, row.names = FALSE)
  }

  # 4. Execute Visidata
  system2(vd_cmd, args = c(tmp_file), wait = TRUE)

  # 5. Cleanup
  if (file.exists(tmp_file)) unlink(tmp_file)
  leo_log("Visidata closed", level = "success")
  
  invisible(df)
}
