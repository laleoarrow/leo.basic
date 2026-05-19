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
#' leo_message("Welcome to use the LEO package!")
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
  invisible()
}

#' Stop execution with formatted message
#'
#' Combines `leo_log(..., level = "danger")` with `stop()` for consistent error handling.
#' Prints a timestamped red message before stopping execution.
#'
#' @param ... Messages to combine (passed to `leo_log`)
#' @param call. Logical; if FALSE (default), the call is not included in error message
#'
#' @return This function never returns; it stops execution with an error.
#' @export
#'
#' @examples
#' \dontrun{
#' if (!is.data.frame(df)) leo_stop("df must be a data.frame.")
#' }
leo_stop <- function(..., call. = FALSE) {
  msg <- paste(...)
  leo_log(msg, level = "danger")
  stop(msg, call. = call.)
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
#' * `< 1 s`    -> **milliseconds (ms)**
#' * `< 60 s`   -> **seconds (sec)**
#' * `< 1 h`    -> **minutes (min)**
#' * `< 1 day`  -> **hours (hr)**
#' * `< 1 week` -> **days (day)**
#' * `< 1 month`-> **weeks (wk)**
#' * `< 1 year` -> **months (mon)**
#' * otherwise -> **years (yr)**
#'
#' @param start_time POSIXct. output from `Sys.time()`.
#' @param digits     Integer. digits to keep (defaut: 2).
#' @param return     Logical. if TRUE, only return the calculated results.
#'
#' @return If `return = FALSE` (default), prints the elapsed time in a formatted string
#' @examples
#' t0 <- Sys.time() - 0.123
#' leo_time_elapsed(t0) # -> [12:34:56] Elapsed 125 ms
#'
#' t1 <- Sys.time() - 3.5
#' leo_time_elapsed(t1) # -> [12:35:00] Elapsed 3.5 sec
#'
#' # Capture string without printing
#' t2 <- Sys.time() - 61
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

#' View DataFrame with VisiData from Terminal
#'
#' `vd()` writes `df` to a temporary TSV file, then opens it with VisiData.
#' This keeps the R side simple while letting VisiData read the table lazily from disk,
#' which is much more usable for large data frames than printing them in the console.
#' Non-default row names are preserved as the first column.
#' A compact terminal summary is printed before launch, including dimensions,
#' row-name handling, temp-file size, and writer backend.
#'
#' @param df Data frame, tibble, or matrix to view.
#'
#' @return Invisibly returns `df`.
#'
#' @export
#' @examples
#' \dontrun{
#' data(mtcars)
#' vd(mtcars)
#' mtcars %>% vd()
#' }
vd <- function(df) {
  if (missing(df)) leo_stop("Usage: vd(df)")
  if (is.matrix(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!is.data.frame(df)) leo_stop("`df` must be a data.frame or matrix.")

  fmt_bytes <- function(x) {
    units <- c("B", "KB", "MB", "GB", "TB")
    x <- as.numeric(x)[1]
    if (is.na(x) || x < 0) return(NA_character_)
    i <- 1
    while (x >= 1024 && i < length(units)) {
      x <- x / 1024
      i <- i + 1
    }
    if (i == 1) sprintf("%.0f %s", x, units[i]) else sprintf("%.2f %s", x, units[i])
  }
  align_ansi <- function(x, width) {
    x <- as.character(x)
    pad <- strrep(" ", max(0, width - nchar(cli::ansi_strip(x), type = "width")))
    paste0(x, pad)
  }

  vd_opt <- getOption("leo.basic.vd_cmd")
  if (isFALSE(vd_opt) || (length(vd_opt) == 1 && is.na(vd_opt))) {
    vd_cmd <- ""
  } else if (is.character(vd_opt) && length(vd_opt) == 1 && nzchar(vd_opt)) {
    vd_cmd <- if (file.exists(vd_opt) || nzchar(Sys.which(vd_opt))) vd_opt else ""
  } else {
    vd_cmd <- Sys.which("vd")
    if (!nzchar(vd_cmd)) {
      common_paths <- c(file.path(Sys.getenv("HOME"), ".local/bin/vd"), "/opt/homebrew/bin/vd", "/usr/local/bin/vd")
      hit <- common_paths[file.exists(common_paths)][1]
      vd_cmd <- if (length(hit) == 0 || is.na(hit)) "" else hit
    }
  }
  if (!nzchar(vd_cmd)) {
    cli::cli_alert_warning("VisiData (vd) not found. Install it with brew install visidata or pip install visidata.")
    return(invisible(df))
  }

  row_col <- NULL
  df_out <- df
  rn <- rownames(df)
  if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(df))))) {
    row_col <- ".rowname"
    while (row_col %in% names(df_out)) row_col <- paste0(row_col, "_")
    df_out[[row_col]] <- rn
    df_out <- df_out[, c(row_col, setdiff(names(df_out), row_col)), drop = FALSE]
    rownames(df_out) <- NULL
  }

  tmp_file <- tempfile(pattern = "vd_", fileext = ".tsv")
  on.exit(if (file.exists(tmp_file)) unlink(tmp_file), add = TRUE)
  writer <- if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fwrite(df_out, tmp_file, sep = "\t", showProgress = FALSE)
    "fwrite"
  } else if (requireNamespace("vroom", quietly = TRUE)) {
    vroom::vroom_write(df_out, tmp_file, delim = "\t", progress = FALSE)
    "vroom_write"
  } else {
    utils::write.table(df_out, tmp_file, sep = "\t", quote = TRUE, row.names = FALSE)
    "write.table"
  }

  rowname_value <- if (is.null(row_col)) cli::col_yellow("none/default") else cli::col_green(paste0("yes (", row_col, ")"))
  writer_value <- switch(
    writer,
    "fwrite" = cli::col_magenta("fwrite"),
    "vroom_write" = cli::col_blue("vroom_write"),
    cli::col_cyan("write.table")
  )
  header <- c("Rows", "Cols", "Rowname", "File size", "Writer")
  value <- c(
    cli::col_cyan(as.character(nrow(df))),
    cli::col_cyan(as.character(ncol(df))),
    rowname_value,
    cli::col_green(fmt_bytes(file.info(tmp_file)$size)),
    writer_value
  )
  width <- pmax(nchar(header, type = "width"), nchar(cli::ansi_strip(value), type = "width"))
  rule <- paste(vapply(width, function(w) strrep("-", w), character(1)), collapse = "-+-")
  path_line <- paste0(cli::col_yellow("Temp file >>> "), cli::col_yellow(tmp_file))

  cat(cli::style_bold(cli::col_cyan("VisiData summary")), "\n")
  cat(paste(mapply(align_ansi, cli::col_cyan(header), width), collapse = " | "), "\n")
  cat(rule, "\n")
  cat(paste(mapply(align_ansi, value, width), collapse = " | "), "\n")
  cat(rule, "\n")
  cat(path_line, "\n")

  status <- system2(vd_cmd, args = tmp_file, wait = TRUE, stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) leo_stop("VisiData exited with status {status}.")
  invisible(df)
}
