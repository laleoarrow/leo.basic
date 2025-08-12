#' @title Plot numeric values by group with jitter / beeswarm, color / shape / size mapping
#' @description Draw a grouped scatter of numeric values with optional jitter /
#'   beeswarm arrangement, color / shape / size encoding, plus optional mean indicator.
#'
#' @param df           data.frame.
#' @param group        column name (chr) mapped to x-axis categories.
#' @param number       column name (chr) mapped to y-axis numeric values.
#' @param color_col    column name (chr) for point color (skip \code{color_rule}).
#' @param color_rule   function(df) → color vector; ignored if \code{color_col} given.
#' @param shape_col    column name (chr) for point shape (skip \code{shape_rule}).
#' @param shape_rule   function(df) → shape vector; ignored if \code{shape_col} given.
#' @param size_col     column name (chr) for point size (skip \code{size_rule}).
#' @param size_rule    function(df) → size vector; ignored if \code{size_col} given.
#' @param jitter       one of \code{"no"}, \code{"yes"}, \code{"bee"}.
#' @param x_axis_pos   \code{"default"} or \code{"zero"} (draw baseline at y = 0).
#' @param mean_type    \code{"none"}, \code{"point"}, \code{"line"}.
#' @param legend Logical. Whether to show legends for colour/shape/size mappings. Default TRUE.
#'               Set FALSE to hide all legends.
#' @return ggplot object.
#'
#' @note
#' **Shape tips** (ggplot shape codes):
#' • 24 ▲, 25 ▼, 22 ■  — filled; outline via \code{stroke}.
#' •  0 □,  2 △,  6 ▽  — hollow.
#' •  3 “+”,  4 “×”,  1 ○ — thin strokes, suit overlaps.
#' • 21–25 accept border (\code{colour}) + fill.
#'   For filled point with black outline: shape = 21, \code{colour = "black"}, \code{fill = "<fill>"}, tweak \code{stroke}.
#' Provide *_col or *_rule, or leave both NULL → defaults (colour: sig/pos = red, sig/neg = blue, else grey; shape 16; size 2).
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_jitter geom_hline stat_summary
#' @importFrom ggplot2 scale_colour_identity scale_shape_identity scale_size_identity
#' @importFrom ggplot2 labs theme_classic element_blank
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom cli cli_abort
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   var        = paste0("gene", 1:60),
#'   group      = sample(c("Tcell","Bcell","NK"), 60, TRUE),
#'   aver_logFC = rnorm(60, 0, 2),
#'   p_val      = runif(60),
#'   my_shape   = sample(c(24,25,22), 60, TRUE),
#'   my_size    = sample(c(2,3,4), 60, TRUE)
#' )
#' plot_group_numbers(df,
#'                    group      = "group",
#'                    number     = "aver_logFC",
#'                    shape_col  = "my_shape",
#'                    size_col   = "my_size",
#'                    jitter     = "bee",
#'                    x_axis_pos = "zero",
#'                    mean_type  = "point")
plot_group_numbers <- function(df, group, number,
                               color_col = NULL, color_rule = NULL,
                               shape_col = NULL, shape_rule = NULL,
                               size_col  = NULL, size_rule  = NULL,
                               jitter    = c("no","yes","bee"),
                               x_axis_pos = c("default","zero"),
                               mean_type  = c("none","point","line"),
                               legend     = TRUE) {
  jitter <- match.arg(jitter); x_axis_pos <- match.arg(x_axis_pos); mean_type <- match.arg(mean_type)

  # ------- sanity checks -------
  for (col in c(group, number)) if (!col %in% names(df)) cli_abort("Column '{col}' not found in df.")
  if (!is.null(color_col) && !color_col %in% names(df)) cli_abort("Color column '{color_col}' not found.")
  if (!is.null(shape_col) && !shape_col %in% names(df)) cli_abort("Shape column '{shape_col}' not found.")
  if (!is.null(size_col)  && !size_col  %in% names(df)) cli_abort("Size column '{size_col}' not found.")
  add_aes <- function(vec, nm) { new <- paste0(".leo_", nm); df[[new]] <<- vec; new }
  n <- nrow(df)
  fill_col <- add_aes(ifelse(df[[shape_col]] %in% 21:25, df[[color_col]], NA), "fill")


  # ------ colour ------
  if (is.null(color_col)) {
    if (!is.null(color_rule)) {
      cols <- color_rule(df)
      if (length(cols) != n) cli_abort("color_rule() must return {n} colours.")
    } else {
      cols <- rep("grey70", n)
      if ("p_val" %in% names(df)) {
        pos <- df[[number]] > 0 & df$p_val < .05
        neg <- df[[number]] < 0 & df$p_val < .05
        cols[pos] <- "red"; cols[neg] <- "blue"
      }
    }
    color_col <- add_aes(cols, "col")
  }
  # ------ shape -------
  if (is.null(shape_col)) {
    shapes <- if (!is.null(shape_rule)) shape_rule(df) else rep(16, nrow(df))
    shape_col <- add_aes(shapes, "shape")
  }
  # ------ size --------
  if (is.null(size_col)) {
    sizes <- if (!is.null(size_rule)) size_rule(df) else rep(2, nrow(df))
    size_col <- add_aes(sizes, "size")
  }
  # ------ beeswarm fallback ------
  if (jitter == "bee" && !requireNamespace("ggbeeswarm", quietly = TRUE)) {
    leo_log("Package 'ggbeeswarm' not installed, fallback to jitter.", level = "warning"); jitter <- "yes"
  }
  leo_log("Plotting {length(unique(df[[group]]))} groups with jitter = '{jitter}', mean = '{mean_type}'.")
  aes_base <- ggplot2::aes(x = !!sym(group), y = !!sym(number), fill = !!sym(fill_col),
                           colour = !!sym(color_col), shape = !!sym(shape_col), size = !!sym(size_col))
  p <- ggplot2::ggplot(df, aes_base) +
    switch(jitter,
           "no"  = ggplot2::geom_point(),
           "yes" = ggplot2::geom_jitter(width = .2, height = 0),
           "bee" = ggbeeswarm::geom_beeswarm())
  if (mean_type == "point") p <- p + ggplot2::stat_summary(fun = mean, geom = "point",
                                                           shape = 23, size = 3, fill = "black", colour = "black")
  if (mean_type == "line")  p <- p + ggplot2::stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                                                           geom = "crossbar", width = .5, colour = "black")
  if (x_axis_pos == "zero") {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.line.x = element_blank(), axis.ticks.x = element_blank())
  } else p <- p + ggplot2::theme_classic()
  if (legend) {
    p <- p + ggplot2::scale_colour_identity() +
      ggplot2::scale_shape_identity() +
      ggplot2::scale_size_identity()
  } else {
    p <- p + ggplot2::scale_colour_identity(guide = "none") +
      ggplot2::scale_shape_identity(guide = "none") +
      ggplot2::scale_size_identity(guide = "none")
  }
  leo_log("Plotting complete.", level = "success"); return(p)
}
