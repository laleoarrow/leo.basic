#' Plot pie/ring via ggpie with install hint
#'
#' @param x Numeric vector: values (type='num') or ratios (type='ratio').
#' @param colors Vector of fill colors; NULL uses package palette.
#' @param type "num" or "ratio" for the meaning of x.
#' @param ring_ratio Visible ring thickness in [0,1]; 1=full pie (no hole).
#' @param annotation_type "in" or "out" for label position.
#' @param label_type "horizon" or "circle" for label style.
#' @param label_size Label size.
#' @return ggplot object.
#'
#' @importFrom ggpie ggpie ggdonut
#' @importFrom ggsci pal_npg
#' @importFrom grDevices adjustcolor
#' @examples
#' plot_pie(c(A=30,B=20,C=50), type="num", ring_ratio=1, annotation_type="in")
#' plot_pie(c(A=0.2,B=0.3,C=0.5), type="ratio", ring_ratio=0.6, annotation_type="out", label_type="horizon",
#'          colors=c("#66c2a5","#fc8d62","#8da0cb"))
#' plot_pie(c(A=30,B=20,C=50), color_alpha = 0.8, type="num", ring_ratio=0.6, annotation_type="out")
#' @export
plot_pie <- function(x, colors=NULL, color_alpha = 1, type=c("num","ratio"), ring_ratio=1,
                     annotation_type=c("in","out"), label_type=c("horizon","circle"), label_size=4) {
  leo_log("Tutorial: https://showteeth.github.io/ggpie/")
  # --- package check (print install snippet if missing) ---
  if (!requireNamespace("ggpie", quietly = TRUE)) {
    cat("# install.packages('remotes')  # if not installed\n",
        "install.packages('ggpie')\n",
        "# or\n",
        "remotes::install_github('showteeth/ggpie')\n", sep = "")
    return(leo_log("Package 'ggpie' not found", level="danger"))
  }
  if (is.null(colors)) colors <- ggsci::pal_npg("nrc")(length(x))
  if (color_alpha < 1) colors <- grDevices::adjustcolor(colors, alpha.f = color_alpha)
  # --- validate input ---
  type <- match.arg(type); annotation_type <- match.arg(annotation_type); label_type <- match.arg(label_type)
  if (!is.numeric(x) || any(is.na(x)) || any(x < 0)) stop("`x` must be non-negative numeric.")
  if (sum(x) == 0) stop("Sum of `x` must be > 0.")
  if (is.null(names(x))) names(x) <- paste0("C", seq_along(x))
  if (!is.null(colors) && length(colors) != length(x)) {leo_log("`colors` length != length(x); use package palette.", level="warning"); colors <- NULL}

  # --- build df for ggpie/ggdonut ---
  # ggpie uses a data.frame with 'group' and 'count' (even if you conceptually pass ratios)
  df <- data.frame(group = names(x), count = as.numeric(x), stringsAsFactors = FALSE)
  label_info <- if (type == "num") "all" else "ratio"

  # --- route: pie vs donut (ring_ratio -> r0,r1) ---
  ring_ratio <- max(0, min(1, ring_ratio)); r0 <- 1 - ring_ratio; r1 <- 1
  leo_log("Plot {ifelse(ring_ratio>=1,'pie','donut')}: n={nrow(df)}, type={type}, labels={label_info}, pos={annotation_type}, style={label_type}, ring_ratio={ring_ratio}")

  if (ring_ratio >= 1) {
    p <- ggpie::ggpie(data = df, group_key = "group", count_type = "count",
                      fill_color = colors, label_info = label_info,
                      label_type = label_type, label_pos = annotation_type,
                      label_size = label_size)
  } else {
    p <- ggpie::ggdonut(data = df, group_key = "group", count_type = "count",
                        fill_color = colors, label_info = label_info,
                        label_type = label_type, label_pos = annotation_type,
                        label_size = label_size, r0 = r0, r1 = r1, donut.label = FALSE)
  }
  leo_log("Ring/Pie plotted successfully.", level="success")
  return(p)
}

#' Plot numeric values by group with jitter / beeswarm, color / shape / size mapping
#'
#' Draw a grouped scatter of numeric values with optional jitter /
#' beeswarm arrangement, color / shape / size encoding, plus optional mean indicator.
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
  for (col in c(group, number)) if (!col %in% names(df)) cli_abort("Column '{col}' not found in df.")
  if (!is.null(color_col) && !color_col %in% names(df)) cli_abort("Color column '{color_col}' not found.")
  if (!is.null(shape_col) && !shape_col %in% names(df)) cli_abort("Shape column '{shape_col}' not found.")
  if (!is.null(size_col)  && !size_col  %in% names(df)) cli_abort("Size column '{size_col}' not found.")
  add_aes <- function(vec, nm) { new <- paste0(".leo_", nm); df[[new]] <<- vec; new }
  n <- nrow(df)
  fill_col <- add_aes(ifelse(df[[shape_col]] %in% 21:25, df[[color_col]], NA), "fill")

  # ------ colour
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
  # ------ shape
  if (is.null(shape_col)) {
    shapes <- if (!is.null(shape_rule)) shape_rule(df) else rep(16, nrow(df))
    shape_col <- add_aes(shapes, "shape")
  }
  # ------ size
  if (is.null(size_col)) {
    sizes <- if (!is.null(size_rule)) size_rule(df) else rep(2, nrow(df))
    size_col <- add_aes(sizes, "size")
  }
  # ------ beeswarm fallback
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


#' Generate a discrete color palette (expanded from a base panel)
#'
#' @description Return \code{n} distinct colors or a named vector for \code{levels}. Uses a fixed base panel and expands smoothly if more colors are needed.
#' @param levels character; category names. If provided, output is named with \code{levels}. If NULL, use \code{n}.
#' @param n integer; number of colors to return when \code{levels} is NULL.
#' @param base_panel named character; optional seed palette (hex). Defaults to an internal panel.
#' @return character vector of hex colors; named if \code{levels} is provided.
#' @examples
#' leo_discrete_color(n = 8)
#' leo_discrete_color(levels = c("Brain","Liver","Heart"))
#' @export
#' @importFrom grDevices colorRampPalette
#' @importFrom cli cli_alert_info cli_alert_success
leo_discrete_color <- function(levels = NULL, n = NULL, base_panel = NULL) {
  # -- validate input
  if (is.null(levels) && is.null(n)) stop("Provide either `levels` or `n`.")
  if (!is.null(levels)) {
    if (!is.character(levels)) stop("`levels` must be character.")
    n <- length(levels)
  } else {
    if (!is.numeric(n) || length(n) != 1 || n < 1) stop("`n` must be a positive integer.")
    n <- as.integer(n)
  }

  # -- base palette (your panel)
  if (is.null(base_panel)) {
    base_panel <- c(
      a1 = "#5BC0EB", a2 = "#9BC53D", a3 = "#C3423F", a4 = "#FDE74C",
      c1 = "#5A7B8F", c2 = "#EE4C97", c3 = "#3D806F", c4 = "#A08634",
      c5 = "#F37C95", c6 = "#608541", c7 = "#7D4E57", c8 = "#BC3C29",
      c9 = "#958056", c10 = "#9FAFA3", c11 = "#6F99AD", c12 = "#0072B5",
      c13 = "#CFC59A", c14 = "#E18727", c15 = "#FFDC91", c16 = "#718DAE",
      c17 = "#7876B1", c18 = "#F9AC93", c19 = "#3E6086", c20 = "#20854E",
      c21 = "#7581AF", c22 = "#4A7985"
    )
  }
  base_vals <- unname(base_panel)

  # -- generate palette (no repetition; expand smoothly when needed)
  cli::cli_alert_info("Building discrete palette: need {n}, base size {length(base_vals)}")
  if (n <= length(base_vals)) pal <- base_vals[seq_len(n)] else pal <- grDevices::colorRampPalette(base_vals)(n)

  # -- name by levels if provided
  if (!is.null(levels)) names(pal) <- levels

  cli::cli_alert_success("Palette ready: {n} colors")
  pal
}


#' Rasterize point-like layers in a ggplot object
#'
#' @param plot A ggplot object.
#' @param dpi Integer DPI for rasterization (default 300).
#' @param layers Character vector of layer types to rasterize, e.g. c("Point","Jitter","Line","Segment","EdgeSegment").
#'
#' @return ggplot object with target layers rasterized.
#'
#' @examples
#' library(ggplot2); library(ggrastr)
#' plot <- ggplot(diamonds, aes(carat, price, colour = cut)) + geom_point()
#' rasterize_layers(plot, dpi = 100)
#' @export
rasterize_layers <- function(plot, dpi = 300, layers = c("Point", "Jitter", "Line", "Segment", "EdgeSegment")){
  if (!inherits(plot, "ggplot")) stop("`plot` must be a ggplot object.")
  # list layer geom classes in the plot
  layer_geoms <- sapply(plot$layers, function(l) class(l$geom)[1])
  leo_log("layers in plot -> {paste(layer_geoms, collapse = \", \")}; target -> {paste(layers, collapse = \", \")}; dpi={dpi}")

  # map human-friendly types to common Geom classes for presence check
  type2geom <- c(Point = "GeomPoint", Jitter = "GeomJitter", Line = "GeomLine",
                 Segment = "GeomSegment", EdgeSegment = "GeomEdgeSegment", EdgeLink = "GeomEdgeLink", Path = "GeomPath")

  # rasterize only when requested type is present (or unknown -> try anyway)
  for (t in layers) {
    present <- if (t %in% names(type2geom)) type2geom[[t]] %in% layer_geoms else TRUE
    if (present) { plot <- ggrastr::rasterize(plot, layers = t, dpi = dpi); leo_log("rasterized {t} layer(s).") }
  }
  return(plot)
}
