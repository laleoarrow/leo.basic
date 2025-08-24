#' leo_permutation_test: flexible one-way permutation / rank-based testing
#'
#' @description Compare one or multiple numeric features between two groups
#'   using permutation-based tests from **coin**. Two usage modes:
#'   \enumerate{
#'     \item \strong{Vector mode} – supply a numeric vector \code{data} and a grouping
#'           vector \code{class_vec}.
#'     \item \strong{Data-frame mode} – supply a data.frame \code{data} plus the
#'           column name holding the groups (\code{class_col}) and, optionally,
#'           the feature columns (\code{feature_cols}).
#'   }
#'   Workflow per feature:
#'   \itemize{
#'     \item Normality test — Shapiro–Wilk for \eqn{n ≤ 5000}, Anderson–Darling
#'           for \eqn{n > 5000}.
#'     \item Variance homogeneity — F-test.
#'     \item If both criteria hold, run \code{coin::oneway_test}; otherwise run
#'           \code{coin::wilcox_test}.
#'   }
#'
#' @param data         Numeric vector \emph{or} data.frame.
#' @param class_vec    Grouping vector when \code{data} is numeric (length must
#'   match \code{data}); accepted as 0/1 or any two-level factor/character.
#' @param class_col    Character. Column name of grouping variable in a
#'   data.frame. Default \code{"Class"}.
#' @param feature_cols Character vector of feature columns in the data.frame.
#'   If \code{NULL} (default), all numeric columns except \code{class_col} are
#'   used.
#' @param set_seed     Integer random seed (passed to \code{set.seed}). Default
#'   \code{725}.
#'
#' @return A tibble with three columns:
#'   \describe{
#'     \item{Feature}{Feature name}
#'     \item{P}{P-value}
#'     \item{Type}{Test used: `"oneway"` or `"wilcox"`}
#'   }
#'
#' @importFrom coin oneway_test wilcox_test pvalue
#' @importFrom stats shapiro.test var.test na.omit
#' @importFrom nortest ad.test
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' ## vector mode --------------------------------------------------------------
#' set.seed(1)
#' feat <- rnorm(100); cls <- rep(c(0, 1), each = 50)
#' leo_permutation_test(feat, class_vec = cls)
#'
#' ## data-frame mode: specify feature columns ---------------------------------
#' df <- data.frame(Class = cls,
#'                  F1 = feat,
#'                  F2 = rnorm(100, 0.3),
#'                  F3 = rnorm(100, 1))
#' leo_permutation_test(df, class_col = "Class", feature_cols = c("F1", "F2", "F3"))
#'
#' ## data-frame mode: all numeric features except Class -----------------------
#' leo_permutation_test(df, class_col = "Class")
#'
#' ## large-n example triggers AD test -----------------------------------------
#' big_feat <- rnorm(6000); big_grp <- rep(c(0, 1), each = 3000)
#' leo_permutation_test(big_feat, class_vec = big_grp)
leo_permutation_test <- function(data,
                                 class_vec    = NULL,
                                 class_col    = "Class",
                                 feature_cols = NULL,
                                 set_seed     = 725) {
  set.seed(set_seed)

  ## -------- input parsing ---------------------------------------------------
  if (is.numeric(data) && !is.null(class_vec)) {               # vector mode
    df            <- data.frame(Class = class_vec, Feature = data)
    feature_names <- "Feature"
    group_vec     <- factor(df$Class)
  } else if (inherits(data, "data.frame")) {                   # data.frame mode
    data <- as.data.frame(data)
    if (!class_col %in% names(data)) stop("`class_col` not found in data.")
    feature_names <- if (is.null(feature_cols))
      setdiff(names(data), class_col) else feature_cols
    if (length(feature_names) == 0) stop("No feature columns specified.")
    if (!all(feature_names %in% names(data))) {
      miss <- feature_names[!feature_names %in% names(data)]
      stop(sprintf("feature_cols not found: %s", paste(miss, collapse = ", ")))
    }
    if (!all(vapply(data[feature_names], is.numeric, logical(1)))) {
      non_num <- feature_names[!vapply(data[feature_names], is.numeric, logical(1))]
      stop(sprintf("Non-numeric features: %s", paste(non_num, collapse = ", ")))
    }
    df        <- data[, c(class_col, feature_names)]
    group_vec <- factor(df[[class_col]])
  } else {
    stop("Unsupported input: supply (numeric feature, class_vec) or (data.frame like object).")
  }
  if (length(levels(group_vec)) != 2) stop("`class_vec` (or class column) must have exactly two levels.")

  ## -------- main loop -------------------------------------------------------
  res <- tibble::tibble(Feature = feature_names,
                        P       = NA_real_,
                        Type    = NA_character_)

  for (i in seq_along(feature_names)) {
    ft <- feature_names[i]
    x1 <- stats::na.omit(df[[ft]][group_vec == levels(group_vec)[1]])
    x2 <- stats::na.omit(df[[ft]][group_vec == levels(group_vec)[2]])

    ## normality --------------------------------------------------------------
    if (length(unique(x1)) == 1 | length(unique(x2)) == 1) {
      normal <- FALSE
    } else {
      p1 <- if (length(x1) > 5000) nortest::ad.test(x1)$p.value else stats::shapiro.test(x1)$p.value
      p2 <- if (length(x2) > 5000) nortest::ad.test(x2)$p.value else stats::shapiro.test(x2)$p.value
      normal <- p1 >= .05 & p2 >= .05
    }

    ## variance homogeneity ---------------------------------------------------
    equal_var <- stats::var.test(x1, x2)$p.value >= .05

    ## choose test ------------------------------------------------------------
    if (normal & equal_var) {
      res$P[i]    <- coin::pvalue(coin::oneway_test(df[[ft]] ~ group_vec,
                                                    distribution = "asymptotic"))
      res$Type[i] <- "oneway"
    } else {
      res$P[i]    <- coin::pvalue(coin::wilcox_test(df[[ft]] ~ group_vec,
                                                    distribution = "asymptotic"))
      res$Type[i] <- "wilcox"
    }
  }

  leo_log("Permutation test finished; processed {nrow(res)} features.", level = "success")
  return(res)
}
