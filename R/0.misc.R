#' Set or unset proxy
#'
#' Quickly enable or disable HTTP/HTTPS (and optionally SOCKS5) proxy for the current R session.
#'
#' @param enable TRUE to enable, FALSE to disable.
#' @param host Proxy host address.
#' @param port Proxy port number.
#' @param socks Whether to also set SOCKS5 proxy.
#'
#' @export
#' @examples
#' # Enable proxy (HTTP/HTTPS + SOCKS5)
#' set_proxy(TRUE, host = "127.0.0.1", port = 7897, socks = TRUE)
#'
#' # Test proxy
#' system("curl cip.cc")
#'
#' # Disable proxy
#' set_proxy(FALSE)
set_proxy <- function(enable = TRUE, host = "127.0.0.1", port = 7897, socks = TRUE) {
  if (enable) {
    Sys.setenv(
      http_proxy  = glue::glue("http://{host}:{port}"),
      https_proxy = glue::glue("http://{host}:{port}")
    )
    if (socks) Sys.setenv(all_proxy = glue::glue("socks5://{host}:{port}"))
    leo_log("✅ Proxy enabled: {host}:{port} {if (socks) '(HTTP/HTTPS + SOCKS5)' else '(HTTP/HTTPS only)'}")
  } else {
    Sys.unsetenv(c("http_proxy", "https_proxy", "all_proxy"))
    leo_log("🚫 Proxy disabled.")
  }
  invisible()
}

#' Install local package
#'
#' This function is writen as I often forget what to code when I want to install a package from local source.
#' This now uses `pak::pkg_install()` for a consistent installation backend.
#' Anyway, it is always good to simplify the process.
#'
#' @param path The path to the local package source.
#'
#' @export
install_local <- function(path) {
  if (!file.exists(path)) {
    leo_log("Wrong path to the package source. Please check the path and try again.", level = "danger")
    return(invisible())
  }
  if (!requireNamespace("pak", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg pak} is required by {.fn install_local}.",
      "i" = "Install with {.code install.packages('pak')} first."
    ))
  }
  leo_log("Installing package from local source...")
  pak::pkg_install(path)
  return(invisible())
}

#' Install LEO package dependencies
#'
#' Install dependency sets for \code{leo.basic} or other \code{leo.*} packages
#' universe using \code{pak}.
#'
#' @param leo.pak Character scalar. LEO package name. Default \code{"leo.basic"}.
#' @param ncpus Integer. Number of CPU cores for parallel compilation. Default 4.
#' @param upgrade Logical. Whether to upgrade already installed packages. Default FALSE.
#'
#' @return Invisibly returns the package specs passed to \code{pak::pkg_install()}.
#'
#' @examples
#' \dontrun{
#' # 1) Install pak
#' install.packages("pak", repos = "https://cloud.r-project.org")
#'
#' # 2) Install leo.basic first
#' pak::pkg_install("laleoarrow/leo.basic")
#'
#' # 3) Install leo.ukb (deps first, then package)
#' leo.basic::install_deps("leo.ukb", ncpus = 8)
#' pak::pkg_install("laleoarrow/leo.ukb")
#'
#' # 4) Install leo.gwas (deps first, then package)
#' leo.basic::install_deps("leo.gwas", ncpus = 8)
#' pak::pkg_install("laleoarrow/leo.gwas")
#' }
#'
#' @export
install_deps <- function(leo.pak = "leo.basic", ncpus = 4L, upgrade = FALSE) {
  if (!requireNamespace("pak", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg pak} is required by {.fn install_deps}.",
      "i" = "Install with {.code install.packages('pak')} first."
    ))
  }

  # Set parallel compilation
  ncpus <- as.integer(ncpus)
  if (ncpus > 1L) {
    Sys.setenv(MAKEFLAGS = sprintf("-j%d", ncpus))
    leo_log("Using {ncpus} CPU cores for compilation", level = "info")
  }

  leo.pak <- as.character(leo.pak)[1]

  specs <- switch(
    leo.pak,
    "leo.basic" = c("showteeth/ggpie", "bioc::clusterProfiler", "bioc::ReactomePA", "bioc::org.Hs.eg.db"),
    "leo.gwas" = c(
      "laleoarrow/leo.basic", "stephenturner/annotables",
      "catboost/catboost/catboost/R-package",
      "bioc::AnnotationDbi", "bioc::biomaRt", "bioc::BSgenome",
      "bioc::GenomicFeatures", "bioc::MungeSumstats", "bioc::rtracklayer",
      "caret", "cli", "data.table", "ggplot2", "ggsci", "glmnet", "glue",
      "ieugwasr", "LDlinkR", "magrittr", "plinkbinr", "pROC", "purrr",
      "RColorBrewer", "rlang", "stringr", "tidyr", "TwoSampleMR", "vroom"
    ),
    "leo.ukb" = c(
      "laleoarrow/leo.basic",
      "broom", "cli", "data.table", "dplyr", "flextable", "glue",
      "lifecycle", "mice", "missRanger", "nortest", "rlang",
      "stringr", "tableone", "tibble", "tidyr", "VIM", "xtable"
    ),
    sprintf("laleoarrow/%s", leo.pak)
  )

  if (!leo.pak %in% c("leo.basic", "leo.gwas", "leo.ukb")) {
    leo_log("No preset dependency map for {leo.pak}; installing remote package with pak.", level = "warning")
  }

  specs <- unique(specs)
  leo_log("Installing {length(specs)} dependencies for {leo.pak}", level = "info")
  pak::pkg_install(specs, upgrade = upgrade, ask = FALSE)
  invisible(specs)
}
