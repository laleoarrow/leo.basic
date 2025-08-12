#' Install local package
#'
#' This function is writen as I often forget what to code when I want to install a package from local source.
#' It is in fact quite simple to add "repos = NULL, type = "source" in `install.packages()`.
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
  leo_log("Installing package from local source...")
  install.packages(path, repos = NULL, type = "source")
  return(invisible())
}
