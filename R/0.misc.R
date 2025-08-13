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
