#' Recode a Vector Using a Two‐Column Mapping Table
#'
#' This function takes a vector \code{x} and a mapping data frame \code{map_df}
#' that has two columns: one with the original values and one with the replacement
#' values. Internally it builds a named vector via \code{tibble::deframe()}, then
#' applies \code{dplyr::recode()} with splicing (\code{!!!}) to perform the recoding.
#'
#' @param x            A vector whose elements you want to recode.
#' @param map_df       A data frame or tibble containing exactly two columns:
#'                     one for the old values, one for the new values.
#' @param from         A string giving the name of the column in \code{map_df}
#'                     that contains the original values.
#' @param to           A string giving the name of the column in \code{map_df}
#'                     that contains the replacement values.
#'
#' @return A vector (same type as \code{x}) with all values matching
#'         \code{map_df[[from]]} replaced by the corresponding
#'         \code{map_df[[to]]}; all other values in \code{x} are left unchanged.
#' @details
#' 就是要recode的时候就建立有两列的df,然后用deframe()然后recode(列, !!!name_vector)
#' @examples
#' library(dplyr)
#' map_df <- tibble::tibble(
#'   old = c("ENSG1", "ENSG2"),
#'   new = c("TP53",  "BRCA1")
#' )
#' recode_by_map(c("ENSG1","foo","ENSG2"), map_df, "old", "new")
#' #> [1] "TP53" "foo"  "BRCA1"
#'
#' @seealso
#' \link[dplyr]{recode}, \link[tibble]{deframe}
#'
#' @export
#' @importFrom dplyr distinct recode
#' @importFrom tibble deframe
#' @importFrom rlang sym
#' @importFrom magrittr %>%
recode_by_map <- function(x, map_df, from, to) {
  # capture the column names
  from_sym <- rlang::sym(from)
  to_sym   <- rlang::sym(to)
  # build a named vector: names = map_df[[from]], values = map_df[[to]]
  mapping_vec <- map_df %>%
    dplyr::distinct(!!from_sym, !!to_sym) %>%
    tibble::deframe()
  # apply recode
  dplyr::recode(x, !!!mapping_vec)
}
