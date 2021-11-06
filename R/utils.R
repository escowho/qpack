#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Skeleton Syntax Printer for Set Up
#'
#' @keywords internal
#' @export

skeleton <- function(){
  cat(
    "#==============================================================================#
qpack::set_up(client = \"\",
              project = \"\",
              task = \"\",
              root = \"\",
              descriptor = \"\",
              folders = c(),
              pack_load = c(),
              pack_check = c(),
              source = c(),
              qpack = TRUE)
#==============================================================================#




# Cleanup -----------------------------------------------------------------
rm()
#
#
#
#
#
#
#
#
#
#")
}

#' Vector of custom colors
#'
#' @examples
#' qcolors["my_orange"]
#'
#' @export
qcolors = c(
  my_orange = '#ff6901',
  my_purple = '#6f54a3',
  my_green = '#53b647',
  my_dark_green = '#118482'
)
