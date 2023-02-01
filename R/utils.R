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

#' Convenience function to pull qpack version number
#'
#' @keywords internal
#' @export

version <- function(){
  packageVersion("qpack")
}
