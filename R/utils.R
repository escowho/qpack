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


#' View .qconfig file
#'
#' @keywords internal
#' @export

view_qconfig <- function(){
  file.edit(".qconfig")
}

#' Create .qconfig file
#'
#' @keywords internal
#' @importFrom gtools ask
#' @export

create_qconfig <- function(){
  key <- gtools::ask(msg="Enter the API Key:")
  url <- gtools::ask(msg="Enter the Base URL:")
  cat(paste0("QUALTRICS_API_KEY = \"", key, "\""), file="./.qconfig", sep="\n")
  cat(paste0("QUALTRICS_BASE_URL = \"", url, "\""), file="./.qconfig", append=TRUE)
}

#' Delete .qconfig file
#'
#' @keywords internal
#' @export

delete_qconfig <- function(){
  if(file.exists(".qconfig")){
    file.remove(".qconfig")
  }
}
