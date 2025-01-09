globalVariables(c(".", ":=", "Column", "Max", "Mean", "Med", "Min", "Missing",
                  "Number", "Q10", "Q25", "Q75", "Q90", "SD", "Unique", "VALUE",
                  "avg", "codebook", "column", "d", "days", "dev.off", "end_date",
                  "jpeg", "label", "med", "median", "nnn", "packageVersion",
                  "percent", "q10", "q25", "q75", "q90", "recorded_date",
                  "setTxtProgressBar", "start_date", "stat", "std",
                  "txtProgressBar", "value", "var", "var_label", "variable",
                  "variable_label", "x", "xxx", "zip",
                  "nps_pro", "nps_pas", "nps_det", "order1", "order2",
                  "valid_percent"))

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Convenience Function to Pull Version
#'
#' @keywords internal

version <- function(){
  packageVersion("qpack")
}

#' Re-export set_colnames from magrittr
#' @export
#' @importFrom magrittr set_colnames
magrittr::set_colnames

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

#' @title Refreshes github install of qpack
#' @description Convenience function to perform a remotes::install_github for qpack.
#' @param dev A logical to indicate if the development version of qpack should be
#' installed. Default: FALSE.
#' @return Nothing, updates package(s).
#' @keywords internal
#' @examples
#' \dontrun{
#' qpack:::refresh()
#' qpack:::refresh(dev=TRUE)
#' }
#' @importFrom remotes install_github

refresh <- function(dev=FALSE){

  # Function ----------------------------------------------------------------

  if (dev==TRUE){
    if (Sys.getenv("QPACK_TEST")==TRUE){
      return("update_dev")
    } else {
      invisible(remotes::install_github("mshefferq/qpack", ref="dev"))
    }

 } else {

    if (Sys.getenv("QPACK_TEST")==TRUE){
      return("update_pack")
    } else {
      invisible(remotes::install_github("mshefferq/qpack"))
    }
  }

}


