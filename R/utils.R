globalVariables(c(".", ":=", "Column", "Max", "Mean", "Med", "Min", "Missing",
                  "Number", "Q10", "Q25", "Q75", "Q90", "SD", "Unique", "VALUE",
                  "avg", "codebook", "column", "d", "days", "dev.off", "end_date",
                  "jpeg", "label", "med", "median", "nnn", "packageVersion",
                  "percent", "q10", "q25", "q75", "q90", "recorded_date",
                  "setTxtProgressBar", "start_date", "stat", "std", "txtProgressBar",
                  "value", "var", "var_label", "variable", "variable_label", "x", "xxx", "zip"))

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
#' @param pack_list A character vector with the names of packages to update. Default: qpack.
#' @param dev A logical to indicate if the development version of qpack should be
#' installed.  Will only work if qpack is the only package listed in the pack_list.
#' Default: FALSE.
#' @return Nothing, updates package(s).
#' @keywords internal
#' @examples
#' \dontrun{
#' refresh()
#' refresh(dev=TRUE)
#' }
#' @importFrom remotes install_github

refresh <- function(pack_list="qpack", dev=FALSE){

  # Helper Functions --------------------------------------------------------

  update_pack <- function(pack){
    remotes::install_github(paste0("mshefferq/", pack))
  }

  update_dev <- function(pack){
    remotes::install_github("mshefferq/qpack", ref="dev")
  }

  # Function ----------------------------------------------------------------

  if(missing(pack_list)){
    if (Sys.getenv("QPACK_SETUP_PACKS")==""){
      pack_list <- c("qpack")
    } else {
      pack_list <- strsplit(Sys.getenv("QPACK_SETUP_PACKS"), ",")[[1]]
      pack_list <- invisible(lapply(pack_list, trimws))
    }
  }

  if (dev==TRUE & length(pack_list)==1 & pack_list=="qpack"){
    invisible(update_dev())
  } else if (dev==TRUE & length(pack_list)>1){
    stop(call. = FALSE, "dev cannot be TRUE if any pack other than qpack is listed in the pack_list.")
  } else {
    invisible(lapply(pack_list, update_pack))
  }

}
